import tkinter as tk
from tkinter import filedialog, messagebox, ttk
import os
import subprocess
import configparser
import re
import shutil

class Tooltip:
    def __init__(self, widget, text):
        self.widget = widget
        self.text = text
        self.tooltip = None
        self.widget.bind("<Enter>", self.show_tooltip)
        self.widget.bind("<Leave>", self.hide_tooltip)

    def show_tooltip(self, event=None):
        x = self.widget.winfo_rootx() + 30
        y = self.widget.winfo_rooty() + 30
        self.tooltip = tk.Toplevel(self.widget)
        self.tooltip.wm_overrideredirect(True)
        self.tooltip.wm_geometry(f"+{x}+{y}")
        label = tk.Label(self.tooltip, text=self.text, bg="lightyellow", relief="solid", borderwidth=1, padx=6, pady=4)
        label.pack()

    def hide_tooltip(self, event=None):
        if self.tooltip:
            self.tooltip.destroy()
            self.tooltip = None


class App:
    def __init__(self):
        self.root = tk.Tk()
        self.root.title("COMOL-FX Stereo Processor Configurator")
        self.root.geometry("1060x820")

        self.config_file = 'config_fx.ini'
        self.settings = self.load_settings()

        # COBOL file selection
        cobol_frame = tk.Frame(self.root)
        cobol_frame.pack(fill='x', padx=15, pady=12)

        tk.Label(cobol_frame, text="COMOL-FX COBOL Source File:", font=('Helvetica', 10, 'bold')).pack(side='left', padx=(0, 10))

        self.cobol_path_var = tk.StringVar()
        path_entry = tk.Entry(cobol_frame, textvariable=self.cobol_path_var, state='readonly', width=85)
        path_entry.pack(side='left', fill='x', expand=True, padx=(0, 10))

        select_btn = tk.Button(
            cobol_frame,
            text="Select File",
            command=self.select_cobol,
            font=('Helvetica', 10, 'bold'),
            padx=14,
            pady=6,
            width=12
        )
        select_btn.pack(side='right')

        # Output directory selection
        out_dir_frame = tk.Frame(self.root)
        out_dir_frame.pack(fill='x', padx=15, pady=(0, 8))

        tk.Label(out_dir_frame, text="Output Directory:", font=('Helvetica', 10, 'bold')).pack(side='left', padx=(0, 10))

        self.output_dir_var = tk.StringVar()
        out_dir_entry = tk.Entry(out_dir_frame, textvariable=self.output_dir_var, state='readonly', width=85)
        out_dir_entry.pack(side='left', fill='x', expand=True, padx=(0, 10))

        select_out_btn = tk.Button(
            out_dir_frame,
            text="Select Folder",
            command=self.select_output_dir,
            font=('Helvetica', 10, 'bold'),
            padx=14,
            pady=6,
            width=12
        )
        select_out_btn.pack(side='right')

        self.compile_run_var = tk.BooleanVar(value=True)
        tk.Checkbutton(
            self.root,
            text="Compile & run each generated program in Command Prompt (requires GnuCOBOL in PATH)",
            variable=self.compile_run_var
        ).pack(anchor='w', padx=15, pady=(0, 10))

        self.notebook = ttk.Notebook(self.root)
        self.notebook.pack(fill='both', expand=True, padx=10, pady=5)

        self.var_descriptions = {
            'FX-SPEC-FREQ': "Spectrum Frequency (0.01 - 0.95)",
            'FX-SPEC-Q': "Spectrum Q Factor (0.1 - 0.99)",
            'FX-SPEC-GAIN': "Spectrum Gain (-0.5 - 2.0)",
            'FX-DRIVE-AMOUNT': "Saturation Drive Amount (0.0 - 2.0)",
            'FX-PHASER-RATE': "Phaser LFO Rate (0.01 - 8.0)",
            'FX-PHASER-DEPTH': "Phaser Depth (0.0 - 1.0)",
            'FX-PHASER-MANUAL': "Phaser Manual Position (0.0 - 1.0)",
            'FX-PHASER-FBACK': "Phaser Feedback (0.0 - 0.95)",
            'FX-PHASER-MIX': "Phaser Wet/Dry Mix (0.0 - 1.0)",
            'FX-CHORUS-RATE': "Chorus LFO Rate (0.01 - 5.0)",
            'FX-CHORUS-DEPTH': "Chorus Depth in samples (0 - 300)",
            'FX-CHORUS-MIX': "Chorus Wet/Dry Mix (0.0 - 1.0)",
            'FX-DELAY-MS': "Delay Time in ms (10 - 3000)",
            'FX-DELAY-FBACK': "Delay Feedback (0.0 - 0.95)",
            'FX-DELAY-MIX': "Delay Wet/Dry Mix (0.0 - 1.0)",
            'FX-REV-DECAY': "Reverb Decay (0.70 - 0.999)",
            'FX-REV-MIX': "Reverb Wet/Dry Mix (0.0 - 1.0)",
            'IN_FILE_PATH': "Path to input mono .raw file (16-bit signed PCM)",
            'OUT_L_FILE_PATH': "Path to output LEFT channel .raw file",
            'OUT_R_FILE_PATH': "Path to output RIGHT channel .raw file"
        }

        self.variables = list(self.var_descriptions.keys())

        vars_per_column = (len(self.variables) + 2) // 3
        col_groups = [
            self.variables[0:vars_per_column],
            self.variables[vars_per_column:vars_per_column * 2],
            self.variables[vars_per_column * 2:]
        ]

        self.tabs = []
        self.entry_vars = [{} for _ in range(4)]
        self.tooltips = [{} for _ in range(4)]

        for i in range(4):
            tab = ttk.Frame(self.notebook)
            self.notebook.add(tab, text=f"Preset {i+1}")
            self.tabs.append(tab)
            self.entry_vars[i] = {}
            self.tooltips[i] = {}

            for col_idx, var_group in enumerate(col_groups):
                for row_idx, var in enumerate(var_group):
                    row = row_idx
                    col_label = col_idx * 3
                    col_entry = col_label + 1
                    col_button = col_label + 2

                    tk.Label(tab, text=var + ":", width=26, anchor='e').grid(
                        row=row, column=col_label, padx=(10, 4), pady=4, sticky='e')

                    self.entry_vars[i][var] = tk.StringVar()
                    entry = tk.Entry(tab, textvariable=self.entry_vars[i][var], width=28)
                    entry.grid(row=row, column=col_entry, padx=(4, 8), pady=4, sticky='ew')

                    self.tooltips[i][var] = Tooltip(entry, self.var_descriptions[var])

                    if var in ('IN_FILE_PATH', 'OUT_L_FILE_PATH', 'OUT_R_FILE_PATH'):
                        if var == 'IN_FILE_PATH':
                            cmd = lambda idx=i: self.browse_input(idx)
                        else:
                            cmd = lambda idx=i, left=(var == 'OUT_L_FILE_PATH'): self.browse_output(idx, left)
                        btn = tk.Button(tab, text="Browse", command=cmd, width=8)
                        btn.grid(row=row, column=col_button, padx=(4, 12), pady=4, sticky='w')

            for c in range(3):
                tab.columnconfigure(c * 3 + 1, weight=1)

            copy_row = vars_per_column + 1
            copy_frame = tk.Frame(tab)
            copy_frame.grid(row=copy_row, column=0, columnspan=9, pady=(15, 6), padx=10, sticky='w')
            tk.Label(copy_frame, text="Copy preset to:", font=('Helvetica', 9, 'bold')).pack(side='left', padx=(0, 8))
            for j in range(4):
                if j != i:
                    btn = tk.Button(
                        copy_frame,
                        text=f"Preset {j+1}",
                        command=lambda src=i, dst=j: self.copy_preset(src, dst),
                        width=10
                    )
                    btn.pack(side='left', padx=4)

        btn_frame = tk.Frame(self.root)
        btn_frame.pack(pady=18)

        tk.Button(
            btn_frame,
            text="Compiler Settings",
            command=self.open_settings,
            font=('Helvetica', 11, 'bold'),
            padx=22,
            pady=11
        ).pack(side='left', padx=12)

        tk.Button(
            btn_frame,
            text="Generate All Presets",
            command=self.generate,
            font=('Helvetica', 11, 'bold'),
            padx=22,
            pady=11
        ).pack(side='left', padx=12)

        tk.Button(
            btn_frame,
            text="Save Template",
            command=self.save_template,
            font=('Helvetica', 11, 'bold'),
            padx=22,
            pady=11
        ).pack(side='left', padx=12)

        tk.Button(
            btn_frame,
            text="Load Template",
            command=self.load_template,
            font=('Helvetica', 11, 'bold'),
            padx=22,
            pady=11
        ).pack(side='left', padx=12)

        self.load_session()
        self.root.protocol("WM_DELETE_WINDOW", self.on_close)
        self.root.mainloop()

    # === TEMPLATE / SESSION / SETTINGS METHODS (unchanged) ===
    def save_template(self):
        path = filedialog.asksaveasfilename(
            title="Save FX Preset Template",
            defaultextension=".ini",
            filetypes=[("Template files", "*.ini"), ("Text files", "*.txt"), ("All files", "*.*")],
            parent=self.root
        )
        if not path: return
        config = configparser.ConfigParser()
        config['Meta'] = {'cobol_path': self.cobol_path_var.get(), 'output_dir': self.output_dir_var.get()}
        for i in range(4):
            section = f'Preset_{i+1}'
            config[section] = {var: self.entry_vars[i][var].get() for var in self.variables}
        with open(path, 'w') as f:
            f.write("# COMOL-FX Stereo Processor - Preset Template File\n")
            f.write("# Edit values below and load back with 'Load Template'.\n\n")
            config.write(f)
        messagebox.showinfo("Template Saved", f"Template saved to:\n{path}")

    def load_template(self):
        path = filedialog.askopenfilename(
            title="Load FX Preset Template",
            filetypes=[("Template files", "*.ini"), ("Text files", "*.txt"), ("All files", "*.*")],
            parent=self.root
        )
        if not path: return
        config = configparser.ConfigParser()
        config.read(path)
        if 'Meta' in config:
            cp = config['Meta'].get('cobol_path', '')
            if cp and os.path.isfile(cp):
                self.cobol_path_var.set(cp)
            od = config['Meta'].get('output_dir', '')
            if od:
                self.output_dir_var.set(od)
        loaded = 0
        for i in range(4):
            section = f'Preset_{i+1}'
            if section in config:
                for var in self.variables:
                    val = config[section].get(var.lower(), config[section].get(var, ''))
                    self.entry_vars[i][var].set(val)
                loaded += 1
        messagebox.showinfo("Template Loaded", f"Loaded {loaded} preset(s) from:\n{path}") if loaded else \
            messagebox.showwarning("Load Failed", "No [Preset_N] sections found.")

    def on_close(self):
        self.save_session()
        self.root.destroy()

    def load_settings(self):
        config = configparser.ConfigParser()
        defaults = {
            'compiler_path': 'cobc.exe', 'path': '', 'cob_config_dir': '', 'cob_copy_dir': '',
            'cob_include_path': '', 'cob_lib_path': '', 'vcvarsall_path': '',
            'output_directory': 'bin', 'copy_runtime_dlls': 'no'
        }
        if os.path.exists(self.config_file):
            config.read(self.config_file)
            if 'DEFAULT' in config:
                for key in defaults:
                    if key in config['DEFAULT']:
                        defaults[key] = config['DEFAULT'][key]
        return defaults

    def save_settings(self, new_settings):
        config = configparser.ConfigParser()
        config['DEFAULT'] = new_settings
        if os.path.exists(self.config_file):
            existing = configparser.ConfigParser()
            existing.read(self.config_file)
            if 'SESSION' in existing:
                config['SESSION'] = dict(existing['SESSION'])
        with open(self.config_file, 'w') as f:
            config.write(f)

    def save_session(self):
        config = configparser.ConfigParser()
        if os.path.exists(self.config_file):
            config.read(self.config_file)
        session = {'cobol_path': self.cobol_path_var.get(), 'output_dir': self.output_dir_var.get()}
        for i in range(4):
            for var in self.variables:
                session[f'tab{i}_{var}'] = self.entry_vars[i][var].get()
        config['SESSION'] = session
        with open(self.config_file, 'w') as f:
            config.write(f)

    def load_session(self):
        if not os.path.exists(self.config_file): return
        config = configparser.ConfigParser()
        config.read(self.config_file)
        if 'SESSION' not in config: return
        session = config['SESSION']
        if cobol_path := session.get('cobol_path'):
            if os.path.isfile(cobol_path):
                self.cobol_path_var.set(cobol_path)
        if output_dir := session.get('output_dir'):
            self.output_dir_var.set(output_dir)
        for i in range(4):
            for var in self.variables:
                if val := session.get(f'tab{i}_{var}'):
                    self.entry_vars[i][var].set(val)

    def open_settings(self):
        # (unchanged - same as before)
        settings_win = tk.Toplevel(self.root)
        settings_win.title("Compiler Settings")
        settings_win.geometry("820x580")
        settings_win.resizable(True, True)
        settings_win.transient(self.root)
        settings_win.grab_set()

        descriptions = {
            'compiler_path': "Full path to the COBOL compiler executable (e.g., cobc.exe).",
            'path': "Additional paths for the system PATH environment variable.",
            'cob_config_dir': "Directory containing COBOL configuration files.",
            'cob_copy_dir': "Directory containing COBOL copybooks.",
            'cob_include_path': "Include path for COBOL headers.",
            'cob_lib_path': "Library path for COBOL linking.",
            'vcvarsall_path': "Path to vcvarsall.bat for Visual C++ environment (if using MSVC build).",
            'output_directory': "Directory where compiled executables are placed (relative or absolute).",
            'copy_runtime_dlls': "Whether to copy runtime DLLs to the output directory (yes/no)."
        }

        entry_vars = {}
        row = 0
        for key, default in self.settings.items():
            tk.Label(settings_win, text=key.replace('_', ' ').title() + ":", font=('Helvetica', 10)).grid(
                row=row, column=0, padx=(30, 12), pady=10, sticky='e')
            entry_vars[key] = tk.StringVar(value=default)
            tk.Entry(settings_win, textvariable=entry_vars[key], width=75, font=('Helvetica', 9)).grid(
                row=row, column=1, padx=(0, 30), pady=10, sticky='ew')
            Tooltip(entry_vars[key].get(), descriptions.get(key, ""))
            row += 1

        settings_win.columnconfigure(1, weight=1)

        def save_and_close():
            new_settings = {k: entry_vars[k].get().strip() for k in self.settings}
            self.save_settings(new_settings)
            self.settings = new_settings
            settings_win.destroy()
            messagebox.showinfo("Settings Saved", "Compiler settings have been updated and saved.")

        btn_frame = tk.Frame(settings_win)
        btn_frame.grid(row=row + 1, column=0, columnspan=2, pady=30)
        tk.Button(btn_frame, text="Cancel", width=14, padx=20, pady=8, command=settings_win.destroy).pack(side='right', padx=12)
        tk.Button(btn_frame, text="Save Settings", width=16, font=('Helvetica', 10, 'bold'), padx=25, pady=8, command=save_and_close).pack(side='right', padx=12)

    def copy_preset(self, src, dst):
        for var in self.variables:
            self.entry_vars[dst][var].set(self.entry_vars[src][var].get())
        self.notebook.select(dst)
        messagebox.showinfo("Copied", f"Preset {src+1} parameters copied to Preset {dst+1}.")

    def select_output_dir(self):
        if path := filedialog.askdirectory(title="Select Output Directory"):
            self.output_dir_var.set(os.path.normpath(path))

    def select_cobol(self):
        if path := filedialog.askopenfilename(filetypes=[("COBOL files", "*.cbl *.cob")]):
            self.cobol_path_var.set(path)

    def browse_input(self, tab_idx):
        current = self.entry_vars[tab_idx]['IN_FILE_PATH'].get().strip()
        initial_dir = os.path.dirname(current) if current and os.path.isdir(os.path.dirname(current)) else os.getcwd()
        if path := filedialog.askopenfilename(
                title="Select Input .raw File",
                initialdir=initial_dir,
                filetypes=[("Raw audio", "*.raw"), ("All files", "*.*")],
                parent=self.root):
            self.entry_vars[tab_idx]['IN_FILE_PATH'].set(os.path.normpath(path))

    def browse_output(self, tab_idx, is_left):
        key = 'OUT_L_FILE_PATH' if is_left else 'OUT_R_FILE_PATH'
        current = self.entry_vars[tab_idx][key].get().strip()
        initial_dir = os.path.dirname(current) if current and os.path.isdir(os.path.dirname(current)) else os.getcwd()
        side = 'L' if is_left else 'R'
        initial_file = os.path.basename(current) if current else f"FX{tab_idx+1}_{side}.raw"
        if path := filedialog.asksaveasfilename(
                title=f"Save {'Left' if is_left else 'Right'} Output .raw File",
                defaultextension=".raw",
                initialdir=initial_dir,
                initialfile=initial_file,
                filetypes=[("Raw audio", "*.raw"), ("All files", "*.*")],
                parent=self.root):
            self.entry_vars[tab_idx][key].set(os.path.normpath(path))

    # ====================== UPDATED GENERATE (NOW IDENTICAL TO AutomatedRun.py) ======================
    def generate(self):
        cobol_path = self.cobol_path_var.get()
        if not cobol_path or not os.path.isfile(cobol_path):
            messagebox.showerror("Error", "Please select the COMOL-FX COBOL source file.")
            return

        try:
            with open(cobol_path, 'r', encoding='utf-8') as f:
                original_lines = f.readlines()
        except Exception as e:
            messagebox.showerror("Error", f"Could not read COBOL file:\n{str(e)}")
            return

        cobol_dir = os.path.normpath(os.path.dirname(cobol_path))
        path_limit = 256

        # === EXACT SAME ENVIRONMENT SETUP AS AutomatedRun.py ===
        if self.settings['path']:
            os.environ['PATH'] = self.settings['path'] + ';' + os.environ.get('PATH', '')
        os.environ['COB_CONFIG_DIR'] = self.settings['cob_config_dir']
        os.environ['COB_COPY_DIR'] = self.settings['cob_copy_dir']
        os.environ['COB_INCLUDE_PATH'] = self.settings['cob_include_path']
        os.environ['COB_LIB_PATH'] = self.settings['cob_lib_path']

        output_dir = self.output_dir_var.get().strip() or (self.settings.get('output_directory') or cobol_dir)
        os.makedirs(output_dir, exist_ok=True)

        generated = 0
        for tab_idx in range(4):
            values = {var: self.entry_vars[tab_idx][var].get().strip() for var in self.variables
                      if self.entry_vars[tab_idx][var].get().strip()}

            if not values.get('OUT_L_FILE_PATH') or not values.get('OUT_R_FILE_PATH'):
                continue

            in_path = values.get('IN_FILE_PATH', '')
            out_l_path = values.get('OUT_L_FILE_PATH', '')
            out_r_path = values.get('OUT_R_FILE_PATH', '')

            if max(len(in_path), len(out_l_path), len(out_r_path)) > path_limit:
                messagebox.showwarning("Warning", f"Path too long for preset {tab_idx+1} (max {path_limit} chars). Skipping.")
                continue

            new_lines = original_lines[:]

            # === FX-specific robust replacement (kept from your original) ===
            for var in [v for v in self.variables if v.startswith('FX-')]:
                new_val = values.get(var)
                if new_val:
                    for i, line in enumerate(new_lines):
                        if var in line and 'VALUE' in line.upper():
                            if not new_val.endswith('.'):
                                new_val += '.'
                            new_lines[i] = re.sub(
                                r'(VALUE\s+)[-\d.]+',
                                lambda m: m.group(1) + new_val,
                                line,
                                flags=re.IGNORECASE
                            )
                            break

            # === Robust file path replacement (forward slashes) ===
            for k in range(len(new_lines) - 1):
                line_upper = new_lines[k].upper().strip()
                if 'SELECT IN-FILE ASSIGN TO' in line_upper and in_path:
                    indent = new_lines[k + 1][:len(new_lines[k + 1]) - len(new_lines[k + 1].lstrip())]
                    safe_path = in_path.replace('\\', '/')
                    new_lines[k + 1] = f'{indent}"{safe_path}"\n'
                elif 'SELECT OUT-FILE-L ASSIGN TO' in line_upper and out_l_path:
                    indent = new_lines[k + 1][:len(new_lines[k + 1]) - len(new_lines[k + 1].lstrip())]
                    safe_path = out_l_path.replace('\\', '/')
                    new_lines[k + 1] = f'{indent}"{safe_path}"\n'
                elif 'SELECT OUT-FILE-R ASSIGN TO' in line_upper and out_r_path:
                    indent = new_lines[k + 1][:len(new_lines[k + 1]) - len(new_lines[k + 1].lstrip())]
                    safe_path = out_r_path.replace('\\', '/')
                    new_lines[k + 1] = f'{indent}"{safe_path}"\n'

            output_cbl = os.path.normpath(os.path.join(output_dir, f"FX{tab_idx+1}.cbl"))
            try:
                with open(output_cbl, 'w', encoding='utf-8') as f:
                    f.writelines(new_lines)
                generated += 1

                if self.compile_run_var.get():
                    # === EXACT SAME COMPILATION BLOCK AS AutomatedRun.py ===
                    try:
                        compiler_path = self.settings['compiler_path']
                        if not os.path.isfile(compiler_path):
                            raise FileNotFoundError(f"Compiler not found at {compiler_path}")

                        exec_name = os.path.join(output_dir, f"FX{tab_idx+1}.exe")
                        compile_cmd = [compiler_path, '-x', '-o', exec_name, output_cbl]
                        result = subprocess.run(compile_cmd, capture_output=True, text=True)

                        if result.returncode != 0:
                            compiler_output = (result.stderr or result.stdout or "No output from compiler.").strip()
                            raise RuntimeError(f"Compiler exited with code {result.returncode}:\n\n{compiler_output}")

                        if self.settings['copy_runtime_dlls'].lower() == 'yes':
                            runtime_dlls = ['libcob.dll', 'gmp.dll', 'ncurses.dll']
                            for dll in runtime_dlls:
                                src_dll = os.path.join(self.settings['cob_lib_path'], dll)
                                if os.path.isfile(src_dll):
                                    shutil.copy(src_dll, os.path.join(output_dir, dll))

                        run_cmd = f'cmd /k "cd /d "{output_dir}" & "{exec_name}""'
                        subprocess.Popen(run_cmd, shell=True)

                    except Exception as e:
                        messagebox.showerror("Compile/Run Error",
                                             f"Preset {tab_idx+1} generated but failed to compile/run:\n{str(e)}")

            except Exception as e:
                messagebox.showerror("Write Error", f"Failed to write preset {tab_idx+1}:\n{str(e)}")

        if generated == 0:
            messagebox.showwarning("Nothing Generated",
                                   "No presets were created.\nMake sure at least one preset has both LEFT and RIGHT output paths.")
        else:
            self.save_session()
            messagebox.showinfo("Success", f"{generated} preset(s) generated successfully.")


if __name__ == "__main__":
    App()
