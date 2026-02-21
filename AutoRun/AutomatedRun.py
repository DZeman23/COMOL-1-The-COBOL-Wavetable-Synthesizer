import tkinter as tk
from tkinter import filedialog, messagebox, ttk
import os
import platform
import subprocess
import configparser
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
        self.root.title("COBOL Waveform Configurator")
        self.root.geometry("960x740")

        self.config_file = 'config_waveform.ini'
        self.settings = self.load_settings()

        # COBOL file selection
        cobol_frame = tk.Frame(self.root)
        cobol_frame.pack(fill='x', padx=15, pady=12)

        tk.Label(cobol_frame, text="COBOL Source File:", font=('Helvetica', 10, 'bold')).pack(side='left', padx=(0, 10))

        self.cobol_path_var = tk.StringVar()
        path_entry = tk.Entry(cobol_frame, textvariable=self.cobol_path_var, state='readonly', width=80)
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
        out_dir_entry = tk.Entry(out_dir_frame, textvariable=self.output_dir_var, state='readonly', width=80)
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
            'WAVE-SOURCE-CHOICE': "1: Generate Internal Sine Wave, 2: Load from Input.raw",
            'USER-OCTAVE': "0-6",
            'USER-NOTE': "0-11 (0=C, 1=C#, ..., 11=B)",
            'RAW-PATTERN-INPUT': "Pattern string e.g. '3331' (1=None, 2=Linear, 3=Sinc)",
            'OPERATION-MODE': "1: Digital (Clean), 2: Analogue (Dirty)",
            'BIAS-INTENSITY': "0-100",
            'USER-DRIVE-IN': "1-10",
            'USER-DRIFT-IN': "0-100",
            'USER-CRUSH-IN': "1-2000",
            'FILTER-TYPE-CHOICE': "'1': LPF, '2': HPF, '3': BPF",
            'KNOB-POSITION': "0-100 (Cutoff Knob)",
            'Q-KNOB-POSITION': "0-100 (Resonance Knob)",
            'T1': "Attack Time (e.g. 0.1)",
            'L1': "Attack Level 0-100",
            'T2': "Decay1 Time (e.g. 0.5)",
            'L2': "Break Level 0-100",
            'T3': "Decay2 Time (e.g. 1.0)",
            'L3': "Sustain Level 0-100",
            'T-SUSTAIN': "Sustain Duration (e.g. 2.0)",
            'T4': "Release Time (e.g. 1.0)",
            'CUT-T1': "Cutoff Attack Time (e.g. 0.1)",
            'CUT-L1': "Cutoff Attack Level -50 to 50",
            'CUT-T2': "Cutoff Decay1 Time (e.g. 0.5)",
            'CUT-L2': "Cutoff Break Level -50 to 50",
            'CUT-T3': "Cutoff Decay2 Time (e.g. 1.0)",
            'CUT-L3': "Cutoff Sustain Level -50 to 50",
            'CUT-T-SUSTAIN': "Cutoff Sustain Duration (e.g. 2.0)",
            'CUT-T4': "Cutoff Release Time (e.g. 1.0)",
            'TVF-DEPTH': "-100 to 100 (Envelope Depth)",
            'IN_FILE_PATH': "Path to input raw file (for WAVE-SOURCE-CHOICE=2)",
            'OUT_FILE_PATH': "Path to output raw file"
        }

        self.variables = list(self.var_descriptions.keys())

        vars_per_column = (len(self.variables) + 2) // 3
        col_groups = [
            self.variables[0:vars_per_column],
            self.variables[vars_per_column:vars_per_column*2],
            self.variables[vars_per_column*2:]
        ]

        self.tabs = []
        self.entry_vars = [{} for _ in range(4)]
        self.tooltips = [{} for _ in range(4)]

        for i in range(4):
            tab = ttk.Frame(self.notebook)
            self.notebook.add(tab, text=f"Waveform {i+1}")
            self.tabs.append(tab)
            self.entry_vars[i] = {}
            self.tooltips[i] = {}

            for col_idx, var_group in enumerate(col_groups):
                for row_idx, var in enumerate(var_group):
                    row = row_idx
                    col_label = col_idx * 3
                    col_entry = col_label + 1
                    col_button = col_label + 2

                    tk.Label(tab, text=var + ":", width=22, anchor='e').grid(
                        row=row, column=col_label, padx=(10, 4), pady=3, sticky='e')

                    self.entry_vars[i][var] = tk.StringVar()
                    entry = tk.Entry(tab, textvariable=self.entry_vars[i][var], width=28)
                    entry.grid(row=row, column=col_entry, padx=(4, 8), pady=3, sticky='ew')

                    self.tooltips[i][var] = Tooltip(entry, self.var_descriptions[var])

                    if var in ('IN_FILE_PATH', 'OUT_FILE_PATH'):
                        if var == 'IN_FILE_PATH':
                            cmd = lambda idx=i: self.browse_input(idx)
                        else:
                            cmd = lambda idx=i: self.browse_output(idx)
                        btn = tk.Button(tab, text="Browse", command=cmd, width=8)
                        btn.grid(row=row, column=col_button, padx=(4, 12), pady=3, sticky='w')

            for c in range(3):
                tab.columnconfigure(c * 3 + 1, weight=1)

            # Copy-to row at the bottom of each tab
            copy_row = vars_per_column + 1
            copy_frame = tk.Frame(tab)
            copy_frame.grid(row=copy_row, column=0, columnspan=9, pady=(10, 4), padx=10, sticky='w')
            tk.Label(copy_frame, text="Copy to:", font=('Helvetica', 9, 'bold')).pack(side='left', padx=(0, 8))
            for j in range(4):
                if j != i:
                    btn = tk.Button(
                        copy_frame,
                        text=f"Waveform {j+1}",
                        command=lambda src=i, dst=j: self.copy_waveform(src, dst),
                        width=10
                    )
                    btn.pack(side='left', padx=4)

        btn_frame = tk.Frame(self.root)
        btn_frame.pack(pady=15)

        btn_settings = tk.Button(
            btn_frame,
            text="Compiler Settings",
            command=self.open_settings,
            font=('Helvetica', 11, 'bold'),
            padx=20,
            pady=10
        )
        btn_settings.pack(side='left', padx=10)

        btn_save_tmpl = tk.Button(
            btn_frame,
            text="Save Template",
            command=self.save_template,
            font=('Helvetica', 11, 'bold'),
            padx=20,
            pady=10
        )
        btn_save_tmpl.pack(side='left', padx=10)

        btn_load_tmpl = tk.Button(
            btn_frame,
            text="Load Template",
            command=self.load_template,
            font=('Helvetica', 11, 'bold'),
            padx=20,
            pady=10
        )
        btn_load_tmpl.pack(side='left', padx=10)

        btn_gen = tk.Button(
            btn_frame,
            text="Generate All Waveforms",
            command=self.generate,
            font=('Helvetica', 11, 'bold'),
            padx=20,
            pady=10
        )
        btn_gen.pack(side='left', padx=10)

        self.load_session()
        self.root.protocol("WM_DELETE_WINDOW", self.on_close)
        self.root.mainloop()

    def save_template(self):
        path = filedialog.asksaveasfilename(
            title="Save Waveform Template",
            defaultextension=".ini",
            filetypes=[("Template files", "*.ini"), ("Text files", "*.txt"), ("All files", "*.*")],
            parent=self.root
        )
        if not path:
            return
        config = configparser.ConfigParser()
        config['Meta'] = {
            'cobol_path': self.cobol_path_var.get(),
            'output_dir': self.output_dir_var.get(),
        }
        for i in range(4):
            section = f'Waveform_{i+1}'
            config[section] = {}
            for var in self.variables:
                config[section][var] = self.entry_vars[i][var].get()
        with open(path, 'w') as f:
            f.write("# COBOL Waveform Configurator - Template File\n")
            f.write("# Edit values below and load back with 'Load Template'.\n")
            f.write("# Each [Waveform_N] section corresponds to one tab.\n\n")
            config.write(f)
        messagebox.showinfo("Template Saved", f"Template saved to:\n{path}")

    def load_template(self):
        path = filedialog.askopenfilename(
            title="Load Waveform Template",
            filetypes=[("Template files", "*.ini"), ("Text files", "*.txt"), ("All files", "*.*")],
            parent=self.root
        )
        if not path:
            return
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
            section = f'Waveform_{i+1}'
            if section in config:
                for var in self.variables:
                    val = config[section].get(var.lower(), config[section].get(var, ''))
                    self.entry_vars[i][var].set(val)
                loaded += 1
        if loaded:
            messagebox.showinfo("Template Loaded", f"Loaded {loaded} waveform(s) from:\n{path}")
        else:
            messagebox.showwarning("Load Failed", "No [Waveform_N] sections found in that file.")

    def on_close(self):
        self.save_session()
        self.root.destroy()

    def load_settings(self):
        config = configparser.ConfigParser()
        defaults = {
            'compiler_path': 'cobc.exe',
            'path': '',
            'cob_config_dir': '',
            'cob_copy_dir': '',
            'cob_include_path': '',
            'cob_lib_path': '',
            'vcvarsall_path': '',
            'output_directory': 'bin',
            'copy_runtime_dlls': 'no'
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
        # Preserve existing session data when saving compiler settings
        if os.path.exists(self.config_file):
            existing = configparser.ConfigParser()
            existing.read(self.config_file)
            if 'SESSION' in existing:
                config['SESSION'] = dict(existing['SESSION'])
        with open(self.config_file, 'w') as f:
            config.write(f)

    def save_session(self):
        config = configparser.ConfigParser()
        # Preserve existing compiler settings
        if os.path.exists(self.config_file):
            config.read(self.config_file)
        session = {
            'cobol_path': self.cobol_path_var.get(),
            'output_dir': self.output_dir_var.get(),
        }
        for i in range(4):
            for var in self.variables:
                val = self.entry_vars[i][var].get()
                session[f'tab{i}_{var}'] = val
        config['SESSION'] = session
        with open(self.config_file, 'w') as f:
            config.write(f)

    def load_session(self):
        if not os.path.exists(self.config_file):
            return
        config = configparser.ConfigParser()
        config.read(self.config_file)
        if 'SESSION' not in config:
            return
        session = config['SESSION']
        cobol_path = session.get('cobol_path', '')
        if cobol_path and os.path.isfile(cobol_path):
            self.cobol_path_var.set(cobol_path)
        output_dir = session.get('output_dir', '')
        if output_dir:
            self.output_dir_var.set(output_dir)
        for i in range(4):
            for var in self.variables:
                val = session.get(f'tab{i}_{var}', '')
                if val:
                    self.entry_vars[i][var].set(val)

    def open_settings(self):
        settings_win = tk.Toplevel(self.root)
        settings_win.title("Compiler Settings")
        settings_win.geometry("820x580")
        settings_win.resizable(True, True)
        settings_win.transient(self.root)
        settings_win.grab_set()          # modal

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
            label_text = key.replace('_', ' ').title() + ":"
            tk.Label(settings_win, text=label_text, font=('Helvetica', 10)).grid(
                row=row, column=0, padx=(30, 12), pady=10, sticky='e')

            entry_vars[key] = tk.StringVar(value=default)
            entry = tk.Entry(settings_win, textvariable=entry_vars[key], width=75, font=('Helvetica', 9))
            entry.grid(row=row, column=1, padx=(0, 30), pady=10, sticky='ew')

            Tooltip(entry, descriptions.get(key, "No description available."))
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

        tk.Button(btn_frame, text="Cancel", width=14, padx=20, pady=8,
                  command=settings_win.destroy).pack(side='right', padx=12)
        tk.Button(btn_frame, text="Save Settings", width=16, font=('Helvetica', 10, 'bold'),
                  padx=25, pady=8, command=save_and_close).pack(side='right', padx=12)

    def copy_waveform(self, src, dst):
        for var in self.variables:
            self.entry_vars[dst][var].set(self.entry_vars[src][var].get())
        self.notebook.select(dst)
        messagebox.showinfo("Copied", f"Waveform {src+1} parameters copied to Waveform {dst+1}.")

    def select_output_dir(self):
        path = filedialog.askdirectory(title="Select Output Directory")
        if path:
            self.output_dir_var.set(os.path.normpath(path))

    def select_cobol(self):
        path = filedialog.askopenfilename(filetypes=[("COBOL files", "*.cbl *.cob")])
        if path:
            self.cobol_path_var.set(path)

    def browse_input(self, tab_idx):
        try:
            current = self.entry_vars[tab_idx]['IN_FILE_PATH'].get().strip()
            initial_dir = os.path.dirname(current) if current and os.path.isdir(os.path.dirname(current)) else os.getcwd()

            path = filedialog.askopenfilename(
                title="Select Input .raw File",
                initialdir=initial_dir,
                filetypes=[("Raw audio", "*.raw"), ("All files", "*.*")],
                parent=self.root
            )

            path = str(path).strip() if path else ""
            print(f"[DEBUG INPUT tab {tab_idx+1}] Dialog returned: {repr(path)} (len: {len(path)})")

            if path and os.path.isfile(path):
                self.entry_vars[tab_idx]['IN_FILE_PATH'].set(os.path.normpath(path))
                print(f"[SUCCESS INPUT] Set IN_FILE_PATH tab {tab_idx+1} → {path}")
            elif path:
                print(f"[WARNING] Selected input path not a file: {path}")
            else:
                print("[INFO] Input selection cancelled")

        except Exception as e:
            print(f"[ERROR browse_input tab {tab_idx+1}]: {str(e)}")

    def browse_output(self, tab_idx):
        try:
            current = self.entry_vars[tab_idx]['OUT_FILE_PATH'].get().strip()
            initial_dir = os.path.dirname(current) if current and os.path.isdir(os.path.dirname(current)) else os.getcwd()
            initial_file = os.path.basename(current) if current else f"waveform_{tab_idx+1}.raw"

            path = filedialog.asksaveasfilename(
                title="Save Output .raw File",
                defaultextension=".raw",
                initialdir=initial_dir,
                initialfile=initial_file,
                filetypes=[("Raw audio", "*.raw"), ("All files", "*.*")],
                parent=self.root
            )

            path = str(path).strip() if path else ""
            print(f"[DEBUG OUTPUT tab {tab_idx+1}] Dialog returned: {repr(path)} (len: {len(path)})")

            if path:
                path = os.path.normpath(path)
                self.entry_vars[tab_idx]['OUT_FILE_PATH'].set(path)
                print(f"[SUCCESS OUTPUT] Set OUT_FILE_PATH tab {tab_idx+1} → {path}")
            else:
                print("[INFO] Output selection cancelled")

        except Exception as e:
            print(f"[ERROR browse_output tab {tab_idx+1}]: {str(e)}")

    def generate(self):
        cobol_path = self.cobol_path_var.get()
        if not cobol_path:
            messagebox.showerror("Error", "Please select the base COBOL source file.")
            return

        try:
            with open(cobol_path, 'r', encoding='utf-8') as f:
                original_lines = f.readlines()
        except Exception as e:
            messagebox.showerror("Error", f"Could not read COBOL file:\n{str(e)}")
            return

        cobol_dir = os.path.normpath(os.path.dirname(cobol_path))
        path_limit = 256

        # Set environment variables from settings
        if self.settings['path']:
            os.environ['PATH'] = self.settings['path'] + ';' + os.environ.get('PATH', '')
        os.environ['COB_CONFIG_DIR'] = self.settings['cob_config_dir']
        os.environ['COB_COPY_DIR'] = self.settings['cob_copy_dir']
        os.environ['COB_INCLUDE_PATH'] = self.settings['cob_include_path']
        os.environ['COB_LIB_PATH'] = self.settings['cob_lib_path']

        generated = 0
        for tab_idx in range(4):
            values = {var: self.entry_vars[tab_idx][var].get().strip() for var in self.variables if self.entry_vars[tab_idx][var].get().strip()}

            if not values.get('OUT_FILE_PATH'):
                continue

            in_path = values.get('IN_FILE_PATH', '')
            out_path = values.get('OUT_FILE_PATH', '')

            if len(in_path) > path_limit or len(out_path) > path_limit:
                messagebox.showwarning("Warning", f"Path too long for waveform {tab_idx+1} (max {path_limit} chars). Skipping.")
                continue

            new_lines = original_lines[:]
            i = 0
            while i < len(new_lines):
                line = new_lines[i].strip()
                if line.startswith('* USER_INPUT:'):
                    var = line.split(':', 1)[1].strip()
                    if var in values and values[var]:
                        i += 1
                        if i < len(new_lines):
                            move_line = new_lines[i].strip()
                            if move_line.startswith('MOVE'):
                                # Use ' TO ' with spaces to avoid matching 'TO' inside variable names
                                to_idx = move_line.find(' TO ')
                                if to_idx != -1:
                                    old_val_part = move_line[5:to_idx]  # text between 'MOVE ' and ' TO '
                                    remainder = move_line[to_idx:]       # ' TO VAR-NAME.' portion
                                    new_val = values[var]
                                    if old_val_part.startswith('"') or old_val_part.startswith("'"):
                                        quote = old_val_part[0]
                                        new_val = f"{quote}{new_val}{quote}"
                                    # Preserve the original line's indentation, reconstruct cleanly
                                    indent = new_lines[i][:len(new_lines[i]) - len(new_lines[i].lstrip())]
                                    line_ending = '\n' if new_lines[i].endswith('\n') else ''
                                    new_lines[i] = f"{indent}MOVE {new_val}{remainder}{line_ending}"
                i += 1

            import re
            for k in range(len(new_lines)):
                if 'SELECT IN-FILE ASSIGN TO' in new_lines[k] and in_path:
                    # Path is on the next line as a quoted string
                    if k + 1 < len(new_lines):
                        new_lines[k + 1] = re.sub(r'"[^"]*"', lambda m: '"' + in_path + '"', new_lines[k + 1], count=1)
                if 'SELECT OUT-FILE ASSIGN TO' in new_lines[k] and out_path:
                    if k + 1 < len(new_lines):
                        new_lines[k + 1] = re.sub(r'"[^"]*"', lambda m: '"' + out_path + '"', new_lines[k + 1], count=1)

            output_dir = self.output_dir_var.get().strip() or (self.settings['output_directory'] if self.settings['output_directory'] else cobol_dir)
            output_cbl = os.path.normpath(os.path.join(output_dir, f"waveform_{tab_idx+1}.cbl"))
            try:
                with open(output_cbl, 'w', encoding='utf-8') as f:
                    f.writelines(new_lines)
                generated += 1

                if self.compile_run_var.get():
                    try:
                        compiler_path = self.settings['compiler_path']
                        if not os.path.isfile(compiler_path):
                            raise FileNotFoundError(f"Compiler not found at {compiler_path}")

                        exec_name = os.path.join(output_dir, f"waveform_{tab_idx+1}.exe")
                        compile_cmd = [compiler_path, '-x', '-o', exec_name, output_cbl]
                        result = subprocess.run(compile_cmd, capture_output=True, text=True)
                        if result.returncode != 0:
                            compiler_output = (result.stderr or result.stdout or "No output from compiler.").strip()
                            raise RuntimeError(f"Compiler exited with code {result.returncode}:\n\n{compiler_output}")

                        # If copy runtime dlls
                        if self.settings['copy_runtime_dlls'].lower() == 'yes':
                            # Assume runtime dlls are in the lib path or bin; copy common ones
                            runtime_dlls = ['libcob.dll', 'gmp.dll', 'ncurses.dll']  # Adjust based on your installation
                            for dll in runtime_dlls:
                                src_dll = os.path.join(self.settings['cob_lib_path'], dll)
                                if os.path.isfile(src_dll):
                                    dest_dll = os.path.join(output_dir, dll)
                                    shutil.copy(src_dll, dest_dll)

                        # Run in new Command Prompt
                        run_cmd = f'cmd /k "cd /d "{output_dir}" & "{exec_name}""'
                        subprocess.Popen(run_cmd, shell=True)

                    except Exception as e:
                        messagebox.showerror("Compile/Run Error", f"Waveform {tab_idx+1} generated but failed to compile/run:\n{str(e)}")
            except Exception as e:
                messagebox.showerror("Write Error", f"Failed to write waveform {tab_idx+1}:\n{str(e)}")

        if generated == 0:
            messagebox.showwarning("Nothing Generated", "No waveforms were created.\nMake sure at least one tab has a valid output path.")
        else:
            self.save_session()
            messagebox.showinfo("Success", f"{generated} waveform(s) generated successfully.")

if __name__ == "__main__":
    App()
