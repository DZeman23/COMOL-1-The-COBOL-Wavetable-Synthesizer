# COBOL Waveform Configurator

A Python GUI tool for configuring and generating COBOL-based waveform synthesis programs. Each waveform is defined by filling in a set of parameters, which the tool injects into a COBOL template — producing ready-to-compile source files that can optionally be compiled and run automatically.

---

## Requirements

- Python 3.x with Tkinter (standard library)
- [GnuCOBOL](https://gnucobol.sourceforge.io/) installed and accessible

---

## Project Folder Setup

Before running the tool, your project folder must be structured correctly.

### 1. Create a `bin` folder

Inside your project directory (the folder containing your COBOL template file), create a folder named `bin`:

```
YourProject/
├── bin/                  ← create this
├── CML1_Harcode.cbl      ← your COBOL template
├── AutomatedRun.py
├── NOTE-SELECTOR.CPY
├── NOTE-FREQ-WS.CPY
├── ... (other copybooks)
```

This is where the tool will write the generated `.cbl` files and compile the `.exe` executables into.

### 2. Point your copybooks to the `bin` folder

GnuCOBOL needs to know where to find your `.CPY` copybook files at compile time. In **Compiler Settings**, set the **Cob copy dir** field to the absolute path of your project folder — the folder that *contains* `bin`, not `bin` itself. For example:

```
C:\Users\YourName\Desktop\YourProject
```

This tells GnuCOBOL to look in that directory when it encounters `COPY` statements in the generated source, which is where all your `.CPY` files live.

---

## How Compilation Works

When you click **Generate All Waveforms**, the tool does the following for each configured waveform tab:

1. **Reads** your COBOL template file (`.cbl`)
2. **Injects** the values you entered in the UI by finding `* USER_INPUT: VARIABLE-NAME` comment markers in the COBOL source and replacing the `MOVE` value on the line immediately below each one
3. **Writes** the modified source to `bin/waveform_N.cbl`
4. **Compiles** it using GnuCOBOL (`cobc -x`) into `bin/waveform_N.exe`
5. **Runs** the executable in a new Command Prompt window, which processes the audio and writes the output `.raw` file

The original template is never modified — a fresh copy is made for each waveform.

---

## Running the Tool

```bash
python AutomatedRun.py
```

---

## Interface Overview

### Top Bar

| Control | Description |
|---|---|
| **COBOL Source File** | Select your `.cbl` template file |
| **Output Directory** | Select the `bin` folder inside your project |
| **Compile & run checkbox** | If checked, automatically compiles and runs each generated waveform after writing |

### Waveform Tabs (1–4)

Each tab represents one independent waveform. Fill in the parameters you want to override from the template defaults — any field left blank will keep the value already written in the template.

#### Wave Source
| Parameter | Description |
|---|---|
| `WAVE-SOURCE-CHOICE` | `1` = generate internal sine wave, `2` = load waveform from input `.raw` file |
| `IN_FILE_PATH` | Path to the input `.raw` file (only used when `WAVE-SOURCE-CHOICE = 2`) |
| `OUT_FILE_PATH` | Path where the output `.raw` file will be written **(required — tab is skipped if blank)** |

#### Pitch
| Parameter | Description |
|---|---|
| `USER-OCTAVE` | Octave `0–6` |
| `USER-NOTE` | Note `0–11` (0=C, 1=C#, 2=D, 3=D#, 4=E, 5=F, 6=F#, 7=G, 8=G#, 9=A, 10=A#, 11=B) |

#### Interpolation
| Parameter | Description |
|---|---|
| `RAW-PATTERN-INPUT` | String of digits defining the interpolation pattern per sample step. `1` = nearest neighbour (raw), `2` = linear, `3` = sinc. Example: `"3331"` cycles through sinc, sinc, sinc, raw |

#### Filter
| Parameter | Description |
|---|---|
| `OPERATION-MODE` | `1` = Digital (clean), `2` = Analogue (adds drift, drive, bias) |
| `FILTER-TYPE-CHOICE` | `1` = Low Pass, `2` = High Pass, `3` = Band Pass |
| `KNOB-POSITION` | Filter cutoff frequency `0–100` |
| `Q-KNOB-POSITION` | Filter resonance `0–100` |

#### Analogue Parameters *(only active when `OPERATION-MODE = 2`)*
| Parameter | Description |
|---|---|
| `BIAS-INTENSITY` | Asymmetric clipping intensity `0–100` |
| `USER-DRIVE-IN` | Soft saturation drive `1–10` |
| `USER-DRIFT-IN` | Random pitch/amplitude drift `0–100` |
| `USER-CRUSH-IN` | Bit crush factor `1–2000` |

#### Volume Envelope (JD800-style, 5-stage)
| Parameter | Description |
|---|---|
| `T1` / `L1` | Attack time (seconds) / Attack peak level `0–100` |
| `T2` / `L2` | Decay 1 time / Break level `0–100` |
| `T3` / `L3` | Decay 2 time / Sustain level `0–100` |
| `T-SUSTAIN` | Sustain hold duration (seconds) |
| `T4` | Release time (seconds) |

#### Filter Envelope (TVF)
Controls how the filter cutoff moves over the note's duration. Values are offsets from the base `KNOB-POSITION`.

| Parameter | Description |
|---|---|
| `CUT-T1` / `CUT-L1` | Attack time / Attack cutoff offset `-50–50` |
| `CUT-T2` / `CUT-L2` | Decay 1 time / Break cutoff offset |
| `CUT-T3` / `CUT-L3` | Decay 2 time / Sustain cutoff offset |
| `CUT-T-SUSTAIN` | Sustain hold duration |
| `CUT-T4` | Release time |
| `TVF-DEPTH` | Overall envelope depth `-100–100` |

### Copy Parameters

At the bottom of each tab is a **Copy to:** row. Clicking a waveform button copies all parameters from the current tab into that tab, then switches to it automatically. Useful for creating variations from a base patch.

### Compiler Settings

Accessed via the **Compiler Settings** button. These are saved to `config.ini` in the same folder as `AutomatedRun.py`.

| Setting | Description |
|---|---|
| `compiler_path` | Full path to `cobc.exe` (e.g. `C:\GnuCOBOL\bin\cobc.exe`) |
| `cob_copy_dir` | Directory containing your `.CPY` copybook files (your project folder) |
| `cob_lib_path` | GnuCOBOL lib directory (e.g. `C:\GnuCOBOL\lib`) |
| `output_directory` | Fallback output directory if none is selected in the UI |
| `copy_runtime_dlls` | `yes` / `no` — copies GnuCOBOL runtime DLLs into the output folder |

---

## Session Persistence

All field values, file paths, and the output directory are automatically saved when you close the window or after a successful generate. They are restored the next time you open the tool.

Settings and session data are stored in `config.ini`. **This file should be added to `.gitignore`** as it will contain paths specific to your machine:

```
# .gitignore
config.ini
bin/
```

---

## Output Files

After a successful run, your `bin` folder will contain:

```
bin/
├── waveform_1.cbl      ← generated COBOL source for tab 1
├── waveform_1.exe      ← compiled executable
├── waveform_2.cbl
├── waveform_2.exe
└── ...
```

The `.exe` files are standalone programs. Running one will process the audio and write the `.raw` output file to the path specified in `OUT_FILE_PATH`.
