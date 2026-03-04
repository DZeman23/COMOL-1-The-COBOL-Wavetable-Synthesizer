# COMOL-1: The COBOL Wavetable Synthesizer

```text
  ______    ______   __       __   ______   __        __   
 /      \  /      \ |  \     /  \ /      \ |  \     _/  \  
|  $$$$$$\|  $$$$$$\| $$\   /  $$|  $$$$$$\| $$    |   $$  
| $$   \$$| $$  | $$| $$$\ /  $$$| $$  | $$| $$ ____\$$$$  
| $$      | $$  | $$| $$$$\  $$$$| $$  | $$| $$|    \| $$  
| $$   __ | $$  | $$| $$\$$ $$ $$| $$  | $$| $$ \$$$$$|$$  
| $$__/  \| $$__/ $$| $$ \$$$| $$| $$__/ $$| $$___  _| $$_ 
 \$$    $$ \$$    $$| $$  \$ | $$ \$$    $$| $$   \|   $$ \
  \$$$$$$   \$$$$$$  \$$      \$$  \$$$$$$  \$$$$$$ \$$$$$$
     .-----.         .-----.   .--------------. 
 100 |  |  |     100 |=====|   |      /\      | 
   - |  |  |       - |=====|   |     /..\     | 
   - |  |  |       - |=====|   |    /::::\    | 
   - |  |  |       - |=====|   |      UP      | 
   - |  |  |       - |  |  |   |______________| 
   - |  |  |       - |  |  |   |  __________  | 
  50 |  |  |      50 |  |  |   |     DOWN     | 
   - |  |  |       - |  |  |   |    \::::/    | 
   - |  |  |       - |  |  |   |     \../     | 
   - |  |  |       - |  |  |   |      \/      | 
   - |=====|       - |  |  |   '--------------' 
   - |=====|       - |  |  |                    
   - |=====|       - |  |  |                    
   0 |=====|       0 |  |  |                    
     '-----'         '-----'                    
```

A highly flexible and versatile wavetable synthesizer written entirely in COBOL. The long-term aim of this project is to fully emulate the time variant control system of the Roland JD800, but with greater user customizability in the selection and editing of waveforms.

---

## What This Program Does

1. Generates or loads waveforms into a wavetable
2. Applies pitch shifting via variable-speed playback
3. Processes audio through Time Variant JD800-style digital biquad filters
4. Simulates analog characteristics (warmth, drift, saturation, bit reduction)
5. Shapes amplitude with Time Variant envelopes
6. Uses high-quality interpolation to prevent aliasing
7. Outputs processed audio as raw 16-bit PCM data
8. Up to four voices can be batch-generated simultaneously via the AutoRun GUI
9. An optional FX module applies a full stereo effects chain to the output

---

## Why COBOL?

**COBOL excels at fixed-point arithmetic.** Audio DSP requires precise decimal calculations for filter coefficients, envelope slopes, and sample interpolation. COBOL's `PICTURE` clauses with explicit decimal placement (like `PIC S9(5)V9(10)`) provide predictable, reproducible math without floating-point rounding errors that plague C/C++. Furthermore, because of the high degree of precision involved in floating point languages such as C++, it is easy to make a sterile sounding instrument. In certain cases, as I will demonstrate, intentionally inducing truncation through fixed-point allows for a degree of imperfection well suited to digital-analogue simulation.

**Packed decimal (COMP-3) is perfect for wavetables.** Storing 2048 samples with 17 decimal places is an ideal job for COBOL. COBOL's packed decimal stores digits efficiently while maintaining the precision needed for high-quality audio.

**Business logic is surprisingly intuitive for handling raw audio stored in binary.** Audio processing is surprisingly similar to batch processing: read records (samples), apply transformations (filters/envelopes), write output. COBOL's procedural structure with named paragraphs creates self-documenting signal flow.

**It's an exercise in algorithm design.** Modern audio software assumes you need C++ and SIMD optimization. This project proves that algorithm design matters more than language choice. A well-structured COBOL program with proper fixed-point math can generate professional-quality audio — it just challenges our preconceptions about "appropriate" tools.

**Is COBOL the best tool for the job?** Well uh... no admittedly not. I intentionally chose an unintuitive approach to the task in hopes that it would force me into a "first principles" way of thinking about the task. But the end product is a surprisingly nice sounding virtual synth. I hope that the way that I have figured out how to process binary for audio processing will open some new possibilities for poor old dusty COBOL. Sometimes the best way to deeply understand a problem domain is to approach it from a completely unexpected angle. Writing a virtual analog synthesizer in a 65-year-old business language forces you to think carefully about every calculation, every data structure, every assumption about how audio synthesis "should" work.

---

## Special Thanks

This project would not have been possible without **Curtis Roads**' exceptional book *The Computer Music Tutorial*. The envelope design owes its form to the Roland JD800 synth, from which I derived its structure.

---

## External Software

**Mandatory:** Audacity, GnuCOBOL Compiler.
Audacity is needed to handle 16-bit headerless PCM files. Technically, any software capable of this works fine, but Audacity is free and is what the guide is based on.

**Optional:** StupidSimpleSampler, Savihost.
These are for mapping the output to a keyboard.

---

## Repository Structure

```
COMOL-1-The-COBOL-Wavetable-Synthesizer/
├── CML1_Harcode.cbl          ← main COBOL synth template (for manual use)
├── [copybooks].CPY
├── AutoRun/
│   ├── AutomatedRun.py       ← Python GUI for batch voice generation
│   └── CML1_Harcode.cbl      ← use THIS version with AutoRun (not the root one)
└── FX-Module/
    ├── COMOLFX.cbl           ← stereo FX chain processor
    └── FX-RUN.py             ← Python GUI for FX parameters
```

> **Important:** The AutoRun folder contains its own copy of the COBOL source file. Always use that version with `AutomatedRun.py`. Using the root-level file will cause the Python script to break.

---

## What is a 16-bit Headerless PCM File?

A headerless PCM (Pulse Code Modulation) file contains pure audio samples with no metadata — no sample rate, no channel count, no file format markers. Just sequential samples from start to finish.

Each sample is stored as a signed 16-bit integer (range: −32,768 to +32,767), representing amplitude at that moment in time. Samples are little-endian (low byte first, then high byte). Since there is no header, you must supply the playback parameters manually in Audacity: **44,100 Hz, 16-bit, Mono, Little-endian**.

---

## Quick-Start Guide

### 1. Initial Setup

1. Clone or download the repository.
2. Ensure all copybook `.CPY` files are in the same directory as the COBOL source you intend to use.
3. Create one or more empty `.raw` files to act as output targets (e.g. `Output1.raw`, `Output2.raw`, `Output3.raw`, `Output4.raw`).
4. Install GnuCOBOL and make note of its installation path — you will need this for the Python GUI.

### 2. Running the Synth via AutoRun (Recommended)

The primary way to use COMOL-1 is through the `AutomatedRun.py` GUI in the `AutoRun/` folder. This lets you configure up to four independent voices simultaneously, then compile and run them all in one click.

```bash
cd AutoRun
python AutomatedRun.py
```

See the **AutoRun GUI Reference** section below for full parameter documentation.

### 3. Listening to the Output in Audacity

1. Launch Audacity and go to `File > Import > Raw Data...`
2. Select your `.raw` output file
3. Set: **Encoding:** Signed 16-bit PCM, **Byte order:** Little-endian, **Channels:** 1 Channel (Mono), **Sample rate:** 44100 Hz, **Start offset:** 0, **Amount:** 100%
4. Click OK

**For multi-voice stereo output:** Import two `.raw` files (e.g. your left and right voices) as separate tracks. Pan one track fully left and the other fully right using the track pan sliders. This gives you a true stereo image from two independently synthesised voices.

### 4. Exporting from Audacity

1. Go to `File > Export > Export Audio...`
2. Set **File type** to `Other uncompressed files`, **Header** to `RAW (header-less)`, **Encoding** to `Signed 16-bit PCM`
3. Click Save

---

## AutoRun GUI Reference

`AutomatedRun.py` injects your parameter values into the COBOL template, writes a separate `.cbl` file per voice into the `bin/` folder, compiles each one, and runs them.

### Project Folder Setup

Create a `bin/` folder inside your `AutoRun/` directory before running:

```
AutoRun/
├── bin/                  ← create this
├── CML1_Harcode.cbl
├── AutomatedRun.py
├── NOTE-SELECTOR.CPY
└── ... (other copybooks)
```

In **Compiler Settings**, set **Cob copy dir** to the absolute path of the `AutoRun/` folder (the folder *containing* `bin/`, not `bin/` itself), so GnuCOBOL can locate your `.CPY` copybooks at compile time.

### Top Bar

| Control | Description |
|---|---|
| **COBOL Source File** | Select the `CML1_Harcode.cbl` in your `AutoRun/` folder |
| **Output Directory** | Select the `bin/` folder |
| **Compile & run checkbox** | If checked, compiles and runs each waveform immediately after generation |

### Waveform Tabs (1–4)

Each tab is one independent voice. Any field left blank keeps the default value already in the template. A tab is skipped entirely if `OUT_FILE_PATH` is blank.

#### Wave Source
| Parameter | Description |
|---|---|
| `WAVE-SOURCE-CHOICE` | `1` = generate internal sine wave, `2` = load from `.raw` file |
| `IN_FILE_PATH` | Path to the input `.raw` file (only used when `WAVE-SOURCE-CHOICE = 2`) |
| `OUT_FILE_PATH` | Path for the output `.raw` file **(required)** |

#### Pitch
| Parameter | Description |
|---|---|
| `USER-OCTAVE` | Octave `0–6` |
| `USER-NOTE` | Note `0–11` (0=C, 1=C#, 2=D, 3=D#, 4=E, 5=F, 6=F#, 7=G, 8=G#, 9=A, 10=A#, 11=B) |

#### Interpolation
| Parameter | Description |
|---|---|
| `RAW-PATTERN-INPUT` | String of digits defining the interpolation sequence. `1` = nearest neighbour (raw/aliased), `2` = linear (smooth), `3` = sinc (highest quality). Example: `"3331"` cycles sinc×3 then raw. The pattern repeats throughout playback. Defaults to `2` (linear) if left blank. Maximum 16 characters. |

#### Filter
| Parameter | Description |
|---|---|
| `OPERATION-MODE` | `1` = Digital (clean), `2` = Analogue (adds drift, drive, bias) |
| `FILTER-TYPE-CHOICE` | `1` = Low Pass, `2` = High Pass, `3` = Band Pass |
| `KNOB-POSITION` | Filter cutoff `0–100` |
| `Q-KNOB-POSITION` | Filter resonance `0–100` |

#### Analogue Parameters *(active only when `OPERATION-MODE = 2`)*
| Parameter | Description |
|---|---|
| `BIAS-INTENSITY` | Asymmetric clipping intensity `0–100` |
| `USER-DRIVE-IN` | Soft saturation drive `1–10` |
| `USER-DRIFT-IN` | Random pitch/amplitude drift `0–100` |
| `USER-CRUSH-IN` | Bit crush factor `1–2000` |

#### Volume Envelope (JD800-style 5-stage TVA)
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

Each tab has a **Copy to:** row at the bottom. Clicking a waveform button copies all parameters from the current tab to the target tab and switches to it. Useful for building variations from a base patch.

### Compiler Settings

Saved to `config.ini` in the `AutoRun/` folder. **Add `config.ini` and `bin/` to your `.gitignore`** as both contain machine-specific paths.

| Setting | Description |
|---|---|
| `compiler_path` | Full path to `cobc.exe` |
| `cob_copy_dir` | Directory containing your `.CPY` copybooks (your `AutoRun/` folder) |
| `cob_lib_path` | GnuCOBOL lib directory |
| `output_directory` | Fallback output directory |
| `copy_runtime_dlls` | `yes` / `no` — copies GnuCOBOL runtime DLLs into `bin/` |

### Session Persistence

All field values and paths are saved automatically on close or after a successful generate run, and restored the next time the tool is opened.

### Output Files

After a successful run, `bin/` will contain:

```
bin/
├── waveform_1.cbl
├── waveform_1.exe
├── waveform_2.cbl
├── waveform_2.exe
└── ...
```

Each `.exe` is standalone. Running one processes the audio and writes the `.raw` output to the path specified in `OUT_FILE_PATH`.

---

## FX Module

Located in `FX-Module/`. COMOLFX is a sample-accurate stereo effects processor written in GnuCOBOL. It takes a mono `.raw` input and produces two separate output files representing the left and right channels of a processed stereo signal.

The signal chain is fixed and runs in this order:

```
Input (mono .raw) → Spectrum EQ → Saturator → Phaser → Chorus → Delay → Reverb → DC Blocker → Left .raw + Right .raw
```

Spectrum, Saturator, and Phaser are applied to the mono signal before it is split. Chorus, Delay, and Reverb each operate in true stereo with independent left/right buffers. A DC blocker runs on both channels as the final stage.

### Running the FX Module

```bash
cd FX-Module
python FX-RUN.py
```

### FX-RUN.py Workflow

1. Click **Select File** and choose `COMOLFX.cbl`
2. Click **Select Folder** to choose an output directory
3. Fill in parameters across up to four preset tabs. Each preset requires a mono input path and separate left/right output paths
4. Open **Compiler Settings** and configure the GnuCOBOL paths (same process as AutoRun)
5. Ensure **Compile & run** is checked, then click **Generate All Presets**

The script patches the COBOL source for each preset, writes `FX1.cbl` through `FX4.cbl`, compiles them, and launches each in a new Command Prompt window.

### FX Effects Reference

#### Effect 1 — Spectrum (State Variable EQ)

A band-pass EQ that boosts or cuts a frequency band centred at `FX-SPEC-FREQ` and blends it back into the dry signal.

| Parameter | Variable | Default | Range |
|---|---|---|---|
| Centre Frequency | `FX-SPEC-FREQ` | `0.45` | `0.01 – 0.95` |
| Resonance (Q) | `FX-SPEC-Q` | `0.85` | `0.1 – 0.99` |
| Band Gain | `FX-SPEC-GAIN` | `0.50` | `−0.50 – 2.00` |

#### Effect 2 — Saturator

Hard-clipping drive stage. Amplifies the signal then clamps it to ±32,000, generating harmonics from subtle warmth to aggressive clipping.

| Parameter | Variable | Default | Range |
|---|---|---|---|
| Drive Amount | `FX-DRIVE-AMOUNT` | `0.75` | `0.0 – 2.0` |

#### Effect 3 — Phaser

A 4-stage all-pass phaser with LFO sweep and feedback. The LFO uses a 16,384-point sine table with linear interpolation.

| Parameter | Variable | Default | Range |
|---|---|---|---|
| LFO Rate | `FX-PHASER-RATE` | `0.400` | `0.01 – 8.0` Hz |
| Depth | `FX-PHASER-DEPTH` | `0.90` | `0.0 – 1.0` |
| Manual Position | `FX-PHASER-MANUAL` | `0.50` | `0.0 – 1.0` |
| Feedback | `FX-PHASER-FBACK` | `0.75` | `0.0 – 0.95` |
| Wet/Dry Mix | `FX-PHASER-MIX` | `0.50` | `0.0 – 1.0` |

#### Effect 4 — Chorus (Stereo)

2-voice-per-channel stereo modulated delay. Left and right LFO phases are offset by 90° to guarantee a wide, constantly-moving stereo image. Uses a 9,000×9-tap sinc table for interpolation on the read path.

| Parameter | Variable | Default | Range |
|---|---|---|---|
| LFO Rate | `FX-CHORUS-RATE` | `0.85` | `0.01 – 5.0` Hz |
| Depth | `FX-CHORUS-DEPTH` | `65.0` | `0 – 300` samples |
| Wet/Dry Mix | `FX-CHORUS-MIX` | `0.60` | `0.0 – 1.0` |

#### Effect 5 — Delay (Stereo Ping-Pong)

Stereo ping-pong delay. Each channel's echo feeds back into the opposite channel's buffer, making repeats alternate sides. Buffer is 44,100 samples (1,000 ms max).

| Parameter | Variable | Default | Range |
|---|---|---|---|
| Delay Time | `FX-DELAY-MS` | `450.00` | `10 – 1000` ms |
| Feedback | `FX-DELAY-FBACK` | `0.65` | `0.0 – 0.95` |
| Wet/Dry Mix | `FX-DELAY-MIX` | `0.45` | `0.0 – 1.0` |

> **Note:** The GUI shows a range up to 3,000 ms but the buffer is fixed at 1,000 ms. Keep delay time within `10–1000` ms to avoid incorrect wrapping.

#### Effect 6 — Reverb (Parallel Comb Tank)

Four-tank parallel Schroeder-style comb filter reverb with stereo cross-feed. Tank lengths are 1,600 / 1,800 / 2,000 / 2,250 samples, chosen to produce a dense tail without obvious periodicity.

| Parameter | Variable | Default | Range |
|---|---|---|---|
| Decay | `FX-REV-DECAY` | `0.960` | `0.70 – 0.999` |
| Wet/Dry Mix | `FX-REV-MIX` | `0.55` | `0.0 – 1.0` |

> Do not set `FX-REV-DECAY` to 1.0 or above — the buffers will grow without bound.

#### DC Blocker (Internal)

A fixed first-order IIR high-pass filter (coefficient 0.995, ~5.5 Hz cutoff) applied to both channels as the final stage. No user parameters. Removes DC offset accumulated through the feedback loops.

### Assembling Stereo Output in Audacity

After running COMOLFX you will have two `.raw` files — one for the left channel and one for the right. To combine them:

1. Import both files into Audacity (`File > Import > Raw Data...`) using the settings from the Quick-Start section above
2. Pan the first track fully left and the second track fully right using the track pan sliders
3. Export the result as a stereo file or continue processing as needed

---

## Making Your Own Waveforms

All wavetables are 2048-sample, 16-bit, mono, 44,100 Hz headerless PCM files.

### Method 1: Recording or Sampling (Recommended)

1. Import or record source audio in Audacity
2. Select exactly 2048 samples. Use the bottom toolbar dropdown to switch the display to samples if it shows seconds
3. For clean loops, select a region that starts and ends at zero-crossing points (where the waveform crosses the centre line)
4. Normalise: `Effect > Normalize...`, set to `-1.0 dB`, ensure "Remove DC offset" is checked
5. Right-click and split the clip. Delete everything outside the 2048-sample region and move it to the start of the track
6. Export: `File > Export > Export Selected Audio...`, set **Header** to `RAW (header-less)`, **Encoding** to `Signed 16-bit PCM`

**Test your loop** by importing the `.raw` back into Audacity and using `Effect > Repeat...` to duplicate it 10–20 times. Audible clicks at the loop point mean the start and end samples don't connect smoothly — find a cleaner zero-crossing.

### Method 2: Drawing Waveforms

1. Go to `Generate > Tone...`: Sine, 440 Hz, Amplitude 0.8, Duration `0.046439909` seconds (= exactly 2048 samples at 44.1 kHz)
2. Switch to the Draw Tool (pencil icon, or `F3`) and zoom in with `Ctrl+1`
3. Draw your waveform. Sharp corners add brightness; smooth curves produce warmth; asymmetry generates even harmonics
4. Verify the loop point: zoom in to confirm the rightmost drawn sample connects cleanly back to the leftmost
5. Normalize and export as above

### Tips

- **Harmonic content matters.** Bright, complex sources (brass, distorted guitar, sawtooth) give you more tonal range to work with than a plain sine.
- **Avoid DC offset.** Always normalize with "Remove DC offset" checked.
- **Bit depth matters less than you think.** The COBOL synth's fixed-point processing will add its own character regardless of source bit depth.
- **Capture movement.** The attack of a plucked string, a vowel formant shift, or a bell's decay tail all make more interesting wavetables than static sustained tones.

---

## Interpolation Sequencer

When prompted (or via the `RAW-PATTERN-INPUT` field in AutoRun), enter a pattern of digits up to 16 characters long:

- `1` = Nearest Neighbour — raw, aliased
- `2` = Linear — smooth
- `3` = Sinc — highest quality, band-limited

The pattern repeats throughout playback. Shorter patterns create rhythmic digital artefacts; longer patterns produce evolving timbral variations. Defaults to `2` if left blank.

**Examples:**
- `2222` — pure linear (smooth, standard)
- `3331` — mostly sinc with occasional aliasing
- `1212` — alternates raw and smooth every sample
- `321` — cycles through all three methods

---

## To Do

1. ~~Implement LFOs~~ 
2. ~~Update the filter from a static to a dynamic time variant style.~~ 
3. Write a Mainframe compliant version of the code. 
4. Expand the sample bank with all the waveforms from the original JD800.
5. ~~Find a way to 'batch' generate 4 or more outputs to better match the 4 voices of the JD800.~~
6. ~~Implement basic JD800 style FX.~~
7. Remake the readmes to better reflect the current state of the program
