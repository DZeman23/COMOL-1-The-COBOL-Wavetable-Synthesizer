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

## Table of Contents

- [What This Program Does](#what-this-program-does)
- [Why COBOL?](#why-cobol)
- [Special Thanks](#special-thanks)
- [External Software](#external-software)
- [Repository Structure](#repository-structure)
- [What is a 16-bit Headerless PCM File?](#what-is-a-16-bit-headerless-pcm-file)
- [Quick-Start Guide](#quick-start-guide)
  - [1. Initial Setup](#1-initial-setup)
  - [2. Running the Synth via AutoRun](#2-running-the-synth-via-autorun-recommended)
  - [3. Listening to the Output in Audacity](#3-listening-to-the-output-in-audacity)
  - [4. Exporting from Audacity](#4-exporting-from-audacity)
- [AutoRun GUI Reference](#autorun-gui-reference)
  - [Project Folder Setup](#project-folder-setup)
  - [Top Bar](#top-bar)
  - [Waveform Tabs (1–4)](#waveform-tabs-14)
    - [Wave Source](#wave-source)
    - [Pitch](#pitch)
    - [Interpolation](#interpolation)
    - [Filter](#filter)
    - [Analogue Parameters](#analogue-parameters-active-only-when-operation-mode--2)
    - [Volume Envelope — TVA](#volume-envelope--tva)
    - [Filter Envelope — TVF](#filter-envelope--tvf)
    - [LFO 1](#lfo-1)
    - [LFO 2](#lfo-2)
  - [Copy Parameters](#copy-parameters)
  - [Compiler Settings](#compiler-settings)
  - [Session Persistence](#session-persistence)
  - [Output Files](#output-files)
- [FX Module](#fx-module)
  - [Running the FX Module](#running-the-fx-module)
  - [FX-RUN.py Workflow](#fx-runpy-workflow)
  - [FX Effects Reference](#fx-effects-reference)
    - [Effect 1 — Spectrum (State Variable EQ)](#effect-1--spectrum-state-variable-eq)
    - [Effect 2 — Saturator](#effect-2--saturator)
    - [Effect 3 — Phaser](#effect-3--phaser)
    - [Effect 4 — Chorus (Stereo)](#effect-4--chorus-stereo)
    - [Effect 5 — Delay (Stereo Ping-Pong)](#effect-5--delay-stereo-ping-pong)
    - [Effect 6 — Reverb (Parallel Comb Tank)](#effect-6--reverb-parallel-comb-tank)
    - [DC Blocker (Internal)](#dc-blocker-internal)
  - [Assembling Stereo Output in Audacity](#assembling-stereo-output-in-audacity)
- [Making Your Own Waveforms](#making-your-own-waveforms)
  - [Method 1: Recording or Sampling](#method-1-recording-or-sampling-recommended)
  - [Method 2: Drawing Waveforms](#method-2-drawing-waveforms)
  - [Tips](#tips)
- [Interpolation Sequencer](#interpolation-sequencer)
- [To Do](#to-do)

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
├── COMOL-1.2.cbl             ← stable release (manual use)
├── COMOL-1.3.cbl             ← current release (manual use)
├── 1.3-Change-log.md
├── Full Documentation.md
├── LICENSE
├── Copybooks/                ← .CPY copybook files
├── AutoRun/
│   ├── AutomatedRun.py       ← Python GUI for batch voice generation
│   └── CML1_Harcode.cbl      ← use THIS version with AutoRun (not the root ones)
├── FX-Module/
│   ├── COMOLFX.cbl           ← stereo FX chain processor
│   └── FX-RUN.py             ← Python GUI for FX parameters
├── Presets/                  ← preset patch instructions
└── Sample-Bank/              ← included wavetable samples
```

> **Important:** The AutoRun folder contains its own copy of the COBOL source file. Always use that version with `AutomatedRun.py`. Using either of the root-level `.cbl` files will cause the Python script to break.

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

---

#### Wave Source

| Parameter | Description |
|---|---|
| `WAVE-SOURCE-CHOICE` | `1` = generate internal sine wave, `2` = load wavetable from `.raw` file |
| `IN_FILE_PATH` | Path to input `.raw` wavetable (only used when `WAVE-SOURCE-CHOICE = 2`) |
| `OUT_FILE_PATH` | Path for the output `.raw` file **(required)** |

---

#### Pitch

| Parameter | Range | Description |
|---|---|---|
| `USER-OCTAVE` | `0–6` | Octave |
| `USER-NOTE` | `0–11` | `0`=C, `1`=C#, `2`=D, `3`=D#, `4`=E, `5`=F, `6`=F#, `7`=G, `8`=G#, `9`=A, `10`=A#, `11`=B |

---

#### Interpolation

| Parameter | Range | Description |
|---|---|---|
| `RAW-PATTERN-INPUT` | Up to 16 digits | Per-sample interpolation sequence. `1` = Nearest Neighbour, `2` = Linear, `3` = Sinc. Pattern repeats cyclically. Defaults to `2` if blank |

**Examples:** `2222` = pure linear; `3331` = mostly sinc with occasional aliasing; `1212` = alternates raw and smooth every sample; `321` = cycles all three methods.

---

#### Filter

| Parameter | Range | Description |
|---|---|---|
| `OPERATION-MODE` | `1` or `2` | `1` = Digital (clean biquad), `2` = Analogue (adds bias, saturation, and drift) |
| `FILTER-TYPE-CHOICE` | `1–3` | `1` = Low Pass, `2` = High Pass, `3` = Band Pass |
| `KNOB-POSITION` | `0–100` | Filter cutoff. Static base value; TVF envelope and LFO modulation apply offsets relative to this |
| `Q-KNOB-POSITION` | `0–100` | Filter resonance |

---

#### Analogue Parameters *(active only when `OPERATION-MODE = 2`)*

| Parameter | Range | Description |
|---|---|---|
| `BIAS-INTENSITY` | `0–100` | Asymmetric clipping intensity |
| `USER-DRIVE-IN` | `1–10` | Soft saturation drive amount |
| `USER-DRIFT-IN` | `0–100` | Random per-sample pitch and amplitude drift |
| `USER-CRUSH-IN` | `1–2000` | Bit crush factor |

---

#### Volume Envelope — TVA

| Parameter | Range | Description |
|---|---|---|
| `T1` | `0.00–99.99` s | Attack time |
| `L1` | `0–100` | Attack peak level |
| `T2` | `0.00–99.99` s | Decay 1 time |
| `L2` | `0–100` | Break level |
| `T3` | `0.00–99.99` s | Decay 2 time |
| `L3` | `0–100` | Sustain level |
| `T-SUSTAIN` | `0.00–99.99` s | Sustain hold duration |
| `T4` | `0.00–99.99` s | Release time |

---

#### Filter Envelope — TVF

Offsets the filter cutoff over the note duration, relative to the base `KNOB-POSITION`, scaled by `TVF-DEPTH`.

| Parameter | Range | Description |
|---|---|---|
| `CUT-T1` | `0.00–99.99` s | Attack time |
| `CUT-L1` | `−100–+100` | Cutoff offset at attack peak |
| `CUT-T2` | `0.00–99.99` s | Decay 1 time |
| `CUT-L2` | `−100–+100` | Cutoff offset at break point |
| `CUT-T3` | `0.00–99.99` s | Decay 2 time |
| `CUT-L3` | `−100–+100` | Sustain cutoff offset |
| `CUT-T-SUSTAIN` | `0.00–99.99` s | Sustain hold duration |
| `CUT-T4` | `0.00–99.99` s | Release time |
| `TVF-DEPTH` | `−100–+100` | Global depth multiplier for the entire TVF envelope |

---

#### LFO 1

| Parameter | Range | Description |
|---|---|---|
| `LFO1-WAVEFORM` | `1–8` | `1`=Sine, `2`=Triangle, `3`=Sawtooth Up, `4`=Sawtooth Down, `5`=Square, `6`=Sample & Hold, `7`=Smooth Random, `8`=User RAW (from `LFO-Wave.raw`) |
| `LFO1-RATE-HZ` | `0.0–999.9` Hz | Oscillation speed. `0` = LFO off |
| `LFO1-DELAY-SEC` | `0.0–999.9` s | Silence before LFO onset |
| `LFO1-FADE-SEC` | `−999–+999` s | Positive = fade in over N seconds. Negative = fade out. `0` = no fade |
| `LFO1-OFFSET` | `−100–+100` | DC offset applied to the LFO output |
| `LFO1-KEY-TRIG` | `0` or `1` | `1` = phase resets on each note. `0` = free-running |
| `LFO1-TVA-DEPTH` | `−100–+100` | Amplitude modulation depth (tremolo) |
| `LFO1-TVF-DEPTH` | `−100–+100` | Filter cutoff modulation depth (filter wobble, in knob units) |
| `LFO1-PTCH-DPTH` | `−120–+120` | Pitch modulation depth (vibrato). `10` units = 1 semitone |
| `LFO1-PHASE-OFFS` | `0–359` degrees | Starting phase offset |
| `LFO1-FM-DEPTH` | `−100–+100` | LFO1-to-LFO2 frequency modulation depth |

---

#### LFO 2

Same parameters as LFO1, except there is no `LFO2-FM-DEPTH` — LFO2 is the FM target, not the source.

| Parameter | Range | Description |
|---|---|---|
| `LFO2-WAVEFORM` | `1–8` | Same waveform options as LFO1 |
| `LFO2-RATE-HZ` | `0.0–999.9` Hz | Oscillation speed (before any FM from LFO1) |
| `LFO2-DELAY-SEC` | `0.0–999.9` s | Silence before LFO onset |
| `LFO2-FADE-SEC` | `−999–+999` s | Fade envelope |
| `LFO2-OFFSET` | `−100–+100` | DC offset on LFO2 output |
| `LFO2-KEY-TRIG` | `0` or `1` | Phase trigger on note start |
| `LFO2-TVA-DEPTH` | `−100–+100` | Amplitude modulation depth. Summed with LFO1 TVA output |
| `LFO2-TVF-DEPTH` | `−100–+100` | Filter cutoff modulation depth. Summed with LFO1 TVF output |
| `LFO2-PTCH-DPTH` | `−120–+120` | Pitch modulation depth. Summed with LFO1 pitch output |
| `LFO2-PHASE-OFFS` | `0–359` degrees | Starting phase offset |

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

All parameters are patched into `COMOLFX.cbl` by `FX-RUN.py` before compilation. The signal chain runs in a fixed order and cannot be reordered.

---

#### Effect 1 — Spectrum (State Variable EQ)

A State Variable Filter (SVF) used as a parametric band-pass EQ. On every sample the SVF simultaneously computes low-pass, band-pass, and high-pass outputs; only the band-pass signal is used, and it is blended back additively into the dry signal. This means it boosts or cuts a frequency band rather than replacing the signal, preserving the original character.

Per-sample equations:
```
SP-LOW  = (SP-LOW  + (FX-SPEC-FREQ × SP-BAND)) × 0.999
SP-HIGH = CURRENT-SAMPLE − SP-LOW − (FX-SPEC-Q × SP-BAND)
SP-BAND = (SP-BAND + (FX-SPEC-FREQ × SP-HIGH)) × 0.999
OUTPUT  = CURRENT-SAMPLE + (SP-BAND × FX-SPEC-GAIN)
```
The `× 0.999` damping factor on both the low and band paths prevents DC build-up at low frequencies.

| Parameter | Variable | Default | Range | Description |
|---|---|---|---|---|
| Centre Frequency | `FX-SPEC-FREQ` | `0.45` | `0.01–0.95` | Normalised cutoff. Maps linearly from near-DC (`0.01`) to near-Nyquist (`0.95`). At 44.1 kHz, `0.45` sits in the upper midrange (~8–10 kHz). Keep below `0.95` to avoid filter instability |
| Resonance (Q) | `FX-SPEC-Q` | `0.85` | `0.1–0.99` | Bandwidth of the band-pass. Low values produce a wide, gentle curve; high values narrow the band and increase resonant ringing. Values above `0.90` can become unstable |
| Band Gain | `FX-SPEC-GAIN` | `0.50` | `−0.50–2.00` | Scales the band-pass signal before it is mixed back. Positive values boost; negative values cut. At `0.0` the effect is fully bypassed. Values above `1.0` produce aggressive boosts and may push the saturator into heavier clipping |

---

#### Effect 2 — Saturator

A hard-clipping drive stage that models the onset of tube or tape saturation. The signal is amplified by a factor derived from `FX-DRIVE-AMOUNT` and then hard-clamped to ±32,000, generating harmonic distortion.

Per-sample equations:
```
AMPLIFIED = CURRENT-SAMPLE × (1.0 + FX-DRIVE-AMOUNT ÷ 2)
OUTPUT    = CLAMP(AMPLIFIED, −32000, +32000)
```
At `FX-DRIVE-AMOUNT = 0.0` the factor is `1.0` and the signal passes through unchanged. At `2.0` the factor is `2.0`, doubling amplitude before clamping. The Saturator runs before all time-based effects, so its harmonic content feeds into Phaser, Chorus, Delay, and Reverb.

| Parameter | Variable | Default | Range | Description |
|---|---|---|---|---|
| Drive Amount | `FX-DRIVE-AMOUNT` | `0.75` | `0.0–2.0` | Pre-clipping gain. `0.0` = bypass. `0.1–0.5` = subtle warmth. `0.5–1.5` = noticeable harmonic saturation. Above `1.5` = hard clipping and heavy distortion |

---

#### Effect 3 — Phaser

A 4-stage all-pass filter swept by a sinusoidal LFO. Each stage shifts the phase of the signal without altering its amplitude; when the four shifted versions are summed with the dry signal, moving cancellation notches produce the characteristic phaser sweep. The LFO reads from a 16,384-point lookup table with linear interpolation between entries.

**LFO and coefficient calculation:**
```
LFO phase advance per sample = FX-PHASER-RATE / 44100 × 2π
AP-COEFF = CLAMP(FX-PHASER-MANUAL + (LFO × FX-PHASER-DEPTH × 0.5), −0.95, +0.95)
```

**Feedback (smoothed one-pole IIR at 70/30 ratio):**
```
P-FB-SIG = (P-FB-SIG × 0.7) + (PHASER-LPF-MEM × 0.3)
P-IN     = CURRENT-SAMPLE + (P-FB-SIG × FX-PHASER-FBACK)
```

**Each all-pass cell:**
```
P-OUT = (AP-COEFF × (STATE-Y-OLD − P-IN)) + STATE-X-OLD
```

| Parameter | Variable | Default | Range | Description |
|---|---|---|---|---|
| LFO Rate | `FX-PHASER-RATE` | `0.400` | `0.01–8.0` Hz | Speed of the sweep. `0.01–0.3` Hz = slow, ambient wash. `0.3–2` Hz = classic phaser feel. Above `4` Hz = fast vibrato-like effect |
| Depth | `FX-PHASER-DEPTH` | `0.90` | `0.0–1.0` | How far the LFO sweeps the all-pass coefficient around the manual position. At `0.0` the coefficient is fixed and there is no movement |
| Manual Position | `FX-PHASER-MANUAL` | `0.50` | `0.0–1.0` | Static bias added to the LFO output before driving the all-pass coefficient. Shifts the centre frequency of the sweep; `0.5` centres in the midrange |
| Feedback | `FX-PHASER-FBACK` | `0.75` | `0.0–0.95` | Amount of phased signal fed back into the input. Below `0.3` = soft diffuse sweep. Above `0.6` = resonant, whistling notches. Do not exceed `0.95` |
| Wet/Dry Mix | `FX-PHASER-MIX` | `0.50` | `0.0–1.0` | Blend between dry and phased signal. At `0.0` the phaser is silent; at `1.0` only the phased signal is heard |

---

#### Effect 4 — Chorus (Stereo)

A 2-voice-per-channel stereo modulated delay. Two slightly detuned, time-varying copies of the signal per channel are blended with the dry signal to produce lush ensemble shimmer. Each channel has a 4,096-sample circular delay buffer. Modulated read positions are interpolated using a 9,000×9-tap polyphase sinc table for smooth, alias-free pitch modulation.

**LFO phase offsets per voice:**

| Channel | Voice | LFO Phase Offset | Base Delay |
|---|---|---|---|
| Left | 1 | 0° | 1,323 samples (~30 ms) |
| Left | 2 | 90° | 1,383 samples (~31.3 ms) |
| Right | 1 | 90° | 1,323 samples (~30 ms) |
| Right | 2 | 180° | 1,383 samples (~31.3 ms) |

The 90° quadrature offset between left and right channels guarantees a wide, constantly-moving stereo image. After both voices for each channel are averaged, a one-pole IIR low-pass filter (~13 kHz cutoff) suppresses interpolation artefacts:
```
LPF-MEM = (LPF-MEM × 0.15) + (WET × 0.85)
```

| Parameter | Variable | Default | Range | Description |
|---|---|---|---|---|
| LFO Rate | `FX-CHORUS-RATE` | `0.85` | `0.01–5.0` Hz | Modulation speed. `0.01–0.3` Hz = slow, deep detuning. `0.5–1.5` Hz = natural ensemble chorus. Above `3` Hz = pitch vibrato territory |
| Depth | `FX-CHORUS-DEPTH` | `65.0` | `0–300` samples | How far the read pointer swings around the base delay tap. At `0` there is no pitch modulation (the effect becomes a fixed comb filter). At `300` the sweep is ±300 samples (~6.8 ms). Most musical values sit between `30` and `100` |
| Wet/Dry Mix | `FX-CHORUS-MIX` | `0.60` | `0.0–1.0` | Blend between dry mono and processed stereo wet signal. `0.4–0.7` covers most musical use cases |

---

#### Effect 5 — Delay (Stereo Ping-Pong)

A stereo ping-pong delay where each channel's echo feeds back into the *opposite* channel's buffer, causing repeats to alternate sides rhythmically. Both channels share a 44,100-sample circular buffer (exactly 1,000 ms at 44.1 kHz). All buffer positions are zeroed at initialisation.

**Cross-feed write (creates the ping-pong behaviour):**
```
DELAY-BUF-L[WRITE-PTR] = SAMPLE-L + (WET-R × FX-DELAY-FBACK)
DELAY-BUF-R[WRITE-PTR] = SAMPLE-R + (WET-L × FX-DELAY-FBACK)
```
Feedback values are clamped to ±32,000 before writing to prevent overflow on long tails.

| Parameter | Variable | Default | Range | Description |
|---|---|---|---|---|
| Delay Time | `FX-DELAY-MS` | `450.00` | `10–1000` ms | Echo time in milliseconds. The buffer is fixed at 1,000 ms; values the GUI shows above 1,000 ms will wrap incorrectly — keep within `10–1000`. Common musical values: `125` ms (eighth note at 120 BPM), `250` ms (quarter), `375` ms (dotted eighth), `500` ms (half) |
| Feedback | `FX-DELAY-FBACK` | `0.65` | `0.0–0.95` | Proportion of echo signal fed back each repeat. At `0.0` only a single echo is heard. At `0.95` echoes repeat many times before fading. Values at or above `1.0` risk runaway accumulation |
| Wet/Dry Mix | `FX-DELAY-MIX` | `0.45` | `0.0–1.0` | Blend between the original signal and the delayed echoes |

---

#### Effect 6 — Reverb (Parallel Comb Tank)

Four parallel Schroeder-style comb filter loops with stereo cross-feed. Running four tanks simultaneously at different lengths builds a dense, non-periodic reverberant tail without the obvious metallic coloration of a single comb.

**Tank buffer lengths:**

| Tank | Buffer Length | Approximate Delay |
|---|---|---|
| 1 | 1,600 samples | ~36 ms |
| 2 | 1,800 samples | ~41 ms |
| 3 | 2,000 samples | ~45 ms |
| 4 | 2,250 samples | ~51 ms |

**Per-tank per-sample update:**
```
NEW-VALUE = (DIRECT × 0.31) + (BUFFER[PTR] × FX-REV-DECAY) + (CROSS-CHANNEL × 0.09)
```
`0.31` is the direct injection coefficient. `FX-REV-DECAY` is the feedback coefficient determining tail length. `0.09` is the cross-channel injection coefficient — a small amount of the opposite channel leaks into each tank, creating stereo widening and preventing both channels from decaying identically.

**Output mix (with 0.55 wet scaling to prevent overload from four summed tanks):**
```
OUTPUT-L = (DRY-L × (1 − FX-REV-MIX)) + (WET-L × 0.55 × FX-REV-MIX)
OUTPUT-R = (DRY-R × (1 − FX-REV-MIX)) + (WET-R × 0.55 × FX-REV-MIX)
```

| Parameter | Variable | Default | Range | Description |
|---|---|---|---|---|
| Decay | `FX-REV-DECAY` | `0.960` | `0.70–0.999` | Comb filter feedback coefficient. `0.70–0.80` = tight room ambience. `0.90–0.95` = medium hall. Close to `0.999` = long cathedral wash. **Do not set at or above `1.0`** — the buffers will grow without bound |
| Wet/Dry Mix | `FX-REV-MIX` | `0.55` | `0.0–1.0` | Blend between dry signal and reverb tail. At `1.0` almost only the tail is heard with minimal transient |

---

#### DC Blocker (Internal)

A fixed first-order IIR high-pass filter applied to both channels as the absolute last stage before writing the output files. No user-facing parameters.

Coefficient: `0.995`. Gives a −3 dB cutoff of approximately `5.5 Hz` — well below the threshold of human hearing. Purpose: removes DC offset accumulated through the comb filter feedback loops, preventing clicks, pops, and DAC clipping on playback.

```
OUTPUT = SAMPLE − DC-IN-OLD + (0.995 × DC-OUT-OLD)
```

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
2. ~~Update the filter from static to dynamic Time Variant style~~
3. Write a mainframe-compliant version of the code
4. Expand the sample bank with all waveforms from the original JD800
5. ~~Batch-generate 4 or more outputs to better match the 4 voices of the JD800~~
6. ~~Implement basic JD800-style FX~~
7. ~~Add a stereo FX module~~
8. Remake the readmes to better reflect the current state of the program
