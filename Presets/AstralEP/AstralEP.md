# COMOL Preset: Astral EP
### Full Patch & FX Documentation

---

## Table of Contents

1. [Overview](#overview)
2. [Signal Chain & Asset Requirements](#signal-chain--asset-requirements)
3. [Layer 1 — Core EP](#layer-1--core-ep)
4. [Layer 2 — SoftPad](#layer-2--softpad)
5. [Layer 3 — HeavenA](#layer-3--heavena)
6. [Layer 4 — MusicBox](#layer-4--musicbox)
7. [The Final Mix (Panning & Levels)](#the-final-mix-panning--levels)

---

## Overview

**Astral EP** is a massive, evolving 80s cinematic hybrid patch. It combines a solid electric piano knock, a lush evolving pad, a breathy digital top-end, and a delicate, sparkling attack. 

This preset is built using four distinct generator passes in `COMOL-1.1`, followed by four distinct stereo processing passes through `COMOLFX`.

## Signal Chain & Asset Requirements

**Required Source Waveforms:**
* `MK-80 EP B.raw`
* `SoftPad.raw`
* `HeavenA.raw`
* `MusicBox.raw`

**Workflow:**
For each layer, you must first compile and run the generator engine to create the mono source waveform, and then compile and run the FX engine to generate the true-stereo left and right outputs.

---

## Layer 1 — Core EP
**Source File:** `MK-80 EP B.raw`
**Goal:** The solid, punchy foundation of the key strike.

### COMOL-1.1 Synthesis Parameters
| Section | Parameter | Value |
| :--- | :--- | :--- |
| **Engine** | Interpolation Sequencer | `3333` (All Sinc) |
| **Filter** | Architecture / Type | `2` (Virtual Analogue) / `1` (LPF) |
| | Cutoff / Resonance | `85` / `10` |
| | Bias / Drive / Drift / Crush | `0` / `2` / `5` / `1` |
| **Amp Env** | T1 / L1 (Attack) | `0.01` / `100` |
| | T2 / L2 (Decay 1) | `0.80` / `50` |
| | T3 / L3 (Decay 2) | `1.00` / `40` |
| | Sus / T4 (Sustain / Release) | `2.00` / `0.60` |
| **TVF Env** | T1 / L1 (Attack) | `0.01` / `+50` |
| | T2 / L2 (Decay 1) | `0.50` / `+20` |
| | T3 / L3 (Decay 2) | `1.00` / `+10` |
| | Sus / T4 (Sustain / Release) | `2.00` / `0.60` |
| | Envelope Depth | `30` |

### COMOLFX Stereo Processing
| Effect | Variable | Value | Notes |
| :--- | :--- | :--- | :--- |
| **Spectrum** | `FX-SPEC-FREQ` | `0.50000` | Centered midrange punch. |
| | `FX-SPEC-Q` | `0.70000` | |
| | `FX-SPEC-GAIN` | `0.40` | |
| **Saturator** | `FX-DRIVE-AMOUNT` | `0.20` | Slight warming. |
| **Phaser** | `FX-PHASER-RATE` | `0.400` | Default. |
| | `FX-PHASER-DEPTH` | `0.90` | Default. |
| | `FX-PHASER-MANUAL` | `0.50` | Default. |
| | `FX-PHASER-FBACK` | `0.75` | Default. |
| | `FX-PHASER-MIX` | `0.00` | Bypassed. |
| **Chorus** | `FX-CHORUS-RATE` | `0.25000` | |
| | `FX-CHORUS-DEPTH` | `15.00000` | |
| | `FX-CHORUS-MIX` | `0.20` | |
| **Delay** | `FX-DELAY-MS` | `0450.00000` | |
| | `FX-DELAY-FBACK` | `0.30` | |
| | `FX-DELAY-MIX` | `0.20` | |
| **Reverb** | `FX-REV-DECAY` | `0.600` | Short decay. |
| | `FX-REV-MIX` | `0.25` | |

---

## Layer 2 — SoftPad
**Source File:** `SoftPad.raw`
**Goal:** A thick, analog-style sustain that swells in slightly after the initial key strike.

### COMOL-1.1 Synthesis Parameters
| Section | Parameter | Value |
| :--- | :--- | :--- |
| **Engine** | Interpolation Sequencer | `3333` (All Sinc) |
| **Filter** | Architecture / Type | `2` (Virtual Analogue) / `1` (LPF) |
| | Cutoff / Resonance | `55` / `20` |
| | Bias / Drive / Drift / Crush | `0` / `4` / `30` / `1` |
| **Amp Env** | T1 / L1 (Attack) | `0.30` / `80` |
| | T2 / L2 (Decay 1) | `1.00` / `70` |
| | T3 / L3 (Decay 2) | `1.00` / `60` |
| | Sus / T4 (Sustain / Release) | `2.00` / `1.50` |
| **TVF Env** | T1 / L1 (Attack) | `0.50` / `+30` |
| | T2 / L2 (Decay 1) | `1.50` / `+40` |
| | T3 / L3 (Decay 2) | `1.00` / `+40` |
| | Sus / T4 (Sustain / Release) | `2.00` / `1.50` |
| | Envelope Depth | `25` |

### COMOLFX Stereo Processing
| Effect | Variable | Value | Notes |
| :--- | :--- | :--- | :--- |
| **Spectrum** | `FX-SPEC-FREQ` | `0.30000` | Emphasizes low-mids. |
| | `FX-SPEC-Q` | `0.50000` | |
| | `FX-SPEC-GAIN` | `0.30` | |
| **Saturator** | `FX-DRIVE-AMOUNT` | `0.00` | Bypassed. |
| **Phaser** | `FX-PHASER-RATE` | `0.150` | Slow sweep. |
| | `FX-PHASER-DEPTH` | `0.60` | |
| | `FX-PHASER-MANUAL` | `0.50` | |
| | `FX-PHASER-FBACK` | `0.75` | |
| | `FX-PHASER-MIX` | `0.40` | |
| **Chorus** | `FX-CHORUS-RATE` | `0.45000` | |
| | `FX-CHORUS-DEPTH` | `50.00000` | |
| | `FX-CHORUS-MIX` | `0.65` | |
| **Delay** | `FX-DELAY-MS` | `0450.00000` | Default. |
| | `FX-DELAY-FBACK` | `0.65` | Default. |
| | `FX-DELAY-MIX` | `0.00` | Bypassed. |
| **Reverb** | `FX-REV-DECAY` | `0.920` | Long decay. |
| | `FX-REV-MIX` | `0.55` | |

---

## Layer 3 — HeavenA
**Source File:** `HeavenA.raw`
**Goal:** A high-frequency, airy shimmer that floats above the mix.

### COMOL-1.1 Synthesis Parameters
| Section | Parameter | Value |
| :--- | :--- | :--- |
| **Engine** | Interpolation Sequencer | `1212` (None/Linear) |
| **Filter** | Architecture / Type | `1` (Pure Digital) / `2` (HPF) |
| | Cutoff / Resonance | `60` / `15` |
| | Bias / Drive / Drift / Crush | `0` / `0` / `0` / `0` |
| **Amp Env** | T1 / L1 (Attack) | `0.50` / `70` |
| | T2 / L2 (Decay 1) | `1.50` / `70` |
| | T3 / L3 (Decay 2) | `2.00` / `70` |
| | Sus / T4 (Sustain / Release) | `2.00` / `2.00` |
| **TVF Env** | T1 / L1 (Attack) | `1.00` / `+50` |
| | T2 / L2 (Decay 1) | `1.00` / `+50` |
| | T3 / L3 (Decay 2) | `1.00` / `+50` |
| | Sus / T4 (Sustain / Release) | `2.00` / `2.00` |
| | Envelope Depth | `0` |

### COMOLFX Stereo Processing
| Effect | Variable | Value | Notes |
| :--- | :--- | :--- | :--- |
| **Spectrum** | `FX-SPEC-FREQ` | `0.85000` | High frequency boost. |
| | `FX-SPEC-Q` | `0.60000` | |
| | `FX-SPEC-GAIN` | `0.45` | |
| **Saturator** | `FX-DRIVE-AMOUNT` | `0.00` | Bypassed. |
| **Phaser** | `FX-PHASER-RATE` | `0.350` | |
| | `FX-PHASER-DEPTH` | `0.90` | |
| | `FX-PHASER-MANUAL` | `0.50` | |
| | `FX-PHASER-FBACK` | `0.75` | |
| | `FX-PHASER-MIX` | `0.70` | Heavy swirling mix. |
| **Chorus** | `FX-CHORUS-RATE` | `0.85000` | |
| | `FX-CHORUS-DEPTH` | `70.00000` | |
| | `FX-CHORUS-MIX` | `0.85` | Deep detune. |
| **Delay** | `FX-DELAY-MS` | `0450.00000` | Default. |
| | `FX-DELAY-FBACK` | `0.65` | Default. |
| | `FX-DELAY-MIX` | `0.00` | Bypassed. |
| **Reverb** | `FX-REV-DECAY` | `0.980` | Massive cathedral wash. |
| | `FX-REV-MIX` | `0.80` | |

---

## Layer 4 — MusicBox
**Source File:** `MusicBox.raw`
**Goal:** A pristine, sharp pluck that instantly fades, adding detail to the finger strike.

### COMOL-1.1 Synthesis Parameters
| Section | Parameter | Value |
| :--- | :--- | :--- |
| **Engine** | Interpolation Sequencer | `1111` (All Nearest) |
| **Filter** | Architecture / Type | `1` (Pure Digital) / `2` (HPF) |
| | Cutoff / Resonance | `75` / `0` |
| | Bias / Drive / Drift / Crush | `0` / `0` / `0` / `0` |
| **Amp Env** | T1 / L1 (Attack) | `0.00` / `100` |
| | T2 / L2 (Decay 1) | `0.30` / `0` |
| | T3 / L3 (Decay 2) | `0.00` / `0` |
| | Sus / T4 (Sustain / Release) | `0.00` / `0.20` |
| **TVF Env** | *Bypassed* | (Set Depth to `0`) |

### COMOLFX Stereo Processing
| Effect | Variable | Value | Notes |
| :--- | :--- | :--- | :--- |
| **Spectrum** | `FX-SPEC-FREQ` | `0.90000` | Presence boost. |
| | `FX-SPEC-Q` | `0.80000` | |
| | `FX-SPEC-GAIN` | `0.50` | |
| **Saturator** | `FX-DRIVE-AMOUNT` | `0.00` | Bypassed. |
| **Phaser** | `FX-PHASER-RATE` | `0.400` | Default. |
| | `FX-PHASER-DEPTH` | `0.90` | Default. |
| | `FX-PHASER-MANUAL` | `0.50` | Default. |
| | `FX-PHASER-FBACK` | `0.75` | Default. |
| | `FX-PHASER-MIX` | `0.00` | Bypassed. |
| **Chorus** | `FX-CHORUS-RATE` | `0.50000` | |
| | `FX-CHORUS-DEPTH` | `25.00000` | |
| | `FX-CHORUS-MIX` | `0.30` | |
| **Delay** | `FX-DELAY-MS` | `0300.00000` | Quick pings. |
| | `FX-DELAY-FBACK` | `0.65` | |
| | `FX-DELAY-MIX` | `0.45` | |
| **Reverb** | `FX-REV-DECAY` | `0.750` | |
| | `FX-REV-MIX` | `0.35` | |

---

## The Final Mix (Panning & Levels)

Because `COMOLFX` outputs discrete `.raw` files for the left and right channels, you will end up with 8 audio files in total. 

**CRITICAL:** When importing these files into your DAW or audio editor, ensure you **hard-pan the left output files 100% Left** and the **right output files 100% Right**. This ensures the internal ping-pong delays and stereo comb filters function correctly to create the immersive stereo field.

**Suggested Mix Offsets:**
* **Layer 1 (Core EP):** -3.0 dB
* **Layer 2 (SoftPad):** -6.0 dB
* **Layer 3 (HeavenA):** -8.5 dB
* **Layer 4 (MusicBox):** -5.0 dB
