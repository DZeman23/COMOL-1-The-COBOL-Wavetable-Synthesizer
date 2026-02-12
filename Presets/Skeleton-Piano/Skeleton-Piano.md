# Skeleton-Piano Preset Tutorial (Updated for v1.1)

This tutorial synthesizes the "Skeleton-Piano" patch using the updated `Comol-1.1` engine. This preset combines a hollow body, a metallic attack, and a textured digital buzz, utilizing the new pattern sequencer and time-variant filter (TVF) features

## Prerequisites:
* Source code file: `COMOL-1.1.cbl` 
* COBOL Compiler
* The three source samples: `EP-TRI-BDY.raw`, `Harsh-Tine.raw`, `DirtyBuzz.raw` 

## Phase 1: Synthesizing The Layers

**Core Instruction:** For each layer, you must edit the Input Filename in the `ENVIRONMENT DIVISION` of the source code to match the specific sample required, then re-compile the program.

### Layer 1: The Foundation (Body)

**Goal:** Create the hollow, warm fundamental using high-fidelity Sinc interpolation and a gentle filter swell.

1. **Code Setup:** Open `COMOL-1.1.cbl`. Locate the `FILE-CONTROL` section.
2. **Edit Path:** Change the `SELECT IN-FILE` path to point to your `EP-TRI-BDY.raw` file .
3. **Compile & Run:** Compile the code and run the executable.
4. **Engine Settings:**
   * **Wave Source:** `2` (Load 2048 Samples)
   * **Pitch:** Octave `3`, Note `0` (C)
   * **Interpolation Sequencer:** `3333` 
      * *Note: Selects "Sinc" mode (3) for all steps for maximum clarity.*
   * **Filter Architecture:** `2` (Virtual Analogue) 
      * Bias: `0`
      * Drive: `2` (Resulting Factor 1.0) 
      * Drift: `5` 
      * Crush: `1` 
   * **Filter Type:** `1` (LPF) 
      * Cutoff Knob: `45` 
      * Resonance Knob: `10` 
   * **Amplitude Envelope (JD-800):** 
      * `T1`: 0.05 | `L1`: 90
      * `T2`: 1.50 | `L2`: 70
      * `T3`: 1.00 | `L3`: 60
      * `Sus`: 2.00
      * `T4`: 0.40
   * **TVF (Cutoff) Envelope:** 
      * `T1`: 1.00 | `L1`: 55
      * `T2`: 1.00 | `L2`: 50
      * `T3`: 1.00 | `L3`: 45
      * `Sus`: 2.00
      * `T4`: 0.40
      * **Depth:** `20` (Gentle opening of the filter) 

5. **Output:** Rename the generated `Output.raw` to `Skeleton_Body.raw`.

### Layer 2: The Transient (Tine)

**Goal:** A sharp, metallic attack. We use the "None" interpolation to intentionally introduce aliasing for brightness, and a snappy TVF.

1. **Code Setup:** Open `COMOL-1.1.cbl` and edit the `SELECT IN-FILE` path to point to `Harsh-Tine.raw`.
2. **Compile & Run.**
3. **Engine Settings:**
   * **Wave Source:** `2` (File)
   * **Pitch:** Octave `3`, Note `0`
   * **Interpolation Sequencer:** `1111` 
      * *Note: Selects "None/Nearest" (1) to preserve jagged transients.*
   * **Filter Architecture:** `1` (Pure Digital) 
   * **Filter Type:** `2` (HPF) 
      * Cutoff Knob: `60`
      * Resonance Knob: `0`
   * **Amplitude Envelope (JD-800):**
      * `T1`: 0.00 | `L1`: 100
      * `T2`: 0.30 | `L2`: 0
      * `T3`: 0.00 | `L3`: 0
      * `Sus`: 0.00
      * `T4`: 0.10
   * **TVF (Cutoff) Envelope:**
      * `T1`: 0.01 | `L1`: 80
      * `T2`: 0.20 | `L2`: 60
      * `T3`: 0.00 | `L3`: 60
      * `Sus`: 0.00
      * `T4`: 0.10
      * **Depth:** `30` (Quick snap down of the filter)

4. **Output:** Rename `Output.raw` to `Skeleton_Tine.raw`.

### Layer 3: The Texture (Buzz)

**Goal:** A background fizz using specific bias and bit-crushing. We use the Pattern Sequencer to alternate interpolation modes for a texture that "shimmers."

1. **Code Setup:** Open `COMOL-1.1.cbl` and edit the `SELECT IN-FILE` path to point to `DirtyBuzz.raw`.
2. **Compile & Run.**
3. **Engine Settings:**
   * **Wave Source:** `2` (File)
   * **Pitch:** Octave `3`, Note `0`
   * **Interpolation Sequencer:** `1212` 
      * *Note: Alternates between None (1) and Linear (2) every sample.*
   * **Filter Architecture:** `2` (Virtual Analogue)
      * **Bias:** `7` (Low asymmetry) 
      * **Drive:** `8` (Factor 4.0) 
      * **Drift:** `25` 
      * **Crush:** `4` (Moderate Bit Reduction) 
   * **Filter Type:** `1` (LPF)
      * Cutoff Knob: `75`
      * Resonance Knob: `40`
   * **Amplitude Envelope (JD-800):**
      * `T1`: 0.10 | `L1`: 40
      * `T2`: 0.50 | `L2`: 20
      * `T3`: 2.00 | `L3`: 10
      * `Sus`: 2.00
      * `T4`: 0.30
   * **TVF (Cutoff) Envelope:**
      * `T1`: 0.50 | `L1`: 75
      * `T2`: 1.00 | `L2`: 60
      * `T3`: 1.00 | `L3`: 60
      * `Sus`: 2.00
      * `T4`: 0.30
      * **Depth:** `-10` (Slight closing of the filter over time)

4. **Output:** Rename `Output.raw` to `Skeleton_Buzz.raw`.

## Phase 2: The Mix

Import the three generated files (`Skeleton_Body.raw`, `Skeleton_Tine.raw`, `Skeleton_Buzz.raw`) into your audio editor using these settings:

* **Encoding:** Signed 16-bit PCM 
* **Byte Order:** Little-Endian (Low Byte first) 
* **Channels:** Mono
* **Sample Rate:** 44100 Hz 

**Volume Levels:**
* **Body** (`Skeleton_Body`): -3.0 dB (Primary Source)
* **Tine** (`Skeleton_Tine`): -6.0 dB (Attack Transient)
* **Buzz** (`Skeleton_Buzz`): -12.0 dB (Texture/Noise Floor)

