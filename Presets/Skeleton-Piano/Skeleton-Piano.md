# Skeleton-Piano Preset Tutorial (v1.1 Compliant)

This tutorial synthesizes the "Skeleton-Piano" patch using the updated `Comol-1.1` engine. This preset combines a hollow body, a metallic attack, and a textured digital buzz.

**IMPORTANT UPDATE:** This version adjusts the TVF (Filter Envelope) settings to respect the new Bipolar limit (-50 to +50).

## Prerequisites:
* Source code file: `COMOL-1.1.cbl`
* COBOL Compiler
* The three source samples: `EP-TRI-BDY.raw`, `Harsh-Tine.raw`, `DirtyBuzz.raw`

## Phase 1: Synthesizing The Layers

**Core Instruction:** For each layer, you must edit the Input Filename in the `ENVIRONMENT DIVISION` of the source code to match the specific sample required, then re-compile the program.

### Layer 1: The Foundation (Body)

**Goal:** Create the hollow, warm fundamental using high-fidelity Sinc interpolation.

1. **Code Setup:** Open `COMOL-1.1.cbl`.
2. **Edit Path:** Change `SELECT IN-FILE` to point to `EP-TRI-BDY.raw`.
3. **Compile & Run.**
4. **Engine Settings:**
   * **Wave Source:** `2` (File)
   * **Pitch:** Octave `3`, Note `0` (C)
   * **Interpolation Sequencer:** `3333` (All Sinc)
   * **Filter Architecture:** `2` (Virtual Analogue)
      * Bias: `0`
      * Drive: `2`
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
   * **TVF (Cutoff) Envelope (Bipolar Adjusted):**
      * `T1`: 1.00 | `L1`: +50  (Max open)
      * `T2`: 1.00 | `L2`: +45
      * `T3`: 1.00 | `L3`: +40
      * `Sus`: 2.00
      * `T4`: 0.40
      * **Depth:** `22` (Scaled up slightly to compensate for lower L1)

5. **Output:** Rename `Output.raw` to `Skeleton_Body.raw`.

### Layer 2: The Transient (Tine)

**Goal:** A sharp, metallic attack using aliasing and a hard pluck.

1. **Code Setup:** Edit `SELECT IN-FILE` to point to `Harsh-Tine.raw`.
2. **Compile & Run.**
3. **Engine Settings:**
   * **Wave Source:** `2` (File)
   * **Pitch:** Octave `3`, Note `0`
   * **Interpolation Sequencer:** `1111` (All Nearest/None)
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
   * **TVF (Cutoff) Envelope (Bipolar Adjusted):**
      * `T1`: 0.01 | `L1`: +50 (Max open)
      * `T2`: 0.20 | `L2`: +30
      * `T3`: 0.00 | `L3`: +30
      * `Sus`: 0.00
      * `T4`: 0.10
      * **Depth:** `48` (Increased Depth to match original brightness)

4. **Output:** Rename `Output.raw` to `Skeleton_Tine.raw`.

### Layer 3: The Texture (Buzz)

**Goal:** A background fizz using Pattern Sequencing and Crush.

1. **Code Setup:** Edit `SELECT IN-FILE` to point to `DirtyBuzz.raw`.
2. **Compile & Run.**
3. **Engine Settings:**
   * **Wave Source:** `2` (File)
   * **Pitch:** Octave `3`, Note `0`
   * **Interpolation Sequencer:** `1212` (Alternating None/Linear)
   * **Filter Architecture:** `2` (Virtual Analogue)
      * Bias: `7`
      * Drive: `8`
      * Drift: `25`
      * Crush: `100-500`
   * **Filter Type:** `1` (LPF)
      * Cutoff Knob: `75`
      * Resonance Knob: `40`
   * **Amplitude Envelope (JD-800):**
      * `T1`: 0.10 | `L1`: 40
      * `T2`: 0.50 | `L2`: 20
      * `T3`: 2.00 | `L3`: 10
      * `Sus`: 2.00
      * `T4`: 0.30
   * **TVF (Cutoff) Envelope (Bipolar Adjusted):**
      * `T1`: 0.50 | `L1`: +50
      * `T2`: 1.00 | `L2`: +40
      * `T3`: 1.00 | `L3`: +40
      * `Sus`: 2.00
      * `T4`: 0.30
      * **Depth:** `-15` (Negative depth closes filter from the top)

4. **Output:** Rename `Output.raw` to `Skeleton_Buzz.raw`.

## Phase 2: The Mix

Import the three generated files (`Skeleton_Body.raw`, `Skeleton_Tine.raw`, `Skeleton_Buzz.raw`) into your audio editor.

* **Format:** Signed 16-bit PCM, Little-Endian, 44100 Hz, Mono.

**Volume Levels:**
* **Body:** -3.0 dB
* **Tine:** -6.0 dB
* **Buzz:** -12.0 dB
