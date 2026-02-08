# Skeleton-Piano Preset Tutorial

This tutorial synthesizes the "Skeleton-Piano" patch using the `Comol-1` engine. This preset combines a hollow body, a metallic attack, and a textured digital buzz.

## Prerequisites:
* Source code file: `JD-ENV.COB`
* COBOL Compiler
* The three source samples: `EP-TRI-BDY.raw`, `Harsh-Tine.raw`, `DirtyBuzz.raw`

## Phase 1: Synthesizing The Layers

**Core Instruction:** For each layer, you must edit the Input Filename in the `ENVIRONMENT DIVISION` of the source code to match the specific sample required, then re-compile the program.

### Layer 1: The Foundation (Body)

**Goal:** Create the hollow, warm fundamental.

1. **Code Setup:** Open `JD-ENV.COB`. Locate the `FILE-CONTROL` section (Lines 6-10).

2. **Edit Path:** Change the `SELECT IN-FILE` path to point to your `EP-TRI-BDY.raw` file.
   * Example: `SELECT IN-FILE ASSIGN TO "C:\YourFolder\EP-TRI-BDY.raw"`

3. **Compile & Run:** Compile the code and run the executable.

4. **Engine Settings:**
   * **Wave Source:** `2` (File)
   * **Pitch:** Octave `3`, Note `0` (C)
   * **Interpolation:** `4` (HI-FI)
   * **Filter Architecture:** `2` (Virtual Analogue)
      * Bias: `0`
      * **Drive:** `2` (Factor 1.0)
      * **Drift:** `5`
      * **Crush:** `1`+1
   * **Filter Type:** `1` (LPF)
      * Cutoff: `45`
      * Resonance: `10`
   * **Envelope:**
      * `T1`: 0.05 | `L1`: 90
      * `T2`: 1.50 | `L2`: 70
      * `T3`: 1.00 | `L3`: 60
      * `Sus`: 2.00
      * `T4`: 0.40

5. **Output:** Rename the generated `Output.raw` to `Skeleton_Body.raw`.

### Layer 2: The Transient (Tine)

**Goal:** A sharp, metallic attack.

1. **Code Setup:** Open `JD-ENV.COB` and edit the `SELECT IN-FILE` path to point to `Harsh-Tine.raw`.

2. **Compile & Run.**

3. **Engine Settings:**
   * **Wave Source:** `2` (File)
   * **Pitch:** Octave `3`, Note `0`
   * **Interpolation:** `1` (VINTAGE)
   * **Filter Architecture:** `1` (Pure Digital)
   * **Filter Type:** `2` (HPF)
      * Cutoff: `60`
      * Resonance: `0`
   * **Envelope:**
      * `T1`: 0.00 | `L1`: 100
      * `T2`: 0.30 | `L2`: 0
      * `T3`: 0.00 | `L3`: 0
      * `Sus`: 0.00
      * `T4`: 0.10

4. **Output:** Rename `Output.raw` to `Skeleton_Tine.raw`.

### Layer 3: The Texture (Buzz)

**Goal:** A background fizz using specific bias and bit-crushing.

1. **Code Setup:** Open `JD-ENV.COB` and edit the `SELECT IN-FILE` path to point to `DirtyBuzz.raw`.

2. **Compile & Run.**

3. **Engine Settings:**
   * **Wave Source:** `2` (File)
   * **Pitch:** Octave `3`, Note `0`
   * **Interpolation:** `3` (TAPE)
   * **Filter Architecture:** `2` (Virtual Analogue)
      * **Bias:** `7` (Low asymmetry)
      * **Drive:** `8` (Factor 4.0)
      * **Drift:** `25`
      * **Crush:** `4` (Digital degradation enabled)+1
   * **Filter Type:** `1` (LPF)
      * Cutoff: `75`
      * Resonance: `40`
   * **Envelope:**
      * `T1`: 0.10 | `L1`: 40
      * `T2`: 0.50 | `L2`: 20
      * `T3`: 2.00 | `L3`: 10
      * `Sus`: 2.00
      * `T4`: 0.30

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

## Phase 3: Post-Processing (OPTIONAL)

The following steps are enhancements and are not required for the base sound.

### EQ Strategy
* **Body:** Cut Highs (>2.5kHz) to reduce digital aliasing overlap.
* **Tine:** Cut Lows (<400Hz) to isolate the metallic impact.
* **Buzz:** Band-pass (1kHz Center) to focus the crush artifacts in the midrange.

### Bus Processing
* **Compression:** Slow attack (30ms) to preserve the Tine snap.
* **Spatial:** Add the "Ambient" reverb from the default Audacity FX to get an almost DX7 like sound.