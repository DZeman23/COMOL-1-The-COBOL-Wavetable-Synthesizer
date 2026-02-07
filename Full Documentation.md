# Techinal Explanation:

## 1. Generates or Loads Waveforms into a Wavetable

The synthesizer offers two methods for populating its 2048-sample wavetable: generating a mathematical sine wave or loading samples from a raw audio file.

### Wavetable Data Structure

The wavetable is defined as an array of 2048 floating-point values, each stored with high precision using COBOL's packed decimal format (COMP-3):

```cobol
01  WAVE-GENERATOR-CONSTANTS.
    05  TABLE-SIZE      PIC 9(4) BINARY VALUE 2048.
    05  TABLE-FLOAT     PIC 9(4)V9(1)   VALUE 2048.0.

01  WAVE-TABLE-STRUCTURE.
    05  WAVE-TABLE      OCCURS 2048 TIMES
                        INDEXED BY WAVE-IDX.
        10  WAVE-SAMPLE PIC S9(1)V9(17) COMP-3.
```

**Key details:**
- `OCCURS 2048 TIMES` creates an array of exactly 2048 elements
- `PIC S9(1)V9(17)` stores values with 1 integer digit and 17 decimal places (range: -9.99999... to +9.99999...)
- `COMP-3` uses packed decimal for memory efficiency and precision
- `INDEXED BY WAVE-IDX` provides fast array access

### Method 1: Generate Internal Sine Wave

When the user selects option 1, the program mathematically generates one complete cycle of a sine wave:

```cobol
ANGLE-RADIANS-SINE.
    PERFORM VARYING LOOP-COUNTER FROM 1 BY 1
      UNTIL LOOP-COUNTER > TABLE-SIZE
        SET WAVE-IDX TO LOOP-COUNTER
        COMPUTE TEMP-ANGLE
        = (2 * PI-CONSTANT * (LOOP-COUNTER - 1)) / TABLE-SIZE
        COMPUTE WAVE-SAMPLE(WAVE-IDX) = FUNCTION SIN(TEMP-ANGLE)
    END-PERFORM.
```

**The mathematics:**

For each sample position `n` (where n = 0 to 2047):

```
angle = (2π × n) / 2048
sample[n] = sin(angle)
```

This generates one complete sine wave cycle (0° to 360°) distributed evenly across 2048 samples. The result is a perfect, band-limited waveform suitable for pitch shifting without aliasing.

**Why 2048 samples?**
- Power of 2 enables efficient modulo operations for wrapping
- At 44.1kHz, provides good resolution for low frequencies
- Large enough for complex waveforms, small enough for fast access

### Method 2: Load 2048 Samples from File

When the user selects option 2, the program reads a headerless 16-bit PCM file and converts it to normalized floating-point values:

```cobol
LOAD-FILE-TO-TABLE.
    DISPLAY "Loading raw audio data..."
    OPEN INPUT IN-FILE.
    PERFORM VARYING WAVE-IDX FROM 1 BY 1
      UNTIL WAVE-IDX > TABLE-SIZE
      
* Read Low Byte
      READ IN-FILE INTO TWO-BYTE-BUFFER(1:1)
        AT END DISPLAY "End of file" END-READ
        
* Read High Byte
      READ IN-FILE INTO TWO-BYTE-BUFFER(2:1)
        AT END DISPLAY "End of file" END-READ

* Convert Binary to Integer
      COMPUTE RESULT-WRITER =
          FUNCTION ORD(TWO-BYTE-BUFFER(1:1)) - 1
      COMPUTE ENDIAN =
          FUNCTION ORD(TWO-BYTE-BUFFER(2:1)) - 1
      COMPUTE RESULT-WRITER =
          RESULT-WRITER + (ENDIAN * 256)

* Handle Signed 16-bit
      IF RESULT-WRITER > 32767
          SUBTRACT 65536 FROM RESULT-WRITER
      END-IF

* Normalize to -1.0 to 1.0 Float
      COMPUTE WAVE-SAMPLE(WAVE-IDX) =
          RESULT-WRITER / 32768.0
    END-PERFORM.
    CLOSE IN-FILE.
```

### Step-by-Step Binary Conversion Process

**Step 1: Read two bytes** (16-bit sample stored in little-endian format)
- Low byte: Least significant 8 bits
- High byte: Most significant 8 bits

**Step 2: Reconstruct the 16-bit integer**

```
unsigned_value = low_byte + (high_byte × 256)
```

Example: Bytes `E8 03` (hex) become:
```
232 + (3 × 256) = 232 + 768 = 1000
```

**Step 3: Convert to signed 16-bit**

Since PCM audio uses signed integers (range -32,768 to +32,767), values above 32,767 represent negative numbers:

```cobol
IF RESULT-WRITER > 32767
    SUBTRACT 65536 FROM RESULT-WRITER
END-IF
```

**Mathematical explanation:**
```
if unsigned_value > 32767:
    signed_value = unsigned_value - 65536
```

Example: Value 50,000 becomes 50,000 - 65,536 = -15,536

**Step 4: Normalize to floating-point range (-1.0 to +1.0)**

```
normalized_sample = signed_value / 32768.0
```

This converts PCM integers to the standard audio floating-point range where:
- +32,767 → +0.999969... (≈ +1.0)
- 0 → 0.0
- -32,768 → -1.0

### Why Normalize to Floating-Point?

**Mathematical precision:** Filter calculations, envelope multiplication, and interpolation require fractional values. Operating in normalized float space prevents overflow and maintains precision.

**Standard range:** Audio DSP conventionally uses ±1.0 range. This makes filter coefficients, gain values, and mixing operations intuitive.

**Final conversion back:** After all processing, the synthesizer converts back to 16-bit PCM for output (see Section 7).

### File Requirements

The input file must be:
- **Exactly 4096 bytes** (2048 samples × 2 bytes per sample)
- **16-bit signed PCM** (little-endian)
- **Headerless** (no WAV/AIFF header—raw audio data only)
- **Normalized** (ideally peaks near ±32,767 for maximum dynamic range)

The file location is hardcoded in the ENVIRONMENT DIVISION:

```cobol
SELECT IN-FILE ASSIGN TO
"your.filename/here.raw"
ORGANIZATION IS SEQUENTIAL
ACCESS MODE IS SEQUENTIAL.
```

### User Interaction

At runtime, the program presents a simple menu:

```
--- WAVEFORM SOURCE SELECTION ---
1. Generate Internal Sine Wave
2. Load 2048 Samples from Input.raw
Choice:
```

```cobol
GET-WAVE-SOURCE.
    DISPLAY " ".
    DISPLAY "--- WAVEFORM SOURCE SELECTION ---".
    DISPLAY "1. Generate Internal Sine Wave".
    DISPLAY "2. Load 2048 Samples from Input.raw".
    DISPLAY "Choice: ".
    ACCEPT WAVE-SOURCE-CHOICE.
    IF WAVE-SOURCE-CHOICE = 2
        PERFORM LOAD-FILE-TO-TABLE
    ELSE
        DISPLAY "Generating Sine Wave..."
        PERFORM ANGLE-RADIANS-SINE
    END-IF.
```

Once the wavetable is populated (by either method), it remains in memory for the entire synthesis process, ready for pitch-shifted playback via variable-speed reading (explained in Section 2).




## 2. Applies Pitch Shifting via Variable-Speed Playback

The synthesizer achieves pitch shifting by reading through the wavetable at different speeds—faster for higher pitches, slower for lower pitches. This is the fundamental principle of wavetable synthesis.

### Pitch Selection Interface

The user selects a musical note using octave and note number:

```cobol
GET-PITCH-SETTINGS.
    DISPLAY " ".
    DISPLAY "--- PITCH SELECTION ---".
    DISPLAY "Enter Octave (0-6): ".
    ACCEPT USER-OCTAVE.
    DISPLAY "Enter Note (0=C, 1=C#, 2=D... 11=B): ".
    ACCEPT USER-NOTE.
```

The program then looks up the corresponding frequency from pre-computed tables (using the NOTE-SELECTOR.CPY copybook):

```cobol
    EVALUATE USER-OCTAVE
        WHEN 0 PERFORM SET-OCTAVE-0
        WHEN 1 PERFORM SET-OCTAVE-1
        WHEN 2 PERFORM SET-OCTAVE-2
        WHEN 3 PERFORM SET-OCTAVE-3
        WHEN 4 PERFORM SET-OCTAVE-4
        WHEN 5 PERFORM SET-OCTAVE-5
        WHEN 6 PERFORM SET-OCTAVE-6
        WHEN OTHER
            DISPLAY "Invalid Octave. Defaulting to C4."
            MOVE FREQ-C4 TO TARGET-FREQUENCY
    END-EVALUATE.
    DISPLAY "Target Frequency Set To: " TARGET-FREQUENCY " Hz".
```

These frequency tables contain equal-tempered tuning values. For example:
- C4 = 261.63 Hz
- A4 = 440.00 Hz
- C5 = 523.25 Hz

### Calculating the Step Size

The critical calculation happens in the `FREQUENCY-MATH` paragraph:

```cobol
FREQUENCY-MATH.
* Calculate how fast we step through the table
    COMPUTE MULTIPLICATION-HOLDER
        = TARGET-FREQUENCY * TABLE-SIZE.
    COMPUTE STEP-SIZE = MULTIPLICATION-HOLDER / SAMPLE-RATE.
    DISPLAY "Step Size: " STEP-SIZE.
```

**The mathematics:**

```
step_size = (target_frequency × table_size) / sample_rate
```

Breaking it down:
```
step_size = (f_target × 2048) / 44100
```

**Example calculations:**

For **A4 (440 Hz)**:
```
step_size = (440 × 2048) / 44100
step_size = 901,120 / 44100
step_size = 20.4308...
```

For **C4 (261.63 Hz)**:
```
step_size = (261.63 × 2048) / 44100
step_size = 535,858.24 / 44100
step_size = 12.1512...
```

For **C5 (523.25 Hz)** (one octave above C4):
```
step_size = (523.25 × 2048) / 44100
step_size = 1,071,616 / 44100
step_size = 24.3015...
```

**What this means:**
- At A4 (440Hz), we advance ~20.43 samples through the wavetable for each output sample
- At C4 (261Hz), we advance ~12.15 samples per output sample
- Doubling the frequency doubles the step size (octave relationship)

### Variable Storage

The step size is stored with high precision:

```cobol
01  PITCH-MATH.
    05  TARGET-FREQUENCY      PIC 9(6)V99.
    05  STEP-SIZE             PIC 9(9)V9999.
    05  MULTIPLICATION-HOLDER PIC 9(12).
```

- `STEP-SIZE PIC 9(9)V9999` provides 4 decimal places of precision
- This fractional precision is essential—without it, tuning would drift out of pitch

### The Read Position Pointer

As samples are generated, a floating-point pointer tracks the current position in the wavetable:

```cobol
01  RESAMPLE-POINTERS.
    05  READ-POSITION   PIC 9(9)V9999 VALUE 1.
    05  READ-INDEX      PIC 9(7).
    05  FRACTIONAL-PART PIC S9V9(5).
    05  INVERSE-FRAC    PIC S9V9(5).
```

**Example progression** for A4 (step_size = 20.4308):

| Output Sample | Read Position | Integer Index | Fractional Part |
|---------------|---------------|---------------|-----------------|
| 1             | 1.0000        | 1             | 0.0000          |
| 2             | 21.4308       | 21            | 0.4308          |
| 3             | 41.8616       | 41            | 0.8616          |
| 4             | 62.2924       | 62            | 0.2924          |
| 5             | 82.7232       | 82            | 0.7232          |

The read position advances continuously through the table, wrapping around when it exceeds 2048.

### Converting Fractional Position to Integer Index

Each time a sample is needed, the fractional position is split into integer and fractional parts:

```cobol
CALCULATE-INDICES.
    COMPUTE READ-INDEX = FUNCTION INTEGER(READ-POSITION).
    COMPUTE FRACTIONAL-PART = READ-POSITION - READ-INDEX.
```

**Example:** If `READ-POSITION = 21.4308`:
- `READ-INDEX = 21` (the sample to read)
- `FRACTIONAL-PART = 0.4308` (how far between sample 21 and 22)

### Linear Interpolation Between Samples

Since the read position falls between samples, the synthesizer interpolates to get a smooth value:

```cobol
CALCULATE-LINEAR-SAMPLE.
    MOVE WAVE-SAMPLE(READ-INDEX) TO SAMPLE-A.
    IF READ-INDEX + 1 > TABLE-SIZE
        MOVE WAVE-SAMPLE(1) TO SAMPLE-B
    ELSE
        MOVE WAVE-SAMPLE(READ-INDEX + 1) TO SAMPLE-B
    END-IF.
    COMPUTE INVERSE-FRAC = 1.0 - FRACTIONAL-PART.
    COMPUTE INTERP-RESULT =
        (SAMPLE-A * INVERSE-FRAC) + (SAMPLE-B * FRACTIONAL-PART).
```

**The interpolation formula:**

```
result = (sample[n] × (1 - frac)) + (sample[n+1] × frac)
```

**Example:** At position 21.4308:
- `SAMPLE-A = WAVE-SAMPLE(21)` = -0.3420
- `SAMPLE-B = WAVE-SAMPLE(22)` = -0.2588
- `FRACTIONAL-PART = 0.4308`
- `INVERSE-FRAC = 1.0 - 0.4308 = 0.5692`

```
result = (-0.3420 × 0.5692) + (-0.2588 × 0.4308)
result = -0.1947 + (-0.1115)
result = -0.3062
```

This weighted average produces a smooth transition between discrete samples, preventing zipper noise and stair-stepping artifacts.

### Advancing the Read Pointer

After each sample is computed, the pointer advances:

```cobol
ADVANCE-POINTERS.
    ADD STEP-SIZE TO READ-POSITION.
    
    PERFORM UNTIL READ-POSITION < TABLE-FLOAT
        SUBTRACT TABLE-FLOAT FROM READ-POSITION
        ADD 1 TO READ-POSITION
    END-PERFORM.
```

**What this does:**

1. Add the step size to current position
2. If position exceeds 2048.0, wrap around to the beginning
3. The `ADD 1` after wrapping maintains continuity (prevents phase discontinuity)

**Example wrapping:**
```
READ-POSITION = 2045.7
ADD STEP-SIZE (20.4308) → 2066.1308
SUBTRACT 2048.0 → 18.1308
ADD 1 → 19.1308
```

The wavetable seamlessly loops, creating continuous periodic oscillation.

### Why This Works: The Math Behind Wavetable Synthesis

**Frequency relationship:**

If the wavetable contains one complete waveform cycle, reading it at different speeds changes the fundamental frequency:

```
f_output = f_table × (step_size / table_size)
```

Since our table is generated/loaded as one cycle at "natural" frequency (or arbitrary frequency for loaded samples), the step size directly controls pitch.

**Proof with A4 example:**

```
f_output = (sample_rate / table_size) × step_size
f_output = (44100 / 2048) × 20.4308
f_output = 21.533 × 20.4308
f_output = 440.00 Hz ✓
```

### The Interpolation Quality Trade-off

The program offers multiple interpolation modes (explained fully in Section 6):

```cobol
GET-INTERPOLATION-MODE.
    DISPLAY " ".
    DISPLAY "--- INTERPOLATION SELECTOR ---".
    DISPLAY "1. VINTAGE (Linear - Grit 100%)".
    DISPLAY "2. MODERN  (Linear - Grit 1%)".
    DISPLAY "3. TAPE    (Linear - 50% Grit)".
    DISPLAY "4. HI-FI   (Sinc - Pure Clean)".
    DISPLAY "5. HYBRID  (Sinc - Clean + Grit)".
    DISPLAY "Choice: ".
    ACCEPT MENU-CHOICE.
```

**Linear interpolation** (shown above): Fast, introduces slight high-frequency roll-off
**Sinc interpolation** (Mode 4-5): Higher quality, computationally expensive, band-limited
**Grit factor**: Intentionally reduces interpolation frequency for vintage aliasing effects

### Sample Generation Loop

The main generation happens inside each envelope stage:

```cobol
RUN-ENVELOPE-STAGE.
* 1. Determine how many actual samples this stage lasts
    COMPUTE TOTAL-SAMPLES = DURATION-SECONDS * SAMPLE-RATE.
    
* ...envelope calculations...

* 3. The Generation Loop for this duration
    PERFORM GENERATE-SAMPLE-BLOCK
        VARYING OUTPUT-COUNT FROM 1 BY 1
        UNTIL OUTPUT-COUNT > TOTAL-SAMPLES.

GENERATE-SAMPLE-BLOCK.
    PERFORM UPDATE-PROGRESS.
    PERFORM CALCULATE-INDICES.
    PERFORM COMPUTE-RAW-SAMPLE.
    PERFORM APPLY-FILTER.
    PERFORM APPLY-VOLUME-AND-WRITE.
    PERFORM ADVANCE-POINTERS.
```

For each output sample:
1. Split read position into integer + fractional parts
2. Interpolate between table samples
3. Apply filtering (Section 3)
4. Apply envelope amplitude (Section 5)
5. Write to output file (Section 7)
6. Advance read position by step size

### Why Variable-Speed Playback?

**Efficiency:** No need to store separate waveforms for every pitch—one 2048-sample table serves all frequencies.

**Authenticity:** This is how classic digital synthesizers (PPG Wave, Waldorf, early Korg) worked. The aliasing artifacts at high pitches are part of the "digital vintage" character.

**Simplicity:** The math is straightforward—just pointer arithmetic and linear interpolation. No complex FFT or resampling algorithms needed.

**Real-time capability:** In hardware synthesizers, this technique allows real-time pitch modulation via MIDI note data or LFOs. Though this COBOL implementation pre-renders audio, the algorithm is identical to real-time wavetable synths.

### Advanced Interpolation Methods

While basic linear interpolation works well, the synthesizer offers multiple interpolation algorithms to balance sound quality, CPU efficiency, and vintage character.

#### Interpolation Mode Selection

```cobol
GET-INTERPOLATION-MODE.
    DISPLAY " ".
    DISPLAY "--- INTERPOLATION SELECTOR ---".
    DISPLAY "1. VINTAGE (Linear - Grit 100%)".
    DISPLAY "2. MODERN  (Linear - Grit 1%)".
    DISPLAY "3. TAPE    (Linear - 50% Grit)".
    DISPLAY "4. HI-FI   (Sinc - Pure Clean)".
    DISPLAY "5. HYBRID  (Sinc - Clean + Grit)".
    DISPLAY "Choice: ".
    ACCEPT MENU-CHOICE.
    EVALUATE MENU-CHOICE
        WHEN 1 MOVE 1   TO GRIT-FACTOR
        WHEN 2 MOVE 100 TO GRIT-FACTOR
        WHEN 3 MOVE 2   TO GRIT-FACTOR
        WHEN 4 MOVE 100 TO GRIT-FACTOR
        WHEN 5 MOVE 2   TO GRIT-FACTOR
    END-EVALUATE.
```

### The "Grit" System: Sample-and-Hold Aliasing

The grit factor controls how often interpolation occurs—creating intentional aliasing for vintage digital character:

```cobol
COMPUTE-RAW-SAMPLE.
* This is the "Grit" Logic
    COMPUTE REMAINDER-VAL =
        FUNCTION MOD(OUTPUT-COUNT, GRIT-FACTOR).
    IF REMAINDER-VAL = 0
* Standard Fetch
        MOVE WAVE-SAMPLE(READ-INDEX) TO INTERP-RESULT
    ELSE
* Interpolated Fetch
        EVALUATE MENU-CHOICE
            WHEN 4
            WHEN 5
                PERFORM CALCULATE-SINC-SAMPLE
            WHEN OTHER
                PERFORM CALCULATE-LINEAR-SAMPLE
        END-EVALUATE
    END-IF.
```

**How grit works:**

- **Grit = 1 (Mode 1: VINTAGE)**: Every sample uses nearest-neighbor (no interpolation)
- **Grit = 2 (Modes 3, 5)**: Alternates between nearest-neighbor and interpolated
- **Grit = 100 (Modes 2, 4)**: Interpolates every sample except multiples of 100

**Example with Grit = 2:**

| Output Sample | MOD(count, 2) | Method Used |
|---------------|---------------|-------------|
| 1             | 1             | Interpolated |
| 2             | 0             | Nearest-neighbor |
| 3             | 1             | Interpolated |
| 4             | 0             | Nearest-neighbor |

**Sonic effect:**
- Creates stair-stepping artifacts (aliasing)
- Mimics early 1980s digital samplers with limited CPU power
- Adds high-frequency harmonics not present in the source waveform
- Produces the characteristic "digital grit" of vintage Fairlight, Emulator, and PPG synthesizers

### Sinc Interpolation: Band-Limited Resampling

For modes 4 and 5, the synthesizer uses **windowed sinc interpolation**—the gold standard for high-quality sample rate conversion.

#### Polyphase Sinc Table Structure

Rather than computing sinc function in real-time, the program pre-computes a **polyphase filter bank** with 900 phases:

```cobol
01  SINC-CONSTANTS.
    05  PI-VAL             PIC 9V9(10) VALUE 3.1415926535.
    05  KERNEL-RADIUS      PIC 9(2)    VALUE 4.
    05  FIXED-POINT-SCALER PIC 9(10)   VALUE 1000000000.

01  POLYPHASE-CONSTANTS.
    05  PHASE-RESOLUTION   PIC 9(3)    VALUE 900.
    05  PHASE-SCALER       PIC 9(3)    VALUE 900.
```

**Table dimensions:**
- **900 phases** (one for each fractional position from 0.000 to 0.999)
- **9 taps per phase** (kernel radius 4 = -4 to +4 samples)
- **Total entries:** 900 × 9 = 8,100 pre-computed weights

This is stored in the `PHASE-ROW` table from the `Sine-Weights-WS` copybook.

#### Sinc Table Initialization

The table is computed once at program startup:

```cobol
INIT-SINC-TABLE.
    DISPLAY "Initializing Sinc Table...".
    PERFORM VARYING PHASE-INDEX FROM 1 BY 1
        UNTIL PHASE-INDEX > PHASE-RESOLUTION
        
        COMPUTE MATCH-FRACTION =
            (PHASE-INDEX - 1) / PHASE-RESOLUTION
        
        PERFORM VARYING KERNEL-INDEX FROM -4 BY 1
            UNTIL KERNEL-INDEX > 4
            
        COMPUTE SINC-X = KERNEL-INDEX - MATCH-FRACTION
        
        IF SINC-X = 0
            COMPUTE FINAL-WEIGHT = 1.0
        ELSE
            COMPUTE SINC-WEIGHT =
            FUNCTION SIN(PI-VAL * SINC-X) / (PI-VAL * SINC-X)
            COMPUTE WINDOW-WEIGHT = 0.5 +
            0.5 * FUNCTION COS(PI-VAL * SINC-X / KERNEL-RADIUS)
            COMPUTE FINAL-WEIGHT = SINC-WEIGHT * WINDOW-WEIGHT
        END-IF
        
        COMPUTE RAW-TABLE-VAL ROUNDED =
            FINAL-WEIGHT * FIXED-POINT-SCALER
        
        COMPUTE TEMP-INDEX-CALC = KERNEL-INDEX + 5
        MOVE RAW-TABLE-VAL TO
        FILTER-TAP OF PHASE-ROW(PHASE-INDEX, TEMP-INDEX-CALC)
        
        END-PERFORM
    END-PERFORM.
```

**The sinc function:**

```
sinc(x) = sin(πx) / (πx)
```

For x = 0, sinc(0) = 1 (by definition, avoiding division by zero)

**The Hann window function:**

```
window(x) = 0.5 + 0.5 × cos(πx / radius)
```

**Final weight:**

```
weight(x) = sinc(x) × window(x)
```

The window function tapers the sinc function to zero at the edges (±4 samples), preventing ringing artifacts while maintaining excellent frequency response.

#### Applying Sinc Interpolation

During playback, the fractional position selects a phase, then convolves 9 wavetable samples with the corresponding weights:

```cobol
CALCULATE-SINC-SAMPLE.
    MOVE 0 TO SUM-ACCUM.

* Get Sinc Phase (0-900)
    COMPUTE PHASE-INDEX =
        FUNCTION INTEGER(FRACTIONAL-PART * PHASE-SCALER) + 1.
    
    IF PHASE-INDEX < 1 MOVE 1 TO PHASE-INDEX END-IF.
    IF PHASE-INDEX > PHASE-RESOLUTION
        MOVE PHASE-RESOLUTION TO PHASE-INDEX END-IF.
        
    PERFORM VARYING KERNEL-INDEX FROM -4 BY 1
        UNTIL KERNEL-INDEX > 4
        
        COMPUTE LOOKUP-INDEX = READ-INDEX + KERNEL-INDEX
        
* Handle Wrap Around for Sinc
        IF LOOKUP-INDEX < 1
            ADD TABLE-SIZE TO LOOKUP-INDEX
        END-IF
        IF LOOKUP-INDEX > TABLE-SIZE
            SUBTRACT TABLE-SIZE FROM LOOKUP-INDEX
        END-IF
        
        COMPUTE TEMP-INDEX-CALC = KERNEL-INDEX + 5
        MOVE FILTER-TAP
        OF PHASE-ROW(PHASE-INDEX, TEMP-INDEX-CALC)
            TO RAW-TABLE-VAL
        
        COMPUTE FINAL-WEIGHT = RAW-TABLE-VAL / FIXED-POINT-SCALER
        
        COMPUTE SUM-ACCUM = SUM-ACCUM +
            (WAVE-SAMPLE(LOOKUP-INDEX) * FINAL-WEIGHT)
    END-PERFORM.
    
    MOVE SUM-ACCUM TO INTERP-RESULT.
```

**Step-by-step example:**

Assume `READ-POSITION = 100.347`
- `READ-INDEX = 100`
- `FRACTIONAL-PART = 0.347`
- `PHASE-INDEX = INTEGER(0.347 × 900) + 1 = 313`

The algorithm retrieves samples 96, 97, 98, 99, 100, 101, 102, 103, 104 (9 samples centered at 100) and weights from `PHASE-ROW(313, 1..9)`.

**Convolution sum:**

```
result = Σ(wave[100 + k] × weight[313][k+5])  for k = -4 to +4
```

Expanded:
```
result = wave[96]×w₁ + wave[97]×w₂ + wave[98]×w₃ + wave[99]×w₄ + 
         wave[100]×w₅ + wave[101]×w₆ + wave[102]×w₇ + wave[103]×w₈ + 
         wave[104]×w₉
```

Where w₁...w₉ are the pre-computed sinc×window weights for phase 313.

#### Why Sinc Interpolation is Superior

**Band-limited:** Sinc is the ideal reconstruction filter in frequency domain—it perfectly reconstructs signals below the Nyquist frequency.

**No aliasing:** Unlike linear interpolation (which acts as a lowpass with gradual roll-off), sinc has a brick-wall frequency response, completely eliminating aliasing.

**Minimal phase distortion:** The symmetric kernel preserves phase relationships in the signal.

**Higher CPU cost:** Requires 9 multiplies and 8 adds per sample (vs. 2 multiplies and 1 add for linear), plus table lookup overhead.

### Comparison of Interpolation Quality

| Mode | Method | Grit | Aliasing | CPU Cost | Character |
|------|--------|------|----------|----------|-----------|
| 1 - VINTAGE | None (nearest) | 100% | Extreme | Minimal | Lo-fi 8-bit sampler |
| 2 - MODERN | Linear | 1% | Minimal | Low | Clean, slight warmth |
| 3 - TAPE | Linear | 50% | Moderate | Low | Vintage digital, "stepped" |
| 4 - HI-FI | Sinc | 1% | None | High | Pristine, transparent |
| 5 - HYBRID | Sinc | 50% | Controlled | High | "Digital analog" hybrid |

**Mode 1 (VINTAGE):** Every sample is nearest-neighbor—maximum aliasing, sounds like early Fairlight CMI.

**Mode 2 (MODERN):** Linear interpolation on 99% of samples—smooth and clean, slight high-frequency roll-off.

**Mode 3 (TAPE):** Linear interpolation every other sample—creates rhythmic aliasing artifacts, like a sample-rate reducer.

**Mode 4 (HI-FI):** Sinc interpolation with minimal grit—professional quality, indistinguishable from oversampled analog.

**Mode 5 (HYBRID):** Sinc interpolation every other sample—combines pristine quality with intentional stair-stepping for character.

### Fixed-Point Scaling for Sinc Weights

The sinc weights are stored as scaled integers to avoid floating-point operations:

```cobol
COMPUTE RAW-TABLE-VAL ROUNDED =
    FINAL-WEIGHT * FIXED-POINT-SCALER
```

Where `FIXED-POINT-SCALER = 1000000000` (1 billion).

**Example:** A weight of 0.876543210 is stored as 876,543,210

During interpolation:
```cobol
COMPUTE FINAL-WEIGHT = RAW-TABLE-VAL / FIXED-POINT-SCALER
```

This converts back to floating-point for the convolution sum. The billion-scale factor provides 9 decimal places of precision—more than sufficient for audio quality.

### Why Multiple Interpolation Methods?

**Creative flexibility:** Different modes suit different musical contexts. Pristine sounds (pads, strings) benefit from sinc; gritty sounds (leads, basses) gain character from aliasing.

**Historical accuracy:** Vintage modes recreate the sound of early digital synthesizers that couldn't afford high-quality interpolation.

**Educational value:** Demonstrates the tradeoff between CPU cost and audio quality in real-time synthesis.

**Hybrid possibilities:** Mode 5 (sinc + grit) creates a unique "high-res lo-fi" aesthetic—mathematically clean interpolation with intentional quantization artifacts.

### Pitch Accuracy

The 4-decimal-place precision in `STEP-SIZE` provides:

```
Precision = 1 / 10000 = 0.0001 samples/sample
Frequency error ≈ (0.0001 / step_size) × 100%
```

For A4 (step_size = 20.4308):
```
Error ≈ (0.0001 / 20.4308) × 100% = 0.00049%
```

This is **far below human pitch perception** (typically ~0.5% or ~10 cents), ensuring perfectly in-tune output across all interpolation modes.



## 3. Processes Audio Through Digital Biquad Filters

After pitch shifting, each sample passes through a digital biquad filter—a second-order IIR (Infinite Impulse Response) filter capable of implementing lowpass, highpass, and bandpass responses with resonance control.

### Filter Architecture Selection

The user first chooses between pure digital filtering or virtual analogue mode (which adds non-linearities after filtering):

```cobol
GET-FILTER-SETTINGS.
    DISPLAY " ".
    DISPLAY "--- FILTER ARCHITECTURE ---".
    DISPLAY "1. PURE DIGITAL (Clean)".
    DISPLAY "2. VIRTUAL ANALOGUE (Dirty)".
    ACCEPT OPERATION-MODE.
```

Then selects filter type and parameters:

```cobol
    DISPLAY " ".
    DISPLAY "--- FILTER TYPE ---".
    DISPLAY "1. LPF, 2. HPF, 3. BPF".
    ACCEPT FILTER-TYPE-CHOICE.
    EVALUATE FILTER-TYPE-CHOICE
        WHEN "1" MOVE 1 TO ACTIVE-FILTER-TYPE
        WHEN "2" MOVE 2 TO ACTIVE-FILTER-TYPE
        WHEN "3" MOVE 3 TO ACTIVE-FILTER-TYPE
        WHEN OTHER MOVE 1 TO ACTIVE-FILTER-TYPE
    END-EVALUATE.
    
    DISPLAY "Cutoff Knob (0-100): ".
    ACCEPT KNOB-POSITION.
    DISPLAY "Resonance Knob (0-100): ".
    ACCEPT Q-KNOB-POSITION.
```

**Filter types:**
- **LPF (Lowpass)**: Passes frequencies below cutoff, attenuates above
- **HPF (Highpass)**: Passes frequencies above cutoff, attenuates below
- **BPF (Bandpass)**: Passes frequencies near cutoff, attenuates both above and below

### Biquad Filter Data Structures

The filter uses six coefficients and four delay lines (history buffers):

```cobol
* Coefficients
01  BIQUAD-COEFFICIENTS.
    05  A0-COEFF            PIC S9(3)V9(8).
    05  A1-COEFF            PIC S9(3)V9(8).
    05  A2-COEFF            PIC S9(3)V9(8).
    05  B0-COEFF            PIC S9(3)V9(8).
    05  B1-COEFF            PIC S9(3)V9(8).
    05  B2-COEFF            PIC S9(3)V9(8).

* Delay Lines (History)
01  DELAY-LINES.
    05  X1-INPUT            PIC S9(12)V9(8) VALUE 0.
    05  X2-INPUT            PIC S9(12)V9(8) VALUE 0.
    05  Y1-OUTPUT           PIC S9(12)V9(8) VALUE 0.
    05  Y2-OUTPUT           PIC S9(12)V9(8) VALUE 0.
```

**Delay line variables:**
- `X1-INPUT`, `X2-INPUT`: Previous two input samples
- `Y1-OUTPUT`, `Y2-OUTPUT`: Previous two output samples

These store the filter's "memory"—essential for IIR (recursive) filtering.

### Coefficient Calculation Process

#### Step 1: Map Knob Position to Frequency

The cutoff knob (0-100) maps to a pre-computed frequency table:

```cobol
CALCULATE-FILTER-COEFFICIENTS.
* 1. Get Freq from Table
    COMPUTE LOOKUP-IDX = KNOB-POSITION + 1.
    MOVE FREQ-HZ(LOOKUP-IDX) TO CURRENT-FREQ-HZ.
    DISPLAY "Filter Hz: " CURRENT-FREQ-HZ.
```

The `FREQ-HZ` table (loaded from `FREQUENCY-TABLE-WS` copybook) contains logarithmically-spaced frequencies from ~20 Hz to ~20,000 Hz, matching the perceptual response of human hearing.

#### Step 2: Calculate Q (Resonance)

```cobol
* 2. Calculate Resonance
    COMPUTE Q-RESONANCE = Q-GRADIENT * Q-KNOB-POSITION
                          + Q-STARTING-POINT.
```

**The Q formula:**

```
Q = (0.015 × knob_position) + 0.707
```

**Q-factor range:**
- Knob at 0: Q = 0.707 (Butterworth response, no resonance peak)
- Knob at 50: Q = 1.457 (moderate resonance)
- Knob at 100: Q = 2.207 (strong resonance peak)

The constants are defined as:

```cobol
78  Q-STARTING-POINT    VALUE 0.707.
78  Q-GRADIENT          VALUE 0.015.
```

Higher Q values create a sharper resonant peak at the cutoff frequency, characteristic of analog synthesizer filters.

#### Step 3: Calculate Angular Frequency (ω)

```cobol
* 3. Calculate Omega/Alpha
    COMPUTE NUMERATOR-VAL = CURRENT-FREQ-HZ / SAMPLE-RATE.
    COMPUTE ANGULAR-FREQUENCY = PI-2 * NUMERATOR-VAL.
```

**The mathematics:**

```
normalized_freq = f_cutoff / f_sample
ω = 2π × normalized_freq
```

**Example:** For 1000 Hz cutoff at 44.1 kHz sample rate:

```
normalized_freq = 1000 / 44100 = 0.02268
ω = 6.28318531 × 0.02268 = 0.14251
```

This converts the cutoff frequency to radians per sample, the unit needed for digital filter design.

#### Step 4: Lookup Sin(ω) and Cos(ω)

Rather than computing sine and cosine on-the-fly (expensive in COBOL), the program uses pre-computed lookup tables:

```cobol
    PERFORM FIND-SINE-FROM-OMEGA.
    PERFORM FIND-COS-FROM-OMEGA.

FIND-SINE-FROM-OMEGA.
    SET IDX-SINE TO 1.
    SEARCH SINE-ENTRY
        WHEN TBL-OMEGA-KEY(IDX-SINE) >= ANGULAR-FREQUENCY
            MOVE TBL-SINE-VAL(IDX-SINE) TO FINAL-SINE-VALUE
    END-SEARCH.

FIND-COS-FROM-OMEGA.
    SET IDX-COS TO 1.
    SEARCH COS-ENTRY
        WHEN TBL-COS-OMEGA-KEY(IDX-COS) >= ANGULAR-FREQUENCY
            MOVE TBL-COS-VAL(IDX-COS) TO FINAL-COS-VALUE
    END-SEARCH.
```

These tables (from `SINE-OMEGA.CPY` and `COS-OMEGA.CPY`) contain pre-computed values for common angular frequencies.

#### Step 5: Calculate Alpha

```cobol
    COMPUTE ALPHA-VALUE ROUNDED =
     FINAL-SINE-VALUE / (2 * Q-RESONANCE).
```

**The alpha formula:**

```
α = sin(ω) / (2 × Q)
```

Alpha is the bandwidth parameter that controls how much the Q-factor affects the filter response.

### Biquad Coefficient Formulas

The standard biquad difference equation is:

```
y[n] = (b0×x[n] + b1×x[n-1] + b2×x[n-2] - a1×y[n-1] - a2×y[n-2]) / a0
```

Where:
- `x[n]` = current input sample
- `y[n]` = current output sample
- `x[n-1], x[n-2]` = previous input samples
- `y[n-1], y[n-2]` = previous output samples

#### Initial A-Coefficients (Common to All Filter Types)

```cobol
INIT-COEFFICIENTS.
    COMPUTE A0-COEFF ROUNDED = 1 + ALPHA-VALUE.
    COMPUTE A1-COEFF ROUNDED = -2 * FINAL-COS-VALUE.
    COMPUTE A2-COEFF ROUNDED = 1 - ALPHA-VALUE.
```

**Formulas:**
```
a0 = 1 + α
a1 = -2 × cos(ω)
a2 = 1 - α
```

#### B-Coefficients (Filter Type Dependent)

**Lowpass Filter (Type 1):**

```cobol
    WHEN 1
* LPF
        COMPUTE B1-COEFF ROUNDED = 1 - FINAL-COS-VALUE
        COMPUTE B0-COEFF ROUNDED = B1-COEFF / 2
        MOVE B0-COEFF TO B2-COEFF
```

**Formulas:**
```
b1 = 1 - cos(ω)
b0 = b1 / 2
b2 = b0
```

Simplified:
```
b0 = (1 - cos(ω)) / 2
b1 = 1 - cos(ω)
b2 = (1 - cos(ω)) / 2
```

**Highpass Filter (Type 2):**

```cobol
    WHEN 2
* HPF
        COMPUTE B1-COEFF ROUNDED = -1 * (1 + FINAL-COS-VALUE)
        COMPUTE B0-COEFF ROUNDED = B1-COEFF / -2
        MOVE B0-COEFF TO B2-COEFF
```

**Formulas:**
```
b1 = -(1 + cos(ω))
b0 = -b1 / 2
b2 = b0
```

Simplified:
```
b0 = (1 + cos(ω)) / 2
b1 = -(1 + cos(ω))
b2 = (1 + cos(ω)) / 2
```

**Bandpass Filter (Type 3):**

```cobol
    WHEN 3
* BPF
        COMPUTE B0-COEFF ROUNDED = Q-RESONANCE * ALPHA-VALUE
        MOVE 0 TO B1-COEFF
        COMPUTE B2-COEFF ROUNDED = -1 * B0-COEFF
```

**Formulas:**
```
b0 = Q × α
b1 = 0
b2 = -Q × α
```

Note: For bandpass, b0 and b2 are opposite signs, and b1 is zero.

#### Normalization by A0

To avoid division during sample processing, all coefficients are pre-divided by a0:

```cobol
* Normalize by A0
    COMPUTE B0-COEFF ROUNDED = B0-COEFF / A0-COEFF.
    COMPUTE B1-COEFF ROUNDED = B1-COEFF / A0-COEFF.
    COMPUTE B2-COEFF ROUNDED = B2-COEFF / A0-COEFF.
    COMPUTE A1-COEFF ROUNDED = A1-COEFF / A0-COEFF.
    COMPUTE A2-COEFF ROUNDED = A2-COEFF / A0-COEFF.
```

This transforms the difference equation to:

```
y[n] = b0×x[n] + b1×x[n-1] + b2×x[n-2] - a1×y[n-1] - a2×y[n-2]
```

(Note: a0 is now implicitly 1.0 and disappears from the equation)

### Applying the Filter to Each Sample

During audio generation, each interpolated sample passes through the filter:

```cobol
APPLY-FILTER.
* 1. Convert Float 0.0-1.0 to Signed Int 16-bit Scale for Filter Math
    COMPUTE SAMPLE-WORK-AREA = INTERP-RESULT * 32767.

* 2. Biquad Math
    COMPUTE FILTERED-SAMPLE ROUNDED =
        (SAMPLE-WORK-AREA * B0-COEFF) +
        (X1-INPUT * B1-COEFF) +
        (X2-INPUT * B2-COEFF) -
        (Y1-OUTPUT * A1-COEFF) -
        (Y2-OUTPUT * A2-COEFF).

* 3. Update Delay Lines
    MOVE X1-INPUT TO X2-INPUT
    MOVE SAMPLE-WORK-AREA TO X1-INPUT
    MOVE Y1-OUTPUT TO Y2-OUTPUT
    MOVE FILTERED-SAMPLE TO Y1-OUTPUT.

* 4. Virtual Analogue Mode (covered in Section 4)
    IF ANALOGUE-MODE
        PERFORM APPLY-BRANCHING-BIAS
        PERFORM APPLY-SOFT-SATURATION
        PERFORM APPLY-COMP-CASCADE
    END-IF.

* 5. Convert back to Float 0.0-1.0 for Envelope
    COMPUTE INTERP-RESULT = FILTERED-SAMPLE / 32767.0.
```

### Step-by-Step Filter Processing

**Step 1: Scale to Integer Range**

```
sample_work_area = interpolated_sample × 32767
```

The filter operates on integer-scaled values to avoid floating-point precision loss during fixed-point arithmetic.

**Step 2: Apply Biquad Equation**

```
y[n] = (x[n] × b0) + (x[n-1] × b1) + (x[n-2] × b2)
       - (y[n-1] × a1) - (y[n-2] × a2)
```

**Detailed breakdown:**

| Term | Meaning |
|------|---------|
| `SAMPLE-WORK-AREA * B0-COEFF` | Current input scaled by b0 |
| `X1-INPUT * B1-COEFF` | Previous input scaled by b1 |
| `X2-INPUT * B2-COEFF` | Two-samples-ago input scaled by b2 |
| `Y1-OUTPUT * A1-COEFF` | Previous output scaled by a1 (feedback) |
| `Y2-OUTPUT * A2-COEFF` | Two-samples-ago output scaled by a2 (feedback) |

The negative signs on the a-coefficients implement **feedback**—this is what makes it an IIR (infinite impulse response) filter with potentially infinite decay.

**Step 3: Update History Buffers**

```cobol
MOVE X1-INPUT TO X2-INPUT      * Shift x[n-1] → x[n-2]
MOVE SAMPLE-WORK-AREA TO X1-INPUT  * Store x[n] → x[n-1]
MOVE Y1-OUTPUT TO Y2-OUTPUT    * Shift y[n-1] → y[n-2]
MOVE FILTERED-SAMPLE TO Y1-OUTPUT  * Store y[n] → y[n-1]
```

This maintains the two-sample delay line needed for the next iteration.

**Step 4: Scale Back to Float**

```
interpolated_result = filtered_sample / 32767.0
```

Returns to normalized float range for envelope processing.

### Why Biquad Filters?

**Efficiency:** Second-order filters provide good frequency response with minimal computation—just 5 multiplies and 4 adds per sample.

**Versatility:** The same structure implements LP, HP, and BP filters by simply changing coefficients.

**Resonance control:** The Q parameter creates the characteristic "resonant peak" essential for synthesizer filters.

**Stability:** When properly designed (Q not too high), biquad filters are numerically stable and won't blow up or oscillate uncontrollably.

**Analog modeling:** Biquad topology closely matches the transfer function of analog state-variable and Sallen-Key filters used in classic synthesizers.

### Frequency Response Characteristics

**Lowpass (LPF):**
- Passes low frequencies unchanged
- Rolls off at -12 dB/octave above cutoff
- Resonance creates peak at cutoff frequency
- Use case: Remove high-frequency aliasing, create mellow tones

**Highpass (HPF):**
- Passes high frequencies unchanged
- Rolls off at -12 dB/octave below cutoff
- Resonance creates peak at cutoff frequency
- Use case: Remove DC offset, create bright/thin tones

**Bandpass (BPF):**
- Passes frequencies near cutoff
- Rolls off on both sides
- Narrower bandwidth with higher Q
- Use case: Vocal formants, "telephone" effect, resonant sweeps

### Example: Lowpass Filter at 1000 Hz with Q=1.5

Given:
- Cutoff = 1000 Hz
- Sample rate = 44100 Hz
- Q = 1.5

**Calculations:**

```
ω = 2π × (1000 / 44100) = 0.14251 rad/sample
sin(ω) = 0.14195
cos(ω) = 0.98987
α = 0.14195 / (2 × 1.5) = 0.04732
```

**Coefficients:**

```
a0 = 1 + 0.04732 = 1.04732
a1 = -2 × 0.98987 = -1.97974
a2 = 1 - 0.04732 = 0.95268

b1 = 1 - 0.98987 = 0.01013
b0 = 0.01013 / 2 = 0.00507
b2 = 0.00507
```

**After normalization:**

```
b0 = 0.00507 / 1.04732 = 0.00484
b1 = 0.01013 / 1.04732 = 0.00967
b2 = 0.00507 / 1.04732 = 0.00484
a1 = -1.97974 / 1.04732 = -1.89004
a2 = 0.95268 / 1.04732 = 0.90962
```

These coefficients create a lowpass filter that:
- Passes frequencies below 1000 Hz with minimal attenuation
- Attenuates frequencies above 1000 Hz at 12 dB/octave
- Has a resonant peak of ~3 dB at 1000 Hz (due to Q=1.5)

### Filter Math Precision

The coefficients use 8 decimal places of precision:

```cobol
05  B0-COEFF            PIC S9(3)V9(8).
```

This provides:
- Sufficient accuracy for filter stability
- Minimal coefficient quantization noise
- Predictable frequency response across the audio spectrum

The `ROUNDED` clause in coefficient calculations prevents accumulation of rounding errors across the audio generation process.




## 4. Simulates Analog Characteristics (Warmth, Drift, Saturation, Bit Reduction)

When "Virtual Analogue" mode is enabled, the synthesizer applies four types of non-linear processing after the digital filter to simulate the imperfections and character of analog hardware.

### Virtual Analogue Parameter Setup

```cobol
GET-FILTER-SETTINGS.
    DISPLAY " ".
    DISPLAY "--- FILTER ARCHITECTURE ---".
    DISPLAY "1. PURE DIGITAL (Clean)".
    DISPLAY "2. VIRTUAL ANALOGUE (Dirty)".
    ACCEPT OPERATION-MODE.
    
    IF ANALOGUE-MODE
        DISPLAY "Set Bias (0-100): "
        ACCEPT BIAS-INTENSITY
        DISPLAY "Set Drive (1-10): "
        ACCEPT USER-DRIVE-IN
        COMPUTE DRIVE-FACTOR = USER-DRIVE-IN / 2.0
        IF DRIVE-FACTOR < 1.0 MOVE 1.0 TO DRIVE-FACTOR END-IF
        
        DISPLAY "Set Drift (0-100): "
        ACCEPT USER-DRIFT-IN
        COMPUTE DRIFT-INTENSITY = USER-DRIFT-IN / 10000
        
        DISPLAY "Set Crush (1-2000): "
        ACCEPT USER-CRUSH-IN
        MOVE USER-CRUSH-IN TO CRUSH-FACTOR
    END-IF.
```

### Analog Parameter Data Structures

```cobol
01  ANALOGUE-PARAMS.
    05  BIAS-INTENSITY      PIC 9(3)    VALUE 0.
    05  DRIVE-FACTOR        PIC 9V9(2)  VALUE 1.0.
    05  DRIFT-INTENSITY     PIC 9V9(5)  VALUE 0.00000.
    05  CRUSH-FACTOR        PIC 9(4)    VALUE 1.

01  ANALOGUE-MATH.
    05  POS-FACTOR          PIC S9(3)V9(8).
    05  NEG-FACTOR          PIC S9(3)V9(8).
    05  UPPER-ADJ           PIC S9(12)V9(8).
    05  LOWER-ADJ           PIC S9(12)V9(8).
    05  TEMP-INT            PIC S9(15).
    05  TEMP-NORM           PIC S9(12)V9(8).
    05  TEMP-DRIVE          PIC S9(12)V9(8).
    05  TEMP-ABS            PIC 9(12)V9(8).
    05  TEMP-X2             PIC 9(12)V9(8).
    05  TEMP-Y              PIC S9(12)V9(8).

01  DRIFT-ENGINE.
    05  RANDOM-SEED         PIC 9(9)    VALUE 123456789.
    05  RANDOM-RESULT       PIC 9V9(8).
    05  DRIFT-AMOUNT        PIC S9V9(8).
```

### The Analog Processing Chain

After digital filtering, each sample passes through three stages when in analogue mode:

```cobol
APPLY-FILTER.
    * ...biquad filtering code...
    
* 4. Virtual Analogue Mode
    IF ANALOGUE-MODE
        PERFORM APPLY-BRANCHING-BIAS
        PERFORM APPLY-SOFT-SATURATION
        PERFORM APPLY-COMP-CASCADE
    END-IF.

* 5. Convert back to Float 0.0-1.0 for Envelope
    COMPUTE INTERP-RESULT = FILTERED-SAMPLE / 32767.0.
```

## Stage 1: Branching Bias (Asymmetric Amplification)

This simulates the asymmetric transfer characteristics of analog circuits, where positive and negative voltages amplify differently.

```cobol
APPLY-BRANCHING-BIAS.
    PERFORM GENERATE-DRIFT.
    COMPUTE POS-FACTOR = 1 +
        ((POSITIVE-EXPANSION - 1) * (BIAS-INTENSITY / 100)) +
        DRIFT-AMOUNT.
    COMPUTE NEG-FACTOR = 1 -
        ((1 - NEGATIVE-SQUASH) * (BIAS-INTENSITY / 100)) +
        DRIFT-AMOUNT.
    
    IF FILTERED-SAMPLE > 0
        COMPUTE FILTERED-SAMPLE = FILTERED-SAMPLE * POS-FACTOR
        IF FILTERED-SAMPLE > UPPER-ADJ
            COMPUTE FILTERED-SAMPLE =
            UPPER-ADJ + ((FILTERED-SAMPLE - UPPER-ADJ) * 0.4)
        END-IF
    ELSE
        COMPUTE FILTERED-SAMPLE = FILTERED-SAMPLE * NEG-FACTOR
        IF FILTERED-SAMPLE < LOWER-ADJ
            COMPUTE FILTERED-SAMPLE =
            LOWER-ADJ + ((FILTERED-SAMPLE - LOWER-ADJ) * 0.2)
        END-IF
    END-IF.
```

### Asymmetric Amplification Constants

```cobol
78  POSITIVE-EXPANSION  VALUE 1.082.
78  NEGATIVE-SQUASH     VALUE 0.850.
78  UPPER-THRESHOLD     VALUE 10000.
78  LOWER-THRESHOLD     VALUE -10000.
```

### The Mathematics of Branching Bias

**Positive samples** (above zero):
```
bias_amount = (1.082 - 1) × (intensity / 100) = 0.082 × (intensity / 100)
pos_factor = 1 + bias_amount + drift
sample_out = sample_in × pos_factor
```

**Negative samples** (below zero):
```
bias_amount = (1 - 0.850) × (intensity / 100) = 0.150 × (intensity / 100)
neg_factor = 1 - bias_amount + drift
sample_out = sample_in × neg_factor
```

**Example with BIAS-INTENSITY = 50:**

Positive factor:
```
pos_factor = 1 + (0.082 × 0.5) + drift = 1.041 + drift
```

Negative factor:
```
neg_factor = 1 - (0.150 × 0.5) + drift = 0.925 + drift
```

This means:
- **Positive peaks expand by 4.1%** (plus drift)
- **Negative peaks compress by 7.5%** (plus drift)

### Soft Clipping at Extremes

When samples exceed thresholds (±10,000 in 16-bit scale), they soft-clip asymmetrically:

**Positive overshoot:**
```
if sample > upper_threshold:
    sample = threshold + ((sample - threshold) × 0.4)
```

This compresses the overshoot to 40% of its excess, creating a gentle saturation curve.

**Negative overshoot:**
```
if sample < lower_threshold:
    sample = threshold + ((sample - threshold) × 0.2)
```

Negative overshoots compress to 20%—**more aggressive clipping** on the negative side, mimicking transistor/tube asymmetry.

### Why Asymmetric Bias?

**Even harmonic generation:** Asymmetric processing adds even-order harmonics (2nd, 4th, 6th), which sound musically pleasing—like the warmth of tubes or tape saturation.

**Analog authenticity:** Real analog circuits have asymmetric transfer functions due to component tolerances, power supply sag, and transistor characteristics.

**Controlled distortion:** At moderate settings (bias 30-50), this adds subtle harmonic richness without obvious distortion. At high settings (70-100), it creates aggressive asymmetric clipping.

## Stage 2: Drift Generation (Random Modulation)

This adds subtle random variations to simulate analog component drift, power supply fluctuations, and thermal noise.

```cobol
GENERATE-DRIFT.
    COMPUTE RANDOM-SEED =
        FUNCTION
        MOD((1103515245 * RANDOM-SEED + 12345), 2147483647).
    
    COMPUTE RANDOM-RESULT = RANDOM-SEED / 2147483647.
    COMPUTE DRIFT-AMOUNT =
    (RANDOM-RESULT - 0.5) * DRIFT-INTENSITY.
```

### Linear Congruential Generator (LCG)

This uses the **POSIX standard LCG algorithm** for pseudo-random number generation:

```
seed_next = (1103515245 × seed + 12345) mod 2^31-1
```

**Constants:**
- **Multiplier:** 1103515245 (chosen for long period and good statistical properties)
- **Increment:** 12345
- **Modulus:** 2,147,483,647 (2^31 - 1, a Mersenne prime)

### Normalization and Centering

```
random_result = seed / 2147483647        (range: 0.0 to 1.0)
drift_amount = (random_result - 0.5) × intensity   (range: -intensity/2 to +intensity/2)
```

**Example with DRIFT-INTENSITY = 0.01 (user input 100):**

```
drift_amount ranges from -0.005 to +0.005
```

This ±0.5% random variation is added to both pos_factor and neg_factor, creating:
- Random amplitude modulation
- Subtle pitch wobble (when combined with bias)
- Noise floor similar to analog circuits

### Drift Application

The drift is recalculated **once per sample** and added to the bias factors:

```
pos_factor = base_expansion + drift
neg_factor = base_squash + drift
```

Since drift changes randomly each sample at audio rate (44.1 kHz), it creates:
- **High-frequency noise** (like thermal noise in resistors)
- **Low-frequency wandering** (due to LCG pseudo-randomness not being truly white)
- **Analog "liveliness"** (signal never perfectly stable)

### Why Use LCG Instead of True Random?

**Deterministic:** Same seed produces same noise pattern, allowing reproducible renders.

**Fast:** Just one multiply, one add, one modulo—minimal CPU cost.

**Good enough:** For audio applications, the pattern repeats every ~2 billion samples (about 13 hours at 44.1kHz), far longer than typical synthesis durations.

## Stage 3: Soft Saturation (Harmonic Distortion)

This applies a smooth, continuous non-linearity that mimics analog saturation (tape, tubes, transistors).

```cobol
APPLY-SOFT-SATURATION.
* Normalize to -1..1
    COMPUTE TEMP-NORM = FILTERED-SAMPLE / 32768.0.
    COMPUTE TEMP-DRIVE = TEMP-NORM * DRIVE-FACTOR.
    COMPUTE TEMP-ABS = FUNCTION ABS(TEMP-DRIVE).
    COMPUTE TEMP-X2  = TEMP-DRIVE * TEMP-DRIVE.
    COMPUTE TEMP-Y   = 1.0 - (1.0 / (1.0 + TEMP-ABS + TEMP-X2 +
                   0.66422417311781 * TEMP-X2 * TEMP-ABS +
                   0.36483285408241 * TEMP-X2 * TEMP-X2)).
    IF TEMP-DRIVE < 0
        COMPUTE TEMP-Y = -1 * TEMP-Y
    END-IF.
    COMPUTE FILTERED-SAMPLE = TEMP-Y * 32768.0.
```

### The Saturation Transfer Function

This uses a **polynomial approximation** of a hyperbolic tangent-like curve:

```
x_driven = x × drive_factor
y = 1 - (1 / (1 + |x| + x² + 0.664·x²·|x| + 0.365·x⁴))
```

If input is negative, flip the sign:
```
if x < 0: y = -y
```

### Mathematical Breakdown

**Terms in the denominator:**

| Term | Meaning | Effect |
|------|---------|--------|
| 1 | Constant | Base curve |
| \|x\| | Linear | Gentle initial compression |
| x² | Quadratic | Symmetric saturation |
| 0.664·x²·|x| | Cubic | Asymmetric harmonics |
| 0.365·x⁴ | Quartic | Hard limiting at extremes |

The **magic constants** (0.66422417311781 and 0.36483285408241) are empirically derived to create a pleasing saturation curve that:
- Starts soft (gentle compression at low levels)
- Transitions smoothly (no sudden kinks or discontinuities)
- Hard limits at extremes (approaches ±1.0 asymptotically)
- Generates musically pleasant odd harmonics

### Transfer Curve Behavior

| Input (x) | Output (y) | Compression |
|-----------|------------|-------------|
| 0.0 | 0.0 | None |
| 0.5 | ~0.47 | Slight |
| 1.0 | ~0.80 | Moderate |
| 2.0 | ~0.95 | Heavy |
| 5.0 | ~0.995 | Extreme |
| ∞ | 1.0 (limit) | Total |

The function **never exceeds ±1.0**, preventing clipping while adding harmonics.

### Drive Factor Scaling

```cobol
COMPUTE DRIVE-FACTOR = USER-DRIVE-IN / 2.0
```

User input 1-10 maps to drive factor 0.5-5.0:

- **Drive = 1.0 (user 2):** Unity gain, minimal saturation
- **Drive = 2.5 (user 5):** Moderate saturation, "warm" tone
- **Drive = 5.0 (user 10):** Heavy saturation, aggressive distortion

Higher drive pushes the signal into the non-linear region, generating more harmonics.

### Why This Saturation Curve?

**Smooth and continuous:** No harsh clipping edges that cause harsh, metallic distortion.

**Asymptotic limiting:** Naturally prevents clipping without brick-wall limiting artifacts.

**Harmonic richness:** The polynomial generates odd harmonics (3rd, 5th, 7th) characteristic of symmetric waveshaping, plus even harmonics from the |x| terms.

**CPU efficient:** Pure arithmetic—no expensive transcendental functions like tanh() or sin().

## Stage 4: Bit Crushing (Sample Quantization)

This reduces bit depth by quantizing the signal to coarser steps, simulating early digital samplers and lo-fi A/D converters.

```cobol
APPLY-COMP-CASCADE.
    IF CRUSH-FACTOR > 1
        COMPUTE TEMP-INT = FILTERED-SAMPLE / CRUSH-FACTOR
        COMPUTE FILTERED-SAMPLE = TEMP-INT * CRUSH-FACTOR
    END-IF.
```

### Quantization Mathematics

**The process:**
1. Divide sample value by crush factor (truncating to integer)
2. Multiply back by crush factor

This rounds the sample to the nearest multiple of `CRUSH-FACTOR`.

**Example with CRUSH-FACTOR = 256:**

```
Original: 12847 (16-bit value)
Step 1: 12847 / 256 = 50.18... → 50 (integer truncation)
Step 2: 50 × 256 = 12800
Result: 12800 (quantized to nearest 256)
```

### Effective Bit Depth Reduction

The relationship between crush factor and bit depth:

```
effective_bits = 16 - log₂(crush_factor)
```

| Crush Factor | Effective Bits | Sound Character |
|--------------|----------------|-----------------|
| 1 | 16-bit | Full resolution, clean |
| 2 | 15-bit | Subtle crunch |
| 8 | 13-bit | Early CD quality |
| 16 | 12-bit | Vintage sampler (Akai, E-mu) |
| 64 | 10-bit | Lo-fi, gritty |
| 256 | 8-bit | Classic chip music |
| 512 | 7-bit | Severely degraded |
| 2048 | 5-bit | Extreme lo-fi |

### Why Crush Instead of Bit Masking?

**Smooth degradation:** Dividing and multiplying creates harmonic distortion that sounds musical.

**Predictable steps:** The quantization grid is uniform across the full amplitude range.

**Interaction with drive:** When combined with saturation, bit crushing creates complex intermodulation distortion—the signature sound of overdriven early samplers.

### Truncation vs. Rounding

COBOL's integer division **truncates toward zero**, creating:
- **Rectification effect** for small signals
- **DC offset** for asymmetric waveforms
- **Additional even harmonics** from the non-linear quantization

This is actually more "authentic" than proper rounding—early digital hardware often used truncation for speed.

## Combined Analog Pipeline Example

**Settings:**
- Bias Intensity = 60
- Drive = 8 (factor = 4.0)
- Drift = 40 (intensity = 0.004)
- Crush = 128

**Sample value = 15,000 (filtered)**

### Step 1: Branching Bias
```
pos_factor = 1 + (0.082 × 0.6) + drift
pos_factor = 1.0492 + (random -0.002 to +0.002)
pos_factor ≈ 1.051 (assume drift = +0.002 this sample)
sample = 15000 × 1.051 = 15,765
```

### Step 2: Soft Saturation
```
normalized = 15765 / 32768 = 0.481
driven = 0.481 × 4.0 = 1.924
y = 1 - (1 / (1 + 1.924 + 3.702 + 4.622 + 13.705))
y = 1 - (1 / 24.953) = 1 - 0.0401 = 0.960
sample = 0.960 × 32768 = 31,457
```

### Step 3: Bit Crushing
```
crushed = (31457 / 128) × 128
crushed = 245 × 128 = 31,360
```

**Final output: 31,360** (from input 15,000)

The signal has been:
- Expanded by 5.1% (bias)
- Saturated heavily (drive 4.0)
- Quantized to 9-bit resolution (crush 128)

## Analog Mode Benefits

**Warmth:** Even harmonics from asymmetric bias sound pleasing and "full"

**Movement:** Drift prevents the sterile static quality of pure digital

**Saturation:** Soft clipping allows "louder" signals without harsh digital clipping

**Character:** Bit crushing adds vintage digital character, not just degradation

**Interaction:** The four stages interact non-linearly, creating complex harmonic structures impossible with any single effect

## When to Use Analog Mode

**Bass sounds:** Saturation and bias add weight and presence

**Leads:** Drive and crush create aggressive, cutting tones

**Pads:** Subtle bias (20-40) and drift (10-30) add organic movement

**Lo-fi aesthetics:** Heavy crush (256-2048) for chip music or vintage sampler vibes

**Clean sounds:** Set all to minimum or use Pure Digital mode for pristine quality

The virtual analogue system transforms mathematically perfect digital synthesis into something with the character, imperfection, and musicality of analog hardware—all through carefully designed COBOL fixed-point arithmetic.




## 5. Shapes Amplitude with Time Variant Envelopes

The synthesizer uses a **JD-800 style envelope** with five stages and three independent level points, providing more control than traditional ADSR envelopes. Each stage generates samples with a linearly ramping amplitude multiplier.

### JD Envelope Structure

Unlike ADSR (Attack-Decay-Sustain-Release), the JD envelope has:
- **Three level points** (L1, L2, L3) instead of just a sustain level
- **Five time-based stages** including two decay stages
- **Timed sustain** instead of indefinite hold

```cobol
01  JD-PARAMS.
    05  L1              PIC 9(3).
    05  L2              PIC 9(3).
    05  L3              PIC 9(3).
    05  T1              PIC 9(2)V9(2).
    05  T2              PIC 9(2)V9(2).
    05  T3              PIC 9(2)V9(2).
    05  T-SUSTAIN       PIC 9(2)V9(2).
    05  T4              PIC 9(2)V9(2).
```

**Parameters:**
- **L1, L2, L3:** Level points (0-100%)
- **T1:** Attack time (seconds)
- **T2:** Decay 1 time (seconds)
- **T3:** Decay 2 time (seconds)
- **T-SUSTAIN:** Sustain hold time (seconds)
- **T4:** Release time (seconds)

### Envelope Input Interface

```cobol
GET-ENVELOPE-SETTINGS.
    DISPLAY " ".
    DISPLAY "--- JD-800 ENVELOPE ---".
    DISPLAY "T1 (Attack Time): " ACCEPT T1.
    DISPLAY "L1 (Attack Lvl 0-100): " ACCEPT L1.
    DISPLAY "T2 (Decay1 Time): " ACCEPT T2.
    DISPLAY "L2 (Break Lvl 0-100): " ACCEPT L2.
    DISPLAY "T3 (Decay2 Time): " ACCEPT T3.
    DISPLAY "L3 (Sustain Lvl 0-100): " ACCEPT L3.
    DISPLAY "Sus Time (Duration): " ACCEPT T-SUSTAIN.
    DISPLAY "T4 (Release Time): " ACCEPT T4.
```

### The Five Envelope Stages

The main program executes each stage sequentially:

```cobol
* STAGE 1: ATTACK (0 to L1 over T1)
    MOVE 0 TO START-VOLUME.
    MOVE L1  TO END-VOLUME.
    MOVE T1  TO DURATION-SECONDS.
    PERFORM RUN-ENVELOPE-STAGE.

* STAGE 2: DECAY 1 (L1 to L2 over T2)
    MOVE L1  TO START-VOLUME.
    MOVE L2  TO END-VOLUME.
    MOVE T2  TO DURATION-SECONDS.
    PERFORM RUN-ENVELOPE-STAGE.

* STAGE 3: DECAY 2 (L2 to L3 over T3)
    MOVE L2  TO START-VOLUME.
    MOVE L3  TO END-VOLUME.
    MOVE T3  TO DURATION-SECONDS.
    PERFORM RUN-ENVELOPE-STAGE.

* STAGE 4: SUSTAIN (Hold L3 over T-SUSTAIN)
    MOVE L3        TO START-VOLUME.
    MOVE L3        TO END-VOLUME.
    MOVE T-SUSTAIN TO DURATION-SECONDS.
    PERFORM RUN-ENVELOPE-STAGE.

* STAGE 5: RELEASE (L3 to 0 over T4)
    MOVE L3  TO START-VOLUME.
    MOVE 0 TO END-VOLUME.
    MOVE T4  TO DURATION-SECONDS.
    PERFORM RUN-ENVELOPE-STAGE.
```

### Stage Execution Logic

Each stage calculates how many samples to generate and the amplitude step per sample:

```cobol
RUN-ENVELOPE-STAGE.
* 1. Determine how many actual samples this stage lasts
    COMPUTE TOTAL-SAMPLES = DURATION-SECONDS * SAMPLE-RATE.

* 2. Calculate Volume Ramp
* We use 0.0-1.0 range internally for cleaner math
    COMPUTE START-AMP = START-VOLUME / 100.0.
    COMPUTE END-AMP   = END-VOLUME / 100.0.

* Handle zero-duration stages
    IF TOTAL-SAMPLES = 0 OR TOTAL-SAMPLES < 1
        MOVE END-AMP TO RUNNING-AMP
        EXIT PARAGRAPH
    END-IF.

    COMPUTE AMP-STEP  = (END-AMP - START-AMP) / TOTAL-SAMPLES.
    MOVE START-AMP TO RUNNING-AMP.

* 3. The Generation Loop for this duration
    PERFORM GENERATE-SAMPLE-BLOCK
        VARYING OUTPUT-COUNT FROM 1 BY 1
        UNTIL OUTPUT-COUNT > TOTAL-SAMPLES.
```

### Envelope Mathematics

**Sample count calculation:**
```
total_samples = duration_seconds × sample_rate
```

**Example:** T1 = 0.5 seconds at 44,100 Hz:
```
total_samples = 0.5 × 44100 = 22,050 samples
```

**Amplitude normalization:**
```
start_amp = start_volume / 100.0
end_amp = end_volume / 100.0
```

This converts 0-100 range to 0.0-1.0 for multiplication.

**Step size calculation:**
```
amp_step = (end_amp - start_amp) / total_samples
```

**Example:** Attack from 0% to 80% over 0.5 seconds:
```
start_amp = 0.0
end_amp = 0.8
total_samples = 22,050
amp_step = (0.8 - 0.0) / 22050 = 0.000036281
```

Each sample increases amplitude by ~0.0036%.

### Per-Sample Amplitude Application

Inside `GENERATE-SAMPLE-BLOCK`, after all audio processing:

```cobol
APPLY-VOLUME-AND-WRITE.
* 1. Apply Envelope Volume
    COMPUTE INTERP-RESULT = INTERP-RESULT * RUNNING-AMP.
* 2. Advance Envelope
    ADD AMP-STEP TO RUNNING-AMP.
* 3. Convert Float (-1.0 to 1.0) back to PCM Integer
    COMPUTE SCALED-SAMPLE = INTERP-RESULT * 32767.
    ...write to file...
```

**Progression example** for the attack stage above:

| Sample | RUNNING-AMP | Effect |
|--------|-------------|--------|
| 1 | 0.000000 | Silence |
| 100 | 0.003628 | Very quiet |
| 1000 | 0.036281 | Audible |
| 5000 | 0.181405 | Growing |
| 11025 | 0.400000 | Half volume |
| 22050 | 0.800000 | Full attack level |

The amplitude multiplier increases linearly from 0.0 to 0.8 over 22,050 samples.

### Stage Variable Storage

```cobol
01  STAGE-MATH.
    05  DURATION-SECONDS PIC 9(2)V9(2).
    05  TOTAL-SAMPLES    PIC 9(9).
    05  START-VOLUME     PIC 9(3).
    05  END-VOLUME       PIC 9(3).
    05  START-AMP        PIC S9(5)V9(10).
    05  END-AMP          PIC S9(5)V9(10).
    05  AMP-STEP         PIC S9(5)V9(10).
    05  RUNNING-AMP      PIC S9(5)V9(10).
    05  MAX-AMP          PIC 9(5) VALUE 32767.
```

**High precision:** 10 decimal places for `AMP-STEP` ensures smooth, click-free envelopes even for very long stages.

## JD Envelope Shapes and Use Cases

### Classic Organ Sound
```
L1 = 100, T1 = 0.01  (instant attack to full)
L2 = 80,  T2 = 0.05  (slight dip)
L3 = 80,  T3 = 0.0   (no second decay)
T-SUSTAIN = 2.0      (held 2 seconds)
T4 = 0.1             (quick release)
```

The slight L1→L2 dip creates the characteristic organ "click" from key contact.

### Percussive Pluck
```
L1 = 100, T1 = 0.001 (immediate attack)
L2 = 30,  T2 = 0.3   (fast decay)
L3 = 5,   T3 = 0.7   (slow decay to tail)
T-SUSTAIN = 0.0      (no sustain)
T4 = 0.2             (natural release)
```

Three-stage decay (L1→L2→L3) creates realistic plucked string envelope.

### Pad with Swell
```
L1 = 60,  T1 = 1.5   (slow attack to 60%)
L2 = 100, T2 = 2.0   (swell to full)
L3 = 80,  T3 = 1.0   (gentle decay)
T-SUSTAIN = 5.0      (long hold)
T4 = 3.0             (slow fade)
```

The L1→L2 rise creates an evolving pad that swells after initial attack.

### Percussive Bass
```
L1 = 100, T1 = 0.005 (sharp attack)
L2 = 0,   T2 = 0.15  (immediate silence)
L3 = 0,   T3 = 0.0   (no further decay)
T-SUSTAIN = 0.0      (no sustain)
T4 = 0.0             (no release)
```

One-shot envelope for kick drums or stabs.

## Advantages of JD Envelopes Over ADSR

**Three breakpoints:** L1, L2, L3 allow complex shapes impossible with single decay stage.

**Timed sustain:** Automatically generates complete waveforms without user interaction—perfect for pre-rendering.

**Two-stage decay:** Models realistic acoustic instruments (initial transient + body resonance + tail).

**Organ simulation:** Can create the attack "click" and keyclick artifacts of tonewheel organs.

**Backwards compatibility:** Can emulate ADSR by setting L2 = L3 (collapsing decay stages).

## Envelope Precision and Artifacts

**Linear ramps:** All stages use linear interpolation, not exponential curves. This:
- Simplifies computation (no logarithms needed)
- Creates slightly "digital" character
- Works well for short envelopes (<1 second)
- May sound mechanical for very long attacks/releases

**10 decimal places:** The `PIC S9(5)V9(10)` precision for `AMP-STEP` prevents:
- Zipper noise (audible stepping)
- Cumulative rounding errors over long stages
- Amplitude drift

**Integer sample counts:** `TOTAL-SAMPLES` is integer, so:
- Very short times may quantize (0.01s = 441 samples minimum at 44.1kHz)
- Fractional sample counts are truncated, not rounded
- This is acceptable for audio applications

## Real-World Envelope Calculation Example

**Settings:**
- T1 = 0.3 seconds, L1 = 90%
- T2 = 0.5 seconds, L2 = 60%
- T3 = 1.0 seconds, L3 = 40%
- T-SUSTAIN = 3.0 seconds
- T4 = 2.0 seconds

**Stage 1 (Attack):**
```
total_samples = 0.3 × 44100 = 13,230
start_amp = 0.0
end_amp = 0.9
amp_step = (0.9 - 0.0) / 13230 = 0.000068027
```

Generates 13,230 samples ramping from silence to 90%.

**Stage 2 (Decay 1):**
```
total_samples = 0.5 × 44100 = 22,050
start_amp = 0.9
end_amp = 0.6
amp_step = (0.6 - 0.9) / 22050 = -0.000013605
```

Generates 22,050 samples ramping down from 90% to 60%.

**Stage 3 (Decay 2):**
```
total_samples = 1.0 × 44100 = 44,100
start_amp = 0.6
end_amp = 0.4
amp_step = (0.4 - 0.6) / 44100 = -0.000004535
```

Generates 44,100 samples ramping down from 60% to 40%.

**Stage 4 (Sustain):**
```
total_samples = 3.0 × 44100 = 132,300
start_amp = 0.4
end_amp = 0.4
amp_step = 0.0
```

Generates 132,300 samples at constant 40% amplitude.

**Stage 5 (Release):**
```
total_samples = 2.0 × 44100 = 88,200
start_amp = 0.4
end_amp = 0.0
amp_step = (0.0 - 0.4) / 88200 = -0.000004535
```

Generates 88,200 samples ramping down from 40% to silence.

**Total duration:** 6.8 seconds
**Total samples:** 299,880 samples

## Integration with Audio Pipeline

The envelope is applied **after all processing** but **before PCM conversion**:

```
Sample Flow:
1. Wavetable read → interpolated float (-1.0 to +1.0)
2. Biquad filter → filtered float
3. Analog processing (if enabled) → processed float
4. Envelope multiplication → final float
5. PCM conversion → 16-bit integer
6. Write to file
```

This order ensures:
- Filter operates on full-amplitude signal (optimal SNR)
- Analog processing shapes timbre before dynamics
- Envelope controls final output level smoothly
- No clipping from envelope overshoots (float multiplication can't clip)

## Why Time-Variant Envelopes Matter

**Dynamic shaping:** Without envelopes, all sounds are static drones. Envelopes create:
- Percussive attacks
- Sustained pads
- Decaying plucks
- Expressive dynamics

**Acoustic modeling:** Real instruments have complex amplitude envelopes:
- Piano: fast attack, multi-stage decay
- Strings: slow attack, long sustain
- Brass: slow attack with swell, sustained tone

**Musical expression:** Envelope shape is as important as timbre—same waveform with different envelopes sounds like different instruments.

**Pre-rendered synthesis:** Since this COBOL synth generates complete audio files (not real-time), timed sustain is essential. ADSR's indefinite sustain would require user intervention to release notes.

The JD envelope system brings the time-variant control of professional synthesizers to COBOL, turning static waveforms into expressive, dynamic sounds with complex amplitude evolution.




## 6. Uses High-Quality Interpolation to Prevent Aliasing

**Note:** Interpolation methods are fully covered in Section 2 (Pitch Shifting via Variable-Speed Playback).

The synthesizer offers five interpolation modes:

1. **VINTAGE** - Nearest-neighbor (no interpolation) for extreme lo-fi aliasing
2. **MODERN** - Linear interpolation for clean, smooth output
3. **TAPE** - 50% grit linear for controlled aliasing character
4. **HI-FI** - Windowed sinc interpolation for pristine, band-limited quality
5. **HYBRID** - Sinc with grit for high-quality aliasing effects

### Why Interpolation Matters

When reading the wavetable at fractional positions (e.g., position 21.4308), the program must construct a sample value between discrete table entries. Without interpolation:

**Nearest-neighbor** (Mode 1):
```
sample = wave[21]  (ignores .4308 fractional part)
```
- Creates stair-stepping artifacts
- Generates high-frequency aliasing
- Sounds gritty and lo-fi

**Linear interpolation** (Modes 2-3):
```
sample = wave[21] × 0.5692 + wave[22] × 0.4308
```
- Smooth transitions between samples
- Minimal aliasing
- Slight high-frequency roll-off

**Sinc interpolation** (Modes 4-5):
```
sample = Σ(wave[21+k] × sinc_weight[k])  for k = -4 to +4
```
- Perfect band-limited reconstruction
- No aliasing
- Preserves all frequencies below Nyquist

### Aliasing Explained

When pitch-shifting upward, the read position advances quickly through the wavetable. Without proper interpolation, this creates **aliasing**—false frequencies that "fold back" into the audible spectrum.

**Example:** Playing a 440 Hz waveform at 880 Hz (one octave up):
- Read position advances 2× as fast
- Without interpolation: high-frequency artifacts appear
- With sinc interpolation: clean octave transposition

For detailed mathematics, implementation, and COBOL code, see **Section 2: Advanced Interpolation Methods**.




## 7. Outputs Processed Audio as Raw PCM Data

After all processing and envelope shaping, each sample is converted from floating-point back to 16-bit signed PCM and written to a headerless .raw file.

### Output File Configuration

The output file is defined in the ENVIRONMENT DIVISION:

```cobol
SELECT OUT-FILE ASSIGN TO
"your file path here"
ORGANIZATION IS SEQUENTIAL
ACCESS MODE IS SEQUENTIAL.
```

**File characteristics:**
- **Sequential access:** Samples written one after another in order
- **No header:** Pure audio data, no metadata
- **Binary format:** Raw bytes, not text

### PCM Output Variables

```cobol
01  PCM-WRITING.
    05  SCALED-SAMPLE   PIC S9(9) BINARY.
    05  LOW-BYTE-VAL    PIC 9(3).
    05  HIGH-BYTE-VAL   PIC 9(3).
```

**Data types:**
- `SCALED-SAMPLE`: Signed 16-bit integer stored as binary
- `LOW-BYTE-VAL`, `HIGH-BYTE-VAL`: Individual bytes for little-endian encoding

### The Output Conversion Process

After envelope multiplication, samples are still in normalized floating-point range (-1.0 to +1.0). The final step converts to 16-bit PCM:

```cobol
APPLY-VOLUME-AND-WRITE.
* 1. Apply Envelope Volume
    COMPUTE INTERP-RESULT = INTERP-RESULT * RUNNING-AMP.
* 2. Advance Envelope
    ADD AMP-STEP TO RUNNING-AMP.
* 3. Convert Float (-1.0 to 1.0) back to PCM Integer
    COMPUTE SCALED-SAMPLE = INTERP-RESULT * 32767.
    
    IF SCALED-SAMPLE < 0
        ADD 65536 TO SCALED-SAMPLE
    END-IF.
* 4. Write Bytes
    COMPUTE LOW-BYTE-VAL = FUNCTION MOD(SCALED-SAMPLE, 256).
    MOVE FUNCTION CHAR(LOW-BYTE-VAL + 1) TO RAW-BYTES.
    WRITE RAW-BYTES.
    
    COMPUTE HIGH-BYTE-VAL = SCALED-SAMPLE / 256.
    MOVE FUNCTION CHAR(HIGH-BYTE-VAL + 1) TO RAW-BYTES.
    WRITE RAW-BYTES.
```

## Step-by-Step Conversion

### Step 1: Envelope Multiplication

```
final_sample = processed_sample × running_amplitude
```

**Example:** 
- Processed sample = 0.8 (after filtering and analog processing)
- Running amplitude = 0.6 (60% envelope level)
- Final sample = 0.8 × 0.6 = 0.48

### Step 2: Envelope Advancement

```
running_amplitude = running_amplitude + amp_step
```

This prepares the envelope for the next sample. Each sample increments the amplitude by the pre-calculated step size.

### Step 3: Float to 16-bit Integer Conversion

```
scaled_sample = final_sample × 32767
```

This maps the normalized float range to signed 16-bit integer range:

| Float Value | Integer Value | Meaning |
|-------------|---------------|---------|
| +1.0 | +32,767 | Maximum positive |
| +0.5 | +16,383 | Half positive |
| 0.0 | 0 | Silence |
| -0.5 | -16,384 | Half negative |
| -1.0 | -32,768 | Maximum negative |

**Example:** 0.48 × 32767 = 15,728

### Step 4: Two's Complement Conversion

For negative values, convert to unsigned representation for byte writing:

```cobol
IF SCALED-SAMPLE < 0
    ADD 65536 TO SCALED-SAMPLE
END-IF.
```

**Why this works:** Signed 16-bit integers use two's complement representation:

| Signed | Unsigned | Binary |
|--------|----------|--------|
| +32,767 | 32,767 | 0111111111111111 |
| +1 | 1 | 0000000000000001 |
| 0 | 0 | 0000000000000000 |
| -1 | 65,535 | 1111111111111111 |
| -32,768 | 32,768 | 1000000000000000 |

Adding 65,536 to negative values converts them to their unsigned representation without changing the bit pattern.

**Example:** Value = -5,000
```
-5000 + 65536 = 60,536
```

In binary, both represent the same 16 bits.

### Step 5: Little-Endian Byte Extraction

16-bit values are stored as **two bytes in little-endian order** (least significant byte first):

**Low byte extraction:**
```cobol
COMPUTE LOW-BYTE-VAL = FUNCTION MOD(SCALED-SAMPLE, 256).
```

This isolates the least significant 8 bits.

**Example:** 15,728 (0x3D70 in hex)
```
15728 mod 256 = 112 (0x70)
```

**High byte extraction:**
```cobol
COMPUTE HIGH-BYTE-VAL = SCALED-SAMPLE / 256.
```

Integer division discards the remainder, leaving the most significant 8 bits.

**Example:**
```
15728 / 256 = 61 (0x3D)
```

### Step 6: Character Encoding and Writing

COBOL's `WRITE` statement expects character data, so numeric bytes must be converted:

```cobol
MOVE FUNCTION CHAR(LOW-BYTE-VAL + 1) TO RAW-BYTES.
WRITE RAW-BYTES.

MOVE FUNCTION CHAR(HIGH-BYTE-VAL + 1) TO RAW-BYTES.
WRITE RAW-BYTES.
```

**Why +1?** COBOL's `CHAR` function uses 1-based indexing:
- `CHAR(1)` = ASCII 0 (null)
- `CHAR(2)` = ASCII 1
- `CHAR(113)` = ASCII 112 (lowercase 'p')

So byte value 112 becomes `CHAR(113)`.

## Complete Conversion Example

**Input:** Normalized float value = -0.6543

### Step 1: Scale to 16-bit
```
scaled = -0.6543 × 32767 = -21,438.5 → -21,439 (rounded)
```

### Step 2: Convert to Unsigned
```
unsigned = -21439 + 65536 = 44,097
```

### Step 3: Extract Bytes
```
low_byte = 44097 mod 256 = 65
high_byte = 44097 / 256 = 172
```

### Step 4: Verify (in hex)
```
44097 decimal = 0xAC41 hex
Low byte: 0x41 = 65 ✓
High byte: 0xAC = 172 ✓
```

### Step 5: Write to File
```
Byte 1: CHAR(66)  = character with code 65
Byte 2: CHAR(173) = character with code 172
```

The file now contains two bytes: `0x41 0xAC` (little-endian for -21,439)

## Output File Structure

The output file is a sequence of **sample pairs** (two bytes each):

```
[low₁][high₁][low₂][high₂][low₃][high₃]...[lowₙ][highₙ]
```

**File size calculation:**
```
file_size_bytes = total_samples × 2
```

**Example:** 3-second audio at 44.1kHz:
```
samples = 3 × 44100 = 132,300
file_size = 132,300 × 2 = 264,600 bytes (≈ 258 KB)
```

## Why Headerless PCM?

**Simplicity:** No complex file format parsing or writing—just raw sample data.

**Flexibility:** Can be imported into any audio software that supports raw import (Audacity, Adobe Audition, etc.).

**Debugging:** Easy to inspect with hex editors or analyze with custom tools.

**Efficiency:** No overhead—every byte is actual audio data.

**Compatibility:** Universal format that works across platforms and software.

## Playback Requirements

To play the output file, the user must manually specify:

**Format parameters:**
- **Sample rate:** 44,100 Hz (fixed in this synthesizer)
- **Bit depth:** 16-bit signed integer
- **Channels:** Mono (1 channel)
- **Byte order:** Little-endian

**In Audacity:**
1. File → Import → Raw Data
2. Set encoding: "Signed 16-bit PCM"
3. Set byte order: "Little-endian"
4. Set channels: "1 Channel (Mono)"
5. Set sample rate: "44100 Hz"

## Output File Closure

After all envelope stages complete, the file is closed:

```cobol
CLOSE OUT-FILE.
DISPLAY "Done. Output.raw created.".
STOP RUN.
```

This ensures:
- All buffered data is flushed to disk
- File handle is released
- Operating system finalizes the file

## Quality Considerations

**Bit depth limitation:** 16-bit provides 96 dB dynamic range—sufficient for most music, but lower than modern 24-bit standards.

**No dithering:** Direct truncation to 16-bit can introduce quantization noise. Professional systems add shaped noise (dithering) to mask this.

**Clipping prevention:** The normalized float range (-1.0 to +1.0) ensures samples never exceed ±32,767, preventing digital clipping.

**DC offset:** The synthesizer doesn't explicitly remove DC offset. If bias or drift creates asymmetry, a small DC component may exist.

## Advanced: Bit Pattern Example

**Sample value:** +12,345 (0x3039 hex)

**Little-endian byte representation:**
```
Byte 1 (low):  0x39 = 57 decimal
Byte 2 (high): 0x30 = 48 decimal
```

**File hex dump:**
```
39 30 [next sample bytes]
```

**Playback reconstruction:**
```
unsigned = (48 × 256) + 57 = 12,288 + 57 = 12,345
signed = 12,345 (no conversion needed, positive)
```

The audio software reads the two bytes in little-endian order and reconstructs the original 16-bit signed value.

## Output Summary

The PCM writing system:
1. **Scales** normalized float to signed 16-bit integer
2. **Converts** negative values to unsigned representation
3. **Extracts** low and high bytes (little-endian)
4. **Encodes** bytes as characters for COBOL I/O
5. **Writes** sequential byte pairs to create raw audio file

This completes the synthesis pipeline—from wavetable oscillation through filtering, analog processing, envelope shaping, and final PCM encoding. The result is a headerless audio file ready for import into any standard audio software for playback, editing, or further processing.