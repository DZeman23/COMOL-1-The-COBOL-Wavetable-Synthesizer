       IDENTIFICATION DIVISION.
       PROGRAM-ID. Comol-1.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      * /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
      * /\          FILE HANDLES          /\
      * /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
           SELECT IN-FILE ASSIGN TO
           "path/to/FineWine.raw"
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL.
           SELECT OUT-FILE ASSIGN TO
           "path/to/Output1.raw"
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL.
      * /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
      * /\   LFO USER WAVEFORM FILE      /\
      * /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
      * Signed 16-bit PCM RAW, 2048 samples, 44100 Hz.
      * Read at startup when LFO waveform type 8 is selected.
      * Both LFO1 and LFO2 share this single source file.
           SELECT LFO-FILE ASSIGN TO
            "path/to/LFO-Wave.raw"
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  OUT-FILE.
       01  RAW-BYTES           PIC X(1).
       FD  IN-FILE.
       01  BINARY-BYTES        PIC X(1).
      * One-byte record for the LFO user RAW waveform source file.
      * Read byte-by-byte to reconstruct little-endian 16-bit samples.
       FD  LFO-FILE.
       01  LFO-FILE-BYTE       PIC X(1).

       WORKING-STORAGE SECTION.
       01  WS-DATE-TIME        PIC X(21).
       01  WS-TIME-SEED        PIC 9(8).
      * /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
      * /\       MENU & USER INPUT        /\
      * /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
       01  WAVE-SOURCE-CHOICE      PIC 9(1).
       01  MENU-CHOICE             PIC 9(1) COMP-3.

       01  MENU-VARS               USAGE IS COMP-3.
           05  USER-OCTAVE         PIC 9(1).
           05  USER-NOTE           PIC 9(2).
      * /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
      * /\    FILTER SYSTEM VARIABLES     /\
      * /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
       01  FILTER-MENU-VARS.
           05  FILTER-TYPE-CHOICE  PIC X(1).
           05  ACTIVE-FILTER-TYPE  PIC 9(1).
           05  KNOB-POSITION       PIC 9(3)V9(8).
           05  Q-KNOB-POSITION     PIC 9(3).
      * Added for Weighting System
           05  BASE-CUTOFF         PIC 9(3)V9(8).
           05  TVF-DEPTH           PIC S9(3)V9(10).
           05  DEPTH-CALC          PIC S9(5)V9(8).
           05  OPERATION-MODE      PIC 9(1).
           88  DIGITAL-MODE        VALUE 1.
           88  ANALOGUE-MODE       VALUE 2.

       01  FILTER-MATH-VARS        USAGE IS COMP-5.
           05  LOOKUP-IDX          PIC 9(5).
           05  CURRENT-FREQ-HZ     PIC 9(5)V9(4).
           05  Q-RESONANCE         PIC S9(2)V9(8).
           05  PI-2                PIC 9(1)V9(8) VALUE 6.28318531.
           05  NUMERATOR-VAL       PIC 9(6)V9(8).
           05  ANGULAR-FREQUENCY   PIC S9(5)V9(8).
           05  FINAL-SINE-VALUE    PIC S9(1)V9(8).
           05  FINAL-COS-VALUE     PIC S9(1)V9(8).
           05  ALPHA-VALUE         PIC S9(2)V9(8).
           05  SAMPLE-WORK-AREA    PIC S9(5)V9(8).
           05  FILTERED-SAMPLE     PIC S9(5)V9(8).
           05  FREQ-FLOOR          PIC 9(5)V9(4).
           05  FREQ-CEIL           PIC 9(5)V9(4).
           05  KNOB-INT            PIC 9(3).
           05  KNOB-FRAC           PIC 9V9(8).
      * Coefficients
       01  BIQUAD-COEFFICIENTS USAGE COMP-5.
           05  A0-COEFF            PIC S9(3)V9(8).
           05  A1-COEFF            PIC S9(3)V9(8).
           05  A2-COEFF            PIC S9(3)V9(8).
           05  B0-COEFF            PIC S9(3)V9(8).
           05  B1-COEFF            PIC S9(3)V9(8).
           05  B2-COEFF            PIC S9(3)V9(8).
      * Delay Lines (History)
       01  DELAY-LINES         USAGE COMP-5.
           05  X1-INPUT            PIC S9(6)V9(8) VALUE 0.
           05  X2-INPUT            PIC S9(6)V9(8) VALUE 0.
           05  Y1-OUTPUT           PIC S9(6)V9(8) VALUE 0.
           05  Y2-OUTPUT           PIC S9(6)V9(8) VALUE 0.

      * /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
      * /\     LFO WAVETABLE MEMORY      /\
      * /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
      * Each LFO has a private 2048-point float wavetable (-1.0..+1.0).
      * Built once at startup by INIT-LFO1-TABLE / INIT-LFO2-TABLE.
       01  LFO1-WAVE-TABLE.
           05  LFO1-TBL-ENTRY  OCCURS 2048 TIMES
                               INDEXED BY LFO1-TBL-IDX.
               10  LFO1-TBL-SAMP PIC S9(1)V9(17) COMP-5.
       01  LFO2-WAVE-TABLE.
           05  LFO2-TBL-ENTRY  OCCURS 2048 TIMES
                               INDEXED BY LFO2-TBL-IDX.
               10  LFO2-TBL-SAMP PIC S9(1)V9(17) COMP-5.
      * /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
      * /\     LFO 1 USER PARAMETERS     /\
      * /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
      * WAVEFORM KEY:
      *   1=Sine      2=Triangle  3=Sawtooth-Up
      *   4=Sawtooth-Down  5=Square    6=Sample+Hold
      *   7=Smooth-Random  8=User-RAW (loaded from LFO-Wave.raw)
      * TVA-DEPTH: -100..+100. Scales LFO into amplitude (tremolo).
      * TVF-DEPTH: -100..+100. Offsets CURRENT-KNOB (filter wobble).
      * PTCH-DEPTH: 10 units = 1 semitone. +/-120 = +/-12 semitones.
      * PHASE-OFFS: 0-359 degrees. LFO starting phase on key trigger.
      * FADE-SEC:  +N = fade in over N seconds. -N = fade out.
      * FM-DEPTH: LFO1 output scales LFO2 rate. -100..+100 percent.
       01  LFO1-PARAMS.
           05  LFO1-WAVEFORM   PIC 9(1)      VALUE 1.
               88  LFO1-SINE       VALUE 1.
               88  LFO1-TRIANGLE   VALUE 2.
               88  LFO1-SAW-UP     VALUE 3.
               88  LFO1-SAW-DOWN   VALUE 4.
               88  LFO1-SQUARE     VALUE 5.
               88  LFO1-SH         VALUE 6.
               88  LFO1-SMTH-RND   VALUE 7.
               88  LFO1-USER-RAW   VALUE 8.
           05  LFO1-RATE-HZ    PIC 9(3)V9(4) VALUE 0.
           05  LFO1-DELAY-SEC  PIC 9(3)V9(4) VALUE 0.
           05  LFO1-FADE-SEC   PIC S9(3)     VALUE 0.
           05  LFO1-OFFSET     PIC S9(3)     VALUE 0.
           05  LFO1-KEY-TRIG   PIC 9(1)      VALUE 1.
           05  LFO1-TVA-DEPTH  PIC S9(3)     VALUE 0.
           05  LFO1-TVF-DEPTH  PIC S9(3)     VALUE 0.
           05  LFO1-PTCH-DPTH  PIC S9(3)     VALUE 0.
           05  LFO1-PHASE-OFFS PIC 9(3)      VALUE 0.
           05  LFO1-FM-DEPTH   PIC S9(3)     VALUE 0.
      * /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
      * /\     LFO 2 USER PARAMETERS     /\
      * /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
       01  LFO2-PARAMS.
           05  LFO2-WAVEFORM   PIC 9(1)      VALUE 1.
               88  LFO2-SINE       VALUE 1.
               88  LFO2-TRIANGLE   VALUE 2.
               88  LFO2-SAW-UP     VALUE 3.
               88  LFO2-SAW-DOWN   VALUE 4.
               88  LFO2-SQUARE     VALUE 5.
               88  LFO2-SH         VALUE 6.
               88  LFO2-SMTH-RND   VALUE 7.
               88  LFO2-USER-RAW   VALUE 8.
           05  LFO2-RATE-HZ    PIC 9(3)V9(4) VALUE 0.
           05  LFO2-DELAY-SEC  PIC 9(3)V9(4) VALUE 0.
           05  LFO2-FADE-SEC   PIC S9(3)     VALUE 0.
           05  LFO2-OFFSET     PIC S9(3)     VALUE 0.
           05  LFO2-KEY-TRIG   PIC 9(1)      VALUE 1.
           05  LFO2-TVA-DEPTH  PIC S9(3)     VALUE 0.
           05  LFO2-TVF-DEPTH  PIC S9(3)     VALUE 0.
           05  LFO2-PTCH-DPTH  PIC S9(3)     VALUE 0.
           05  LFO2-PHASE-OFFS PIC 9(3)      VALUE 0.
      * LFO2 has no FM-DEPTH (LFO1 is the FM source, LFO2 the target)
      * /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
      * /\     LFO 1 RUNTIME STATE       /\
      * /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
      * PHASE: current position in wavetable (0.0 to 2047.999...).
      * STEP:  wavetable index advance per sample.
      * DELAY-SMPL: total delay in samples (LFO silent until elapsed).
      * DELAY-CNT:  counts down from DELAY-SMPL to 0.
      * FADE-TOTAL: fade duration in samples (0 = no fade).
      * FADE-CNT:   samples elapsed within current fade.
      * FADE-AMT:   current fade multiplier (0.0 to 1.0).
      * FADE-STEP:  added to FADE-AMT each sample (+ve in, -ve out).
      * SH-HELD:    last captured value for Sample+Hold mode.
      * SH-PERIOD:  samples between S+H triggers (= TABLE/STEP).
      * SH-CNT:     samples since last S+H trigger.
      * PREV-IDX:   previous integer table index (edge detection).
       01  LFO1-STATE          USAGE COMP-5.
           05  LFO1-PHASE      PIC 9(7)V9(8)  VALUE 0.
           05  LFO1-STEP       PIC 9(4)V9(8)  VALUE 0.
           05  LFO1-DELAY-SMPL PIC 9(9)       VALUE 0.
           05  LFO1-DELAY-CNT  PIC 9(9)       VALUE 0.
           05  LFO1-FADE-TOTAL PIC 9(9)       VALUE 0.
           05  LFO1-FADE-CNT   PIC 9(9)       VALUE 0.
           05  LFO1-FADE-AMT   PIC 9(1)V9(8)  VALUE 1.
           05  LFO1-FADE-STEP  PIC S9(1)V9(10) VALUE 0.
           05  LFO1-IS-ACTIVE  PIC 9(1)       VALUE 0.
           05  LFO1-SH-HELD    PIC S9(1)V9(8) VALUE 0.
           05  LFO1-SH-PERIOD  PIC 9(9)       VALUE 0.
           05  LFO1-SH-CNT     PIC 9(9)       VALUE 0.
           05  LFO1-PREV-IDX   PIC 9(4)       VALUE 0.
      * /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
      * /\     LFO 2 RUNTIME STATE       /\
      * /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
       01  LFO2-STATE          USAGE COMP-5.
           05  LFO2-PHASE      PIC 9(7)V9(8)  VALUE 0.
           05  LFO2-STEP       PIC 9(4)V9(8)  VALUE 0.
           05  LFO2-DELAY-SMPL PIC 9(9)       VALUE 0.
           05  LFO2-DELAY-CNT  PIC 9(9)       VALUE 0.
           05  LFO2-FADE-TOTAL PIC 9(9)       VALUE 0.
           05  LFO2-FADE-CNT   PIC 9(9)       VALUE 0.
           05  LFO2-FADE-AMT   PIC 9(1)V9(8)  VALUE 1.
           05  LFO2-FADE-STEP  PIC S9(1)V9(10) VALUE 0.
           05  LFO2-IS-ACTIVE  PIC 9(1)       VALUE 0.
           05  LFO2-SH-HELD    PIC S9(1)V9(8) VALUE 0.
           05  LFO2-SH-PERIOD  PIC 9(9)       VALUE 0.
           05  LFO2-SH-CNT     PIC 9(9)       VALUE 0.
           05  LFO2-PREV-IDX   PIC 9(4)       VALUE 0.
      * /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
      * /\     LFO OUTPUT + COMPUTE VARS  /\
      * /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
      * LFO1-VALUE / LFO2-VALUE: final per-sample output (-1.0..+1.0)
      *   after waveform lookup, fade, and DC offset are applied.
      * LFO-TVA-OUT: combined amplitude mod signal. Post-filter.
      * LFO-TVF-OUT: combined cutoff offset. Added to CURRENT-KNOB.
      * LFO-PTCH-OUT: combined pitch shift in semitones.
      * LFO-CALC-STEP: effective LFO2 step (after FM modulation).
       01  LFO-OUTPUT-VARS     USAGE COMP-5.
           05  LFO1-VALUE      PIC S9(1)V9(8) VALUE 0.
           05  LFO2-VALUE      PIC S9(1)V9(8) VALUE 0.
           05  LFO-TVA-OUT     PIC S9(2)V9(8) VALUE 0.
           05  LFO-TVF-OUT     PIC S9(4)V9(8) VALUE 0.
           05  LFO-PTCH-OUT    PIC S9(3)V9(8) VALUE 0.
       01  LFO-COMPUTE-VARS    USAGE COMP-5.
           05  LFO-CALC-IDX    PIC 9(4)       VALUE 0.
           05  LFO-NEXT-IDX    PIC 9(4)       VALUE 0.
           05  LFO-FRAC        PIC 9V9(8)     VALUE 0.
           05  LFO-SAMP-A      PIC S9(1)V9(17) VALUE 0.
           05  LFO-SAMP-B      PIC S9(1)V9(17) VALUE 0.
           05  LFO-WORK-VAL    PIC S9(1)V9(8) VALUE 0.
           05  LFO-2PI         PIC 9(1)V9(8)  VALUE 6.28318531.
           05  LFO-RND-SEED    PIC 9(9)       VALUE 987654321.
           05  LFO-RND-RESULT  PIC 9V9(8)     VALUE 0.
           05  LFO-CALC-STEP   PIC S9(4)V9(8) VALUE 0.
           05  LFO-MOD-RATE    PIC S9(2)V9(8) VALUE 0.
           05  LFO-BUILD-IDX   PIC 9(4)       VALUE 0.
           05  LFO-BUILD-ANG   PIC S9(3)V9(15) VALUE 0.
           05  LFO-PTCH-ADJ    PIC S9(5)V9(8) VALUE 0.
           05  LFO-FILE-W1     PIC S9(10)     VALUE 0.
           05  LFO-FILE-W2     PIC S9(10)     VALUE 0.
           05  LFO-FILE-BUF    PIC X(2).

      * /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
      * /\   VIRTUAL ANALOGUE VARIABLES   /\
      * /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
       01  ANALOGUE-PARAMS     USAGE COMP-5.
           05  BIAS-INTENSITY      PIC 9(3)    VALUE 0.
           05  DRIVE-FACTOR        PIC 9V9(2)  VALUE 1.0.
           05  DRIFT-INTENSITY     PIC 9V9(5)  VALUE 0.00000.
           05  CRUSH-FACTOR        PIC 9(4)    VALUE 1.

       01  ANALOGUE-MATH       USAGE COMP-5.
      * Drive factors only need small integers but high precision
           05  POS-FACTOR          PIC S9(3)V9(15).
           05  NEG-FACTOR          PIC S9(3)V9(15).

      * Clipping thresholds (Values around +/- 32000)
           05  UPPER-ADJ           PIC S9(5)V9(13).
           05  LOWER-ADJ           PIC S9(5)V9(13).

      * Intermediate Calculation Variables
           05  TEMP-INT            PIC S9(18).
           05  TEMP-NORM           PIC S9(3)V9(15).
           05  TEMP-DRIVE          PIC S9(5)V9(13).
           05  TEMP-ABS            PIC 9(5)V9(13).
           05  TEMP-X2             PIC 9(5)V9(13).
           05  TEMP-Y              PIC S9(5)V9(13).

       01  DRIFT-ENGINE        USAGE COMP-5.
           05  RANDOM-SEED         PIC 9(9)    VALUE 123456789.
           05  RANDOM-RESULT       PIC 9V9(8).
           05  DRIFT-AMOUNT        PIC S9V9(8).

       01  USER-INPUT-HELPERS  USAGE COMP-5.
           05  USER-DRIVE-IN       PIC 9(3).
           05  USER-DRIFT-IN       PIC 9(3).
           05  USER-CRUSH-IN       PIC 9(4).
      * Constants for Analogue Math
       78  Q-STARTING-POINT    VALUE 0.707.
       78  Q-GRADIENT          VALUE 0.015.
       78  POSITIVE-EXPANSION  VALUE 1.082.
       78  NEGATIVE-SQUASH     VALUE 0.850.
       78  UPPER-THRESHOLD     VALUE 10000.
       78  LOWER-THRESHOLD     VALUE -10000.
      * /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
      * /\    RECONSTRUCTION VARIABLES    /\
      * /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
       01  TWO-BYTE-BUFFER     PIC X(2).

       01  FILE-READER-VARS    USAGE COMP-5.
           05  RESULT-WRITER   PIC S9(10).
           05  ENDIAN          PIC S9(10) COMP-5.
           05  NORMALIZED-VAL  PIC S9(9).

      * /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
      * /\        PHYSICS VARIABLES       /\
      * /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
       01  SAMPLE-RATE         PIC 9(5) COMP-5 VALUE 44100.
       01  PI-CONSTANT         PIC 9(1)V9(18) COMP-3
                               VALUE 3.141592653589793238.
       01  PITCH-MATH          USAGE COMP-5.
           05  TARGET-FREQUENCY      PIC 9(6)V99.
           05  STEP-SIZE             PIC 9(9)V9999.
           05  MULTIPLICATION-HOLDER PIC 9(12).

      * /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
      * /\      PROGRESS TRACKING         /\
      * /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
       01  PROGRESS-VARS       USAGE COMP-5.
           05  GLOBAL-SAMPLE-COUNT PIC 9(9) VALUE 0.
           05  NEXT-UPDATE         PIC 9(9) VALUE 10000.
           05  PROGRESS-THRESHOLD  PIC 9(5) VALUE 10000.

      * /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
      * /\        WAVETABLE MEMORY        /\
      * /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
       01  WAVE-GENERATOR-CONSTANTS USAGE COMP-5.
           05  TABLE-SIZE      PIC 9(4) BINARY VALUE 2048.
           05  TABLE-FLOAT     PIC 9(4)V9(1)   VALUE 2048.0.
       01  WAVE-TABLE-STRUCTURE.
           05  WAVE-TABLE      OCCURS 2048 TIMES
                               INDEXED BY WAVE-IDX.
      * >>>> CRITICAL FIX: Math uses WAVE-SAMPLE, not WAVE-TABLE <<<<
               10  WAVE-SAMPLE PIC S9(1)V9(17) COMP-5.
       01  SINE-MATH-VARS      USAGE COMP-5.
           05  TEMP-ANGLE      PIC S9(3)V9(15) COMP-5.
           05  LOOP-COUNTER    PIC 9(4) BINARY.

       01  FILTER-COEFF-LUT.
           05  LUT-ENTRY OCCURS 1001 TIMES INDEXED BY LUT-IDX.
               10  L-B0 PIC S9(3)V9(8) COMP-5.
               10  L-B1 PIC S9(3)V9(8) COMP-5.
               10  L-B2 PIC S9(3)V9(8) COMP-5.
               10  L-A1 PIC S9(3)V9(8) COMP-5.
               10  L-A2 PIC S9(3)V9(8) COMP-5.

       01  LUT-MATH-VARS USAGE COMP-5.
           05  LUT-KNOB-TEMP PIC 9(3)V9(8).
      * /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
      * /\            RESAMPLE            /\
      * /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
       01  RESAMPLE-POINTERS   USAGE COMP-5.
           05  READ-POSITION   PIC 9(9)V9999 VALUE 1.
           05  READ-INDEX      PIC 9(7).
           05  FRACTIONAL-PART PIC S9V9(5).
           05  INVERSE-FRAC    PIC S9V9(5).
      * /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
      * /\            INTERPOLATION       /\
      * /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
       01  GRIT-CONTROL-VARS   USAGE COMP-5.
           05  GRIT-FACTOR     PIC 9(3) VALUE 1.
           05  OUTPUT-COUNT    PIC 9(9) VALUE 1.
           05  REMAINDER-VAL   PIC 9(2).

       01  LINEAR-MATH         USAGE COMP-5.
      * These fit perfectly (1 + 17 = 18 digits)
           05  SAMPLE-A        PIC S9(1)V9(17).
           05  SAMPLE-B        PIC S9(1)V9(17).

      * OPTIMIZED: Reduced from S9(2) to S9(1) to fit hardware limit
           05  INTERP-RESULT   PIC S9(2)V9(16).

       01  PATTERN-CONTROL.
           05  RAW-PATTERN-INPUT   PIC X(16).
           05  PATTERN-LENGTH      PIC 9(2) VALUE 1.
           05  PATTERN-IDX         PIC 9(2).
           05  CURRENT-MODE        PIC 9(1).

       01  PATTERN-TABLE-STRUCTURE.
           05  SEQ-STEP OCCURS 16 TIMES.
               10  INTERP-MODE     PIC 9(2).
      * /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
      * /\       SINC INTERPOLATION       /\
      * /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
       01  SINC-CONSTANTS      USAGE COMP-5.
           05  PI-VAL             PIC 9V9(10) VALUE 3.1415926535.
           05  KERNEL-RADIUS      PIC 9(2)    VALUE 4.
           05  FIXED-POINT-SCALER PIC 9(10)   VALUE 1000000000.

       01  POLYPHASE-CONSTANTS USAGE COMP-5.
           05  PHASE-RESOLUTION   PIC 9(3)    VALUE 900.
           05  PHASE-SCALER       PIC 9(3)    VALUE 900.

       01  SINC-VARS           USAGE COMP-5.
           05  WEIGHT-SUM      PIC S9(5)V9(10).
           05  PHASE-INDEX     PIC 9(3).
           05  KERNEL-INDEX    PIC S9(3).
           05  LOOKUP-INDEX    PIC 9(7).
           05  TEMP-INDEX-CALC PIC 9(3).
           05  RAW-TABLE-VAL   PIC S9(13).
           05  FINAL-WEIGHT    PIC S9(5)V9(10).
           05  SUM-ACCUM       PIC S9(5)V9(10).
           05  SINC-X          PIC S9(5)V9(10).
           05  MATCH-FRACTION  PIC 9V9(10).
           05  SINC-WEIGHT     PIC S9(5)V9(10).
           05  WINDOW-WEIGHT   PIC S9(5)V9(10).
      * /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
      * /\      ENVELOPE VARIABLES        /\
      * /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
       01  JD-PARAMS           USAGE COMP-5.
           05  L1              PIC 9(3).
           05  L2              PIC 9(3).
           05  L3              PIC 9(3).
           05  T1              PIC 9(2)V9(2).
           05  T2              PIC 9(2)V9(2).
           05  T3              PIC 9(2)V9(2).
           05  T-SUSTAIN       PIC 9(2)V9(2).
           05  T4              PIC 9(2)V9(2).
       01  STAGE-MATH          USAGE COMP-3.
           05  DURATION-SECONDS PIC 9(2)V9(2).
           05  TOTAL-SAMPLES    PIC 9(9).
           05  START-VOLUME     PIC 9(3).
           05  END-VOLUME       PIC 9(3).
           05  START-AMP        PIC S9(5)V9(10).
           05  END-AMP          PIC S9(5)V9(10).
           05  AMP-STEP         PIC S9(5)V9(10).
           05  RUNNING-AMP      PIC S9(5)V9(10).
           05  MAX-AMP          PIC 9(5) VALUE 32767.

      * TVF (Filter Envelope) Variables
       01  CUT-PARAMS          USAGE COMP-5.
           05  CUT-L1              PIC S9(3).
           05  CUT-L2              PIC S9(3).
           05  CUT-L3              PIC S9(3).
           05  CUT-T1              PIC 9(2)V9(2).
           05  CUT-T2              PIC 9(2)V9(2).
           05  CUT-T3              PIC 9(2)V9(2).
           05  CUT-T-SUSTAIN       PIC 9(2)V9(2).
           05  CUT-T4              PIC 9(2)V9(2).

       01  CUT-STAGES.
           05  STAGE-START-SAMPLE-CUT PIC 9(9) OCCURS 5 TIMES.
           05  STAGE-END-SAMPLE-CUT   PIC 9(9) OCCURS 5 TIMES.
           05  STAGE-START-CUT-VAL    PIC S9(3)V9(8) OCCURS 5 TIMES.
           05  STAGE-END-CUT-VAL      PIC S9(3)V9(8) OCCURS 5 TIMES.

       01  TVF-CONTROL-VARS    USAGE COMP-5.
           05  CURRENT-KNOB        PIC S9(3)V9(8) VALUE 0.
           05  CURRENT-STAGE-CUT   PIC 9(1).
           05  LOCAL-POS           PIC 9(9).
           05  STAGE-DURATION      PIC 9(9).
           05  FRAC-POS            PIC S9V9(8).


       01  COEFF-SMOOTH-VARS   USAGE COMP-5.
       05  SMOOTHED-DEPTH      PIC S9(3)V9(8) VALUE 77.
       05  SMOOTH-RATE         PIC 9V9(8)     VALUE 0.005.
      * /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
      * /\      OUTPUT PCM VARIABLES      /\
      * /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
       01  PCM-WRITING         USAGE COMP-5.
           05  SCALED-SAMPLE   PIC S9(9) BINARY.
           05  LOW-BYTE-VAL    PIC 9(3).
           05  HIGH-BYTE-VAL   PIC 9(3).

      * /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
      * /\      EXTERNAL DATA TABLES      /\
      * /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
       COPY NOTE-FREQ-WS.
       COPY Sine-Weights-WS.
      * FILTER TABLES
       COPY FREQUENCY-TABLE-WS.
       COPY SINE-OMEGA.
       COPY COS-OMEGA.


       PROCEDURE DIVISION.

       MAIN-LOGIC.

           MOVE FUNCTION CURRENT-DATE TO WS-DATE-TIME.


           MOVE WS-DATE-TIME(9:8) TO WS-TIME-SEED.


           MOVE WS-TIME-SEED TO RANDOM-SEED.


           COMPUTE LFO-RND-SEED = WS-TIME-SEED + 87654321.

           PERFORM GENERATE-DRIFT.
           COMPUTE READ-POSITION = (RANDOM-RESULT * 2047.0) + 1.0.
           PERFORM INIT-SINC-TABLE.
           OPEN OUTPUT OUT-FILE.
           PERFORM ASCII-VANITY.
           PERFORM GET-WAVE-SOURCE.
           PERFORM GET-PITCH-SETTINGS.
           PERFORM FREQUENCY-MATH.
           PERFORM GET-INTERPOLATION-MODE.
      * >>>> INSERTED FILTER SETUP HERE <<<
           PERFORM GET-FILTER-SETTINGS.
           PERFORM CALCULATE-FILTER-COEFFICIENTS.

      * ==========================================
      * >>>> FAST IIR COEFFICIENT GENERATOR <<<<
           PERFORM GENERATE-FILTER-LUT.
      * ==========================================

           PERFORM GET-ENVELOPE-SETTINGS.
      * >>>> PRE-CALCULATE TVF ENVELOPE <<<<
           PERFORM CALCULATE-CUT-BREAKPOINTS.
      * /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
      * /\   LFO SETUP (JD800 + EXTRAS)   /\
      * /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
      * Hard-coded LFO parameters are set inside GET-LFO-SETTINGS.
      * INIT-LFO-WAVEFORMS builds the wavetables and pre-computes
      * all runtime state (step size, delay/fade counters, phase).
           PERFORM GET-LFO-SETTINGS.
           PERFORM INIT-LFO-WAVEFORMS.
           DISPLAY "Processing...".

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

           CLOSE OUT-FILE.
           DISPLAY "Done. Output.raw created.".
           STOP RUN.
      * /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
      * /\      SECTION 1: SETUP          /\
      * /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
       GET-WAVE-SOURCE.
      * USER_INPUT: WAVE-SOURCE-CHOICE
           MOVE 2 TO WAVE-SOURCE-CHOICE.
           IF WAVE-SOURCE-CHOICE = 2
               PERFORM LOAD-FILE-TO-TABLE
           ELSE
               PERFORM ANGLE-RADIANS-SINE
           END-IF.
       LOAD-FILE-TO-TABLE.
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

       ANGLE-RADIANS-SINE.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1
             UNTIL LOOP-COUNTER > TABLE-SIZE
               SET WAVE-IDX TO LOOP-COUNTER
               COMPUTE TEMP-ANGLE
               = (2 * PI-CONSTANT * (LOOP-COUNTER - 1)) / TABLE-SIZE
               COMPUTE WAVE-SAMPLE(WAVE-IDX)
               = FUNCTION SIN(TEMP-ANGLE)
           END-PERFORM.
       GET-PITCH-SETTINGS.
      * USER_INPUT: USER-OCTAVE
           MOVE 3 TO USER-OCTAVE.
      * USER_INPUT: USER-NOTE
           MOVE 0 TO USER-NOTE.
      * Logic from NOTE-SELECTOR.CPY determines which paragraph to run
           EVALUATE USER-OCTAVE
               WHEN 0 PERFORM SET-OCTAVE-0
               WHEN 1 PERFORM SET-OCTAVE-1
               WHEN 2 PERFORM SET-OCTAVE-2
               WHEN 3 PERFORM SET-OCTAVE-3
               WHEN 4 PERFORM SET-OCTAVE-4
               WHEN 5 PERFORM SET-OCTAVE-5
               WHEN 6 PERFORM SET-OCTAVE-6
               WHEN OTHER
                   MOVE FREQ-C4 TO TARGET-FREQUENCY
           END-EVALUATE.


       GET-INTERPOLATION-MODE.
      * USER_INPUT: RAW-PATTERN-INPUT
           MOVE "3" TO RAW-PATTERN-INPUT.

           PERFORM PARSE-PATTERN.

       PARSE-PATTERN.
           MOVE 0 TO PATTERN-LENGTH.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1
             UNTIL LOOP-COUNTER > 16

               IF RAW-PATTERN-INPUT(LOOP-COUNTER:1) = SPACE
                   EXIT PERFORM
               END-IF

               ADD 1 TO PATTERN-LENGTH
               MOVE RAW-PATTERN-INPUT(LOOP-COUNTER:1)
                 TO INTERP-MODE(LOOP-COUNTER)
           END-PERFORM.
           IF PATTERN-LENGTH = 0
               MOVE 1 TO PATTERN-LENGTH
               MOVE 2 TO INTERP-MODE(1)
           END-IF.

      * /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
      * /\      SECTION: FILTER SETUP     /\
      * /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
       GET-FILTER-SETTINGS.
      * USER_INPUT: OPERATION-MODE
           MOVE 1 TO OPERATION-MODE.
           IF OPERATION-MODE NOT = 1 AND OPERATION-MODE NOT = 2
               MOVE 1 TO OPERATION-MODE
           END-IF.
           IF ANALOGUE-MODE
      * USER_INPUT: BIAS-INTENSITY
               MOVE 0 TO BIAS-INTENSITY
      * USER_INPUT: USER-DRIVE-IN
               MOVE 0 TO USER-DRIVE-IN
               COMPUTE DRIVE-FACTOR = USER-DRIVE-IN / 2.0
               IF DRIVE-FACTOR < 1.0 MOVE 1.0 TO DRIVE-FACTOR END-IF

      * USER_INPUT: USER-DRIFT-IN
               MOVE 0 TO USER-DRIFT-IN
               COMPUTE DRIFT-INTENSITY = USER-DRIFT-IN / 10000

      * USER_INPUT: USER-CRUSH-IN
               MOVE 1 TO USER-CRUSH-IN
               MOVE USER-CRUSH-IN TO CRUSH-FACTOR
                  END-IF.
      * USER_INPUT: FILTER-TYPE-CHOICE
           MOVE "1" TO FILTER-TYPE-CHOICE.
           EVALUATE FILTER-TYPE-CHOICE
               WHEN "1" MOVE 1 TO ACTIVE-FILTER-TYPE
               WHEN "2" MOVE 2 TO ACTIVE-FILTER-TYPE
               WHEN "3" MOVE 3 TO ACTIVE-FILTER-TYPE
               WHEN OTHER MOVE 1 TO ACTIVE-FILTER-TYPE
           END-EVALUATE.
      * USER_INPUT: KNOB-POSITION
           MOVE 77 TO KNOB-POSITION.
      * Save the static base value
           MOVE KNOB-POSITION TO BASE-CUTOFF.

      * USER_INPUT: Q-KNOB-POSITION
           MOVE 10 TO Q-KNOB-POSITION.

       CALCULATE-FILTER-COEFFICIENTS.
      * 1. Get Freq from Table
           COMPUTE LOOKUP-IDX = KNOB-POSITION + 1.
           MOVE FREQ-HZ(LOOKUP-IDX) TO CURRENT-FREQ-HZ.

      * 2. Calculate Resonance
           COMPUTE Q-RESONANCE = Q-GRADIENT * Q-KNOB-POSITION
                                 + Q-STARTING-POINT.
      * 3. Calculate Omega/Alpha
           COMPUTE NUMERATOR-VAL = CURRENT-FREQ-HZ / SAMPLE-RATE.
           COMPUTE ANGULAR-FREQUENCY = PI-2 * NUMERATOR-VAL.

           PERFORM FIND-SINE-FROM-OMEGA.
           PERFORM FIND-COS-FROM-OMEGA.
           COMPUTE ALPHA-VALUE ROUNDED =
            FINAL-SINE-VALUE / (2 * Q-RESONANCE).
      * 4. Calculate Coefficients (Standard Biquad)
           PERFORM INIT-COEFFICIENTS.

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
       INIT-COEFFICIENTS.
           COMPUTE A0-COEFF ROUNDED = 1 + ALPHA-VALUE.
           COMPUTE A1-COEFF ROUNDED = -2 * FINAL-COS-VALUE.
           COMPUTE A2-COEFF ROUNDED = 1 - ALPHA-VALUE.

           EVALUATE ACTIVE-FILTER-TYPE
             WHEN 1
      * LPF
               COMPUTE B1-COEFF ROUNDED = 1 - FINAL-COS-VALUE
               COMPUTE B0-COEFF ROUNDED = B1-COEFF / 2
               MOVE B0-COEFF TO B2-COEFF
             WHEN 2
      * HPF
               COMPUTE B1-COEFF ROUNDED = -1 * (1 + FINAL-COS-VALUE)
               COMPUTE B0-COEFF ROUNDED = B1-COEFF / -2
               MOVE B0-COEFF TO B2-COEFF
             WHEN 3
      * BPF
               COMPUTE B0-COEFF ROUNDED = Q-RESONANCE * ALPHA-VALUE
               MOVE 0 TO B1-COEFF
               COMPUTE B2-COEFF ROUNDED = -1 * B0-COEFF
           END-EVALUATE.
      * Normalize by A0
           COMPUTE B0-COEFF ROUNDED = B0-COEFF / A0-COEFF.
           COMPUTE B1-COEFF ROUNDED = B1-COEFF / A0-COEFF.
           COMPUTE B2-COEFF ROUNDED = B2-COEFF / A0-COEFF.
           COMPUTE A1-COEFF ROUNDED = A1-COEFF / A0-COEFF.
           COMPUTE A2-COEFF ROUNDED = A2-COEFF / A0-COEFF.
      * Pre-calc Bias Thresholds if needed
           IF ANALOGUE-MODE
               COMPUTE UPPER-ADJ = 32767 -
                   ((32767 - UPPER-THRESHOLD) * (BIAS-INTENSITY / 100))
               COMPUTE LOWER-ADJ = -32768 +
             ((-32768 - LOWER-THRESHOLD) * -1 * (BIAS-INTENSITY / 100))
           END-IF.

       GET-ENVELOPE-SETTINGS.
      * USER_INPUT: T1
           MOVE 2.50 TO T1.
      * USER_INPUT: L1
           MOVE 100 TO L1.
      * USER_INPUT: T2
           MOVE 1.50 TO T2.
      * USER_INPUT: L2
           MOVE 90 TO L2.
      * USER_INPUT: T3
           MOVE 2.00 TO T3.
      * USER_INPUT: L3
           MOVE 80 TO L3.
      * USER_INPUT: T-SUSTAIN
           MOVE 4.00 TO T-SUSTAIN.
      * USER_INPUT: T4
           MOVE 3.00 TO T4.

      * USER_INPUT: CUT-T1
           MOVE 2.00 TO CUT-T1.
      * USER_INPUT: CUT-L1
           MOVE 100 TO CUT-L1.
      * USER_INPUT: CUT-T2
           MOVE 1.50 TO CUT-T2.
      * USER_INPUT: CUT-L2
           MOVE 90 TO CUT-L2.
      * USER_INPUT: CUT-T3
           MOVE 2.00 TO CUT-T3.
      * USER_INPUT: CUT-L3
           MOVE 80 TO CUT-L3.
      * USER_INPUT: CUT-T-SUSTAIN
           MOVE 4.00 TO CUT-T-SUSTAIN.
      * USER_INPUT: CUT-T4
           MOVE 3.00 TO CUT-T4.
      * USER_INPUT: TVF-DEPTH
           MOVE 20 TO TVF-DEPTH.
       CALCULATE-CUT-BREAKPOINTS.
      * Stage 1: Attack
           MOVE 1 TO STAGE-START-SAMPLE-CUT(1).
           COMPUTE STAGE-END-SAMPLE-CUT(1)
               = FUNCTION INTEGER(CUT-T1 * SAMPLE-RATE).
           MOVE 0 TO STAGE-START-CUT-VAL(1).
           MOVE CUT-L1 TO STAGE-END-CUT-VAL(1).

      * Stage 2: Decay1
           COMPUTE STAGE-START-SAMPLE-CUT(2)
               = STAGE-END-SAMPLE-CUT(1) + 1.
           COMPUTE STAGE-END-SAMPLE-CUT(2)
               = STAGE-END-SAMPLE-CUT(1)
                   + FUNCTION INTEGER(CUT-T2 * SAMPLE-RATE).
           MOVE CUT-L1 TO STAGE-START-CUT-VAL(2).
           MOVE CUT-L2 TO STAGE-END-CUT-VAL(2).

      * Stage 3: Decay2
           COMPUTE STAGE-START-SAMPLE-CUT(3)
               = STAGE-END-SAMPLE-CUT(2) + 1.
           COMPUTE STAGE-END-SAMPLE-CUT(3)
               = STAGE-END-SAMPLE-CUT(2)
                   + FUNCTION INTEGER(CUT-T3 * SAMPLE-RATE).
           MOVE CUT-L2 TO STAGE-START-CUT-VAL(3).
           MOVE CUT-L3 TO STAGE-END-CUT-VAL(3).

      * Stage 4: Sustain
           COMPUTE STAGE-START-SAMPLE-CUT(4)
               = STAGE-END-SAMPLE-CUT(3) + 1.
           COMPUTE STAGE-END-SAMPLE-CUT(4)
               = STAGE-END-SAMPLE-CUT(3)
                   + FUNCTION INTEGER(CUT-T-SUSTAIN * SAMPLE-RATE).
           MOVE CUT-L3 TO STAGE-START-CUT-VAL(4).
           MOVE CUT-L3 TO STAGE-END-CUT-VAL(4).

      * Stage 5: Release
           COMPUTE STAGE-START-SAMPLE-CUT(5)
               = STAGE-END-SAMPLE-CUT(4) + 1.
           COMPUTE STAGE-END-SAMPLE-CUT(5)
               = STAGE-END-SAMPLE-CUT(4)
                   + FUNCTION INTEGER(CUT-T4 * SAMPLE-RATE).
           MOVE CUT-L3 TO STAGE-START-CUT-VAL(5).
           MOVE 0 TO STAGE-END-CUT-VAL(5).

      * /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
      * /\      SECTION 2: CORE LOGIC     /\
      * /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\

       FREQUENCY-MATH.
      * Calculate how fast we step through the table
           COMPUTE MULTIPLICATION-HOLDER
               = TARGET-FREQUENCY * TABLE-SIZE.
           COMPUTE STEP-SIZE = MULTIPLICATION-HOLDER / SAMPLE-RATE.

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
       GENERATE-SAMPLE-BLOCK.
           PERFORM UPDATE-PROGRESS.
           PERFORM CALCULATE-INDICES.
      * >>>> ADVANCE BOTH LFOs EACH SAMPLE <<<<
      * Runs before TVF and TVA modulation paragraphs below.
           PERFORM ADVANCE-LFO-ENGINES.
           PERFORM COMPUTE-RAW-SAMPLE.
      * >>>> LFO PITCH MODULATION (VIBRATO) <<<<
      * Offsets READ-POSITION fractionally each sample.
           PERFORM APPLY-LFO-PITCH.

      * >>>> INSERT FILTER HERE <<<<
           PERFORM UPDATE-CUT-PARAMETER.
      * >>>> LFO TVF MODULATION (FILTER WOBBLE) <<<<
      * Adds the LFO signal to CURRENT-KNOB before LUT lookup.
           PERFORM APPLY-LFO-TVF.
           PERFORM RECALCULATE-COEFFICIENTS.
           PERFORM APPLY-FILTER.
      * >>>> LFO TVA MODULATION (TREMOLO) <<<<
      * Scales INTERP-RESULT after filter, before volume write.
           PERFORM APPLY-LFO-TVA.
           PERFORM APPLY-VOLUME-AND-WRITE.
           PERFORM ADVANCE-POINTERS.

       UPDATE-CUT-PARAMETER.
           PERFORM VARYING CURRENT-STAGE-CUT FROM 1 BY 1
               UNTIL CURRENT-STAGE-CUT > 5
               IF GLOBAL-SAMPLE-COUNT
                   >= STAGE-START-SAMPLE-CUT(CURRENT-STAGE-CUT)
                   AND GLOBAL-SAMPLE-COUNT
                   <= STAGE-END-SAMPLE-CUT(CURRENT-STAGE-CUT)
                   EXIT PERFORM
               END-IF
           END-PERFORM.
           IF CURRENT-STAGE-CUT > 5
      * Hold the last value if beyond cutoff envelope duration
               MOVE STAGE-END-CUT-VAL(5) TO CURRENT-KNOB
               EXIT PARAGRAPH
           END-IF.
           COMPUTE LOCAL-POS = GLOBAL-SAMPLE-COUNT
           - STAGE-START-SAMPLE-CUT(CURRENT-STAGE-CUT) + 1.
           COMPUTE STAGE-DURATION
           = STAGE-END-SAMPLE-CUT(CURRENT-STAGE-CUT)
           - STAGE-START-SAMPLE-CUT(CURRENT-STAGE-CUT) + 1.
           IF STAGE-DURATION = 0
               MOVE STAGE-END-CUT-VAL(CURRENT-STAGE-CUT) TO CURRENT-KNOB
               EXIT PARAGRAPH
           END-IF.
           COMPUTE FRAC-POS = LOCAL-POS / STAGE-DURATION.
           COMPUTE CURRENT-KNOB
           = STAGE-START-CUT-VAL(CURRENT-STAGE-CUT) +
               (STAGE-END-CUT-VAL(CURRENT-STAGE-CUT)
               - STAGE-START-CUT-VAL(CURRENT-STAGE-CUT)) * FRAC-POS.

       RECALCULATE-COEFFICIENTS.
      *      1. Calculate Envelope Modulation
           COMPUTE DEPTH-CALC = BASE-CUTOFF +
                (CURRENT-KNOB * TVF-DEPTH / 100.0).

      *      2. Clamp Range to 0-100
       IF DEPTH-CALC > 100 MOVE 100 TO DEPTH-CALC END-IF.
       IF DEPTH-CALC < 0   MOVE 0   TO DEPTH-CALC END-IF.

      *      3. Smooth toward target (one-pole IIR on the control signal)
       COMPUTE SMOOTHED-DEPTH = SMOOTHED-DEPTH +
           SMOOTH-RATE * (DEPTH-CALC - SMOOTHED-DEPTH).

      *      4. Map smoothed value to LUT index
       COMPUTE LUT-IDX = FUNCTION INTEGER(SMOOTHED-DEPTH * 10) + 1.
       IF LUT-IDX < 1    MOVE 1    TO LUT-IDX END-IF.
       IF LUT-IDX > 1001 MOVE 1001 TO LUT-IDX END-IF.

      *      5. Direct Fetch
       MOVE L-B0(LUT-IDX) TO B0-COEFF.
       MOVE L-B1(LUT-IDX) TO B1-COEFF.
       MOVE L-B2(LUT-IDX) TO B2-COEFF.
       MOVE L-A1(LUT-IDX) TO A1-COEFF.
       MOVE L-A2(LUT-IDX) TO A2-COEFF.

       CALCULATE-ANGULAR-FREQUENCY.
           COMPUTE NUMERATOR-VAL = CURRENT-FREQ-HZ / SAMPLE-RATE.
           COMPUTE ANGULAR-FREQUENCY = PI-2 * NUMERATOR-VAL.

       CALCULATE-ALPHA.
           COMPUTE ALPHA-VALUE ROUNDED =
            (FINAL-SINE-VALUE) / (2 * Q-RESONANCE).

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
      * 4. Virtual Analogue Mode
           IF ANALOGUE-MODE
               PERFORM APPLY-BRANCHING-BIAS
               PERFORM APPLY-SOFT-SATURATION
               PERFORM APPLY-COMP-CASCADE
           END-IF.
      * 5. Convert back to Float 0.0-1.0 for Envelope
           COMPUTE INTERP-RESULT = FILTERED-SAMPLE / 32767.0.
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
       GENERATE-DRIFT.
           COMPUTE RANDOM-SEED =
               FUNCTION
               MOD((1103515245 * RANDOM-SEED + 12345), 2147483647).
           COMPUTE RANDOM-RESULT = RANDOM-SEED / 2147483647.
           COMPUTE DRIFT-AMOUNT =
           (RANDOM-RESULT - 0.5) * DRIFT-INTENSITY.
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

       APPLY-COMP-CASCADE.
           IF CRUSH-FACTOR > 1
               COMPUTE TEMP-INT = FILTERED-SAMPLE / CRUSH-FACTOR
               COMPUTE FILTERED-SAMPLE = TEMP-INT * CRUSH-FACTOR
           END-IF.
       UPDATE-PROGRESS.
           ADD 1 TO GLOBAL-SAMPLE-COUNT.
           IF GLOBAL-SAMPLE-COUNT >= NEXT-UPDATE
               DISPLAY "Processed " GLOBAL-SAMPLE-COUNT " samples..."
               ADD PROGRESS-THRESHOLD TO NEXT-UPDATE
           END-IF.
       CALCULATE-INDICES.
           COMPUTE READ-INDEX = FUNCTION INTEGER(READ-POSITION).
           COMPUTE FRACTIONAL-PART = READ-POSITION - READ-INDEX.

       COMPUTE-RAW-SAMPLE.
      * 1. Determine position in the sequence (Modulo Math)
      * We use OUTPUT-COUNT (current sample number)
           COMPUTE REMAINDER-VAL =
               FUNCTION MOD(OUTPUT-COUNT - 1, PATTERN-LENGTH) + 1.

           MOVE INTERP-MODE(REMAINDER-VAL) TO CURRENT-MODE.
      * 2. Execute the chosen mode
           EVALUATE CURRENT-MODE
               WHEN 1
      * NEAREST NEIGHBOR (Raw)
                   MOVE WAVE-SAMPLE(READ-INDEX) TO INTERP-RESULT

               WHEN 2
      * LINEAR
                   PERFORM CALCULATE-LINEAR-SAMPLE

               WHEN 3
      * SINC
                   PERFORM CALCULATE-SINC-SAMPLE

               WHEN OTHER
      * Fail-safe to Linear
                   PERFORM CALCULATE-LINEAR-SAMPLE
           END-EVALUATE.

       CALCULATE-LINEAR-SAMPLE.
      * ERROR FIX: Using WAVE-SAMPLE here
           MOVE WAVE-SAMPLE(READ-INDEX) TO SAMPLE-A.
           IF READ-INDEX + 1 > TABLE-SIZE
               MOVE WAVE-SAMPLE(1) TO SAMPLE-B
           ELSE
               MOVE WAVE-SAMPLE(READ-INDEX + 1) TO SAMPLE-B
           END-IF.
           COMPUTE INVERSE-FRAC = 1.0 - FRACTIONAL-PART.
           COMPUTE INTERP-RESULT =
               (SAMPLE-A * INVERSE-FRAC) + (SAMPLE-B * FRACTIONAL-PART).
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

      * ERROR FIX: Using WAVE-SAMPLE here
               COMPUTE SUM-ACCUM = SUM-ACCUM +
                   (WAVE-SAMPLE(LOOKUP-INDEX) * FINAL-WEIGHT)
           END-PERFORM.
           MOVE SUM-ACCUM TO INTERP-RESULT.

       APPLY-VOLUME-AND-WRITE.
      * 1. Apply Envelope Volume
           COMPUTE INTERP-RESULT = INTERP-RESULT * RUNNING-AMP.
      * 2. Advance Envelope
           ADD AMP-STEP TO RUNNING-AMP.
      * 3. Convert Float (-1.0 to 1.0) back to PCM Integer
           COMPUTE SCALED-SAMPLE = INTERP-RESULT * 32767.


           IF SCALED-SAMPLE > 32767
               MOVE 32767 TO SCALED-SAMPLE
                   ELSE IF SCALED-SAMPLE < -32768
                       MOVE -32768 TO SCALED-SAMPLE
           END-IF.


           IF SCALED-SAMPLE > 32767
            MOVE 32767 TO SCALED-SAMPLE
               ELSE IF SCALED-SAMPLE < -32768
               MOVE -32768 TO SCALED-SAMPLE
           END-IF.

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

       ADVANCE-POINTERS.
           ADD STEP-SIZE TO READ-POSITION.


           COMPUTE READ-POSITION
               = FUNCTION MOD(READ-POSITION - 1, TABLE-FLOAT) + 1.


           IF READ-POSITION < 1.0
               MOVE 1.0 TO READ-POSITION
           END-IF.
      * /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
      * /\      SECTION 3: TABLES         /\
      * /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\

       INIT-SINC-TABLE.
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
       COPY NOTE-SELECTOR.CPY.

       ASCII-VANITY.
           COPY ASCII-ART.

       GENERATE-FILTER-LUT.
           DISPLAY "Pre-calculating TVF Coefficient Table...".
           PERFORM VARYING LUT-IDX FROM 1 BY 1 UNTIL LUT-IDX > 1001
      * 1. Map Index 1-1001 to Knob Range 0.0 - 100.0
               COMPUTE LUT-KNOB-TEMP = (LUT-IDX - 1) / 10.0

      * 2. Split into Integer and Fractional parts
               COMPUTE KNOB-INT = FUNCTION INTEGER(LUT-KNOB-TEMP)
               COMPUTE KNOB-FRAC = LUT-KNOB-TEMP - KNOB-INT
               COMPUTE LOOKUP-IDX = KNOB-INT + 1

      * 3. Fetch base frequency and next frequency
               MOVE FREQ-HZ(LOOKUP-IDX) TO FREQ-FLOOR

      * Guard against over-reading the table bounds at the maximum value
               IF LOOKUP-IDX < 101
                   MOVE FREQ-HZ(LOOKUP-IDX + 1) TO FREQ-CEIL
               ELSE
                   MOVE FREQ-FLOOR TO FREQ-CEIL
               END-IF

      * 4. Linearly interpolate the exact target frequency
               COMPUTE CURRENT-FREQ-HZ =
                   FREQ-FLOOR + ((FREQ-CEIL - FREQ-FLOOR) * KNOB-FRAC)

      * 5. Proceed with Omega/Trig Math using the smoothed frequency
               COMPUTE NUMERATOR-VAL = CURRENT-FREQ-HZ / SAMPLE-RATE
               COMPUTE ANGULAR-FREQUENCY = PI-2 * NUMERATOR-VAL

      * 6. Do the slow SEARCH lookups here (only happens at startup!)
               PERFORM FIND-SINE-FROM-OMEGA
               PERFORM FIND-COS-FROM-OMEGA

      * 7. Calculate Resonance & Coefficients
               COMPUTE ALPHA-VALUE ROUNDED =
                   FINAL-SINE-VALUE / (2 * Q-RESONANCE)
               PERFORM INIT-COEFFICIENTS

      * 8. Save the final coefficients into the LUT
               MOVE B0-COEFF TO L-B0(LUT-IDX)
               MOVE B1-COEFF TO L-B1(LUT-IDX)
               MOVE B2-COEFF TO L-B2(LUT-IDX)
               MOVE A1-COEFF TO L-A1(LUT-IDX)
               MOVE A2-COEFF TO L-A2(LUT-IDX)
           END-PERFORM.
           DISPLAY "Filter LUT Ready.".
      * /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
      * /\                                              /\
      * /\   SECTION 4: JD800-STYLE LFO ENGINE        /\
      * /\   LFO1 and LFO2 - Full feature set:        /\
      * /\   Rate / Delay / Fade / Waveform / Offset  /\
      * /\   Key Trig / TVA / TVF / Pitch modulation  /\
      * /\   LFO1 can FM-modulate LFO2 rate (extra)   /\
      * /\                                              /\
      * /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\

      * ==========================================
      * GET-LFO-SETTINGS  (HARDCODED VERSION)
      * All values are MOVE statements. Each is tagged with a
      * USER_INPUT comment identifying the parameter for easy editing.
      *
      * WAVEFORM KEY:
      *   1=Sine  2=Triangle  3=Saw-Up  4=Saw-Down
      *   5=Square  6=Sample+Hold  7=Smooth-Random
      *   8=User RAW (loaded from LFO-Wave.raw)
      *
      * RATE: oscillation speed in Hz (0=off, 10=10Hz).
      * DELAY-SEC: seconds of silence before LFO onset.
      * FADE-SEC: +N=fade-in N sec, -N=fade-out N sec, 0=none.
      * OFFSET: DC shift of waveform center (-100..+100).
      * KEY-TRIG: 1=reset phase each note, 0=free-running.
      * TVA-DEPTH: amplitude mod depth (-100..+100).
      * TVF-DEPTH: cutoff mod depth (-100..+100, knob units).
      * PTCH-DEPTH: vibrato depth (-120..+120, 10=1 semitone).
      * PHASE-OFFS: starting phase in degrees (0-359).
      * FM-DEPTH: LFO1->LFO2 rate FM depth (-100..+100).
      * ==========================================
       GET-LFO-SETTINGS.
      * --- LFO 1 ---
      * USER_INPUT: LFO1-WAVEFORM
           MOVE 1 TO LFO1-WAVEFORM.
      * USER_INPUT: LFO1-RATE-HZ
           MOVE 5 TO LFO1-RATE-HZ.
      * USER_INPUT: LFO1-DELAY-SEC
           MOVE 0 TO LFO1-DELAY-SEC.
      * USER_INPUT: LFO1-FADE-SEC
           MOVE 0 TO LFO1-FADE-SEC.
      * USER_INPUT: LFO1-OFFSET
           MOVE 0 TO LFO1-OFFSET.
      * USER_INPUT: LFO1-KEY-TRIG
           MOVE 1 TO LFO1-KEY-TRIG.
      * USER_INPUT: LFO1-TVA-DEPTH
           MOVE 0 TO LFO1-TVA-DEPTH.
      * USER_INPUT: LFO1-TVF-DEPTH
           MOVE 0 TO LFO1-TVF-DEPTH.
      * USER_INPUT: LFO1-PTCH-DEPTH
           MOVE 0 TO LFO1-PTCH-DPTH.
      * USER_INPUT: LFO1-PHASE-OFFS
           MOVE 0 TO LFO1-PHASE-OFFS.
      * USER_INPUT: LFO1-FM-DEPTH (modulates LFO2 rate)
           MOVE 0 TO LFO1-FM-DEPTH.
      * --- LFO 2 ---
      * USER_INPUT: LFO2-WAVEFORM
           MOVE 1 TO LFO2-WAVEFORM.
      * USER_INPUT: LFO2-RATE-HZ
           MOVE 0 TO LFO2-RATE-HZ.
      * USER_INPUT: LFO2-DELAY-SEC
           MOVE 0 TO LFO2-DELAY-SEC.
      * USER_INPUT: LFO2-FADE-SEC
           MOVE 0 TO LFO2-FADE-SEC.
      * USER_INPUT: LFO2-OFFSET
           MOVE 0 TO LFO2-OFFSET.
      * USER_INPUT: LFO2-KEY-TRIG
           MOVE 1 TO LFO2-KEY-TRIG.
      * USER_INPUT: LFO2-TVA-DEPTH
           MOVE 0 TO LFO2-TVA-DEPTH.
      * USER_INPUT: LFO2-TVF-DEPTH
           MOVE 0 TO LFO2-TVF-DEPTH.
      * USER_INPUT: LFO2-PTCH-DEPTH
           MOVE 0 TO LFO2-PTCH-DPTH.
      * USER_INPUT: LFO2-PHASE-OFFS
           MOVE 0 TO LFO2-PHASE-OFFS.

      * ==========================================
      * INIT-LFO-WAVEFORMS
      * Top-level controller. Dispatches to waveform builders,
      * then pre-computes runtime state for both LFOs.
      * ==========================================
       INIT-LFO-WAVEFORMS.
           PERFORM INIT-LFO1-TABLE.
           PERFORM INIT-LFO2-TABLE.
           PERFORM INIT-LFO1-STATE.
           PERFORM INIT-LFO2-STATE.

       INIT-LFO1-TABLE.
           EVALUATE LFO1-WAVEFORM
               WHEN 1 PERFORM GEN-SINE-TO-LFO1
               WHEN 2 PERFORM GEN-TRI-TO-LFO1
               WHEN 3 PERFORM GEN-SAW-UP-TO-LFO1
               WHEN 4 PERFORM GEN-SAW-DN-TO-LFO1
               WHEN 5 PERFORM GEN-SQUARE-TO-LFO1
               WHEN 6 PERFORM GEN-SH-TO-LFO1
               WHEN 7 PERFORM GEN-SMTH-RND-TO-LFO1
               WHEN 8 PERFORM LOAD-LFO-RAW-TO-LFO1
               WHEN OTHER PERFORM GEN-SINE-TO-LFO1
           END-EVALUATE.

       INIT-LFO2-TABLE.
           EVALUATE LFO2-WAVEFORM
               WHEN 1 PERFORM GEN-SINE-TO-LFO2
               WHEN 2 PERFORM GEN-TRI-TO-LFO2
               WHEN 3 PERFORM GEN-SAW-UP-TO-LFO2
               WHEN 4 PERFORM GEN-SAW-DN-TO-LFO2
               WHEN 5 PERFORM GEN-SQUARE-TO-LFO2
               WHEN 6 PERFORM GEN-SH-TO-LFO2
               WHEN 7 PERFORM GEN-SMTH-RND-TO-LFO2
               WHEN 8 PERFORM LOAD-LFO-RAW-TO-LFO2
               WHEN OTHER PERFORM GEN-SINE-TO-LFO2
           END-EVALUATE.

      * ==========================================
      * INIT-LFO1-STATE
      * Pre-computes STEP (wavetable index advance per sample),
      * converts DELAY-SEC and FADE-SEC to sample counts, and
      * seeds the initial phase from PHASE-OFFS degrees.
      * ==========================================
       INIT-LFO1-STATE.
           IF LFO1-RATE-HZ = 0
               MOVE 0 TO LFO1-STEP
           ELSE
               COMPUTE LFO1-STEP =
                   2048.0 * LFO1-RATE-HZ / SAMPLE-RATE
           END-IF.
           COMPUTE LFO1-DELAY-SMPL =
               LFO1-DELAY-SEC * SAMPLE-RATE.
           MOVE LFO1-DELAY-SMPL TO LFO1-DELAY-CNT.
           EVALUATE TRUE
               WHEN LFO1-FADE-SEC = 0
                   MOVE 1.0 TO LFO1-FADE-AMT
                   MOVE 0   TO LFO1-FADE-TOTAL
                   MOVE 0   TO LFO1-FADE-STEP
               WHEN LFO1-FADE-SEC > 0
      * Positive: fade in - amplitude starts at 0, ramps to 1
                   MOVE 0.0 TO LFO1-FADE-AMT
                   COMPUTE LFO1-FADE-TOTAL =
                       LFO1-FADE-SEC * SAMPLE-RATE
                   COMPUTE LFO1-FADE-STEP =
                       1.0 / LFO1-FADE-TOTAL
               WHEN OTHER
      * Negative: fade out - amplitude starts at 1, ramps to 0
                   MOVE 1.0 TO LFO1-FADE-AMT
                   COMPUTE LFO1-FADE-TOTAL =
                       FUNCTION ABS(LFO1-FADE-SEC) * SAMPLE-RATE
                   COMPUTE LFO1-FADE-STEP =
                       -1.0 / LFO1-FADE-TOTAL
           END-EVALUATE.
           MOVE 0 TO LFO1-FADE-CNT.
           IF LFO1-PHASE-OFFS > 0
               COMPUTE LFO1-PHASE =
                   (LFO1-PHASE-OFFS / 360.0) * 2048.0
           ELSE
               MOVE 0 TO LFO1-PHASE
           END-IF.
           MOVE 0 TO LFO1-IS-ACTIVE.
           MOVE 0 TO LFO1-SH-HELD.
           IF LFO1-STEP > 0
               COMPUTE LFO1-SH-PERIOD =
                   FUNCTION INTEGER(2048.0 / LFO1-STEP)
               IF LFO1-SH-PERIOD < 1
                   MOVE 1 TO LFO1-SH-PERIOD
               END-IF
           ELSE
               MOVE 9999999 TO LFO1-SH-PERIOD
           END-IF.
           MOVE 0 TO LFO1-SH-CNT.
           MOVE 0 TO LFO1-PREV-IDX.

       INIT-LFO2-STATE.
           IF LFO2-RATE-HZ = 0
               MOVE 0 TO LFO2-STEP
           ELSE
               COMPUTE LFO2-STEP =
                   2048.0 * LFO2-RATE-HZ / SAMPLE-RATE
           END-IF.
           COMPUTE LFO2-DELAY-SMPL =
               LFO2-DELAY-SEC * SAMPLE-RATE.
           MOVE LFO2-DELAY-SMPL TO LFO2-DELAY-CNT.
           EVALUATE TRUE
               WHEN LFO2-FADE-SEC = 0
                   MOVE 1.0 TO LFO2-FADE-AMT
                   MOVE 0   TO LFO2-FADE-TOTAL
                   MOVE 0   TO LFO2-FADE-STEP
               WHEN LFO2-FADE-SEC > 0
                   MOVE 0.0 TO LFO2-FADE-AMT
                   COMPUTE LFO2-FADE-TOTAL =
                       LFO2-FADE-SEC * SAMPLE-RATE
                   COMPUTE LFO2-FADE-STEP =
                       1.0 / LFO2-FADE-TOTAL
               WHEN OTHER
                   MOVE 1.0 TO LFO2-FADE-AMT
                   COMPUTE LFO2-FADE-TOTAL =
                       FUNCTION ABS(LFO2-FADE-SEC) * SAMPLE-RATE
                   COMPUTE LFO2-FADE-STEP =
                       -1.0 / LFO2-FADE-TOTAL
           END-EVALUATE.
           MOVE 0 TO LFO2-FADE-CNT.
           IF LFO2-PHASE-OFFS > 0
               COMPUTE LFO2-PHASE =
                   (LFO2-PHASE-OFFS / 360.0) * 2048.0
           ELSE
               MOVE 0 TO LFO2-PHASE
           END-IF.
           MOVE 0 TO LFO2-IS-ACTIVE.
           MOVE 0 TO LFO2-SH-HELD.
           IF LFO2-STEP > 0
               COMPUTE LFO2-SH-PERIOD =
                   FUNCTION INTEGER(2048.0 / LFO2-STEP)
               IF LFO2-SH-PERIOD < 1
                   MOVE 1 TO LFO2-SH-PERIOD
               END-IF
           ELSE
               MOVE 9999999 TO LFO2-SH-PERIOD
           END-IF.
           MOVE 0 TO LFO2-SH-CNT.
           MOVE 0 TO LFO2-PREV-IDX.

      * ==========================================
      * ADVANCE-LFO-ENGINES
      * Called once per sample. Drives both oscillators forward,
      * then combines their outputs into TVA/TVF/Pitch mod signals.
      * LFO1 FM: LFO1-VALUE scales LFO2 step when FM-DEPTH <> 0.
      * ==========================================
       ADVANCE-LFO-ENGINES.
           PERFORM ADVANCE-LFO1.
           PERFORM ADVANCE-LFO2.
           COMPUTE LFO-TVA-OUT =
               (LFO1-VALUE * LFO1-TVA-DEPTH / 100.0) +
               (LFO2-VALUE * LFO2-TVA-DEPTH / 100.0).
           COMPUTE LFO-TVF-OUT =
               (LFO1-VALUE * LFO1-TVF-DEPTH) +
               (LFO2-VALUE * LFO2-TVF-DEPTH).
           COMPUTE LFO-PTCH-OUT =
               (LFO1-VALUE * LFO1-PTCH-DPTH / 10.0) +
               (LFO2-VALUE * LFO2-PTCH-DPTH / 10.0).

      * ==========================================
      * ADVANCE-LFO1
      * Core per-sample oscillator tick for LFO1.
      * Sequence: delay -> fade -> phase advance -> table lookup
      *           -> DC offset -> fade multiply -> output.
      * S+H mode holds a random value for SH-PERIOD samples then
      * re-samples. All other modes use linear interpolation.
      * ==========================================
       ADVANCE-LFO1.
           IF LFO1-STEP = 0
               MOVE 0 TO LFO1-VALUE
               EXIT PARAGRAPH
           END-IF.
      * 1. Delay: suppress output until delay counter expires
           IF LFO1-DELAY-CNT > 0
               SUBTRACT 1 FROM LFO1-DELAY-CNT
               MOVE 0 TO LFO1-VALUE
               EXIT PARAGRAPH
           END-IF.
           MOVE 1 TO LFO1-IS-ACTIVE.
      * 2. Advance fade envelope
           IF LFO1-FADE-TOTAL > 0 AND
              LFO1-FADE-CNT < LFO1-FADE-TOTAL
               ADD 1 TO LFO1-FADE-CNT
               ADD LFO1-FADE-STEP TO LFO1-FADE-AMT
               IF LFO1-FADE-AMT > 1.0
                   MOVE 1.0 TO LFO1-FADE-AMT
               END-IF
               IF LFO1-FADE-AMT < 0.0
                   MOVE 0.0 TO LFO1-FADE-AMT
               END-IF
           END-IF.
      * 3. Advance phase, wrap within 0..2047
           ADD LFO1-STEP TO LFO1-PHASE.
           PERFORM UNTIL LFO1-PHASE < 2048.0
               SUBTRACT 2048.0 FROM LFO1-PHASE
           END-PERFORM.
      * 4a. S+H mode: hold last value until period elapses
           IF LFO1-SH
               ADD 1 TO LFO1-SH-CNT
               IF LFO1-SH-CNT >= LFO1-SH-PERIOD
                   MOVE 0 TO LFO1-SH-CNT
                   PERFORM GEN-LFO-RANDOM
                   COMPUTE LFO1-SH-HELD =
                       (LFO-RND-RESULT * 2.0) - 1.0
               END-IF
               COMPUTE LFO-WORK-VAL = LFO1-SH-HELD
           ELSE
      * 4b. All other modes: interpolated table lookup
               COMPUTE LFO-CALC-IDX =
                   FUNCTION INTEGER(LFO1-PHASE) + 1
               IF LFO-CALC-IDX < 1
                   MOVE 1 TO LFO-CALC-IDX
               END-IF
               IF LFO-CALC-IDX > 2048
                   MOVE 2048 TO LFO-CALC-IDX
               END-IF
               COMPUTE LFO-NEXT-IDX = LFO-CALC-IDX + 1
               IF LFO-NEXT-IDX > 2048
                   MOVE 1 TO LFO-NEXT-IDX
               END-IF
               COMPUTE LFO-FRAC =
                   LFO1-PHASE - (LFO-CALC-IDX - 1)
               MOVE LFO1-TBL-SAMP(LFO-CALC-IDX) TO LFO-SAMP-A
               MOVE LFO1-TBL-SAMP(LFO-NEXT-IDX) TO LFO-SAMP-B
               COMPUTE LFO-WORK-VAL = LFO-SAMP-A +
                   (LFO-FRAC * (LFO-SAMP-B - LFO-SAMP-A))
           END-IF.
      * 5. Apply DC offset (-100..+100 / 100 = +/-1.0)
           IF LFO1-OFFSET NOT = 0
               COMPUTE LFO-WORK-VAL =
                   LFO-WORK-VAL + (LFO1-OFFSET / 100.0)
           END-IF.
           IF LFO-WORK-VAL >  1.0 MOVE  1.0 TO LFO-WORK-VAL END-IF.
           IF LFO-WORK-VAL < -1.0 MOVE -1.0 TO LFO-WORK-VAL END-IF.
      * 6. Apply fade multiplier
           COMPUTE LFO1-VALUE = LFO-WORK-VAL * LFO1-FADE-AMT.

      * ==========================================
      * ADVANCE-LFO2
      * Identical to ADVANCE-LFO1 except LFO2 state is used
      * and FM modulation from LFO1 is applied to the step size.
      * FM formula: effective_step = LFO2-STEP * (1 + FM_depth%)
      * This produces frequency modulation of LFO2's rate by LFO1.
      * ==========================================
       ADVANCE-LFO2.
           IF LFO2-STEP = 0
               MOVE 0 TO LFO2-VALUE
               EXIT PARAGRAPH
           END-IF.
           IF LFO2-DELAY-CNT > 0
               SUBTRACT 1 FROM LFO2-DELAY-CNT
               MOVE 0 TO LFO2-VALUE
               EXIT PARAGRAPH
           END-IF.
           MOVE 1 TO LFO2-IS-ACTIVE.
           IF LFO2-FADE-TOTAL > 0 AND
              LFO2-FADE-CNT < LFO2-FADE-TOTAL
               ADD 1 TO LFO2-FADE-CNT
               ADD LFO2-FADE-STEP TO LFO2-FADE-AMT
               IF LFO2-FADE-AMT > 1.0
                   MOVE 1.0 TO LFO2-FADE-AMT
               END-IF
               IF LFO2-FADE-AMT < 0.0
                   MOVE 0.0 TO LFO2-FADE-AMT
               END-IF
           END-IF.
      * FM modulation: LFO1 output scales LFO2 step
           IF LFO1-FM-DEPTH NOT = 0
               COMPUTE LFO-MOD-RATE =
                   LFO1-VALUE * LFO1-FM-DEPTH / 100.0
               COMPUTE LFO-CALC-STEP =
                   LFO2-STEP * (1.0 + LFO-MOD-RATE)
               IF LFO-CALC-STEP < 0
                   MOVE 0 TO LFO-CALC-STEP
               END-IF
           ELSE
               MOVE LFO2-STEP TO LFO-CALC-STEP
           END-IF.
           ADD LFO-CALC-STEP TO LFO2-PHASE.
           PERFORM UNTIL LFO2-PHASE < 2048.0
               SUBTRACT 2048.0 FROM LFO2-PHASE
           END-PERFORM.
           IF LFO2-SH
               ADD 1 TO LFO2-SH-CNT
               IF LFO2-SH-CNT >= LFO2-SH-PERIOD
                   MOVE 0 TO LFO2-SH-CNT
                   PERFORM GEN-LFO-RANDOM
                   COMPUTE LFO2-SH-HELD =
                       (LFO-RND-RESULT * 2.0) - 1.0
               END-IF
               COMPUTE LFO-WORK-VAL = LFO2-SH-HELD
           ELSE
               COMPUTE LFO-CALC-IDX =
                   FUNCTION INTEGER(LFO2-PHASE) + 1
               IF LFO-CALC-IDX < 1
                   MOVE 1 TO LFO-CALC-IDX
               END-IF
               IF LFO-CALC-IDX > 2048
                   MOVE 2048 TO LFO-CALC-IDX
               END-IF
               COMPUTE LFO-NEXT-IDX = LFO-CALC-IDX + 1
               IF LFO-NEXT-IDX > 2048
                   MOVE 1 TO LFO-NEXT-IDX
               END-IF
               COMPUTE LFO-FRAC =
                   LFO2-PHASE - (LFO-CALC-IDX - 1)
               MOVE LFO2-TBL-SAMP(LFO-CALC-IDX) TO LFO-SAMP-A
               MOVE LFO2-TBL-SAMP(LFO-NEXT-IDX) TO LFO-SAMP-B
               COMPUTE LFO-WORK-VAL = LFO-SAMP-A +
                   (LFO-FRAC * (LFO-SAMP-B - LFO-SAMP-A))
           END-IF.
           IF LFO2-OFFSET NOT = 0
               COMPUTE LFO-WORK-VAL =
                   LFO-WORK-VAL + (LFO2-OFFSET / 100.0)
           END-IF.
           IF LFO-WORK-VAL >  1.0 MOVE  1.0 TO LFO-WORK-VAL END-IF.
           IF LFO-WORK-VAL < -1.0 MOVE -1.0 TO LFO-WORK-VAL END-IF.
           COMPUTE LFO2-VALUE = LFO-WORK-VAL * LFO2-FADE-AMT.

      * ==========================================
      * APPLY-LFO-TVF
      * Adds the combined LFO TVF output to CURRENT-KNOB.
      * RECALCULATE-COEFFICIENTS clamps DEPTH-CALC to 0-100 after
      * applying TVF-DEPTH scaling, so no extra clamping needed here.
      * ==========================================
       APPLY-LFO-TVF.
           IF LFO-TVF-OUT NOT = 0
               ADD LFO-TVF-OUT TO CURRENT-KNOB
           END-IF.

      * ==========================================
      * APPLY-LFO-TVA
      * Scales INTERP-RESULT by (1 + LFO-TVA-OUT) for tremolo.
      * LFO-TVA-OUT = 0.0 leaves the signal unchanged.
      * Result is clamped to -1.0..+1.0 before volume write.
      * ==========================================
       APPLY-LFO-TVA.
           IF LFO-TVA-OUT NOT = 0
               COMPUTE INTERP-RESULT =
                   INTERP-RESULT * (1.0 + LFO-TVA-OUT)
               IF INTERP-RESULT >  1.0
                   MOVE  1.0 TO INTERP-RESULT
               END-IF
               IF INTERP-RESULT < -1.0
                   MOVE -1.0 TO INTERP-RESULT
               END-IF
           END-IF.

      * ==========================================
      * APPLY-LFO-PITCH
      * Adds a fractional offset to READ-POSITION to produce vibrato.
      * Semitone ratio approximation: 2^(1/12)-1 = 0.059463094.
      * LFO-PTCH-OUT is in semitones (1.0 = 1 semitone).
      * Called after CALCULATE-INDICES so READ-INDEX is already set.
      * ==========================================
       APPLY-LFO-PITCH.
           IF LFO-PTCH-OUT NOT = 0
               COMPUTE LFO-PTCH-ADJ =
                   STEP-SIZE * LFO-PTCH-OUT * 0.05946309
               ADD LFO-PTCH-ADJ TO READ-POSITION
           END-IF.

      * ==========================================
      * WAVEFORM GENERATORS - LFO1
      * Fill the 2048-entry LFO1-WAVE-TABLE with values -1.0..+1.0.
      * ==========================================

      * --- SINE: Uses FUNCTION SIN for maximum accuracy ---
       GEN-SINE-TO-LFO1.
           PERFORM VARYING LFO-BUILD-IDX FROM 1 BY 1
               UNTIL LFO-BUILD-IDX > 2048
               COMPUTE LFO-BUILD-ANG =
                   LFO-2PI * (LFO-BUILD-IDX - 1) / 2048
               COMPUTE LFO1-TBL-SAMP(LFO-BUILD-IDX) =
                   FUNCTION SIN(LFO-BUILD-ANG)
           END-PERFORM.

      * --- TRIANGLE: ramp 0->+1->-1->0 over 2048 entries ---
       GEN-TRI-TO-LFO1.
           PERFORM VARYING LFO-BUILD-IDX FROM 1 BY 1
               UNTIL LFO-BUILD-IDX > 2048
               COMPUTE LFO-BUILD-ANG =
                   (LFO-BUILD-IDX - 1) / 2048.0
               EVALUATE TRUE
                 WHEN LFO-BUILD-ANG < 0.25
                   COMPUTE LFO1-TBL-SAMP(LFO-BUILD-IDX) =
                       LFO-BUILD-ANG * 4.0
                 WHEN LFO-BUILD-ANG < 0.75
                   COMPUTE LFO1-TBL-SAMP(LFO-BUILD-IDX) =
                       2.0 - (LFO-BUILD-ANG * 4.0)
                 WHEN OTHER
                   COMPUTE LFO1-TBL-SAMP(LFO-BUILD-IDX) =
                       (LFO-BUILD-ANG * 4.0) - 4.0
               END-EVALUATE
           END-PERFORM.

      * --- SAW UP: linear ramp from -1.0 to +1.0 ---
       GEN-SAW-UP-TO-LFO1.
           PERFORM VARYING LFO-BUILD-IDX FROM 1 BY 1
               UNTIL LFO-BUILD-IDX > 2048
               COMPUTE LFO1-TBL-SAMP(LFO-BUILD-IDX) =
                   ((LFO-BUILD-IDX - 1) / 1024.0) - 1.0
           END-PERFORM.

      * --- SAW DOWN: linear ramp from +1.0 to -1.0 ---
       GEN-SAW-DN-TO-LFO1.
           PERFORM VARYING LFO-BUILD-IDX FROM 1 BY 1
               UNTIL LFO-BUILD-IDX > 2048
               COMPUTE LFO1-TBL-SAMP(LFO-BUILD-IDX) =
                   1.0 - ((LFO-BUILD-IDX - 1) / 1024.0)
           END-PERFORM.

      * --- SQUARE: first 1024 entries = +1.0, rest = -1.0 ---
       GEN-SQUARE-TO-LFO1.
           PERFORM VARYING LFO-BUILD-IDX FROM 1 BY 1
               UNTIL LFO-BUILD-IDX > 2048
               IF LFO-BUILD-IDX <= 1024
                   MOVE 1.0 TO LFO1-TBL-SAMP(LFO-BUILD-IDX)
               ELSE
                   MOVE -1.0 TO LFO1-TBL-SAMP(LFO-BUILD-IDX)
               END-IF
           END-PERFORM.

      * --- S+H: 16 random steps each 128 entries wide ---
      * Table is pre-populated; ADVANCE-LFO1 still uses SH-PERIOD
      * logic for real-time random capture in the sample loop.
       GEN-SH-TO-LFO1.
           MOVE 0 TO LFO-WORK-VAL.
           PERFORM VARYING LFO-BUILD-IDX FROM 1 BY 1
               UNTIL LFO-BUILD-IDX > 2048
               IF FUNCTION MOD(LFO-BUILD-IDX - 1, 128) = 0
                   PERFORM GEN-LFO-RANDOM
                   COMPUTE LFO-WORK-VAL =
                       (LFO-RND-RESULT * 2.0) - 1.0
               END-IF
               MOVE LFO-WORK-VAL TO LFO1-TBL-SAMP(LFO-BUILD-IDX)
           END-PERFORM.

      * --- SMOOTH RANDOM: linearly interpolated random steps ---
      * 16 random target values, each blended over 128 entries.
       GEN-SMTH-RND-TO-LFO1.
           MOVE 0 TO LFO-SAMP-A.
           PERFORM GEN-LFO-RANDOM.
           COMPUTE LFO-SAMP-B = (LFO-RND-RESULT * 2.0) - 1.0.
           PERFORM VARYING LFO-BUILD-IDX FROM 1 BY 1
               UNTIL LFO-BUILD-IDX > 2048
               COMPUTE LFO-FRAC =
                   FUNCTION MOD(LFO-BUILD-IDX - 1, 128) / 128.0
               IF FUNCTION MOD(LFO-BUILD-IDX - 1, 128) = 0 AND
                  LFO-BUILD-IDX > 1
                   MOVE LFO-SAMP-B TO LFO-SAMP-A
                   PERFORM GEN-LFO-RANDOM
                   COMPUTE LFO-SAMP-B =
                       (LFO-RND-RESULT * 2.0) - 1.0
               END-IF
               COMPUTE LFO1-TBL-SAMP(LFO-BUILD-IDX) =
                   LFO-SAMP-A +
                   (LFO-FRAC * (LFO-SAMP-B - LFO-SAMP-A))
           END-PERFORM.

      * --- USER RAW: load signed 16-bit PCM from LFO-Wave.raw ---
      * Reads 2048 samples (4096 bytes) little-endian.
      * Entries beyond end-of-file are zeroed.
       LOAD-LFO-RAW-TO-LFO1.
           OPEN INPUT LFO-FILE.
           PERFORM VARYING LFO-BUILD-IDX FROM 1 BY 1
               UNTIL LFO-BUILD-IDX > 2048
               MOVE 0 TO LFO1-TBL-SAMP(LFO-BUILD-IDX)
               READ LFO-FILE INTO LFO-FILE-BUF(1:1)
                   AT END EXIT PERFORM
                   NOT AT END
                       READ LFO-FILE INTO LFO-FILE-BUF(2:1)
                           AT END EXIT PERFORM
                           NOT AT END
                               COMPUTE LFO-FILE-W1 =
                                 FUNCTION ORD
                                 (LFO-FILE-BUF(1:1)) - 1
                               COMPUTE LFO-FILE-W2 =
                                 FUNCTION ORD
                                 (LFO-FILE-BUF(2:1)) - 1
                               COMPUTE LFO-FILE-W1 =
                                 LFO-FILE-W1 +
                                 (LFO-FILE-W2 * 256)
                               IF LFO-FILE-W1 > 32767
                                   SUBTRACT 65536
                                       FROM LFO-FILE-W1
                               END-IF
                               COMPUTE LFO1-TBL-SAMP
                                 (LFO-BUILD-IDX) =
                                 LFO-FILE-W1 / 32768.0
                       END-READ
               END-READ
           END-PERFORM.
           CLOSE LFO-FILE.

      * ==========================================
      * WAVEFORM GENERATORS - LFO2
      * Mirror of LFO1 generators writing into LFO2-WAVE-TABLE.
      * USER RAW copies from LFO1 table if LFO1 also used type 8,
      * otherwise re-reads LFO-Wave.raw directly.
      * ==========================================

       GEN-SINE-TO-LFO2.
           PERFORM VARYING LFO-BUILD-IDX FROM 1 BY 1
               UNTIL LFO-BUILD-IDX > 2048
               COMPUTE LFO-BUILD-ANG =
                   LFO-2PI * (LFO-BUILD-IDX - 1) / 2048
               COMPUTE LFO2-TBL-SAMP(LFO-BUILD-IDX) =
                   FUNCTION SIN(LFO-BUILD-ANG)
           END-PERFORM.

       GEN-TRI-TO-LFO2.
           PERFORM VARYING LFO-BUILD-IDX FROM 1 BY 1
               UNTIL LFO-BUILD-IDX > 2048
               COMPUTE LFO-BUILD-ANG =
                   (LFO-BUILD-IDX - 1) / 2048.0
               EVALUATE TRUE
                 WHEN LFO-BUILD-ANG < 0.25
                   COMPUTE LFO2-TBL-SAMP(LFO-BUILD-IDX) =
                       LFO-BUILD-ANG * 4.0
                 WHEN LFO-BUILD-ANG < 0.75
                   COMPUTE LFO2-TBL-SAMP(LFO-BUILD-IDX) =
                       2.0 - (LFO-BUILD-ANG * 4.0)
                 WHEN OTHER
                   COMPUTE LFO2-TBL-SAMP(LFO-BUILD-IDX) =
                       (LFO-BUILD-ANG * 4.0) - 4.0
               END-EVALUATE
           END-PERFORM.

       GEN-SAW-UP-TO-LFO2.
           PERFORM VARYING LFO-BUILD-IDX FROM 1 BY 1
               UNTIL LFO-BUILD-IDX > 2048
               COMPUTE LFO2-TBL-SAMP(LFO-BUILD-IDX) =
                   ((LFO-BUILD-IDX - 1) / 1024.0) - 1.0
           END-PERFORM.

       GEN-SAW-DN-TO-LFO2.
           PERFORM VARYING LFO-BUILD-IDX FROM 1 BY 1
               UNTIL LFO-BUILD-IDX > 2048
               COMPUTE LFO2-TBL-SAMP(LFO-BUILD-IDX) =
                   1.0 - ((LFO-BUILD-IDX - 1) / 1024.0)
           END-PERFORM.

       GEN-SQUARE-TO-LFO2.
           PERFORM VARYING LFO-BUILD-IDX FROM 1 BY 1
               UNTIL LFO-BUILD-IDX > 2048
               IF LFO-BUILD-IDX <= 1024
                   MOVE 1.0 TO LFO2-TBL-SAMP(LFO-BUILD-IDX)
               ELSE
                   MOVE -1.0 TO LFO2-TBL-SAMP(LFO-BUILD-IDX)
               END-IF
           END-PERFORM.

       GEN-SH-TO-LFO2.
           MOVE 0 TO LFO-WORK-VAL.
           PERFORM VARYING LFO-BUILD-IDX FROM 1 BY 1
               UNTIL LFO-BUILD-IDX > 2048
               IF FUNCTION MOD(LFO-BUILD-IDX - 1, 128) = 0
                   PERFORM GEN-LFO-RANDOM
                   COMPUTE LFO-WORK-VAL =
                       (LFO-RND-RESULT * 2.0) - 1.0
               END-IF
               MOVE LFO-WORK-VAL TO LFO2-TBL-SAMP(LFO-BUILD-IDX)
           END-PERFORM.

       GEN-SMTH-RND-TO-LFO2.
           MOVE 0 TO LFO-SAMP-A.
           PERFORM GEN-LFO-RANDOM.
           COMPUTE LFO-SAMP-B = (LFO-RND-RESULT * 2.0) - 1.0.
           PERFORM VARYING LFO-BUILD-IDX FROM 1 BY 1
               UNTIL LFO-BUILD-IDX > 2048
               COMPUTE LFO-FRAC =
                   FUNCTION MOD(LFO-BUILD-IDX - 1, 128) / 128.0
               IF FUNCTION MOD(LFO-BUILD-IDX - 1, 128) = 0 AND
                  LFO-BUILD-IDX > 1
                   MOVE LFO-SAMP-B TO LFO-SAMP-A
                   PERFORM GEN-LFO-RANDOM
                   COMPUTE LFO-SAMP-B =
                       (LFO-RND-RESULT * 2.0) - 1.0
               END-IF
               COMPUTE LFO2-TBL-SAMP(LFO-BUILD-IDX) =
                   LFO-SAMP-A +
                   (LFO-FRAC * (LFO-SAMP-B - LFO-SAMP-A))
           END-PERFORM.

      * LFO2 USER RAW: re-use LFO1 table if it also loaded type 8,
      * otherwise open LFO-Wave.raw and load independently.
       LOAD-LFO-RAW-TO-LFO2.
           IF LFO1-USER-RAW
               PERFORM VARYING LFO-BUILD-IDX FROM 1 BY 1
                   UNTIL LFO-BUILD-IDX > 2048
                   MOVE LFO1-TBL-SAMP(LFO-BUILD-IDX) TO
                       LFO2-TBL-SAMP(LFO-BUILD-IDX)
               END-PERFORM
           ELSE
               OPEN INPUT LFO-FILE
               PERFORM VARYING LFO-BUILD-IDX FROM 1 BY 1
                   UNTIL LFO-BUILD-IDX > 2048
                   MOVE 0 TO LFO2-TBL-SAMP(LFO-BUILD-IDX)
                   READ LFO-FILE INTO LFO-FILE-BUF(1:1)
                       AT END EXIT PERFORM
                       NOT AT END
                           READ LFO-FILE INTO LFO-FILE-BUF(2:1)
                               AT END EXIT PERFORM
                               NOT AT END
                                   COMPUTE LFO-FILE-W1 =
                                     FUNCTION ORD
                                     (LFO-FILE-BUF(1:1)) - 1
                                   COMPUTE LFO-FILE-W2 =
                                     FUNCTION ORD
                                     (LFO-FILE-BUF(2:1)) - 1
                                   COMPUTE LFO-FILE-W1 =
                                     LFO-FILE-W1 +
                                     (LFO-FILE-W2 * 256)
                                   IF LFO-FILE-W1 > 32767
                                       SUBTRACT 65536
                                           FROM LFO-FILE-W1
                                   END-IF
                                   COMPUTE LFO2-TBL-SAMP
                                     (LFO-BUILD-IDX) =
                                     LFO-FILE-W1 / 32768.0
                           END-READ
                   END-READ
               END-PERFORM
               CLOSE LFO-FILE
           END-IF.

      * ==========================================
      * GEN-LFO-RANDOM
      * Park-Miller LCG. Writes a float 0.0..1.0 to LFO-RND-RESULT.
      * Uses its own seed so it runs independently from the
      * analogue-engine GENERATE-DRIFT Park-Miller instance.
      * ==========================================
       GEN-LFO-RANDOM.
           COMPUTE LFO-RND-SEED = FUNCTION MOD(
               (1103515245 * LFO-RND-SEED + 12345),
               2147483647).
           COMPUTE LFO-RND-RESULT =
               LFO-RND-SEED / 2147483647.
