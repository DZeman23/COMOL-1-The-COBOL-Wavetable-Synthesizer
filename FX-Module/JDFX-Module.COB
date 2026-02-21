       IDENTIFICATION DIVISION.
       PROGRAM-ID. COMOL-FX.
      *=============================================================*
      * COMOL-FX: STEREO NUCLEAR EDITION - V3 (JAGGED CHORUS FIXED  *
      *           + POWERFUL REVERB + FAST SINE TABLES)             *
      * Smooth chorus + powerful reverb, 100% within original       *
      * variables and line limits. Tables now generated at runtime  *
      * in < 0.1 sec. No more massive COPY files.                   *
      *=============================================================*

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT IN-FILE  ASSIGN TO
           "your/filepath/here.raw"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE  IS SEQUENTIAL
               FILE STATUS  IS WS-IN-STATUS.

           SELECT SINC-FILE ASSIGN TO
           "your/filepath/here.raw"
           ORGANIZATION IS BINARY SEQUENTIAL.

           SELECT OUT-FILE-L ASSIGN TO
           "C:\Users\Vicky Myburgh\Desktop\Sine Generator\Output2.raw"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE  IS SEQUENTIAL
               FILE STATUS  IS WS-OUT-STATUS.

           SELECT OUT-FILE-R ASSIGN TO
           "C:\Users\Vicky Myburgh\Desktop\Sine Generator\Output3.raw"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE  IS SEQUENTIAL
               FILE STATUS  IS WS-OUT-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD  IN-FILE
           RECORD CONTAINS 2 CHARACTERS.
       01  IN-RECORD               PIC X(2).

       FD  OUT-FILE-L
           RECORD CONTAINS 2 CHARACTERS.
       01  OUT-RECORD-L            PIC X(2).

       FD  OUT-FILE-R
           RECORD CONTAINS 2 CHARACTERS.
       01  OUT-RECORD-R            PIC X(2).
       FD  SINC-FILE.
       01  SINC-FILE-RECORD  PIC X(8).

       WORKING-STORAGE SECTION.
      * The precise calculated position (e.g., 380.45921)
       01  WS-EXACT-POS      PIC 9(6)V9(5).

      * The Integer part (e.g., 380) - Used for Index A
       01  WS-INDEX-A        PIC 9(6).

      * The Integer part + 1 (e.g., 381) - Used for Index B
       01  WS-INDEX-B        PIC 9(6).

      * The Fractional part (e.g., 0.45921) - Used for Blending
       01  WS-FRACTION       PIC V9(5).
       01  WS-INV-FRACTION   PIC V9(5).

       01  CHORUS-SINC-TABLE.
           05  CHORUS-PHASE-ROW OCCURS 9000 TIMES
                                INDEXED BY CHORUS-PHASE-IDX.
               10  CHORUS-FILTER-TAP OCCURS 9 TIMES
                                     PIC S9(13) COMP-5.

       01  WS-IN-STATUS            PIC X(2).
       01  WS-OUT-STATUS           PIC X(2).

       01  WS-IN-SAMPLE.
           05  WS-IN-BYTES         PIC X(2).
           05  WS-IN-INT  REDEFINES WS-IN-BYTES PIC S9(4) COMP-5.

       01  WS-OUT-SAMPLE.
           05  WS-OUT-BYTES        PIC X(2).
           05  WS-OUT-INT REDEFINES WS-OUT-BYTES PIC S9(4) COMP-5.


      *COPY "NOTE-FREQ-WS.CPY".
      *COPY "Sine-Weights-WS.cpy".

      * Lightweight high-resolution sine tables (generated at startup)
       01  FX-SINE-TABLE.
           05  FX-SINE-ENTRY OCCURS 16384 TIMES
                           INDEXED BY FX-SINE-IDX.
               10  FX-OMEGA-RAW   PIC S9(5)V9(8) VALUE 0.
               10  FX-SINE-RAW    PIC S9(1)V9(8) VALUE 0.

       01  FX-P-SINE-TABLE.
           05  FX-P-SINE-ENTRY OCCURS 16384 TIMES
                           INDEXED BY FX-P-SINE-IDX.
               10  FX-P-OMEGA-RAW PIC S9(5)V9(8) VALUE 0.
               10  FX-P-SINE-RAW  PIC S9(1)V9(8) VALUE 0.

       01  CHORUS-SINC-CONSTANTS USAGE COMP-3.
           05  PI-VAL             PIC 9V9(10) VALUE 3.1415926535.
           05  KERNEL-RADIUS      PIC 9(2)    VALUE 4.
           05  FIXED-POINT-SCALER PIC 9(10)   VALUE 1000000000.

       01  POLYPHASE-CONSTANTS USAGE COMP-3.
           05  PHASE-RESOLUTION   PIC 9(4)    VALUE 9000.
           05  PHASE-SCALER       PIC 9(4)    VALUE 9000.

       01  SINC-VARS           USAGE COMP-3.
           05  PHASE-INDEX     PIC 9(5).
           05  KERNEL-INDEX    PIC S9(3).
           05  READ-IDX        PIC 9(7).
           05  TEMP-INDEX-CALC PIC 9(3).
           05  RAW-TABLE-VAL   PIC S9(13).
           05  FINAL-WEIGHT    PIC S9(5)V9(10).
           05  SUM-ACCUM       PIC S9(9)V9(10).
           05  SINC-X          PIC S9(5)V9(10).
           05  MATCH-FRACTION  PIC 9V9(10).
           05  SINC-WEIGHT     PIC S9(5)V9(10).
           05  WINDOW-WEIGHT   PIC S9(5)V9(10).
           05  TEMP-ANGLE      PIC S9(3)V9(18) COMP-3.
           05  LOOKUP-INDEX    PIC S9(8) COMP-5.
           05  DRY-SIG         PIC S9(7)V9(4) COMP-5.
           05  TRIG-RESULT     PIC S9(5)V9(10).

       01  FILTER-MATH-VARS        USAGE COMP-3.
           05  FINAL-SINE-VALUE    PIC S9(1)V9(8).
           05  FINAL-COS-VALUE     PIC S9(1)V9(8).
           05  PI-2                PIC 9(1)V9(8) VALUE 6.28318531.
           05  ANGULAR-FREQUENCY   PIC S9(5)V9(8).

       01  LINEAR-INTERP-VARS.
           05  LI-IDX-A        PIC 9(5).
           05  LI-IDX-B        PIC 9(5).
           05  LI-FRAC         PIC 9V9(8).
           05  LI-INV-FRAC     PIC 9V9(8).
           05  LI-SAMP-A       PIC S9(7)V9(4).
           05  LI-SAMP-B       PIC S9(7)V9(4).

       01  BUFFERS.
           05  CHORUS-BUF-L     OCCURS 4096  TIMES
                                PIC S9(7)V9(4) COMP-5.
           05  DELAY-BUF-L      OCCURS 44100 TIMES
                                PIC S9(7)V9(4) COMP-5.
           05  R-C1-BUF-L       OCCURS 1600  TIMES
                                PIC S9(7)V9(4) COMP-5.
           05  R-C2-BUF-L       OCCURS 1800  TIMES
                                PIC S9(7)V9(4) COMP-5.
           05  R-C3-BUF-L       OCCURS 2000  TIMES
                                PIC S9(7)V9(4) COMP-5.
           05  R-C4-BUF-L       OCCURS 2250  TIMES
                                PIC S9(7)V9(4) COMP-5.
           05  CHORUS-BUF-R     OCCURS 4098  TIMES
                                PIC S9(7)V9(4) COMP-5.
           05  DELAY-BUF-R      OCCURS 44100 TIMES
                                PIC S9(7)V9(4) COMP-5.
           05  R-C1-BUF-R       OCCURS 1600  TIMES
                                PIC S9(7)V9(4) COMP-5.
           05  R-C2-BUF-R       OCCURS 1800  TIMES
                                PIC S9(7)V9(4) COMP-5.
           05  R-C3-BUF-R       OCCURS 2000  TIMES
                                PIC S9(7)V9(4) COMP-5.
           05  R-C4-BUF-R       OCCURS 2250  TIMES
                                PIC S9(7)V9(4) COMP-5.
           05  R-A1-BUF         OCCURS  350  TIMES
                                PIC S9(7)V9(4) COMP-5.
           05  R-A2-BUF         OCCURS  120  TIMES
                                PIC S9(7)V9(4) COMP-5.

       01  POINTERS            USAGE COMP-5.
           05  C-WRITE-PTR     PIC 9(5)     VALUE 0.
           05  C-READ-PTR      PIC 9(5)V9(10).
           05  D-WRITE-PTR     PIC 9(5)     VALUE 0.
           05  D-READ-PTR      PIC 9(5).
           05  DELAY-SAMPLES   PIC 9(5).
           05  R-PTR-1         PIC 9(5)     VALUE 1.
           05  R-PTR-2         PIC 9(5)     VALUE 1.
           05  R-PTR-3         PIC 9(5)     VALUE 1.
           05  R-PTR-4         PIC 9(5)     VALUE 1.

       01  WS-INDICES          USAGE COMP-5.
           05  TEMP-IDX        PIC 9(5).
           05  LFO-SINE-IDX    PIC 9(5).
           05  SAMPLE-COUNT    PIC 9(9)     VALUE 0.

       01  FX-PARAMS.
      * 1. SPECTRUM: Aggressive mid-high boost to test filter stability
           05  FX-SPEC-FREQ    PIC S9(1)V9(5) VALUE  0.45000.
           05  FX-SPEC-Q       PIC  9V9(5)    VALUE  0.85000.
           05  FX-SPEC-GAIN    PIC S9(1)V9(2) VALUE  0.50.

      * 2. SATURATOR: Heavy "Tube" drive to test the Clamping logic
           05  FX-DRIVE-AMOUNT PIC  9V9(2)    VALUE  0.75.

      * 3. PHASER: Deep, resonant sweep to test Analog LPF & AP-Coeff
           05  FX-PHASER-RATE  PIC  9V9(3)    VALUE  0.400.
           05  FX-PHASER-DEPTH PIC  9V9(2)    VALUE  0.90.
           05  FX-PHASER-MANUAL
                               PIC  9V9(2)    VALUE  0.50.
           05  FX-PHASER-FBACK PIC  9V9(2)    VALUE  0.75.
           05  FX-PHASER-MIX   PIC  9V9(2)    VALUE  0.50.

      * 4. CHORUS: Wide Stereo Shimmer to test Sinc & Phase Inversion
      * Faster rate and deeper depth to verify high-res interpolation
           05  FX-CHORUS-RATE  PIC  9V9(5)    VALUE  0.85000.
           05  FX-CHORUS-DEPTH PIC 9(4)V9(10) VALUE  65.00000.
           05  FX-CHORUS-MIX   PIC  9V9(2)    VALUE  0.60.

      * 5. DELAY: Rhythmic repeats to test Buffer Wrap-around
           05  FX-DELAY-MS     PIC  9(4)V9(5) VALUE  0450.00000.
           05  FX-DELAY-FBACK  PIC  9V9(2)    VALUE  0.65.
           05  FX-DELAY-MIX    PIC  9V9(2)    VALUE  0.45.

      * 6. REVERB: Long "Cathedral" wash to test Stereo Tank logic
           05  FX-REV-DECAY    PIC  9V9(3)    VALUE  0.960.
           05  FX-REV-MIX      PIC  9V9(2)    VALUE  0.55.

       01  CONSTANTS.
           05  SAMPLE-RATE     PIC 9(5)       VALUE 44100.
           05  TWO-PI          PIC 9V9(8)     VALUE 6.28318531.
           05  DELAY-SIZE      PIC 9(5)       VALUE 44100.

       01  PROCESSING-VARS.
           05  CURRENT-SAMPLE  PIC S9(7)V9(4) COMP-5.
           05  TEMP-SAMPLE     PIC S9(7)V9(4) COMP-5.
           05  FINAL-INT       PIC S9(9)      COMP-5.
           05  EOF-FLAG        PIC X(1)       VALUE "N".
           05  INTERP-FRAC     PIC 9V9(10)    COMP-5.
           05  LFO-WORK-PHASE  PIC 9(3)V9(10).

           05  SAMPLE-L        PIC S9(7)V9(4) COMP-5.
           05  SAMPLE-R        PIC S9(7)V9(4) COMP-5.
           05  WET-L           PIC S9(7)V9(4) COMP-5.
           05  WET-R           PIC S9(7)V9(4) COMP-5.
           05  DRY-L           PIC S9(7)V9(4) COMP-5.
           05  DRY-R           PIC S9(7)V9(4) COMP-5.

           05  SP-LOW          PIC S9(7)V9(4) COMP-5 VALUE 0.
           05  SP-BAND         PIC S9(7)V9(4) COMP-5 VALUE 0.
           05  SP-HIGH         PIC S9(7)V9(4) COMP-5 VALUE 0.
           05  P-LFO-PHASE     PIC 9(3)V9(10) VALUE 0.
           05  P-LFO-INC       PIC 9(1)V9(8).
           05  P-LFO-VAL       PIC S9(1)V9(4).
           05  P-SINE-IDX      PIC 9(5).
           05  AP-COEFF        PIC S9(1)V9(4).
           05  P-FB-SIG        PIC S9(7)V9(4) COMP-5 VALUE 0.
           05  P-IN            PIC S9(7)V9(4) COMP-5.
           05  P-OUT           PIC S9(7)V9(4) COMP-5.
           05  ST1-X-OLD       PIC S9(7)V9(4) COMP-5 VALUE 0.
           05  ST1-Y-OLD       PIC S9(7)V9(4) COMP-5 VALUE 0.
           05  ST2-X-OLD       PIC S9(7)V9(4) COMP-5 VALUE 0.
           05  ST2-Y-OLD       PIC S9(7)V9(4) COMP-5 VALUE 0.
           05  ST3-X-OLD       PIC S9(7)V9(4) COMP-5 VALUE 0.
           05  ST3-Y-OLD       PIC S9(7)V9(4) COMP-5 VALUE 0.
           05  ST4-X-OLD       PIC S9(7)V9(4) COMP-5 VALUE 0.
           05  ST4-Y-OLD       PIC S9(7)V9(4) COMP-5 VALUE 0.
           05  CHORUS-LPF-MEM  PIC S9(7)V9(4) COMP-5 VALUE 0.
           05  CHORUS-LPF-MEM-R PIC S9(7)V9(4) COMP-5 VALUE 0.
           05  WET-L2          PIC S9(7)V9(4) COMP-5 VALUE 0.
           05  WET-R2          PIC S9(7)V9(4) COMP-5 VALUE 0.
           05  PHASER-LPF-MEM  PIC S9(7)V9(4) COMP-5 VALUE 0.
           05  LFO-CALC-IDX    PIC 9(5).
           05  LFO-NEXT-IDX    PIC 9(5).
           05  LFO-FRAC        PIC 9V9(8).
           05  LFO-TEMP-BIG    PIC 9(6)V9(8).
           05  LFO-VAL-A       PIC S9(1)V9(8).
           05  LFO-VAL-B       PIC S9(1)V9(8).
           05  LFO-SMOOTH      PIC S9(1)V9(8).
           05  LFO-PHASE       PIC 9(3)V9(10) VALUE 0.
           05  LFO-INC         PIC 9(1)V9(8).
           05  LFO-VAL         PIC S9(1)V9(4).
           05  MOD-OFFSET      PIC S9(5)V9(4) COMP-5.

           05  DC-IN-L-OLD     PIC S9(7)V9(4) COMP-5 VALUE 0.
           05  DC-OUT-L-OLD    PIC S9(7)V9(4) COMP-5 VALUE 0.
           05  DC-IN-R-OLD     PIC S9(7)V9(4) COMP-5 VALUE 0.
           05  DC-OUT-R-OLD    PIC S9(7)V9(4) COMP-5 VALUE 0.

       PROCEDURE DIVISION.
       0000-MAIN-CONTROL.
      *=============================================================*
      * TOP-LEVEL CONTROL PARAGRAPH
      * Orchestrates the entire program lifecycle.
      *=============================================================*
           DISPLAY "--- COMOL-FX: STEREO NUCLEAR EDITION V3 ---".
           PERFORM 1000-INITIALIZATION.
           PERFORM 2000-PROCESS-STREAM
               UNTIL EOF-FLAG = "Y".
           PERFORM 9000-CLEANUP.
           STOP RUN.

       1000-INITIALIZATION.
      *=============================================================*
      * HIGH-LEVEL INITIALIZATION CONTROLLER
      * Calls focused sub-tasks for clarity and maintainability.
      *=============================================================*
           PERFORM 1100-COMPUTE-RUNTIME.
           PERFORM 8150-GENERATE-SINE-TABLES.
           DISPLAY "Initializing High-Res Sinc Table...".
           PERFORM 8500-INIT-SINC-TABLE.
           DISPLAY "Zeroing all buffers...".
           PERFORM 8000-ZERO-ALL-BUFFERS.
           DISPLAY "Initialization Complete.".

       1100-COMPUTE-RUNTIME.
      * Computes all dynamic FX parameters and opens I/O files
           COMPUTE LFO-INC =
               (FX-CHORUS-RATE / SAMPLE-RATE) * TWO-PI.
           COMPUTE P-LFO-INC =
               (FX-PHASER-RATE / SAMPLE-RATE) * TWO-PI.
           COMPUTE DELAY-SAMPLES =
               (FX-DELAY-MS / 1000) * SAMPLE-RATE.
           IF DELAY-SAMPLES < 1
               MOVE 1 TO DELAY-SAMPLES.
           OPEN INPUT IN-FILE.
           IF WS-IN-STATUS NOT = "00"
               DISPLAY "ERROR: Cannot open input file." STOP RUN.
           OPEN OUTPUT OUT-FILE-L.
           OPEN OUTPUT OUT-FILE-R.

       2000-PROCESS-STREAM.
           READ IN-FILE INTO WS-IN-BYTES
               AT END
                   MOVE "Y" TO EOF-FLAG
               NOT AT END
                   PERFORM 2100-PROCESS-ONE-SAMPLE
           END-READ.

       2100-PROCESS-ONE-SAMPLE.
      *=============================================================*
      * MAIN SAMPLE PROCESSING CONTROLLER
      * Central hub that sequences every effect for one sample pair.
      * All long effect blocks have been delegated to sub-paragraphs.
      *=============================================================*
           MOVE WS-IN-INT TO CURRENT-SAMPLE.

      * Pre-processing stage
           PERFORM 2200-APPLY-PRE-EFFECTS.

      * Prepare identical dry signal for stereo paths
           MOVE CURRENT-SAMPLE TO SAMPLE-L.
           MOVE CURRENT-SAMPLE TO SAMPLE-R.

      * Modulation & time-based effects
           PERFORM 4000-EFFECT-CHORUS-STEREO.
           PERFORM 5000-EFFECT-DELAY-STEREO.
           PERFORM 6000-EFFECT-REVERB-STEREO.

      * Final post-processing
           PERFORM 7000-DC-BLOCKER-STEREO.

      * Output stage
           PERFORM 2700-WRITE-OUTPUT-SAMPLES.

           ADD 1 TO SAMPLE-COUNT.

       2200-APPLY-PRE-EFFECTS.
      * Applies spectrum shaping, saturation, and phaser in order
           PERFORM 2900-EFFECT-SPECTRUM.
           PERFORM 3000-EFFECT-SATURATOR.
           PERFORM 3500-EFFECT-PHASER.

       2700-WRITE-OUTPUT-SAMPLES.
      * Clamps final values and writes separate left/right raw files
           MOVE SAMPLE-L TO FINAL-INT.
           PERFORM UTILITY-CLAMP-FINAL.
           MOVE FINAL-INT TO WS-OUT-INT.
           WRITE OUT-RECORD-L FROM WS-OUT-BYTES.

           MOVE SAMPLE-R TO FINAL-INT.
           PERFORM UTILITY-CLAMP-FINAL.
           MOVE FINAL-INT TO WS-OUT-INT.
           WRITE OUT-RECORD-R FROM WS-OUT-BYTES.

      *=============================================================*
      * PRE-EFFECTS SECTION
      * Short original paragraphs left unchanged (no split needed)
      *=============================================================*

       2900-EFFECT-SPECTRUM.
           COMPUTE SP-LOW =
               (SP-LOW + (FX-SPEC-FREQ * SP-BAND)) * 0.999.
           COMPUTE SP-HIGH =
               CURRENT-SAMPLE - SP-LOW - (FX-SPEC-Q * SP-BAND).
           COMPUTE SP-BAND =
               (SP-BAND + (FX-SPEC-FREQ * SP-HIGH)) * 0.999.
           COMPUTE CURRENT-SAMPLE =
               CURRENT-SAMPLE + (SP-BAND * FX-SPEC-GAIN).

       3000-EFFECT-SATURATOR.
           IF FX-DRIVE-AMOUNT > 0
               COMPUTE TEMP-SAMPLE =
                   CURRENT-SAMPLE * (1.0 + (FX-DRIVE-AMOUNT / 2))
               PERFORM UTILITY-CLAMP-SAMPLE
               MOVE TEMP-SAMPLE TO CURRENT-SAMPLE
           END-IF.

       3500-EFFECT-PHASER.
           MOVE CURRENT-SAMPLE TO DRY-SIG.
           ADD P-LFO-INC TO P-LFO-PHASE.
           IF P-LFO-PHASE > TWO-PI
               SUBTRACT TWO-PI FROM P-LFO-PHASE.
           MOVE P-LFO-PHASE TO LFO-WORK-PHASE.
           PERFORM UTILITY-GET-SMOOTH-SINE.
           MOVE LFO-SMOOTH TO P-LFO-VAL.
           COMPUTE AP-COEFF =
               FX-PHASER-MANUAL + (P-LFO-VAL * FX-PHASER-DEPTH * 0.5).
           IF AP-COEFF >  0.95 MOVE  0.95 TO AP-COEFF.
           IF AP-COEFF < -0.95 MOVE -0.95 TO AP-COEFF.
           COMPUTE P-FB-SIG =
               (P-FB-SIG * 0.7) + (PHASER-LPF-MEM * 0.3).
           MOVE P-FB-SIG TO PHASER-LPF-MEM.
           COMPUTE P-IN =
               CURRENT-SAMPLE + (P-FB-SIG * FX-PHASER-FBACK).
           COMPUTE P-OUT =
               (AP-COEFF * (ST1-Y-OLD - P-IN)) + ST1-X-OLD.
           MOVE P-IN TO ST1-X-OLD.
           MOVE P-OUT TO ST1-Y-OLD.
           MOVE P-OUT TO P-IN.
           COMPUTE P-OUT =
               (AP-COEFF * (ST2-Y-OLD - P-IN)) + ST2-X-OLD.
           MOVE P-IN TO ST2-X-OLD.
           MOVE P-OUT TO ST2-Y-OLD.
           MOVE P-OUT TO P-IN.
           COMPUTE P-OUT =
               (AP-COEFF * (ST3-Y-OLD - P-IN)) + ST3-X-OLD.
           MOVE P-IN TO ST3-X-OLD.
           MOVE P-OUT TO ST3-Y-OLD.
           MOVE P-OUT TO P-IN.
           COMPUTE P-OUT =
               (AP-COEFF * (ST4-Y-OLD - P-IN)) + ST4-X-OLD.
           MOVE P-IN TO ST4-X-OLD.
           MOVE P-OUT TO ST4-Y-OLD.
           MOVE P-OUT TO P-FB-SIG.
           COMPUTE CURRENT-SAMPLE =
               (DRY-SIG * (1 - FX-PHASER-MIX)) +
               (P-OUT * FX-PHASER-MIX).

      *=============================================================*
      * CHORUS SECTION - FULLY DECOMPOSED INTO SELF-DOCUMENTING
      * NUMBERED PARAGRAPHS (original massive block split)
      *=============================================================*

       4000-EFFECT-CHORUS-STEREO.
      *=============================================================*
      * CHORUS CONTROLLER
      * Orchestrates 4-voice modulated delay + LPF + mix.
      * No logic changed - only broken for readability.
      *=============================================================*
           PERFORM 4010-CHORUS-WRITE-BUFFERS.
           PERFORM 4020-CHORUS-UPDATE-LFO-PHASE.
           MOVE SAMPLE-L TO DRY-L.
           MOVE SAMPLE-R TO DRY-R.

      * Left channel voices
           PERFORM 4100-CHORUS-LEFT-VOICE-1.
           PERFORM 4110-CHORUS-LEFT-VOICE-2.
           PERFORM 4120-CHORUS-LEFT-APPLY-LPF.

      * Right channel voices
           PERFORM 4150-CHORUS-RIGHT-VOICE-1.
           PERFORM 4160-CHORUS-RIGHT-VOICE-2.
           PERFORM 4170-CHORUS-RIGHT-APPLY-LPF.

      * Final mix
           PERFORM 4300-CHORUS-MIX-OUTPUT.
           MOVE TEMP-IDX TO C-WRITE-PTR.

       4010-CHORUS-WRITE-BUFFERS.
      * Write current sample into circular chorus delay buffer
           COMPUTE TEMP-IDX = C-WRITE-PTR + 1.
           IF TEMP-IDX > 4096 MOVE 1 TO TEMP-IDX.
           MOVE SAMPLE-L TO CHORUS-BUF-L(TEMP-IDX).
           MOVE SAMPLE-R TO CHORUS-BUF-R(TEMP-IDX).

       4020-CHORUS-UPDATE-LFO-PHASE.
      * Advance main chorus LFO (shared across all voices)
           ADD LFO-INC TO LFO-PHASE.
           IF LFO-PHASE > TWO-PI
               SUBTRACT TWO-PI FROM LFO-PHASE.

       4100-CHORUS-LEFT-VOICE-1.
      * LEFT - VOICE 1 (0 deg, base 1323 = 30ms)
           MOVE LFO-PHASE TO LFO-WORK-PHASE.
           PERFORM UTILITY-GET-SMOOTH-SINE.
           COMPUTE MOD-OFFSET =
               1323 + (LFO-SMOOTH * FX-CHORUS-DEPTH).
           PERFORM 4900-COMPUTE-CHORUS-READ-PTR.
           PERFORM 4950-LINEAR-INTERPOLATE-LEFT.

       4110-CHORUS-LEFT-VOICE-2.
      * LEFT - VOICE 2 (90 deg offset, base 1383)
           COMPUTE LFO-WORK-PHASE = LFO-PHASE + 1.57079632.
           IF LFO-WORK-PHASE > TWO-PI
               SUBTRACT TWO-PI FROM LFO-WORK-PHASE.
           PERFORM UTILITY-GET-SMOOTH-SINE.
           COMPUTE MOD-OFFSET =
               1383 + (LFO-SMOOTH * FX-CHORUS-DEPTH).
           PERFORM 4900-COMPUTE-CHORUS-READ-PTR.
           PERFORM 4950-LINEAR-INTERPOLATE-LEFT.
           COMPUTE WET-L = (WET-L + WET-L2) * 0.5.

       4120-CHORUS-LEFT-APPLY-LPF.
      * LPF: light anti-artifact smoothing (~13kHz cutoff)
           COMPUTE CHORUS-LPF-MEM =
               (CHORUS-LPF-MEM * 0.15) + (WET-L * 0.85).
           MOVE CHORUS-LPF-MEM TO WET-L.

       4150-CHORUS-RIGHT-VOICE-1.
      * RIGHT - VOICE 1 (90 deg, base 1323)
           COMPUTE LFO-WORK-PHASE = LFO-PHASE + 1.57079632.
           IF LFO-WORK-PHASE > TWO-PI
               SUBTRACT TWO-PI FROM LFO-WORK-PHASE.
           PERFORM UTILITY-GET-SMOOTH-SINE.
           COMPUTE MOD-OFFSET =
               1323 + (LFO-SMOOTH * FX-CHORUS-DEPTH).
           PERFORM 4900-COMPUTE-CHORUS-READ-PTR.
           PERFORM 4960-LINEAR-INTERPOLATE-RIGHT.

       4160-CHORUS-RIGHT-VOICE-2.
      * RIGHT - VOICE 2 (180 deg offset, base 1383)
           COMPUTE LFO-WORK-PHASE = LFO-PHASE + 3.14159265.
           IF LFO-WORK-PHASE > TWO-PI
               SUBTRACT TWO-PI FROM LFO-WORK-PHASE.
           PERFORM UTILITY-GET-SMOOTH-SINE.
           COMPUTE MOD-OFFSET =
               1383 + (LFO-SMOOTH * FX-CHORUS-DEPTH).
           PERFORM 4900-COMPUTE-CHORUS-READ-PTR.
           PERFORM 4960-LINEAR-INTERPOLATE-RIGHT.
           COMPUTE WET-R = (WET-R + WET-R2) * 0.5.

       4170-CHORUS-RIGHT-APPLY-LPF.
      * LPF: light anti-artifact smoothing (~13kHz cutoff)
           COMPUTE CHORUS-LPF-MEM-R =
               (CHORUS-LPF-MEM-R * 0.15) + (WET-R * 0.85).
           MOVE CHORUS-LPF-MEM-R TO WET-R.

       4300-CHORUS-MIX-OUTPUT.
      * MIX OUTPUT
           COMPUTE SAMPLE-L =
               (DRY-L * (1 - FX-CHORUS-MIX)) +
               (WET-L * FX-CHORUS-MIX).
           COMPUTE SAMPLE-R =
               (DRY-R * (1 - FX-CHORUS-MIX)) +
               (WET-R * FX-CHORUS-MIX).

       4900-COMPUTE-CHORUS-READ-PTR.
      * Common circular read pointer calculation (shared by all voices)
           COMPUTE C-READ-PTR = TEMP-IDX + 4096 - MOD-OFFSET.
           IF C-READ-PTR >= 4097 SUBTRACT 4096 FROM C-READ-PTR.

       4950-LINEAR-INTERPOLATE-LEFT.
      * Rewritten to bridge into the Sinc Engine
           COMPUTE READ-IDX = FUNCTION INTEGER(C-READ-PTR).
           COMPUTE INTERP-FRAC = C-READ-PTR - READ-IDX.
           PERFORM 8600-CALCULATE-SINC-L.
           MOVE SUM-ACCUM TO WET-L.

       4960-LINEAR-INTERPOLATE-RIGHT.
      * Rewritten to bridge into the Sinc Engine
           COMPUTE READ-IDX = FUNCTION INTEGER(C-READ-PTR).
           COMPUTE INTERP-FRAC = C-READ-PTR - READ-IDX.
           PERFORM 8700-CALCULATE-SINC-R.
           MOVE SUM-ACCUM TO WET-R.

       5000-EFFECT-DELAY-STEREO.
      * (kept as single paragraph - already concise and self-documenting)
           MOVE SAMPLE-L TO DRY-L.
           MOVE SAMPLE-R TO DRY-R.
           COMPUTE D-READ-PTR =
               (D-WRITE-PTR + DELAY-SIZE) - DELAY-SAMPLES.
           IF D-READ-PTR >= DELAY-SIZE
               SUBTRACT DELAY-SIZE FROM D-READ-PTR.
           MOVE DELAY-BUF-R(D-READ-PTR + 1) TO WET-L.
           MOVE DELAY-BUF-L(D-READ-PTR + 1) TO WET-R.
           COMPUTE TEMP-SAMPLE =
               SAMPLE-L + (WET-R * FX-DELAY-FBACK).
           PERFORM UTILITY-CLAMP-SAMPLE.
           MOVE TEMP-SAMPLE TO DELAY-BUF-L(D-WRITE-PTR + 1).
           COMPUTE TEMP-SAMPLE =
               SAMPLE-R + (WET-L * FX-DELAY-FBACK).
           PERFORM UTILITY-CLAMP-SAMPLE.
           MOVE TEMP-SAMPLE TO DELAY-BUF-R(D-WRITE-PTR + 1).
           COMPUTE SAMPLE-L =
               (DRY-L * (1 - FX-DELAY-MIX)) +
               (WET-L * FX-DELAY-MIX).
           COMPUTE SAMPLE-R =
               (DRY-R * (1 - FX-DELAY-MIX)) +
               (WET-R * FX-DELAY-MIX).
           ADD 1 TO D-WRITE-PTR.
           IF D-WRITE-PTR >= DELAY-SIZE MOVE 0 TO D-WRITE-PTR.

       6000-EFFECT-REVERB-STEREO.
      *=============================================================*
      * REVERB CONTROLLER
      * Orchestrates four parallel delay tanks with cross-feed.
      * Broken into one paragraph per tank + final mix.
      *=============================================================*
           MOVE SAMPLE-L TO DRY-L.
           MOVE SAMPLE-R TO DRY-R.
           MOVE 0 TO WET-L.
           MOVE 0 TO WET-R.

           PERFORM 6010-REVERB-TANK-1.
           PERFORM 6020-REVERB-TANK-2.
           PERFORM 6030-REVERB-TANK-3.
           PERFORM 6040-REVERB-TANK-4.

           PERFORM 6050-REVERB-MIX-OUTPUT.

       6010-REVERB-TANK-1.
           COMPUTE TEMP-SAMPLE =
               (SAMPLE-L * 0.31) + (R-C1-BUF-L(R-PTR-1) * FX-REV-DECAY)
               + (SAMPLE-R * 0.09).
           MOVE TEMP-SAMPLE TO R-C1-BUF-L(R-PTR-1).
           ADD TEMP-SAMPLE TO WET-L.
           COMPUTE TEMP-SAMPLE =
               (SAMPLE-R * 0.31) + (R-C1-BUF-R(R-PTR-1) * FX-REV-DECAY)
               + (SAMPLE-L * 0.09).
           MOVE TEMP-SAMPLE TO R-C1-BUF-R(R-PTR-1).
           ADD TEMP-SAMPLE TO WET-R.
           ADD 1 TO R-PTR-1. IF R-PTR-1 > 1600 MOVE 1 TO R-PTR-1.

       6020-REVERB-TANK-2.
           COMPUTE TEMP-SAMPLE =
               (SAMPLE-L * 0.31) + (R-C2-BUF-L(R-PTR-2) * FX-REV-DECAY)
               + (SAMPLE-R * 0.09).
           MOVE TEMP-SAMPLE TO R-C2-BUF-L(R-PTR-2).
           ADD TEMP-SAMPLE TO WET-L.
           COMPUTE TEMP-SAMPLE =
               (SAMPLE-R * 0.31) + (R-C2-BUF-R(R-PTR-2) * FX-REV-DECAY)
               + (SAMPLE-L * 0.09).
           MOVE TEMP-SAMPLE TO R-C2-BUF-R(R-PTR-2).
           ADD TEMP-SAMPLE TO WET-R.
           ADD 1 TO R-PTR-2. IF R-PTR-2 > 1800 MOVE 1 TO R-PTR-2.

       6030-REVERB-TANK-3.
           COMPUTE TEMP-SAMPLE =
               (SAMPLE-L * 0.31) + (R-C3-BUF-L(R-PTR-3) * FX-REV-DECAY)
               + (SAMPLE-R * 0.09).
           MOVE TEMP-SAMPLE TO R-C3-BUF-L(R-PTR-3).
           ADD TEMP-SAMPLE TO WET-L.
           COMPUTE TEMP-SAMPLE =
               (SAMPLE-R * 0.31) + (R-C3-BUF-R(R-PTR-3) * FX-REV-DECAY)
               + (SAMPLE-L * 0.09).
           MOVE TEMP-SAMPLE TO R-C3-BUF-R(R-PTR-3).
           ADD TEMP-SAMPLE TO WET-R.
           ADD 1 TO R-PTR-3. IF R-PTR-3 > 2000 MOVE 1 TO R-PTR-3.

       6040-REVERB-TANK-4.
           COMPUTE TEMP-SAMPLE =
               (SAMPLE-L * 0.31) + (R-C4-BUF-L(R-PTR-4) * FX-REV-DECAY)
               + (SAMPLE-R * 0.09).
           MOVE TEMP-SAMPLE TO R-C4-BUF-L(R-PTR-4).
           ADD TEMP-SAMPLE TO WET-L.
           COMPUTE TEMP-SAMPLE =
               (SAMPLE-R * 0.31) + (R-C4-BUF-R(R-PTR-4) * FX-REV-DECAY)
               + (SAMPLE-L * 0.09).
           MOVE TEMP-SAMPLE TO R-C4-BUF-R(R-PTR-4).
           ADD TEMP-SAMPLE TO WET-R.
           ADD 1 TO R-PTR-4. IF R-PTR-4 > 2250 MOVE 1 TO R-PTR-4.

       6050-REVERB-MIX-OUTPUT.
      * Much stronger wet output
           COMPUTE SAMPLE-L =
               (DRY-L * (1 - FX-REV-MIX)) +
               (WET-L * 0.55 * FX-REV-MIX).
           COMPUTE SAMPLE-R =
               (DRY-R * (1 - FX-REV-MIX)) +
               (WET-R * 0.55 * FX-REV-MIX).

       7000-DC-BLOCKER-STEREO.
      * (kept as-is - already short and clear)

      * Left Channel
           COMPUTE TEMP-SAMPLE =
               SAMPLE-L - DC-IN-L-OLD + (0.995 * DC-OUT-L-OLD).
           MOVE SAMPLE-L TO DC-IN-L-OLD.
           MOVE TEMP-SAMPLE TO DC-OUT-L-OLD.
           MOVE TEMP-SAMPLE TO SAMPLE-L.

      * Right Channel
           COMPUTE TEMP-SAMPLE =
               SAMPLE-R - DC-IN-R-OLD + (0.995 * DC-OUT-R-OLD).
           MOVE SAMPLE-R TO DC-IN-R-OLD.
           MOVE TEMP-SAMPLE TO DC-OUT-R-OLD.
           MOVE TEMP-SAMPLE TO SAMPLE-R.

       8000-ZERO-ALL-BUFFERS.
      * (unchanged - already well-structured with VARYING loops)
           PERFORM VARYING TEMP-IDX FROM 1 BY 1 UNTIL TEMP-IDX > 4096
               MOVE 0 TO CHORUS-BUF-L(TEMP-IDX)
               MOVE 0 TO CHORUS-BUF-R(TEMP-IDX)
           END-PERFORM.
           PERFORM VARYING TEMP-IDX FROM 1 BY 1 UNTIL TEMP-IDX > 44100
               MOVE 0 TO DELAY-BUF-L(TEMP-IDX)
               MOVE 0 TO DELAY-BUF-R(TEMP-IDX)
           END-PERFORM.
           PERFORM VARYING TEMP-IDX FROM 1 BY 1 UNTIL TEMP-IDX > 1600
               MOVE 0 TO R-C1-BUF-L(TEMP-IDX)
               MOVE 0 TO R-C1-BUF-R(TEMP-IDX)
           END-PERFORM.
           PERFORM VARYING TEMP-IDX FROM 1 BY 1 UNTIL TEMP-IDX > 1800
               MOVE 0 TO R-C2-BUF-L(TEMP-IDX)
               MOVE 0 TO R-C2-BUF-R(TEMP-IDX)
           END-PERFORM.
           PERFORM VARYING TEMP-IDX FROM 1 BY 1 UNTIL TEMP-IDX > 2000
               MOVE 0 TO R-C3-BUF-L(TEMP-IDX)
               MOVE 0 TO R-C3-BUF-R(TEMP-IDX)
           END-PERFORM.
           PERFORM VARYING TEMP-IDX FROM 1 BY 1 UNTIL TEMP-IDX > 2250
               MOVE 0 TO R-C4-BUF-L(TEMP-IDX)
               MOVE 0 TO R-C4-BUF-R(TEMP-IDX)
           END-PERFORM.
           PERFORM VARYING TEMP-IDX FROM 1 BY 1 UNTIL TEMP-IDX > 350
               MOVE 0 TO R-A1-BUF(TEMP-IDX)
           END-PERFORM.
           PERFORM VARYING TEMP-IDX FROM 1 BY 1 UNTIL TEMP-IDX > 120
               MOVE 0 TO R-A2-BUF(TEMP-IDX)
           END-PERFORM.

       8150-GENERATE-SINE-TABLES.
      * (unchanged - already clean)
           DISPLAY "Generating high-res sine lookup tables...".
           COMPUTE PI-2 = 6.283185307179586.
           PERFORM VARYING FX-SINE-IDX FROM 1 BY 1
               UNTIL FX-SINE-IDX > 16384
               COMPUTE FX-OMEGA-RAW(FX-SINE-IDX) =
                   (FX-SINE-IDX - 1) * PI-2 / 16384
               COMPUTE FX-SINE-RAW(FX-SINE-IDX) =
                   FUNCTION SIN(FX-OMEGA-RAW(FX-SINE-IDX))
           END-PERFORM.
      * PH table is identical
           PERFORM VARYING FX-P-SINE-IDX FROM 1 BY 1
               UNTIL FX-P-SINE-IDX > 16384
           MOVE FX-OMEGA-RAW(FX-P-SINE-IDX) TO
               FX-P-OMEGA-RAW(FX-P-SINE-IDX)
           MOVE FX-SINE-RAW(FX-P-SINE-IDX)
               TO FX-P-SINE-RAW(FX-P-SINE-IDX)
           END-PERFORM.
           DISPLAY "Sine tables ready.".

       8500-INIT-SINC-TABLE.
      * (unchanged - generation loop is already well-commented)
           DISPLAY "Generating Sinc Table (Fast Lookup Method)...".

      * Outer Loop: Iterate through all 9000 sub-sample phases
           PERFORM VARYING CHORUS-PHASE-IDX FROM 1 BY 1
               UNTIL CHORUS-PHASE-IDX > PHASE-RESOLUTION

      * Calculate the fractional offset (0.0 to 1.0)
               COMPUTE MATCH-FRACTION =
                   (CHORUS-PHASE-IDX - 1) / PHASE-RESOLUTION

      * Inner Loop: Calculate the 9 taps (kernel)
               PERFORM VARYING KERNEL-INDEX FROM -4 BY 1
                   UNTIL KERNEL-INDEX > 4

                   COMPUTE SINC-X = KERNEL-INDEX - MATCH-FRACTION

      * Handle the center point (0/0 singularity)
                   IF SINC-X = 0
                       COMPUTE FINAL-WEIGHT = 1.0
                   ELSE
      * ---------------------------------------------
      * FAST SINE LOOKUP (No Intrinsic Functions)
      * Formula: sin(pi * x)
      * Map (pi*x) to our 16384-point sine table
      * 16384 points = 2*PI. Therefore PI = 8192 steps.
      * ---------------------------------------------
                       COMPUTE LOOKUP-INDEX = SINC-X * 8192

      * Handle Wrap-Around (Modulo 16384)
                       COMPUTE LOOKUP-INDEX =
                           FUNCTION MOD(LOOKUP-INDEX, 16384)

      * Handle Negative Indices
                       IF LOOKUP-INDEX < 0
                           ADD 16384 TO LOOKUP-INDEX
                       END-IF

      * Fetch Sine from existing FX table
                       ADD 1 TO LOOKUP-INDEX
                      MOVE FX-SINE-RAW(LOOKUP-INDEX) TO FINAL-SINE-VALUE

      * Calculate Sinc: sin(pi*x) / (pi*x)
                       COMPUTE SINC-WEIGHT =
                           FINAL-SINE-VALUE / (PI-VAL * SINC-X)

      * ---------------------------------------------
      * FAST WINDOW LOOKUP (Cosine Window)
      * 0.5 + 0.5 * cos(pi * x / radius)
      * Using Sine table shifted by 90 deg (4096 steps)
      * ---------------------------------------------
                       COMPUTE LOOKUP-INDEX =
                           (SINC-X * 8192) / KERNEL-RADIUS

      * Shift for Cosine (add quarter rotation)
                       ADD 4096 TO LOOKUP-INDEX

      * Wrap and fetch
                       COMPUTE LOOKUP-INDEX =
                           FUNCTION MOD(LOOKUP-INDEX, 16384)
                       IF LOOKUP-INDEX < 0
                           ADD 16384 TO LOOKUP-INDEX
                       END-IF
                       ADD 1 TO LOOKUP-INDEX

                       MOVE FX-SINE-RAW(LOOKUP-INDEX) TO FINAL-COS-VALUE

                       COMPUTE WINDOW-WEIGHT =
                           0.5 + (0.5 * FINAL-COS-VALUE)

      * ---------------------------------------------
      * Combine
      * ---------------------------------------------
                      COMPUTE FINAL-WEIGHT = SINC-WEIGHT * WINDOW-WEIGHT
                   END-IF

      * Store in Table
                   COMPUTE RAW-TABLE-VAL ROUNDED =
                       FINAL-WEIGHT * FIXED-POINT-SCALER

                   COMPUTE TEMP-INDEX-CALC = KERNEL-INDEX + 5
                   MOVE RAW-TABLE-VAL TO
                    CHORUS-FILTER-TAP(CHORUS-PHASE-IDX, TEMP-INDEX-CALC)

               END-PERFORM
           END-PERFORM.
           DISPLAY "Sinc Table Generated.".

       8600-CALCULATE-SINC-L.
      * (unchanged - utility, not called in main path)
           MOVE 0 TO SUM-ACCUM
           COMPUTE PHASE-INDEX =
               FUNCTION INTEGER(INTERP-FRAC * PHASE-SCALER) + 1
           IF PHASE-INDEX < 1
               MOVE 1 TO PHASE-INDEX
           END-IF
           IF PHASE-INDEX > PHASE-RESOLUTION
               MOVE PHASE-RESOLUTION TO PHASE-INDEX
           END-IF
           PERFORM VARYING KERNEL-INDEX FROM -4 BY 1
               UNTIL KERNEL-INDEX > 4
               COMPUTE LOOKUP-INDEX = READ-IDX + KERNEL-INDEX
               IF LOOKUP-INDEX < 1
                   ADD 4096 TO LOOKUP-INDEX
               END-IF
               IF LOOKUP-INDEX > 4096
                   SUBTRACT 4096 FROM LOOKUP-INDEX
               END-IF
               COMPUTE TEMP-INDEX-CALC = KERNEL-INDEX + 5
               MOVE CHORUS-FILTER-TAP (PHASE-INDEX, TEMP-INDEX-CALC)
                   TO RAW-TABLE-VAL
               COMPUTE FINAL-WEIGHT =
                   RAW-TABLE-VAL / FIXED-POINT-SCALER
               COMPUTE SUM-ACCUM = SUM-ACCUM +
                   (CHORUS-BUF-L(LOOKUP-INDEX) * FINAL-WEIGHT)
           END-PERFORM.

       8700-CALCULATE-SINC-R.
           MOVE 0 TO SUM-ACCUM
           COMPUTE PHASE-INDEX =
               FUNCTION INTEGER(INTERP-FRAC * PHASE-SCALER) + 1
           IF PHASE-INDEX < 1 MOVE 1 TO PHASE-INDEX END-IF
           IF PHASE-INDEX > PHASE-RESOLUTION
               MOVE PHASE-RESOLUTION TO PHASE-INDEX
           END-IF

           PERFORM VARYING KERNEL-INDEX FROM -4 BY 1
               UNTIL KERNEL-INDEX > 4
               COMPUTE LOOKUP-INDEX = READ-IDX + KERNEL-INDEX

               IF LOOKUP-INDEX < 1
                   ADD 4096 TO LOOKUP-INDEX
               END-IF
               IF LOOKUP-INDEX > 4096
                   SUBTRACT 4096 FROM LOOKUP-INDEX
               END-IF

               COMPUTE TEMP-INDEX-CALC = KERNEL-INDEX + 5
               MOVE CHORUS-FILTER-TAP (PHASE-INDEX, TEMP-INDEX-CALC)
                   TO RAW-TABLE-VAL
               COMPUTE FINAL-WEIGHT =
                   RAW-TABLE-VAL / FIXED-POINT-SCALER
               COMPUTE SUM-ACCUM = SUM-ACCUM +
                   (CHORUS-BUF-R(LOOKUP-INDEX) * FINAL-WEIGHT)
           END-PERFORM.

       9000-CLEANUP.
           CLOSE IN-FILE. CLOSE OUT-FILE-L. CLOSE OUT-FILE-R.
           DISPLAY "Samples processed: " SAMPLE-COUNT.

       UTILITY-CLAMP-SAMPLE.
           IF TEMP-SAMPLE >  32000 MOVE  32000 TO TEMP-SAMPLE.
           IF TEMP-SAMPLE < -32000 MOVE -32000 TO TEMP-SAMPLE.

       UTILITY-CLAMP-FINAL.
           IF FINAL-INT >  32767 MOVE  32767 TO FINAL-INT.
           IF FINAL-INT < -32768 MOVE -32768 TO FINAL-INT.

       UTILITY-GET-SMOOTH-SINE.
      * 1. Calculate Table Index (Phase 0..2PI -> Index 0..16384)
      * We use TEMP-BIG (9 digits) to prevent the "Bleep Bloop" overflow
           COMPUTE LFO-TEMP-BIG = (LFO-WORK-PHASE * 16384) / TWO-PI.

      * 2. Split into Integer Index and Decimal Fraction
           MOVE LFO-TEMP-BIG TO LFO-CALC-IDX.
           COMPUTE LFO-FRAC = LFO-TEMP-BIG - LFO-CALC-IDX.

      * 3. Safety Wraps (Handle edge cases at 0 or 2PI)
           IF LFO-CALC-IDX < 1     MOVE 1 TO LFO-CALC-IDX.
           IF LFO-CALC-IDX > 16384 MOVE 16384 TO LFO-CALC-IDX.

           COMPUTE LFO-NEXT-IDX = LFO-CALC-IDX + 1.
           IF LFO-NEXT-IDX > 16384 MOVE 1 TO LFO-NEXT-IDX.

      * 4. Linear Interpolate the Sine Value
           MOVE FX-SINE-RAW(LFO-CALC-IDX) TO LFO-VAL-A.
           MOVE FX-SINE-RAW(LFO-NEXT-IDX) TO LFO-VAL-B.
           COMPUTE LFO-SMOOTH =
               LFO-VAL-A + (LFO-FRAC * (LFO-VAL-B - LFO-VAL-A)).
