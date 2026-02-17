       IDENTIFICATION DIVISION.
       PROGRAM-ID. Comol-1.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      * /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
      * /\          FILE HANDLES          /\
      * /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
           SELECT IN-FILE ASSIGN TO
           "Your/filepath/here.raw"
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL.
           SELECT OUT-FILE ASSIGN TO
           "Your/filepath/here.raw"
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  OUT-FILE.
       01  RAW-BYTES           PIC X(1).
       FD  IN-FILE.
       01  BINARY-BYTES        PIC X(1).

       WORKING-STORAGE SECTION.
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
           05  TVF-DEPTH           PIC S9(3).
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

           PERFORM GET-ENVELOPE-SETTINGS.
      * >>>> PRE-CALCULATE TVF ENVELOPE <<<<
           PERFORM CALCULATE-CUT-BREAKPOINTS.

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
           MOVE 1 TO WAVE-SOURCE-CHOICE.
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
           MOVE 4 TO USER-OCTAVE.
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
           MOVE "3331" TO RAW-PATTERN-INPUT.

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
               MOVE 2 TO USER-DRIVE-IN
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
           MOVE 50 TO KNOB-POSITION.
      * Save the static base value
           MOVE KNOB-POSITION TO BASE-CUTOFF.

      * USER_INPUT: Q-KNOB-POSITION
           MOVE 0 TO Q-KNOB-POSITION.

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
           MOVE 0.1 TO T1.
      * USER_INPUT: L1
           MOVE 100 TO L1.
      * USER_INPUT: T2
           MOVE 0.5 TO T2.
      * USER_INPUT: L2
           MOVE 80 TO L2.
      * USER_INPUT: T3
           MOVE 1.0 TO T3.
      * USER_INPUT: L3
           MOVE 50 TO L3.
      * USER_INPUT: T-SUSTAIN
           MOVE 2.0 TO T-SUSTAIN.
      * USER_INPUT: T4
           MOVE 1.0 TO T4.

      * USER_INPUT: CUT-T1
           MOVE 0.1 TO CUT-T1.
      * USER_INPUT: CUT-L1
           MOVE 20 TO CUT-L1.
      * USER_INPUT: CUT-T2
           MOVE 0.5 TO CUT-T2.
      * USER_INPUT: CUT-L2
           MOVE 10 TO CUT-L2.
      * USER_INPUT: CUT-T3
           MOVE 1.0 TO CUT-T3.
      * USER_INPUT: CUT-L3
           MOVE 0 TO CUT-L3.
      * USER_INPUT: CUT-T-SUSTAIN
           MOVE 2.0 TO CUT-T-SUSTAIN.
      * USER_INPUT: CUT-T4
           MOVE 1.0 TO CUT-T4.
      * USER_INPUT: TVF-DEPTH
           MOVE 50 TO TVF-DEPTH.
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
           PERFORM COMPUTE-RAW-SAMPLE.

      * >>>> INSERT FILTER HERE <<<<
           PERFORM UPDATE-CUT-PARAMETER.
           PERFORM RECALCULATE-COEFFICIENTS.
           PERFORM APPLY-FILTER.
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
      * 1. Calculate Envelope Modulation
           COMPUTE DEPTH-CALC = CURRENT-KNOB * (TVF-DEPTH / 100.0).
           COMPUTE DEPTH-CALC = BASE-CUTOFF + DEPTH-CALC.

      * 2. Clamp Range
           IF DEPTH-CALC > 100 MOVE 100 TO DEPTH-CALC.
           IF DEPTH-CALC < 0   MOVE 0   TO DEPTH-CALC.

      * 3. Convert Knob (0-100) to Table Index (1-5001)
           COMPUTE LOOKUP-IDX = (DEPTH-CALC * 50) + 1.

      * 4. DIRECT LOOKUP (Using Original Variable Names)
           MOVE TBL-SINE-VAL(LOOKUP-IDX) TO FINAL-SINE-VALUE.
           MOVE TBL-COS-VAL(LOOKUP-IDX)  TO FINAL-COS-VALUE.

      * 5. Calculate Resonance
           COMPUTE ALPHA-VALUE ROUNDED =
               FINAL-SINE-VALUE / (2 * Q-RESONANCE).

      * 6. Finalize Biquad Coefficients
           PERFORM INIT-COEFFICIENTS.

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
           PERFORM UNTIL READ-POSITION <= TABLE-FLOAT
               SUBTRACT TABLE-FLOAT FROM READ-POSITION
               ADD 1 TO READ-POSITION
           END-PERFORM.
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


