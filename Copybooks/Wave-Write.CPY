           CONTROL-PROCEDURE.
           OPEN INPUT IN-FILE.
           OPEN OUTPUT OUT-FILE.
           PERFORM PROCESS-LOOP UNTIL TERMINATOR.
           CLOSE IN-FILE.
           CLOSE OUT-FILE.
            STOP RUN.

       PROCESS-LOOP.
           PERFORM BINARY-PULL.  *> Read two bytes.
           IF TERMINATOR
               EXIT PARAGRAPH  *> Exit loop early if EOF after first byte.
           END-IF.
           PERFORM GET-NUMERIC.
           PERFORM COMBINE-BYTES.
           PERFORM MOVE-TEST.
           PERFORM BACK-2-BYTES.
           PERFORM EXTRACT-LOW.
           PERFORM EXTRACT-HIGH.




       BINARY-PULL.
           READ IN-FILE INTO TWO-BYTE-BUFFER(1:1)
               AT END MOVE "Y" TO FILE-STATUS
               EXIT PARAGRAPH  *> Exit if EOF on first byte.
           END-READ.
           READ IN-FILE INTO TWO-BYTE-BUFFER(2:1)
               AT END MOVE "Y" TO FILE-STATUS  *> Set flag if EOF on second byte.
           END-READ.

       GET-NUMERIC.
           COMPUTE LITTLE-END =
               FUNCTION ORD (TWO-BYTE-BUFFER(1:1)) - 1.

           COMPUTE BIG-END =
               FUNCTION ORD (TWO-BYTE-BUFFER(2:1)) - 1.

       COMBINE-BYTES.
           COMPUTE RESULT-WRITER =
               LITTLE-END + (BIG-END * 256).

           IF RESULT-WRITER > 32767
               SUBTRACT 65536 FROM RESULT-WRITER.

           MOVE RESULT-WRITER TO ENDIAN.

       MOVE-TEST.
           DISPLAY RESULT-WRITER.
           DISPLAY ENDIAN.



       BACK-2-BYTES.
           IF RESULT-WRITER < 0
               COMPUTE RESULT-WRITER = RESULT-WRITER + 65536.

       EXTRACT-LOW.
           COMPUTE PROCESSED-LITTLE = FUNCTION MOD
               (RESULT-WRITER, 256).

           MOVE FUNCTION CHAR(PROCESSED-LITTLE + 1) TO RAW-BYTES.  *> Added +1.
           WRITE RAW-BYTES.

       EXTRACT-HIGH.
           IF RESULT-WRITER < 0
               COMPUTE RESULT-WRITER = RESULT-WRITER + 65536.

           COMPUTE PROCESSED-BIG = RESULT-WRITER / 256.

           MOVE FUNCTION CHAR(PROCESSED-BIG + 1) TO RAW-BYTES.
           WRITE RAW-BYTES.
           DISPLAY PROCESSED-LITTLE.
           DISPLAY PROCESSED-BIG.
