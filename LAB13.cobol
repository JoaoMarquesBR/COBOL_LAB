*************************************************************
      *  LAB  7                                  *
      *************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.  LAB7.
       AUTHOR. Joao Marques.
      *  LAB EXERCISE 5.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT INPUT-FILE ASSIGN TO DA-S-INPUT
                FILE STATUS IS WS-IN-STATUS.
            SELECT PRNT-FILE ASSIGN TO UR-S-PRNT.

       DATA DIVISION.

       FILE SECTION.
       FD  INPUT-FILE
           RECORDING MODE IS F.

       COPY CCS1SLP.

       FD  PRNT-FILE
           RECORDING MODE IS F.
       01  PRNT-REC                   PIC X(125).
       WORKING-STORAGE SECTION.

       01 WS-SWITCHES.
          05 WS-SALESMAST-EOF-SWITCH PIC X VALUE "N".
             88 WS-SALESMAST-EOF           VALUE "Y".
             88 WS-SALESMAST-NOT-EOF       VALUE "N".


      **************************************************************
      *           LAYOUT FOR THE INPUT FILE                       *
      **************************************************************


      **************************************************************
      *    LAYOUT FOR THE 1ST HEADING LINE OF REPORT PRNTING       *
      **************************************************************
       01  WS-HEADING-LINE-1.
           05  FILLER                 PIC X(2)  VALUE SPACES.
           05  FILLER                 PIC X(4)  VALUE "FAGE".
           05  FILLER                 PIC X(2)  VALUE SPACES.
           05  WS-HL1-PAGENO1         PIC Z9.
           05  FILLER                 PIC X(32) VALUE SPACES.
           05  FILLER                 PIC X(15) VALUE "ABC CORPORATION".
           05  FILLER                 PIC X(25)  VALUE SPACES.
           05  WS-HL1-MONTH           PIC 9(2).
           05  FILLER                 PIC X(1)  VALUE "/".
           05  WS-HL1-DAY             PIC 9(2).
           05  FILLER                 PIC X(1)  VALUE "/".
           05  WS-HL1-YEAR            PIC 9(4).
           05  FILLER                 PIC X  VALUE SPACES.

       01  WS-HEADING-LINE-2.
           05  FILLER                 PIC X(20)  VALUE SPACES.
           05  FILLER                 PIC X(19)  VALUE SPACES.
           05  FILLER                 PIC X(12)  VALUE "SALESPERSON ".
           05  FILLER                 PIC X(9)  VALUE "BY BRANCH".

       01  WS-HEADING-LINE-3.
           05  FILLER                 PIC X(8)  VALUE "BRANCH: ".
           05  BRANCH-HEADING         PIC X(4).


       01  WS-CURRENT-DATE.
           05 WS-CD-YEAR               PIC X(4).
           05 WS-CD-MONTH              PIC XX.
           05 WS-CD-DAY                PIC XX.

       01 WS-RATING-VALUES.
          05 FILLER       PIC X(4) VALUE X"0000000C".
          05 FILLER       PIC X(4) VALUE X"1999999C".
          05 FILLER       PIC X(5) VALUE "    *".
          05 FILLER       PIC X(4) VALUE X"2000000C".
          05 FILLER       PIC X(4) VALUE X"3999990C".
          05 FILLER       PIC X(5) VALUE "   **".
          05 FILLER       PIC X(4) VALUE X"4000000C".
          05 FILLER       PIC X(4) VALUE X"5999999C".
          05 FILLER       PIC X(5) VALUE "  ***".
          05 FILLER       PIC X(4) VALUE X"6000000C".
          05 FILLER       PIC X(4) VALUE X"7999999C".
          05 FILLER       PIC X(5) VALUE " ****".
          05 FILLER       PIC X(4) VALUE X"8000000C".
          05 FILLER       PIC X(4) VALUE X"9999999C".
          05 FILLER       PIC X(5) VALUE "*****".

       01 WS-RATING-TABLE REDEFINES WS-RATING-VALUES.
          05 WS-RATING-GROUP OCCURS 5 TIMES.
             10   WS-SALES-RANGE-MIN-VALUE PIC 9(5)V99 COMP-3.
             10   WS-SALES-RANGE-MAX-VALUE PIC 9(5)V99 COMP-3.
             10   WS-STAR-RATING           PIC X(5).

          05 WS-RATING-SUB                    PIC 9(2) COMP.
      **************************************************************
      *      LAYOUT FOR THE 1ST  DATA LINE OF REPORT PRNTING       *
      **************************************************************
       01  PRNT-DATA1.
           05  SALESPERSON-LAST-NAME1      PIC X(15).
           05  FILLER                      PIC X(3)      VALUE SPACES.
           05  SALESPERSON-FIRST-NAME1     PIC X(10).
           05  FILLER                      PIC X(2)      VALUE SPACES.
           05  SALESPERSON-GROSS-SALES1    PIC $$$,$$$,$$$.$$.
           05  FILLER                      PIC X(2)      VALUE SPACES.
           05  SALESPERSON-RETURN-SALES1   PIC $$$,$$$,$$$.$$.
           05  FILLER                      PIC X(2)      VALUE SPACES.
           05  SALESPERSON-NET1            PIC $$$,$$$,$$$.$$.
           05  FILLER                      PIC X(2)      VALUE SPACES.
           05  SALESPERSON-COMM1           PIC $$$,$$$,$$$.$$.
           05  FILLER                      PIC X(2)      VALUE SPACES.
           05  SALESPERSON-RATING          PIC X(5).


       01 PRNT-BRANCH.
           05 FILLER                   PIC X(12)  VALUE "TOTAL BRANCH".
           05 FILLER                       PIC X(1)     VALUE SPACES.
           05 CURRENT-BRANCH               PIC X(3).
           05 FILLER                       PIC X(14)      VALUE SPACES.
           05 BRANCH-SUM1                  PIC $$$,$$$,$$$.$$.
           05 FILLER                       PIC X(2)      VALUE SPACES.
           05 BRANCH-RETURNS1              PIC $$$,$$$,$$$.$$.
           05 FILLER                       PIC X(2)      VALUE SPACES.
           05 BRANCH-NET1                  PIC $$$,$$$,$$$.$$.
           05 FILLER                       PIC X(2)      VALUE SPACES.
           05 BRANCH-COMM1                 PIC $$$,$$$,$$$.$$.

       01 PRNT-SEPARATOR.
           05 FILLER                   PIC X(31)      VALUE SPACES.
           05 FILLER                   PIC X(13)  VALUE "-------------".
           05 FILLER                   PIC X(6)      VALUE SPACES.
           05 FILLER                   PIC X(11)  VALUE "----------".
           05 FILLER                   PIC X(2)      VALUE SPACES.
           05 FILLER                   PIC X(13)  VALUE "-------------".
           05 FILLER                   PIC X(5)      VALUE SPACES.
           05 FILLER                   PIC X(11)  VALUE "-----------".


       01 PRNT-ENDING.
           05 FILLER                  PIC X(14)  VALUE "COMPANY TOTALS".
           05 FILLER                  PIC X(16)      VALUE SPACES.
           05 COMPANY-SUM1            PIC $$$,$$$,$$$.$$.
           05 FILLER                  PIC X(2)      VALUE SPACES.
           05 COMPANY-RETURNS1        PIC $$$,$$$,$$$.$$.
           05 FILLER                  PIC X(2)      VALUE SPACES.
           05 COMPANY-NET1            PIC $$$,$$$,$$$.$$.
           05 FILLER                  PIC X(2)      VALUE SPACES.
           05 COMPANY-COMM1           PIC $$$,$$$,$$$.$$.

       01 PRNT-SEPARATOR-TOTAL.
           05 FILLER                   PIC X(31)      VALUE SPACES.
           05 FILLER                   PIC X(13)  VALUE "=============".
           05 FILLER                   PIC X(6)      VALUE SPACES.
           05 FILLER                   PIC X(11)  VALUE "==========".
           05 FILLER                   PIC X(2)      VALUE SPACES.
           05 FILLER                   PIC X(13)  VALUE "=============".
           05 FILLER                   PIC X(5)      VALUE SPACES.
           05 FILLER                   PIC X(11)  VALUE "===========".

       01 PRNT-HEADERS.
           05 FILLER                  PIC X(9)  VALUE "LAST NAME".
           05 FILLER                  PIC X(7)  VALUE SPACES.
           05 FILLER                  PIC X(10) VALUE "FIRST NAME".
           05 FILLER                  PIC X(7)  VALUE SPACES.
           05 FILLER                  PIC X(11) VALUE "GROSS SALES".
           05 FILLER                  PIC X(8)  VALUE SPACES.
           05 FILLER                  PIC X(7) VALUE "RETURNS".
           05 FILLER                  PIC X(7)  VALUE SPACES.
           05 FILLER                  PIC X(9) VALUE "NET SALES".
           05 FILLER                  PIC X(7)  VALUE SPACES.
           05 FILLER                  PIC X(10) VALUE "COMMISSION".


      **************************************************************
      *                     BRANCH HOLD INFO      *
      **************************************************************
       01 BRANCH-DATA.
           05 BRANCH-SUM                    PIC 9(5)V99.
           05 BRANCH-NET                    PIC 9(5)V99.
           05 BRANCH-RETURNS                PIC 9(5)V99.
           05 BRANCH-COMM                   PIC 9(5)V99.

       01 CURRENT-READ.
           05 SALESPERSON-NET              PIC 9(10)V99.
           05 CURRENT-SUM-FORMAT           PIC $$$$,$$$.$$.
           05 CURRENT-SUM                  PIC 9(10)V99.
           05 CURRENT-RETURNS              PIC 9(10)V99.
           05 CURRENT-NET                  PIC 9(10)V99.
           05 CURRENT-COMM                 PIC 9(10)V99.
           05 PREVIOUS-SUM                 PIC 9(10)V99 VALUE 0.
           05 WS-HL1-PAGENO                PIC 9(3).

       01 COMPANY-DATA.
           05 COMPANY-SUM             PIC 9(10)V99.
           05 COMPANY-RETURNS         PIC 9(10)V99.
           05 COMPANY-NET             PIC 9(10)V99.
           05 COMPANY-COMM            PIC 9(10)V99  VALUE 0.

       01 SPACE-LINE.
           05 FILLER                  PIC X(50)     VALUE SPACES.

      **************************************************************
      *           LAYOUT FOR THE INPUT FILE                       *
      **************************************************************

       01  MISCELLEANOUS.
      **************************************************************
      *                 END OF FILE (EOF) SWITCHES                 *
      *            0 = NOT AT EOF          1 = AT EOF              *
      **************************************************************
           03  EOF-I                  PIC 9         VALUE 0.
           03  WS-IN-STATUS           PIC XX        VALUE SPACES.
      **************************************************************
      *               START OF PROCEDURE DIVISION                  *
      **************************************************************
       PROCEDURE DIVISION.
       000-MAINLINE.
           OPEN INPUT INPUT-FILE
                OUTPUT PRNT-FILE.
           IF WS-IN-STATUS NOT EQUAL "00"
           THEN DISPLAY "ERROR FILE, WS-IN-STATUS = ", WS-IN-STATUS
             DISPLAY "CHECK IF JCL AND COBOL FILE DEFINITIONS MATCH"
           ELSE
               PERFORM U100-FORMAT-DATE
               PERFORM 1400-STARTUP
               PERFORM 2000-READ-INPUT
               PERFORM 1400-PRINT-HEAD
               PERFORM 1500-LOOP UNTIL WS-SALESMAST-EOF-SWITCH = "Y"
               PERFORM 1400-GET-TOTAL
               CLOSE INPUT-FILE
                     PRNT-FILE
           END-IF.
           STOP RUN.

       1400-STARTUP.
           MOVE 0 TO WS-HL1-PAGENO.

       1400-PRINT-HEAD.
           MOVE SPACES TO PRNT-REC.
           MOVE 0.00 TO BRANCH-SUM
           MOVE 0.00 TO COMPANY-SUM
           MOVE 0.00 TO CURRENT-SUM
           MOVE 0.00 TO CURRENT-NET
           MOVE 0.00 TO CURRENT-RETURNS
           MOVE 0.00 TO COMPANY-NET
           MOVE 0.00 TO COMPANY-RETURNS
           MOVE 0.00 TO COMPANY-COMM.


       1400-GET-TOTAL.
           PERFORM 1500-PRNT-SPACEL

           WRITE PRNT-REC FROM PRNT-BRANCH
                AFTER ADVANCING 1 LINE

           WRITE PRNT-REC FROM PRNT-SEPARATOR
                AFTER ADVANCING 1 LINE

           MOVE COMPANY-SUM TO COMPANY-SUM1
           MOVE COMPANY-RETURNS TO COMPANY-RETURNS1

           MOVE COMPANY-NET TO COMPANY-NET1

           MOVE COMPANY-COMM TO COMPANY-COMM1


           WRITE PRNT-REC FROM PRNT-ENDING
                AFTER ADVANCING 1 LINE

           WRITE PRNT-REC FROM PRNT-SEPARATOR-TOTAL
                AFTER ADVANCING 1 LINE.

       1500-PRNT-SPACEL.
           WRITE PRNT-REC FROM SPACE-LINE
               AFTER ADVANCING 1 LINE.

       1500-LOOP.
           PERFORM 1600-SALESPERSON-MOVES.
           IF WS-IN-STATUS NOT EQUAL "00"
              DISPLAY "FILE ERROR WS-IN-STATUS = " , WS-IN-STATUS
           ELSE
              PERFORM 1600-SALESPERSON-CALCULATIONS
              PERFORM 2000-READ-INPUT
                 WRITE PRNT-REC FROM PRNT-DATA1
                 AFTER ADVANCING 1 LINE.


       1600-SALESPERSON-MOVES.
           MOVE SALESPERSON-LAST-NAME TO SALESPERSON-LAST-NAME1.
           MOVE SALESPERSON-FIRST-NAME TO SALESPERSON-FIRST-NAME1.
           MOVE SALESPERSON-GROSS-SALES TO SALESPERSON-GROSS-SALES1.
           MOVE SALESPERSON-RETURN-SALES TO SALESPERSON-RETURN-SALES1.



       1600-SALESPERSON-CALCULATIONS.
           COMPUTE COMPANY-SUM = SALESPERSON-GROSS-SALES + COMPANY-SUM

           COMPUTE COMPANY-RETURNS = SALESPERSON-RETURN-SALES +
           COMPANY-RETURNS

           IF SALESPERSON-BRANCH-NO IS NOT EQUAL TO CURRENT-BRANCH
           THEN
           COMPUTE WS-HL1-PAGENO = WS-HL1-PAGENO + 1
           PERFORM 1600-WORK
           END-IF.

           MOVE SALESPERSON-BRANCH-NO TO CURRENT-BRANCH.

           COMPUTE CURRENT-SUM = SALESPERSON-GROSS-SALES + CURRENT-SUM
           MOVE CURRENT-SUM TO BRANCH-SUM

           COMPUTE SALESPERSON-NET = SALESPERSON-GROSS-SALES -
           SALESPERSON-RETURN-SALES
           MOVE SALESPERSON-NET TO SALESPERSON-NET1

           PERFORM WITH TEST AFTER
             VARYING WS-RATING-SUB FROM 1 BY 1 UNTIL
              SALESPERSON-NET > WS-SALES-RANGE-MIN-VALUE (WS-RATING-SUB)
              AND
              SALESPERSON-NET < WS-SALES-RANGE-MAX-VALUE (WS-RATING-SUB)
              MOVE WS-STAR-RATING (WS-RATING-SUB) TO SALESPERSON-RATING
           END-PERFORM

           COMPUTE CURRENT-NET = SALESPERSON-NET + CURRENT-NET
           MOVE CURRENT-NET TO BRANCH-NET1

           COMPUTE CURRENT-RETURNS = SALESPERSON-RETURN-SALES
           + CURRENT-RETURNS
           MOVE CURRENT-RETURNS TO BRANCH-RETURNS1

           COMPUTE CURRENT-COMM ROUNDED = (SALESPERSON-GROSS-SALES -
           SALESPERSON-RETURN-SALES) * SALESPERSON-COMM-RATE

           MOVE CURRENT-COMM TO SALESPERSON-COMM1


           COMPUTE COMPANY-NET = SALESPERSON-NET + COMPANY-NET
           COMPUTE COMPANY-COMM = COMPANY-COMM + CURRENT-COMM
           COMPUTE BRANCH-COMM = BRANCH-COMM + CURRENT-COMM

           MOVE CURRENT-SUM TO BRANCH-SUM1
           MOVE BRANCH-COMM TO BRANCH-COMM1.

       1600-WORK.
           MOVE 0 TO CURRENT-SUM
           MOVE 0 TO CURRENT-NET
           MOVE 0 TO CURRENT-COMM
           MOVE 0 TO CURRENT-RETURNS
           MOVE 0 TO BRANCH-COMM

           MOVE SALESPERSON-BRANCH-NO TO BRANCH-HEADING

           MOVE WS-HL1-PAGENO TO WS-HL1-PAGENO1

           IF CURRENT-BRANCH > 0 THEN
           PERFORM 1500-PRNT-SPACEL
           WRITE PRNT-REC FROM PRNT-BRANCH
                AFTER ADVANCING 1 LINE
           WRITE PRNT-REC FROM PRNT-SEPARATOR
                AFTER ADVANCING 1 LINE
           END-IF


           WRITE PRNT-REC FROM WS-HEADING-LINE-1
                AFTER ADVANCING 1 LINE

           WRITE PRNT-REC FROM WS-HEADING-LINE-2
                AFTER ADVANCING 1 LINE

           PERFORM 1500-PRNT-SPACEL

           WRITE PRNT-REC FROM WS-HEADING-LINE-3
                AFTER ADVANCING 1 LINE

           PERFORM 1500-PRNT-SPACEL

           WRITE PRNT-REC FROM PRNT-HEADERS
               AFTER ADVANCING 1 LINE.

       U100-FORMAT-DATE.
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD.
           MOVE WS-CD-MONTH TO WS-HL1-MONTH.
           MOVE WS-CD-DAY   TO WS-HL1-DAY.
           MOVE WS-CD-YEAR  TO WS-HL1-YEAR.


      **************************************************************
      *                READS THE INPUT FILE                        *
      **************************************************************
       2000-READ-INPUT.
           READ INPUT-FILE
               AT END MOVE 'Y' TO
                WS-SALESMAST-EOF-SWITCH.