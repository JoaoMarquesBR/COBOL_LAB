       IDENTIFICATION DIVISION.
       PROGRAM-ID.  PCS2PRG1.
       AUTHOR. KEVIN MARQUEZ AND JOAO MARQUES.
      *************************************************************
      *                     INFO3105 CASE2                        *
      *                        PCS2PRG1                           *
      *                COBOL ZOS PROGRAM DESCRIPTION              *
      *************************************************************
      * PROGRAM DESCRIPTION:                                      *
      *   PROGRAM TO PROCESS TRANSACTION SALES RECORDS AGAINST    *
      *   THE SALESPERSON MASTER FILE. NOTE THE MASTER FILE WAS   *
      *   CONVERTED TO A VSAM KSDS FOR THIS CASE STUDY.           *
      *                                                           *
      *  INPUT DD NAME     FILE IDENTIFIER     FILE DESCRIPTION   *
      *  -------------     ---------------     ----------------   *
      *  SLSTRANS          SALESTRANS          SALES TRANSACTIONS *
      *  SLSPKS            SALESMAST         SALESPERSON VSAM KSDS*
      *                                                           *
      *  OUTPUT DD NAME    FILE IDENTIFIER     FILE DESCRIPTION   *
      *  --------------    ---------------     -----------------  *
      *  PRNT               SALESRPT           SYSOUT REPORT FILE *
      *                                                           *
      *  COPYBOOKS              DESCRIPTION                       *
      *  ---------              -----------                       *
      *  CCS2SLST               LAYOUT FOR STATIC PART OF TRANS   *
      *  CCS2SLSW               LAYOUT FOR DYNAMIC PART OF TRANS  *
      *  CCS2SLSP               LAYOUT FOR SALESPERSON MASTER     *
      *************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT SALESMAST ASSIGN TO SLSPKS
              ORGANIZATION IS INDEXED
              ACCESS IS RANDOM
              RECORD KEY IS SALESPERSON-NO
              ALTERNATE KEY IS SALESPERSON-BRANCH-NO WITH DUPLICATES
              FILE STATUS IS WS-IN-STATUS.
            SELECT SALESTRANS ASSIGN TO SLSTRANS
              FILE STATUS IS WS-TRN-STATUS.
            SELECT SALESRPT ASSIGN TO PRNT.

       DATA DIVISION.

       FILE SECTION.

      *************************************************************
      *                      CHANGE LOG                           *
      *************************************************************
      *  VER.  WRITTEN/CHANGED BY           IMPLEMENTATION        *
      *  3.0   KEVIN MARQUEZ                2023-06-01            *
      *  ----  ------------------           --------------        *
      *                                                           *
      *  DESCRIPTION OF CHANGE:                                   *
      *  ----------------------                                   *
      *  ADDED IN TOTALS OF ALL TRANSACTIONS AND AN EVALUATE CASE *
      *  FOR EACH TRANSACTION OPTION WE WILL RUN INTO IN SLSTRANS.*
      *  MADE THE FUNCTIONALITY OF EACH TRANSACTION TYPE AND      *
      *  IMPROVED FORMATTING OF THE REPORT TO INCLUDE CREDIT AND  *
      *  IGNORING LEADING ZEROES                                  *
      *************************************************************
      *Salesperson-gross-sales
       FD  SALESMAST.
      *COPY BOOK FOR SALESPERSON MASTER FILE
       COPY CCS2SLSP.

       FD  SALESTRANS
           RECORDING MODE IS F.
      *COPY BOOK FOR SALES TRANSACTION FILES
       COPY CCS2SLST.

       FD  SALESRPT
           RECORDING MODE IS F.
       01  PRINT-AREA                   PIC X(100).
       WORKING-STORAGE SECTION.
       COPY CCS2SLSW.
      **************************************************************
      *    LAYOUT FOR THE 1ST HEADING LINE OF REPORT PRNTING       *
      **************************************************************
       01  WS-HEADING-LINE-1.
           03  FILLER                 PIC X(5)       VALUE SPACES.
           03  FILLER                 PIC X(6)       VALUE "PAGE  ".
           03  WS-HL1-PAGENO          PIC Z9.
           03  FILLER                 PIC X(29)      VALUE SPACES.
           03  FILLER                 PIC X(15)
                                      VALUE "ABC CORPORATION".
           03  FILLER                 PIC X(27)      VALUE SPACES.
           03  WS-HL1-MONTH           PIC 9(2).
           03  FILLER                 PIC X(1)       VALUE "/".
           03  WS-HL1-DAY             PIC 9(2).
           03  FILLER                 PIC X(1)       VALUE "/".
           03  WS-HL1-YEAR            PIC 9(4).
           03  FILLER                 PIC X          VALUE SPACES.

      **************************************************************
      *    LAYOUT FOR THE 2ND HEADING LINE OF REPORT PRNTING       *
      **************************************************************
       01  WS-HEADING-LINE-2.
           03  FILLER                 PIC X(40)      VALUE SPACES.
           03  FILLER                 PIC X(5)       VALUE 'SALES'.
           03  FILLER                 PIC X          VALUE SPACES.
           03  FILLER                 PIC X(11)
                                      VALUE 'TRANSACTION'.
           03  FILLER                 PIC X          VALUE SPACES.
           03  FILLER                 PIC X(3)       VALUE 'LOG'.

      **************************************************************
      *    LAYOUT FOR THE COLUMN HEADINGS OF REPORT PRINTING       *
      **************************************************************
       01  WS-COLUMN-HEADINGS.
           03  FILLER                 PIC X(3)       VALUE SPACES.
           03  FILLER                 PIC X(9)       VALUE 'TRANS NO.'.
           03  FILLER                 PIC X(2)       VALUE SPACES.
           03  FILLER                 PIC X(11)
                                      VALUE 'SALESPERSON'.
           03  FILLER                 PIC X(3)       VALUE SPACES.
           03  FILLER                 PIC X(5)       VALUE 'TRANS'.
           03  FILLER                 PIC X          VALUE SPACES.
           03  FILLER                 PIC X(4)       VALUE 'DATE'.
           03  FILLER                 PIC X(5)       VALUE SPACES.
           03  FILLER                 PIC X(4)       VALUE 'TYPE'.
           03  FILLER                 PIC X(8)       VALUE SPACES.
           03  FILLER                 PIC X(6)       VALUE 'AMOUNT'.

      **************************************************************
      *    LAYOUT FOR THE DETAIL LINE OF REPORT PRINTING           *
      **************************************************************
       01  WS-PRNT-DETAIL-LINE.
           03  FILLER                 PIC X(3)       VALUE SPACES.
           03  WS-PRNT-TRANS-NO       PIC Z(5).
           03  FILLER                 PIC X(10)      VALUE SPACES.
           03  WS-PRNT-SALESPERSON-NO PIC 9(5).
           03  FILLER                 PIC X(7)       VALUE SPACES.
           03  WS-PRNT-TRANS-YEAR     PIC X(2).
           03  FILLER                 PIC X          VALUE '/'.
           03  WS-PRNT-TRANS-MONTH    PIC X(2).
           03  FILLER                 PIC X          VALUE '/'.
           03  WS-PRNT-TRANS-DAY      PIC X(2).
           03  FILLER                 PIC X(6)       VALUE SPACES.
           03  WS-PRNT-TYPE           PIC X(6).
           03  FILLER                 PIC X(6)       VALUE SPACES.
           03  WS-TL-AMT   PIC $$$,$$$.99CR
               BLANK WHEN ZERO VALUE ZERO.

      **************************************************************
      *    LAYOUT FOR THE TOTAL TRANSACTIONS OF REPORT PRINTING    *
      **************************************************************
       01  WS-PRINT-TOTALS.
           03  FILLER                 PIC X(5)       VALUE '# OF '.
           03  WS-PRNT-TYPE-NAME      PIC X(7).
           03  FILLER                 PIC X          VALUE SPACES.
           03  FILLER                 PIC X(6)       VALUE 'TRANS '.
           03  WS-PRNT-TYPE-TOTALS    PIC 9(3).

      **************************************************************
      *    LAYOUT FOR THE CALC-FIELDS OF THE REPORT                *
      **************************************************************
       01  WS-CALC-SUM-FIELDS.
           05  WS-CALC-PAGE-NO         PIC 9(3)       VALUE ZERO.
           05  WS-NUM-SALES-TRANS      PIC 9(3)       VALUE ZEROES.
           05  WS-NUM-RETURN-TRANS     PIC 9(3)       VALUE ZEROES.
           05  WS-NUM-ADD-TRANS        PIC 9(3)       VALUE ZEROES.
           05  WS-NUM-DELETE-TRANS     PIC 9(3)       VALUE ZEROES.
           05  WS-NUM-CHANGE-TRANS     PIC 9(3)       VALUE ZEROES.
           05  WS-NUM-FAILED-TRANS     PIC 9(3)       VALUE ZEROES.
           05  WS-TRANS-GROSS-SALES    PIC S9(7)V99   VALUE ZERO.
           05  WS-TRANS-RETURNS        PIC S9(5)V99   VALUE ZERO.

      **************************************************************
      *    LAYOUT FOR THE CURRENT DATE OF THE REPORT DATE          *
      **************************************************************
       01  WS-CURRENT-DATE.
           05 WS-CD-YEAR               PIC X(4).
           05 WS-CD-MONTH              PIC XX.
           05 WS-CD-DAY                PIC XX.

      **************************************************************
      *    LAYOUT FOR THE TRANSACTION DATE OF THE REPORT           *
      **************************************************************
       01  WS-TRANS-DATE.
           05 WS-TRANS-YEAR            PIC XX.
           05 WS-TRANS-MONTH           PIC XX.
           05 WS-TRANS-DAY             PIC XX.

      **************************************************************
      *    LAYOUT FOR THE EOF-SWITCHES OF THE REPORT               *
      **************************************************************
       01  WS-SWITCHES.
           05 WS-SALESMAST-EOF-SWITCH   PIC X          VALUE "N".
              88 WS-SALESMAST-EOF                      VALUE "Y".
              88 WS-SALESMAST-NOT-EOF                  VALUE "N".

      **************************************************************
      *    LAYOUT TO CAST BRANCH AND DEPARTMENT NO TO SALESMAST    *
      **************************************************************
       01  WS-SALESBRANCH-NO-CAT.
           03 WS-CAT-BRANCH-NO          PIC 9(3).
           03 WS-CAT-DEPARTMENT-NO      PIC 9(2).

      **************************************************************
      *    LAYOUT FOR THE ERROR LINE OF THE REPORT                 *
      **************************************************************
       01  WS-ERROR-LINE.
           03  FILLER                 PIC X(3)      VALUE SPACES.
           03  WS-ER-TRN-NO           PIC Z(5)      VALUE ZEROES.
           03  FILLER                 PIC X(3)      VALUE SPACES.
           03  WS-ER-PROBLEM          PIC X(35)     VALUE SPACES.
           03  WS-ER-IDX-STATUS       PIC XX        VALUE SPACES.

       01  MISCELLEANOUS.
      **************************************************************
      *                 WS-IN-STATUS AND BREAK-FIELD               *
      **************************************************************
           03  WS-IN-STATUS           PIC XX        VALUE SPACES.
           03  WS-TRN-STATUS          PIC XX        VALUE SPACES.
           03  BREAK-FIELD            PIC 9(3)      VALUE ZEROES.
      **************************************************************
      *               START OF PROCEDURE DIVISION                  *
      **************************************************************
       PROCEDURE DIVISION.
       A000-MAINLINE.
           OPEN INPUT SALESTRANS
                I-O SALESMAST
                OUTPUT SALESRPT
           IF WS-IN-STATUS NOT EQUAL "00"
               DISPLAY "FILE ERROR WS-IN-STATUS = ", WS-IN-STATUS
           ELSE
                PERFORM B1500-LOOP-PARAGRAPH UNTIL WS-SALESMAST-EOF

                CLOSE SALESTRANS
                      SALESMAST
                      SALESRPT
           END-IF
           STOP RUN.

       B1500-LOOP-PARAGRAPH.
           IF BREAK-FIELD EQUAL 0
              MOVE 100 TO BREAK-FIELD
              PERFORM W140-PRINT-HEAD.
           PERFORM R2000-READ-INPUT.
              IF WS-SALESMAST-NOT-EOF
                 PERFORM U220-EVALUATE-TRANSACTION-TYPE
              ELSE
                 PERFORM W240-PRINT-TRANSACTION-TOTAL.

      **************************************************************
      *               READS THE INPUT FILE                         *
      **************************************************************
       R2000-READ-INPUT.
           READ SALESTRANS AT END MOVE 'Y' TO WS-SALESMAST-EOF-SWITCH.

      **************************************************************
      *               OBTAIN THE CURRENT DATE                      *
      **************************************************************
       U100-FORMAT-DATE.
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD.
           MOVE WS-CD-MONTH TO WS-HL1-MONTH.
           MOVE WS-CD-DAY TO WS-HL1-DAY.
           MOVE WS-CD-YEAR TO WS-HL1-YEAR.

      **************************************************************
      *               OBTAIN THE TRANSACTION DATE                  *
      **************************************************************
       U200-FORMAT-TRANS-DATE.
           MOVE TRANS-DATE TO WS-TRANS-DATE.
           MOVE WS-TRANS-YEAR TO WS-PRNT-TRANS-YEAR.
           MOVE WS-TRANS-MONTH TO WS-PRNT-TRANS-MONTH.
           MOVE WS-TRANS-DAY TO WS-PRNT-TRANS-DAY.

      **************************************************************
      *               PRINTS THE HEADER AND COLUMNS                *
      **************************************************************
       W140-PRINT-HEAD.
           COMPUTE
              WS-CALC-PAGE-NO = WS-CALC-PAGE-NO + 1
           END-COMPUTE.
           MOVE WS-CALC-PAGE-NO TO WS-HL1-PAGENO
           PERFORM U100-FORMAT-DATE.
           WRITE PRINT-AREA FROM WS-HEADING-LINE-1
                 AFTER ADVANCING PAGE
           MOVE SPACES TO PRINT-AREA
           WRITE PRINT-AREA FROM WS-HEADING-LINE-2
                 AFTER ADVANCING 1 LINES
           MOVE SPACES TO PRINT-AREA

           WRITE PRINT-AREA FROM WS-COLUMN-HEADINGS
                 AFTER ADVANCING 1 LINES
           MOVE SPACES TO PRINT-AREA.

      **************************************************************
      *                 EVALUATE TYPE OF TRANSACTION               *
      **************************************************************
       U220-EVALUATE-TRANSACTION-TYPE.
            EVALUATE TRUE
                WHEN TRANS-ADD
                   MOVE TRANS-DATA TO WS-TRANS-MAINTENANCE
                   PERFORM C100-PROCESS-ADD
                WHEN TRANS-DEL
                   MOVE TRANS-DATA TO WS-TRANS-MAINTENANCE
                   PERFORM C200-PROCESS-DEL
                WHEN TRANS-CHG
                   MOVE TRANS-DATA TO WS-TRANS-MAINTENANCE
                   PERFORM C300-PROCESS-CHG
                WHEN TRANS-SALE
                   MOVE TRANS-DATA TO WS-TRANS-SALE
                   PERFORM C400-PROCESS-SALE
                WHEN TRANS-RET
                   MOVE TRANS-DATA TO WS-TRANS-RETURN
                   PERFORM C500-PROCESS-RETURN
                WHEN OTHER
                   DISPLAY 'INVALID INPUT'
            END-EVALUATE.

      **************************************************************
      *               PROCESS ADD TRANSACTIONS                     *
      **************************************************************
       C100-PROCESS-ADD.
            PERFORM M100-MOVE-TRANS-TO-SALES.
            MOVE 0 TO SALESPERSON-COMM-RATE.
            MOVE 0 TO SALESPERSON-GROSS-SALES.
            MOVE 0 TO SALESPERSON-RETURN-SALES.

            WRITE SALESPERSON-MASTER
             IF WS-IN-STATUS = '00' OR WS-IN-STATUS = '02'
                 MOVE 'ADD' TO WS-PRNT-TYPE
                 MOVE 0 TO WS-TL-AMT
                 PERFORM W230-PRINT-TRANSACTION-LOG
                 COMPUTE
                    WS-NUM-ADD-TRANS = WS-NUM-ADD-TRANS + 1
                 END-COMPUTE
            ELSE
                 MOVE TRANS-NO TO WS-ER-TRN-NO
                 MOVE 'PROBLEM DOING ADD, STATUS IS:'
                       TO WS-ER-PROBLEM
                 MOVE WS-IN-STATUS TO WS-ER-IDX-STATUS
                 WRITE PRINT-AREA FROM WS-ERROR-LINE
                 COMPUTE
                    WS-NUM-FAILED-TRANS = WS-NUM-FAILED-TRANS + 1
                 END-COMPUTE
            END-IF.

      **************************************************************
      *               PROCESS DELETE TRANSACTIONS                  *
      **************************************************************
       C200-PROCESS-DEL.
            MOVE TRANS-SALESPERSON-NO TO SALESPERSON-NO.

            DELETE SALESMAST RECORD
             IF WS-IN-STATUS = '00' OR WS-IN-STATUS = '02'
                 MOVE 'DELETE' TO WS-PRNT-TYPE
                 MOVE 0 TO WS-TL-AMT
                 PERFORM W230-PRINT-TRANSACTION-LOG
                 COMPUTE
                   WS-NUM-DELETE-TRANS = WS-NUM-DELETE-TRANS + 1
                 END-COMPUTE
            ELSE
                 MOVE TRANS-NO TO WS-ER-TRN-NO
                 MOVE 'PROBLEM DOING DELETE, STATUS IS:'
                       TO WS-ER-PROBLEM
                 MOVE WS-IN-STATUS TO WS-ER-IDX-STATUS
                 WRITE PRINT-AREA FROM WS-ERROR-LINE
                 COMPUTE
                    WS-NUM-FAILED-TRANS = WS-NUM-FAILED-TRANS + 1
                 END-COMPUTE
            END-IF.

      **************************************************************
      *               PROCESS UPDATE TRANSACTIONS                  *
      **************************************************************
       C300-PROCESS-CHG.
            READ SALESMAST
             IF WS-IN-STATUS = '00' OR WS-IN-STATUS = '02'
                PERFORM M100-MOVE-TRANS-TO-SALES
                REWRITE SALESPERSON-MASTER
                 IF WS-IN-STATUS = '00' OR WS-IN-STATUS = '02'
                      MOVE 'CHANGE' TO WS-PRNT-TYPE
                      MOVE 0 TO WS-TL-AMT
                      PERFORM W230-PRINT-TRANSACTION-LOG
                      COMPUTE
                         WS-NUM-CHANGE-TRANS = WS-NUM-CHANGE-TRANS + 1
                      END-COMPUTE
                 ELSE
                      MOVE TRANS-NO TO WS-ER-TRN-NO
                      MOVE 'PROBLEM DOING CHANGE, STATUS IS:'
                            TO WS-ER-PROBLEM
                      MOVE WS-IN-STATUS TO WS-ER-IDX-STATUS
                      WRITE PRINT-AREA FROM WS-ERROR-LINE
                       COMPUTE
                         WS-NUM-FAILED-TRANS = WS-NUM-FAILED-TRANS + 1
                       END-COMPUTE
                 END-IF
             ELSE
                 MOVE TRANS-NO TO WS-ER-TRN-NO
                 MOVE 'PROBLEM DOING CHANGE, STATUS IS:'
                       TO WS-ER-PROBLEM
                 MOVE WS-IN-STATUS TO WS-ER-IDX-STATUS
                 WRITE PRINT-AREA FROM WS-ERROR-LINE
                 COMPUTE
                    WS-NUM-FAILED-TRANS = WS-NUM-FAILED-TRANS + 1
                 END-COMPUTE
             END-IF.

      **************************************************************
      *               PROCESS SALES TRANSACTIONS                   *
      **************************************************************
       C400-PROCESS-SALE.
            COMPUTE
               WS-TRANS-GROSS-SALES ROUNDED = WS-SALES-AMOUNT -
               (WS-SALES-AMOUNT * WS-DISCOUNT-PCT)
            END-COMPUTE.

            MOVE TRANS-SALESPERSON-NO TO SALESPERSON-NO.

            READ SALESMAST
             IF WS-IN-STATUS = '00' OR WS-IN-STATUS = '02'
                COMPUTE
                    SALESPERSON-GROSS-SALES =
                    SALESPERSON-GROSS-SALES + WS-TRANS-GROSS-SALES
                END-COMPUTE
                REWRITE SALESPERSON-MASTER
                 IF WS-IN-STATUS = '00' OR WS-IN-STATUS = '02'
                      MOVE 'SALE' TO WS-PRNT-TYPE
                      MOVE WS-TRANS-GROSS-SALES TO WS-TL-AMT
                      PERFORM W230-PRINT-TRANSACTION-LOG

                      COMPUTE
                         WS-NUM-SALES-TRANS = WS-NUM-SALES-TRANS + 1
                      END-COMPUTE
                 ELSE
                      MOVE TRANS-NO TO WS-ER-TRN-NO
                      MOVE 'PROBLEM DOING SALE, STATUS IS:'
                            TO WS-ER-PROBLEM
                      MOVE WS-IN-STATUS TO WS-ER-IDX-STATUS
                      WRITE PRINT-AREA FROM WS-ERROR-LINE
                      COMPUTE
                         WS-NUM-FAILED-TRANS = WS-NUM-FAILED-TRANS + 1
                      END-COMPUTE
                 END-IF
             ELSE
                 MOVE TRANS-NO TO WS-ER-TRN-NO
                 MOVE 'PROBLEM DOING SALE, STATUS IS:'
                       TO WS-ER-PROBLEM
                 MOVE WS-IN-STATUS TO WS-ER-IDX-STATUS
                 WRITE PRINT-AREA FROM WS-ERROR-LINE
                 COMPUTE
                    WS-NUM-FAILED-TRANS = WS-NUM-FAILED-TRANS + 1
                 END-COMPUTE
             END-IF.

      **************************************************************
      *               PROCESS RETURN TRANSACTIONS                  *
      **************************************************************
       C500-PROCESS-RETURN.
            MOVE TRANS-SALESPERSON-NO TO SALESPERSON-NO.

            READ SALESMAST
             IF WS-IN-STATUS = '00' OR WS-IN-STATUS = '02'
                COMPUTE
                    SALESPERSON-RETURN-SALES =
                    SALESPERSON-RETURN-SALES + WS-RETURN-AMOUNT
                END-COMPUTE
                REWRITE SALESPERSON-MASTER
                 IF WS-IN-STATUS = '00' OR WS-IN-STATUS = '02'
                      MOVE 'RETURN' TO WS-PRNT-TYPE
                      COMPUTE
                          WS-TRANS-RETURNS = WS-RETURN-AMOUNT * -1
                      END-COMPUTE
                      MOVE WS-TRANS-RETURNS TO WS-TL-AMT
                      PERFORM W230-PRINT-TRANSACTION-LOG

                      COMPUTE
                        WS-NUM-RETURN-TRANS = WS-NUM-RETURN-TRANS + 1
                      END-COMPUTE
                 ELSE
                      MOVE TRANS-NO TO WS-ER-TRN-NO
                      MOVE 'PROBLEM DOING RETURN, STATUS IS:'
                            TO WS-ER-PROBLEM
                      MOVE WS-IN-STATUS TO WS-ER-IDX-STATUS
                      WRITE PRINT-AREA FROM WS-ERROR-LINE
                      COMPUTE
                         WS-NUM-FAILED-TRANS = WS-NUM-FAILED-TRANS + 1
                      END-COMPUTE
                 END-IF
             ELSE
                 MOVE TRANS-NO TO WS-ER-TRN-NO
                 MOVE 'PROBLEM DOING RETURN, STATUS IS:'
                       TO WS-ER-PROBLEM
                 MOVE WS-IN-STATUS TO WS-ER-IDX-STATUS
                 WRITE PRINT-AREA FROM WS-ERROR-LINE
                 COMPUTE
                    WS-NUM-FAILED-TRANS = WS-NUM-FAILED-TRANS + 1
                 END-COMPUTE
             END-IF.

      **************************************************************
      *          MOVE TRANSACTION TO SALESPERSON DATA              *
      **************************************************************
       M100-MOVE-TRANS-TO-SALES.
            MOVE TRANS-SALESPERSON-NO TO SALESPERSON-NO.
            MOVE WS-TRANS-LAST-NAME TO SALESPERSON-LAST-NAME.
            MOVE WS-TRANS-FIRST-NAME TO SALESPERSON-FIRST-NAME.
            MOVE WS-TRANS-BRANCH-NO TO WS-CAT-BRANCH-NO.
            MOVE WS-TRANS-DEPT-NO TO WS-CAT-DEPARTMENT-NO.
            MOVE WS-SALESBRANCH-NO-CAT TO SALESPERSON-BRANCH-NO.

      **************************************************************
      *               PRINT TO TRANSACTION LOG                     *
      **************************************************************
       W230-PRINT-TRANSACTION-LOG.
            MOVE TRANS-NO TO WS-PRNT-TRANS-NO.
            MOVE TRANS-SALESPERSON-NO TO WS-PRNT-SALESPERSON-NO.
            PERFORM U200-FORMAT-TRANS-DATE.
            WRITE PRINT-AREA FROM WS-PRNT-DETAIL-LINE
            AFTER ADVANCING 1 LINE.

      **************************************************************
      *               PRINT TO TRANSACTION TOTAL                   *
      **************************************************************
       W240-PRINT-TRANSACTION-TOTAL.
            MOVE 'SALES' TO WS-PRNT-TYPE-NAME.
            MOVE WS-NUM-SALES-TRANS TO WS-PRNT-TYPE-TOTALS.
            WRITE PRINT-AREA FROM WS-PRINT-TOTALS
            AFTER ADVANCING 1 LINE.

            MOVE 'RETURN' TO WS-PRNT-TYPE-NAME.
            MOVE WS-NUM-RETURN-TRANS TO WS-PRNT-TYPE-TOTALS.
            WRITE PRINT-AREA FROM WS-PRINT-TOTALS
            AFTER ADVANCING 1 LINE.

            MOVE 'ADD' TO WS-PRNT-TYPE-NAME.
            MOVE WS-NUM-ADD-TRANS TO WS-PRNT-TYPE-TOTALS.
            WRITE PRINT-AREA FROM WS-PRINT-TOTALS
            AFTER ADVANCING 1 LINE.

            MOVE 'DEL' TO WS-PRNT-TYPE-NAME.
            MOVE WS-NUM-DELETE-TRANS TO WS-PRNT-TYPE-TOTALS.
            WRITE PRINT-AREA FROM WS-PRINT-TOTALS
            AFTER ADVANCING 1 LINE.

            MOVE 'CHANGE' TO WS-PRNT-TYPE-NAME.
            MOVE WS-NUM-CHANGE-TRANS TO WS-PRNT-TYPE-TOTALS.
            WRITE PRINT-AREA FROM WS-PRINT-TOTALS
            AFTER ADVANCING 1 LINE.

            MOVE 'INVALID' TO WS-PRNT-TYPE-NAME.
            MOVE WS-NUM-FAILED-TRANS TO WS-PRNT-TYPE-TOTALS.
            WRITE PRINT-AREA FROM WS-PRINT-TOTALS
            AFTER ADVANCING 1 LINE.
