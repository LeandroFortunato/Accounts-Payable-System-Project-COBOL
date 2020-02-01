IDENTIFICATION DIVISION.
PROGRAM-ID. deductibles-report.
ENVIRONMENT DIVISION.
   INPUT-OUTPUT SECTION.
      FILE-CONTROL.

         COPY "SLVOUCH.CBL".        
         COPY "SLVND02.CBL".

         SELECT PRINTER-FILE 
                ASSIGN TO "deductibles-report.prn"
                ORGANIZATION IS LINE SEQUENTIAL.         

         SELECT WORK-FILE 
                ASSIGN TO "work-file"
                ORGANIZATION IS SEQUENTIAL.

         SELECT SORT-FILE
                ASSIGN TO "sort-file.tmp".

DATA DIVISION.
   FILE SECTION.
 
         COPY "FDVOUCH.CBL".        
         COPY "FDVND02.CBL".

         FD PRINTER-FILE
            LABEL RECORDS ARE OMITTED.
         01 PRINTER-RECORD         PIC X(80).

         FD WORK-FILE
            LABEL RECORDS ARE STANDARD.
         01 WORK-RECORD.
             05 WORK-NUMBER        PIC 9(5).
             05 WORK-VENDOR        PIC 9(5).
             05 WORK-INVOICE       PIC X(15).
             05 WORK-FOR           PIC X(30).
             05 WORK-AMOUNT        PIC S9(6)V99.
             05 WORK-DATE          PIC 9(8).
             05 WORK-DUE           PIC 9(8).
             05 WORK-DEDUCTIBLE    PIC X.
             05 WORK-SELECTED      PIC X.
             05 WORK-PAID-AMOUNT   PIC S9(6)V99.
             05 WORK-PAID-DATE     PIC 9(8).
             05 WORK-CHECK-NO      PIC 9(6).

        SD SORT-FILE.
        01 SORT-RECORD.
            05 SORT-NUMBER        PIC 9(5).
            05 SORT-VENDOR        PIC 9(5).
            05 SORT-INVOICE       PIC X(15).
            05 SORT-FOR           PIC X(30).
            05 SORT-AMOUNT        PIC S9(6)V99.
            05 SORT-DATE          PIC 9(8).
            05 SORT-DUE           PIC 9(8).
            05 SORT-DEDUCTIBLE    PIC X.
            05 SORT-SELECTED      PIC X.
            05 SORT-PAID-AMOUNT   PIC S9(6)V99.
            05 SORT-PAID-DATE     PIC 9(8).
            05 SORT-CHECK-NO      PIC 9(6).

   WORKING-STORAGE SECTION.

         01 TITLE.
            05 FILLER              PIC X(25) VALUE SPACES.
            05 FILLER              PIC X(18) VALUE "DEDUCTIBLES REPORT".
            05 FILLER              PIC X(22) VALUE SPACES.
            05 FILLER              PIC X(05) VALUE "PAGE:".
            05 PAGE-NUMBER         PIC 9(04) VALUE 0.

         01 HEADING-1.
            05 FILLER              PIC X(19) VALUE "VOUCHER VENDOR/For".
            05 FILLER              PIC X(22) VALUE SPACES.
            05 FILLER              PIC X(38) VALUE "PAID DATE  AMOUNT PAID INVOICE".

         01 HEADING-2.
            05 FILLER              PIC X(38) VALUE "======= ==============================".
            05 FILLER              PIC X(03) VALUE SPACES.
            05 FILLER              PIC X(38) VALUE "========== =========== ===============".

         01 DETAIL-1.
            05 D-WORK-NUMBER              PIC ZZZZ9.
            05 FILLER                     PIC X(03) VALUE SPACES.
            05 D-VENDOR-NAME              PIC X(30).
            05 FILLER                     PIC X(03) VALUE SPACES.
            05 FORMATTED-DATE-MM-DD-CCYY  PIC 99/99/9999.
            05 FILLER                     PIC X(01) VALUE SPACES.
            05 D-WORK-PAID-AMOUNT         PIC ZZZ,ZZ9.99-.
            05 FILLER                     PIC X(01) VALUE SPACES.
            05 D-WORK-INVOICE             PIC X(15).

         01 DETAIL-2.
            05 FILLER                     PIC X(01) VALUE SPACES.
            05 D-WORK-FOR                 PIC X(30).

         01 CONTROL-BREAK.
            05 D-DESCRIPTION              PIC X(12).
            05 D-DATE-REFERENCE           PIC 99/99/9999.
            05 D-TOTAL                    PIC ZZZ,ZZZ,ZZ9.99-.
 
         01 W-END-OF-FILE          PIC X.
            88 END-OF-FILE      VALUE "Y".

         01 W-FOUND-VENDOR-RECORD  PIC X.
            88 FOUND-VENDOR-RECORD  VALUE "Y".

         01 W-PRINTED-LINES        PIC 99.
            88 PAGE-FULL        VALUE 30 THROUGH 99.

         77 DUMMY-DATE-MM-DD-CCYY-12             PIC 9(12).
         77 DUMMY-DATE-MM-DD-CCYY-8              PIC 9(8).

         77 CURRENT-PAID-DATE                    PIC 9(8).         
         77 CURRENT-PAID-DATE-TOTAL              PIC S9(7)V99.
         77 GRAND-TOTAL                          PIC S9(8)V99.
*>_________________________________________________________________________

PROCEDURE DIVISION.
   
    SORT SORT-FILE
       ON ASCENDING KEY SORT-PAID-DATE 
       USING VOUCHER-FILE
       GIVING WORK-FILE.

     OPEN I-O WORK-FILE.
     OPEN I-O VENDOR-FILE.
     OPEN OUTPUT PRINTER-FILE.

     MOVE 0 TO PAGE-NUMBER.
     MOVE "N" TO W-END-OF-FILE.

     PERFORM PRINT-HEADINGS.     

     PERFORM READ-WORK-NEXT-RECORD.
     PERFORM READ-WORK-NEXT-RECORD UNTIL
                                   (WORK-PAID-DATE NOT EQUAL ZEROS AND WORK-DEDUCTIBLE = "Y")
                                                             OR
                                                        END-OF-FILE.

     IF END-OF-FILE
        MOVE "NO DEDUCTIBLE PAID VOUCHERS IN THE FILE !" TO PRINTER-RECORD
        WRITE PRINTER-RECORD BEFORE ADVANCING 1
     ELSE
        MOVE 0 TO GRAND-TOTAL
        PERFORM PRINT-ALL-VOUCHERS-BY-PAID-DATE UNTIL END-OF-FILE

   *>------------- Print Grand Total -------------------

         MOVE SPACES                     TO CONTROL-BREAK
         MOVE "Grand Total"              TO D-DESCRIPTION
         MOVE GRAND-TOTAL TO D-TOTAL

         MOVE SPACES               TO PRINTER-RECORD
         WRITE PRINTER-RECORD BEFORE ADVANCING 1

         MOVE CONTROL-BREAK        TO PRINTER-RECORD
         WRITE PRINTER-RECORD.
    *>-------------------------------------------------------- 

     PERFORM FINALIZE-PAGE.

     CLOSE WORK-FILE.
     CLOSE VENDOR-FILE.
     CLOSE PRINTER-FILE.

     EXIT PROGRAM.

     STOP RUN.
*>_________________________________________________________________________

PRINT-ALL-VOUCHERS-BY-PAID-DATE.

*>--------------"Zero" Totals --------------------------------

   MOVE 0        TO CURRENT-PAID-DATE-TOTAL.

*>------------- Save breaking information -----------------------

   MOVE WORK-PAID-DATE TO CURRENT-PAID-DATE.

*>--------------------------------------------------------------

   PERFORM PRINT-A-RECORD UNTIL 
                               WORK-PAID-DATE NOT = CURRENT-PAID-DATE
                                                OR
                                           END-OF-FILE.

*>------------- Print Total -------------------
   MOVE "TOTAL THRU "            TO D-DESCRIPTION.

   COMPUTE DUMMY-DATE-MM-DD-CCYY-12 = CURRENT-PAID-DATE * 10000.0001.
   MOVE DUMMY-DATE-MM-DD-CCYY-12 TO DUMMY-DATE-MM-DD-CCYY-8.
   MOVE DUMMY-DATE-MM-DD-CCYY-8  TO D-DATE-REFERENCE.

   MOVE CURRENT-PAID-DATE-TOTAL        TO D-TOTAL.

   MOVE SPACES                 TO PRINTER-RECORD.
   WRITE PRINTER-RECORD BEFORE ADVANCING 1.
   MOVE CONTROL-BREAK          TO PRINTER-RECORD.
   WRITE PRINTER-RECORD BEFORE ADVANCING 2.
   ADD 3 TO W-PRINTED-LINES.      

*>------------- Acumulate into the superior hierarchical item -----------

   ADD CURRENT-PAID-DATE-TOTAL TO GRAND-TOTAL.

*>_________________________________________________________________________

PRINT-A-RECORD. 

       IF PAGE-FULL            
          PERFORM FINALIZE-PAGE
          PERFORM PRINT-HEADINGS. 

       MOVE WORK-NUMBER   TO D-WORK-NUMBER.
       MOVE WORK-PAID-AMOUNT   TO D-WORK-PAID-AMOUNT.
       MOVE WORK-INVOICE  TO D-WORK-INVOICE.
       MOVE WORK-FOR      TO D-WORK-FOR.

       MOVE WORK-VENDOR   TO VENDOR-NUMBER.
       MOVE "Y" TO W-FOUND-VENDOR-RECORD.
       PERFORM LOOK-FOR-VENDOR-RECORD.

       IF FOUND-VENDOR-RECORD
          MOVE VENDOR-NAME       TO D-VENDOR-NAME
       ELSE
          MOVE "** Not found **" TO D-VENDOR-NAME.

       COMPUTE DUMMY-DATE-MM-DD-CCYY-12 = WORK-PAID-DATE * 10000.0001
       MOVE DUMMY-DATE-MM-DD-CCYY-12 TO DUMMY-DATE-MM-DD-CCYY-8
       MOVE DUMMY-DATE-MM-DD-CCYY-8  TO FORMATTED-DATE-MM-DD-CCYY

       ADD WORK-PAID-AMOUNT TO CURRENT-PAID-DATE-TOTAL.

       MOVE DETAIL-1 TO PRINTER-RECORD.
       WRITE PRINTER-RECORD BEFORE ADVANCING 1.
       MOVE DETAIL-2 TO PRINTER-RECORD.
       WRITE PRINTER-RECORD BEFORE ADVANCING 1.
       ADD 2 TO W-PRINTED-LINES.      

     PERFORM READ-WORK-NEXT-RECORD.
     PERFORM READ-WORK-NEXT-RECORD UNTIL
                                   (WORK-PAID-DATE NOT EQUAL ZEROS AND WORK-DEDUCTIBLE = "Y")
                                                             OR
                                                        END-OF-FILE.
*>_________________________________________________________________________

COPY "PL-LOOK-FOR-VENDOR-RECORD.CBL".
COPY "PLSORT.CBL".
*>_________________________________________________________________________




