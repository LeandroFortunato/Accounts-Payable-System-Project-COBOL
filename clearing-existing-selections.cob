IDENTIFICATION DIVISION.
PROGRAM-ID. clearing-existing-selections.
ENVIRONMENT DIVISION.
   INPUT-OUTPUT SECTION.
      FILE-CONTROL.

      COPY "SLVOUCH.CBL".

DATA DIVISION.
   FILE SECTION.

      COPY "FDVOUCH.CBL".

   WORKING-STORAGE SECTION.

      COPY "wscase01.cbl".

      01 W-VALID-ANSWER                       PIC X.
         88 VALID-ANSWER                  VALUE "Y","N".
         88 CLEARING-ALL-IS-CONFIRMED     VALUE "Y".   

      01 W-END-OF-FILE  		      PIC X.
         88 END-OF-FILE     		  VALUE "Y".

      77 DUMMY                                PIC X.
      77 MSG-CONFIRMATION                     PIC X(79).
      77 TOTAL-RECORDS-CHANGED                PIC 9(7).
      77 FORMAT-TOTAL-RECORDS-CHANGED         PIC ZZZZZZ9.
*>_________________________________________________________________________

PROCEDURE DIVISION. 
  
   PERFORM CLEAR-SCREEN.

    MOVE "DO YOU WANT ME TO CLEAR ALL THE SELECTIONS THAT HAVE BEEN MADE ? (Y/N)" TO MSG-CONFIRMATION.
    PERFORM CONFIRM-EXECUTION. *> force first loop
    PERFORM CONFIRM-EXECUTION UNTIL VALID-ANSWER.

   IF CLEARING-ALL-IS-CONFIRMED  
      OPEN I-O VOUCHER-FILE
      MOVE ZEROS TO TOTAL-RECORDS-CHANGED
      PERFORM READ-VOUCHER-NEXT-RECORD
      PERFORM CLEAR-IF-SELECTED-READ-NEXT UNTIL END-OF-FILE
   
      MOVE TOTAL-RECORDS-CHANGED TO FORMAT-TOTAL-RECORDS-CHANGED
      PERFORM CLEAR-SCREEN
      DISPLAY FORMAT-TOTAL-RECORDS-CHANGED 
              " VOUCHER(S) CLEARED OF PAYMENT ! <ENTER> TO CONTINUE"
         ACCEPT DUMMY  
      CLOSE VOUCHER-FILE.

EXIT PROGRAM.
STOP RUN.
*>________________________________________________________________________

CLEAR-IF-SELECTED-READ-NEXT.

   IF VOUCHER-PAID-DATE EQUAL ZEROS *> Not paid yet
      IF VOUCHER-SELECTED = "Y"
         MOVE "N" TO VOUCHER-SELECTED
         ADD 1 TO TOTAL-RECORDS-CHANGED
         REWRITE VOUCHER-RECORD
            INVALID KEY
               SUBTRACT 1 FROM TOTAL-RECORDS-CHANGED
               DISPLAY "*** ERROR RE-WRITING THE VOUCHER ! *** <ENTER> TO CONTINUE"
               ACCEPT DUMMY.
   
   PERFORM READ-VOUCHER-NEXT-RECORD.   
*>_________________________________________________________________________

COPY "PLGENERAL.CBL".
COPY "READ-VOUCHER-NEXT-RECORD.CBL".
*>_________________________________________________________________________




