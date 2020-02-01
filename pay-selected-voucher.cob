IDENTIFICATION DIVISION.
PROGRAM-ID. pay-selected-voucher.
ENVIRONMENT DIVISION.
  INPUT-OUTPUT SECTION.
   FILE-CONTROL.

      COPY "SLVOUCH.CBL".
      COPY "SLCONTRL.CBL".       
      COPY "SLVND02.CBL".

DATA DIVISION.
   FILE SECTION.

      COPY "FDVOUCH.CBL".
      COPY "FDCONTRL.CBL".       
      COPY "FDVND02.CBL".

   WORKING-STORAGE SECTION.

     COPY "wscase01.cbl".
     COPY "wsdate.cbl".

      01 W-SHOW-ALL-THE-FIELDS             PIC X.
         88 SHOW-ALL-THE-FIELDS           VALUE "Y".

      01 W-FOUND-VENDOR-RECORD             PIC X.
         88 FOUND-VENDOR-RECORD           VALUE "Y".

      01 W-FOUND-VOUCHER-RECORD            PIC X.
         88 FOUND-VOUCHER-RECORD          VALUE "Y".

      01 W-ERROR-WRITING                   PIC X.
         88 ERROR-WRITING                 VALUE "Y".

      01 W-VALID-ANSWER                    PIC X.
         88 VALID-ANSWER                  VALUE "Y","N".
         88 QUIT-IS-CONFIRMED             VALUE "Y".
         88 PAYMENT-IS-CONFIRMED          VALUE "Y".
         88 RE-OPENING-IS-CONFIRMED       VALUE "Y".
         88 VOUCHER-PAID-TODAY            VALUE "Y".
         88 VOUCHER-PAID-TOTAL-AMOUNT     VALUE "Y".
         88 GENERATE-BALANCE              VALUE "Y".
         88 PROCESS-AS-PAID               VALUE "Y".

      01 W-ERROR-R-W-NEW-VOUCHER-NUMBER    PIC X.
         88 ERROR-R-W-NEW-VOUCHER-NUMBER  VALUE "Y".

      01 W-DAY-AND-TIME-RIGHT-NOW.
         05 W-DAY-TODAY                   PIC 9(8).
         05 FILLER                        PIC X(1).
         05 W-PIECE-OF-TIME-NOW           PIC 9(5). 
         05 FILLER                        PIC X(7).

      01 VOUCHER-CHECK-NUMBER             PIC 9(5).
         88 VALID-CHECK-NUMBER            VALUE 0 THROUGH 99999.

      77 VOUCHER-FORMATTED-DATE           PIC 99/99/9999.
      77 VOUCHER-MM-YY-CCYY               PIC 9(8).
      77 VOUCHER-FORMATTED-AMOUNT         PIC ZZ,ZZZ,ZZ9.99-.
      77 DUMMY                            PIC X.
      77 DUMMY-FOR-DATE-12                PIC 9(12).
      77 MSG-CONFIRMATION                 PIC X(75).
      77 MSG-AFTER-SAVING                 PIC X(75).
      77 MSG-OPTION                       PIC X(25).
      77 BK-NEW-VOUCHER-RECORD-INFORMED   PIC X(103).
*>_________________________________________________________________________

PROCEDURE DIVISION.

   OPEN I-O VOUCHER-FILE.
   OPEN I-O VENDOR-FILE.

   PERFORM CLEAR-SCREEN.

   MOVE "MARK AS PAID" TO MSG-OPTION
   PERFORM GET-AN-EXISTANT-VOUCHER-NUMBER.
   PERFORM CONFIRM-SELECTION-GET-ANOTHER UNTIL 
                                       VOUCHER-NUMBER EQUAL ZEROS.


   CLOSE VOUCHER-FILE.
   CLOSE VENDOR-FILE.

   EXIT PROGRAM.

   STOP RUN.
*>____________________________________________________________________

CONFIRM-SELECTION-GET-ANOTHER.

     MOVE "Y" TO W-SHOW-ALL-THE-FIELDS. 
     PERFORM DISPLAY-VOUCHER-RECORD.      


     IF VOUCHER-PAID-DATE NOT EQUAL ZEROS
        MOVE  "*** VOUCHER MARKED AS PAID ALREADY ! ***  SHOULD I RE-OPEN IT ?" 
          TO  MSG-CONFIRMATION         

        PERFORM CONFIRM-EXECUTION *> force first loop
        PERFORM CONFIRM-EXECUTION UNTIL VALID-ANSWER

        PERFORM CONTINUE-CHECK-IF-RE-OPEN-IT

     ELSE
      
         MOVE "DO YOU CONFIRM TO MARK THIS VOUCHER AS PAID ?  <Y/N>"
           TO  MSG-CONFIRMATION     

         PERFORM CONFIRM-EXECUTION *> force first loop
         PERFORM CONFIRM-EXECUTION UNTIL VALID-ANSWER       

         IF PAYMENT-IS-CONFIRMED
  
            MOVE "WAS THIS VOUCHER PAID TODAY ?" TO MSG-CONFIRMATION
            PERFORM CONFIRM-EXECUTION *> force first loop
            PERFORM CONFIRM-EXECUTION UNTIL VALID-ANSWER

            PERFORM CONTINUE-VOUCHER-PAID-DATE

            IF NOT QUIT-IS-CONFIRMED
               PERFORM DISPLAY-VOUCHER-RECORD
               PERFORM GET-VOUCHER-CHECK-NUMBER 
               PERFORM GET-VOUCHER-CHECK-NUMBER 
                          UNTIL VALID-CHECK-NUMBER
                             OR QUIT-IS-CONFIRMED

               IF NOT QUIT-IS-CONFIRMED
                  PERFORM DISPLAY-VOUCHER-RECORD 
                  MOVE "WAS IT A TOTAL PAYMENT ?" TO MSG-CONFIRMATION
                  PERFORM CONFIRM-EXECUTION *> force first loop
                  PERFORM CONFIRM-EXECUTION UNTIL VALID-ANSWER

                  IF VOUCHER-PAID-TOTAL-AMOUNT
                     MOVE VOUCHER-AMOUNT TO VOUCHER-PAID-AMOUNT
                     MOVE "PAYMENT IS CONFIRMED ! <ENTER> TO CONTINUE" TO MSG-AFTER-SAVING   
                     PERFORM SAVE-CHANGES-ON-THE-VOUCHER
                  ELSE
                     PERFORM GET-VOUCHER-PAID-AMOUNT
                                  UNTIL VOUCHER-PAID-AMOUNT NOT EQUAL ZEROS 
                                     OR QUIT-IS-CONFIRMED.

     PERFORM GET-AN-EXISTANT-VOUCHER-NUMBER.
*>_________________________________________________________________________

CONTINUE-CHECK-IF-RE-OPEN-IT.

   IF RE-OPENING-IS-CONFIRMED
      MOVE ZEROS TO VOUCHER-PAID-DATE
      MOVE ZEROS TO VOUCHER-PAID-AMOUNT
      MOVE ZEROS TO VOUCHER-CHECK-NO
      MOVE "THE VOUCHER IS NOW RE-OPENED ! <ENTER> TO CONTINUE" TO MSG-AFTER-SAVING
      PERFORM SAVE-CHANGES-ON-THE-VOUCHER.
*>_________________________________________________________________________

CONTINUE-VOUCHER-PAID-DATE.

   IF VOUCHER-PAID-TODAY      
      MOVE FUNCTION CURRENT-DATE TO W-DAY-AND-TIME-RIGHT-NOW
      MOVE W-DAY-TODAY           TO VOUCHER-PAID-DATE
      MOVE "N" TO W-VALID-ANSWER  *> quit (QUIT-IS-CONFIRMED)  
   ELSE 
      PERFORM GET-VOUCHER-DATE-OF-PAYMENT
                    UNTIL VOUCHER-PAID-DATE NOT EQUAL ZEROS 
                       OR QUIT-IS-CONFIRMED.
*>_________________________________________________________________________

GET-VOUCHER-DATE-OF-PAYMENT.

   MOVE "INFORM DATE OF PAYMENT: (MM-DD-YYYY)" TO GDTV-DATE-HEADING. 
   MOVE 1900 TO GDTV-FIRST-YEAR-VALID.                        
   MOVE 2100 TO GDTV-LAST-YEAR-VALID.         
   MOVE "Y"  TO GDTV-ACCEPT-EMPTY-DATE.           
 
   PERFORM GET-VALI-DATE-RETURN-GDTV-DATE.
   
   MOVE GDTV-DATE TO VOUCHER-PAID-DATE.

   IF VOUCHER-PAID-DATE EQUAL ZEROS
      DISPLAY "DATE OF PAYMENT MUST BE INFORMED !"
       PERFORM CONFIRM-IF-WANT-TO-QUIT.
*>_________________________________________________________________________

GET-VOUCHER-CHECK-NUMBER.

   DISPLAY "INFORM CHECK NUMBER (<ENTER> FOR CASH)".
   ACCEPT VOUCHER-CHECK-NUMBER.
  
   IF NOT VALID-CHECK-NUMBER
      DISPLAY "INVALID CHECK NUMBER !"
      PERFORM CONFIRM-IF-WANT-TO-QUIT
   ELSE
      MOVE VOUCHER-CHECK-NUMBER TO VOUCHER-CHECK-NO.

*>_________________________________________________________________________

GET-VOUCHER-PAID-AMOUNT.

   DISPLAY "INFORM AMOUNT THAT WAS PAID: ".
   ACCEPT VOUCHER-FORMATTED-AMOUNT.
  
   MOVE VOUCHER-FORMATTED-AMOUNT TO VOUCHER-PAID-AMOUNT.

   IF VOUCHER-PAID-AMOUNT EQUAL ZEROS
      DISPLAY "AMOUNT PAID MUST BE INFORMED !"
      PERFORM CONFIRM-IF-WANT-TO-QUIT   
   ELSE

      IF VOUCHER-PAID-AMOUNT NOT LESS THAN VOUCHER-AMOUNT
         MOVE VOUCHER-AMOUNT TO VOUCHER-FORMATTED-AMOUNT
         DISPLAY "AMOUNT PAID WAS INFORMED TO BE LESS THAN " VOUCHER-FORMATTED-AMOUNT " <ENTER> TO CONTINUE !" 
         MOVE ZEROS TO VOUCHER-PAID-AMOUNT 
         PERFORM CONFIRM-IF-WANT-TO-QUIT 
      ELSE
 
         MOVE " " TO MSG-AFTER-SAVING   
         PERFORM SAVE-CHANGES-ON-THE-VOUCHER

         COMPUTE VOUCHER-FORMATTED-AMOUNT = VOUCHER-AMOUNT - VOUCHER-PAID-AMOUNT
         
         STRING "MARKED AS PAID! GENERATE NEW VOUCHER FOR THE BALANCE "
                VOUCHER-FORMATTED-AMOUNT 
                " ? <Y/N>" 
           INTO MSG-CONFIRMATION      
         END-STRING
     
         PERFORM CONFIRM-EXECUTION *> force first loop
         PERFORM CONFIRM-EXECUTION UNTIL VALID-ANSWER

         IF GENERATE-BALANCE
            MOVE VOUCHER-FORMATTED-AMOUNT TO VOUCHER-AMOUNT 
            MOVE ZEROS TO VOUCHER-PAID-DATE
            MOVE ZEROS TO VOUCHER-PAID-AMOUNT
            MOVE ZEROS TO VOUCHER-CHECK-NO
            MOVE "N" TO VOUCHER-SELECTED
            PERFORM ADD-NEW-VOUCHER-AND-SHOW-VALUE.
*>_________________________________________________________________________

ADD-NEW-VOUCHER-AND-SHOW-VALUE.

   MOVE SPACES TO MSG-AFTER-SAVING.
   PERFORM ADD-NEW-VOUCHER.
 
   MOVE VOUCHER-AMOUNT TO VOUCHER-FORMATTED-AMOUNT.

   STRING  "NEW VOUCHER (" 
           VOUCHER-NUMBER 
           ") GENERATED FOR " 
           VOUCHER-FORMATTED-AMOUNT
           "! <ENTER> TO CONTINUE" 
      INTO MSG-AFTER-SAVING        
   END-STRING.

   DISPLAY MSG-AFTER-SAVING.
   ACCEPT DUMMY.
   MOVE "Y" TO W-VALID-ANSWER. *> To force a quit of the calling loop
*>_________________________________________________________________________

COPY "PLGENERAL.CBL".
COPY "PLVOUCHER.CBL".
COPY "PL-LOOK-FOR-VENDOR-RECORD.CBL".
COPY "ADD-NEW-VOUCHER.CBL".
COPY "PLDATE.CBL".
*>_________________________________________________________________________

