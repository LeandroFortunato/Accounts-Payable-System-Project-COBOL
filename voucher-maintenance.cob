IDENTIFICATION DIVISION.
PROGRAM-ID. voucher-maintenance.
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

      01 W-VOUCHER-MENU-OPTION             PIC 9.
         88 VALID-VOUCHER-MENU-OPTION     VALUE  0 THROUGH 4.  
         88 SHOW-ALL-THE-FIELDS           VALUE 2 THROUGH 4.

      01 W-FOUND-VENDOR-RECORD             PIC X.
         88 FOUND-VENDOR-RECORD           VALUE "Y".

      01 W-FOUND-VOUCHER-RECORD             PIC X.
         88 FOUND-VOUCHER-RECORD           VALUE "Y".

      01 W-ERROR-WRITING                   PIC X.
         88 ERROR-WRITING                 VALUE "Y".

      01 W-ERROR-R-W-NEW-VOUCHER-NUMBER    PIC X.
         88 ERROR-R-W-NEW-VOUCHER-NUMBER  VALUE "Y".

      01 W-VALID-ANSWER                    PIC X.
         88 VALID-ANSWER                  VALUE "Y","N".
         88 QUIT-IS-CONFIRMED             VALUE "Y".
         88 DELETING-IS-CONFIRMED         VALUE "Y".

      01 ENTRY-RECORD-FIELD         PIC 9.
         88 VALID-FIELD             VALUE 1 THROUGH 7.

      77 VOUCHER-FORMATTED-DATE           PIC 99/99/9999.
      77 VOUCHER-MM-YY-CCYY               PIC 9(8).
      77 VOUCHER-FORMATTED-AMOUNT         PIC ZZ,ZZZ,ZZ9.99-. 

      77 DUMMY                            PIC X.
      77 DUMMY-FOR-DATE-12                PIC 9(12).
      77 MSG-CONFIRMATION                 PIC X(60).
      77 MSG-AFTER-SAVING                 PIC X(60).
      77 MSG-OPTION                       PIC X(07).
      77 BK-NEW-VOUCHER-RECORD-INFORMED   PIC X(103).
*>_________________________________________________________________________

PROCEDURE DIVISION.

   OPEN I-O VOUCHER-FILE.
   OPEN I-O VENDOR-FILE.


   PERFORM GET-MENU-OPTION *> force first pass
   PERFORM GET-MENU-OPTION UNTIL 
                               W-VOUCHER-MENU-OPTION EQUAL ZERO 
                            OR VALID-VOUCHER-MENU-OPTION.

   PERFORM DO-OPTIONS UNTIL 
                               W-VOUCHER-MENU-OPTION EQUAL ZERO 
   CLOSE VOUCHER-FILE.
   CLOSE VENDOR-FILE.

   EXIT PROGRAM.

   STOP RUN.
*>_________________________________________________________________________

GET-MENU-OPTION.
         
         PERFORM CLEAR-SCREEN.
         DISPLAY "                          VOUCHER-FILE MAINTENANCE PROGRAM".
         DISPLAY " "
         DISPLAY "                              -------------------------".
         DISPLAY "                              | 1 - ADD VOUCHER        |".
         DISPLAY "                              | 2 - CHANGE VOUCHER     |".
         DISPLAY "                              | 3 - LOOK UP VOUCHER    |".
         DISPLAY "                              | 4 - DELETE VOUCHER     |".
         DISPLAY "                              | 0 - EXIT               |".
         DISPLAY "                              --------------------------".
         DISPLAY " "
         DISPLAY "                           - CHOOSE AN OPTION FROM MENU:  ".
         PERFORM JUMP-LINE 11 TIMES. 
         ACCEPT W-VOUCHER-MENU-OPTION.

        IF W-VOUCHER-MENU-OPTION EQUAL ZERO
           DISPLAY "PROGRAM TERMINATED !"
        ELSE
           IF NOT VALID-VOUCHER-MENU-OPTION    
              DISPLAY "INVALID OPTION ! <ENTER> TO CONTINUE"
              ACCEPT DUMMY.
*>_________________________________________________________________________

DO-OPTIONS.

   PERFORM CLEAR-SCREEN.

   IF W-VOUCHER-MENU-OPTION = 1
      PERFORM ADD-MODULE.

   IF W-VOUCHER-MENU-OPTION = 2
      MOVE "CHANGE " TO MSG-OPTION
      PERFORM CHANGE-MODULE.

   IF W-VOUCHER-MENU-OPTION = 3
      MOVE "LOOK UP" TO MSG-OPTION
      PERFORM INQUIRY-MODULE.

   IF W-VOUCHER-MENU-OPTION = 4
      MOVE "DELETE " TO MSG-OPTION
      PERFORM DELETE-MODULE.

   PERFORM GET-MENU-OPTION. *> force first pass
   PERFORM GET-MENU-OPTION UNTIL 
                               W-VOUCHER-MENU-OPTION EQUAL ZERO 
                            OR VALID-VOUCHER-MENU-OPTION.
*>_________________________________________________________________________

ADD-MODULE.
   
   PERFORM ADD-REC-GET-ANOTHER-NUMBER. *> force first loop to initialize QUIT-IS-CONFIRMED
   PERFORM ADD-REC-GET-ANOTHER-NUMBER UNTIL QUIT-IS-CONFIRMED. 
*>_________________________________________________________________________

ADD-REC-GET-ANOTHER-NUMBER.

   MOVE SPACES TO VOUCHER-RECORD.
   MOVE ZEROS  TO VOUCHER-NUMBER.
   MOVE ZEROS  TO VOUCHER-VENDOR.
   MOVE ZEROS  TO VOUCHER-AMOUNT.
   MOVE ZEROS  TO VOUCHER-DATE.
   MOVE ZEROS  TO VOUCHER-DUE.
   MOVE ZEROS TO VOUCHER-PAID-AMOUNT.
   MOVE ZEROS TO VOUCHER-PAID-DATE.
   MOVE ZEROS TO VOUCHER-CHECK-NO.
   MOVE "N" TO VOUCHER-SELECTED *> Default is "N"

   PERFORM GET-FIELDS.    

   IF VOUCHER-RECORD NOT EQUAL SPACES *> quit from get-fields
      MOVE "THE NEW VOUCHER HAS BEEN ADDED ! <ENTER> TO CONTINUE" TO MSG-AFTER-SAVING 
      PERFORM ADD-NEW-VOUCHER.
*>_________________________________________________________________________

GET-FIELDS.
   
   MOVE "N" TO W-VALID-ANSWER.  *> quit (QUIT-IS-CONFIRMED) 
   MOVE "N" TO W-FOUND-VENDOR-RECORD.
   
   PERFORM GET-VOUCHER-VENDOR 
                         UNTIL (VOUCHER-VENDOR NOT EQUAL ZEROS 
                                          AND 
                                    FOUND-VENDOR-RECORD)

                            OR QUIT-IS-CONFIRMED.

   PERFORM GET-VOUCHER-INVOICE
                         UNTIL VOUCHER-INVOICE NOT EQUAL SPACES 
                            OR QUIT-IS-CONFIRMED.

   PERFORM GET-VOUCHER-FOR
                         UNTIL VOUCHER-FOR NOT EQUAL SPACES 
                            OR QUIT-IS-CONFIRMED.

   PERFORM GET-VOUCHER-AMOUNT
                         UNTIL VOUCHER-AMOUNT NOT EQUAL ZEROS 
                            OR QUIT-IS-CONFIRMED.

   PERFORM GET-VOUCHER-DATE
                         UNTIL VOUCHER-DATE NOT EQUAL ZEROS 
                            OR QUIT-IS-CONFIRMED.

   PERFORM GET-VOUCHER-DUE
                         UNTIL VOUCHER-DUE NOT EQUAL ZEROS 
                            OR QUIT-IS-CONFIRMED.

   PERFORM GET-VOUCHER-DEDUCTIBLE
                         UNTIL VOUCHER-DEDUCTIBLE EQUAL "Y"
                            OR VOUCHER-DEDUCTIBLE EQUAL "N"
                            OR QUIT-IS-CONFIRMED.

   IF VOUCHER-VENDOR NOT EQUAL ZEROS AND QUIT-IS-CONFIRMED *> quit after vendor was informed 
      DISPLAY "OPERATION CANCELED ! <ENTER> TO CONTINUE" 
      ACCEPT DUMMY. 

   IF QUIT-IS-CONFIRMED 
      MOVE SPACES TO VOUCHER-RECORD.
      
*>_________________________________________________________________________

GET-VOUCHER-VENDOR.

   PERFORM SHOW-ALL-INFORM-ALREADY-GIVEN.

   DISPLAY "1) INFORM VENDOR - <ENTER> TO RETURN"
   ACCEPT VOUCHER-VENDOR.

   IF VOUCHER-VENDOR EQUAL ZEROS
      MOVE "Y" TO W-VALID-ANSWER  *> quit (QUIT-IS-CONFIRMED) 
   ELSE
       MOVE VOUCHER-VENDOR TO VENDOR-NUMBER
       MOVE "Y" TO W-FOUND-VENDOR-RECORD
       PERFORM LOOK-FOR-VENDOR-RECORD

        IF NOT FOUND-VENDOR-RECORD
           DISPLAY "*** VENDOR NOT FOUND IN VENDOR-FILE ! ***  <ENTER> TO CONTINUE"
           MOVE ZEROS TO VOUCHER-VENDOR
           ACCEPT DUMMY.
*>_________________________________________________________________________

GET-VOUCHER-INVOICE.

   PERFORM SHOW-ALL-INFORM-ALREADY-GIVEN.

   DISPLAY "2) INFORM INVOICE: ". 
   ACCEPT VOUCHER-INVOICE.

   IF VOUCHER-INVOICE EQUAL SPACES
      DISPLAY "INVOICE MUST BE INFORMED !"
       PERFORM CONFIRM-IF-WANT-TO-QUIT
   ELSE
       INSPECT VOUCHER-INVOICE CONVERTING LOWER-ALPHA TO UPPER-ALPHA.
*>_________________________________________________________________________

GET-VOUCHER-FOR.

   PERFORM SHOW-ALL-INFORM-ALREADY-GIVEN.

   DISPLAY "3) INFORM VOUCHER PURPOSE: ". 
   ACCEPT VOUCHER-FOR.

   IF VOUCHER-FOR EQUAL SPACES
      DISPLAY "PURPOSE MUST BE INFORMED !"
       PERFORM CONFIRM-IF-WANT-TO-QUIT
   ELSE
       INSPECT VOUCHER-FOR CONVERTING LOWER-ALPHA TO UPPER-ALPHA.
*>_________________________________________________________________________

GET-VOUCHER-AMOUNT.

   PERFORM SHOW-ALL-INFORM-ALREADY-GIVEN.

   DISPLAY "4) INFORM AMOUNT: ". 
   ACCEPT VOUCHER-AMOUNT.

   IF VOUCHER-AMOUNT EQUAL ZEROS
      DISPLAY "AMOUNT MUST BE INFORMED !"
       PERFORM CONFIRM-IF-WANT-TO-QUIT.
*>_________________________________________________________________________

GET-VOUCHER-DATE.

   PERFORM SHOW-ALL-INFORM-ALREADY-GIVEN.

   MOVE "5) INFORM RECEPTION DATE: (MM-DD-YYYY)" TO GDTV-DATE-HEADING. 
   MOVE 1900 TO GDTV-FIRST-YEAR-VALID.                        
   MOVE 2100 TO GDTV-LAST-YEAR-VALID.         
   MOVE "Y"  TO GDTV-ACCEPT-EMPTY-DATE.           
 
   PERFORM GET-VALI-DATE-RETURN-GDTV-DATE.
   
   MOVE GDTV-DATE TO VOUCHER-DATE.

   IF VOUCHER-DATE EQUAL ZEROS
      DISPLAY "RECEPTION DATE MUST BE INFORMED !"
       PERFORM CONFIRM-IF-WANT-TO-QUIT.
*>_________________________________________________________________________

GET-VOUCHER-DUE.

   PERFORM SHOW-ALL-INFORM-ALREADY-GIVEN.

   MOVE "6) INFORM DUE: (MM-DD-YYYY)" TO GDTV-DATE-HEADING. 
   MOVE 1900 TO GDTV-FIRST-YEAR-VALID.                        
   MOVE 2100 TO GDTV-LAST-YEAR-VALID.         
   MOVE "Y"  TO GDTV-ACCEPT-EMPTY-DATE.           

   PERFORM GET-VALI-DATE-RETURN-GDTV-DATE.
   
   MOVE GDTV-DATE TO VOUCHER-DUE.

   IF VOUCHER-DUE EQUAL ZEROS
      DISPLAY "DUE MUST BE INFORMED !"
       PERFORM CONFIRM-IF-WANT-TO-QUIT.

*>_________________________________________________________________________

GET-VOUCHER-DEDUCTIBLE.

   PERFORM SHOW-ALL-INFORM-ALREADY-GIVEN.

   DISPLAY "7) DEDUCTIBLE: (Y/N)". 
   ACCEPT VOUCHER-DEDUCTIBLE.

   IF VOUCHER-DEDUCTIBLE EQUAL SPACES
      DISPLAY "ONLY (Y/N) IS ACCEPTED !"
       PERFORM CONFIRM-IF-WANT-TO-QUIT
   ELSE
      INSPECT VOUCHER-DEDUCTIBLE CONVERTING LOWER-ALPHA TO UPPER-ALPHA.
*>_________________________________________________________________________

SHOW-ALL-INFORM-ALREADY-GIVEN.

   PERFORM DISPLAY-VOUCHER-RECORD.
   DISPLAY "INSERT THE FOLLOWING INFORMATION FOR THE NEW VOUCHER: ". 
   DISPLAY " ".
*>_________________________________________________________________________

CHANGE-MODULE.

   PERFORM GET-AN-EXISTANT-VOUCHER-NUMBER.
   PERFORM GET-RECORD-AND-CHANGE UNTIL 
                                      VOUCHER-NUMBER EQUAL ZEROS.
*>_________________________________________________________________________


GET-RECORD-AND-CHANGE.

       PERFORM DISPLAY-VOUCHER-RECORD.
       PERFORM ASK-USER-WHICH-FIELD-TO-CHANGE.
       PERFORM CHANGE-SAVE-GET-ANOTHER-FIELD 
                                      UNTIL ENTRY-RECORD-FIELD EQUAL ZERO.

       PERFORM GET-AN-EXISTANT-VOUCHER-NUMBER.
*>_________________________________________________________________________

CHANGE-SAVE-GET-ANOTHER-FIELD.

     MOVE "N" TO W-VALID-ANSWER.  *> not to quit (QUIT-IS-CONFIRMED)

     IF ENTRY-RECORD-FIELD = 1
        PERFORM GET-VOUCHER-VENDOR
        PERFORM GET-VOUCHER-VENDOR 
                         UNTIL (VOUCHER-VENDOR NOT EQUAL ZEROS 
                                          AND 
                                    FOUND-VENDOR-RECORD)

                            OR QUIT-IS-CONFIRMED.
    
     IF ENTRY-RECORD-FIELD = 2
        PERFORM GET-VOUCHER-INVOICE
        PERFORM GET-VOUCHER-INVOICE
                         UNTIL VOUCHER-INVOICE NOT EQUAL SPACES 
                            OR QUIT-IS-CONFIRMED.

     IF ENTRY-RECORD-FIELD = 3
        PERFORM GET-VOUCHER-FOR
        PERFORM GET-VOUCHER-FOR
                         UNTIL VOUCHER-FOR NOT EQUAL SPACES 
                            OR QUIT-IS-CONFIRMED.

     IF ENTRY-RECORD-FIELD = 4
        PERFORM GET-VOUCHER-AMOUNT
        PERFORM GET-VOUCHER-AMOUNT
                         UNTIL VOUCHER-AMOUNT NOT EQUAL ZEROS 
                            OR QUIT-IS-CONFIRMED.

     IF ENTRY-RECORD-FIELD = 5
         PERFORM GET-VOUCHER-DATE
         PERFORM GET-VOUCHER-DATE
                         UNTIL VOUCHER-DATE NOT EQUAL ZEROS 
                            OR QUIT-IS-CONFIRMED.

     IF ENTRY-RECORD-FIELD = 6
        PERFORM GET-VOUCHER-DUE
        PERFORM GET-VOUCHER-DUE
                         UNTIL VOUCHER-DUE NOT EQUAL ZEROS 
                            OR QUIT-IS-CONFIRMED.

     IF ENTRY-RECORD-FIELD = 7
        PERFORM GET-VOUCHER-DEDUCTIBLE
        PERFORM GET-VOUCHER-DEDUCTIBLE
                         UNTIL VOUCHER-DEDUCTIBLE EQUAL "Y"
                            OR VOUCHER-DEDUCTIBLE EQUAL "N"
                            OR QUIT-IS-CONFIRMED.

     IF QUIT-IS-CONFIRMED
        DISPLAY "OPERATION CANCELED ! <ENTER> TO CONTINUE" 
        ACCEPT DUMMY
        MOVE 0 TO ENTRY-RECORD-FIELD *> to force quit and get another VOUCHER  number
     ELSE
        MOVE "THE CHANGES HAVE BEEN SAVED ! <ENTER> TO CONTINUE" TO MSG-AFTER-SAVING                
        PERFORM SAVE-CHANGES-ON-THE-VOUCHER
        PERFORM ASK-USER-WHICH-FIELD-TO-CHANGE.
*>_________________________________________________________________________

GET-A-FIELD-TO-CHANGE.

     DISPLAY "INFORM A FIELD TO CHANGE 1 TO 7 (<ENTER> TO RETURN)".
     ACCEPT ENTRY-RECORD-FIELD.
     
     IF ENTRY-RECORD-FIELD NOT EQUAL ZERO
        IF NOT VALID-FIELD
           DISPLAY "INVALID FIELD !".
*>_________________________________________________________________________

DELETE-MODULE.

       PERFORM GET-AN-EXISTANT-VOUCHER-NUMBER.
       PERFORM GET-REC-DELETE-SEARCH-ANOTHER UNTIL 
                                         VOUCHER-NUMBER EQUAL ZEROS.
*>_________________________________________________________________________

GET-REC-DELETE-SEARCH-ANOTHER.

     PERFORM DISPLAY-VOUCHER-RECORD.

     IF VOUCHER-PAID-DATE NOT EQUAL ZEROS
        DISPLAY " *** NOT PERMITTED ! THIS VOUCHER HAS BEEN PAID! ***  <ENTER> TO CONTINUE"
        ACCEPT DUMMY
     ELSE
        MOVE "DO YOU CONFIRM DELETING THIS RECORD ?" TO MSG-CONFIRMATION
        PERFORM ASK-USER-IF-WANT-TO-COMPLETE

        IF DELETING-IS-CONFIRMED
           DISPLAY "DELETING..."
           DELETE VOUCHER-FILE RECORD
              INVALID KEY 
                 DISPLAY "ERROR WHILE DELETING THE RECORD ! <ENTER> TO CONTINUE"
                 ACCEPT DUMMY.
     
     PERFORM GET-AN-EXISTANT-VOUCHER-NUMBER.
*>_________________________________________________________________________

INQUIRY-MODULE.

       PERFORM GET-VOUCHER-NUMBER-AND-SEARCH. *> force first pass
       PERFORM GET-VOUCHER-NUMBER-AND-SEARCH UNTIL 
                                         VOUCHER-NUMBER EQUAL ZEROS
                                      OR FOUND-VOUCHER-RECORD.

       PERFORM GET-RECORD-SHOW-AND-GET-ANOTHER UNTIL 
                                         VOUCHER-NUMBER EQUAL ZEROS.
*>_________________________________________________________________________

GET-RECORD-SHOW-AND-GET-ANOTHER.

     PERFORM DISPLAY-VOUCHER-RECORD.
     
     PERFORM GET-VOUCHER-NUMBER-AND-SEARCH. *> force first pass
     PERFORM GET-VOUCHER-NUMBER-AND-SEARCH UNTIL 
                                         VOUCHER-NUMBER EQUAL ZEROS
                                      OR FOUND-VOUCHER-RECORD.
*>_________________________________________________________________________

COPY "PLDATE.CBL".
COPY "PLGENERAL.CBL".
COPY "PLVOUCHER.CBL".
COPY "PL-ASK-USER-WHICH-FIELD-TO-CHANGE.CBL".
COPY "PL-LOOK-FOR-VENDOR-RECORD.CBL".
COPY "ADD-NEW-VOUCHER.CBL".

*>_________________________________________________________________________

