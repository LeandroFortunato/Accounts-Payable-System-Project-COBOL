IDENTIFICATION DIVISION.
PROGRAM-ID. vendor-maintenance.
ENVIRONMENT DIVISION.
  INPUT-OUTPUT SECTION.
   FILE-CONTROL.

      COPY "SLVND02.CBL".
      COPY "SLSTATE.CBL".

DATA DIVISION.
   FILE SECTION.

      COPY "FDVND02.CBL".
      COPY "FDSTATE.CBL".

   WORKING-STORAGE SECTION.

     COPY "wscase01.cbl".

     01 W-VENDOR-MENU-OPTION          PIC 9.
         88 VALID-VENDOR-MENU-OPTION  VALUE  0 THROUGH 8.  
         88 CLOSE-FILES             VALUE 1,2,6,7,8.
         88 OPEN-FILES              VALUE 1,2,6,7,8.

      01 ENTRY-VENDOR-NUMBER        PIC 9(5).
         88 VALID-NUMBER            VALUE 1 THROUGH 99999.

      01 W-FOUND-VENDOR-RECORD      PIC X.
         88 FOUND-VENDOR-RECORD     VALUE "Y".

      01 W-FOUND-STATE-RECORD       PIC X.
         88 FOUND-STATE-RECORD      VALUE "Y".

      01 W-ERROR-WRITING                 PIC X.
         88 ERROR-WRITING                VALUE "Y".

      01 ENTRY-RECORD-FIELD         PIC 9.
         88 VALID-FIELD             VALUE 1 THROUGH 8.

      01 W-VALID-ANSWER             PIC X.
         88 VALID-ANSWER            VALUE "Y","N".
         88 DELETING-IS-CONFIRMED   VALUE "Y".
         88 QUIT-IS-CONFIRMED       VALUE "Y".

      77 MSG-CONFIRMATION           PIC X(40).
      77 MSG-OPTION                 PIC X(06).
      77 DUMMY                      PIC X.
*>_________________________________________________________________________

PROCEDURE DIVISION.

   OPEN I-O VENDOR-FILE.
   OPEN I-O STATE-FILE.

   PERFORM GET-MENU-OPTION *> force first pass
   PERFORM GET-MENU-OPTION UNTIL 
                               W-VENDOR-MENU-OPTION EQUAL ZERO 
                            OR VALID-VENDOR-MENU-OPTION.

   PERFORM DO-OPTIONS UNTIL 
                               W-VENDOR-MENU-OPTION EQUAL ZERO 
   CLOSE STATE-FILE.
   CLOSE VENDOR-FILE.

   EXIT PROGRAM.

   STOP RUN.
*>_________________________________________________________________________

GET-MENU-OPTION.
         
         PERFORM CLEAR-SCREEN.
         DISPLAY "                          VENDOR-FILE MAINTENANCE PROGRAM".
         DISPLAY " "
         DISPLAY "                          --------------------------------".
         DISPLAY "                          | 1 - LOOK UP VENDOR BY NUMBER  |".
         DISPLAY "                          | 2 - LOOK UP VENDOR BY NAME    |".
         DISPLAY "                          | 3 - ADD VENDOR                |".
         DISPLAY "                          | 4 - CHANGE VENDOR             |".
         DISPLAY "                          | 5 - DELETE VENDOR             |".
         DISPLAY "                          | 6 - PRINT VENDORS BY NUMBER   |".
         DISPLAY "                          | 7 - PRINT VENDORS BY NAME     |".
         DISPLAY "                          | 8 - DISPLAY ALL VENDORS       |".
         DISPLAY "                          | 0 - EXIT                      |".
         DISPLAY "                          --------------------------------".
         DISPLAY " "
         DISPLAY "                           - CHOOSE AN OPTION FROM MENU:  ".
         PERFORM JUMP-LINE 7 TIMES. 
         ACCEPT W-VENDOR-MENU-OPTION.

        IF W-VENDOR-MENU-OPTION EQUAL ZERO
           DISPLAY "PROGRAM TERMINATED !"
        ELSE
           IF NOT VALID-VENDOR-MENU-OPTION    
              DISPLAY "INVALID OPTION ! <ENTER> TO CONTINUE"
              ACCEPT DUMMY.
*>_________________________________________________________________________

DO-OPTIONS.

   PERFORM CLEAR-SCREEN.

   IF CLOSE-FILES
      CLOSE STATE-FILE
      CLOSE VENDOR-FILE.                        

   IF W-VENDOR-MENU-OPTION = 1
      CALL "inquiry-vendor-by-number".

   IF W-VENDOR-MENU-OPTION = 2
      CALL "inquiry-vendor-by-name".

   IF W-VENDOR-MENU-OPTION = 3
      MOVE "ADD" TO MSG-OPTION
      PERFORM ADD-MODULE.

   IF W-VENDOR-MENU-OPTION = 4
      MOVE "CHANGE" TO MSG-OPTION
      PERFORM CHANGE-MODULE.

   IF W-VENDOR-MENU-OPTION = 5
      MOVE "DELETE" TO MSG-OPTION
      PERFORM DELETE-MODULE.

   IF W-VENDOR-MENU-OPTION = 6  
      DISPLAY "PRINTING VENDOR BY NUMBER..."
      CALL "print-vendor-by-number"
      DISPLAY "PRINTING IS DONE ! <ENTER> TO CONTINUE"
      ACCEPT DUMMY.

   IF W-VENDOR-MENU-OPTION = 7
      DISPLAY "PRINTING VENDOR BY NAME..."
      CALL "print-vendor-by-name"
      DISPLAY "PRINTING IS DONE ! <ENTER> TO CONTINUE"
      ACCEPT DUMMY.

   IF W-VENDOR-MENU-OPTION = 8
      CALL "display-vendor-by-number".

   IF OPEN-FILES
      OPEN I-O VENDOR-FILE
      OPEN I-O STATE-FILE.

   PERFORM GET-MENU-OPTION. *> force first pass
   PERFORM GET-MENU-OPTION UNTIL 
                               W-VENDOR-MENU-OPTION EQUAL ZERO 
                            OR VALID-VENDOR-MENU-OPTION.
*>______________________________________________________________________

ASK-USER-FOR-THE-VENDOR-NUMBER.

     MOVE "Y" TO W-FOUND-VENDOR-RECORD.
     DISPLAY "INFORM A VENDOR NUMBER TO " MSG-OPTION " (<ENTER> FOR MENU)".
     ACCEPT ENTRY-VENDOR-NUMBER.
*>_________________________________________________________________________

ASK-USER-FOR-NEW-VENDOR-NUMBER.

   PERFORM GET-A-NEW-VENDOR-NUMBER *> force a first pass
   PERFORM GET-A-NEW-VENDOR-NUMBER UNTIL 
                                     ENTRY-VENDOR-NUMBER EQUAL ZEROS *> quit
                              OR NOT FOUND-VENDOR-RECORD. *> it's not duplicity
*>_________________________________________________________________________

GET-A-NEW-VENDOR-NUMBER.

   PERFORM ASK-USER-FOR-THE-VENDOR-NUMBER.

   IF ENTRY-VENDOR-NUMBER NOT EQUAL ZEROS
      IF NOT VALID-NUMBER    
         DISPLAY "INVALID VENDOR NUMBER ! <ENTER> TO CONTINUE"
         ACCEPT DUMMY
      ELSE 
         MOVE ENTRY-VENDOR-NUMBER TO VENDOR-NUMBER
         PERFORM LOOK-FOR-VENDOR-RECORD 
         IF FOUND-VENDOR-RECORD
            DISPLAY "VENDOR NUMBER ALREADY EXISTS ! <ENTER> TO CONTINUE"
            ACCEPT DUMMY.
*>_________________________________________________________________________

DISPLAY-VENDOR-RECORD.
   
     PERFORM CLEAR-SCREEN.
     DISPLAY "VENDOR.............: " VENDOR-NUMBER.
     DISPLAY "1) NAME............: " VENDOR-NAME.
     DISPLAY "2) ADDRESS - LINE 1: " VENDOR-ADDRESS-1.
     DISPLAY "3) ADDRESS - LINE 2: " VENDOR-ADDRESS-2.    
     DISPLAY "4) CITY............: " VENDOR-CITY.

     MOVE VENDOR-STATE TO STATE-CODE.
     MOVE "Y" TO W-FOUND-STATE-RECORD.
     PERFORM LOOK-FOR-STATE-RECORD.

     IF FOUND-STATE-RECORD
        DISPLAY "5) STATE...........: " VENDOR-STATE " - " STATE-NAME
     ELSE
        DISPLAY "5) STATE...........: " VENDOR-STATE " - ** Not found **".

     DISPLAY "6) ZIP CODE........: " VENDOR-ZIP. 
     DISPLAY "7) CONTACT NAME....: " VENDOR-CONTACT.  
     DISPLAY "8) PHONE NUMBER....: " VENDOR-PHONE.

     PERFORM JUMP-LINE 8 TIMES. 
*>_________________________________________________________________________

ADD-MODULE.

   PERFORM ASK-USER-FOR-NEW-VENDOR-NUMBER.
   PERFORM ADD-REC-GET-ANOTHER-NUMBER UNTIL 
                                           ENTRY-VENDOR-NUMBER EQUAL ZEROS. *> quit
*>_________________________________________________________________________

ADD-REC-GET-ANOTHER-NUMBER.

   MOVE SPACES TO VENDOR-RECORD.
   MOVE ENTRY-VENDOR-NUMBER TO VENDOR-NUMBER.
   DISPLAY "INSERT THE INFORMATION FOR VENDOR NUMBER " VENDOR-NUMBER.

   PERFORM GET-OTHER-FIELDS.     

   IF VENDOR-RECORD NOT EQUAL SPACES *> quit from get-other-fields
      PERFORM WRITE-RECORD
      IF ERROR-WRITING
         DISPLAY "ERROR WHILE WRITING THE RECORD ! <ENTER> TO CONTINUE" 
         ACCEPT DUMMY
      ELSE 
          PERFORM DISPLAY-VENDOR-RECORD
          DISPLAY "----- RECORD ADDED! ----- <ENTER> TO CONTINUE"
          ACCEPT DUMMY.

   PERFORM ASK-USER-FOR-NEW-VENDOR-NUMBER.
*>_________________________________________________________________________

GET-OTHER-FIELDS.
   
   MOVE "N" TO W-VALID-ANSWER.  *> not to quit (QUIT-IS-CONFIRMED)
   MOVE "N" TO W-FOUND-STATE-RECORD.

   PERFORM GET-VENDOR-NAME 
                         UNTIL VENDOR-NAME NOT EQUAL SPACES 
                            OR QUIT-IS-CONFIRMED.

   PERFORM GET-VENDOR-ADDRESS-1-AND-2
                         UNTIL VENDOR-ADDRESS-1 NOT EQUAL SPACES 
                            OR QUIT-IS-CONFIRMED.

   PERFORM GET-VENDOR-CITY
                         UNTIL VENDOR-CITY NOT EQUAL SPACES 
                            OR QUIT-IS-CONFIRMED.

   PERFORM GET-VENDOR-STATE-ZIP-CONTACT
                         UNTIL (VENDOR-STATE NOT EQUAL SPACES AND 
                                FOUND-STATE-RECORD)
                            OR QUIT-IS-CONFIRMED.

   PERFORM GET-VENDOR-PHONE
                         UNTIL VENDOR-PHONE NOT EQUAL SPACES 
                            OR QUIT-IS-CONFIRMED.

   IF QUIT-IS-CONFIRMED
      DISPLAY "OPERATION CANCELED ! <ENTER> TO CONTINUE" 
      ACCEPT DUMMY
      MOVE SPACES TO VENDOR-RECORD.
*>_________________________________________________________________________

GET-VENDOR-NAME.

    DISPLAY "1) INFORM NAME: ".
    ACCEPT VENDOR-NAME.

    IF VENDOR-NAME EQUAL SPACES
       DISPLAY "NAME MUST BE INFORMED !"
       PERFORM CONFIRM-IF-WANT-TO-QUIT
    ELSE
       INSPECT VENDOR-NAME CONVERTING LOWER-ALPHA TO UPPER-ALPHA.
*>_________________________________________________________________________

GET-VENDOR-ADDRESS-1-AND-2.

    PERFORM GET-VENDOR-ADDRESS-1.

    IF VENDOR-ADDRESS-1 EQUAL SPACES
       DISPLAY "ADDRESS MUST BE INFORMED !"
       PERFORM CONFIRM-IF-WANT-TO-QUIT
    ELSE
       PERFORM GET-VENDOR-ADDRESS-2.
*>_________________________________________________________________________

GET-VENDOR-ADDRESS-1.

    DISPLAY "2) INFORM ADDRESS - LINE 1: ". 
    ACCEPT VENDOR-ADDRESS-1.
    INSPECT VENDOR-ADDRESS-1 CONVERTING LOWER-ALPHA TO UPPER-ALPHA.
*>_________________________________________________________________________

GET-VENDOR-ADDRESS-2.

    DISPLAY "3) INFORM ADDRESS - LINE 2: ".
    ACCEPT VENDOR-ADDRESS-2.
    INSPECT VENDOR-ADDRESS-2 CONVERTING LOWER-ALPHA TO UPPER-ALPHA.
*>_________________________________________________________________________

GET-VENDOR-CITY.

    DISPLAY "4) INFORM CITY: ".
    ACCEPT VENDOR-CITY.

    IF VENDOR-CITY EQUAL SPACES
      DISPLAY "CITY MUST BE INFORMED !"
      PERFORM CONFIRM-IF-WANT-TO-QUIT
    ELSE
       INSPECT VENDOR-CITY CONVERTING LOWER-ALPHA TO UPPER-ALPHA.
*>_________________________________________________________________________

GET-VENDOR-STATE-ZIP-CONTACT.

      DISPLAY "5) INFORM STATE: ". 
      ACCEPT VENDOR-STATE.

      IF VENDOR-STATE EQUAL SPACES
         DISPLAY "STATE MUST BE INFORMED !"
         PERFORM CONFIRM-IF-WANT-TO-QUIT
      ELSE
         INSPECT VENDOR-STATE 
                         CONVERTING LOWER-ALPHA
                                 TO UPPER-ALPHA
         MOVE VENDOR-STATE TO STATE-CODE
         MOVE "Y" TO W-FOUND-STATE-RECORD
         PERFORM LOOK-FOR-STATE-RECORD
         IF NOT FOUND-STATE-RECORD
            DISPLAY "STATE NOT FOUND IN THE STATE-FILE ! <ENTER> TO CONTINUE"
            ACCEPT DUMMY
         ELSE
            DISPLAY " - " STATE-NAME
            IF MSG-OPTION = "ADD"   *>  ADD-MODULE OPTION
               PERFORM GET-VENDOR-ZIP
               PERFORM GET-VENDOR-CONTACT.
*>_________________________________________________________________________

GET-VENDOR-ZIP.
      
       DISPLAY "6) INFORM ZIP CODE: ". 
       ACCEPT VENDOR-ZIP.
       INSPECT VENDOR-ZIP CONVERTING LOWER-ALPHA TO UPPER-ALPHA.
*>_________________________________________________________________________

GET-VENDOR-CONTACT.
      
       DISPLAY "7) INFORM CONTACT NAME: ". 
       ACCEPT VENDOR-CONTACT.
       INSPECT VENDOR-CONTACT CONVERTING LOWER-ALPHA TO UPPER-ALPHA.
*>_________________________________________________________________________

GET-VENDOR-PHONE.

       DISPLAY "8) INFORM PHONE NUMBER: ".
       ACCEPT VENDOR-PHONE.

       IF VENDOR-PHONE EQUAL SPACES
          DISPLAY "PHONE MUST BE INFORMED !"
          PERFORM CONFIRM-IF-WANT-TO-QUIT.
*>_________________________________________________________________________

WRITE-RECORD.

   MOVE "N" TO W-ERROR-WRITING.
   WRITE VENDOR-RECORD
       INVALID KEY 
          MOVE "Y" TO W-ERROR-WRITING.  
*>_________________________________________________________________________

CHANGE-MODULE.

   PERFORM GET-AN-EXISTANT-VENDOR-NUMBER.
   PERFORM GET-RECORD-AND-CHANGE UNTIL 
                                      ENTRY-VENDOR-NUMBER EQUAL ZEROS.
*>_________________________________________________________________________

GET-AN-EXISTANT-VENDOR-NUMBER.

       PERFORM GET-VENDOR-NUMBER-AND-SEARCH. *> force first pass
       PERFORM GET-VENDOR-NUMBER-AND-SEARCH UNTIL 
                                         ENTRY-VENDOR-NUMBER EQUAL ZEROS
                                      OR FOUND-VENDOR-RECORD.
*>_________________________________________________________________________

GET-VENDOR-NUMBER-AND-SEARCH.

     PERFORM ASK-USER-FOR-THE-VENDOR-NUMBER.

     IF ENTRY-VENDOR-NUMBER NOT EQUAL ZEROS
        MOVE ENTRY-VENDOR-NUMBER TO VENDOR-NUMBER
        PERFORM LOOK-FOR-VENDOR-RECORD
        IF NOT FOUND-VENDOR-RECORD
           DISPLAY "VENDOR NOT FOUND ! ".
*>_________________________________________________________________________

GET-RECORD-AND-CHANGE.

       PERFORM DISPLAY-VENDOR-RECORD.
       PERFORM ASK-USER-WHICH-FIELD-TO-CHANGE.
       PERFORM CHANGE-SAVE-GET-ANOTHER-FIELD 
                                      UNTIL ENTRY-RECORD-FIELD EQUAL ZERO.

       PERFORM GET-AN-EXISTANT-VENDOR-NUMBER.
*>_________________________________________________________________________

GET-A-FIELD-TO-CHANGE.

     DISPLAY "INFORM A FIELD TO CHANGE 1 TO 8 (<ENTER> TO RETURN)".
     ACCEPT ENTRY-RECORD-FIELD.
     
     IF ENTRY-RECORD-FIELD NOT EQUAL ZERO
        IF NOT VALID-FIELD
           DISPLAY "INVALID FIELD !".
*>_________________________________________________________________________

CHANGE-SAVE-GET-ANOTHER-FIELD.

     DISPLAY "VENDOR: " VENDOR-NUMBER. 

     MOVE "N" TO W-VALID-ANSWER.  *> not to quit (QUIT-IS-CONFIRMED)
     MOVE "N" TO W-FOUND-STATE-RECORD.

     IF ENTRY-RECORD-FIELD = 1
        PERFORM GET-VENDOR-NAME *> force first loop
        PERFORM GET-VENDOR-NAME 
                         UNTIL VENDOR-NAME NOT EQUAL SPACES 
                            OR QUIT-IS-CONFIRMED.
    
     IF ENTRY-RECORD-FIELD = 2
        PERFORM GET-VENDOR-ADDRESS-1 *> force first loop
        PERFORM GET-VENDOR-ADDRESS-1
                         UNTIL VENDOR-ADDRESS-1 NOT EQUAL SPACES 
                            OR QUIT-IS-CONFIRMED.

     IF ENTRY-RECORD-FIELD = 3
        PERFORM GET-VENDOR-ADDRESS-2. 

     IF ENTRY-RECORD-FIELD = 4
        PERFORM GET-VENDOR-CITY *> force first loop
        PERFORM GET-VENDOR-CITY
                         UNTIL VENDOR-CITY NOT EQUAL SPACES 
                            OR QUIT-IS-CONFIRMED.


     IF ENTRY-RECORD-FIELD = 5
         PERFORM GET-VENDOR-STATE-ZIP-CONTACT *> force first loop
         PERFORM GET-VENDOR-STATE-ZIP-CONTACT *> just VENDOR-STATE for module 3
                         UNTIL (VENDOR-STATE NOT EQUAL SPACES AND 
                                FOUND-STATE-RECORD)
                            OR QUIT-IS-CONFIRMED.

     IF ENTRY-RECORD-FIELD = 6
        PERFORM GET-VENDOR-ZIP.

     IF ENTRY-RECORD-FIELD = 7 
        PERFORM GET-VENDOR-CONTACT.

     IF ENTRY-RECORD-FIELD = 8
        PERFORM GET-VENDOR-PHONE *> force first loop
        PERFORM GET-VENDOR-PHONE
                         UNTIL VENDOR-PHONE NOT EQUAL SPACES 
                            OR QUIT-IS-CONFIRMED.

     IF QUIT-IS-CONFIRMED
        DISPLAY "OPERATION CANCELED ! <ENTER> TO CONTINUE" 
        ACCEPT DUMMY
        MOVE 0 TO ENTRY-RECORD-FIELD *> to force quit and get another vendor number
     ELSE
        PERFORM SAVE-CHANGES-ON-THE-RECORD
        PERFORM ASK-USER-WHICH-FIELD-TO-CHANGE.
*>_________________________________________________________________________

DELETE-MODULE.

       PERFORM GET-AN-EXISTANT-VENDOR-NUMBER.
       PERFORM GET-REC-DELETE-SEARCH-ANOTHER UNTIL 
                                         ENTRY-VENDOR-NUMBER EQUAL ZEROS.
*>_________________________________________________________________________

GET-REC-DELETE-SEARCH-ANOTHER.

     PERFORM DISPLAY-VENDOR-RECORD.

     MOVE "DO YOU CONFIRM DELETING THIS RECORD ?" TO MSG-CONFIRMATION.
     PERFORM ASK-USER-IF-WANT-TO-COMPLETE.

     IF DELETING-IS-CONFIRMED
        DISPLAY "DELETING..."
        DELETE VENDOR-FILE RECORD
            INVALID KEY 
                 DISPLAY "ERROR WHILE DELETING THE RECORD ! <ENTER> TO CONTINUE"
                    ACCEPT DUMMY.
     
     PERFORM GET-AN-EXISTANT-VENDOR-NUMBER.
*>_________________________________________________________________________

SAVE-CHANGES-ON-THE-RECORD.

     PERFORM REWRITE-VENDOR-RECORD.
      
     IF ERROR-WRITING
        PERFORM DISPLAY-VENDOR-RECORD *> User can see how the record is now
        DISPLAY "ERROR WHILE REWRITING VENDOR RECORD ! <ENTER> TO CONTINUE"
        ACCEPT DUMMY
     ELSE
        PERFORM DISPLAY-VENDOR-RECORD
        DISPLAY "----- VENDOR RECORD CHANGED! ----- <ENTER> TO CONTINUE"
        ACCEPT DUMMY.
*>_________________________________________________________________________

REWRITE-VENDOR-RECORD.

   MOVE "N" TO W-ERROR-WRITING. 

   REWRITE VENDOR-RECORD
            INVALID KEY 
          MOVE "Y" TO W-ERROR-WRITING. 
*>_________________________________________________________________________

COPY "PLGENERAL.CBL".
COPY "PL-LOOK-FOR-VENDOR-RECORD.CBL".
COPY "PL-LOOK-FOR-STATE-RECORD.CBL".
COPY "PL-ASK-USER-WHICH-FIELD-TO-CHANGE.CBL".
*>_________________________________________________________________________

