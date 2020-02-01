IDENTIFICATION DIVISION.
PROGRAM-ID. state-code-maintenance.
ENVIRONMENT DIVISION.
  INPUT-OUTPUT SECTION.
   FILE-CONTROL.

      COPY "SLSTATE.CBL".

DATA DIVISION.
   FILE SECTION.

      COPY "FDSTATE.CBL".

   WORKING-STORAGE SECTION.

      COPY "wscase01.cbl".

      01 W-OPTION                         PIC 9.
         88 VALID-OPTION                  VALUE  1 THROUGH 4.             

      01 ENTRY-STATE-CODE.
         05 FILLER                       PIC X.
            88 VALID-STATE-FIRST-CHAR    VALUE "A" THROUGH "Z",
                                               "a" THROUGH "z".
         05 FILLER                       PIC X.
            88 VALID-STATE-SECOND-CHAR   VALUE "A" THROUGH "Z",
                                               "a" THROUGH "z".
      01 W-FOUND-RECORD                  PIC X.
         88 FOUND-RECORD                 VALUE "Y".

      01 W-ERROR-WRITING                 PIC X.
         88 ERROR-WRITING                VALUE "Y".

      01 W-VALID-ANSWER                  PIC X.
         88 VALID-ANSWER                 VALUE "Y","N","y","n".
         88 DELETING-IS-CONFIRMED        VALUE "Y","y". 

      01 ENTRY-CHARACTER      PIC X. 
         88 ONLY-VALID-CHARACTERS        VALUE "A" THROUGH "Z",
                                               "a" THROUGH "z",
                                               "'",
                                               SPACE.
      77 ENTRY-STATE-NAME                PIC X(20).
   
      77 POSITION-OF-CHARACTER           PIC 99.

      77 MSG-OPTION                      PIC X(06).
      77 MSG-CONFIRMATION                 PIC X(40).
      77 DUMMY                           PIC X.
*>_________________________________________________________________________

PROCEDURE DIVISION.

   OPEN I-O STATE-FILE.

   PERFORM GET-MENU-OPTION *> force first pass
   PERFORM GET-MENU-OPTION UNTIL 
                               W-OPTION EQUAL ZERO 
                            OR VALID-OPTION.

   PERFORM DO-OPTIONS UNTIL 
                               W-OPTION EQUAL ZERO 
   CLOSE STATE-FILE.

   EXIT PROGRAM.

   STOP RUN.
*>_________________________________________________________________________

JUMP-2-LINES-AND-PAUSE.

PERFORM JUMP-LINE.
PERFORM JUMP-LINE.
ACCEPT DUMMY.
*>_________________________________________________________________________

GET-MENU-OPTION.

        PERFORM CLEAR-SCREEN.
        DISPLAY "                        STATE-CODE FILE MAINTENANCE PROGRAM". 
        DISPLAY " "
        DISPLAY "                             ---------------------------".
        DISPLAY "                             | 1 - LOOK UP STATE-CODE  |".
        DISPLAY "                             | 2 - ADD STATE-CODE      |".
        DISPLAY "                             | 3 - CHANGE STATE-CODE   |".
        DISPLAY "                             | 4 - DELETE STATE-CODE   |".
        DISPLAY "                             |                         |".
        DISPLAY "                             | 0 - EXIT                |".
        DISPLAY "                             ---------------------------".
        DISPLAY " "
        DISPLAY "                           - CHOOSE AN OPTION FROM MENU:".
        PERFORM JUMP-LINE 10 TIMES. 
        ACCEPT W-OPTION
 
        IF W-OPTION EQUAL ZERO
           DISPLAY "PROGRAM TERMINATED !"
        ELSE
           IF NOT VALID-OPTION    
              DISPLAY "INVALID OPTION ! <ENTER> TO CONTINUE"
              PERFORM JUMP-2-LINES-AND-PAUSE.
*>_________________________________________________________________________

DO-OPTIONS.

   PERFORM CLEAR-SCREEN.

   IF W-OPTION = 1
      MOVE "SEARCH" TO MSG-OPTION
      PERFORM INQUIRY-MODULE.

   IF W-OPTION = 2
      MOVE "ADD" TO MSG-OPTION
      PERFORM ADD-MODULE.

   IF W-OPTION = 3
      MOVE "CHANGE" TO MSG-OPTION
      PERFORM CHANGE-MODULE.

   IF W-OPTION = 4
      MOVE "DELETE" TO MSG-OPTION
      PERFORM DELETE-MODULE.

   PERFORM GET-MENU-OPTION. *> force first pass
   PERFORM GET-MENU-OPTION UNTIL 
                               W-OPTION EQUAL ZERO 
                            OR VALID-OPTION.
*>_________________________________________________________________________
    
GET-EXISTANT-ST-CODE-FROM-USER.

       PERFORM GET-STATE-CODE-TO-SEARCH. *> force first pass
       PERFORM GET-STATE-CODE-TO-SEARCH UNTIL 
                                         ENTRY-STATE-CODE EQUAL SPACES
                                      OR FOUND-RECORD.
*>_________________________________________________________________________

GET-STATE-CODE-TO-SEARCH.

     PERFORM ASK-THE-STATE-CODE-TO-THE-USER.

     IF ENTRY-STATE-CODE NOT EQUAL SPACES
        INSPECT ENTRY-STATE-CODE 
                         CONVERTING LOWER-ALPHA 
                                 TO UPPER-ALPHA
        MOVE ENTRY-STATE-CODE TO STATE-CODE
        PERFORM LOOK-FOR-RECORD
        PERFORM JUMP-LINE
        IF NOT FOUND-RECORD
           DISPLAY "STATE CODE NOT FOUND ! <ENTER> TO CONTINUE"
           PERFORM JUMP-2-LINES-AND-PAUSE
        ELSE
           DISPLAY "------- RECORD FOUND ! ----------".
*>_________________________________________________________________________

ASK-THE-STATE-CODE-TO-THE-USER.

     MOVE "Y" TO W-FOUND-RECORD.
     DISPLAY "INFORM THE STATE CODE TO " MSG-OPTION " (<ENTER> FOR MENU)".
     ACCEPT ENTRY-STATE-CODE.
*>_________________________________________________________________________

GET-THE-NEW-ST-CODE-FROM-USER.

       PERFORM GET-A-VALID-NEW-STATE-CODE *> force a first pass
       PERFORM GET-A-VALID-NEW-STATE-CODE UNTIL 
                                     ENTRY-STATE-CODE EQUAL SPACES *> quit
                              OR NOT FOUND-RECORD. *> it's not duplicity
*>_________________________________________________________________________

GET-A-VALID-NEW-STATE-CODE.

   PERFORM ASK-THE-STATE-CODE-TO-THE-USER.

   IF ENTRY-STATE-CODE NOT EQUAL SPACES
      IF NOT VALID-STATE-FIRST-CHAR OR 
         NOT VALID-STATE-SECOND-CHAR
    
         DISPLAY "2 LETTERS HAVE TO BE INFORMED ! <ENTER> TO CONTINUE"
         PERFORM JUMP-2-LINES-AND-PAUSE
      ELSE 
         INSPECT ENTRY-STATE-CODE 
                         CONVERTING LOWER-ALPHA 
                                 TO UPPER-ALPHA
         MOVE ENTRY-STATE-CODE TO STATE-CODE
         PERFORM LOOK-FOR-RECORD 
         IF FOUND-RECORD
            PERFORM DISPLAY-STATE-CODE-RECORD
            DISPLAY "( ****** STATE CODE ALREADY EXISTS ! ****** )  <ENTER> TO CONTINUE"
            PERFORM JUMP-2-LINES-AND-PAUSE
         ELSE 
            PERFORM JUMP-LINE 3 TIMES
            DISPLAY "NEW STATE CODE.: " STATE-CODE.
*>_________________________________________________________________________
 
GET-THE-NEW-ST-NAME-FROM-USER.

       PERFORM GET-A-VALID-NEW-STATE-NAME. *> force first pass
       PERFORM GET-A-VALID-NEW-STATE-NAME
                                UNTIL ENTRY-STATE-NAME EQUAL SPACES *> a quit 
                                   OR ONLY-VALID-CHARACTERS. 
*>_________________________________________________________________________

GET-A-VALID-NEW-STATE-NAME.

     MOVE "N" TO W-ERROR-WRITING.
     MOVE SPACE  TO ENTRY-CHARACTER. 
     MOVE 1      TO POSITION-OF-CHARACTER.
     DISPLAY "INFORM THE NEW NAME TO " MSG-OPTION " (<ENTER> FOR MENU)".
     ACCEPT ENTRY-STATE-NAME.

     IF ENTRY-STATE-NAME NOT EQUAL SPACES *> a quit
        PERFORM CHECK-CHARACTER UNTIL POSITION-OF-CHARACTER > 20 
                               OR NOT ONLY-VALID-CHARACTERS
        IF NOT ONLY-VALID-CHARACTERS
           DISPLAY "ONLY LETTERS (A-Z) AND (') ARE ACCEPTED ! <ENTER> TO CONTINUE"
           PERFORM JUMP-2-LINES-AND-PAUSE
        ELSE
           INSPECT ENTRY-STATE-NAME 
                           CONVERTING LOWER-ALPHA 
                                   TO UPPER-ALPHA.
*>_________________________________________________________________________

CHECK-CHARACTER.
     
     UNSTRING ENTRY-STATE-NAME
        INTO ENTRY-CHARACTER
             WITH POINTER POSITION-OF-CHARACTER
     END-UNSTRING.
*>_________________________________________________________________________

INQUIRY-MODULE.

       PERFORM GET-EXISTANT-ST-CODE-FROM-USER.
       PERFORM GET-REC-SHOW-GET-ANOTHER-CODE UNTIL 
                                         ENTRY-STATE-CODE EQUAL SPACES.
*>_________________________________________________________________________

GET-REC-SHOW-GET-ANOTHER-CODE.

     PERFORM DISPLAY-STATE-CODE-RECORD.
     DISPLAY "<ENTER> TO CONTINUE".
     PERFORM JUMP-2-LINES-AND-PAUSE.
     
     PERFORM GET-EXISTANT-ST-CODE-FROM-USER.
*>_________________________________________________________________________

LOOK-FOR-RECORD.

   READ STATE-FILE RECORD
        INVALID KEY
           MOVE "N" TO W-FOUND-RECORD.
*>_________________________________________________________________________

DISPLAY-STATE-CODE-RECORD.
     
     PERFORM JUMP-LINE.    
     DISPLAY "-------------------------------------------".
     DISPLAY "STATE CODE: " STATE-CODE " - " STATE-NAME.
     DISPLAY "-------------------------------------------".
     PERFORM JUMP-LINE.
 *>_________________________________________________________________________

ADD-MODULE.

   PERFORM GET-THE-NEW-ST-CODE-FROM-USER.
   PERFORM ADD-REC-GET-ANOTHER-STATE-CODE UNTIL 
                                           ENTRY-STATE-CODE EQUAL SPACES. *> quit
*>_________________________________________________________________________

ADD-REC-GET-ANOTHER-STATE-CODE.

    PERFORM GET-THE-NEW-ST-NAME-FROM-USER.

    IF ENTRY-STATE-NAME NOT EQUAL SPACES *> not a quit
       MOVE ENTRY-STATE-NAME TO STATE-NAME
       PERFORM WRITE-RECORD
       PERFORM JUMP-LINE
       IF ERROR-WRITING
          DISPLAY "ERROR WHILE WRITING THE RECORD ! <ENTER> TO CONTINUE"
          PERFORM JUMP-2-LINES-AND-PAUSE
       ELSE 
          DISPLAY "----- RECORD ADDED! -----"
          PERFORM DISPLAY-STATE-CODE-RECORD 
          PERFORM JUMP-LINE 3 TIMES.

    PERFORM GET-THE-NEW-ST-CODE-FROM-USER.
*>_________________________________________________________________________
  
WRITE-RECORD.

   WRITE STATE-RECORD
       INVALID KEY 
          MOVE "Y" TO W-ERROR-WRITING.  
*>_________________________________________________________________________

REWRITE-THE-RECORD.

    REWRITE STATE-RECORD
         INVALID KEY 
               MOVE "Y" TO W-ERROR-WRITING.  
*>_________________________________________________________________________

CHANGE-MODULE.

   PERFORM GET-EXISTANT-ST-CODE-FROM-USER.
   PERFORM GET-RECORD-AND-CHANGE UNTIL 
                                      ENTRY-STATE-CODE EQUAL SPACES.
*>_________________________________________________________________________

GET-RECORD-AND-CHANGE.

        PERFORM DISPLAY-STATE-CODE-RECORD.
        PERFORM GET-THE-NEW-ST-NAME-FROM-USER.

        IF ENTRY-STATE-NAME NOT EQUAL SPACES *> not a quit
           MOVE ENTRY-STATE-NAME TO STATE-NAME
           PERFORM REWRITE-THE-RECORD
           PERFORM JUMP-LINE    
           IF ERROR-WRITING
              DISPLAY "ERROR WHILE REWRITING THE RECORD ! <ENTER> TO CONTINUE"
              PERFORM JUMP-2-LINES-AND-PAUSE
           ELSE 
              DISPLAY "----- RECORD CHANGED! -----"
              PERFORM DISPLAY-STATE-CODE-RECORD
              PERFORM JUMP-LINE 3 TIMES.
 
        PERFORM GET-EXISTANT-ST-CODE-FROM-USER.
*>_________________________________________________________________________

DELETE-MODULE.

       PERFORM GET-EXISTANT-ST-CODE-FROM-USER.
       PERFORM GET-REC-DELETE-SEARCH-ANOTHER UNTIL 
                                         ENTRY-STATE-CODE EQUAL SPACES.
*>_________________________________________________________________________

GET-REC-DELETE-SEARCH-ANOTHER.

     PERFORM DISPLAY-STATE-CODE-RECORD.

     MOVE "DO YOU CONFIRM DELETING THIS RECORD ?" TO MSG-CONFIRMATION.
     PERFORM ASK-USER-IF-WANT-TO-COMPLETE.

     IF DELETING-IS-CONFIRMED
        DISPLAY "DELETING..."
        DELETE STATE-FILE RECORD
            INVALID KEY 
                 DISPLAY "ERROR WHILE DELETING THE RECORD ! <ENTER> TO CONTINUE"
                 PERFORM JUMP-2-LINES-AND-PAUSE.
 
      PERFORM GET-EXISTANT-ST-CODE-FROM-USER.
*>_________________________________________________________________________

COPY "PLGENERAL.CBL".
*>_________________________________________________________________________




