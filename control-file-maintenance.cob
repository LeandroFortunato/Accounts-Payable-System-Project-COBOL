IDENTIFICATION DIVISION.
PROGRAM-ID. control-file-maintenance.
ENVIRONMENT DIVISION.
   INPUT-OUTPUT SECTION.
      FILE-CONTROL.

         COPY "SLCONTRL.CBL".

DATA DIVISION.
   FILE SECTION.

      COPY "FDCONTRL.CBL".

   WORKING-STORAGE SECTION.

      COPY "wscase01.cbl".

     01 W-CONTROL-MENU-OPTION          PIC 9.
         88 VALID-CONTROL-MENU-OPTION  VALUE  0 THROUGH 2.  

     01 W-ERROR-READING-CTRL-FILE      PIC X.
        88 ERROR-READING-CTRL-FILE VALUE "Y".

     01 W-ERROR-WRITING-CTRL-FILE      PIC X.
        88 ERROR-WRITING-CTRL-FILE VALUE "Y".

     01 W-VALID-ANSWER                 PIC X.
        88 VALID-ANSWER            VALUE "Y","N".
        88 SAVING-IS-CONFIRMED     VALUE "Y".

     01 ENTRY-RECORD-FIELD         PIC 9.
         88 VALID-FIELD             VALUE 0 THROUGH 1.

     77 MSG-CONFIRMATION           PIC X(45).
     77 ENTRY-CONTROL-LAST-VOUCHER PIC 9(5).
     77 DUMMY                      PIC X.
*>_________________________________________________________________________

PROCEDURE DIVISION.

   PERFORM GET-MENU-OPTION *> force first pass
   PERFORM GET-MENU-OPTION UNTIL 
                               W-CONTROL-MENU-OPTION EQUAL ZERO 
                            OR VALID-CONTROL-MENU-OPTION.

   PERFORM DO-OPTIONS UNTIL W-CONTROL-MENU-OPTION EQUAL ZERO. *> will be performed just if option was 1 or 2

   EXIT PROGRAM.

   STOP RUN.
*>_________________________________________________________________________

GET-MENU-OPTION.
         
         PERFORM CLEAR-SCREEN.
         DISPLAY "                         CONTROL-FILE MAINTENANCE PROGRAM".
         DISPLAY " "
         DISPLAY "                          ------------------------------".
         DISPLAY "                          | 1 - DISPLAY CONTROL-FILE   |".
         DISPLAY "                          | 2 - CHANGE CONTROL-FILE    |".
         DISPLAY "                          | 0 - EXIT                   |".
         DISPLAY "                          ------------------------------".
         DISPLAY " "
         DISPLAY "                          - CHOOSE AN OPTION FROM MENU:  ".
         PERFORM JUMP-LINE 13 TIMES. 
         ACCEPT W-CONTROL-MENU-OPTION
 
        IF W-CONTROL-MENU-OPTION EQUAL ZERO
           DISPLAY "PROGRAM TERMINATED !"
        ELSE
           IF NOT VALID-CONTROL-MENU-OPTION    
              DISPLAY "INVALID OPTION ! <ENTER> TO CONTINUE"
              ACCEPT DUMMY.
*>_________________________________________________________________________

DO-OPTIONS.

   OPEN I-O CONTROL-FILE. 
   PERFORM READ-CONTROL-FILE-ONLY-RECORD.

   IF ERROR-READING-CTRL-FILE
      DISPLAY "*** ERROR READING CONTROL-FILE !!! *** <ENTER> TO CONTINUE"
      ACCEPT DUMMY
   ELSE
      IF W-CONTROL-MENU-OPTION = 1
         PERFORM DISPLAY-CONTROL-RECORD 
         DISPLAY "<ENTER> TO RETURN" 
         ACCEPT DUMMY
      ELSE *> Option is 2
         PERFORM ASK-USER-WHICH-FIELD-TO-CHANGE
         PERFORM CHANGE-SAVE-GET-ANOTHER-FIELD 
                                            UNTIL ENTRY-RECORD-FIELD EQUAL ZERO.
   CLOSE CONTROL-FILE. 

   PERFORM GET-MENU-OPTION. *> force first pass
   PERFORM GET-MENU-OPTION UNTIL 
                               W-CONTROL-MENU-OPTION EQUAL ZERO 
                            OR VALID-CONTROL-MENU-OPTION.
*>_________________________________________________________________________

DISPLAY-CONTROL-RECORD.
   
   PERFORM CLEAR-SCREEN.
   DISPLAY "1) LAST VOUCHER ISSUED...: " CONTROL-LAST-VOUCHER.
   PERFORM JUMP-LINE 16 TIMES.

ASK-USER-WHICH-FIELD-TO-CHANGE.

        PERFORM GET-A-FIELD-TO-CHANGE. *> force first pass
        PERFORM GET-A-FIELD-TO-CHANGE 
                                      UNTIL ENTRY-RECORD-FIELD EQUAL ZERO 
                                         OR VALID-FIELD. 
*>_________________________________________________________________________

GET-A-FIELD-TO-CHANGE.

   PERFORM READ-CONTROL-FILE-ONLY-RECORD. 

   IF ERROR-READING-CTRL-FILE
      DISPLAY "*** ERROR READING CONTROL-FILE !!! *** <ENTER> TO CONTINUE"
      ACCEPT DUMMY
      MOVE 0 TO ENTRY-RECORD-FIELD *> to force a quit
   ELSE 
      PERFORM DISPLAY-CONTROL-RECORD
      DISPLAY "INFORM A FIELD TO CHANGE 1 TO 1 (<ENTER> TO RETURN)"
      ACCEPT ENTRY-RECORD-FIELD
     
      IF ENTRY-RECORD-FIELD NOT EQUAL ZERO
         IF NOT VALID-FIELD
            DISPLAY "INVALID FIELD ! <ENTER> TO CONTINUE"
            ACCEPT DUMMY.
*>_________________________________________________________________________

CHANGE-SAVE-GET-ANOTHER-FIELD.

   IF ENTRY-RECORD-FIELD = 1
      PERFORM GET-SAVE-CONTROL-LAST-VOUCHER.
*>   IF ENTRY-RECORD-FIELD =  2

   PERFORM ASK-USER-WHICH-FIELD-TO-CHANGE.
*>_________________________________________________________________________

GET-SAVE-CONTROL-LAST-VOUCHER.

   DISPLAY "INFORM A NEW VALUE FOR LAST VOUCHER ISSUED: "
   ACCEPT ENTRY-CONTROL-LAST-VOUCHER
   
   IF ENTRY-CONTROL-LAST-VOUCHER
          NOT EQUAL 
      CONTROL-LAST-VOUCHER 

      DISPLAY "NEW VALUE INFORMED: " ENTRY-CONTROL-LAST-VOUCHER
      
      MOVE "DO YOU WANT TO SAVE THE NEW VALUE ? <Y/N>" TO MSG-CONFIRMATION
      PERFORM ASK-USER-IF-WANT-TO-COMPLETE

      IF SAVING-IS-CONFIRMED        
         MOVE ENTRY-CONTROL-LAST-VOUCHER TO CONTROL-LAST-VOUCHER
         PERFORM SAVE-CHANGES-ON-CONTROL-RECORD.
*>_________________________________________________________________________

SAVE-CHANGES-ON-CONTROL-RECORD.

   PERFORM WRITE-CONTROL-FILE-ONLY-RECORD.

   IF ERROR-WRITING-CTRL-FILE
      DISPLAY "*** ERROR DURING REWRITING OF CONTROL-FILE ! ***"
      ACCEPT DUMMY
   ELSE
      DISPLAY "THE NEW VALUE WAS SAVED ! <ENTER> TO CONTINUE"
      ACCEPT DUMMY.
*>_________________________________________________________________________

READ-CONTROL-FILE-ONLY-RECORD.
   
   MOVE 1 TO CONTROL-KEY.
   MOVE "N" TO W-ERROR-READING-CTRL-FILE.
   
   READ CONTROL-FILE RECORD
      INVALID KEY 
         MOVE "Y" TO W-ERROR-READING-CTRL-FILE.
*>_________________________________________________________________________

WRITE-CONTROL-FILE-ONLY-RECORD.
   
   MOVE "N" TO W-ERROR-WRITING-CTRL-FILE.
 
   REWRITE CONTROL-RECORD
      INVALID KEY 
         MOVE "Y" TO W-ERROR-WRITING-CTRL-FILE.
*>_________________________________________________________________________

COPY "PLGENERAL.CBL".
*>_________________________________________________________________________

