IDENTIFICATION DIVISION.
PROGRAM-ID. accounts-payable-system.
ENVIRONMENT DIVISION.
DATA DIVISION.

   WORKING-STORAGE SECTION.

     01 W-MAIN-MENU-OPTION          PIC 9.
         88 VALID-MAIN-MENU-OPTION  VALUE  0 THROUGH 4.  

     77 DUMMY                      PIC X.
*>_________________________________________________________________________

PROCEDURE DIVISION.

   PERFORM GET-MENU-OPTION *> force first pass
   PERFORM GET-MENU-OPTION UNTIL 
                               W-MAIN-MENU-OPTION EQUAL ZERO 
                            OR VALID-MAIN-MENU-OPTION.

   PERFORM DO-OPTIONS UNTIL 
                               W-MAIN-MENU-OPTION EQUAL ZERO 

   STOP RUN.
*>_________________________________________________________________________

GET-MENU-OPTION.
         
         PERFORM CLEAR-SCREEN.
         DISPLAY "                              ACCOUNTS PAYABLE SYSTEM".
         DISPLAY " "
         DISPLAY "                          --------------------------------".
         DISPLAY "                          | 1 - CONTROL-FILE MAINTENANCE |".
         DISPLAY "                          | 2 - STATE-CODE MAINTENANCE   |".
         DISPLAY "                          | 3 - VENDOR MAINTENANCE       |".
         DISPLAY "                          | 4 - VOUCHER PROCESSING       |".
         DISPLAY "                          | 0 - EXIT                     |".
         DISPLAY "                          -------------------------------".
         DISPLAY " "
         DISPLAY "                          - CHOOSE AN OPTION FROM MENU:  ".
         PERFORM JUMP-LINE 11 TIMES. 
         ACCEPT W-MAIN-MENU-OPTION
 
        IF W-MAIN-MENU-OPTION EQUAL ZERO
           DISPLAY "PROGRAM TERMINATED !"
        ELSE
           IF NOT VALID-MAIN-MENU-OPTION    
              DISPLAY "INVALID OPTION ! <ENTER> TO CONTINUE"
              ACCEPT DUMMY.
*>_________________________________________________________________________

DO-OPTIONS.

   PERFORM CLEAR-SCREEN.

   IF W-MAIN-MENU-OPTION = 1
      CALL "control-file-maintenance".

   IF W-MAIN-MENU-OPTION = 2
      CALL "state-code-maintenance".

   IF W-MAIN-MENU-OPTION = 3
      CALL "vendor-maintenance".

   IF W-MAIN-MENU-OPTION = 4
      CALL "payment-mode".


   PERFORM GET-MENU-OPTION. *> force first pass
   PERFORM GET-MENU-OPTION UNTIL 
                               W-MAIN-MENU-OPTION EQUAL ZERO 
                            OR VALID-MAIN-MENU-OPTION.
*>_________________________________________________________________________

COPY "PLMENU.CBL".
*>_________________________________________________________________________



