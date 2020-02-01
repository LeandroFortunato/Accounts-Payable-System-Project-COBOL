IDENTIFICATION DIVISION.
PROGRAM-ID. payment-mode.
ENVIRONMENT DIVISION.
   INPUT-OUTPUT SECTION.
      FILE-CONTROL.

DATA DIVISION.
   FILE SECTION.

   WORKING-STORAGE SECTION.

     COPY "wscase01.cbl".

     01 W-CONTROL-MENU-OPTION          PIC 9.
         88 VALID-CONTROL-MENU-OPTION       VALUE  0 THROUGH 8.  

     01 W-VALID-ANSWER                 PIC X.
         88 VALID-ANSWER                    VALUE "Y","N".
         88 BILLS-REPORT-IS-CONFIRMED       VALUE "Y".
         88 CASH-REQUIREMENT-IS-CONFIRMED   VALUE "Y".
         88 DEDUCTIBLE-REPORT-IS-CONFIRMED  VALUE "Y".

     77 MSG-CONFIRMATION               PIC X(75).
     77 DUMMY                          PIC X.
*>_________________________________________________________________________

PROCEDURE DIVISION.

   PERFORM GET-MENU-OPTION. *> force first pass
   PERFORM GET-MENU-OPTION UNTIL 
                               W-CONTROL-MENU-OPTION EQUAL ZERO 
                            OR VALID-CONTROL-MENU-OPTION.

   PERFORM DO-OPTIONS UNTIL W-CONTROL-MENU-OPTION EQUAL ZERO. 

   EXIT PROGRAM.

   STOP RUN.
*>_________________________________________________________________________

GET-MENU-OPTION.
         
         PERFORM CLEAR-SCREEN.
         DISPLAY "                                VOUCHER PROCESSING            ".
         DISPLAY " "
         DISPLAY "                    ------------------------------------------".
         DISPLAY "                    | 1 - VOUCHER ENTRY                      |".
         DISPLAY "                    | 2 - BILLS REPORT                       |".
         DISPLAY "                    | 3 - SELECT VOUCHERS BY DUE DATE RANGE  |".
         DISPLAY "                    | 4 - SELECT INDIVIDUAL VOUCHERS         |".
         DISPLAY "                    | 5 - CLEAR PREVIOUS SELECTIONS          |".
         DISPLAY "                    | 6 - CASH REQUIREMENTS REPORT           |".
         DISPLAY "                    | 7 - PAID BILLS ENTRY                   |".
         DISPLAY "                    | 8 - DEDUCTIBLES REPORT                 |".
         DISPLAY "                    | 0 - EXIT                               |".
         DISPLAY "                     -----------------------------------------".
         DISPLAY " "
         DISPLAY "                          - CHOOSE AN OPTION FROM MENU:  ".
         PERFORM JUMP-LINE 08 TIMES. 
         ACCEPT W-CONTROL-MENU-OPTION.
 
        IF W-CONTROL-MENU-OPTION EQUAL ZERO
           DISPLAY "PROGRAM TERMINATED !"
        ELSE
           IF NOT VALID-CONTROL-MENU-OPTION    
              DISPLAY "INVALID OPTION ! <ENTER> TO CONTINUE"
              ACCEPT DUMMY.
*>_________________________________________________________________________

DO-OPTIONS.

      IF W-CONTROL-MENU-OPTION = 1
         CALL "voucher-maintenance".

      IF W-CONTROL-MENU-OPTION = 2

         PERFORM CLEAR-SCREEN

         MOVE "DO YOU CONFIRM PRINTING THE BILLS REPORT ?  <Y/N>"
           TO  MSG-CONFIRMATION     

         PERFORM CONFIRM-EXECUTION *> force first loop
         PERFORM CONFIRM-EXECUTION UNTIL VALID-ANSWER       

         IF BILLS-REPORT-IS-CONFIRMED
            CALL "bills-report"
            DISPLAY "BILLS-REPORT HAS BEEN PRINTED ! <ENTER> TO CONTINUE"
            ACCEPT DUMMY.

      IF W-CONTROL-MENU-OPTION = 3
         CALL "select-by-cut-off-date".

      IF W-CONTROL-MENU-OPTION = 4
         CALL "select-voucher-to-pay".

      IF W-CONTROL-MENU-OPTION = 5
         CALL "clearing-existing-selections".

      IF W-CONTROL-MENU-OPTION = 6

         PERFORM CLEAR-SCREEN

         MOVE "DO YOU CONFIRM PRINTING THE CASH-REQUIREMENT REPORT ?  <Y/N>"
           TO  MSG-CONFIRMATION     

         PERFORM CONFIRM-EXECUTION *> force first loop
         PERFORM CONFIRM-EXECUTION UNTIL VALID-ANSWER       

         IF CASH-REQUIREMENT-IS-CONFIRMED
            CALL "cash-requirement-report"
            DISPLAY "CASH-REQUIREMENT REPORT HAS BEEN PRINTED ! <ENTER> TO CONTINUE"
            ACCEPT DUMMY.

      IF W-CONTROL-MENU-OPTION = 7
         CALL "pay-selected-voucher".

      IF W-CONTROL-MENU-OPTION = 8

         PERFORM CLEAR-SCREEN

         MOVE "DO YOU CONFIRM PRINTING THE DEDUCTIBLES REPORT ?  <Y/N>"
           TO  MSG-CONFIRMATION     

         PERFORM CONFIRM-EXECUTION *> force first loop
         PERFORM CONFIRM-EXECUTION UNTIL VALID-ANSWER       

         IF DEDUCTIBLE-REPORT-IS-CONFIRMED
            CALL "deductibles-report"
            DISPLAY "DEDUCTIBLES REPORT HAS BEEN PRINTED ! <ENTER> TO CONTINUE"
            ACCEPT DUMMY.

   PERFORM GET-MENU-OPTION. *> force first pass
   PERFORM GET-MENU-OPTION UNTIL 
                               W-CONTROL-MENU-OPTION EQUAL ZERO 
                            OR VALID-CONTROL-MENU-OPTION.
*>_________________________________________________________________________

COPY "PLGENERAL.CBL".
*>_________________________________________________________________________


