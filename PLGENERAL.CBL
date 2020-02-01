*>----------------------------------------------------
*> These procedures are used by:
*>   
*>     vendor-maintenance.cob
*>     voucher-maintenance.cob
*>     select-voucher-to-pay.
*>     pay-selected-voucher
*>     clearing-existing-selections 
*>_________________________________________________________________________

CONFIRM-IF-WANT-TO-QUIT.

MOVE "DO YOU WANT TO QUIT ? <Y/N>" TO MSG-CONFIRMATION.
PERFORM ASK-USER-IF-WANT-TO-COMPLETE.

*>_________________________________________________________________________

ASK-USER-IF-WANT-TO-COMPLETE.

       PERFORM CONFIRM-EXECUTION. *> force first loop
       PERFORM CONFIRM-EXECUTION UNTIL VALID-ANSWER.      
*>_________________________________________________________________________

CONFIRM-EXECUTION.

       DISPLAY MSG-CONFIRMATION.
       ACCEPT W-VALID-ANSWER.

       INSPECT W-VALID-ANSWER 
                         CONVERTING LOWER-ALPHA
                                 TO UPPER-ALPHA.
*>_________________________________________________________________________

COPY "PLMENU.CBL".
*>_________________________________________________________________________


