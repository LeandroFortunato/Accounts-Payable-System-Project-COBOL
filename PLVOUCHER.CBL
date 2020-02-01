*> PLVOUCHER.CBL
*>------------------------------------------------------------------------
*> Procedures used by:

*>    vendor-maintenance.cob
*>    select-voucher-to-pay.cob
*>------------------------------------------------------------------------

SAVE-CHANGES-ON-THE-VOUCHER.

     PERFORM REWRITE-VOUCHER-RECORD.
      
     IF ERROR-WRITING
        PERFORM DISPLAY-VOUCHER-RECORD *> User can see how the record is now
        DISPLAY "ERROR WHILE REWRITING THE VOUCHER RECORD! <ENTER> TO CONTINUE"
        ACCEPT DUMMY
     ELSE
        PERFORM DISPLAY-VOUCHER-RECORD
        IF MSG-AFTER-SAVING NOT EQUAL SPACES
           DISPLAY MSG-AFTER-SAVING
           ACCEPT DUMMY.
*>_________________________________________________________________________

REWRITE-VOUCHER-RECORD.

   MOVE "N" TO W-ERROR-WRITING. 

   REWRITE VOUCHER-RECORD
            INVALID KEY 
          MOVE "Y" TO W-ERROR-WRITING. 
*>_________________________________________________________________________

GET-AN-EXISTANT-VOUCHER-NUMBER.

       PERFORM GET-VOUCHER-NUMBER-AND-SEARCH. *> force first pass
       PERFORM GET-VOUCHER-NUMBER-AND-SEARCH UNTIL 
                                         VOUCHER-NUMBER EQUAL ZEROS
                                      OR FOUND-VOUCHER-RECORD.
*>_________________________________________________________________________

GET-VOUCHER-NUMBER-AND-SEARCH.

     PERFORM ASK-USER-FOR-THE-VOUCHER-NUMBER.

     IF VOUCHER-NUMBER NOT EQUAL ZEROS
        PERFORM LOOK-FOR-VOUCHER-RECORD
        IF NOT FOUND-VOUCHER-RECORD
           DISPLAY "VOUCHER NOT FOUND ! ".
*>_________________________________________________________________________

ASK-USER-FOR-THE-VOUCHER-NUMBER.

     MOVE "Y" TO W-FOUND-VOUCHER-RECORD.
     DISPLAY "INFORM A VOUCHER NUMBER TO " MSG-OPTION " (<ENTER> FOR MENU)".
     ACCEPT VOUCHER-NUMBER.
*>_________________________________________________________________________

LOOK-FOR-VOUCHER-RECORD.

   READ VOUCHER-FILE RECORD
        INVALID KEY
           MOVE "N" TO W-FOUND-VOUCHER-RECORD.
*>________________________________________________________________________

DISPLAY-VOUCHER-RECORD.

     PERFORM CLEAR-SCREEN.

     IF VOUCHER-NUMBER EQUAL ZEROS *> It is including now
        DISPLAY "   VOUCHER NUMBER...: *** to be Generated ***"        
     ELSE *> It is finished 
        DISPLAY "   VOUCHER NUMBER...: " VOUCHER-NUMBER.
 
     DISPLAY " "

     IF VOUCHER-VENDOR EQUAL ZEROS
        DISPLAY " 1) VENDOR...........: " 
      ELSE
        MOVE VOUCHER-VENDOR TO VENDOR-NUMBER
        MOVE "Y" TO W-FOUND-VENDOR-RECORD
        PERFORM LOOK-FOR-VENDOR-RECORD

        IF FOUND-VENDOR-RECORD
           DISPLAY " 1) VENDOR...........: " VOUCHER-VENDOR " - " VENDOR-NAME
        ELSE
           DISPLAY " 1) VENDOR...........: " VOUCHER-VENDOR " - ** Not found **".

     DISPLAY " 2) INVOICE..........: " VOUCHER-INVOICE.
     DISPLAY " 3) VOUCHER FOR......: " VOUCHER-FOR.    

     IF VOUCHER-AMOUNT EQUAL ZEROS *> It is including now
        DISPLAY " 4) AMOUNT...........: " 
     ELSE
        MOVE VOUCHER-AMOUNT TO VOUCHER-FORMATTED-AMOUNT        
        DISPLAY " 4) AMOUNT...........: " VOUCHER-FORMATTED-AMOUNT.

     COMPUTE DUMMY-FOR-DATE-12 = VOUCHER-DATE * 10000.0001
     MOVE DUMMY-FOR-DATE-12  TO VOUCHER-MM-YY-CCYY.
     MOVE VOUCHER-MM-YY-CCYY TO VOUCHER-FORMATTED-DATE.

     IF VOUCHER-DATE EQUAL ZEROS *> It is including now
        DISPLAY " 5) RECEIVED ON......: " 
     ELSE
        DISPLAY " 5) RECEIVED ON......: " VOUCHER-FORMATTED-DATE.

     COMPUTE DUMMY-FOR-DATE-12 = VOUCHER-DUE * 10000.0001
     MOVE DUMMY-FOR-DATE-12  TO VOUCHER-MM-YY-CCYY.
     MOVE VOUCHER-MM-YY-CCYY TO VOUCHER-FORMATTED-DATE.

     IF VOUCHER-DUE EQUAL ZEROS *> It is including now
        DISPLAY " 6) DUE TO...........: " 
     ELSE
        DISPLAY " 6) DUE TO...........: " VOUCHER-FORMATTED-DATE.

     IF VOUCHER-DEDUCTIBLE EQUAL SPACES *> It is including now
        DISPLAY " 7) DEDUCTIBLE.......: " 
     ELSE
        DISPLAY " 7) DEDUCTIBLE.......: " VOUCHER-DEDUCTIBLE.


     IF NOT SHOW-ALL-THE-FIELDS
        PERFORM JUMP-LINE 11 TIMES
     ELSE

        DISPLAY " 8) SELECTED.........: " VOUCHER-SELECTED
   
        COMPUTE DUMMY-FOR-DATE-12 = VOUCHER-PAID-DATE * 10000.0001
        MOVE DUMMY-FOR-DATE-12  TO VOUCHER-MM-YY-CCYY
        MOVE VOUCHER-MM-YY-CCYY TO VOUCHER-FORMATTED-DATE

        DISPLAY " 9) PAID DATE........: " VOUCHER-FORMATTED-DATE

        MOVE VOUCHER-PAID-AMOUNT TO VOUCHER-FORMATTED-AMOUNT        

        DISPLAY "10) CHECK NO.........: " VOUCHER-CHECK-NO
        DISPLAY "11) PAID AMOUNT......: " VOUCHER-FORMATTED-AMOUNT
      
        PERFORM JUMP-LINE 7 TIMES. 
*>_________________________________________________________________________



