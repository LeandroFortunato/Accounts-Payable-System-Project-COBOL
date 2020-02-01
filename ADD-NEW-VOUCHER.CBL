
*> ADD-NEW-VOUCHER.CBL
*>------------------------------------------------------------------------
*> Procedures used by:

*>    voucher-maintenance.cob
*>    pay-selected-voucher.cob
*>------------------------------------------------------------------------

ADD-NEW-VOUCHER.
 
  PERFORM GENERATE-NEW-VOUCHER-NUMBER
  IF NOT ERROR-R-W-NEW-VOUCHER-NUMBER 
     PERFORM WRITE-RECORD
     IF ERROR-WRITING
        DISPLAY "ERROR WHILE WRITING THE RECORD ! <ENTER> TO CONTINUE" 
        ACCEPT DUMMY
     ELSE 
        PERFORM DISPLAY-VOUCHER-RECORD
        IF MSG-AFTER-SAVING NOT EQUAL SPACES
           DISPLAY MSG-AFTER-SAVING
           ACCEPT DUMMY.
*>_________________________________________________________________________

GENERATE-NEW-VOUCHER-NUMBER.

   OPEN I-O CONTROL-FILE.       
   
   MOVE 1   TO CONTROL-KEY.
   MOVE "N" TO W-ERROR-R-W-NEW-VOUCHER-NUMBER.
   
   READ CONTROL-FILE RECORD
      INVALID KEY 
         MOVE "Y" TO W-ERROR-R-W-NEW-VOUCHER-NUMBER
         DISPLAY "ERROR READING THE CONTROL-FILE ! <ENTER> TO CONTINUE" 
         ACCEPT DUMMY.

   IF NOT ERROR-R-W-NEW-VOUCHER-NUMBER *> Reading was OK
      MOVE "Y" TO W-FOUND-VOUCHER-RECORD
      
      PERFORM ADD-1-TO-CONTROL-LAST-VOUCHER UNTIL NOT FOUND-VOUCHER-RECORD
                                                   OR ERROR-R-W-NEW-VOUCHER-NUMBER.
   CLOSE CONTROL-FILE.
*>_________________________________________________________________________

WRITE-RECORD.

   MOVE "N" TO W-ERROR-WRITING.
   WRITE VOUCHER-RECORD
       INVALID KEY 
          MOVE "Y" TO W-ERROR-WRITING.  
*>_________________________________________________________________________
                                                
ADD-1-TO-CONTROL-LAST-VOUCHER.

   ADD 1 TO CONTROL-LAST-VOUCHER.
   REWRITE CONTROL-RECORD
      INVALID KEY 
         MOVE "Y" TO W-ERROR-R-W-NEW-VOUCHER-NUMBER
         DISPLAY "ERROR WRITING THE NEW VOUCHER NUMBER IN THE CONTROL-FILE ! <ENTER> TO CONTINUE" 
         ACCEPT DUMMY.
 
   IF NOT ERROR-R-W-NEW-VOUCHER-NUMBER *> Re-writing was OK
      MOVE VOUCHER-RECORD TO BK-NEW-VOUCHER-RECORD-INFORMED *> Save the new voucher informed, that is on the buffer of the VOUCHER RECORD
      MOVE CONTROL-LAST-VOUCHER TO VOUCHER-NUMBER
      MOVE "Y" TO W-FOUND-VOUCHER-RECORD
      PERFORM LOOK-FOR-VOUCHER-RECORD
      MOVE BK-NEW-VOUCHER-RECORD-INFORMED TO VOUCHER-RECORD *> Restore to the buffer of the VOUCHER RECORD, the new voucher informed that is on backup
      MOVE CONTROL-LAST-VOUCHER TO VOUCHER-NUMBER. *> Restore the new VOUCHER-NUMBER generated (the one from the backup is ZEROS)
*>_________________________________________________________________________


