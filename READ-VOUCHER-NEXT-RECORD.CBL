*>----------------------------------------------------
*> These procedures are used by:
*>  
*>     clearing-existing-selections 
*>     select-by-cut-off-date
*>  
*>_________________________________________________________________________

READ-VOUCHER-NEXT-RECORD.
   
   MOVE "N" TO W-END-OF-FILE. 
   READ VOUCHER-FILE NEXT RECORD
      AT END
         MOVE "Y" TO W-END-OF-FILE.
*>_________________________________________________________________________

