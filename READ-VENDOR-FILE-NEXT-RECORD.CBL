*>----------------------------------------------------
*> These procedure is used by:
*>   
*>     print-vendor-by-name.cob
*>     display-vendor-by-number.cob
*>_________________________________________________________________________
 
READ-VENDOR-FILE-NEXT-RECORD.

     READ VENDOR-FILE NEXT RECORD
        AT END 
               MOVE "Y" TO W-END-OF-FILE.
*>_________________________________________________________________________




