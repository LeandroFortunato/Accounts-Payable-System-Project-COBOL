*>----------------------------------------------------
*> These procedure is used by:
*>   
*>     print-vendor-by-number.cob
*>     vendor-maintenance.cob
*>     print-vendor-by-name.cob
*>     inquiry-vendor-by-name.cob
*>     display-vendor-by-number.cob
*>     
*>_________________________________________________________________________
 
LOOK-FOR-STATE-RECORD.

   READ STATE-FILE RECORD
        INVALID KEY
           MOVE "N" TO W-FOUND-STATE-RECORD.
*>_________________________________________________________________________


