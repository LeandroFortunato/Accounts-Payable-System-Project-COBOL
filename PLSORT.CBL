*>----------------------------------------------------
*> These procedure is used by:
*>   
*>     print-all-vouchers-to-pay.cob
*>     control-break-by-vendor.cob
*>     cash-requirement-report.cob
*>     control-break-by-due.cob
*>________________________________________________________________________

READ-WORK-NEXT-RECORD.
   
   MOVE "N" TO W-END-OF-FILE. 
   READ WORK-FILE NEXT RECORD
      AT END
         MOVE "Y" TO W-END-OF-FILE.
*>_________________________________________________________________________

COPY "PLPRINT.CBL".
*>_________________________________________________________________________

