*> PL-LOOK-FOR-VENDOR-RECORD.CBL
*>------------------------------------------------------------------------
*> Procedure used by:

*>    vendor-maintenance.cob
*>    voucher-maintenance.cob
*>    select-voucher-to-pay.cob
*>------------------------------------------------------------------------

*>_________________________________________________________________________

LOOK-FOR-VENDOR-RECORD.

   READ VENDOR-FILE RECORD
        INVALID KEY
           MOVE "N" TO W-FOUND-VENDOR-RECORD.
*>_________________________________________________________________________
