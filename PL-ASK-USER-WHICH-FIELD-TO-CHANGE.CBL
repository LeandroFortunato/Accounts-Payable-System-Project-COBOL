*> PL-ASK-USER-WHICH-FIELD-TO-CHANGE.CBL
*>------------------------------------------------------------------------
*> Procedures used by:

*>    vendor-maintenance.cob
*>    voucher-maintenance.cob
*>------------------------------------------------------------------------

*>_________________________________________________________________________

ASK-USER-WHICH-FIELD-TO-CHANGE.

        PERFORM GET-A-FIELD-TO-CHANGE. *> force first pass
        PERFORM GET-A-FIELD-TO-CHANGE 
                                      UNTIL ENTRY-RECORD-FIELD EQUAL ZERO 
                                         OR VALID-FIELD. 
*>_________________________________________________________________________


