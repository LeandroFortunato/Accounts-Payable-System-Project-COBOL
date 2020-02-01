*>--------------------------------
*> FDCHK01.CBL
*> Primary Key - CHECK-KEY
*>   if you use more than 1 check
*>   account to pay bills, using
*>   check numbers only may
*>   cause duplicates.
*> CHECK-INVOICE is the vendor's
*>   invoice that this check paid
*>   and can be blank.
*> CHECK-CLEARED = "Y" once the
*>   the check is reported cashed
*>   on a bank statement. Setting
*>   this flag is done in the
*>   check clearance program
*>   chkclr.cbl.
*> CHECK-REFERENCE for any notes
*>   about the check.
*> CHECK-VENDOR can be zero for a
*>   general check to someone who
*>   is not a regular vendor, but
*> CHECK-REFERENCE should be
*>   filled in with payee.
*>--------------------------------

   FD CHECK-FILE
      LABEL RECORDS ARE STANDARD.

   01 CHECK-RECORD.
      05 CHECK-KEY.
         10 CHECK-ACCOUNT PIC 9(10).
         10 CHECK-NUMBER  PIC 9(6).
      05 CHECK-AMOUNT     PIC S9(6)V99.
      05 CHECK-INVOICE    PIC X(15).
      05 CHECK-VENDOR     PIC 9(5).
      05 CHECK-REFERENCE  PIC X(30).
      05 CHECK-CLEARED    PIC X.



.


.













