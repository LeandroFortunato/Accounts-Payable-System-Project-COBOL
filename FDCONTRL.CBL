*>--------------------------------
*> FDCONTRL.CBL
*> Primary Key - CONTROL-KEY
*> LAST-VOUCHER is used
*> to track the last
*> used voucher number.
*> This is a single record file
*> CONTROL-KEY always = 1.
*>--------------------------------
   FD CONTROL-FILE
      LABEL RECORDS ARE STANDARD.

   01 CONTROL-RECORD.
      05 CONTROL-KEY PIC 9.
      05 CONTROL-LAST-VOUCHER PIC 9(5).

