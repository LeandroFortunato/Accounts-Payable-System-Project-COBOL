*>--------------------------------
*> FDBILL01.CBL
*> Primary Key - BILL-NUMBER
*> BILL-DATE, BILL-DUE and BILL-PAID
*> are all dates in CCYYMMDD format.
*>--------------------------------

   FD BILL-FILE
      LABEL RECORDS ARE STANDARD.

   01 BILL-RECORD.
      05 BILL-NUMBER     PIC 9(6).
      05 BILL-DATE       PIC 9(8).
      05 BILL-DUE        PIC 9(8).
      05 BILL-AMOUNT     PIC S9(6)V99.
      05 BILL-INVOICE    PIC X(15).
      05 BILL-VENDOR     PIC 9(5).
      05 BILL-NOTES      PIC X(30).
      05 BILL-PAID       PIC 9(8).

