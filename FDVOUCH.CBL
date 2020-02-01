*>--------------------------------
*> FDVOUCH.CBL
*> Primary Key - VOUCHER-NUMBER
*> Dates are in CCYYMMDD format
*>--------------------------------

 FD VOUCHER-FILE 
    LABEL RECORDS ARE STANDARD.

 01 VOUCHER-RECORD.
    05 VOUCHER-NUMBER        PIC 9(5).
    05 VOUCHER-VENDOR        PIC 9(5).
    05 VOUCHER-INVOICE       PIC X(15).
    05 VOUCHER-FOR           PIC X(30).
    05 VOUCHER-AMOUNT        PIC S9(6)V99.
    05 VOUCHER-DATE          PIC 9(8).
    05 VOUCHER-DUE           PIC 9(8).
    05 VOUCHER-DEDUCTIBLE    PIC X.
    05 VOUCHER-SELECTED      PIC X.
    05 VOUCHER-PAID-AMOUNT   PIC S9(6)V99.
    05 VOUCHER-PAID-DATE     PIC 9(8).
    05 VOUCHER-CHECK-NO      PIC 9(6).




