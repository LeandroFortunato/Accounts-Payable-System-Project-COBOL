IDENTIFICATION DIVISION.
PROGRAM-ID. select-voucher-to-pay.
ENVIRONMENT DIVISION.
  INPUT-OUTPUT SECTION.
   FILE-CONTROL.

      COPY "SLVOUCH.CBL".
      COPY "SLVND02.CBL".

DATA DIVISION.
   FILE SECTION.

      COPY "FDVOUCH.CBL".
      COPY "FDVND02.CBL".

   WORKING-STORAGE SECTION.

     COPY "wscase01.cbl".

      01 W-SHOW-ALL-THE-FIELDS             PIC X.
         88 SHOW-ALL-THE-FIELDS           VALUE "Y".

      01 W-FOUND-VENDOR-RECORD             PIC X.
         88 FOUND-VENDOR-RECORD           VALUE "Y".

      01 W-FOUND-VOUCHER-RECORD            PIC X.
         88 FOUND-VOUCHER-RECORD          VALUE "Y".

      01 W-ERROR-WRITING                   PIC X.
         88 ERROR-WRITING                 VALUE "Y".

      01 W-VALID-ANSWER                    PIC X.
         88 VALID-ANSWER                  VALUE "Y","N".
         88 QUIT-IS-CONFIRMED             VALUE "Y".
         88 CHANGE-IS-CONFIRMED           VALUE "Y".

      77 VOUCHER-FORMATTED-DATE           PIC ZZ/ZZ/ZZZZ.
      77 VOUCHER-MM-YY-CCYY               PIC 9(8).
      77 VOUCHER-FORMATTED-AMOUNT         PIC ZZ,ZZZ,ZZ9.99-. 
      77 VOUCHER-CHECK-NUMBER             PIC ZZZZZ.

      77 DUMMY                            PIC X.
      77 DUMMY-FOR-DATE-12                PIC 9(12).
      77 MSG-CONFIRMATION                 PIC X(52).
      77 MSG-AFTER-SAVING                 PIC X(60).
      77 MSG-OPTION                       PIC X(25).
*>_________________________________________________________________________

PROCEDURE DIVISION.

   OPEN I-O VOUCHER-FILE.
   OPEN I-O VENDOR-FILE.

   PERFORM CLEAR-SCREEN.
   MOVE "SELECT/CLEAR FOR PAYMENT" TO MSG-OPTION.
   PERFORM GET-AN-EXISTANT-VOUCHER-NUMBER.
   PERFORM CONFIRM-SELECTION-GET-ANOTHER UNTIL 
                                       VOUCHER-NUMBER EQUAL ZEROS.


   CLOSE VOUCHER-FILE.
   CLOSE VENDOR-FILE.

   EXIT PROGRAM.

   STOP RUN.
*>____________________________________________________________________

CONFIRM-SELECTION-GET-ANOTHER.

     MOVE "Y" TO W-SHOW-ALL-THE-FIELDS. 
     PERFORM DISPLAY-VOUCHER-RECORD.      

     IF VOUCHER-PAID-DATE NOT EQUAL ZEROS
        DISPLAY " *** THIS VOUCHER HAS BEEN ALREADY PAID ! ***  <ENTER> TO CONTINUE"
        ACCEPT DUMMY
     ELSE
        PERFORM CONTINUE-CSGA-PROCESS.
      
     PERFORM GET-AN-EXISTANT-VOUCHER-NUMBER.
*>_________________________________________________________________________

CONTINUE-CSGA-PROCESS.

   IF VOUCHER-SELECTED NOT EQUAL "Y"
      MOVE "DO YOU CONFIRM SELECTING THIS VOUCHER FOR PAYMENT ?" TO MSG-CONFIRMATION
   ELSE
      MOVE "DO YOU CONFIRM CLEARING THIS VOUCHER ?" TO MSG-CONFIRMATION.

   PERFORM CONFIRM-EXECUTION. *> force first loop
   PERFORM CONFIRM-EXECUTION UNTIL VALID-ANSWER.

   IF CHANGE-IS-CONFIRMED 
      INSPECT VOUCHER-SELECTED CONVERTING "YN" TO "NY"
      PERFORM SAVE-CHANGES-ON-THE-VOUCHER.
*>_________________________________________________________________________

COPY "PLGENERAL.CBL".
COPY "PLVOUCHER.CBL".
COPY "PL-LOOK-FOR-VENDOR-RECORD.CBL".

*>_________________________________________________________________________

