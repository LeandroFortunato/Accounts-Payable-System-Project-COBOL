*>
*> FDVND02.CBL
*>
*> Primary Key - VENDOR-NUMBER
*> Alternate - NAME with duplicates
*>
*> NAME, ADDRESS-1, CITY, STATE,
*> and PHONE are required fields.
*>
*> VENDOR-STATE must be looked up
*> and must exist in the STATE-FILE
*> to be valid.
*>
*> VENDOR-ADDRESS-2 not always used
*> so may be SPACES
*>
*> VENDOR-PHONE is usually the
*> number for VENDOR-CONTACT
*>
*> All fields should be entered in
*> UPPER case.
*>--------------------------------
   FD VENDOR-FILE
         LABEL RECORDS ARE STANDARD. 

   01 VENDOR-RECORD.
      05 VENDOR-NUMBER        PIC 9(5).
      05 VENDOR-NAME          PIC X(30).
      05 VENDOR-ADDRESS-1     PIC X(30).
      05 VENDOR-ADDRESS-2     PIC X(30).
      05 VENDOR-CITY          PIC X(20).
      05 VENDOR-STATE         PIC X(2).
      05 VENDOR-ZIP           PIC X(10).
      05 VENDOR-CONTACT       PIC X(30).
      05 VENDOR-PHONE         PIC X(15).




