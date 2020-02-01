
*> wsdate.cbl

*>-------------------------------------------------------------------------
*>    WORKING-STORAGE to be used by PLDATE.CBL

*>-------------------------------------------------------------------------
*>    Variables that will be received from main-program:

*>       GDTV-DATE-HEADING      ---  Heading containing date specification to be informed by the user
*>       GDTV-FIRST-YEAR-VALID  ---  First year-limit valid         
*>       GDTV-LAST-YEAR-VALID   ---  Last year-limit valid.
*>       GDTV-ACCEPT-EMPTY-DATE ---  "Y" or "N"

*>-------------------------------------------------------------------------
*>    Variable that will be returned to main-program
      
*>        GDTV-DATE (format CCYY-MM-DD)
*>-------------------------------------------------------------------------

      01 GDTV-DATE-MM-DD-CCYY          PIC 9(8).
      01 FILLER REDEFINES GDTV-DATE-MM-DD-CCYY.
         05 GDTV-DATE-MM               PIC 99.
            88 GDTV-MONTH-VALID        VALUE 1 THROUGH 12.
         05 GDTV-DATE-DD               PIC 99.
         05 GDTV-DATE-CCYY             PIC 9999.
                    
      01 GDTV-MATRIX.
         02 GDTV-TABLE-MONTH OCCURS 12 TIMES.
            05 GDTV-TABLE-MONTH-NUMBER  PIC 99.
            05 GDTV-TABLE-MONTH-NAME    PIC X(09).

      01 W-GDTV-VALID-DATE-INFORMED    PIC X.
         88 GDTV-VALID-DATE-INFORMED   VALUE "Y".

      77 GDTV-DATE-TEMP-FOR-CALC       PIC 9(12).
      77 GDTV-LEAP-YEAR-REMAINDER      PIC 999.
      77 GDTV-DUMMY                    PIC X.    
      77 GDTV-LEAP-YEAR-DUMMY-QUO      PIC 9999.

  *>---------- Values received from main program
      77 GDTV-ACCEPT-EMPTY-DATE        PIC X.
      77 GDTV-DATE-HEADING             PIC X(79).
      77 GDTV-FIRST-YEAR-VALID         PIC 9(4).         
      77 GDTV-LAST-YEAR-VALID          PIC 9(4).

  *>---------- Value returned to main program 
      77 GDTV-DATE                     PIC 9(8). *> format will be CCYY-MM-DD
 



