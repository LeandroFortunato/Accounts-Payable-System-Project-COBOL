IDENTIFICATION DIVISION.
PROGRAM-ID. inquiry-vendor-by-number.
ENVIRONMENT DIVISION.
  INPUT-OUTPUT SECTION.
    FILE-CONTROL.

      COPY "SLVND02.CBL".

DATA DIVISION.
  FILE SECTION.

      COPY "FDVND02.CBL".

  WORKING-STORAGE SECTION.

     01 W-FOUND-RECORD         PIC X.
        88 FOUND-RECORD     VALUE "Y".

     77 ENTRY-VENDOR-NUMBER        PIC 9(5). 
     77 DUMMY                      PIC X.
*>_________________________________________________________________________
     
PROCEDURE DIVISION.

       OPEN I-O VENDOR-FILE.

       PERFORM GET-VENDOR-NUMBER-AND-SEARCH. *> force first pass
       PERFORM GET-VENDOR-NUMBER-AND-SEARCH UNTIL 
                                         ENTRY-VENDOR-NUMBER EQUAL ZEROS
                                      OR FOUND-RECORD.

       PERFORM GET-RECORD-SHOW-AND-GET-ANOTHER UNTIL 
                                         ENTRY-VENDOR-NUMBER EQUAL ZEROS.
       CLOSE VENDOR-FILE.

       EXIT PROGRAM.

       STOP RUN.
*>_________________________________________________________________________

GET-VENDOR-NUMBER-AND-SEARCH.

     MOVE ZEROS TO ENTRY-VENDOR-NUMBER. 
     DISPLAY "INFORM A VENDOR NUMBER TO SEARCH AND DISPLAY (<ENTER> TO QUIT)".
     ACCEPT ENTRY-VENDOR-NUMBER.

     IF ENTRY-VENDOR-NUMBER EQUAL ZEROS
        DISPLAY "PROGRAM TERMINATED !"
     ELSE
        MOVE ENTRY-VENDOR-NUMBER TO VENDOR-NUMBER
        MOVE "Y" TO W-FOUND-RECORD
        READ VENDOR-FILE RECORD
        INVALID KEY
              MOVE "N" TO W-FOUND-RECORD
              DISPLAY "VENDOR NOT FOUND ! ".
*>_________________________________________________________________________

GET-RECORD-SHOW-AND-GET-ANOTHER.

     PERFORM DISPLAY-THE-RECORD.
     DISPLAY "<ENTER> TO CONTINUE".
     ACCEPT DUMMY.
     
     PERFORM GET-VENDOR-NUMBER-AND-SEARCH. *> force first pass
     PERFORM GET-VENDOR-NUMBER-AND-SEARCH UNTIL 
                                         ENTRY-VENDOR-NUMBER EQUAL ZEROS
                                      OR FOUND-RECORD.
*>_________________________________________________________________________

DISPLAY-THE-RECORD.
     
     DISPLAY "VENDOR.............: " VENDOR-NUMBER.
     DISPLAY "1) NAME............: " VENDOR-NAME.
     DISPLAY "2) ADDRESS - LINE 1: " VENDOR-ADDRESS-1.
     DISPLAY "3) ADDRESS - LINE 2: " VENDOR-ADDRESS-2.    
     DISPLAY "4) CITY............: " VENDOR-CITY.
     DISPLAY "5) STATE...........: " VENDOR-STATE.  
     DISPLAY "6) ZIP CODE........: " VENDOR-ZIP. 
     DISPLAY "7) CONTACT NAME....: " VENDOR-CONTACT.  
     DISPLAY "8) PHONE NUMBER....: " VENDOR-PHONE.
*>_________________________________________________________________________



