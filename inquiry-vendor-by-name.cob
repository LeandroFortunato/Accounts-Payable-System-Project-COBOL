IDENTIFICATION DIVISION.
PROGRAM-ID. inquiry-vendor-by-name.
ENVIRONMENT DIVISION.
  INPUT-OUTPUT SECTION.
    FILE-CONTROL.

      COPY "SLVND02.CBL".
      COPY "SLSTATE.CBL".
DATA DIVISION.
  FILE SECTION.

      COPY "FDVND02.CBL".
      COPY "FDSTATE.CBL".

  WORKING-STORAGE SECTION.

      COPY "wscase01.cbl".

      01 W-FOUND-VENDOR-RECORD      PIC X.
         88 FOUND-VENDOR-RECORD     VALUE "Y".

      01 W-DISPLAY-NEXT-RECORD      PIC X.
         88 DISPLAY-NEXT-RECORD     VALUE "Y","y".
         88 VALID-ANSWER            VALUE "Y","y","N","n".

      01 W-FOUND-STATE-RECORD       PIC X.
         88 FOUND-STATE-RECORD      VALUE "Y".

     77 ENTRY-VENDOR-NAME           PIC X(30). 
     77 DUMMY                       PIC X.
*>_________________________________________________________________________
      
PROCEDURE DIVISION.

       OPEN I-O STATE-FILE.
       OPEN I-O VENDOR-FILE.

       PERFORM GET-VENDOR-NAME-AND-SEARCH. *> force first pass
       PERFORM GET-VENDOR-NAME-AND-SEARCH UNTIL 
                                         ENTRY-VENDOR-NAME EQUAL SPACES
                                      OR FOUND-VENDOR-RECORD.

       PERFORM SHOW-RECORD-GET-ANOTHER-NUMBER UNTIL 
                                         ENTRY-VENDOR-NAME EQUAL SPACES.
       CLOSE VENDOR-FILE.
       CLOSE STATE-FILE.

       EXIT PROGRAM.
       STOP RUN.
*>_________________________________________________________________________
 
GET-VENDOR-NAME-AND-SEARCH.

     PERFORM CLEAR-SCREEN. 
     MOVE SPACES TO ENTRY-VENDOR-NAME. 
     DISPLAY "INFORM A VENDOR NAME TO SEARCH AND DISPLAY (<ENTER> TO QUIT)".
     ACCEPT ENTRY-VENDOR-NAME.

     IF ENTRY-VENDOR-NAME EQUAL SPACES
        DISPLAY "PROGRAM TERMINATED !"
     ELSE
        PERFORM LOOK-FOR-VENDOR-RECORD
        IF NOT FOUND-VENDOR-RECORD
           DISPLAY "VENDOR NOT FOUND ! "
           ACCEPT DUMMY.
*>_________________________________________________________________________
 
SHOW-RECORD-GET-ANOTHER-NUMBER.

     PERFORM DISPLAY-THE-RECORD.

     MOVE "Y" TO W-DISPLAY-NEXT-RECORD.
     PERFORM READ-VENDOR-NEXT-RECORD-PAUSE.

     PERFORM SHOW-RECORD-ASK-TO-SHOW-NEXT
                                        UNTIL NOT DISPLAY-NEXT-RECORD.

     PERFORM GET-VENDOR-NAME-AND-SEARCH. *> force first pass
     PERFORM GET-VENDOR-NAME-AND-SEARCH UNTIL 
                                         ENTRY-VENDOR-NAME EQUAL SPACES
                                      OR FOUND-VENDOR-RECORD.
*>_________________________________________________________________________
 
SHOW-RECORD-ASK-TO-SHOW-NEXT.

     PERFORM CONFIRM-SHOWING-THE-NEXT-RECORD. *> force first loop         
     PERFORM CONFIRM-SHOWING-THE-NEXT-RECORD UNTIL VALID-ANSWER.

     IF DISPLAY-NEXT-RECORD 
        PERFORM DISPLAY-THE-RECORD
        PERFORM READ-VENDOR-NEXT-RECORD-PAUSE.
*>_________________________________________________________________________
 
CONFIRM-SHOWING-THE-NEXT-RECORD.

   DISPLAY "DISPLAY NEXT RECORD ? (Y/N) ".
   ACCEPT W-DISPLAY-NEXT-RECORD.
   INSPECT W-DISPLAY-NEXT-RECORD CONVERTING SPACE TO "Y".
*>_________________________________________________________________________
       
DISPLAY-THE-RECORD.

     PERFORM CLEAR-SCREEN. 
     DISPLAY "VENDOR.............: " VENDOR-NUMBER.
     DISPLAY "1) NAME............: " VENDOR-NAME.
     DISPLAY "2) ADDRESS - LINE 1: " VENDOR-ADDRESS-1.
     DISPLAY "3) ADDRESS - LINE 2: " VENDOR-ADDRESS-2.    
     DISPLAY "4) CITY............: " VENDOR-CITY.

     MOVE VENDOR-STATE TO STATE-CODE.
     MOVE "Y" TO W-FOUND-STATE-RECORD.
     PERFORM LOOK-FOR-STATE-RECORD.

     IF FOUND-STATE-RECORD
        DISPLAY "5) STATE...........: " VENDOR-STATE " - " STATE-NAME
     ELSE
        DISPLAY "5) STATE...........: " VENDOR-STATE " - ** Not found **".

     DISPLAY "6) ZIP CODE........: " VENDOR-ZIP. 
     DISPLAY "7) CONTACT NAME....: " VENDOR-CONTACT.  
     DISPLAY "8) PHONE NUMBER....: " VENDOR-PHONE.
*>_________________________________________________________________________

LOOK-FOR-VENDOR-RECORD.

       INSPECT ENTRY-VENDOR-NAME CONVERTING LOWER-ALPHA
				         TO UPPER-ALPHA.
       MOVE "Y" TO W-FOUND-VENDOR-RECORD.
       MOVE ENTRY-VENDOR-NAME TO VENDOR-NAME.

       START VENDOR-FILE 
         KEY NOT < VENDOR-NAME 
            INVALID KEY 
      	          MOVE "N" TO W-FOUND-VENDOR-RECORD.
       
       IF FOUND-VENDOR-RECORD
          READ VENDOR-FILE NEXT RECORD
            AT END  	  	
                MOVE "N" TO W-FOUND-VENDOR-RECORD.  
*>_________________________________________________________________________
 
READ-VENDOR-NEXT-RECORD-PAUSE.
 
   READ VENDOR-FILE NEXT RECORD
      AT END  	  	
         DISPLAY "<ENTER> TO CONTINUE"
         ACCEPT DUMMY *> pause to show last record
         MOVE "N" TO W-DISPLAY-NEXT-RECORD.
*>_________________________________________________________________________

COPY "PLMENU.CBL".
COPY "PL-LOOK-FOR-STATE-RECORD.CBL".
*>_________________________________________________________________________

 
