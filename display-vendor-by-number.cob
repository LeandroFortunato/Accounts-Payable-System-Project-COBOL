IDENTIFICATION DIVISION.
PROGRAM-ID. display-vendor-by-number.
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

         01 TITLE.
            05 FILLER              PIC X(28) VALUE SPACES.
            05 FILLER              PIC X(29) VALUE "LIST OF ALL VENDORS BY NUMBER".
            05 FILLER              PIC X(10) VALUE SPACES.
            05 FILLER              PIC X(04) VALUE "PAG:".
            05 PAGE-NUMBER         PIC 9(03).

         01 HEADING-1.
            05 FILLER              PIC X(02) VALUE "NO".
            05 FILLER              PIC X(04) VALUE SPACES.
            05 FILLER              PIC X(14) VALUE "NAME / ADDRESS".
            05 FILLER              PIC X(30) VALUE SPACES.
            05 FILLER              PIC X(30) VALUE "CONTACT/PHONE NUMBER/ZIP CODE".

         01 HEADING-2.
            05 FILLER              PIC X(05) VALUE "=====".
            05 FILLER              PIC X(01) VALUE SPACE.
            05 FILLER              PIC X(43) VALUE "==========================================".
            05 FILLER              PIC X(01) VALUE SPACE.
            05 FILLER              PIC X(30) VALUE "==============================".

         01 DETAIL-1.
            05 D-VENDOR-NUMBER     PIC 9(05).
            05 FILLER              PIC X(01).
            05 D-VENDOR-NAME       PIC X(30).
            05 FILLER              PIC X(14).
            05 D-VENDOR-CONTACT    PIC X(30).

         01 DETAIL-2.
            05 FILLER              PIC X(06) VALUE SPACES.
            05 D-VENDOR-ADDRESS-1  PIC X(30).
            05 FILLER              PIC X(14) VALUE SPACES.
            05 D-VENDOR-PHONE      PIC X(30).

         01 DETAIL-3.
            05 FILLER              PIC X(06) VALUE SPACES.
            05 D-VENDOR-ADDRESS-2  PIC X(30).

         01 DETAIL-4.
            05 FILLER              PIC X(06) VALUE SPACES.
            05 D-VENDOR-CITY       PIC X(20).
            05 FILLER              PIC X(01) VALUE SPACE.
            05 D-VENDOR-STATE      PIC X(02).
            05 FILLER              PIC X(01) VALUE "-".
            05 D-STATE-NAME        PIC X(20).
            05 D-VENDOR-ZIP        PIC X(10).

         01 W-END-OF-FILE          PIC X.
            88 END-OF-FILE      VALUE "Y".

         01 W-DISPLAYED-LINES        PIC 99.
            88 PAGE-FULL        VALUE 18 THROUGH 99.

         01 W-FOUND-STATE-RECORD       PIC X.
            88 FOUND-STATE-RECORD      VALUE "Y".

         77 DUMMY                      PIC X.
*>_________________________________________________________________________

 PROCEDURE DIVISION.
 
     OPEN I-O VENDOR-FILE.
     OPEN I-O STATE-FILE.

     MOVE 0 TO PAGE-NUMBER.
     MOVE "N" TO W-END-OF-FILE.

     PERFORM DISPLAY-HEADINGS.     

     PERFORM READ-VENDOR-FILE-NEXT-RECORD.

     IF END-OF-FILE
        DISPLAY "NO RECORDS IN THE VENDOR FILE ! <ENTER> TO CONTINUE"
     ELSE
        PERFORM DISPLAY-A-RECORD UNTIL END-OF-FILE
        DISPLAY "*** END OF RECORDS ***** ! <ENTER> TO CONTINUE".

     ACCEPT DUMMY.
     CLOSE VENDOR-FILE.
     CLOSE STATE-FILE.

     EXIT PROGRAM.

     STOP RUN.
*>_________________________________________________________________________

DISPLAY-HEADINGS.
       PERFORM CLEAR-SCREEN        
       ADD 1 TO PAGE-NUMBER.
       DISPLAY TITLE.
       PERFORM JUMP-LINE 3 TIMES.
       DISPLAY HEADING-1.
       DISPLAY HEADING-2.      

       MOVE 6 TO W-DISPLAYED-LINES.
*>_________________________________________________________________________

DISPLAY-A-RECORD. 

      IF PAGE-FULL            
         DISPLAY "<ENTER> TO CONTINUE"
         ACCEPT DUMMY
         PERFORM DISPLAY-HEADINGS. 

       MOVE VENDOR-NUMBER    TO D-VENDOR-NUMBER.
       MOVE VENDOR-NAME      TO D-VENDOR-NAME.      
       MOVE VENDOR-ADDRESS-1 TO D-VENDOR-ADDRESS-1.   
       MOVE VENDOR-ADDRESS-2 TO D-VENDOR-ADDRESS-2.   
       MOVE VENDOR-CITY      TO D-VENDOR-CITY.      
       MOVE VENDOR-STATE     TO D-VENDOR-STATE.      

       MOVE VENDOR-STATE TO STATE-CODE.
       MOVE "Y" TO W-FOUND-STATE-RECORD.
       PERFORM LOOK-FOR-STATE-RECORD.

       IF FOUND-STATE-RECORD
          MOVE STATE-NAME TO D-STATE-NAME
       ELSE
          MOVE "** Not found **" TO D-STATE-NAME.

       MOVE VENDOR-ZIP       TO D-VENDOR-ZIP.      
       MOVE VENDOR-CONTACT   TO D-VENDOR-CONTACT.    
       MOVE VENDOR-PHONE     TO D-VENDOR-PHONE.      

       DISPLAY DETAIL-1.
       DISPLAY DETAIL-2.
       ADD 2 TO W-DISPLAYED-LINES.      

       IF D-VENDOR-ADDRESS-2 NOT EQUAL SPACES
           DISPLAY DETAIL-3.
           ADD 1 TO W-DISPLAYED-LINES.  

       DISPLAY DETAIL-4.
       PERFORM JUMP-LINE.

       ADD 2 TO W-DISPLAYED-LINES. 

       PERFORM READ-VENDOR-FILE-NEXT-RECORD.     
*>_________________________________________________________________________

COPY "PLMENU.CBL".
COPY "PL-LOOK-FOR-STATE-RECORD.CBL".
COPY "READ-VENDOR-FILE-NEXT-RECORD.CBL".
*>_________________________________________________________________________





