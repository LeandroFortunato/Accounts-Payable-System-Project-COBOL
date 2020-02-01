IDENTIFICATION DIVISION.
PROGRAM-ID. print-vendor-by-number.
ENVIRONMENT DIVISION.
   INPUT-OUTPUT SECTION.
      FILE-CONTROL.

         SELECT PRINTER-FILE 
                ASSIGN TO "print-vendor-by-number.prn"
                ORGANIZATION IS LINE SEQUENTIAL.         
        
         COPY "SLVND02.CBL".
         COPY "SLSTATE.CBL".

DATA DIVISION.
   FILE SECTION.
 
         FD PRINTER-FILE
            LABEL RECORDS ARE OMITTED.

         01 PRINTER-RECORD         PIC X(80).

         COPY "FDVND02.CBL".
         COPY "FDSTATE.CBL".

   WORKING-STORAGE SECTION.

         01 TITLE.
            05 FILLER              PIC X(28) VALUE SPACES.
            05 FILLER              PIC X(29) VALUE "LIST OF ALL VENDORS BY NUMBER".
            05 FILLER              PIC X(10) VALUE SPACES.
            05 FILLER              PIC X(04) VALUE "PAG:".
            05 PAGE-NUMBER         PIC 9(03) VALUE 0.

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

         01 W-PRINTED-LINES        PIC 99.
            88 PAGE-FULL        VALUE 30 THROUGH 99.

         01 W-FOUND-STATE-RECORD       PIC X.
            88 FOUND-STATE-RECORD      VALUE "Y".
*>_________________________________________________________________________
  
PROCEDURE DIVISION.
   
     OPEN I-O VENDOR-FILE.
     OPEN I-O STATE-FILE.
     OPEN OUTPUT PRINTER-FILE.

     MOVE 0 TO PAGE-NUMBER.
     MOVE "N" TO W-END-OF-FILE.

     PERFORM PRINT-HEADINGS.     

     READ VENDOR-FILE NEXT RECORD
        AT END 
               MOVE "NO RECORDS IN THE VENDOR FILE !" TO PRINTER-RECORD
               WRITE PRINTER-RECORD BEFORE ADVANCING 1
               PERFORM FINALIZE-PAGE
               MOVE "Y" TO W-END-OF-FILE.

     PERFORM PRINT-A-RECORD UNTIL END-OF-FILE.

     CLOSE VENDOR-FILE.
     CLOSE STATE-FILE.
     CLOSE PRINTER-FILE.

     EXIT PROGRAM.

     STOP RUN.
*>_________________________________________________________________________
 
PRINT-A-RECORD. 

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

       MOVE DETAIL-1 TO PRINTER-RECORD.
       WRITE PRINTER-RECORD BEFORE ADVANCING 1.
       MOVE DETAIL-2 TO PRINTER-RECORD.
       WRITE PRINTER-RECORD BEFORE ADVANCING 1.
       ADD 2 TO W-PRINTED-LINES.      

       IF D-VENDOR-ADDRESS-2 NOT EQUAL SPACES
           MOVE DETAIL-3 TO PRINTER-RECORD
           WRITE PRINTER-RECORD BEFORE ADVANCING 1
           ADD 1 TO W-PRINTED-LINES.  

       MOVE DETAIL-4 TO PRINTER-RECORD.
       WRITE PRINTER-RECORD BEFORE ADVANCING 1.
       MOVE SPACES TO PRINTER-RECORD.
       WRITE PRINTER-RECORD AFTER ADVANCING 1.
       ADD 2 TO W-PRINTED-LINES. 

       READ VENDOR-FILE NEXT RECORD
            AT END 
                  MOVE "Y" TO W-END-OF-FILE.

       IF END-OF-FILE  
          PERFORM FINALIZE-PAGE
       ELSE
          IF PAGE-FULL            
             PERFORM FINALIZE-PAGE
             PERFORM PRINT-HEADINGS. 
*>_________________________________________________________________________
 
COPY "PL-LOOK-FOR-STATE-RECORD.CBL".
COPY "PLPRINT.CBL".
*>_________________________________________________________________________






