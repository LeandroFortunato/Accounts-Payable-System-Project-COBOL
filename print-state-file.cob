IDENTIFICATION DIVISION.
PROGRAM-ID. print-state-file.
ENVIRONMENT DIVISION.
   INPUT-OUTPUT SECTION.
      FILE-CONTROL.

         COPY "SLSTATE.CBL".
         
         SELECT PRINTER-FILE
                ASSIGN TO "print-state-file.prn"
                ORGANIZATION IS LINE SEQUENTIAL.

DATA DIVISION.
   FILE SECTION.

         COPY "FDSTATE.CBL".

         FD PRINTER-FILE
            LABEL RECORDS ARE OMITTED.

         01 PRINTER-RECORD        PIC X(80).
         
   WORKING-STORAGE SECTION.

         01 TITLE.
            05 FILLER                   PIC X(23) VALUE SPACES.
            05 FILLER                   PIC X(19) VALUE "LIST OF STATE CODES".      
            05 FILLER                   PIC X(15) VALUE SPACES.
            05 FILLER                   PIC X(05) VALUE "PAG: ".      
            05 PAGE-NUMBER              PIC 9(03).

         01 HEADING-ITEMS.
            05 FILLER                   PIC X(11) VALUE SPACES.
            05 FILLER                   PIC X(04) VALUE "CODE".    
            05 FILLER                   PIC X(04) VALUE SPACES.
            05 FILLER                   PIC X(05) VALUE "STATE".    
   
       01 HEADING-LINE.
            05 FILLER                   PIC X(11) VALUE SPACES.
            05 FILLER                   PIC X(04) VALUE "====".    
            05 FILLER                   PIC X(04) VALUE SPACES.
            05 FILLER                   PIC X(20) VALUE "====================". 
 
        01 DETAIL-1.
            05 FILLER                   PIC X(12) VALUE SPACES.
            05 D-STATE-CODE             PIC X(02).    
            05 FILLER                   PIC X(05) VALUE SPACES.
            05 D-STATE-NAME             PIC X(20). 
  

        01 W-END-OF-FILE                PIC X.
           88 END-OF-FILE               VALUE "Y".

        01 W-PRINTED-LINES              PIC 99.    
           88 PAGE-FULL                 VALUE 50 THROUGH 99.
*>_________________________________________________________________________

PROCEDURE DIVISION.
 
     OPEN INPUT STATE-FILE.
     OPEN OUTPUT PRINTER-FILE.

     MOVE 0 TO PAGE-NUMBER.
     MOVE "N" TO W-END-OF-FILE.

     PERFORM PRINT-HEADINGS.     

     PERFORM READ-STATE-FILE-NEXT-RECORD.
 
     IF END-OF-FILE 
        MOVE "NO RECORDS IN THE VENDOR FILE !" TO PRINTER-RECORD
        WRITE PRINTER-RECORD BEFORE ADVANCING 1.

     PERFORM PRINT-A-RECORD UNTIL END-OF-FILE.

     PERFORM FINALIZE-PAGE.

     CLOSE STATE-FILE.
     CLOSE PRINTER-FILE.

     EXIT PROGRAM.
     STOP RUN.
*>_________________________________________________________________________

PRINT-A-RECORD. 

      IF PAGE-FULL            
         PERFORM FINALIZE-PAGE
         PERFORM PRINT-HEADINGS. 
 
       MOVE STATE-CODE       TO D-STATE-CODE.
       MOVE STATE-NAME       TO D-STATE-NAME.

       MOVE DETAIL-1 TO PRINTER-RECORD.
       WRITE PRINTER-RECORD AFTER ADVANCING 1.
  
       ADD 1 TO W-PRINTED-LINES. 

       PERFORM READ-STATE-FILE-NEXT-RECORD.     
*>_________________________________________________________________________

READ-STATE-FILE-NEXT-RECORD.

     READ STATE-FILE NEXT RECORD
        AT END 
               MOVE "Y" TO W-END-OF-FILE.
*>_________________________________________________________________________

FINALIZE-PAGE.
       MOVE SPACES TO PRINTER-RECORD.
       WRITE PRINTER-RECORD BEFORE ADVANCING PAGE.
*>_________________________________________________________________________

PRINT-HEADINGS.
        
       ADD 1 TO PAGE-NUMBER.
       MOVE TITLE TO PRINTER-RECORD.
       WRITE PRINTER-RECORD BEFORE ADVANCING 1.

       MOVE HEADING-ITEMS TO PRINTER-RECORD.
       WRITE PRINTER-RECORD AFTER ADVANCING 3.

       MOVE HEADING-LINE TO PRINTER-RECORD.
       WRITE PRINTER-RECORD AFTER ADVANCING 1.

       MOVE 5 TO W-PRINTED-LINES.
*>_________________________________________________________________________

    
 
