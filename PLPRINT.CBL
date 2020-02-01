*>----------------------------------------------------
*> These procedure is used by:
*>   
*>     PLSORT.CBL
*>     print-vendor-by-number.cob
*>________________________________________________________________________

FINALIZE-PAGE.

   MOVE SPACES TO PRINTER-RECORD.
   WRITE PRINTER-RECORD BEFORE ADVANCING PAGE.
*>_________________________________________________________________________

PRINT-HEADINGS.
        
       ADD 1 TO PAGE-NUMBER.
       MOVE TITLE TO PRINTER-RECORD.
       WRITE PRINTER-RECORD BEFORE ADVANCING 1.
       MOVE SPACES TO PRINTER-RECORD.
       WRITE PRINTER-RECORD AFTER ADVANCING 2.
       MOVE HEADING-1 TO PRINTER-RECORD.
       WRITE PRINTER-RECORD BEFORE ADVANCING 1.
       MOVE HEADING-2 TO PRINTER-RECORD.
       WRITE PRINTER-RECORD BEFORE ADVANCING 1.

       MOVE 6 TO W-PRINTED-LINES.
*>_________________________________________________________________________
 

