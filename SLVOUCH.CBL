*>--------------------------------
*> SLVOUCH.CBL
*>--------------------------------

SELECT VOUCHER-FILE
       ASSIGN TO "VOUCHER"
       ORGANIZATION IS INDEXED
       RECORD KEY IS VOUCHER-NUMBER
       ACCESS MODE IS DYNAMIC.
