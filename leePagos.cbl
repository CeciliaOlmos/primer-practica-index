      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT is COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PAGOS ASSIGN TO "..\pagos.dat"
           ORGANISATION IS INDEXED
           ACCESS MODE IS SEQUENTIAL
           RECORD KEY IS pagos-llave.
       DATA DIVISION.
       FILE SECTION.
       FD  PAGOS.
       01  pag-reg.
           03 pagos-llave.
               05 pag-curso pic 99.
               05 pag-cta-cte pic 9(5).
               05 pag-cuota pic 99.
           03 pag-fec-ven pic 9(8).
           03 pag-fec-pago pic 9(8).
           03 pag-importe pic 9(6)v99.
       WORKING-STORAGE SECTION.
       77  sen pic 9 value 0.
       77  w-anterior pic 9(5).
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN input PAGOS.
           perform leo.

           perform until sen = 1
             display "Curso ", pag-curso," Cta cte ", pag-cta-cte
              DISPLAY "Nro cuota ",pag-cuota
              DISPLAY "Fech.vto ",pag-fec-ven," Fech.pago ",pag-fec-pago
              DISPLAY "Importe   ",pag-importe
           DISPLAY "--------------------------------------"
              perform leo
           END-PERFORM.

            close PAGOS.
            STOP RUN.
           leo.
           read PAGOS at end move 1 to sen.
       END PROGRAM YOUR-PROGRAM-NAME.
