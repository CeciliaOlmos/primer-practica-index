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
           03 pag-fecha pic 9(8).

       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            OPEN OUTPUT PAGOS.
            CLOSE PAGOS.
            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
