      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION. SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT NOVEDADES ASSIGN TO "..\novedades.dat"
           ORGANIZATION LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  NOVEDADES.
       01  nov-reg.
           03 nov-dni pic 9(8).
           03 nov-cuota pic 99.
           03 nov-fec-pago pic 9(8).
           03 nov-importe pic 9(6)v99.

       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
