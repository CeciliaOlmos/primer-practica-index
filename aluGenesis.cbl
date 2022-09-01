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
           SELECT ALUMNOS
               ASSIGN TO "..\alumnos.dat"
                ORGANISATION IS INDEXED
                ACCESS MODE IS SEQUENTIAL
                RECORD KEY IS alu-llave.
       DATA DIVISION.
       FILE SECTION.
       FD  ALUMNOS.
       01  alu-reg.
           03 alu-llave.
               05 alu-dni pic 9(8).
               05 alu-est pic x.
           03 alu-legajo pic 9(5).
           03 alu-nombre pic x(20).
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            PERFORM 100-ABRIR-ARCHIVO.
            PERFORM 200-CERRAR-ARCHIVO.
            STOP RUN.

       100-ABRIR-ARCHIVO.
           OPEN OUTPUT ALUMNOS.
       200-CERRAR-ARCHIVO.
           CLOSE ALUMNOS.
       END PROGRAM YOUR-PROGRAM-NAME.
