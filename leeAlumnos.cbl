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
       77  sen pic 9 value 0.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN input ALUMNOS.
           perform leo.
           perform until sen = 1
               display "DNI    ", alu-dni
               display "estado ", alu-est
               DISPLAY "legajo ",alu-legajo
               DISPLAY "Nombre ",alu-nombre
               DISPLAY "--------------"
               perform leo
            END-PERFORM.
            close ALUMNOS.
            STOP RUN.
        leo.
           read ALUMNOS at end move 1 to sen.
       END PROGRAM YOUR-PROGRAM-NAME.
