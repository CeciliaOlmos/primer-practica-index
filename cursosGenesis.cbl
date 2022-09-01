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
       SPECIAL-NAMES. DECIMAL-POINT is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CURSOS
               ASSIGN TO "..\cursos.dat"
                   ORGANISATION IS INDEXED
                   ACCESS MODE IS SEQUENTIAL
                   RECORD KEY IS curso-llave
                   ALTERNATE record key is cur-legajo with DUPLICATES.
       DATA DIVISION.
       FILE SECTION.
       FD  CURSOS.
       01  cur-reg.
           03 curso-llave.
              05 cur-anio pic 9.
              05 cur-curso pic 99.
              05 cur-orden pic 99.
           03 cur-legajo pic 9(5).
           03 cur-cta-cte pic 9(5).

       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            PERFORM 100-ABRIR-ARCHIVO.
            PERFORM 200-CERRAR-ARCHIVO.
            STOP RUN.

       100-ABRIR-ARCHIVO.
           OPEN OUTPUT CURSOS.
       200-CERRAR-ARCHIVO.
           CLOSE CURSOS.
       END PROGRAM YOUR-PROGRAM-NAME.
