      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CURSOS ASSIGN TO "..\cursos.dat"
           ORGANIZATION INDEXED
           ACCESS MODE is SEQUENTIAL
           record KEY is curso-llave
           ALTERNATE record key is cur-legajo WITH DUPLICATES.
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
       77  sen pic 9 value 0.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN input CURSOS.
           perform leo.
           perform until sen = 1
               display "anio    ", cur-anio
               display "curso   ", cur-curso
               DISPLAY "orden   ",cur-orden
               DISPLAY "legajo  ",cur-legajo
               DISPLAY "CTA CTE ",cur-cta-cte
               DISPLAY "--------------"
               perform leo
            END-PERFORM.
            close CURSOS.
            STOP RUN.
       leo.
           read CURSOS at end move 1 to sen.


       END PROGRAM YOUR-PROGRAM-NAME.
