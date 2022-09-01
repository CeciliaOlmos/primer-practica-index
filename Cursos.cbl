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
                   ACCESS MODE is DYNAMIC
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
            PERFORM 100-INICIO.
           PERFORM 200-INGRESO.
           PERFORM UNTIL cur-anio = 0
               PERFORM 300-INGRESO-RESTO
               PERFORM 400-GRABO-REGISTRO
              PERFORM 200-INGRESO
           END-PERFORM.
           PERFORM 500-FIN.
            STOP RUN.
       100-INICIO.
           OPEN I-O CURSOS.
       200-INGRESO.
           DISPLAY "Ingrese anio que cursa el alumno (x fin 0)".
           ACCEPT cur-anio.
       300-INGRESO-RESTO.
           DISPLAY "Ingrese los siguientes datos del alumno:".
           DISPLAY "Numero de curso".
           ACCEPT cur-curso.
           DISPLAY "Numero de Orden del alumno".
           ACCEPT cur-orden.
           DISPLAY "Legajo".
           ACCEPT cur-legajo.
           DISPLAY "Nro de cta cte dentro del curso".
           ACCEPT cur-cta-cte.
       400-GRABO-REGISTRO.
           WRITE cur-reg INVALID KEY DISPLAY "NO PUDE GRABAR"
           END-WRITE.
       500-FIN.
           CLOSE CURSOS.


       END PROGRAM YOUR-PROGRAM-NAME.
