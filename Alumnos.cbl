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
           SELECT ALUMNOS ASSIGN TO "..\alumnos.dat"
           ORGANISATION IS INDEXED
           ACCESS MODE is DYNAMIC
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
           PERFORM 100-INICIO.
           PERFORM 200-INGRESO.
           PERFORM UNTIL alu-dni = 0
               PERFORM 300-INGRESO-RESTO
               PERFORM 400-GRABO-REGISTRO
              PERFORM 200-INGRESO
           END-PERFORM.
           PERFORM 500-FIN.
            STOP RUN.
       100-INICIO.
           OPEN I-O ALUMNOS.
       200-INGRESO.
           DISPLAY "Ingrese DNI del alumno (x fin 0)".
           ACCEPT alu-dni.
       300-INGRESO-RESTO.
           DISPLAY "Ingrese los siguientes datos del alumno:".
           DISPLAY "estado de pago ".
           ACCEPT alu-est.
           DISPLAY "legajo del alumno".
           ACCEPT alu-legajo.
           DISPLAY "nombre alumno ".
           ACCEPT alu-nombre.
       400-GRABO-REGISTRO.
           WRITE alu-reg.
       500-FIN.
           CLOSE ALUMNOS.
       END PROGRAM YOUR-PROGRAM-NAME.
