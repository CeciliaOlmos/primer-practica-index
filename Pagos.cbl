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
           ORGANIZATION INDEXED
           ACCESS MODE is DYNAMIC
           RECORD KEY IS pagos-llave.
       DATA DIVISION.
       FILE SECTION.
       FD  PAGOS.
       01  pag-reg.
           03 pagos-llave.
               05 pag-curso pic 99.
               05 pag-cta-cte pic 9(5).
               05 pag-cuota pic 99.
           03 pag-fec-venc pic 9(8).
           03 pag-fec-pago pic 9(8).
           03 pag-importe pic 9(6)v99.
       WORKING-STORAGE SECTION.
       77  w-cta-anterior pic 99.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 100-INICIO.
           PERFORM 200-INGRESO.
              PERFORM UNTIL  pag-curso =0
               PERFORM 220-INICIO-ALU
               PERFORM UNTIL  pag-curso =0
               or pag-cuota IS EQUAL w-cta-anterior
                       PERFORM 300-INGRESO-RESTO
                        PERFORM 400-GRABO-REGISTRO
                       PERFORM 200-INGRESO
             end-perform
           END-PERFORM.
           PERFORM 500-FIN.
            STOP RUN.
        100-INICIO.
           OPEN I-O PAGOS.
       200-INGRESO.
           DISPLAY "Ingrese NRO de curso (x fin 0)".
           ACCEPT pag-curso.
       220-INICIO-ALU.
           DISPLAY "cuenta corriente ".
           ACCEPT pag-cta-cte.
           MOVE pag-cta-cte to w-cta-anterior.
       300-INGRESO-RESTO.
           DISPLAY "Ingrese los siguientes datos del alumno:".
           DISPLAY "cuenta corriente ".
           ACCEPT pag-cta-cte.
           DISPLAY "nro cuota".
           ACCEPT pag-cuota.
           DISPLAY "fecha de vencimiento".
           ACCEPT pag-fec-venc.
           display "fecha de pago".
           accept pag-fec-pago.
           display "importe de la cuota".
           accept pag-importe.
       400-GRABO-REGISTRO.
           WRITE pag-reg.
       500-FIN.
           CLOSE PAGOS.
       END PROGRAM YOUR-PROGRAM-NAME.
