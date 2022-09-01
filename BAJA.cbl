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
           ACCESS MODE is RANDOM
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
       77  w-acuerdo-eliminado pic x.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            PERFORM INICIO-GENERAL.
            PERFORM PEDIR-SOCIO-CODIGO.
            PERFORM UNTIL pagos-llave IS EQUALS ZERO
               PERFORM BUSCAR-REGISTRO
               PERFORM ANALIZAR-BAJA
               PERFORM PEDIR-SOCIO-CODIGO
            END-PERFORM.
            PERFORM FIN-GENERAL.
            STOP RUN.
       INICIO-GENERAL.
           PERFORM ABRIR-ARCHIVO.

       ABRIR-ARCHIVO.
           OPEN I-O PAGOS.
       PEDIR-SOCIO-CODIGO.
           DISPLAY "CURSO".
           ACCEPT pag-curso.
           DISPLAY "INGRESE CTA CTE QUE DESEA ELIMINAR. x FIN "-
           "INGRESE 0".
           ACCEPT pag-cta-cte.
           DISPLAY "CUOTA".
           ACCEPT pag-cuota.

       BUSCAR-REGISTRO.
           PERFORM LEER-PAGOS.

       LEER-PAGOS.
           READ PAGOS INVALID KEY
                       DISPLAY "ALUMNO INEXISTENTE"
                       NOT INVALID KEY
                       PERFORM MOSTRAR-REGISTRO.

       MOSTRAR-REGISTRO.
           DISPLAY "CTA-CTE: ", pag-cta-cte, " PAG-CURSO: ", pag-curso.
           DISPLAY "PAG-FEC-VEN",pag-fec-venc,"PAGO",pag-fec-pago.
           DISPLAY "IMPORTE",pag-importe.
           DISPLAY "Estas seguro que desea eliminar el registro?? "-
           "S(SI), N(NO)".
           ACCEPT w-acuerdo-eliminado.

       ANALIZAR-BAJA.
           IF w-acuerdo-eliminado IS EQUALS 'S' THEN
               DELETE PAGOS
           END-IF.

       FIN-GENERAL.
           PERFORM CERRAR-ARCHIVO.

       CERRAR-ARCHIVO.
           CLOSE PAGOS.
       END PROGRAM YOUR-PROGRAM-NAME.
