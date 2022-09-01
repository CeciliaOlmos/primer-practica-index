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
                   ACCESS MODE IS RANDOM
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
       77  w-acuerdo-eliminado pic x.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            PERFORM INICIO-GENERAL.
            PERFORM PEDIR-SOCIO-CODIGO.
            PERFORM UNTIL alu-llave IS EQUALS ZERO
               PERFORM BUSCAR-REGISTRO
               PERFORM ANALIZAR-BAJA
               PERFORM PEDIR-SOCIO-CODIGO
            END-PERFORM.
            PERFORM FIN-GENERAL.
            STOP RUN.
        INICIO-GENERAL.
           PERFORM ABRIR-ARCHIVO.

       ABRIR-ARCHIVO.
           OPEN I-O ALUMNOS.
       PEDIR-SOCIO-CODIGO.
           DISPLAY "DNI".
           ACCEPT alu-dni.
           DISPLAY "ESTADO".
           ACCEPT alu-est.

       BUSCAR-REGISTRO.
           PERFORM LEER-ALUMNOS.

       LEER-ALUMNOS.
           READ ALUMNOS INVALID KEY
                       DISPLAY "ALUMNO INEXISTENTE"
                       NOT INVALID KEY
                       PERFORM MOSTRAR-REGISTRO.

       MOSTRAR-REGISTRO.
           DISPLAY "DNI", alu-dni, " ESTADO ", alu-est.
           DISPLAY "alu-legajo",alu-legajo,"NOMBRE",alu-nombre.
           DISPLAY "Estas seguro que desea eliminar el registro?? "-
           "S(SI), N(NO)".
           ACCEPT w-acuerdo-eliminado.
       ANALIZAR-BAJA.
           IF w-acuerdo-eliminado IS EQUALS 'S' THEN
               DELETE ALUMNOS
           END-IF.

       FIN-GENERAL.
           PERFORM CERRAR-ARCHIVO.

       CERRAR-ARCHIVO.
           CLOSE ALUMNOS.
       END PROGRAM YOUR-PROGRAM-NAME.
