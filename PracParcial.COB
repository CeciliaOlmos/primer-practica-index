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
           ACCESS MODE is RANDOM
           record KEY is curso-llave
           ALTERNATE record key is cur-legajo WITH DUPLICATES.

           SELECT ALUMNOS ASSIGN TO "..\alumnos.dat"
           ORGANIZATION INDEXED
           ACCESS MODE is DYNAMIC
           record KEY is alu-llave.

           SELECT PAGOS ASSIGN TO "..\pagos.dat"
           ORGANIZATION INDEXED
           ACCESS MODE is DYNAMIC
           record KEY is pagos-llave.

           SELECT NOVEDADES ASSIGN TO "..\novedades.dat"
           ORGANIZATION LINE SEQUENTIAL.

           SELECT ERRORES ASSIGN TO "..\errores.txt"
           ORGANIZATION LINE SEQUENTIAL.
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

       FD  ALUMNOS.
       01  alu-reg.
           03 alu-llave.
               05 alu-dni pic 9(8).
               05 alu-est pic x.
           03 alu-legajo pic 9(5).
           03 alu-nombre pic x(20).

       FD  PAGOS.
       01  pag-reg.
           03 pagos-llave.
               05 pag-curso pic 99.
               05 pag-cta-cte pic 9(5).
               05 pag-cuota pic 99.
           03 pag-fec-venc pic 9(8).
           03 pag-fec-pago pic 9(8).
           03 pag-importe pic 9(6)v99.

       FD  NOVEDADES.
       01  nov-reg.
           03 nov-dni pic 9(8).
           03 nov-cuota pic 99.
           03 nov-fec-pago pic 9(8).
           03 nov-importe pic 9(6)v99.
       FD  ERRORES.
       01  err-reg.
           03 err-dni pic 9(8).
           03 err-cuota pic 99.
           03 err-fec-pago pic 9(8).
           03 err-importe pic 9(6)v99.
           03 err-tipo pic x(30).
       WORKING-STORAGE SECTION.

       01  W-FLAG-NOVEDADES PIC 9.
           88 FIN-DE-ARCH-NOV VALUE 1.

       01  W-FLAG-ALUMNOS PIC 9.
           88 FIN-DE-ARCH-ALUM VALUE 1.

       01  W-FLAG-PAGOS PIC 9.
           88 FIN-DE-ARCH-PAGOS VALUE 1.

       01  W-SIGUIENTE PIC 9 VALUE 0.
           88 EXISTE-SIG VALUE 1.

       77  W-CUOTA-ANT PIC 99.
       77  W-CUOTA-POST PIC 99.

       01  w-nombre PIC X(20).

       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
           PERFORM 100-INICIO.
           PERFORM 200-LEER-NOVEDADES.
           PERFORM UNTIL FIN-DE-ARCH-NOV
               PERFORM 300-PROCESO-GRAL
               PERFORM 200-LEER-NOVEDADES
           END-PERFORM.
           PERFORM 400-FIN.
           STOP RUN.

       100-INICIO.
           OPEN INPUT NOVEDADES CURSOS.
           OPEN I-O ALUMNOS PAGOS.
           OPEN OUTPUT ERRORES.

       200-LEER-NOVEDADES.
           READ NOVEDADES AT END MOVE 1 TO W-FLAG-NOVEDADES.

       300-PROCESO-GRAL.
           PERFORM 330-TRAER-INFO.
       330-TRAER-INFO.
           PERFORM 332-BUSCO-DNI-ALUMNOS.

       332-BUSCO-DNI-ALUMNOS.
           PERFORM 336-ARMAR-CLAVE-ALUMNO.
           START ALUMNOS KEY IS = alu-dni INVALID KEY
                           PERFORM 333-ARMO-ARCH-ERRORES
                           move " DNI inexistente" to err-tipo
                         NOT INVALID KEY
                         PERFORM 337-LEER-ALUMNOS
                         PERFORM 338-VERIFICAR-CUENTA-ACTIVA.
       333-ARMO-ARCH-ERRORES.
           MOVE nov-dni to err-dni
           MOVE nov-cuota to err-cuota
           move nov-fec-pago to err-fec-pago
           move nov-importe to err-importe
           WRITE err-reg.
       336-ARMAR-CLAVE-ALUMNO.
           MOVE nov-dni TO alu-dni.
           MOVE ' ' TO alu-est.
       337-LEER-ALUMNOS.
           READ ALUMNOS NEXT AT END MOVE 1 TO W-FLAG-ALUMNOS.
       338-VERIFICAR-CUENTA-ACTIVA.
           IF alu-est NOT = 'C' PERFORM 380-BUSCO-LEGAJO-EN-CURSO
               ELSE
                   MOVE " Cuenta Cancelada" to err-tipo
                   PERFORM 333-ARMO-ARCH-ERRORES

           END-IF.

       380-BUSCO-LEGAJO-EN-CURSO.
           MOVE alu-legajo TO cur-legajo.
           PERFORM 381-LEER-CURSOS.

       381-LEER-CURSOS.
           READ CURSOS KEY IS cur-legajo
                       INVALID KEY
                       MOVE " Legajo no encontrado" to err-tipo
                       PERFORM 333-ARMO-ARCH-ERRORES
                       NOT INVALID KEY PERFORM 382-BUSCAR-CTA-CTE.

       382-BUSCAR-CTA-CTE.
           PERFORM 383-ARMAR-CLAVE-PAGO.
       393-ME-POSICIONO-EN-ARCHIVO.
           START PAGOS KEY IS = pagos-llave
                        INVALID KEY
                        MOVE " LA CUENTA NO EXISTE" to err-tipo
                        PERFORM 333-ARMO-ARCH-ERRORES
                         NOT INVALID KEY
                         PERFORM 390-LEER-PAGOS.

       383-ARMAR-CLAVE-PAGO.
           MOVE cur-cta-cte TO pag-cta-cte
           MOVE cur-curso TO pag-curso
           MOVE nov-cuota TO pag-cuota
           IF NOT nov-cuota = 1
               COMPUTE W-CUOTA-ANT = nov-cuota - 1
               MOVE W-CUOTA-ANT TO pag-cuota
               PERFORM 393-ME-POSICIONO-EN-ARCHIVO
               PERFORM 384-VERIFICO-PAGO-ANTERIOR
           ELSE
               PERFORM 393-ME-POSICIONO-EN-ARCHIVO
               PERFORM 385-VERIFICO-RESTO-DATOS
           END-IF.
       384-VERIFICO-PAGO-ANTERIOR.
      *    si la fecha anterior es mayor a cero, esta paga esa cuota
      *    leo y me encuentro en la cuota actual
           IF pag-fec-pago > 0
               PERFORM 390-LEER-PAGOS
               PERFORM 385-VERIFICO-RESTO-DATOS
           ELSE
               MOVE " Cuota anterior impaga" to err-tipo
               PERFORM 333-ARMO-ARCH-ERRORES
           END-IF.
       390-LEER-PAGOS.
           READ PAGOS NEXT AT END MOVE 1 TO W-FLAG-PAGOS.

       385-VERIFICO-RESTO-DATOS.
           IF nov-cuota=pag-cuota
               IF pag-fec-pago=0
                   IF nov-importe=pag-importe
                       PERFORM 394-ACTUALIZO-PAGO
                       PERFORM 391-BUSCAR-SIGUIENTE
                   ELSE
                   MOVE " Difieren los importes" to err-tipo
                   PERFORM 333-ARMO-ARCH-ERRORES
               ELSE
               MOVE " La cuota esta pagada" to err-tipo
               PERFORM 333-ARMO-ARCH-ERRORES
           ELSE
            MOVE " La cuota NO EXISTE" to err-tipo
            PERFORM 333-ARMO-ARCH-ERRORES
           END-IF.
       394-ACTUALIZO-PAGO.
           MOVE nov-fec-pago to pag-fec-pago.
           REWRITE pag-reg.
       391-BUSCAR-SIGUIENTE.
           PERFORM 390-LEER-PAGOS.
           IF pag-cta-cte <> cur-cta-cte OR W-FLAG-PAGOS =0
               DISPLAY "No tiene mas cuotas".
               PERFORM 392-CAMBIAR-ESTADO-ALUMNO.

       392-CAMBIAR-ESTADO-ALUMNO.
           MOVE alu-nombre TO w-nombre.
           DELETE ALUMNOS.
           MOVE "C" TO alu-est.
           MOVE w-nombre TO alu-nombre.
           MOVE nov-dni TO alu-dni.
           MOVE cur-legajo TO alu-legajo.
           WRITE alu-reg.

       400-FIN.
           CLOSE NOVEDADES
           CLOSE CURSOS
           CLOSE ALUMNOS
           CLOSE PAGOS.
           CLOSE ERRORES.
       END PROGRAM YOUR-PROGRAM-NAME.
