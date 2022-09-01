      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION. SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
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

           SELECT NOVEDADES ASSIGN TO "..\novedades.txt"
           ORGANIZATION LINE SEQUENTIAL.

           SELECT ERRORES ASSIGN TO "..\errores.dat"
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

       01  linea-titulo.
           03 filler pic x(28).
           03 filler pic x(24) value "LISTADO DE MOVIMIENTOS".
           03 filler pic x(28) value spaces.
       01  lin-guarda.
           03 filler pic x(80) value all "-".
       01  lin-cabecera.
           03 filler pic x(5) value spaces.
           03 filler pic x(10) value "DNI ALUMNO".
           03 filler pic x(4) value spaces.
           03 filler pic x(5) value "CUOTA".
           03 filler pic x(3) value spaces.
           03 filler pic x(13) value "FECHA DE PAGO".
           03 filler pic x(5) value spaces.
           03 filler pic x(7) value "IMPORTE".
           03 filler pic x(5) value spaces.
           03 filler pic x(9) value "RESULTADO".
       01  lin-detalle.
           03 filler pic x(5) value spaces.
           03 l-dni pic zzzzzzzz value spaces.
           03 filler pic x(5) value spaces.
           03 l-cuota pic zz.
           03 filler pic x(8) value spaces.
           03 l-fec-pago pic x(8) value spaces.
           03 filler pic x(7) value spaces.
           03 l-imp pic zzz.zz9,99.
           03 filler pic x(5) value spaces.
           03 l-tipo pic x(30).
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 100-INICIO.
           PERFORM 200-LEER-NOVEDADES.
           PERFORM UNTIL FIN-DE-ARCH-NOV
               PERFORM 300-PROCESO-GRAL
               PERFORM 200-LEER-NOVEDADES
           END-PERFORM.
           PERFORM 450-FIN.
            STOP RUN.

       100-INICIO.
           PERFORM 120-ABRIR-ARCHIVOS.
           PERFORM 130-ENCABEZAR-LISTADO.

       120-ABRIR-ARCHIVOS.
           OPEN INPUT NOVEDADES CURSOS.
           OPEN I-O ALUMNOS PAGOS.
           OPEN OUTPUT ERRORES.

       130-ENCABEZAR-LISTADO.
           DISPLAY linea-titulo.
           DISPLAY lin-cabecera.
           DISPLAY lin-guarda.

       200-LEER-NOVEDADES.
           READ NOVEDADES AT END MOVE 1 TO W-FLAG-NOVEDADES.

       300-PROCESO-GRAL.
           PERFORM 332-BUSCO-DNI-ALUMNOS.

       332-BUSCO-DNI-ALUMNOS.
           PERFORM 336-ARMAR-CLAVE-ALUMNO.
           START ALUMNOS KEY IS = alu-dni INVALID KEY
                           PERFORM 320-PREPARAR-LISTADO
                         NOT INVALID KEY
                         PERFORM 337-LEER-ALUMNOS
                         PERFORM 338-VERIFICAR-CUENTA-ACTIVA.

       320-PREPARAR-LISTADO.
           move " DNI inexistente" to err-tipo, l-tipo
           PERFORM 331-ARMO-IMPRESION
           PERFORM 333-ARMO-ARCH-ERRORES.

       336-ARMAR-CLAVE-ALUMNO.
           MOVE nov-dni TO alu-dni.
           MOVE ' ' TO alu-est.

       331-ARMO-IMPRESION.
           MOVE nov-dni to l-dni.
           MOVE nov-cuota to l-cuota.
           move nov-fec-pago to l-fec-pago.
           move nov-importe to l-imp.
           DISPLAY lin-detalle.

       333-ARMO-ARCH-ERRORES.
           MOVE nov-dni to err-dni
           MOVE nov-cuota to err-cuota
           move nov-fec-pago to err-fec-pago
           move nov-importe to err-importe
           WRITE err-reg.

       337-LEER-ALUMNOS.
           READ ALUMNOS NEXT AT END MOVE 1 TO W-FLAG-ALUMNOS.

       338-VERIFICAR-CUENTA-ACTIVA.
           IF alu-est NOT = 'C' PERFORM 380-BUSCO-LEGAJO-EN-CURSO
               ELSE
                   PERFORM 386-PREPARO-LISTADO-2
           END-IF.

       386-PREPARO-LISTADO-2.
           MOVE " Cuenta Cancelada" to err-tipo, l-tipo
           PERFORM 333-ARMO-ARCH-ERRORES
           PERFORM 331-ARMO-IMPRESION.

       380-BUSCO-LEGAJO-EN-CURSO.
           MOVE alu-legajo TO cur-legajo.
           PERFORM 381-LEER-CURSOS.

       381-LEER-CURSOS.
           READ CURSOS KEY IS cur-legajo
                       INVALID KEY
                       PERFORM 387-PREPARAR-LISTADO-3
                       NOT INVALID KEY PERFORM 382-BUSCAR-CTA-CTE.

       387-PREPARAR-LISTADO-3.
           MOVE " Legajo no encontrado" to err-tipo, l-tipo
           PERFORM 331-ARMO-IMPRESION
           PERFORM 333-ARMO-ARCH-ERRORES.

       382-BUSCAR-CTA-CTE.
           PERFORM 383-ARMAR-CLAVE-PAGO.

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

       393-ME-POSICIONO-EN-ARCHIVO.
           START PAGOS KEY IS = pagos-llave
                        INVALID KEY
                        PERFORM 398-PREPARAR-LISTADO-4
                         NOT INVALID KEY
                         PERFORM 390-LEER-PAGOS.

       398-PREPARAR-LISTADO-4.
           MOVE " LA CUENTA NO EXISTE" to err-tipo, l-tipo
           PERFORM 331-ARMO-IMPRESION
           PERFORM 333-ARMO-ARCH-ERRORES.

       384-VERIFICO-PAGO-ANTERIOR.
           IF pag-fec-pago > 0
               PERFORM 390-LEER-PAGOS
               PERFORM 385-VERIFICO-RESTO-DATOS
           ELSE
               PERFORM 399-PREPARAR-LISTADO-5
           END-IF.

       399-PREPARAR-LISTADO-5.
           MOVE " Cuota anterior impaga" to err-tipo, l-tipo
           PERFORM 331-ARMO-IMPRESION
           PERFORM 333-ARMO-ARCH-ERRORES.

       390-LEER-PAGOS.
           READ PAGOS NEXT AT END MOVE 1 TO W-FLAG-PAGOS.

       385-VERIFICO-RESTO-DATOS.
           IF nov-cuota=pag-cuota
               IF pag-fec-pago=0
                   IF nov-importe=pag-importe
                       PERFORM 394-ACTUALIZO-PAGO
                       PERFORM 391-BUSCAR-SIGUIENTE
                   ELSE
                       PERFORM 400-PREPARAR-LISTADO-6
               ELSE
                   PERFORM 410-PREPARAR-LISTADO-7
           ELSE
               PERFORM 420-PREPARO-LISTADO-8
           END-IF.

       400-PREPARAR-LISTADO-6.
           MOVE " Difieren los importes" to err-tipo, l-tipo
           PERFORM 331-ARMO-IMPRESION
           PERFORM 333-ARMO-ARCH-ERRORES.

       410-PREPARAR-LISTADO-7.
           MOVE " La cuota esta pagada" to err-tipo, l-tipo

           PERFORM 331-ARMO-IMPRESION
           PERFORM 333-ARMO-ARCH-ERRORES.

       420-PREPARO-LISTADO-8.
            MOVE " La cuota NO EXISTE" to err-tipo, l-tipo
            PERFORM 333-ARMO-ARCH-ERRORES
            PERFORM 331-ARMO-IMPRESION.

       394-ACTUALIZO-PAGO.
           MOVE nov-fec-pago to pag-fec-pago.
           REWRITE pag-reg.

       391-BUSCAR-SIGUIENTE.
           PERFORM 390-LEER-PAGOS.
           IF pag-cta-cte <> cur-cta-cte OR FIN-DE-ARCH-PAGOS
               PERFORM 392-CAMBIAR-ESTADO-ALUMNO
               PERFORM 331-ARMO-IMPRESION
               MOVE "TODO OK" TO l-tipo.

       392-CAMBIAR-ESTADO-ALUMNO.
           MOVE alu-nombre TO w-nombre.
           DELETE ALUMNOS.
           MOVE "C" TO alu-est.
           WRITE alu-reg.

       450-FIN.
           CLOSE NOVEDADES CURSOS ALUMNOS PAGOS  ERRORES.
       END PROGRAM YOUR-PROGRAM-NAME.
