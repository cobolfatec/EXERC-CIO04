      *DATE-WRITTEN. 02/04/2019.
      *--------------------------------------------------------------*
      * DISCIPLINA PROGRAMACAO MAINFRAME
      *--------------------------------------------------------------*
      * OBJETIVO: RECEBER DADOS DA SYSIN(ACCEPT)
      *           CALCULAR A MEDIA ARITMETICA BIMESTRAL
      *--------------------------------------------------------------*
      *------------------> HISTORICO - MANUTENCAO <------------------*
      * VERSAO  MES/ANO  NR.DOC  IDENT.  DESCRICAO
      * ------  -------  ------  ------  -------------------------   *
      *  V01    FEV/2013 010001  SISTEMA MOSTRA SYSOUT
      *  V02    ABR/2019 01000   SISTEMA MOSTRA SYSOUT COM FORMATACAO*
      *--------------------------------------------------------------*
       ENVIRONMENT DIVISION.
      *====================*
       CONFIGURATION SECTION.
      *---------------------*
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA
           CURRENCY SIGN IS "R$ " WITH PICTURE SYMBOL "$"
           .
       INPUT-OUTPUT SECTION.
      *---------------------*
       DATA DIVISION.
      *=============*
       FILE SECTION.
      *------------*
       WORKING-STORAGE SECTION.
      *-----------------------*
       01  FILLER                 PIC X(35)        VALUE
           '**** INICIO DA WORKING-STORAGE ****'.

      *-----> VARIAVEIS AUXILIARES UTILIZADA NO PROCESSAMENTO
       01  WS-AREA-AUX.
           05  WS-FIM                 PIC X(01).
           05  WS-CTLIDO              PIC 9(02).
           05  WS-MEDIA               PIC 9(02)V99.
           05  WS-MEDIAT              PIC 9(02).
           05  WS-SEX                 PIC 9(02).
           05  WS-ABMED               PIC 9(02).
           05  WS-MOSTRAN             PIC 9(02).
           05  WS-MOSTRAP             PIC 9(02)V99.
           05  WS-MOSTRAR             PIC 9(02)V99.
      *-----> ENTRADA - DADOS VIA SYSIN (NO JCL DE EXECUCAO)
       01  WS-REG-SYSIN.
           05 WS-NUMERO-IN        PIC 9(04).
           05 WS-NOME-IN          PIC X(20).
           05 WS-SEXO-IN          PIC X(01).
           05 WS-IDADE-IN         PIC 9(02).
           05 WS-CURSO-IN         PIC X(12).
           05 WS-NOTA1-IN         PIC 9(02)V99.
           05 WS-NOTA2-IN         PIC 9(02)V99.
      *-----> SAIDA - DADOS VIA SYSOUT (NO JCL DE EXECUCAO)
       01  WS-REG-SYSOUT.
           05 WS-NUM              PIC 9(04).
           FILLER                 PIC X(01) VALUES SPACES.
           05 WS-NOM              PIC X(20).
           FILLER                 PIC X(01) VALUES SPACES.
           05 WS-SEX              PIC X(01).
           FILLER                 PIC X(01) VALUES SPACES.
           05 WS-IDA              PIC 9(02).
           FILLER                 PIC X(01) VALUES SPACES.
           05 WS-CUR              PIC X(12).
           FILLER                 PIC X(01) VALUES SPACES.
           05 WS-NOT1-IN          PIC Z9V99.
           FILLER                 PIC X(01) VALUES SPACES.
           05 WS-NOT2-IN          PIC Z9V99.
           FILLER                 PIC X(01) VALUES SPACES.
           WS-MED                 PIC Z9V99.

       01  FILLER                 PIC X(35)        VALUE
           '****** FIM DA WORKING-STORAGE *****'.
      *
       PROCEDURE DIVISION.
      *==================*
      *--------------------------------------------------------------*
      *    PROCESSO PRINCIPAL
      *--------------------------------------------------------------*
       000-RSPRG002.

           PERFORM 010-INICIAR
           PERFORM 030-PROCESSAR UNTIL WS-FIM = 'S'
           PERFORM 090-TERMINAR
           STOP RUN
           .
      *--------------------------------------------------------------*
      *    PROCEDIMENTOS INICIAIS
      *--------------------------------------------------------------*
       010-INICIAR.

           PERFORM 025-LER-SYSIN
           .
      *--------------------------------------------------------------*
      *    LEITURA DADOS DA SYSIN
      *--------------------------------------------------------------*
       025-LER-SYSIN.

           ACCEPT WS-REG-SYSIN  FROM SYSIN

           IF WS-REG-SYSIN = ALL '9'
              MOVE   'S'     TO  WS-FIM
           ELSE
              ADD 1  TO WS-CTLIDO
           END-IF
           .
      *--------------------------------------------------------------*
      *    PROCESSAR DADOS RECEBIDOS DA SYSIN ATE FIM DOS REGISTROS
      *--------------------------------------------------------------*
       030-PROCESSAR.

           COMPUTE WS-MEDIA = (WS-NOTA1-IN + WS-NOTA2-IN) / 2
           DISPLAY WS-REG-SYSIN
           DISPLAY WS-MEDIA
           WS-MEDIAT ADD WS-MEDIA.
      
      *    VERIFICADOR SE SEXO E MASCULINO    *
           IF   AS-SEXO-IN  = 'M'
              ADD 1  TO WS-SEX
           END IF.
      *    VERIFICAR SE ALUNO ESTA ABAIXO DA MEDIA   *
           IF   WS-MED < 6
              ADD 1  TO WS-ABMED
           END IF.
           PERFORM 025-LER-SYSIN
           .
      *--------------------------------------------------------------*
      *    PROCEDIMENTOS FINAIS
      *--------------------------------------------------------------*
       090-TERMINAR.

           DISPLAY ' *========================================*'
           DISPLAY ' *   TOTAIS DE CONTROLE - CGPRG004        *'
           DISPLAY ' *----------------------------------------*'
           DISPLAY ' * REGISTROS LIDOS        = ' WS-CTLIDO
           COMPUTE  WS-MOSTRAN = WS-CTLIDO - WS-SEX 
           DISPLAY ' * TOTAL DE MULHERES      = ' WS-MOSTRAN
           COMPUTE  WS-MOSTRAR = WS-MEDIAT / WS-CTLIDO
           DISPLAY ' * MEDIA GERAL DOS ALUNOS = '  WS-MOSTRAR
           DISPLAY ' TOTAL DE ALUNOS ABAIXO DA MEDIA 6,00 = ' WS-ABMED
           COMPUTE  WS-MOSTRARP = ( WS-ABMED / WS-CTLIDO ) * 100
           DISPLAY ' * PORCENTAGEM DE ALUNOS ABAIXO DA MEDIA 6,00 = ' WS-MOSTRARP
           DISPLAY ' *========================================*'
           DISPLAY ' *----------------------------------------*'
           DISPLAY ' *      TERMINO NORMAL DO RSPRG002        *'
           DISPLAY ' *----------------------------------------*'
           .
      *---------------> FIM DO PROGRAMA RSPRG002 <-------------------*
