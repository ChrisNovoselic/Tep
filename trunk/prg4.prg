PUSH KEY CLEAR
ON KEY LABEL F1 HELP ID 10
HIDE POPUP MAIN
DEFINE POPUPS POP4 FROM 01,21 IN SCREEN TITLE'[���� ������� ������]' COLOR ;
       bg/w*,n/w*,gr+/w,gr+/w,,w+/r* FONT "Courier New",11
DEFINE BAR 01 OF POP4 PROMPT '������� ������� �������            ' SKIP FOR FILE('inblok.dbf')
DEFINE BAR 02 OF POP4 PROMPT '���������� ������� �������         ' SKIP FOR .T. &&.NOT.FILE('inblok.dbf')
DEFINE BAR 03 OF POP4 PROMPT '������ �� ������� ������� ������ �� �.�.' SKIP FOR .NOT.FILE('inblok.dbf')
DEFINE BAR 04 OF POP4 PROMPT '��������������� ������� �������    ' SKIP FOR .NOT.FILE('inblok.dbf')
DEFINE BAR 05 OF POP4 PROMPT '�������� ������                    ' SKIP FOR .NOT.FILE('inblok.dbf')
DEFINE BAR 06 OF POP4 PROMPT '����������� �������� �������       '
DEFINE BAR 07 OF POP4 PROMPT '����������� ������� �������        ' SKIP FOR .NOT.FILE('inblok.dbf')
DEFINE BAR 08 OF POP4 PROMPT '����������� �������� �������       '
DEFINE BAR 09 OF POP4 PROMPT '������� ����� 15506                '
DEFINE BAR 10 OF POP4 PROMPT '����������� ����� 15506            '
DEFINE BAR 11 OF POP4 PROMPT '����������� ����� 15506            '
DEFINE BAR 12 OF POP4 PROMPT '������� ����� 3-���                '
DEFINE BAR 13 OF POP4 PROMPT '����������� ����� 3-���            '
DEFINE BAR 14 OF POP4 PROMPT '����������� ����� 3-���            '
DEFINE BAR 15 OF POP4 PROMPT '3-��� �� �������, �������, ���     '
DEFINE BAR 16 OF POP4 PROMPT '�������� 3-���                     '
DEFINE BAR 17 OF POP4 PROMPT '������ ������������� ������.����   '
DEFINE BAR 18 OF POP4 PROMPT 'MAX����� � MIN����� ������� �������' SKIP FOR .NOT.FILE('inblok.dbf')
DEFINE BAR 19 OF POP4 PROMPT '��� ������ ����-5                  ' SKIP FOR BL1 OR BL2 OR BL3 OR BL4 OR BL5 OR BL5
DEFINE BAR 20 OF POP4 PROMPT '������� ����                       '
ON SELECTION POPUPS POP4 DO tmenu4 WITH PROMPT()
DO scr WITH "����� �������� � ��������� - �� [F1]","w/n"
ACTIVATE POPUPS POP4
*****************************************************************************
*                          Procedure   tmenu4                               *
*****************************************************************************
PROCEDURE tmenu4
PARAMETERS MPROMPT
DO CASE
   CASE MPROMPT="������� ������� �������" && ������.�.� PRG3.PRG
      COPY FILE BASE\inblok.dbf TO inblok.dbf
      KEYBOARD "{DNARROW}"
   CASE MPROMPT="���������� ������� �������" && ������.�.� PRG3.PRG
      HIDE POPUP ALL
      DEFINE WINDOW pri FROM 0,0 TO 24,79 NONE color n/w,w+/r FONT "Courier New",10
      ACTIV WINDOW pri
      yn='A'
      DO WHILE .NOT.(UPPER(yn)='Y' OR UPPER(yn)='N')
        yn='N'
        @ 10,15 SAY "�� �������, ��� ��� ��� �����,(Y/N) " GET yn
        READ
      ENDDO
      RELEASE WIND pri
      IF UPPER(yn)='Y'
         DELETE FILE inblok.dbf
         KEYBOARD "{UPARROW}"
      ENDIF
   CASE MPROMPT="��������������� ������� �������" && ������.�.� PRG3.PRG
      PUSH KEY CLEAR
      ON KEY LABEL S do s
      FormNum="1"
      DO FORM TABLE1 WITH "inblok",.T.
      ON KEY LABEL S
      POP KEY
   CASE MPROMPT="������ �� ������� ������� ������ �� �.�." && ������.�.� PRG3.PRG
      HIDE POPUP ALL
      _Screen.MousePointer=11
      error=0
      on error store error() to error
      SET DEFAULT TO (KTSpath)
      on error 
      SET DEFAULT TO (DefaultDir)
      IF error=0
         USE inblok
         file_name='par_el'
         do forma dir
         file_name='par_el5msc'
         do forma dir
         file_name='par_e6'
         do forma dir
         file_name='par_e6msc'
         do forma dir
         file_name='tep0'
         do forma dir
         file_name='par_tp'
         do forma dir
         file_name='par_tp5msc'
         do forma dir
         file_name='par_t6'
         do forma dir
         file_name='par_t6msc'
         do forma dir
         file_name='par_b1'
         do forma dir
         file_name='par_b1dms'
         do forma dir
         file_name='par_b6'
         do forma dir
         file_name='par_b6dms'
         do forma dir
         USE
      ELSE
         wait window '��� ������� � ���������. ������� ����� �������.'  
      ENDIF
      _Screen.MousePointer=0
   CASE MPROMPT="�������� ������" && ������.�.� PRG3.PRG
      do precalc.prg with 'outmkt','calc_m.prg'
   CASE MPROMPT="����������� �������� �������" && ������.�.� PRG3.PRG
      PUSH KEY CLEAR
      ON KEY LABEL = do ravno
      FormNum="3"
      DO FORM TABLE1 WITH "outmkt",.T.
      ON KEY LABEL =
      POP KEY
   CASE MPROMPT="����������� ������� �������" && ������.�.� PRG3.PRG
      DO PRINT WITH "inblok",'������� ������� ��� ������'
   CASE MPROMPT="����������� �������� �������" && ������.�.� PRG3.PRG
      DO PRINT WITH "outmkt",'������� ������� ������ 15506'
   CASE MPROMPT="������� ����� 15506"
      _Screen.MousePointer=11
      DO CR_FORM WITH 8,'IN15506','OUT15506'
      _Screen.MousePointer=0
   CASE MPROMPT="����������� ����� 15506"
      DEFINE POPUPS s15506 FROM 04,22 TITLE'[���� ��������� �������� ����]' ;
      COLOR gr+/w,n/w,gr+/w,gr+/w,,w+/r*  FONT "Courier New",10
      DEFINE BAR 01 OF s15506 PROMPT '����1            '
      DEFINE BAR 02 OF s15506 PROMPT '����2            '
      DEFINE BAR 03 OF s15506 PROMPT '����3            '
      DEFINE BAR 04 OF s15506 PROMPT '����4            '
      DEFINE BAR 05 OF s15506 PROMPT '����5            '
      DEFINE BAR 06 OF s15506 PROMPT '����6            '
      DEFINE BAR 07 OF s15506 PROMPT '����7            '
      DEFINE BAR 08 OF s15506 PROMPT '����8            '
      DEFINE BAR 09 OF s15506 PROMPT '���������� ����     '
      ON SELECTION POPUPS s15506 DEACT POPUP s15506
      HIDE POPUPS POP4,MAIN
      DO WHILE NOT(BAR()=0 OR BAR()=9)
      ACTIVATE POPUPS s15506 BAR BAR()
      IF BETWEEN(BAR(),1,8)
        file='FORMA\OUT15506.'+STR(BAR(),1)
        define window view from 1,0 to 25,80 title ' ' FONT "Courier New",10
        ACTIV SCREEN
        @ 0,0 SAY "����� ��������� - [ESC]             " COLOR gr+/n FONT "Courier New",10
        ON KEY LABEL ESC KEYB'{Ctrl+W}'
        SET SYSMENU ON
        HIDE MENU _MSYSMENU
        =INSMODE(.F.)
        MODIFY FILE &file WINDOW view
        ON KEY LABEL ESC
        SET SYSMENU OFF
        releas window view
*       run obview.com &file
        DO scr WITH "����� �������� � ��������� - �� [F1]","w/n"
      ENDIF
      ENDDO
   CASE MPROMPT="����������� ����� 15506"
      PRIV fontsize1
      fontsize1=7
      do forma ffontsize
      DEFINE POPUPS s15506 FROM 04,22 TITLE'[���� ������ �������� ����]' ;
      COLOR gr+/w,n/w,gr+/w,gr+/w,,w+/r* FONT "Courier New",10
      DEFINE BAR 01 OF s15506 PROMPT '����1            '
      DEFINE BAR 02 OF s15506 PROMPT '����2            '
      DEFINE BAR 03 OF s15506 PROMPT '����3            '
      DEFINE BAR 04 OF s15506 PROMPT '����4            '
      DEFINE BAR 05 OF s15506 PROMPT '����5            '
      DEFINE BAR 06 OF s15506 PROMPT '����6            '
      DEFINE BAR 07 OF s15506 PROMPT '����7            '
      DEFINE BAR 08 OF s15506 PROMPT '����8            '
      DEFINE BAR 09 OF s15506 PROMPT '������� ����     '
      ON SELECTION POPUPS s15506 DEACT POPUP s15506
      HIDE POPUPS POP4,MAIN
      DO WHILE NOT(BAR()=0 OR BAR()=9)
      ACTIVATE POPUPS s15506 BAR BAR()
      IF BETWEEN(BAR(),1,8)
        file='FORMA\OUT15506.'+STR(BAR(),1)
        SET PRINTER FONT "Courier New",fontsize1
        =sys(1037)
        TYPE &file TO PRINTER
        DO scr WITH "����� �������� � ��������� - �� [F1]","w/n"
      ENDIF
      ENDDO
   CASE MPROMPT="������� ����� 3-���"
      _Screen.MousePointer=11
      DO CR_FORM WITH 4,'IN3_TEX','OUT3_TEX'
      _Screen.MousePointer=0
   CASE MPROMPT="�������� 3-���"
      DO sut3tec
   CASE MPROMPT="����������� ����� 3-���"
      DEFINE POPUPS s3_tex FROM 06,22 TITLE'[���� ��������� �������� ����]' ;
      COLOR gr+/w,n/w,gr+/w,gr+/w,,w+/r* FONT "Courier New",10
      DEFINE BAR 01 OF s3_tex PROMPT '����1            '
      DEFINE BAR 02 OF s3_tex PROMPT '����2            '
      DEFINE BAR 03 OF s3_tex PROMPT '����3            '
      DEFINE BAR 04 OF s3_tex PROMPT '����4            '
      DEFINE BAR 05 OF s3_tex PROMPT '������� ����     '
      ON SELECTION POPUPS s3_tex DEACT POPUP s3_tex
      HIDE POPUPS POP4,MAIN
      DO WHILE NOT(BAR()=0 OR BAR()=5)
      ACTIVATE POPUPS s3_tex BAR BAR()
      IF BETWEEN(BAR(),1,4)
        file='FORMA\OUT3_TEX.'+STR(BAR(),1)
        define window view from 1,0 to 25,80 title ' ' FONT "Courier New",10
        ACTIV SCREEN
        @ 0,0 SAY "����� ��������� - [ESC]             " COLOR gr+/n FONT "Courier New",10
        ON KEY LABEL ESC KEYB'{Ctrl+W}'
        SET SYSMENU ON
        HIDE MENU _MSYSMENU
        =INSMODE(.F.)
        MODIFY FILE &file WINDOW view
        ON KEY LABEL ESC
        SET SYSMENU OFF
        releas window view
        DO scr WITH "����� �������� � ��������� - �� [F1]","w/n"
      ENDIF
      ENDDO
   CASE MPROMPT="����������� ����� 3-���"
      PRIV fontsize1
      DEFINE POPUPS s3_tex FROM 06,22 TITLE'[���� ������(����������) �������� ����]' ;
      COLOR gr+/w,n/w,gr+/w,gr+/w,,w+/r* FONT "Courier New",10
      DEFINE BAR 01 OF s3_tex PROMPT '����1            '
      DEFINE BAR 02 OF s3_tex PROMPT '����2            '
      DEFINE BAR 03 OF s3_tex PROMPT '����3            '
      DEFINE BAR 04 OF s3_tex PROMPT '����4            '
      DEFINE BAR 05 OF s3_tex PROMPT '������� ����     '
      ON SELECTION POPUPS s3_tex DEACT POPUP s3_tex
      HIDE POPUPS POP4,MAIN
      DO WHILE NOT(BAR()=0 OR BAR()=5)
      ACTIVATE POPUPS s3_tex BAR BAR()
      IF BETWEEN(BAR(),1,4)
        fontsize1=IIF(BETWEEN(BAR(),2,3),8,12)
        do forma ffontsize
        file='FORMA\OUT3_TEX.'+STR(BAR(),1)
        SET PRINTER FONT "Courier New",fontsize1
        =sys(1037)
        TYPE &file TO PRINTER
        DO scr WITH "����� �������� � ��������� - �� [F1]","w/n"
      ENDIF
      ENDDO
   CASE MPROMPT="3-��� �� �������, �������, ���"
      *=sys(1037)
      DO FORM TRI_TKV
   CASE MPROMPT="������ ������������� ������.����"
      DO an_el_sn
   CASE MPROMPT="��� ������ ����-5"
      PUSH KEY CLEAR
      ON KEY LABEL F1 HELP ID 12
      IF MESSAGEBOX('������� �������� ����� ������?',4+32+0,'')=6
         DO CR_FORM WITH 1,'INTEP','OUTTEP'
      ENDIF
      HIDE POPUPS POP4,MAIN
      define window view from 1,0 to 25,115 title ' ' FONT "Courier New",10
      ACTIV SCREEN
      @ 0,0 SAY "����� ������������� - [ESC]             " COLOR gr+/n FONT "Courier New",10
      ON KEY LABEL ESC KEYB'{Ctrl+W}'
      SET SYSMENU ON
      HIDE MENU _MSYSMENU
      =INSMODE(.F.)
      MODIFY FILE FORMA\OUTTEP.1 WINDOW view
      ON KEY LABEL ESC
      SET SYSMENU OFF
      releas window view
      DO scr WITH "����� �������� � ��������� - �� [F1]","w/n"
      DEFINE WINDOW pri FROM 0,0 TO 24,79 NONE color n/w,w+/r FONT "Courier New",9
      ACTIV WINDOW pri
      yn='A'
      DO WHILE .NOT.(UPPER(yn)='Y' OR UPPER(yn)='N')
        yn='N'
        @ 10,15 SAY "����� �� �������?(Y/N) " GET yn
        READ
      ENDDO
      RELEASE WIND pri
      IF UPPER(yn)='Y'
        SET PRINTER FONT "Courier New",9
*       SET PRINTER ON
        =sys(1037)
        TYPE FORMA\OUTTEP.1 TO PRINTER
*       SET PRINTER OFF
        DO scr WITH "����� �������� � ��������� - �� [F1]","w/n"
      ENDIF
      POP KEY
   CASE MPROMPT="MAX����� � MIN����� ������� �������"
      DO MAXMIN  && proc maxmin ��������� � tep.prg
   CASE MPROMPT="������� ����"
      DO scr WITH "����� �������� � ��������� - �� [F1]","w+/w"
      POP KEY
      RETURN TO MASTER
ENDCASE
*************************************************************
PROCEDURE ravno  && ���������/�� ��������� ����/������� ;
������� ����� ������ ��������� ����� � PRG3.PRG
x=LOWER(varread())      && �����.����
y=SUBSTR(&x,1,1)
DO CASE
   CASE x='order'
        IF recno()=1
           IF MESSAGEBOX('������ ��� ������?',4+32+256,'')=6
              REPL order WITH STRTRAN(order,'=',' '),blok1 WITH STRTRAN(blok1,'=',''),;
                   blok2 WITH STRTRAN(blok2,'=',''),blok3 WITH STRTRAN(blok3,'=',''),;
                   blok4 WITH STRTRAN(blok4,'=',''),blok5 WITH STRTRAN(blok5,'=',''),;
                   blok6 WITH STRTRAN(blok6,'=',''),gruppa WITH STRTRAN(gruppa,'=',''),;
                   pk WITH STRTRAN(pk,'=',''),station WITH STRTR(station,'=',''),;
                   pvk WITH STRTRAN(pvk,'=','') ALL
              GOTO 1
           ELSE
              REPL order WITH IIF(y='=',STRTRAN(order,'=',' '),'='+RIGHT(order,5)),;
                   blok1 WITH IIF(y='=',STRTRAN(blok1,'=',''),'='+RIGHT(RTRIM(blok1),9)),;
                   blok2 WITH IIF(y='=',STRTRAN(blok2,'=',''),'='+RIGHT(RTRIM(blok2),9)),;
                   blok3 WITH IIF(y='=',STRTRAN(blok3,'=',''),'='+RIGHT(RTRIM(blok3),9)),;
                   blok4 WITH IIF(y='=',STRTRAN(blok4,'=',''),'='+RIGHT(RTRIM(blok4),9)),;
                   blok5 WITH IIF(y='=',STRTRAN(blok5,'=',''),'='+RIGHT(RTRIM(blok5),9)),;
                   blok6 WITH IIF(y='=',STRTRAN(blok6,'=',''),'='+RIGHT(RTRIM(blok6),9)),;
                   gruppa WITH IIF(y='=',STRTRAN(gruppa,'=',''),'='+RIGHT(RTRIM(gruppa),9)),;
                   pk WITH IIF(y='=',STRTRAN(pk,'=',''),'='+RIGHT(RTRIM(pk),9)),;
                   station WITH IIF(y='=',STRTR(station,'=',''),'='+RIGHT(RTRIM(station),9)),;
                   pvk WITH IIF(y='=',STRTRAN(pvk,'=',''),'='+RIGHT(RTRIM(pvk),9))
                   KEYBOARD "{TAB}{BACKTAB}"
           ENDIF
        ELSE
        REPL order WITH IIF(y='=',STRTRAN(order,'=',' '),'='+RIGHT(order,5)),;
             blok1 WITH IIF(y='=',STRTRAN(blok1,'=',''),'='+RIGHT(RTRIM(blok1),9)),;
             blok2 WITH IIF(y='=',STRTRAN(blok2,'=',''),'='+RIGHT(RTRIM(blok2),9)),;
             blok3 WITH IIF(y='=',STRTRAN(blok3,'=',''),'='+RIGHT(RTRIM(blok3),9)),;
             blok4 WITH IIF(y='=',STRTRAN(blok4,'=',''),'='+RIGHT(RTRIM(blok4),9)),;
             blok5 WITH IIF(y='=',STRTRAN(blok5,'=',''),'='+RIGHT(RTRIM(blok5),9)),;
             blok6 WITH IIF(y='=',STRTRAN(blok6,'=',''),'='+RIGHT(RTRIM(blok6),9)),;
             gruppa WITH IIF(y='=',STRTRAN(gruppa,'=',''),'='+RIGHT(RTRIM(gruppa),9)),;
             pk WITH IIF(y='=',STRTRAN(pk,'=',''),'='+RIGHT(RTRIM(pk),9)),;
             station WITH IIF(y='=',STRTR(station,'=',''),'='+RIGHT(RTRIM(station),9)),;
             pvk WITH IIF(y='=',STRTRAN(pvk,'=',''),'='+RIGHT(RTRIM(pvk),9))
             KEYBOARD "{TAB}{BACKTAB}"
        ENDIF
   CASE SUBSTR(x,1,4)='blok' OR x='gruppa' OR x='pk' OR x='station' OR x='pvk'
        REPLACE &x WITH IIF(y='=',STRTRAN(&x,'=',''),'='+RIGHT(RTRIM(&x),9)),;
                order WITH IIF(y='=',order,'='+RIGHT(order,5))
                KEYBOARD "{TAB}{BACKTAB}"
ENDCASE