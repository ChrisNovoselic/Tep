PUSH KEY CLEAR
HIDE POPUP MAIN
*ON KEY LABEL F1 HELP ID 2
DEFINE POPUPS POPPVK FROM 05,24 IN SCREEN TITLE'[���� ������� ���]' COLOR ;
       bg/w*,n/w*,gr+/w,gr+/w,,w+/r* FONT "Courier New",11
DEFINE BAR 01 OF POPPVK PROMPT '��������������� ������� �������  ' SKIP FOR .NOT.FILE('inpvk.dbf')
DEFINE BAR 02 OF POPPVK PROMPT '�������� ������                  ' SKIP FOR .T. &&.NOT.FILE('inpvk.dbf')
DEFINE BAR 03 OF POPPVK PROMPT '����������� �������� �������     '
DEFINE BAR 04 OF POPPVK PROMPT '����������� ������� �������      ' SKIP FOR .T. &&.NOT.FILE('inpvk.dbf')
DEFINE BAR 05 OF POPPVK PROMPT '����������� �������� �������     ' SKIP FOR .T.
DEFINE BAR 06 OF POPPVK PROMPT '������� ����                     '
ON SELECTION POPUPS POPPVK DO tmenupvk WITH PROMPT()
DO scr WITH "����� �������� � ��������� - �� [F1]","w/n"
ACTIVATE POPUPS POPPVK
*****************************************************************************
*                          Procedure   tmenupvk                             *
*****************************************************************************
PROCEDURE tmenupvk
PARAMETERS MPROMPT
DO CASE
   CASE MPROMPT="��������������� ������� �������"
      PUSH KEY CLEAR
      ON KEY LABEL S do s
      FormNum="1"
      DO FORM TABLE2 WITH "inpvk",.T.
      ON KEY LABEL S
      POP KEY
   CASE MPROMPT="�������� ������"
*     do precalc.prg  with 'outblok','calc.prg'
   CASE MPROMPT="����������� �������� �������"
      PUSH KEY CLEAR
      ON KEY LABEL = do ravno
      FormNum="2"
      DO FORM TABLE2 WITH "outpvk",.T.
      ON KEY LABEL =
      POP KEY
   CASE MPROMPT="����������� ������� �������"
      DO PRINT WITH "inpvk",'������� ������� ��� ������'
   CASE MPROMPT="����������� �������� �������"
      DO PRINT WITH "outpvk",'�������� ������� ��� ������'
   CASE MPROMPT="������� ����"
      DO scr WITH "����� �������� � ��������� - �� [F1]","w+/w"
      POP KEY
      RETURN TO MASTER
ENDCASE
*************************************************************
PROCEDURE ravno  && ���������/�� ��������� ���n/��� ���
x=LOWER(varread())
y=SUBSTR(&x,1,1)
DO CASE
   CASE x='order'
        IF recno()=1
           IF MESSAGEBOX('������ ��� ������?',4+32+256,'')=6
              REPL order WITH STRTRAN(order,'=',' '),pvk1 WITH STRTRAN(pvk1,'=',''),;
                   pvk2 WITH STRTRAN(pvk2,'=',''),pvk3 WITH STRTRAN(pvk3,'=',''),;
                   pvk4 WITH STRTRAN(pvk4,'=',''),pvk5 WITH STRTRAN(pvk5,'=',''),;
                   pvk6 WITH STRTRAN(pvk6,'=',''),pvk7 WITH STRTRAN(pvk7,'=',''),;
                   vsepvk WITH STRTR(vsepvk,'=','') ALL
              GOTO 1
           ELSE
              REPL order WITH IIF(y='=',STRTRAN(order,'=',' '),'='+RIGHT(order,5)),;
                   pvk1 WITH IIF(y='=',STRTRAN(pvk1,'=',''),'='+RIGHT(RTRIM(pvk1),9)),;
                   pvk2 WITH IIF(y='=',STRTRAN(pvk2,'=',''),'='+RIGHT(RTRIM(pvk2),9)),;
                   pvk3 WITH IIF(y='=',STRTRAN(pvk3,'=',''),'='+RIGHT(RTRIM(pvk3),9)),;
                   pvk4 WITH IIF(y='=',STRTRAN(pvk4,'=',''),'='+RIGHT(RTRIM(pvk4),9)),;
                   pvk5 WITH IIF(y='=',STRTRAN(pvk5,'=',''),'='+RIGHT(RTRIM(pvk5),9)),;
                   pvk6 WITH IIF(y='=',STRTRAN(pvk6,'=',''),'='+RIGHT(RTRIM(pvk6),9)),;
                   pvk7 WITH IIF(y='=',STRTRAN(pvk7,'=',''),'='+RIGHT(RTRIM(pvk7),9)),;
                   vsepvk WITH IIF(y='=',STRTR(vsepvk,'=',''),'='+RIGHT(RTRIM(vsepvk),9))
                   KEYBOARD "{TAB}{BACKTAB}"
           ENDIF
        ELSE
        REPL order WITH IIF(y='=',STRTRAN(order,'=',' '),'='+RIGHT(order,5)),;
             pvk1 WITH IIF(y='=',STRTRAN(pvk1,'=',''),'='+RIGHT(RTRIM(pvk1),9)),;
             pvk2 WITH IIF(y='=',STRTRAN(pvk2,'=',''),'='+RIGHT(RTRIM(pvk2),9)),;
             pvk3 WITH IIF(y='=',STRTRAN(pvk3,'=',''),'='+RIGHT(RTRIM(pvk3),9)),;
             pvk4 WITH IIF(y='=',STRTRAN(pvk4,'=',''),'='+RIGHT(RTRIM(pvk4),9)),;
             pvk5 WITH IIF(y='=',STRTRAN(pvk5,'=',''),'='+RIGHT(RTRIM(pvk5),9)),;
             pvk6 WITH IIF(y='=',STRTRAN(pvk6,'=',''),'='+RIGHT(RTRIM(pvk6),9)),;
             pvk7 WITH IIF(y='=',STRTRAN(pvk7,'=',''),'='+RIGHT(RTRIM(pvk7),9)),;
             vsepvk WITH IIF(y='=',STRTR(vsepvk,'=',''),'='+RIGHT(RTRIM(vsepvk),9))
             KEYBOARD "{TAB}{BACKTAB}"
         ENDIF
   CASE SUBSTR(x,1,3)='pvk' OR x='vsepvk'
        REPL &x WITH IIF(y='=',STRTRAN(&x,'=',''),'='+RIGHT(RTRIM(&x),9)),;
             order WITH IIF(y='=',order,'='+RIGHT(order,5))
             KEYBOARD "{TAB}{BACKTAB}"
ENDCASE
