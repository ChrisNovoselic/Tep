PUSH KEY CLEAR
ON KEY LABEL F1 HELP ID 7
DEFINE POPUPS S_OUTBL FROM 08,21 TITLE'[������ � S-������ (���������)]' COLOR ;
       gr+/w,n/w,b/w,gr+/w,,w+/r* FONT "Courier New",10
DEFINE BAR 01 OF s_outbl PROMPT '������� s-����                       ' SKIP FOR .T.
DEFINE BAR 02 OF s_outbl PROMPT '���������� s-����                    ' SKIP FOR .T.
DEFINE BAR 03 OF s_outbl PROMPT '����������� s-���� � ���������� SBASE' SKIP FOR .T.
DEFINE BAR 04 OF s_outbl PROMPT '����������/���������� s-���          ' SKIP FOR .NOT.FILE('s_outbl.dbf').OR..NOT.FILE('outblok.dbf')
DEFINE BAR 05 OF s_outbl PROMPT '������� ����                         '
ON SELECTION POPUPS s_outbl DO tsob WITH PROMPT()
DO scr WITH "����� �������� � ��������� - �� [F1]","w+/w"
HIDE POPUPS ALL
ACTIVATE POPUPS s_outbl
*****************************************************************************
*                          Procedure   tsob
*****************************************************************************
PROCEDURE tsob
PARAMETERS MPROMPT
DO CASE
   CASE MPROMPT="������� s-����"
      _Screen.MousePointer=11
      COPY FILE SBASE\s_outbl.dbf TO s_outbl.dbf
      _Screen.MousePointer=0
      KEYBOARD "{DNARROW}"
   CASE MPROMPT="���������� s-����"
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
         DELETE FILE s_outbl.dbf
         KEYBOARD "{UPARROW}"
      ENDIF
   CASE MPROMPT="����������� s-���� � ���������� SBASE"
      _Screen.MousePointer=11
      COPY FILE s_outbl.dbf TO SBASE\s_outbl.dbf
      _Screen.MousePointer=0
   CASE MPROMPT="����������/���������� s-���"
        DO FORMA F_OUTBL
   CASE MPROMPT="������� ����"
      DO scr WITH "����� �������� � ��������� - �� [F1]","w+/w"
      POP KEY
      RETURN TO MASTER
ENDCASE
