PUSH KEY CLEAR
ON KEY LABEL F1 HELP ID 7
DEFINE POPUPS SEL_TYPE FROM 10,20 TITLE'[����� ���� S-���]' COLOR ;
       gr+/w,n/w,b/w,gr+/w,,w+/r* FONT "Courier New",10
DEFINE BAR 01 OF sel_type PROMPT 's-���� ��� ������� �������             '
DEFINE BAR 02 OF sel_type PROMPT 's-���� ��� �������� ������� (���������)'
DEFINE BAR 03 OF sel_type PROMPT '������� ����                           '
ON SELECTION POPUPS sel_type DO tst WITH PROMPT()
DO scr WITH "����� �������� � ��������� - �� [F1]","w+/w"
HIDE POPUPS main
ACTIVATE POPUPS sel_type
*****************************************************************************
*                          Procedure   tst
*****************************************************************************
PROCEDURE tst
PARAMETERS MPROMPT
DO CASE
   CASE MPROMPT="s-���� ��� ������� �������"
        DO s_base
   CASE MPROMPT="s-���� ��� �������� ������� (���������)"
        DO s_outbl
   CASE MPROMPT="������� ����"
      DO scr WITH "����� �������� � ��������� - �� [F1]","w+/w"
      POP KEY
      RETURN TO MASTER
ENDCASE
