PUSH KEY CLEAR
ON KEY LABEL F1 HELP ID 10
HIDE POPUP MAIN,POP4
DEFINE POPUPS SUT3TEC FROM 06,21 TITLE'[�������� 3-���]' COLOR gr+/w,n/w,b/w,gr+/w,,w+/r*  FONT "Courier New",11
DEFINE BAR 01 OF SUT3TEC PROMPT '������������ 3-��� �� �����        '
DEFINE BAR 02 OF SUT3TEC PROMPT '������                             '
DEFINE BAR 03 OF SUT3TEC PROMPT '���������� ����                    '
ON SELECTION POPUPS SUT3TEC DO tmenu WITH PROMPT()
DO scr WITH "����� �������� � ��������� - �� [F1]","n/w"
ACTIVATE POPUPS SUT3TEC
*****************************************************************************
*                          Procedure   tmenu                                *
*****************************************************************************
PROCEDURE tmenu
PARAMETERS MPROMPT
DO CASE
   CASE MPROMPT="������������ 3-��� �� �����"
        DO FORM stri_tec
   CASE MPROMPT="������"
        =sys(1037)
        DO FORM STRI_TKV
   CASE MPROMPT="���������� ����"
      DO scr WITH "����� �������� � ��������� - �� [F1]","w/n"
      POP KEY
      RETURN TO prg4
ENDCASE
