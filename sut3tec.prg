PUSH KEY CLEAR
ON KEY LABEL F1 HELP ID 10
HIDE POPUP MAIN,POP4
DEFINE POPUPS SUT3TEC FROM 06,21 TITLE'[СУТОЧНЫЙ 3-ТЕХ]' COLOR gr+/w,n/w,b/w,gr+/w,,w+/r*  FONT "Courier New",11
DEFINE BAR 01 OF SUT3TEC PROMPT 'Формирование 3-тех за сутки        '
DEFINE BAR 02 OF SUT3TEC PROMPT 'Отчёты                             '
DEFINE BAR 03 OF SUT3TEC PROMPT 'Предыдущее меню                    '
ON SELECTION POPUPS SUT3TEC DO tmenu WITH PROMPT()
DO scr WITH "Более подробно о программе - по [F1]","n/w"
ACTIVATE POPUPS SUT3TEC
*****************************************************************************
*                          Procedure   tmenu                                *
*****************************************************************************
PROCEDURE tmenu
PARAMETERS MPROMPT
DO CASE
   CASE MPROMPT="Формирование 3-тех за сутки"
        DO FORM stri_tec
   CASE MPROMPT="Отчёты"
        =sys(1037)
        DO FORM STRI_TKV
   CASE MPROMPT="Предыдущее меню"
      DO scr WITH "Более подробно о программе - по [F1]","w/n"
      POP KEY
      RETURN TO prg4
ENDCASE
