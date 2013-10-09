PUSH KEY CLEAR
ON KEY LABEL F1 HELP ID 7
DEFINE POPUPS SEL_TYPE FROM 10,20 TITLE'[ВЫБОР ТИПА S-БАЗ]' COLOR ;
       gr+/w,n/w,b/w,gr+/w,,w+/r* FONT "Courier New",10
DEFINE BAR 01 OF sel_type PROMPT 's-базы для входной таблицы             '
DEFINE BAR 02 OF sel_type PROMPT 's-базы для выходной таблицы (нормативы)'
DEFINE BAR 03 OF sel_type PROMPT 'Главное меню                           '
ON SELECTION POPUPS sel_type DO tst WITH PROMPT()
DO scr WITH "Более подробно о программе - по [F1]","w+/w"
HIDE POPUPS main
ACTIVATE POPUPS sel_type
*****************************************************************************
*                          Procedure   tst
*****************************************************************************
PROCEDURE tst
PARAMETERS MPROMPT
DO CASE
   CASE MPROMPT="s-базы для входной таблицы"
        DO s_base
   CASE MPROMPT="s-базы для выходной таблицы (нормативы)"
        DO s_outbl
   CASE MPROMPT="Главное меню"
      DO scr WITH "Более подробно о программе - по [F1]","w+/w"
      POP KEY
      RETURN TO MASTER
ENDCASE
