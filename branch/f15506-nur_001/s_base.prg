PUSH KEY CLEAR
ON KEY LABEL F1 HELP ID 7
DEFINE POPUPS S_BASE FROM 06,21 TITLE'[РАБОТА С S-БАЗАМИ]' COLOR ;
       gr+/w,n/w,b/w,gr+/w,,w+/r* FONT "Courier New",10
DEFINE BAR 01 OF s_base PROMPT 'Создать s-базы                       ' SKIP FOR .T. &&FILE('s_blok.dbf')
DEFINE BAR 02 OF s_base PROMPT 'Уничтожить s-базы                    ' SKIP FOR .T. &&.NOT.FILE('s_blok.dbf')
DEFINE BAR 03 OF s_base PROMPT 'Скопировать s-базы в директорию SBASE' SKIP FOR .T. &&.NOT.FILE('s_blok.dbf')
DEFINE BAR 04 OF s_base PROMPT 'Заполнить s-базы за сутки            ' SKIP FOR .NOT.FILE('s_blok.dbf').OR..NOT.FILE('inblok.dbf')
DEFINE BAR 05 OF s_base PROMPT 'Заполнить s-базы за месяц            ' SKIP FOR .NOT.FILE('s_blok.dbf').OR..NOT.FILE('inblok.dbf')
DEFINE BAR 06 OF s_base PROMPT 'Заменить s-дату                      ' SKIP FOR .NOT.FILE('s_blok.dbf')
DEFINE BAR 07 OF s_base PROMPT 'Выдать входные базы                  ' SKIP FOR .NOT.FILE('s_blok.dbf')
DEFINE BAR 08 OF s_base PROMPT 'Главное меню                         '
ON SELECTION POPUPS s_base DO tsb WITH PROMPT()
DO scr WITH "Более подробно о программе - по [F1]","w+/w"
*IDE POPUPS main
HIDE POPUPS ALL
ACTIVATE POPUPS s_base
*****************************************************************************
*                          Procedure   tsb
*****************************************************************************
PROCEDURE tsb
PARAMETERS MPROMPT
DO CASE
   CASE MPROMPT="Создать s-базы"
      _Screen.MousePointer=11
      COPY FILE SBASE\s_blok.dbf TO s_blok.dbf
      _Screen.MousePointer=0
      KEYBOARD "{DNARROW}"
   CASE MPROMPT="Уничтожить s-базы"
      HIDE POPUP ALL
      DEFINE WINDOW pri FROM 0,0 TO 24,79 NONE color n/w,w+/r FONT "Courier New",10
      ACTIV WINDOW pri
      yn='A'
      DO WHILE .NOT.(UPPER(yn)='Y' OR UPPER(yn)='N')
        yn='N'
        @ 10,15 SAY "Вы уверены, что это Вам нужно,(Y/N) " GET yn
        READ
      ENDDO
      RELEASE WIND pri
      IF UPPER(yn)='Y'
         DELETE FILE s_blok.dbf
         KEYBOARD "{UPARROW}"
      ENDIF
   CASE MPROMPT="Скопировать s-базы в директорию SBASE"
      _Screen.MousePointer=11
      COPY FILE s_blok.dbf TO SBASE\s_blok.dbf
      _Screen.MousePointer=0
   CASE MPROMPT="Заполнить s-базы за сутки"
      HIDE POPUP ALL
      DEFINE WINDOW pri FROM 0,0 TO 24,79 NONE color n/w,w+/r FONT "Courier New",10
      DEFINE WINDOW a_r_e FROM 10,07 TO 14,72 color w+/r FONT "Courier New",10
      USE s_blok
      set order to data
      ACTIV WINDOW pri
      yn=2
      dt=date()
      DO WHILE yn=2
        @ 04,05 SAY "Введите дату для заполнения s-баз" GET dt;
        valid BETWEEN(dt,{^1980-01-01},{^2100-01-01}) error 'Неверная дата'
        READ
        exdt=.F.       && признак существ-ия дубликата даты
        IF SEEK(dt)
           exdt=.T.
           @ 17,05 SAY "Такая дата уже была занесена" color w+/n
        ENDIF
        ACTIV WINDOW a_r_e
        @ 2,02 GET yn PICTURE "@*IHT ;;" SIZE 1,12,2 COLOR w+/r,,,,,w+/b,,,w+/r
        @ 2,04 SAY "отменить" SIZE 1,8, 0
        @ 2,17 SAY "повторить" SIZE 1,9, 0
        @ 2,32 SAY "выполнить" SIZE 1,9, 0
        READ CYCLE
        DEACTIV WINDOW a_r_e
        CLEAR
      ENDDO
      RELEASE WIND pri,a_r_e
      IF yn=3
         _Screen.MousePointer=11
         IF exdt        && существ.дубликат
            SET ORDER TO
            DELETE FOR data=dt
            pack
         ELSE
            CALCULATE CNT(),MIN(data) TO cnt,mindata && колчество дат и мин.дата
            SET ORDER TO
            IF cnt=40      && предел количества хранимых дат
               DELETE FOR data=mindata
               pack
            ENDIF
         ENDIF
         APPEND FROM inblok FIELDS order,blok1,blok2,blok3,blok4,blok5,;
         blok6,station
         REPLACE data WITH dt,period with 'DAY' FOR data={  .  .  }
         _Screen.MousePointer=0
      ENDIF
      USE
   CASE MPROMPT="Заполнить s-базы за месяц"
      HIDE POPUP ALL
      dimension mes(12)
      DEFINE WINDOW pri FROM 0,0 TO 24,79 NONE color n/w,w+/r FONT "Courier New",10
      DEFINE WINDOW a_r_e FROM 10,07 TO 14,72 color w+/r FONT "Courier New",10
      FOR i=1 TO 12
      mes(i)=SUBSTR("01Январь  02Февраль 03Март    04Апрель  05Май     06Июнь    "+;
           "07Июль    08Август  09Сентябрь10Октябрь 11Ноябрь  12Декабрь ",(i-1)*10+1,10)
      ENDFOR
      USE s_blok
      set order to period
      ACTIV WINDOW pri
      yn=2
      dt=date()
      ms=MONTH(date())
      DO WHILE yn=2
        @ 05,05 SAY "Укажите номер месяца для заполнения "
        @ 04,05 SAY "Укажите текущую дату " GET dt;
        valid BETWEEN(dt,{^1980-01-01},{^2100-01-01}) error 'Неверная дата'
        @ 05,41 GET ms SIZE 1,2 valid BETWEEN(ms,1,12) error 'Неверный № мес.'
        READ
        ACTIV WINDOW a_r_e
        @ 2,02 GET yn PICTURE "@*IHT ;;" SIZE 1,12,2 COLOR w+/r,,,,,w+/b,,,w+/r
        @ 2,04 SAY "отменить" SIZE 1,8, 0
        @ 2,17 SAY "повторить" SIZE 1,9, 0
        @ 2,32 SAY "выполнить" SIZE 1,9, 0
        READ CYCLE
        DEACTIV WINDOW a_r_e
        CLEAR
      ENDDO
      RELEASE WIND pri,a_r_e
      IF yn=3
         _Screen.MousePointer=11
         IF SEEK(mes(ms)) && существ.дубликат
            SET ORDER TO
            DELETE FOR period=mes(ms)
            pack
         ELSE
            SET ORDER TO
         ENDIF
         APPEND FROM inblok FIELDS order,blok1,blok2,blok3,blok4,blok5,;
         blok6,station
         REPLACE data WITH dt,period with mes(ms) FOR data={  .  .  }
         _Screen.MousePointer=0
      ENDIF
      REINDEX
      USE
   CASE MPROMPT="Заменить s-дату"
      HIDE POPUP ALL
      DO FORM RS
   CASE MPROMPT="Выдать входные базы"
      PRIV i,i1,i2,j,yn
      SET EXACT ON
      HIDE POPUP ALL
      DEFINE WINDOW pri FROM 0,0 TO 24,79 NONE color n/w,w+/r FONT "Courier New",10
      DEFINE WINDOW a_r_e FROM 10,14 TO 14,55 color w+/r FONT "Courier New",10
      DEFINE WINDOW a_r_e1 FROM 10,08 TO 14,61 color w+/r FONT "Courier New",10
      USE inblok && sum_norm - массив флажков суммируемости-средневзвешенности
      COPY TO ARRAY sum_norm FIELDS order,sum_norm
      *iarray - массив весов.коэффиц-тов, rez,rez1 - результирующие
      DIMENSION iarray(alen(sum_norm,1),8),rez(alen(sum_norm,1),8),rez1(7)
      STORE 0 TO iarray
      STORE '' TO rez
      FOR i=1 TO ALEN(sum_norm,1)
          sum_norm(i,1)=SUBS(sum_norm(i,1),2)
          iarray(i,1)=sum_norm(i,1)
          rez(i,1)=sum_norm(i,1)
      ENDFOR
      ACTIV WINDOW pri
      ACTIV WINDOW a_r_e
      yn=1
      @ 0,12 SAY "Работа"
      @ 2,02 GET yn PICTURE "@*IHT ;" SIZE 1,12,2 COLOR w+/r,,,,,w+/b,,,w+/r
      @ 2,03 SAY "с сутками     с месяцами"
      READ CYCLE
      DEACT WINDOW a_r_e
      DO CASE
         CASE yn=1 && работа с сутками
           USE s_blok
           set order to data && DATA UNIQUE
           yn=1
           DO WHILE yn=1
              @ 04,05 SAY "Начальная дата" GET begd
              @ 05,05 SAY "Конечная  дата" GET endd
              READ
              IF SEEK(begd) AND SEEK(endd) AND begd<=endd && ответ корректен
                ACTIV WINDOW a_r_e1
                @ 2,02 GET yn PICTURE "@*IHT ;;" SIZE 1,11,2;
                COLOR w+/r,,,,,w+/b,,,w+/r
                @ 2,03 SAY "выполнить    повторить    отменить"
                READ CYCLE
                DEACT WIND a_r_e1
                IF yn=1
                  EXIT
                ENDIF
                yn=yn-1
              ELSE
                ACTIV WINDOW a_r_e
                @ 0,03 SAY "Введены неверные даты"
                @ 2,02 GET yn PICTURE "@*IHT ;" SIZE 1,12,2;
                COLOR w+/r,,,,,w+/b,,,w+/r
                @ 2,03 SAY "повторить     отменить"
                READ CYCLE
                DEACT WIND a_r_e
              ENDIF
           ENDDO
           IF yn=1
              COPY TO ARRAY ar_data FIELDS data FOR BETW(data,begd,endd) && создание массива дат
              filter='period="DAY" AND data=ar_data(i,1)'
           ENDIF
         CASE yn=2 && работа с месяцами
           USE s_blok
           set order to period  && PERIOD UNIQUE
           yn=1
           DO WHILE yn=1
              @ 04,05 SAY "Начальный месяц (число)" GET begm SIZE 1,2
              @ 05,05 SAY "Конечный  месяц (число)" GET endm SIZE 1,2
              READ
              begm=RIGHT('0'+ALLTRIM(begm),2)
              endm=RIGHT('0'+ALLTRIM(endm),2)
*wait wind 'begm='+begm+ALLTRIM(mes(VAL(begm)))+IIF(SEEK(begm+ALLTRIM(mes(VAL(begm)))),'+','-')
              IF SEEK(begm+ALLTRIM(mes(VAL(begm)))) AND SEEK(endm+ALLTRIM(mes(VAL(endm))));
              AND begm<=endm && ответ корректен
                ACTIV WINDOW a_r_e1
                @ 2,02 GET yn PICTURE "@*IHT ;;" SIZE 1,11,2 COLOR w+/r,,,,,w+/b,,,w+/r
                @ 2,03 SAY "выполнить    повторить    отменить"
                READ CYCLE
                DEACT WIND a_r_e1
                IF yn=1
                  EXIT
                ENDIF
                yn=yn-1
              ELSE
                ACTIV WINDOW a_r_e
                @ 0,03 SAY "Введены неверные даты"
                @ 2,02 GET yn PICTURE "@*IHT ;" SIZE 1,12,2;
                COLOR w+/r,,,,,w+/b,,,w+/r
                @ 2,03 SAY "повторить     отменить"
                READ CYCLE
                DEACT WIND a_r_e
              ENDIF
           ENDDO
           IF yn=1
              COPY TO ARRA ar_data FIEL period ;
              FOR BETW(period,begm+ALLTRIM(mes(VAL(begm))),endm+ALLTRIM(mes(VAL(endm)))) && создан.массива мес-в
              filter='period=ar_data(i,1)'
           ENDIF
      ENDCASE
      RELEASE WIND pri,a_r_e,a_r_e1
      _Screen.MousePointer=11
      SET ORDER TO
      FOR i=1 to IIF(type('ar_data')='U',0,ALEN(ar_data,1)) && с проверкой непустоты массива
        SET FILTER TO &filter
        COPY TO ARRAY add FIELDS order,blok1,blok2,blok3,blok4,blok5,blok6,station
        FOR i1=1 TO ALEN(add,1)
            STORE SUBS(add(i1,1),2) TO add(i1,1)
        ENDFOR
        FOR i1=1 TO alen(add,1)
            IF ASCAN(sum_norm,add(i1,1))>0 && в rez нашлось обозначение add(i1,1)
               j=ASUBSCRIPT(sum_norm,ASCAN(sum_norm,add(i1,1)),1)
               FOR i2=2 TO 8
                   IF ALEN(ar_data,1)=1 && кол-во вх.таблиц=1 => не идем на PROC ADD
                      rez(j,i2)=add(i1,i2)
                   ELSE 
                      STORE STRTRAN(add(i1,i2),'s    -    ','    -     ') TO add(i1,i2)
                      IF 's'$add(i1,i2) OR (add(i1,i2)='    -    ' AND i>1) OR ;
                      (ALLTR(SUBS(add(i1,1),2))=='74' AND i>1) && не идем на PROC ADD
                      ELSE
                         iarray(j,i2)=iarray(j,i2)+1 && идем на PROC ADD и
                         DO add WITH iarray(j,i2)    && параметр - весовой коэффициент
                      ENDIF
                   ENDIF
               ENDFOR
            ENDIF
        ENDFOR
      ENDFOR
      _Screen.MousePointer=0
      SET FILTER TO
      IF NOT type('ar_data')='U'
      USE inblok
      FOR i1=1 TO alen(rez,1)
      FOR i2=2 TO alen(rez,2)
          rez1(i2-1)=rez(i1,i2)
      ENDFOR
         LOCATE FOR SUBSTR(order,2)==rez(i1,1)
         GATHER FROM rez1 FIELDS blok1,blok2,blok3,blok4,blok5,blok6,station
      ENDFOR
      ENDIF
      USE
      SET EXACT OFF
   CASE MPROMPT="Главное меню"
      DO scr WITH "Более подробно о программе - по [F1]","w+/w"
      POP KEY
      RETURN TO MASTER
ENDCASE
*****************************************************************************
PROCEDURE add   && вызов из PROC tsb,MPROMPT="Выдать входные базы"
PARAMETER i
PRIVATE n,x,x1,x2
IF i=1
   rez(j,i2)=add(i1,i2)
   RETURN
ENDIF
n=MAX(IIF(AT('.',rez(j,i2))=0,0,LEN(TRIM(rez(j,i2)))-AT('.',rez(j,i2))),;
IIF(AT('.',add(i1,i2))=0,0,LEN(TRIM(add(i1,i2)))-AT('.',add(i1,i2))))
x1=VAL(rez(j,i2))
x2=VAL(add(i1,i2))
IF sum_norm(j,2)  && норматив - суммируемый
  x=x1+x2
ELSE              && норматив - средневзвешиваемый
  x=(x1*(i-1)+x2)/i
ENDIF
rez(j,i2)=LTRIM(STR(x,20,n))