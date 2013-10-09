ON KEY LABEL F1 HELP ID 14
HIDE POPUP MAIN
DO scr WITH "Более подробно о программе - по [F1]","w/n"
PRIVATE data1,time1,data2,time2,FuncPrint
STORE date() TO data1,data2
WrongHours=SPACE(4)
PRIV n,fl,i,j,j1,RealHour
do forma PERIODTI
@ 09,25 TO 12,56 COLOR w+/b*
@ 10,30 say " Ж Д И Т Е ... "  FONT "Courier New",10 STYLE 'B'
data1=CTOD(STR(DEN(IDEN1),2)+'.'+STR(IMES1,2)+'.'+STR(GOD(IGOD1),4))
data2=CTOD(STR(DEN(IDEN2),2)+'.'+STR(IMES2,2)+'.'+STR(GOD(IGOD2),4))
time1=CHAS(ICHAS1)
time2=CHAS(ICHAS2)
error=0
on error store error() to error
SET DEFAULT TO (KTSpath)
on error 
SET DEFAULT TO (DefaultDir)
IF error=0
**************  определение количества часов работы (=NumbHours)
   PRIV NumbHours
   IF DATE()<data2 OR ;                                                          && конечн.граница еще не
      DATE()=data2 AND VAL(LEFT(TIME(),2))<time2 OR;                             && настала если указан.конечн.
      DATE()=data2 AND VAL(LEFT(TIME(),2))=time2 AND VAL(SUBSTR(TIME(),4,2))<15  && дата,время+15мин больше реального
      IF MESSAGEBOX('Конечная граница диапазона превышает'+CHR(13)+;
                    'допустимую. Расчет отменяется.',0+16+0,'')=1
         clear
         RETURN TO MASTER
      ENDIF 
   ENDIF
   NumbHours=IIF(data1=data2,time2-time1,24-time1+time2+24*(data2-data1-1))
   USE inblok
   **************  выбор из КТС электрической
   DO CASE
*  CASE MESSAGEBOX('Информацию берем из КТС-электрическая версии 6.2?',4+32+0,'')=6
   CASE .T.
   *+++ version 6.2
   n=ADIR(fl,KTSpath+'\ZTU_E6\par_elhr.*')
   j1=0
   FOR i=1 TO n
       RealHour=(VAL(STRTRAN(SUBSTR(fl(i,4),1,5),':'))-15)/100 && от времени образования файла -15 мин. и /100 
       IF RealHour<0
          RealHour=23
          fl(i,3)=fl(i,3)-1
       ELSE
          RealHour=INT(RealHour)
       ENDIF
       IF BETWEEN(fl(i,3),data1,data2) AND ;
          IIF(fl(i,3)=data1,RealHour>=time1,.T.) AND ;
          IIF(fl(i,3)=data2,RealHour<time2,.T.)
          j1=j1+1     && вычисление кол-ва итераций (участвующих в расчете вх.файлов)
       ENDIF
   ENDFOR
   j=0
   FOR i=1 TO n
       RealHour=(VAL(STRTRAN(SUBSTR(fl(i,4),1,5),':'))-15)/100 && от времени образования файла -15 мин. и /100 
       IF RealHour<0
          RealHour=23
       ELSE
          RealHour=INT(RealHour)
       ENDIF
       IF BETWEEN(fl(i,3),data1,data2) AND ;
          IIF(fl(i,3)=data1,RealHour>=time1,.T.) AND ;
          IIF(fl(i,3)=data2,RealHour<time2,.T.)
          j=j+1       && № тек. итерации
          DO import with KTSpath+'\ZTU_E6\'+fl(i,1),j,j1
       ENDIF
   ENDFOR
   WrongHours=IIF(j#NumbHours,STUFF(WrongHours,1,1,'E'),WrongHours)
   OTHERWISE
   *+++ version 5.19
   n=ADIR(fl,KTSpath+'\ZTU_EN\par_elhr.*')
   j1=0
   FOR i=1 TO n
       RealHour=(VAL(STRTRAN(SUBSTR(fl(i,4),1,5),':'))-15)/100 && от времени образования файла -15 мин. и /100 
       IF RealHour<0
          RealHour=23
          fl(i,3)=fl(i,3)-1
       ELSE
          RealHour=INT(RealHour)
       ENDIF
       IF BETWEEN(fl(i,3),data1,data2) AND ;
          IIF(fl(i,3)=data1,RealHour>=time1,.T.) AND ;
          IIF(fl(i,3)=data2,RealHour<time2,.T.)
          j1=j1+1     && вычисление кол-ва итераций (участвующих в расчете вх.файлов)
       ENDIF
   ENDFOR
   j=0
   FOR i=1 TO n
       RealHour=(VAL(STRTRAN(SUBSTR(fl(i,4),1,5),':'))-15)/100 && от времени образования файла -15 мин. и /100 
       IF RealHour<0
          RealHour=23
       ELSE
          RealHour=INT(RealHour)
       ENDIF
       IF BETWEEN(fl(i,3),data1,data2) AND ;
          IIF(fl(i,3)=data1,RealHour>=time1,.T.) AND ;
          IIF(fl(i,3)=data2,RealHour<time2,.T.)
          j=j+1       && № тек. итерации
          DO import with KTSpath+'\ZTU_EN\'+fl(i,1),j,j1
       ENDIF
   ENDFOR
   WrongHours=IIF(j#NumbHours*2,STUFF(WrongHours,1,1,'E'),WrongHours)
   ENDCASE
   **************  выбор из КТС тепловой
   src_adding_path = '\ZTU_T6\'
*  n=ADIR(fl,KTSpath + src_adding_path + 'par_tphr.*')
   n = ADIR (fl, KTSpath + src_adding_path + 'par_tphr.*')
   j1=0
   FOR i=1 TO n
       RealHour=(VAL(STRTRAN(SUBSTR(fl(i,4),1,5),':'))-15)/100 && от времени образования файла -15 мин. и /100 
       IF RealHour<0
          RealHour=23
          fl(i,3)=fl(i,3)-1
       ELSE
          RealHour=INT(RealHour)
       ENDIF
       IF BETWEEN(fl(i,3),data1,data2) AND ;
          IIF(fl(i,3)=data1,RealHour>=time1,.T.) AND ;
          IIF(fl(i,3)=data2,RealHour<time2,.T.)
          j1=j1+1
       ENDIF
   ENDFOR
   j=0
   FOR i=1 TO n
       RealHour=(VAL(STRTRAN(SUBSTR(fl(i,4),1,5),':'))-15)/100 && от времени образования файла -15 мин. и /100 
       IF RealHour<0
          RealHour=23
       ELSE
          RealHour=INT(RealHour)
       ENDIF
       IF BETWEEN(fl(i,3),data1,data2) AND ;
          IIF(fl(i,3)=data1,RealHour>=time1,.T.) AND ;
          IIF(fl(i,3)=data2,RealHour<time2,.T.)
          j = j + 1
          DO import with KTSpath + src_adding_path + fl (i, 1) , j , j1
       ENDIF
   ENDFOR
   WrongHours=IIF(j#NumbHours*2,STUFF(WrongHours,2,1,'T'),WrongHours)
   **************  выбор из АСУ ТП бл.№1
   IF BL1 && проверочка только для параметра '/1'
   n=ADIR(fl,KTSpath+'\ZTU_B1\par_b1hr.*')
   j1=0
   FOR i=1 TO n
       RealHour=(VAL(STRTRAN(SUBSTR(fl(i,4),1,5),':'))-25)/100 && от времени образования файла -25 мин. и /100 
       IF RealHour<0
          RealHour=23
          fl(i,3)=fl(i,3)-1
       ELSE
          RealHour=INT(RealHour)
       ENDIF
       IF BETWEEN(fl(i,3),data1,data2) AND ;
          IIF(fl(i,3)=data1,RealHour>=time1,.T.) AND ;
          IIF(fl(i,3)=data2,RealHour<time2,.T.)
          j1=j1+1
       ENDIF
   ENDFOR
   j=0
   FOR i=1 TO n
       RealHour=(VAL(STRTRAN(SUBSTR(fl(i,4),1,5),':'))-25)/100 && от времени образования файла -25 мин. и /100 
       IF RealHour<0
          RealHour=23
       ELSE
          RealHour=INT(RealHour)
       ENDIF
       IF BETWEEN(fl(i,3),data1,data2) AND ;
          IIF(fl(i,3)=data1,RealHour>=time1,.T.) AND ;
          IIF(fl(i,3)=data2,RealHour<time2,.T.)
          j=j+1
          DO import with KTSpath+'\ZTU_B1\'+fl(i,1),j,j1
       ENDIF
   ENDFOR
   WrongHours=IIF(j#NumbHours,STUFF(WrongHours,3,1,'1'),WrongHours)
   ENDIF
   **************  выбор из АСУ ТП бл.№6
   IF BL6 && проверочка только для параметра '/6'
   n=ADIR(fl,KTSpath+'\ZTU_B6\par_b6hr.*')
   j1=0
   FOR i=1 TO n
       RealHour=(VAL(STRTRAN(SUBSTR(fl(i,4),1,5),':'))-25)/100 && от времени образования файла -25 мин. и /100 
       IF RealHour<0
          RealHour=23
          fl(i,3)=fl(i,3)-1
       ELSE
          RealHour=INT(RealHour)
       ENDIF
       IF BETWEEN(fl(i,3),data1,data2) AND ;
          IIF(fl(i,3)=data1,RealHour>=time1,.T.) AND ;
          IIF(fl(i,3)=data2,RealHour<time2,.T.)
          j1=j1+1
       ENDIF
   ENDFOR
   j=0
   FOR i=1 TO n
       RealHour=(VAL(STRTRAN(SUBSTR(fl(i,4),1,5),':'))-25)/100 && от времени образования файла -25 мин. и /100 
       IF RealHour<0
          RealHour=23
       ELSE
          RealHour=INT(RealHour)
       ENDIF
       IF BETWEEN(fl(i,3),data1,data2) AND ;
          IIF(fl(i,3)=data1,RealHour>=time1,.T.) AND ;
          IIF(fl(i,3)=data2,RealHour<time2,.T.)
          j=j+1
          DO import with KTSpath+'\ZTU_B6\'+fl(i,1),j,j1
       ENDIF
   ENDFOR
   WrongHours=IIF(j#NumbHours,STUFF(WrongHours,4,1,'6'),WrongHours)
   ENDIF
   IF NOT WrongHours==SPACE(4)
      IF MESSAGEBO('Количество сгенерированных КТС "Энергия"'+;
                   IIF(BL1,'/АСУ ТП блока №1',IIF(BL6,'/АСУ ТП блока №6',''))+CHR(13)+;
                   'файлов не достаточно для указанного периода.'+CHR(13)+CHR(13)+;
                   'Отсутствуют:'+CHR(13)+;
                   IIF(SUBS(WrongHours,1,1)='E',SPACE(4)+;
                   'электрические параметры (КТС "Энергия")'+CHR(13),'')+;
                   IIF(SUBS(WrongHours,2,1)='T',SPACE(4)+;
                   'тепловые параметры (КТС "Энергия")'+CHR(13),'')+;
                   IIF(SUBS(WrongHours,3,1)='1',SPACE(4)+;
                   'данные по блоку №1 (АСУ ТП блока №1)'+CHR(13),'')+;
                   IIF(SUBS(WrongHours,4,1)='6',SPACE(4)+;
                   'данные по блоку №6 (АСУ ТП блока №6)'+CHR(13),'')+;
                   'Результат будет неверным.'+CHR(13)+CHR(13)+;
                   'Тем не менее продолжить рассчет?',4+32+256,'')=7
         clear
         RETURN TO MASTER
      ENDIF
      WrongHours=SPACE(4)
   ENDIF
   **************
   LOCATE FOR ALLTRIM(SUBSTR(order,2))=='70'
   REPLACE inblok.station WITH STR(NumbHours,10,2)
   LOCATE FOR ALLTRIM(SUBSTR(order,2))=='71'
   REPLACE inblok.blok1 WITH STR(NumbHours,10,2),inblok.blok2 WITH STR(NumbHours,10,2),;
           inblok.blok3 WITH STR(NumbHours,10,2),inblok.blok4 WITH STR(NumbHours,10,2),;
           inblok.blok5 WITH STR(NumbHours,10,2),inblok.blok6 WITH STR(NumbHours,10,2),;
           inblok.station WITH STR(NumbHours,10,2)
   LOCATE FOR ALLTRIM(SUBSTR(order,2))=='1'
   REPLACE inblok.blok1 WITH STR(NumbHours,10,2),inblok.blok2 WITH STR(NumbHours,10,2),;
           inblok.blok3 WITH STR(NumbHours,10,2),inblok.blok4 WITH STR(NumbHours,10,2),;
           inblok.blok5 WITH STR(NumbHours,10,2),inblok.blok6 WITH STR(NumbHours,10,2)
   * По просьбе ПТО (20.07.2012) для режима 2а
   * PRIV i47_1,i47_2,i47_3,i47_4,i47_5,i47_6
   * LOCATE FOR ALLTRIM(SUBSTR(order,2))=='47'
   * i47_1=VAL(inblok.blok1)
   * i47_2=VAL(inblok.blok2)
   * i47_3=VAL(inblok.blok3)
   * i47_4=VAL(inblok.blok4)
   * i47_5=VAL(inblok.blok5)
   * i47_6=VAL(inblok.blok6)
   * LOCATE FOR ALLTRIM(SUBSTR(order,2))=='74'
   * REPLACE inblok.blok1 WITH IIF(NumbHours=0,inblok.blok1,;
   *        IIF(i47_1/NumbHours>=1.0,'2','1')),;
   *        inblok.blok2 WITH IIF(NumbHours=0,inblok.blok2,;
   *        IIF(i47_2/NumbHours>=1.0,'2','1')),;
   *        inblok.blok3 WITH IIF(NumbHours=0,inblok.blok3,;
   *        IIF(i47_3/NumbHours>=1.0,'2','1')),;
   *        inblok.blok4 WITH IIF(NumbHours=0,inblok.blok4,;
   *        IIF(i47_4/NumbHours>=1.0,'2','1')),;
   *        inblok.blok5 WITH IIF(NumbHours=0,inblok.blok5,;
   *        IIF(i47_5/NumbHours>=1.0,'2','1')),;
   *        inblok.blok6 WITH IIF(NumbHours=0,inblok.blok6,;
   *        IIF(i47_6/NumbHours>=1.0,'2','1'))
   USE
   clear
   do precalc.prg with 'outblok','calc.prg'
   KEYB'{ENTER}'
   do precalc.prg with 'outmkt','calc_m.prg'
   ************************** вывод таблицы
   SELECT 2
   USE outmkt
   PUBLIC mkt(RECCOUNT(),2)
   DO CASE
      CASE BL1
           COPY TO ARRAY mkt FIELDS order,blok1
      CASE BL2
           COPY TO ARRAY mkt FIELDS order,blok2
      CASE BL3
           COPY TO ARRAY mkt FIELDS order,blok3
      CASE BL4
           COPY TO ARRAY mkt FIELDS order,blok4
      CASE BL5
           COPY TO ARRAY mkt FIELDS order,blok5
      CASE BL6
           COPY TO ARRAY mkt FIELDS order,blok6
   ENDCASE
   FOR i=1 TO ALEN(mkt,1)
       mkt(i,1)=SUBS(mkt(i,1),2)
       mkt(i,2)=SUBS(mkt(i,2),2)
   ENDFOR
   SET EXACT ON
   DEFINE WINDOW W FROM 0,0 TO 25,40 name w
   WITH w
        .WindowState=0  && 0-normal,2-maximized
        .MaxButton=.T.
        .HalfHeightCaption=.T.
        .Closable=.T.
        .Caption="После просмотра - [ESC]"
   ENDWITH
   set sysmenu on
   hide menu _msysmenu
   keyb "{Ctrl+F10}"
   REPORT FORM tabtep NOCONSOLE PREVIEW WINDOW W
   set sysmenu off
   RELEASE WIND W
   SET EXACT OFF
   CLOSE DATA
   RELEAS mkt
ELSE
   wait window 'Нет доступа к источнику. Нажмите любую клавишу.'  
ENDIF
ON KEY LABEL F1 HELP ID 1
RETURN TO MASTER
