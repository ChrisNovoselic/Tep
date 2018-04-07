PARAM outdbf,PrgCalc
      IF BL1 OR BL2 OR BL3 OR BL4 OR BL5 OR BL6
         @ 23,0 SAY "Укажите количество работающих блоков:" COLOR gr+/n FONT "Courier New",14 ;
         STYLE 'U'
         @ 23,col() GET n_blokov1 SIZE 1,1 COLOR ,w+/n VALID BETWEEN(n_blokov1,1,6);
         ERROR "Число д.б. от 1 до 6" FONT "Courier New",14 STYLE 'N'
         READ
      ENDIF
      SELECT 1
      USE inblok       && входная табл.
      n_blokov=6       && кол-во блоков
      fld_arr='blok1,blok2,blok3,blok4,blok5,blok6,station'
      nfld_arr=''
      FOR i=1 TO 6
          bloki='blok'+STR(i,1)
          IF SUBSTR(&bloki,1,1)='s' OR (i#1 AND BL1) OR (i#2 AND BL2) OR (i#3 AND BL3) OR (i#4 AND BL4) OR (i#5 AND BL5) OR;
                   (i#6 AND BL6) && бл.не учитыв-ся
             n_blokov=n_blokov-1
             fld_arr=STRTRAN(fld_arr,SUBSTR(fld_arr,AT(bloki,fld_arr),6))
             nfld_arr=nfld_arr+IIF(nfld_arr=='','',',')+bloki
          ENDIF
      ENDFOR
      PUBLIC ai(n_blokov+1) && массив указыв.фактич.№№ оставл.блоков
      FOR i=1 TO n_blokov
          ai(i)=VAL(SUBSTR(fld_arr,i*6-1,1))
      ENDFOR
      ai(n_blokov+1)=7         && последний элемент всегда = 7
      PUBLIC Iinm(reccount(),1),inm(reccount(),n_blokov+1)  && 
      COPY TO ARRAY Iinm FIELDS order    && коп-ие вх.№№ в Iinm
      FOR i=1 TO ALEN(Iinm,1)
          Iinm(i)=ALLTRIM(SUBS(Iinm(i),2))
      ENDFOR
      COPY TO ARRAY inm FIELDS &fld_arr  && коп-ие вх.т.в inm
      PUBLIC minmax(reccount(),3)
      COPY TO ARRAY minmax FIELDS min_z,max_z,sum_norm  && коп-ие вх.т.в minmax
      SELECT 2
      ON ERROR DO precalcerror
      USE (outdbf)
      ON ERROR
      PUBLIC Ioutm(reccount(),1),outm(reccount(),n_blokov+IIF(outdbf='outblok',1,4)) && 
      PUBLIC exact(reccount(),1)           && 
      IF outdbf='outmkt'      
         fld_arr1=STRTRAN(fld_arr,'station')+'gruppa,pk,station,pvk'
         SELECT 4
         USE outblok       && выходная табл.нормативов
         PUBLIC Iiom(reccount(),1),iom(reccount(),n_blokov+1)  && 
         COPY TO ARRAY Iiom FIELDS order    && коп-ие вых.№№ в Iiom
         FOR i=1 TO ALEN(Iiom,1)
             Iiom(i)=ALLTRIM(SUBS(Iiom(i),2))
         ENDFOR
         COPY TO ARRAY iom FIELDS &fld_arr  && коп-ие вых.т.в iom
         fld_arr=fld_arr1
         SELECT 2
      ENDIF
      COPY TO ARRAY Ioutm FIELDS order   && коп-ие вых.№№ в Iinm
      FOR i=1 TO ALEN(Ioutm,1)
          Ioutm(i)=ALLTRIM(SUBS(Ioutm(i),2))
      ENDFOR
      COPY TO ARRAY outm FIELDS &fld_arr && коп-ие вых.т.в outm
      IF (BL1 OR BL2 OR BL3 OR BL4 OR BL5 OR BL6)
      COPY TO ARRAY exact FIELDS exact_rt&& кол-во знаков после запятой
      ELSE
      COPY TO ARRAY exact FIELDS exact   && кол-во знаков после запятой
      ENDIF
      SELECT 3
      USE ftabl
      SET ORDER TO TAG id
      DO scr WITH "Более подробно о программе - по [F1]","w/n"
      DEFINE WINDOW wait FROM 22,45 TO 24,79 DOUBLE COLOR gr+/r FONT "Courier New",10
      _Screen.MousePointer=11
      ACTIVATE WINDOW wait
      @ 0,5 SAY "Пар-р "+SPACE(6)+" подсчитан..." COLOR gr+/r
*       sss=SECON()
      DO (PrgCalc)
*       wait window (str(SECON()-sss,10,3))
      _Screen.MousePointer=0
      RELEASE WINDOW wait
      CLOSE DATA
*************************************************************
PROC precalcerror      && из PROC в PRECALC.PRG (быстрый переход)
PUSH KEY CLEAR
MESSAGEBOX('Ошибка. Вероятно, запущены более 1-го экземпляра программы.',0+16,'')
DO tmenu WITH "Выход"
POP KEY
