PROCEDURE PRINT
PARAMETERS TblName,text
PUSH KEY CLEAR
ON KEY LABEL F1 HELP ID 5
PRIVATE size,string,Show_bi
HIDE POPUP ALL
size=IIF(TblName='outmkt',14,11) && кол-во полей
DIMENSION b(size)
SELECT 1
USE &TblName
DEFINE WINDOW pri FROM 0,0 TO 24,100 NONE color rb/w  FONT "Courier New",11
ACTIV WINDOW pri
@ 10,10 SAY "  Спрячьте ненужные в распечатке поля.   "
@ 11,10 SAY "Для продолжения нажмите на любую клавишу."
WAIT ''
DEACT WIND pri
USE
DO CASE
   CASE TblName='inblok'
        FormNum='1'
   CASE TblName='outblok'
        FormNum='2'
   CASE TblName='outmkt'
        FormNum='3'
ENDCASE
PUSH KEY CLEAR
DO FORM TABLE1 WITH TblName,.F.
POP KEY
USE &TblName
b(1)="SUBSTR(order,2)"           && массив b
FOR i=2 TO size                  && содержит
    b(i)=LOWER(FIELD(i))         && имена 
ENDFOR                           && выводимых на
FOR i=size TO 5 STEP -1          && печать
 DO CASE
    CASE BETWEEN(i,5,10)          && полей
         Show_bi='Show_b'+STR(i-4,1)
         IF &Show_bi=.F.
            =ADEL(b,i)
            DIMENSION b(ALEN(b)-1)
         ENDIF   
    CASE i=11 AND TblName#'outmkt'
         IF Show_station=.F.
            =ADEL(b,i)
            DIMENSION b(ALEN(b)-1)
         ENDIF   
    CASE i=13
         IF Show_station=.F.
            =ADEL(b,i)
            DIMENSION b(ALEN(b)-1)
         ENDIF   
    CASE i=11 AND TblName='outmkt'
         IF Show_gruppa=.F.
            =ADEL(b,i)
            DIMENSION b(ALEN(b)-1)
         ENDIF   
    CASE i=12
         IF Show_pk=.F.
            =ADEL(b,i)
            DIMENSION b(ALEN(b)-1)
         ENDIF   
    CASE i=14
         IF Show_pvk=.F.
            =ADEL(b,i)
            DIMENSION b(ALEN(b)-1)
         ENDIF   
 ENDCASE
ENDFOR
ACTIV WINDOW pri
SET COLOR TO n/n
CLEAR
DEFINE POPUPS ppri1 FROM 10,31 TITLE'[ОБЪЕМ ВЫВОДА]' COLOR ;
       bg/w,n/w,gr+/w,gr+/w,,n/r* FONT "Courier New",11
DEFINE BAR 1 OF ppri1 PROMPT 'Полный     '               && full
DEFINE BAR 2 OF ppri1 PROMPT 'Сокращенный' SKIP FOR .T.  && redu
DEFINE BAR 3 OF ppri1 PROMPT 'Выборочный '               && cust
ON SELECTION POPUPS ppri1 DEACT POPUPS ppri1
ACTIVATE POPUPS ppri1
DO CASE
   CASE BAR()=0
        RELEASE WIND pri
        CLOSE DATA
        RETURN
   CASE BAR()=3
        SET FILTER TO SUBSTR(order,1,1)='*'
ENDCASE
SET COLOR TO w/w
CLEAR
DEFINE POPUPS ppri2 FROM 10,24 TITLE'[НАПРАВЛЕНИЕ ВЫВОДА]' COLOR ;
       n+/w,n/w,b/w,b/w,,n/r* FONT "Courier New",11
DEFINE BAR 1 OF ppri2 PROMPT 'Вывести в файл с именем '+TblName
DEFINE BAR 2 OF ppri2 PROMPT 'Вывести на принтер'
DEFINE BAR 3 OF ppri2 PROMPT 'Отказ от ВЫВОДА'
ON SELECTION POPUPS ppri2 DEACT POPUPS ppri2
ACTIVATE POPUPS ppri2
DEACT WIND pri
DO CASE
   CASE BAR()=0 OR BAR()=3
        DEACT WIND pri
        CLOSE DATA
        RETURN
   CASE BAR()=1
        PRIV NumRow,CurRow,iRow,SizeFont
        NumRow=40
        SizeFont=12
        SET DEVICE TO FILE (TblName+'.txt')
   CASE BAR()=2
        PRIV NumCol,NumRow,CurRow,iRow,SizeFont,SizeFont1,SizeFont2
        NumCol=IIF(Show_opisanie=.F.,32,68)+(ALEN(b)-4)*11+1 && кол-во колонок 
        SizeFont2=INT(1400/NumCol)
        SizeFont1=INT(SizeFont2*.7)
        SizeFont=SizeFont2
        ACTIV WINDOW pri
        SET COLOR TO b/w
        @ 08,10 SAY "Укажите размер шрифта."
        @ 09,10 SAY "Рекомендуемые значения :"
        @ 10,10 SAY "              для книжного формата   -"+STR(SizeFont1,3)
        @ 11,10 SAY "              для альбомного формата -"+STR(SizeFont2,3)
        @ 13,10 SAY "Ваш вариант:" GET SizeFont SIZE 1,3
        READ
        DO CASE
           CASE ABS(SizeFont-SizeFont1)<=ABS(SizeFont-SizeFont2) && книжный формат
                NumRow=63
           CASE ABS(SizeFont-SizeFont1)> ABS(SizeFont-SizeFont2) && альбомный формат
                NumRow=40
        ENDCASE
        DEACT WIND pri
        SET DEVICE TO PRINTER
        =sys(1037)
        do forma warn_leo
ENDCASE
RELEASE WIND pri
SET BLINK ON
GO TOP
CurRow=prow()
iRow=0
@ CurRow+iRow,0 SAY text+' за '+DTOC(n_k_p)+'-'+DTOC(k_k_p) ;
FONT "Courier New",SizeFont COLOR n && заголовок
iRow=iRow+1
@ CurRow+iRow,0 SAY REPLIC('-',IIF(Show_opisanie=.F.,32,68)+(ALEN(b)-4)*11)+'-';
FONT "Courier New",SizeFont COLOR n 
string='¦ п/п ¦'+IIF(Show_opisanie=.F.,'',;
'            описание               ¦')+'  обозначение  ¦ единицы ¦'
FOR i=5 TO ALEN(b)
    DO CASE
       CASE b(i)='blok'
            string=string+'  блок '+SUBSTR(b(i),5,1)+'  ¦'
       CASE b(i)='gruppa'
            string=string+' группа   ¦'
       CASE b(i)='pk'
            string=string+'   пк     ¦'
       CASE b(i)='station'
            string=string+' станция  ¦'
       CASE b(i)='pvk'
            string=string+'   пвк    ¦'
    ENDCASE
ENDFOR
iRow=iRow+2
@ CurRow+iRow,2 SAY string FONT "Courier New",SizeFont COLOR n 
iRow=iRow+1
@ CurRow+iRow,2 SAY REPLIC('-',IIF(Show_opisanie=.F.,32,68)+(ALEN(b)-4)*11)+'-' ;
FONT "Courier New",SizeFont COLOR n 
DO WHILE .NOT.EOF()
   string='¦'+&b(1)+'¦'+IIF(Show_opisanie=.F.,'',SUBSTR(&b(2),1,35)+'¦')
   b(3)='SUBSTR(mark,1,15)'
   FOR i=3 TO ALEN(b)
       string=string+&b(i)+'¦'
   ENDFOR
   iRow=iRow+1
   IF iRow>NumRow
      STORE 1 TO CurRow,iRow
   ENDIF
   @ CurRow+iRow,2 SAY string FONT "Courier New",SizeFont COLOR n 
   IF Show_opisanie=.T.
      FOR i=2 TO INT((LEN(RTRIM(&b(2)))-1)/35)+1
          iRow=iRow+1
          IF iRow>NumRow
             STORE 1 TO CurRow,iRow
          ENDIF
          @ CurRow+iRow,2 SAY '¦'+SPACE(5)+'¦'+SUBSTR(&b(2)+SPACE(35),;
          35*(i-1)+1,35)+'¦'+SPACE(15)+'¦'+SPACE(9)+'¦'+;
          REPLIC(SPACE(10)+'¦',ALEN(b)-4) FONT "Courier New",SizeFont COLOR n 
      ENDFOR
   ENDIF
   SKIP
ENDDO
iRow=iRow+1
IF iRow>NumRow
   STORE 1 TO CurRow,iRow
ENDIF
@ CurRow+iRow,2 SAY REPLIC('-',IIF(Show_opisanie=.F.,32,68)+(ALEN(b)-4)*11)+'-' ;
FONT "Courier New",SizeFont COLOR n 
*SET PRINTER OFF
SET DEVICE TO SCREEN
POP KEY
CLOSE DATA
