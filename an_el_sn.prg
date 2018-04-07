PUSH KEY CLEAR
ON KEY LABEL F1 HELP ID 13
ON KEY LABEL F5 DO FORM gr_anelsn
_Screen.MousePointer=11
PRIV i,j,v1,v2,sum1,sum2
PUBLIC a1(12,7),a2(22,7),a3(12,7),titul(11,3),punkt,stan
USE inblok
PUBLIC Iinm(reccount(),1)
COPY TO ARRAY Iinm FIELDS order
FOR i=1 TO ALEN(Iinm,1)
    Iinm(i)=ALLTRIM(SUBS(Iinm(i),2))
ENDFOR
COPY TO ARRAY inm FIELDS blok1,blok2,blok3,blok4,blok5,blok6,station
USE outblok
PUBLIC Iiom(reccount(),1)
COPY TO ARRAY Iiom FIELDS order
FOR i=1 TO ALEN(Iiom,1)
    Iiom(i)=ALLTRIM(SUBS(Iiom(i),2))
ENDFOR
COPY TO ARRAY iom FIELDS blok1,blok2,blok3,blok4,blok5,blok6,station
USE outmkt
PUBLIC Ioutm(reccount(),1)
COPY TO ARRAY Ioutm FIELDS order
FOR i=1 TO ALEN(Ioutm,1)
    Ioutm(i)=ALLTRIM(SUBS(Ioutm(i),2))
ENDFOR
COPY TO ARRAY outm FIELDS blok1,blok2,blok3,blok4,blok5,blok6,gruppa
FOR i=1 TO ALEN(inm,1)
FOR j=1 TO 7
    IF NOT Iinm(i)=='74'           && единственный не чисто цифровой признак вх.базы
       inm(i,j)=IIF('s'$inm(i,j) OR '    -'$inm(i,j),'    -',VAL(inm(i,j)))
    ELSE   
       inm(i,j)=RTRIM(inm(i,j))
    ENDIF
ENDFOR    
ENDFOR    
FOR i=1 TO ALEN(iom,1)
FOR j=1 TO 7
    iom(i,j)=IIF('    -'$iom(i,j),'    -',VAL(SUBSTR(iom(i,j),2))) 
ENDFOR    
ENDFOR
FOR i=1 TO ALEN(outm,1)
FOR j=1 TO 7
    outm(i,j)=IIF('    -'$outm(i,j),'    -',VAL(SUBSTR(outm(i,j),2))) 
ENDFOR    
ENDFOR

STORE '    -' TO a1,a2,a3
STORE '' TO titul
FOR i=1 TO 6
    ON ERROR a1(1,i)='    -'
    a1(1,i)=iom(getIndexOfIIoM('30'),i)*iom(getIndexOfIIoM('1'),i)
    ON ERROR
    a1(1,7)=VV('a1(1,7)')+ROUND(VV('a1(1,i)'),0)
    a1(2,i)=inm(getIndexOfIInM('4.1'),i)
    a1(2,7)=VV('a1(2,7)')+ROUND(VV('a1(2,i)'),0)
    ON ERROR a1(3,i)='    -'
    a1(3,i)=iom(getIndexOfIIoM('31'),i)*iom(getIndexOfIIoM('1'),i)
    ON ERROR
    a1(3,7)=VV('a1(3,7)')+ROUND(VV('a1(3,i)'),0)
    a1(4,i)=inm(getIndexOfIInM('4.2'),i)
    a1(4,7)=VV('a1(4,7)')+ROUND(VV('a1(4,i)'),0)
ENDFOR
titul(1,1)=' онденсатные насосы т/а и ѕЌЁ (тыс.к¬тч)'
titul(2,1)='ѕрочие блочные механизмы т/а (тыс.к¬тч)'
ON ERROR a1(5,7)='    -'
a1(5,7)=iom(getIndexOfIIoM('32'),7)*inm(getIndexOfIInM('70'),7)
ON ERROR
a1(6,7)=inm(getIndexOfIInM('5'),7)
titul(3,1)='ѕрочие общестанционные механизмы турбинного отделени€ (тыс.к¬тч)'
ON ERROR a1(7,7)='    -'
a1(7,7)=iom(getIndexOfIIoM('29'),7)*inm(getIndexOfIInM('70'),7)
ON ERROR
a1(8,7)=inm(getIndexOfIInM('6'),7)
titul(4,1)='÷иркул€ционные насосы (тыс.к¬тч)'
FOR i=1 TO 6
    ON ERROR a1(9,i)='    -'
    a1(9,i)=1.03*(iom(getIndexOfIIoM('30'),i)*iom(getIndexOfIIoM('1'),i)+iom(getIndexOfIIoM('31'),i)*iom(getIndexOfIIoM('1'),i)+;
    (iom(getIndexOfIIoM('29'),7)+iom(getIndexOfIIoM('32'),7))*inm(getIndexOfIInM('70'),7)*iom(getIndexOfIIoM('2'),i)/iom(getIndexOfIIoM('2'),7))+;
    iom(getIndexOfIIoM('35'),i)
    ON ERROR
    a1(10,i)=outm(getIndexOfIOutM('28'),i)
ENDFOR
STORE 0 TO sum1,sum2
FOR i=1 TO 6
    v1=iom(getIndexOfIIoM('30'),i)
    v2=iom(getIndexOfIIoM('1'),i)
    IF TYPE('v1')='C' OR TYPE('v2')='C'
       LOOP
    ENDIF
    sum1=sum1+v1*v2
ENDFOR
FOR i=1 TO 6
    v1=iom(getIndexOfIIoM('31'),i)
    v2=iom(getIndexOfIIoM('1'),i)
    IF TYPE('v1')='C' OR TYPE('v2')='C'
       LOOP
    ENDIF
    sum2=sum2+v1*v2
ENDFOR
ON ERROR a1(9,7)='    -'
a1(9,7)=1.03*(sum1+sum2+(iom(getIndexOfIIoM('29'),7)+iom(getIndexOfIIoM('32'),7))*inm(getIndexOfIInM('70'),7))+SUM('iom(getIndexOfIIoM("35"),')
ON ERROR
a1(10,7)=outm(getIndexOfIOutM('28'),7)
titul(5,1)='–асход электроэнергии на —Ќ турбоагрегата (тыс.к¬тч)'
FOR i=1 TO 6
    a1(11,i)=iom(getIndexOfIIoM('36'),i)
    a1(12,i)=outm(getIndexOfIOutM('148'),i)
ENDFOR
a1(11,7)=iom(getIndexOfIIoM('36'),7)
a1(12,7)=outm(getIndexOfIOutM('148'),7)
titul(6,1)='–асход электроэнергии на —Ќ турбоагрегата (%)'
FOR i=1 TO 12
FOR j=1 TO 7
    a1(i,j)=IIF(TYPE('a1(i,j)')='C','    -',ROUND(a1(i,j),IIF(i>=11,2,0)))
ENDFOR
ENDFOR
FOR i=1 TO 6
    a2(1,i)=outm(getIndexOfIOutM('159'),i)
    a2(2,i)=outm(getIndexOfIOutM('160'),i)
    a2(3,i)=outm(getIndexOfIOutM('161'),i)
    a2(4,i)=outm(getIndexOfIOutM('162'),i)
    a2(5,i)=outm(getIndexOfIOutM('157'),i)
    a2(6,i)=outm(getIndexOfIOutM('158'),i)
ENDFOR
a2(1,7)=outm(getIndexOfIOutM('159'),7)
a2(2,7)=outm(getIndexOfIOutM('160'),7)
titul(1,2)='ѕЁЌы (к¬тч/т.воды)'
a2(3,7)=outm(getIndexOfIOutM('161'),7)
a2(4,7)=outm(getIndexOfIOutM('162'),7)
titul(2,2)='“€га и дутье (к¬тч/√кал)'
a2(5,7)=outm(getIndexOfIOutM('157'),7)
a2(6,7)=outm(getIndexOfIOutM('158'),7)
titul(3,2)='ѕылеприготовление (к¬тч/тнт)'
ON ERROR a2(7,7)='    -'
a2(7,7)=iom(getIndexOfIIoM('101'),7)*inm(getIndexOfIInM('88'),7)/1E3
ON ERROR
a2(8,7)=inm(getIndexOfIInM('9.1'),7)
titul(4,2)='¬ыгрузка твердого топлива (тыс.к¬тч)'
STORE 0 TO sum1
FOR i=1 TO 6
    v1=iom(getIndexOfIIoM('100'),i)
    v2=iom(getIndexOfIIoM('90'),i)
    IF TYPE('v1')='C' OR TYPE('v2')='C'
       LOOP
    ENDIF
    sum1=sum1+v1*v2
ENDFOR
a2(9,7)=sum1/1E3
a2(10,7)=inm(getIndexOfIInM('9.2'),7)
titul(5,2)='ѕодача твердого топлива на технологию (тыс.к¬тч)'
ON ERROR a2(11,7)='    -'
a2(11,7)=iom(getIndexOfIIoM('102'),7)*inm(getIndexOfIInM('70'),7)/1E3
ON ERROR
ON ERROR a2(12,7)='    -'
a2(12,7)=inm(getIndexOfIInM('9.3'),7)+inm(getIndexOfIInM('9.4'),7)
ON ERROR
titul(6,2)='«олошлакоудаление + электрофильтры (включа€ компрессор ¬”—) (тыс.к¬тч)'
ON ERROR a2(13,7)='    -'
a2(13,7)=232.6*inm(getIndexOfIInM('70'),7)/1E3
ON ERROR
a2(14,7)=inm(getIndexOfIInM('9.5'),7)
titul(7,2)='ћазутонасосна€ (тыс.к¬тч)'
ON ERROR a2(15,7)='    -'
a2(15,7)=iom(getIndexOfIIoM('103'),7)*inm(getIndexOfIInM('70'),7)/1E3
ON ERROR
a2(16,7)=inm(getIndexOfIInM('9.6'),7)
titul(8,2)='’¬ќ (подпитка котлов) (тыс.к¬тч)'
ON ERROR a2(17,7)='    -'
a2(17,7)=iom(getIndexOfIIoM('105'),7)*inm(getIndexOfIInM('70'),7)/1E3
ON ERROR
a2(18,7)=inm(getIndexOfIInM('9.7'),7)
titul(9,2)='ƒополнительные прочие по котлу (тыс.к¬тч)'
FOR i=1 TO 6
    a2(19,i)=iom(getIndexOfIIoM('108'),i)
    a2(20,i)=outm(getIndexOfIOutM('29'),i)
    a2(21,i)=outm(getIndexOfIOutM('155'),i)
    a2(22,i)=outm(getIndexOfIOutM('156'),i)
ENDFOR
a2(19,7)=iom(getIndexOfIIoM('108'),7)
a2(20,7)=outm(getIndexOfIOutM('29'),7)
titul(10,2)='—уммарный расход электроэнергии на —Ќ котла (тыс.к¬тч)'
a2(21,7)=outm(getIndexOfIOutM('155'),7)
a2(22,7)=outm(getIndexOfIOutM('156'),7)
titul(11,2)='—уммарный расход электроэнергии на —Ќ котла (%)'
FOR i=1 TO 22
FOR j=1 TO 7
    a2(i,j)=IIF(TYPE('a2(i,j)')='C','    -',;
    ROUND(a2(i,j),IIF(INLIST(i,1,2,3,4,5,6,21,22),2,0)))
ENDFOR
ENDFOR
FOR i=1 TO 6
    ON ERROR a3(1,i)='    -'
    a3(1,i)=iom(getIndexOfIIoM('139'),i)*inm(getIndexOfIInM('71'),i)
    ON ERROR
ENDFOR
STORE 0 TO sum1
FOR i=1 TO 6
    v1=iom(getIndexOfIIoM('139'),i)
    v2=inm(getIndexOfIInM('71'),i)
    IF TYPE('v1')='C' OR TYPE('v2')='C'
       LOOP
    ENDIF
    sum1=sum1+v1*v2
ENDFOR
a3(1,7)=sum1
FOR i=1 TO 6
    ON ERROR a3(2,i)='    -'
    a3(2,i)=inm(getIndexOfIInM('10.1'),i)+inm(getIndexOfIInM('10.2'),i)
    ON ERROR
    ON ERROR a3(3,i)='    -'
    a3(3,i)=iom(getIndexOfIIoM('143'),i)*inm(getIndexOfIInM('71'),i)
    ON ERROR
    a3(3,7)=VV('a3(3,7)')+ROUND(VV('a3(3,i)'),0)
    a3(4,i)=inm(getIndexOfIInM('10.3'),i)
    a3(4,7)=VV('a3(4,7)')+ROUND(VV('a3(4,i)'),0)
    a3(9,i)=iom(getIndexOfIIoM('144'),i)
    a3(10,i)=outm(getIndexOfIOutM('27'),i)
    a3(11,i)=outm(getIndexOfIOutM('153'),i)
    a3(12,i)=outm(getIndexOfIOutM('154'),i)
ENDFOR
ON ERROR a3(2,7)='    -'
a3(2,7)=inm(getIndexOfIInM('10.1'),7)+inm(getIndexOfIInM('10.2'),7)+inm(getIndexOfIInM('10.4'),7)
ON ERROR
titul(1,3)='—етевые и подпорные насосы (тыс.к¬тч)'
titul(2,3)=' онденсатные насосы ѕ—√ (тыс.к¬тч)'
ON ERROR a3(5,7)='    -'
a3(5,7)=iom(getIndexOfIIoM('141'),7)*inm(getIndexOfIInM('71'),7)
ON ERROR
a3(6,7)=inm(getIndexOfIInM('11.1'),7)
titul(3,3)='Ќасосы подпитки теплосети (тыс.к¬тч)'
ON ERROR a3(7,7)='    -'
a3(7,7)=iom(getIndexOfIIoM('142'),7)*inm(getIndexOfIInM('71'),7)
ON ERROR
a3(8,7)=inm(getIndexOfIInM('11.2'),7)
titul(4,3)='’имводоочистка (подпитка теплосети) (тыс.к¬тч)'
a3(9,7)=iom(getIndexOfIIoM('144'),7)
a3(10,7)=outm(getIndexOfIOutM('27'),7)
titul(5,3)='—уммарный расход электроэнергии на —Ќ теплофикационной установки (тыс.к¬тч)'
a3(11,7)=outm(getIndexOfIOutM('153'),7)
a3(12,7)=outm(getIndexOfIOutM('154'),7)
titul(6,3)='—уммарный расход электроэнергии на —Ќ теплофикационной установки (к¬тч/√кал)'
FOR i=1 TO 12
FOR j=1 TO 7
    a3(i,j)=IIF(TYPE('a3(i,j)')='C','    -',ROUND(a3(i,j),IIF(INLIST(i,11,12),2,0)))
ENDFOR
ENDFOR
_Screen.MousePointer=0
SET FILTER TO recno()=1
punkt=1
stan=.F.
DO FORM an_el_sn
USE
RELEASE Iinm,Iiom,Ioutm,a1,a2,a3,titul,punkt,stan
POP KEY
**********************************************************************
FUNCTION SUM
PARAMETERS part
PRIVATE i,sum,v
sum=0
FOR i=1 TO 6
  v=EVAL(part+STR(i,1)+')')
  IF TYPE('v')='C'
     LOOP
  ENDIF
  sum=sum+v
ENDFOR
RETURN sum
**********************************************************************
FUNCTION VV
PARAMETER arg
RETURN IIF(TYPE(arg)='C',VAL(&arg),&arg)