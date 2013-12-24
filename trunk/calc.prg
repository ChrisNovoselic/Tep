PROC CALC
* inm,outm - массив-базы INBLOK,OUTBLOK
* Iinm,Ioutm - массивы порядковых №№ INBLOK,OUTBLOK
* outm1 - 1-мерн.массив для GATHER в OUTBLOK
* oum - 2-мерн.массив для GATHER в OUTBLOK
* exact - 2-мерн.массив кол-ва зн.после запятой в OUTBLOK
PUBLIC outm1(ALEN(outm,2)),oum(ALEN(outm,1),ALEN(outm,2))
STORE 1 TO min11,aa11,min12,aa12,min21,aa21,min22,aa22,;
           min111,aa111,min112,aa112,min121,aa121,min122,aa122,;
           min31,aa31,min32,aa32,min211,aa211,min212,aa212,min221,aa221,min222,aa222,;
           min1111,aa1111,min1112,aa1112,min1121,aa1121,min1122,aa1122,;
           min1211,aa1211,min1212,aa1212,min1221,aa1221,min1222,aa1222
FOR i=1 TO ALEN(inm,1)
	FOR j=1 TO ALEN(inm,2)
	    IF NOT Iinm(i)=='74'           && единственный не чисто цифровой признак вх.базы
	       inm(i,j)=IIF(VAL(inm(i,j))=0,IIF('0'$inm(i,j),0,inm(i,j)),;
	       VAL(inm(i,j)))
	    ELSE   
	       inm(i,j)=RTRIM(inm(i,j))
	    ENDIF
	ENDFOR    
ENDFOR
    
FOR i=1 TO ALEN(outm,1)
	FOR j=1 TO ALEN(outm,2)             && отсечение первого символа
	    oum(i,j)=SUBSTR(outm(i,j),2)   && являющегося служебным
	ENDFOR    
ENDFOR

ON ERROR i=i

DO COR_IN.PRG  && ПРОВЕРКА КОРРЕКТНОСТИ ВХ.ТАБЛ.

DO CIKL WITH getIndexOfIOutM("1"),'inm(getIndexOfIInM("1"),i)'                                            && 1 TAU раб
oum(getIndexOfIOutM("1"),n_blokov+1)=CIKL1(getIndexOfIOutM("1"),SUM('inm(getIndexOfIInM("1"),'))                        && 1 TAU раб

DO CIKL WITH getIndexOfIOutM("2"),'inm(getIndexOfIInM("2"),i)'                                            && 2 Э т
oum(getIndexOfIOutM("2"),n_blokov+1)=CIKL1(getIndexOfIOutM("2"),SUM('inm(getIndexOfIInM("2"),'))                        && 2 Э т

IF BL1 OR BL2 OR BL3 OR BL4 OR BL5 OR BL6
	DO CIKL WITH getIndexOfIOutM("3"),'IIF(inm(getIndexOfIInM("47"),i)/inm(getIndexOfIInM("1"),i)<0.7,0,inm(getIndexOfIInM("47"),i)*(inm(getIndexOfIInM("48"),i)-inm(getIndexOfIInM("49"),i)))'                                                            && 3 Q то
ELSE
	DO CIKL WITH getIndexOfIOutM("3"),'inm(getIndexOfIInM("47"),i)*(inm(getIndexOfIInM("48"),i)-inm(getIndexOfIInM("49"),i))'           && 3 Q то
ENDIF
oum(getIndexOfIOutM("3"),n_blokov+1)=CIKL1(getIndexOfIOutM("3"),SUM('oum(getIndexOfIOutM("3"),'))                        && 3 Q то

IF BL1 OR BL2 OR BL3 OR BL4 OR BL5 OR BL6
	DO CIKL WITH getIndexOfIOutM("4"),'IIF(oum(getIndexOfIOutM("3"),i)=0,0,inm(getIndexOfIInM("46"),i))'                    && 4 Q пп
ELSE
	DO CIKL WITH getIndexOfIOutM("4"),'inm(getIndexOfIInM("46"),i)'                                           && 4 Q пп
ENDIF

oum(getIndexOfIOutM("4"),n_blokov+1)=CIKL1(getIndexOfIOutM("4"),SUM('oum(getIndexOfIOutM("4"),'))                        && 4 Q пп
oum(getIndexOfIOutM("5"),n_blokov+1)=CIKL1(getIndexOfIOutM("5"),inm(getIndexOfIInM("81"),n_blokov+1))                   && 5 Q отп ст
oum(getIndexOfIOutM("6"),n_blokov+1)=CIKL1(getIndexOfIOutM("6"),inm(getIndexOfIInM("82"),n_blokov+1))                   && 6 Q отп роу

IF BL1 OR BL2 OR BL3 OR BL4 OR BL5 OR BL6
	DO CIKL WITH getIndexOfIOutM("6"),'oum(getIndexOfIOutM("4"),i)/2'                                          && 6 Q отп роу
ELSE
sum=0
FOR i=1 TO n_blokov
  sum=sum+IIF(oum(getIndexOfIOutM("3"),i)=0,0,oum(getIndexOfIOutM("4"),i))
ENDFOR
DO CIKL WITH getIndexOfIOutM("6"),'IIF(oum(getIndexOfIOutM("3"),i)=0,0,IIF(oum(getIndexOfIOutM("4"),i)=0,0,oum(getIndexOfIOutM("6"),n_blokov+1)*oum(getIndexOfIOutM("4"),i)/sum))'                                                          && 6 Q отп роу
	IF SUM('oum(getIndexOfIOutM("6"),')#oum(getIndexOfIOutM("6"),n_blokov+1)   && устранение ошибк.округления
	   oum(getIndexOfIOutM("6"),1)=oum(getIndexOfIOutM("6"),1)+IIF(SUM('oum(getIndexOfIOutM("6"),')<oum(getIndexOfIOutM("6"),n_blokov+1),1,-1)
	ENDIF
ENDIF

oum(getIndexOfIOutM("7"),n_blokov+1)=CIKL1(getIndexOfIOutM("7"),oum(getIndexOfIOutM("5"),n_blokov+1)-oum(getIndexOfIOutM("6"),n_blokov+1)-inm(getIndexOfIInM("85"),n_blokov+1))                                                       && 7 Q отп тепл
IF BL1 OR BL2 OR BL3 OR BL4 OR BL5 OR BL6
DO CIKL WITH getIndexOfIOutM("7"),'oum(getIndexOfIOutM("3"),i)*IIF(BETWEEN(MONTH(DATE()),6,9) OR MONTH(DATE())=5 AND DAY(DATE())>15,.97,.95)'&& 7 Q отп тепл
ELSE
DO CIKL WITH getIndexOfIOutM("7"),'IIF(oum(getIndexOfIOutM("3"),n_blokov+1)=0,0,oum(getIndexOfIOutM("7"),n_blokov+1)*oum(getIndexOfIOutM("3"),i)/oum(getIndexOfIOutM("3"),n_blokov+1))'                                                      && 7 Q отп тепл
IF SUM('oum(getIndexOfIOutM("7"),')#oum(getIndexOfIOutM("7"),n_blokov+1)   && устранение ошибк.округления 
   oum(getIndexOfIOutM("7"),1)=oum(getIndexOfIOutM("7"),1)+IIF(SUM('oum(getIndexOfIOutM("7"),')<oum(getIndexOfIOutM("7"),n_blokov+1),1,-1)
ENDIF
ENDIF

DO CIKL WITH getIndexOfIOutM("8"),'oum(getIndexOfIOutM("6"),i)+oum(getIndexOfIOutM("7"),i)'                              && 8 Q отп
oum(getIndexOfIOutM("8"),n_blokov+1)=CIKL1(getIndexOfIOutM("8"),SUM('oum(getIndexOfIOutM("8"),'))                        && 8 Q отп

DO CIKL WITH getIndexOfIOutM("9"),'oum(getIndexOfIOutM("2"),i)/oum(getIndexOfIOutM("1"),i)'                              && 9 N т
oum(getIndexOfIOutM("9"),n_blokov+1)=CIKL1(getIndexOfIOutM("9"),oum(getIndexOfIOutM("2"),n_blokov+1)/oum(getIndexOfIOutM("1"),n_blokov+1))&& 9 N т

DO CIKL WITH getIndexOfIOutM("10"),'oum(getIndexOfIOutM("3"),i)/oum(getIndexOfIOutM("1"),i)'                             &&10 Q т ср
oum(getIndexOfIOutM("10"),n_blokov+1)=CIKL1(getIndexOfIOutM("10"),oum(getIndexOfIOutM("3"),n_blokov+1)/oum(getIndexOfIOutM("1"),n_blokov+1))&&10 Q т ср

DO CIKL WITH getIndexOfIOutM("10.1"),'inm(getIndexOfIInM("37"),i)'                                        &&10.1 P вто

DO CIKL WITH getIndexOfIOutM("11"),'oum(getIndexOfIOutM("4"),i)/oum(getIndexOfIOutM("1"),i)'                             &&11 Q роу ср
oum(getIndexOfIOutM("11"),n_blokov+1)=CIKL1(getIndexOfIOutM("11"),oum(getIndexOfIOutM("4"),n_blokov+1)/oum(getIndexOfIOutM("1"),n_blokov+1))&&11 Q роу ср

*       sss=SECON()
DO CIKL WITH getIndexOfIOutM("12"),'IIF(inm(getIndexOfIInM("74"),i)=="1",F1(oum(getIndexOfIOutM("9"),i),"2.40:1"),'+;
'IIF(inm(getIndexOfIInM("74"),i)=="2",F3(oum(getIndexOfIOutM("9"),i),oum(getIndexOfIOutM("10"),i),oum(getIndexOfIOutM("10.1"),i),"2.1:3"),'+;
'IIF(inm(getIndexOfIInM("74"),i)=="2а",F3(oum(getIndexOfIOutM("9"),i),oum(getIndexOfIOutM("10"),i),inm(getIndexOfIInM("38"),i),"2.86:3"),'+;
'IIF(inm(getIndexOfIInM("74"),i)=="3",F2(oum(getIndexOfIOutM("9"),i),oum(getIndexOfIOutM("10.1"),i),"2.50:2"),1/0))))'  &&12 q т бр (исх)

*       wait window (str(SECON()-sss,10,3))
DO CIKL WITH getIndexOfIOutM("13"),'IIF(inm(getIndexOfIInM("74"),i)=="1",F1(oum(getIndexOfIOutM("9"),i),"2.55:1")+inm(getIndexOfIInM("46"),i)/.7/inm(getIndexOfIInM("1"),i),'+;
'IIF(inm(getIndexOfIInM("74"),i)=="2",F3(oum(getIndexOfIOutM("9"),i),oum(getIndexOfIOutM("10"),i),oum(getIndexOfIOutM("10.1"),i),"2.2:3")+inm(getIndexOfIInM("46"),i)/.7/inm(getIndexOfIInM("1"),i),'+;
'IIF(inm(getIndexOfIInM("74"),i)=="2а",F3(oum(getIndexOfIOutM("9"),i),oum(getIndexOfIOutM("10"),i),inm(getIndexOfIInM("38"),i),"2.87:3")+inm(getIndexOfIInM("46"),i)/.7/inm(getIndexOfIInM("1"),i),'+;
'IIF(inm(getIndexOfIInM("74"),i)=="3",F3(oum(getIndexOfIOutM("9"),i),oum(getIndexOfIOutM("10"),i),oum(getIndexOfIOutM("10.1"),i),"2.2:3")+inm(getIndexOfIInM("46"),i)/.7/inm(getIndexOfIInM("1"),i),1/0))))'                                      &&13 G o

DO CIKL WITH getIndexOfIOutM("14"),'IIF(inm(getIndexOfIInM("74"),i)=="1",F1(oum(getIndexOfIOutM("13"),i),"2.3:1")-inm(getIndexOfIInM("46"),i)/.7/inm(getIndexOfIInM("1"),i),'+;
'IIF(inm(getIndexOfIInM("74"),i)=="2" OR inm(getIndexOfIInM("74"),i)=="3",F3(oum(getIndexOfIOutM("13"),i),oum(getIndexOfIOutM("10"),i),oum(getIndexOfIOutM("10.1"),i),"2.3б:3")-inm(getIndexOfIInM("46"),i)/.7/inm(getIndexOfIInM("1"),i),'+;
'IIF(inm(getIndexOfIInM("74"),i)=="2а",F3(oum(getIndexOfIOutM("13"),i),oum(getIndexOfIOutM("10"),i),inm(getIndexOfIInM("38"),i),"2.3а:3")-inm(getIndexOfIInM("46"),i)/.7/inm(getIndexOfIInM("1"),i),1/0)))'                                      &&14 G 2
DO CIKL WITH getIndexOfIOutM("14"),'IIF(oum(getIndexOfIOutM("14"),i)<0,0,oum(getIndexOfIOutM("14"),i))'                  &&14 G 2
oum(getIndexOfIOutM("14"),n_blokov+1)=CIKL1(getIndexOfIOutM("14"),IIF(BL1 OR BL4 OR BL5 OR BL6,SUM('oum(getIndexOfIOutM("14"),')*n_blokov1,SUM('oum(getIndexOfIOutM("14"),')))                                                          &&14 G 2
oum(getIndexOfIOutM("14.1"),n_blokov+1)=CIKL1(getIndexOfIOutM("14.1"),IIF(inm(getIndexOfIInM("70"),n_blokov+1)=0,0,F2(IIF(BL1 OR BL4 OR BL5 OR BL6,n_blokov1,inm(getIndexOfIInM("89"),n_blokov+1)),;
ROUND(inm(getIndexOfIInM("6"),n_blokov+1)/inm(getIndexOfIInM("70"),n_blokov+1)/1.9,1),"2.4а:2")))        &&14.1 G цв

DO CIKL WITH getIndexOfIOutM("15"),'F3(oum(getIndexOfIOutM("14"),i),inm(getIndexOfIInM("28"),i),oum(getIndexOfIOutM("14.1"),n_blokov+1),"2.4:3")'
                                                                               &&15 Р 2(н)
DO CIKL WITH getIndexOfIOutM("15.1"),'F2(oum(getIndexOfIOutM("15"),i),oum(getIndexOfIOutM("14"),i),"2.84:2")'            &&15.1 dQ э(P2)

DO CIKL WITH getIndexOfIOutM("16"),'1E3*oum(getIndexOfIOutM("15.1"),i)/oum(getIndexOfIOutM("9"),i)'                      &&16 dqт бр(P2)

DO CIKL WITH getIndexOfIOutM("17"),'IIF(inm(getIndexOfIInM("74"),i)=="1",F1(oum(getIndexOfIOutM("13"),i),"2.5а:1")*oum(getIndexOfIOutM("11"),i),'+;
'IIF(inm(getIndexOfIInM("74"),i)=="2" OR inm(getIndexOfIInM("74"),i)=="2а" OR inm(getIndexOfIInM("74"),i)=="3",F2(oum(getIndexOfIOutM("13"),i),oum(getIndexOfIOutM("10"),i),"2.5:2")*oum(getIndexOfIOutM("11"),i),1/0))'               &&17 dqт бр(Qпп)

DO CIKL WITH getIndexOfIOutM("18"),'IIF(inm(getIndexOfIInM("74"),i)=="1",1/0,'+;
'IIF(inm(getIndexOfIInM("74"),i)=="2",F1(oum(getIndexOfIOutM("10.1"),i),"2.6:1"),'+;
'IIF(inm(getIndexOfIInM("74"),i)=="2а",F1(inm(getIndexOfIInM("38"),i),"2.89:1"),'+;
'IIF(inm(getIndexOfIInM("74"),i)=="3",F1(oum(getIndexOfIOutM("10.1"),i),"2.6:1"),1/0))))'                 &&18 t 2(н)

DO CIKL WITH getIndexOfIOutM("19"),'inm(getIndexOfIInM("49"),i)-oum(getIndexOfIOutM("18"),i)'                           &&19 dt 2

DO CIKL WITH getIndexOfIOutM("20"),'IIF(inm(getIndexOfIInM("74"),i)=="1" OR inm(getIndexOfIInM("74"),i)=="2а",0,1/0)'  &&20 dqт бр(t 2)   

PUBLIC o20_1(n_blokov),o20_2(n_blokov)                                         && для 4-мерного(!!!) графика
DO CIKL WITH getIndexOfIOutM("20"),'IIF(inm(getIndexOfIInM("74"),i)=="2" OR inm(getIndexOfIInM("74"),i)=="3",'+;
'F3(oum(getIndexOfIOutM("13"),i),oum(getIndexOfIOutM("10"),i),oum(getIndexOfIOutM("19"),i),'+;
'"2.7"+IIF(oum(getIndexOfIOutM("10.1"),i)<0.8,"",IIF(BETWEEN(oum(getIndexOfIOutM("10.1"),i),0.8,0.99),"",IIF(BETWEEN(oum(getIndexOfIOutM("10.1"),i),1,1.19),"а",'+;
'IIF(BETWEEN(oum(getIndexOfIOutM("10.1"),i),1.2,1.39),"б",IIF(BETWEEN(oum(getIndexOfIOutM("10.1"),i),1.4,1.59),"в",'+;
'IIF(BETWEEN(oum(getIndexOfIOutM("10.1"),i),1.6,1.79),"г",IIF(oum(getIndexOfIOutM("10.1"),i)>=1.8,"д","")))))))'+;
'+IIF(oum(getIndexOfIOutM("19"),i)<0,"-","+")+":3"),oum(getIndexOfIOutM("20"),i))'                         &&20 dqт бр(t 2)   
FOR i=1 TO n_blokov
  o20_1(i)=oum(getIndexOfIOutM("20"),i)
ENDFOR
DO CIKL WITH getIndexOfIOutM("20"),'IIF(inm(getIndexOfIInM("74"),i)=="2" OR inm(getIndexOfIInM("74"),i)=="3",'+;
'F3(oum(getIndexOfIOutM("13"),i),oum(getIndexOfIOutM("10"),i),oum(getIndexOfIOutM("19"),i),'+;
'"2.7"+IIF(oum(getIndexOfIOutM("10.1"),i)<0.8,"",IIF(BETWEEN(oum(getIndexOfIOutM("10.1"),i),0.8,0.99),"а",'+;
'IIF(BETWEEN(oum(getIndexOfIOutM("10.1"),i),1,1.19),"б",IIF(BETWEEN(oum(getIndexOfIOutM("10.1"),i),1.2,1.39),"в",'+;
'IIF(BETWEEN(oum(getIndexOfIOutM("10.1"),i),1.4,1.59),"г",IIF(BETWEEN(oum(getIndexOfIOutM("10.1"),i),1.6,1.79),"д",'+;
'IIF(oum(getIndexOfIOutM("10.1"),i)>=1.8,"е","")))))))'+;
'+IIF(oum(getIndexOfIOutM("19"),i)<0,"-","+")+":3"),oum(getIndexOfIOutM("20"),i))'                         &&20 dqт бр(t 2)   
FOR i=1 TO n_blokov
  o20_2(i)=oum(getIndexOfIOutM("20"),i)
ENDFOR
*wait wind str(o20_1(n_blokov),10,2)+" | "+str(o20_2(n_blokov),10,2)
DO CIKL WITH getIndexOfIOutM("20"),'IIF(inm(getIndexOfIInM("74"),i)=="2" OR inm(getIndexOfIInM("74"),i)=="3",'+;
'IIF(oum(getIndexOfIOutM("10.1"),i)<0.8,o20_1(i),'+;
'IIF(BETWEEN(oum(getIndexOfIOutM("10.1"),i),0.8,0.99),(o20_1(i)*(0.99-oum(getIndexOfIOutM("10.1"),i))+o20_2(i)*(oum(getIndexOfIOutM("10.1"),i)-0.8))/0.19,'+;
'IIF(BETWEEN(oum(getIndexOfIOutM("10.1"),i),1,1.19),(o20_1(i)*(1.19-oum(getIndexOfIOutM("10.1"),i))+o20_2(i)*(oum(getIndexOfIOutM("10.1"),i)-1))/0.19,'+;
'IIF(BETWEEN(oum(getIndexOfIOutM("10.1"),i),1.2,1.39),(o20_1(i)*(1.39-oum(getIndexOfIOutM("10.1"),i))+o20_2(i)*(oum(getIndexOfIOutM("10.1"),i)-1.2))/0.19,'+;
'IIF(BETWEEN(oum(getIndexOfIOutM("10.1"),i),1.4,1.59),(o20_1(i)*(1.59-oum(getIndexOfIOutM("10.1"),i))+o20_2(i)*(oum(getIndexOfIOutM("10.1"),i)-1.4))/0.19,'+;
'IIF(BETWEEN(oum(getIndexOfIOutM("10.1"),i),1.6,1.79),(o20_1(i)*(1.79-oum(getIndexOfIOutM("10.1"),i))+o20_2(i)*(oum(getIndexOfIOutM("10.1"),i)-1.6))/0.19,'+;
'IIF(BETWEEN(oum(getIndexOfIOutM("10.1"),i),1.8,1.99),(o20_1(i)*(1.99-oum(getIndexOfIOutM("10.1"),i))+o20_2(i)*(oum(getIndexOfIOutM("10.1"),i)-1.8))/0.19,'+;
'o20_2(i) ))))))),oum(getIndexOfIOutM("20"),i))'                                             &&20 dqт бр(t 2)   
RELEASE o20_1,o20_2                                                            && для 4-мерного(!!!) графика
DO CIKL WITH getIndexOfIOutM("20"),'IIF(oum(getIndexOfIOutM("19"),i)=0,0,oum(getIndexOfIOutM("20"),i))'                  &&20 dqт бр(t 2) 

DO CIKL WITH getIndexOfIOutM("21"),'IIF(inm(getIndexOfIInM("25"),i)/oum(getIndexOfIOutM("1"),i)=oum(getIndexOfIOutM("13"),i),0,'+;
'IIF(inm(getIndexOfIInM("74"),i)=="1",F1(oum(getIndexOfIOutM("13"),i),"2.8"+'+;
'IIF(inm(getIndexOfIInM("25"),i)/oum(getIndexOfIOutM("1"),i)>oum(getIndexOfIOutM("13"),i),"-","+")+":1"),'+;
'IIF( inm(getIndexOfIInM("74"),i)=="2" OR inm(getIndexOfIInM("74"),i)=="2а" OR inm(getIndexOfIInM("74"),i)=="3",F2(oum(getIndexOfIOutM("13"),i),oum(getIndexOfIOutM("10"),i),"2.8а"+'+;
'IIF(inm(getIndexOfIInM("25"),i)/oum(getIndexOfIOutM("1"),i)>oum(getIndexOfIOutM("13"),i),"-","+")+":2"),1/0)))'        &&21 dqт бр(Gпв)
*O ALTERC WITH getIndexOfIOutM("22"),1,'IIF(inm(getIndexOfIInM("72"),i)>35000,oum(getIndexOfIOutM("12"),i)*.0085*'+;
'(inm(getIndexOfIInM("72"),i)-57135)/1E5,0)'                                                &&22 dqт бр(рес)
*O ALTERC WITH getIndexOfIOutM("22"),2,'IIF(inm(getIndexOfIInM("72"),i)>35000,oum(getIndexOfIOutM("12"),i)*.0085*'+;
'(inm(getIndexOfIInM("72"),i)-53904)/1E5,0)'                                                &&22 dqт бр(рес)
*O ALTERC WITH getIndexOfIOutM("22"),3,'IIF(inm(getIndexOfIInM("72"),i)>35000,oum(getIndexOfIOutM("12"),i)*.0085*'+;
'(inm(getIndexOfIInM("72"),i)-44557)/1E5,0)'                                                &&22 dqт бр(рес)
*O ALTERC WITH getIndexOfIOutM("22"),4,'IIF(inm(getIndexOfIInM("72"),i)>35000,oum(getIndexOfIOutM("12"),i)*.0085*'+;
'(inm(getIndexOfIInM("72"),i)-35717)/1E5,0)'                                                &&22 dqт бр(рес)
*O ALTERC WITH getIndexOfIOutM("22"),5,'IIF(inm(getIndexOfIInM("72"),i)>35000,oum(getIndexOfIOutM("12"),i)*.0085*'+;
'(inm(getIndexOfIInM("72"),i)-14771)/1E5,0)'                                                &&22 dqт бр(рес)
*O ALTERC WITH getIndexOfIOutM("22"),6,'IIF(inm(getIndexOfIInM("72"),i)>35000,oum(getIndexOfIOutM("12"),i)*.0085*'+;
'(inm(getIndexOfIInM("72"),i)-0)/1E5,0)'                                                    &&22 dqт бр(рес)
DO ALTERC WITH getIndexOfIOutM("22"),1,'IIF(inm(getIndexOfIInM("72"),i)>35000,oum(getIndexOfIOutM("12"),i)*.0085*(inm(getIndexOfIInM("72"),i)-35000)/1E5,0)'                                                &&22 dqт бр(рес)
DO ALTERC WITH getIndexOfIOutM("22"),2,'IIF(inm(getIndexOfIInM("72"),i)>35000,oum(getIndexOfIOutM("12"),i)*.0085*(inm(getIndexOfIInM("72"),i)-35000)/1E5,0)'                                                &&22 dqт бр(рес)
DO ALTERC WITH getIndexOfIOutM("22"),3,'IIF(inm(getIndexOfIInM("72"),i)>35000,oum(getIndexOfIOutM("12"),i)*.0085*(inm(getIndexOfIInM("72"),i)-35000)/1E5,0)'                                                &&22 dqт бр(рес)
DO ALTERC WITH getIndexOfIOutM("22"),4,'IIF(inm(getIndexOfIInM("72"),i)>35000,oum(getIndexOfIOutM("12"),i)*.0085*(inm(getIndexOfIInM("72"),i)-35000)/1E5,0)'                                                &&22 dqт бр(рес)
DO ALTERC WITH getIndexOfIOutM("22"),5,'IIF(inm(getIndexOfIInM("72"),i)>35000,oum(getIndexOfIOutM("12"),i)*.0085*(inm(getIndexOfIInM("72"),i)-35000)/1E5,0)'                                                &&22 dqт бр(рес)
DO ALTERC WITH getIndexOfIOutM("22"),6,'IIF(inm(getIndexOfIInM("72"),i)>35000,oum(getIndexOfIOutM("12"),i)*.0085*(inm(getIndexOfIInM("72"),i)-35000)/1E5,0)'                                                    &&22 dqт бр(рес)

DO CIKL WITH getIndexOfIOutM("23"),'182.3*inm(getIndexOfIInM("69"),i)*1E3/oum(getIndexOfIOutM("2"),i)'                  &&23 dqт бр(пуск)

DO CIKL WITH getIndexOfIOutM("24"),'oum(getIndexOfIOutM("12"),i)+oum(getIndexOfIOutM("16"),i)+oum(getIndexOfIOutM("17"),i)+oum(getIndexOfIOutM("20"),i)+oum(getIndexOfIOutM("21"),i)+oum(getIndexOfIOutM("22"),i)+oum(getIndexOfIOutM("23"),i)'                                 &&24 qт бр(ном)
sum=0
FOR i=1 TO n_blokov
  sum=sum+oum(getIndexOfIOutM("24"),i)*oum(getIndexOfIOutM("2"),i)
ENDFOR
oum(getIndexOfIOutM("24"),n_blokov+1)=CIKL1(getIndexOfIOutM("24"),sum/oum(getIndexOfIOutM("2"),n_blokov+1))              &&24 qт бр(ном)

DO CIKL WITH getIndexOfIOutM("25"),'IIF(inm(getIndexOfIInM("74"),i)=="1",0,'+;
'IIF(inm(getIndexOfIInM("74"),i)=="2",F2(oum(getIndexOfIOutM("13"),i),oum(getIndexOfIOutM("10.1"),i),"2.9:2"),'+;
'IIF(inm(getIndexOfIInM("74"),i)=="2а",F2(oum(getIndexOfIOutM("13"),i),inm(getIndexOfIInM("38"),i),"2.9а:2"),'+;
'IIF(inm(getIndexOfIInM("74"),i)=="3",F2(oum(getIndexOfIOutM("13"),i),inm(getIndexOfIInM("38"),i),"2.9б:2"),1/0))))'   &&25 W т/тф(ном)
*DO CIKL WITH getIndexOfIOutM("49"),'(inm(getIndexOfIInM("17"),i)+inm(getIndexOfIInM("17.1"),i))/2-'+;
'F1((inm(getIndexOfIInM("13"),i)+inm(getIndexOfIInM("14"),i))/inm(getIndexOfIInM("1"),i),"2.22:1")+273.15',.F.        &&49 D пе
*DO CIKL WITH getIndexOfIOutM("49"),'(inm(getIndexOfIInM("13"),i)+inm(getIndexOfIInM("14"),i))*SQRT(.02526/(4.7061/1E2*'+;
'(oum(getIndexOfIOutM("49"),i)/1000)/((inm(getIndexOfIInM("15.2"),i)+F1((inm(getIndexOfIInM("13"),i)+inm(getIndexOfIInM("14"),i))/oum(getIndexOfIOutM("1"),i),'+;
'"2.41:1"))/100)+.32371/1E3+2.5/1E4*(oum(getIndexOfIOutM("49"),i)/1000)-'+;
'1.1354/1E3/(oum(getIndexOfIOutM("49"),i)/1000)^2-4.381/1E4/((oum(getIndexOfIOutM("49"),i)/1000)-.21)^2-(2.549/1E5/'+;
'(oum(getIndexOfIOutM("49"),i)/1000)^8+1.236/1E7/(oum(getIndexOfIOutM("49"),i)/1000)^14-5.5/1E5)*((inm(getIndexOfIInM("15.2"),i)+'+;
'F1((inm(getIndexOfIInM("13"),i)+inm(getIndexOfIInM("14"),i))/oum(getIndexOfIOutM("1"),i),"2.41:1"))/100)))'           &&49 D пе
*DO ALTERC WITH getIndexOfIOutM("49"),1,'inm(getIndexOfIInM("13"),i)+inm(getIndexOfIInM("14"),i)'                       &&49 D пе
*DO ALTERC WITH getIndexOfIOutM("49"),4,'inm(getIndexOfIInM("13"),i)+inm(getIndexOfIInM("14"),i)'                       &&49 D пе
*DO ALTERC WITH getIndexOfIOutM("49"),5,'inm(getIndexOfIInM("13"),i)+inm(getIndexOfIInM("14"),i)'                       &&49 D пе
DO CIKL WITH getIndexOfIOutM("49"),'inm(getIndexOfIInM("13"),i)+inm(getIndexOfIInM("14"),i)'                           &&49 D пе
oum(getIndexOfIOutM("49"),n_blokov+1)=CIKL1(getIndexOfIOutM("49"),SUM('oum(getIndexOfIOutM("49"),'))                     &&49 D пе
DO CIKL WITH getIndexOfIOutM("50"),'oum(getIndexOfIOutM("49"),i)/oum(getIndexOfIOutM("1"),i)'                            &&50 D пе
oum(getIndexOfIOutM("50"),n_blokov+1)=CIKL1(getIndexOfIOutM("50"),oum(getIndexOfIOutM("49"),n_blokov+1)/oum(getIndexOfIOutM("1"),n_blokov+1))&&50 D пе
DO CIKL WITH getIndexOfIOutM("51"),'(inm(getIndexOfIInM("17"),i)+inm(getIndexOfIInM("17.1"),i))/2'                     &&51 t пе
DO ALTERC WITH getIndexOfIOutM("51"),5,'(inm(getIndexOfIInM("18"),i)+inm(getIndexOfIInM("18.1"),i))/2+'+;
'F1(oum(getIndexOfIOutM("50"),i),"2.22:1")'                                                  &&51 t пе
DO CIKL WITH getIndexOfIOutM("51.1"),'(inm(getIndexOfIInM("17"),i)+inm(getIndexOfIInM("17.1"),i))/2-F1(oum(getIndexOfIOutM("50"),i),"2.22:1")'&&51.1 t oп
DO ALTERC WITH getIndexOfIOutM("51.1"),1,'(inm(getIndexOfIInM("18"),i)+inm(getIndexOfIInM("18.1"),i))/2'               &&51.1 t oп
DO ALTERC WITH getIndexOfIOutM("51.1"),4,'(inm(getIndexOfIInM("18"),i)+inm(getIndexOfIInM("18.1"),i))/2'               &&51.1 t oп
DO ALTERC WITH getIndexOfIOutM("51.1"),5,'(inm(getIndexOfIInM("18"),i)+inm(getIndexOfIInM("18.1"),i))/2'               &&51.1 t oп
DO ALTERC WITH getIndexOfIOutM("51.1"),6,'(inm(getIndexOfIInM("18"),i)+inm(getIndexOfIInM("18.1"),i))/2'               &&51.1 t oп
DO CIKL WITH getIndexOfIOutM("54"),'IIF(inm(getIndexOfIInM("74"),i)=="1",F1(oum(getIndexOfIOutM("50"),i),"2.24а:1"),'+;
'IIF(inm(getIndexOfIInM("74"),i)=="2" OR inm(getIndexOfIInM("74"),i)=="3",F1(oum(getIndexOfIOutM("50"),i),"2.24:1"),'+;
'IIF(inm(getIndexOfIInM("74"),i)=="2а",F1(oum(getIndexOfIOutM("50"),i),"2.24б:1"),1/0)))'                 &&54 D хпп
DO CIKL WITH getIndexOfIOutM("55"),'oum(getIndexOfIOutM("54"),i)*oum(getIndexOfIOutM("1"),i)-inm(getIndexOfIInM("46"),i)/.7'          &&55 D гпп
DO CIKL WITH getIndexOfIOutM("52"),'(inm(getIndexOfIInM("21"),i)+inm(getIndexOfIInM("21.1"),i))/2'                     &&52 t гпп
DO ALTERC WITH getIndexOfIOutM("52"),5,'(inm(getIndexOfIInM("22"),i)+inm(getIndexOfIInM("22.1"),i))/2+F1(oum(getIndexOfIOutM("55"),i)/oum(getIndexOfIOutM("1"),i),"2.22б:1")'                                   &&52 t гпп
DO CIKL WITH getIndexOfIOutM("52.1"),'(inm(getIndexOfIInM("21"),i)+inm(getIndexOfIInM("21.1"),i))/2-F1(oum(getIndexOfIOutM("55"),i)/oum(getIndexOfIOutM("1"),i),"2.22б:1")'                                   &&52.1 t гпп
DO ALTERC WITH getIndexOfIOutM("52.1"),1,'(inm(getIndexOfIInM("22"),i)+inm(getIndexOfIInM("22.1"),i))/2'               &&52.1 t гпп
DO ALTERC WITH getIndexOfIOutM("52.1"),4,'(inm(getIndexOfIInM("22"),i)+inm(getIndexOfIInM("22.1"),i))/2'               &&52.1 t гпп
DO ALTERC WITH getIndexOfIOutM("52.1"),5,'(inm(getIndexOfIInM("22"),i)+inm(getIndexOfIInM("22.1"),i))/2'               &&52.1 t гпп
DO ALTERC WITH getIndexOfIOutM("52.1"),6,'(inm(getIndexOfIInM("22"),i)+inm(getIndexOfIInM("22.1"),i))/2'               &&52.1 t гпп

DO CIKL WITH getIndexOfIOutM("53"),'inm(getIndexOfIInM("19"),i)'                                          &&53 P гпп

DO CIKL WITH getIndexOfIOutM("56"),'(inm(getIndexOfIInM("15"),i)+inm(getIndexOfIInM("15.1"),i))/2+F1(oum(getIndexOfIOutM("50"),i),"2.41а:1")'                                                 &&56 P пе

DO ALTERC WITH getIndexOfIOutM("56"),4,'(inm(getIndexOfIInM("15"),i)+inm(getIndexOfIInM("15.1"),i))/2'                 &&56 P пе

DO ALTERC WITH getIndexOfIOutM("56"),6,'(inm(getIndexOfIInM("15"),i)+inm(getIndexOfIInM("15.1"),i))/2'                 &&56 P пе

DO CIKL WITH getIndexOfIOutM("56.1"),'inm(getIndexOfIInM("15.2"),i)'                                      &&56.1 Pо
*O ALTERC WITH getIndexOfIOutM("56.1"),5,'(inm(getIndexOfIInM("15"),i)+inm(getIndexOfIInM("15.1"),i))/2-F1(oum(getIndexOfIOutM("50"),i),"2.41:1")'&&56.1 Pо

DO CIKL WITH getIndexOfIOutM("57"),'503.43+11.02849*LOG((oum(getIndexOfIOutM("51"),i)+273.15)/647.27)+'+;
'229.2569*(oum(getIndexOfIOutM("51"),i)+273.15)/647.27+37.93129*((oum(getIndexOfIOutM("51"),i)+273.15)/647.27)**2+'+;
'(0.758195-7.97826/((oum(getIndexOfIOutM("51"),i)+273.15)/1000)**2-(3.078455*'+;
'(oum(getIndexOfIOutM("51"),i)+273.15)/1000-.21549)/((oum(getIndexOfIOutM("51"),i)+273.15)/1000-.21)**3)*'+;
'oum(getIndexOfIOutM("56"),i)/100+(.0644126-.268671/((oum(getIndexOfIOutM("51"),i)+273.15)/1000)**8-'+;
'.216661/100/((oum(getIndexOfIOutM("51"),i)+273.15)/1000)**14)*(oum(getIndexOfIOutM("56"),i)/100)**2'      &&57 i пе

DO CIKL WITH getIndexOfIOutM("57.1"),'503.43+11.02849*LOG((oum(getIndexOfIOutM("51.1"),i)+273.15)/647.27)+'+;
'229.2569*(oum(getIndexOfIOutM("51.1"),i)+273.15)/647.27+37.93129*((oum(getIndexOfIOutM("51.1"),i)+273.15)/647.27)**2+'+;
'(0.758195-7.97826/((oum(getIndexOfIOutM("51.1"),i)+273.15)/1000)**2-(3.078455*'+;
'(oum(getIndexOfIOutM("51.1"),i)+273.15)/1000-.21549)/((oum(getIndexOfIOutM("51.1"),i)+273.15)/1000-.21)**3)*'+;
'oum(getIndexOfIOutM("56.1"),i)/100+(.0644126-.268671/((oum(getIndexOfIOutM("51.1"),i)+273.15)/1000)**8-'+;
'.216661/100/((oum(getIndexOfIOutM("51.1"),i)+273.15)/1000)**14)*(oum(getIndexOfIOutM("56.1"),i)/100)**2'  &&57.1 i оп

DO CIKL WITH getIndexOfIOutM("58"),'(49.4+402.5*inm(getIndexOfIInM("26"),i)/100+4.767*(inm(getIndexOfIInM("26"),i)/100)**2+'+;
'.0333*(inm(getIndexOfIInM("26"),i)/100)**6+(-9.25+1.67*inm(getIndexOfIInM("26"),i)/100+.00736*'+;
'(inm(getIndexOfIInM("26"),i)/100)**6-.008*(1/(inm(getIndexOfIInM("26"),i)/100+.5))**5)*(50-inm(getIndexOfIInM("16"),i)*'+;
'.0980665)/10+(-.073+.079*inm(getIndexOfIInM("26"),i)/100+.00068*(inm(getIndexOfIInM("26"),i)/100)**6)*'+;
'((50-inm(getIndexOfIInM("16"),i)*.0980665)/10)**2+3.39/1E8*(inm(getIndexOfIInM("26"),i)/100)**12*'+;
'((50-inm(getIndexOfIInM("16"),i)*.0980665)/10)**4)/4.1868'                                 &&58 i пв

DO CIKL WITH getIndexOfIOutM("59"),'503.43+11.02849*LOG((oum(getIndexOfIOutM("52"),i)+273.15)/647.27)+'+;
'229.2569*(oum(getIndexOfIOutM("52"),i)+273.15)/647.27+37.93129*((oum(getIndexOfIOutM("52"),i)+273.15)/647.27)**2+'+;
'(0.758195-7.97826/((oum(getIndexOfIOutM("52"),i)+273.15)/1000)**2-(3.078455*'+;
'(oum(getIndexOfIOutM("52"),i)+273.15)/1000-.21549)/((oum(getIndexOfIOutM("52"),i)+273.15)/1000-.21)**3)*'+;
'oum(getIndexOfIOutM("53"),i)/100+(.0644126-.268671/((oum(getIndexOfIOutM("52"),i)+273.15)/1000)**8-'+;
'.216661/100/((oum(getIndexOfIOutM("52"),i)+273.15)/1000)**14)*(oum(getIndexOfIOutM("53"),i)/100)**2'      &&59 i гпп к

DO CIKL WITH getIndexOfIOutM("59.1"),'503.43+11.02849*LOG((oum(getIndexOfIOutM("52.1"),i)+273.15)/647.27)+'+;
'229.2569*(oum(getIndexOfIOutM("52.1"),i)+273.15)/647.27+37.93129*((oum(getIndexOfIOutM("52.1"),i)+273.15)/647.27)**2+'+;
'(0.758195-7.97826/((oum(getIndexOfIOutM("52.1"),i)+273.15)/1000)**2-(3.078455*'+;
'(oum(getIndexOfIOutM("52.1"),i)+273.15)/1000-.21549)/((oum(getIndexOfIOutM("52.1"),i)+273.15)/1000-.21)**3)*'+;
'oum(getIndexOfIOutM("53"),i)/100+(.0644126-.268671/((oum(getIndexOfIOutM("52.1"),i)+273.15)/1000)**8-'+;
'.216661/100/((oum(getIndexOfIOutM("52.1"),i)+273.15)/1000)**14)*(oum(getIndexOfIOutM("53"),i)/100)**2'    &&59 i гпп т

DO CIKL WITH getIndexOfIOutM("60"),'503.43+11.02849*LOG((inm(getIndexOfIInM("24"),i)+273.15)/647.27)+'+;
'229.2569*(inm(getIndexOfIInM("24"),i)+273.15)/647.27+37.93129*((inm(getIndexOfIInM("24"),i)+273.15)/647.27)**2+'+;
'(0.758195-7.97826/((inm(getIndexOfIInM("24"),i)+273.15)/1000)**2-(3.078455*'+;
'(inm(getIndexOfIInM("24"),i)+273.15)/1000-.21549)/((inm(getIndexOfIInM("24"),i)+273.15)/1000-.21)**3)*'+;
'inm(getIndexOfIInM("23"),i)/100+(.0644126-.268671/((inm(getIndexOfIInM("24"),i)+273.15)/1000)**8-'+;
'.216661/100/((inm(getIndexOfIInM("24"),i)+273.15)/1000)**14)*(inm(getIndexOfIInM("23"),i)/100)**2'      &&60 i хпп

* 61-ый - в 2 приема:
DO CIKL WITH getIndexOfIOutM("61"),'1/(2.6864264-.20096551*LOG(inm(getIndexOfIInM("41"),i))-2.16688/1E3*'+;
'LOG(inm(getIndexOfIInM("41"),i))**2-9.480808/1E5*LOG(inm(getIndexOfIInM("41"),i))**3+6.135062/1E6*'+;
'LOG(inm(getIndexOfIInM("41"),i))**4+3.6917245/1E6*LOG(inm(getIndexOfIInM("41"),i))**5)',.F.             &&61 i пр
DO CIKL WITH getIndexOfIOutM("61"),'-753.317+6959.4093*oum(getIndexOfIOutM("61"),i)-29257.981*oum(getIndexOfIOutM("61"),i)**2+'+;
'71285.169*oum(getIndexOfIOutM("61"),i)**3-86752.84*oum(getIndexOfIOutM("61"),i)**4+42641.056*'+;
'oum(getIndexOfIOutM("61"),i)**5'                                                            &&61 i пр

DO CIKL WITH getIndexOfIOutM("62"),'IIF(inm(getIndexOfIInM("74"),i)=="1",1/0,'+;
'IIF(inm(getIndexOfIInM("74"),i)=="2" OR inm(getIndexOfIInM("74"),i)=="3",F2(oum(getIndexOfIOutM("50"),i),oum(getIndexOfIOutM("10.1"),i),"2.83:2"),'+;
'IIF(inm(getIndexOfIInM("74"),i)=="2а",F2(oum(getIndexOfIOutM("50"),i),inm(getIndexOfIInM("38"),i),"2.83а:2"),1/0)))'  &&62 i т

* 63-ий - написанному в инструкции не верить
* P2 = F1(inm(getIndexOfIInM("41.1"),i),"2.50:1") - f(tвых цнд1) для бл.2,3,4,5
* для блока 1 - f((tвых цнд1 + tвых цнд2)/2)
*F BL5
*O CIKL WITH getIndexOfIOutM("63"),'F1(1-inm(getIndexOfIInM("30"),i)/100,"2.91:1")'                       &&63 i 2
DO CIKL WITH getIndexOfIOutM("63"),'F1(inm(getIndexOfIInM("30"),i)/98.067,"2.91:1")'                      &&63 i 2
*O ALTERC WITH getIndexOfIOutM("63"),1,'F1(inm(getIndexOfIInM("90"),n_blokov+1)/735.6-inm(getIndexOfIInM("30"),i),'+;
*"2.91:1")'                                                                    &&63 i 2
*O ALTERC WITH getIndexOfIOutM("63"),5,'F1(inm(getIndexOfIInM("90"),n_blokov+1)/735.6-inm(getIndexOfIInM("30"),i),'+;
*"2.91:1")'                                                                    &&63 i 2
DO ALTERC WITH getIndexOfIOutM("63"),6,'F1(inm(getIndexOfIInM("30"),i),"2.91:1")'                         &&63 i 2
*LSE
*O CIKL WITH getIndexOfIOutM("63"),'F1(F1(inm(getIndexOfIInM("41.1"),i),"2.50:1"),"2.91:1")'              &&63 i 2
*NDIF
*O ALTERC WITH getIndexOfIOutM("63"),1,'F1(F1((inm(getIndexOfIInM("41.1"),i)+inm(getIndexOfIInM("41.2"),i))/2,'+;
*'"2.50:1"),"2.91:1")'                                                         &&63 i 2

IF BL5
	DO CIKL WITH getIndexOfIOutM("64"),'(oum(getIndexOfIOutM("49"),i)*(oum(getIndexOfIOutM("57"),i)-oum(getIndexOfIOutM("58"),i))+oum(getIndexOfIOutM("55"),i)*'+;
	'(oum(getIndexOfIOutM("59"),i)-oum(getIndexOfIOutM("60"),i))+inm(getIndexOfIInM("25"),i)*.004*(oum(getIndexOfIOutM("61"),i)-oum(getIndexOfIOutM("58"),i)))/1000'&&64 Q к бр
ELSE
	DO CIKL WITH getIndexOfIOutM("64"),'(oum(getIndexOfIOutM("49"),i)*(oum(getIndexOfIOutM("57"),i)-oum(getIndexOfIOutM("58"),i))+oum(getIndexOfIOutM("55"),i)*'+;
	'(oum(getIndexOfIOutM("59"),i)-oum(getIndexOfIOutM("60"),i))+inm(getIndexOfIInM("27"),i)*(oum(getIndexOfIOutM("61"),i)-oum(getIndexOfIOutM("58"),i)))/1000'&&64 Q к бр
ENDIF
oum(getIndexOfIOutM("64"),n_blokov+1)=CIKL1(getIndexOfIOutM("64"),SUM('oum(getIndexOfIOutM("64"),'))                     &&64 Q к бр

&& уникальный пересчет доли газа только для параметра '/1' в реальн.врем.
IF BL1 AND RealTime
	inm(getIndexOfIInM("59"),ASCAN(ai,1))=MIN(100,inm(getIndexOfIInM("59"),ASCAN(ai,1))*inm(getIndexOfIInM("58"),n_blokov+1)/1E6/oum(getIndexOfIOutM("64"),ASCAN(ai,1))*100)
	inm(getIndexOfIInM("59"),ASCAN(ai,1))=IIF(inm(getIndexOfIInM("59"),ASCAN(ai,1))>=95,100,inm(getIndexOfIInM("59"),ASCAN(ai,1)))
SELECT 1 && inblok.dbf
LOCATE FOR ALLTRIM(SUBSTR(order,2))=='59'
REPLACE inblok.blok1 WITH STR(inm(getIndexOfIInM("59"),ASCAN(ai,1)),10,2)
SELECT 3 && ftabl.dbf
ENDIF
&& уникальный пересчет доли газа только для параметра '/1' в реальн.врем.

&& уникальный пересчет доли газа только для параметра '/2' в реальн.врем.
IF BL2 AND RealTime
inm(getIndexOfIInM("59"),ASCAN(ai,2))=MIN(100,inm(getIndexOfIInM("59"),ASCAN(ai,2))*inm(getIndexOfIInM("58"),n_blokov+1)/1E6/;
oum(getIndexOfIOutM("64"),ASCAN(ai,2))*100)
inm(getIndexOfIInM("59"),ASCAN(ai,2))=IIF(inm(getIndexOfIInM("59"),ASCAN(ai,2))>=95,100,inm(getIndexOfIInM("59"),ASCAN(ai,2)))
SELECT 1 && inblok.dbf
LOCATE FOR ALLTRIM(SUBSTR(order,2))=='59'
REPLACE inblok.blok2 WITH STR(inm(getIndexOfIInM("59"),ASCAN(ai,2)),10,2)
SELECT 3 && ftabl.dbf
ENDIF
&& уникальный пересчет доли газа только для параметра '/2' в реальн.врем.

DO CIKL WITH getIndexOfIOutM("65"),'oum(getIndexOfIOutM("64"),i)/oum(getIndexOfIOutM("1"),i)'                            &&65 Q к бр
oum(getIndexOfIOutM("65"),n_blokov+1)=CIKL1(getIndexOfIOutM("65"),oum(getIndexOfIOutM("64"),n_blokov+1)/oum(getIndexOfIOutM("1"),n_blokov+1))&&65 Q к бр

DO CIKL WITH getIndexOfIOutM("26"),'oum(getIndexOfIOutM("4"),i)*(oum(getIndexOfIOutM("57.1"),i)-oum(getIndexOfIOutM("60"),i))/.7/860'  &&26 Э тф п
DO CIKL WITH getIndexOfIOutM("27"),'IIF(oum(getIndexOfIOutM("4"),i)=0,0,F1(oum(getIndexOfIOutM("13"),i),"2.9в:1"))'      &&27 W п/тф
DO CIKL WITH getIndexOfIOutM("28"),'oum(getIndexOfIOutM("9"),i)-oum(getIndexOfIOutM("25"),i)*oum(getIndexOfIOutM("10"),i)/1E3-oum(getIndexOfIOutM("27"),i)*oum(getIndexOfIOutM("11"),i)/1E3'                                            &&28 N кн (ном)
sum=0
FOR i=1 TO n_blokov
  sum=sum+oum(getIndexOfIOutM("28"),i)*oum(getIndexOfIOutM("1"),i)
ENDFOR
oum(getIndexOfIOutM("28"),n_blokov+1)=CIKL1(getIndexOfIOutM("28"),sum/oum(getIndexOfIOutM("1"),n_blokov+1))              &&28 N кн (ном)
STORE 0 TO sum,sum1
FOR i=1 TO n_blokov
  sum=sum+oum(getIndexOfIOutM("14"),i)
  sum1=sum1+inm(getIndexOfIInM("28"),i)
ENDFOR
sum=IIF(BL1 OR BL2 OR BL3 OR BL4 OR BL5 OR BL6,sum*n_blokov1,sum)
sum1=sum1/n_blokov
o29_0=0
o29_1=2.3
o29_2=IIF(sum<240,2.3,IIF(BETW(sum,240,470),IIF(sum1<=F1(sum,"2.10:1"),2.3,4.6),4.6))
o29_3=IIF(sum<390,4,IIF(BETW(sum,390,870),IIF(sum1<=F1(sum,"2.10б:1"),4,6),6))
o29_4=IIF(sum<510,4,IIF(BETW(sum,510,1200),IIF(sum1<=F1(sum,"2.10в:1"),4,6),6))
o29_5=IIF(sum<740,IIF(sum1<=F1(sum,"2.10д:1"),4,6),6)
o29_6=IIF(sum<740,IIF(sum1<=F1(sum,"2.10д:1"),4,6),6)
o29_7=0
sum1=INT(IIF(BL1 OR BL2 OR BL3 OR BL4 OR BL5 OR BL6,n_blokov1,inm(getIndexOfIInM("89"),n_blokov+1)))
sum2=CEIL(IIF(BL1 OR BL2 OR BL3 OR BL4 OR BL5 OR BL6,n_blokov1,inm(getIndexOfIInM("89"),n_blokov+1)))
sum2=IIF(sum1=sum2,sum2+1,sum2)
o29_01=EVAL('o29_'+STR(sum1,1))
o29_02=EVAL('o29_'+STR(sum2,1))
oum(getIndexOfIOutM("29"),n_blokov+1)=CIKL1(getIndexOfIOutM("29"),o29_01*(sum2-;
IIF(BL1 OR BL2 OR BL3 OR BL4 OR BL5 OR BL6,n_blokov1,inm(getIndexOfIInM("89"),n_blokov+1)))+;
o29_02*(IIF(BL1 OR BL2 OR BL3 OR BL4 OR BL5 OR BL6,n_blokov1,inm(getIndexOfIInM("89"),n_blokov+1))-sum1)) &&29 N цн (н) гр
DO CIKL WITH getIndexOfIOutM("30"),'F1(oum(getIndexOfIOutM("13"),i),"2.95:1")/1E3'                         &&30 N кэн (н)
DO CIKL WITH getIndexOfIOutM("31"),'0.29'                                                    &&31 N бл(н)т
IF BL1 OR BL2 OR BL3 OR BL4 OR BL5 OR BL6
oum(getIndexOfIOutM("32"),n_blokov+1)=CIKL1(getIndexOfIOutM("32"),F1(SUM('oum(getIndexOfIOutM("14"),')*n_blokov1,"2.11:1"))&&32 N ст(н)т гр
ELSE
oum(getIndexOfIOutM("32"),n_blokov+1)=CIKL1(getIndexOfIOutM("32"),F1(SUM('oum(getIndexOfIOutM("14"),'),"2.11:1"))        &&32 N ст(н)т гр
ENDIF
DO CIKL WITH getIndexOfIOutM("35"),'5.19*inm(getIndexOfIInM("69"),i)'                                     &&35 Э т сн(пуск)
oum(getIndexOfIOutM("35"),n_blokov+1)=CIKL1(getIndexOfIOutM("35"),SUM('oum(getIndexOfIOutM("35"),'))                     &&35 Э т сн(пуск)
IF BL1 OR BL2 OR BL3 OR BL4 OR BL5 OR BL6
DO CIKL WITH getIndexOfIOutM("36"),'(1.03*(oum(getIndexOfIOutM("30"),i)*oum(getIndexOfIOutM("1"),i)+oum(getIndexOfIOutM("31"),i)*oum(getIndexOfIOutM("1"),i)+'+;
'(oum(getIndexOfIOutM("29"),n_blokov+1)+oum(getIndexOfIOutM("32"),n_blokov+1))*inm(getIndexOfIInM("70"),n_blokov+1)/'+;
'n_blokov1)+oum(getIndexOfIOutM("35"),i))/oum(getIndexOfIOutM("2"),i)*100'                                 &&36 Э т сн/(ном)
ELSE
DO CIKL WITH getIndexOfIOutM("36"),'(1.03*(oum(getIndexOfIOutM("30"),i)*oum(getIndexOfIOutM("1"),i)+oum(getIndexOfIOutM("31"),i)*oum(getIndexOfIOutM("1"),i)+'+;
'(oum(getIndexOfIOutM("29"),n_blokov+1)+oum(getIndexOfIOutM("32"),n_blokov+1))*inm(getIndexOfIInM("70"),n_blokov+1)*oum(getIndexOfIOutM("2"),i)/'+;
'oum(getIndexOfIOutM("2"),n_blokov+1))+oum(getIndexOfIOutM("35"),i))/oum(getIndexOfIOutM("2"),i)*100'                    &&36 Э т сн/(ном)
ENDIF
STORE 0 TO sum,sum1,sum2
FOR i=1 TO n_blokov
  sum=sum+oum(getIndexOfIOutM("30"),i)*oum(getIndexOfIOutM("1"),i)
  sum1=sum1+oum(getIndexOfIOutM("31"),i)*oum(getIndexOfIOutM("1"),i)
  sum2=sum2+oum(getIndexOfIOutM("35"),i)
ENDFOR
oum(getIndexOfIOutM("36"),n_blokov+1)=CIKL1(getIndexOfIOutM("36"),(1.03*(sum+sum1+(oum(getIndexOfIOutM("29"),n_blokov+1)+oum(getIndexOfIOutM("32"),n_blokov+1))*;
inm(getIndexOfIInM("70"),n_blokov+1))+sum2)/oum(getIndexOfIOutM("2"),n_blokov+1)*100)                     &&36 Э т сн/(ном)
DO CIKL WITH getIndexOfIOutM("37"),'oum(getIndexOfIOutM("29"),n_blokov+1)*inm(getIndexOfIInM("70"),n_blokov+1)*oum(getIndexOfIOutM("2"),i)/'+;
'oum(getIndexOfIOutM("2"),n_blokov+1)/oum(getIndexOfIOutM("28"),i)/oum(getIndexOfIOutM("1"),i)*100'                      &&37 Э цн (ном) гр
oum(getIndexOfIOutM("37"),n_blokov+1)=CIKL1(getIndexOfIOutM("37"),oum(getIndexOfIOutM("29"),n_blokov+1)*inm(getIndexOfIInM("70"),n_blokov+1)/;
oum(getIndexOfIOutM("28"),n_blokov+1)/oum(getIndexOfIOutM("1"),n_blokov+1)*100)                            &&37 Э цн (ном) гр
oum(getIndexOfIOutM("38"),n_blokov+1)=CIKL1(getIndexOfIOutM("38"),IIF(inm(getIndexOfIInM("43"),n_blokov+1)>10,0,;
F2(inm(getIndexOfIInM("43"),n_blokov+1),(inm(getIndexOfIInM("70"),n_blokov+1)*6-oum(getIndexOfIOutM("1"),n_blokov+1))/;
(6*inm(getIndexOfIInM("70"),n_blokov+1)),"2.13:2")))                                        &&38 Q т.о (отопл)
oum(getIndexOfIOutM("39"),n_blokov+1)=CIKL1(getIndexOfIOutM("39"),IIF(inm(getIndexOfIInM("43"),n_blokov+1)>10,0,;
F2(inm(getIndexOfIInM("43"),n_blokov+1),oum(getIndexOfIOutM("65"),n_blokov+1)/446.1,"2.13а:2")))          &&39 Q т.о (вент)
DO CIKL WITH getIndexOfIOutM("40"),'15.4*inm(getIndexOfIInM("69"),i)'                                     &&40 Q т сн (пуск)
oum(getIndexOfIOutM("40"),n_blokov+1)=CIKL1(getIndexOfIOutM("40"),SUM('oum(getIndexOfIOutM("40"),'))                     &&40 Q т сн (пуск)
IF BL1 OR BL2 OR BL3 OR BL4 OR BL5 OR BL6
DO CIKL WITH getIndexOfIOutM("41"),'(oum(getIndexOfIOutM("38"),n_blokov+1)+oum(getIndexOfIOutM("39"),n_blokov+1))*'+;
'oum(getIndexOfIOutM("1"),i)*1E5/n_blokov1/(oum(getIndexOfIOutM("24"),i)*oum(getIndexOfIOutM("2"),i))'                   &&41 q т сн (ном)
ELSE
DO CIKL WITH getIndexOfIOutM("41"),'((oum(getIndexOfIOutM("38"),n_blokov+1)+oum(getIndexOfIOutM("39"),n_blokov+1))*'+;
'inm(getIndexOfIInM("70"),n_blokov+1)*oum(getIndexOfIOutM("1"),i)/oum(getIndexOfIOutM("1"),n_blokov+1)+oum(getIndexOfIOutM("40"),i))*'+;
'1E5/(oum(getIndexOfIOutM("24"),i)*oum(getIndexOfIOutM("2"),i))'                                           &&41 q т сн (ном)
ENDIF
oum(getIndexOfIOutM("41"),n_blokov+1)=CIKL1(getIndexOfIOutM("41"),((oum(getIndexOfIOutM("38"),n_blokov+1)+oum(getIndexOfIOutM("39"),n_blokov+1))*;
inm(getIndexOfIInM("70"),n_blokov+1)+SUM('oum(getIndexOfIOutM("40"),'))*1E5/(oum(getIndexOfIOutM("24"),n_blokov+1)*;
oum(getIndexOfIOutM("2"),n_blokov+1)))                                                       &&41 q т сн (ном)
DO CIKL WITH getIndexOfIOutM("42"),'oum(getIndexOfIOutM("24"),i)*(100+oum(getIndexOfIOutM("41"),i))/(100-oum(getIndexOfIOutM("36"),i))'&&42 q т н (ном)
oum(getIndexOfIOutM("42"),n_blokov+1)=CIKL1(getIndexOfIOutM("42"),oum(getIndexOfIOutM("24"),n_blokov+1)*(100+oum(getIndexOfIOutM("41"),n_blokov+1))/;
(100-oum(getIndexOfIOutM("36"),n_blokov+1)))                                                 &&42 q т н (ном)
DO CIKL WITH getIndexOfIOutM("43"),'(oum(getIndexOfIOutM("60"),i)+(oum(getIndexOfIOutM("59.1"),i)-oum(getIndexOfIOutM("60"),i))-oum(getIndexOfIOutM("63"),i))/'+;
'(oum(getIndexOfIOutM("57.1"),i)+(oum(getIndexOfIOutM("59.1"),i)-oum(getIndexOfIOutM("60"),i))-oum(getIndexOfIOutM("63"),i))*(1+.4*(oum(getIndexOfIOutM("57.1"),i)-'+;
'oum(getIndexOfIOutM("60"),i))/(oum(getIndexOfIOutM("57.1"),i)+(oum(getIndexOfIOutM("59.1"),i)-oum(getIndexOfIOutM("60"),i))-oum(getIndexOfIOutM("63"),i)))'&&43 k по
DO CIKL WITH getIndexOfIOutM("44"),'IIF(inm(getIndexOfIInM("74"),i)=="1",0,((oum(getIndexOfIOutM("62"),i)-oum(getIndexOfIOutM("63"),i))/'+;
'(oum(getIndexOfIOutM("57.1"),i)+(oum(getIndexOfIOutM("59.1"),i)-oum(getIndexOfIOutM("60"),i))-oum(getIndexOfIOutM("63"),i)))*(1+.4*(oum(getIndexOfIOutM("57.1"),i)+'+;
'(oum(getIndexOfIOutM("59.1"),i)-oum(getIndexOfIOutM("60"),i))-oum(getIndexOfIOutM("62"),i))/(oum(getIndexOfIOutM("57.1"),i)+(oum(getIndexOfIOutM("59.1"),i)-'+;
'oum(getIndexOfIOutM("60"),i))-oum(getIndexOfIOutM("63"),i))))'                                            &&44 k то
DO CIKL WITH getIndexOfIOutM("45"),'IIF(oum(getIndexOfIOutM("3"),i)+oum(getIndexOfIOutM("4"),i)=0,0,(oum(getIndexOfIOutM("4"),i)*'+;
'(1-oum(getIndexOfIOutM("43"),i))*oum(getIndexOfIOutM("8"),i))/(oum(getIndexOfIOutM("3"),i)+oum(getIndexOfIOutM("4"),i)))'             &&45 dQ э по
oum(getIndexOfIOutM("45"),n_blokov+1)=CIKL1(getIndexOfIOutM("45"),SUM('oum(getIndexOfIOutM("45"),'))                     &&45 dQ э по
DO CIKL WITH getIndexOfIOutM("46"),'IIF(oum(getIndexOfIOutM("3"),i)+oum(getIndexOfIOutM("4"),i)=0,0,(oum(getIndexOfIOutM("3"),i)*'+;
'(1-oum(getIndexOfIOutM("44"),i))*oum(getIndexOfIOutM("8"),i))/(oum(getIndexOfIOutM("3"),i)+oum(getIndexOfIOutM("4"),i)))'             &&46 dQ э то
oum(getIndexOfIOutM("46"),n_blokov+1)=CIKL1(getIndexOfIOutM("46"),SUM('oum(getIndexOfIOutM("46"),'))                     &&46 dQ э то
DO CIKL WITH getIndexOfIOutM("47"),'(oum(getIndexOfIOutM("45"),i)+oum(getIndexOfIOutM("46"),i))'                         &&47 dQ э
oum(getIndexOfIOutM("47"),n_blokov+1)=CIKL1(getIndexOfIOutM("47"),SUM('oum(getIndexOfIOutM("47"),'))                     &&47 dQ э
DO CIKL WITH getIndexOfIOutM("48"),'(oum(getIndexOfIOutM("24"),i)*oum(getIndexOfIOutM("2"),i)*(100+oum(getIndexOfIOutM("41"),i))/1E5+oum(getIndexOfIOutM("47"),i))/'+;
'(oum(getIndexOfIOutM("24"),i)*oum(getIndexOfIOutM("2"),i)*(100+oum(getIndexOfIOutM("41"),i))/1E5)'                      &&48 k отр (т)
oum(getIndexOfIOutM("48"),n_blokov+1)=CIKL1(getIndexOfIOutM("48"),(oum(getIndexOfIOutM("24"),n_blokov+1)*oum(getIndexOfIOutM("2"),n_blokov+1)*;
(100+oum(getIndexOfIOutM("41"),n_blokov+1))/1E5+SUM('oum(getIndexOfIOutM("47"),'))/(oum(getIndexOfIOutM("24"),n_blokov+1)*;
oum(getIndexOfIOutM("2"),n_blokov+1)*(100+oum(getIndexOfIOutM("41"),n_blokov+1))/1E5))                     &&48 k отр (т)
IF BL5
DO CIKL WITH getIndexOfIOutM("66"),'oum(getIndexOfIOutM("49"),i)+inm(getIndexOfIInM("25"),i)*.004'                      &&66 D пв
ELSE
DO CIKL WITH getIndexOfIOutM("66"),'oum(getIndexOfIOutM("49"),i)+inm(getIndexOfIInM("27"),i)'                           &&66 D пв
ENDIF
oum(getIndexOfIOutM("66"),n_blokov+1)=CIKL1(getIndexOfIOutM("66"),SUM('oum(getIndexOfIOutM("66"),'))                     &&66 D пв
DO CIKL WITH getIndexOfIOutM("66.1"),'oum(getIndexOfIOutM("66"),i)/oum(getIndexOfIOutM("1"),i)'                          &&66.1 D пв ср
oum(getIndexOfIOutM("66.1"),n_blokov+1)=CIKL1(getIndexOfIOutM("66.1"),SUM('oum(getIndexOfIOutM("66.1"),'))               &&66.1 D пв ср
DO CIKL WITH getIndexOfIOutM("67"),'F2(oum(getIndexOfIOutM("65"),i),inm(getIndexOfIInM("59"),i),"2.65:2")'              &&67 alfa вэк (н)
DO ALTERC WITH getIndexOfIOutM("67"),3,'F1(oum(getIndexOfIOutM("65"),i),"2.65в:1")'                        &&67 alfa вэк (н)
DO ALTERC WITH getIndexOfIOutM("67"),4,'F1(oum(getIndexOfIOutM("65"),i),"2.65в:1")'                        &&67 alfa вэк (н)
*F BL5
*LSE
DO ALTERC WITH getIndexOfIOutM("67"),5,'F1(oum(getIndexOfIOutM("65"),i),"2.63:1")'                         &&67 alfa вэк (н)
DO ALTERC WITH getIndexOfIOutM("67"),6,'F1(oum(getIndexOfIOutM("65"),i),"2.63:1(6)")'                      &&67 alfa вэк (н)
*NDIF
DO CIKL WITH getIndexOfIOutM("68"),'F2(oum(getIndexOfIOutM("65"),i),inm(getIndexOfIInM("59"),i),"2.82:2")'              &&68 dalfa ух (н)
*O ALTERC WITH getIndexOfIOutM("68"),3,'F1(oum(getIndexOfIOutM("65"),i),"2.82а:1")'                        &&68 dalfa ух (н)
*O ALTERC WITH getIndexOfIOutM("68"),4,'F1(oum(getIndexOfIOutM("65"),i),"2.82а:1")'                        &&68 dalfa ух (н)
*F BL5
*LSE
DO ALTERC WITH getIndexOfIOutM("68"),5,'F1(oum(getIndexOfIOutM("65"),i),"2.80:1")'                         &&68 dalfa ух (н)
DO ALTERC WITH getIndexOfIOutM("68"),6,'F1(oum(getIndexOfIOutM("65"),i),"2.80:1(6)")'                      &&68 dalfa ух (н)
*NDIF
DO CIKL WITH getIndexOfIOutM("69"),'oum(getIndexOfIOutM("67"),i)+oum(getIndexOfIOutM("68"),i)'                           &&69 alfa ух (н)
DO CIKL WITH getIndexOfIOutM("70"),'F2(oum(getIndexOfIOutM("65"),i),inm(getIndexOfIInM("59"),i),"2.96:2")'              &&70 q 4 исх
*O ALTERC WITH getIndexOfIOutM("70"),3,'F1(oum(getIndexOfIOutM("65"),i),"2.96а:1")'                        &&70 q 4 исх
*O ALTERC WITH getIndexOfIOutM("70"),4,'F1(oum(getIndexOfIOutM("65"),i),"2.96а:1")'                        &&70 q 4 исх
*F BL5
DO ALTERC WITH getIndexOfIOutM("70"),5,'0.912'                                               &&70 q 4 исх
DO ALTERC WITH getIndexOfIOutM("70"),6,'F1(oum(getIndexOfIOutM("65"),i),"2.96:1(6)")'                      &&70 q 4 исх
*LSE
*O ALTERC WITH getIndexOfIOutM("70"),5,'F1(oum(getIndexOfIOutM("65"),i),"2.97:1")'                         &&70 q 4 исх
*NDIF
DO CIKL WITH getIndexOfIOutM("71"),;
'.064*(inm(getIndexOfIInM("55"),n_blokov+1)-14.3)*(100-inm(getIndexOfIInM("59"),i))/100'                 &&71 dq 4 (Ар)
*DO CIKL WITH getIndexOfIOutM("71"),'IIF(inm(getIndexOfIInM("59"),i)<=50,'+;
*'(.063*(inm(getIndexOfIInM("55"),n_blokov+1)-14.2)*(50-inm(getIndexOfIInM("59"),i))+.022*(inm(getIndexOfIInM("55"),n_blokov+1)-15)*inm(getIndexOfIInM("59"),i))/50,'+;
*'IIF(inm(getIndexOfIInM("59"),i)>50,.022*(inm(getIndexOfIInM("55"),n_blokov+1)-15)*(100-inm(getIndexOfIInM("59"),i))/50,1/0))'&&71 dq 4 (Ар)
DO ALTERC WITH getIndexOfIOutM("71"),5,'.075*(inm(getIndexOfIInM("55"),n_blokov+1)-14.3)'                 &&71 dq 4 (Ар)
DO ALTERC WITH getIndexOfIOutM("71"),6,'.085*(inm(getIndexOfIInM("55"),n_blokov+1)-14.3)'                 &&71 dq 4 (Ар)
DO CIKL WITH getIndexOfIOutM("72"),;
'.01*(inm(getIndexOfIInM("54"),n_blokov+1)-13.4)*(100-inm(getIndexOfIInM("59"),i))/100'                  &&72 dq 4 (Wp)
*O CIKL WITH getIndexOfIOutM("72"),'IIF(inm(getIndexOfIInM("59"),i)<=50,'+;
*(.009*(inm(getIndexOfIInM("54"),n_blokov+1)-12.6)*(50-inm(getIndexOfIInM("59"),i))+.003*(inm(getIndexOfIInM("54"),n_blokov+1)-9.1)*inm(getIndexOfIInM("59"),i))/50,'+;
*IIF(inm(getIndexOfIInM("59"),i)>50,.003*(inm(getIndexOfIInM("54"),n_blokov+1)-9.1)*(100-inm(getIndexOfIInM("59"),i))/50,1/0))'&&72 dq 4 (Wp)
DO ALTERC WITH getIndexOfIOutM("72"),5,'.012*(inm(getIndexOfIInM("54"),n_blokov+1)-13.4)'                 &&72 dq 4 (Wp)
DO ALTERC WITH getIndexOfIOutM("72"),6,'.013*(inm(getIndexOfIInM("54"),n_blokov+1)-13.4)'                 &&72 dq 4 (Wp)
DO CIKL WITH getIndexOfIOutM("73"),'oum(getIndexOfIOutM("70"),i)+oum(getIndexOfIOutM("71"),i)+oum(getIndexOfIOutM("72"),i)'            &&73 q 4 (н)
sum=0
FOR i=1 TO n_blokov
  sum=sum+oum(getIndexOfIOutM("73"),i)*oum(getIndexOfIOutM("64"),i)
ENDFOR
oum(getIndexOfIOutM("73"),n_blokov+1)=CIKL1(getIndexOfIOutM("73"),sum/oum(getIndexOfIOutM("64"),n_blokov+1))             &&73 q 4 (н)
DO CIKL WITH getIndexOfIOutM("74"),'IIF(inm(getIndexOfIInM("74"),i)=="1",F1(oum(getIndexOfIOutM("50"),i),"2.20:1"),'+;
'IIF(inm(getIndexOfIInM("74"),i)=="2" OR inm(getIndexOfIInM("74"),i)=="2а" OR inm(getIndexOfIInM("74"),i)=="3",F1(oum(getIndexOfIOutM("50"),i),"2.20а:1"),'+;
'1/0))'                                                                        &&74 t пв (н)
sum=0
FOR i=1 TO n_blokov
  sum=sum+oum(getIndexOfIOutM("74"),i)*oum(getIndexOfIOutM("66"),i)
ENDFOR
oum(getIndexOfIOutM("74"),n_blokov+1)=CIKL1(getIndexOfIOutM("74"),sum/oum(getIndexOfIOutM("66"),n_blokov+1))             &&74 t пв (н)
DO ALTERC WITH getIndexOfIOutM("75"),1,'F2(oum(getIndexOfIOutM("65"),i),inm(getIndexOfIInM("59"),i),"2.23:2")'          &&75 t ух исх
DO ALTERC WITH getIndexOfIOutM("75"),2,'F2(oum(getIndexOfIOutM("65"),i),inm(getIndexOfIInM("59"),i),"2.23:2")'          &&75 t ух исх
DO ALTERC WITH getIndexOfIOutM("75"),3,'F1(oum(getIndexOfIOutM("65"),i),"2.23а:1")'                        &&75 t ух исх
DO ALTERC WITH getIndexOfIOutM("75"),4,'F1(oum(getIndexOfIOutM("65"),i),"2.23а:1")'                        &&75 t ух исх
DO ALTERC WITH getIndexOfIOutM("75"),5,'F1(oum(getIndexOfIOutM("65"),i),"2.21:1")'                         &&75 t ух исх
DO ALTERC WITH getIndexOfIOutM("75"),6,'F1(oum(getIndexOfIOutM("65"),i),"2.21:1(6)")'                      &&75 t ух исх
DO CIKL WITH getIndexOfIOutM("76"),'.2*(inm(getIndexOfIInM("26"),i)-oum(getIndexOfIOutM("74"),i))'                      &&76 dt ух (t пв)
DO CIKL WITH getIndexOfIOutM("77"),'.50*((inm(getIndexOfIInM("32"),i)+inm(getIndexOfIInM("32.1"),i))/2-30)'            &&77 dt ух (t вп)
DO CIKL WITH getIndexOfIOutM("78"),'-.3*((inm(getIndexOfIInM("32"),i)+inm(getIndexOfIInM("32.1"),i))/2-'+;
'(inm(getIndexOfIInM("31"),i)+inm(getIndexOfIInM("31.1"),i))/2)'                                         &&78 dt ух (t рец)
DO CIKL WITH getIndexOfIOutM("79"),'oum(getIndexOfIOutM("75"),i)+oum(getIndexOfIOutM("76"),i)+oum(getIndexOfIOutM("77"),i)+oum(getIndexOfIOutM("78"),i)'&&79 t ух (н)
sum=0
FOR i=1 TO n_blokov
  sum=sum+oum(getIndexOfIOutM("79"),i)*oum(getIndexOfIOutM("64"),i)
ENDFOR
oum(getIndexOfIOutM("79"),n_blokov+1)=CIKL1(getIndexOfIOutM("79"),sum/oum(getIndexOfIOutM("64"),n_blokov+1))             &&79 t ух (н)
DO CIKL WITH getIndexOfIOutM("80"),'(3.5+.02*1E3*inm(getIndexOfIInM("54"),n_blokov+1)/inm(getIndexOfIInM("53"),n_blokov+1))*'+;
'(100-inm(getIndexOfIInM("59"),i))/1E2+3.53*inm(getIndexOfIInM("59"),i)/1E2'                             &&80 k
DO CIKL WITH getIndexOfIOutM("81"),'(.4+.04*1E3*inm(getIndexOfIInM("54"),n_blokov+1)/inm(getIndexOfIInM("53"),n_blokov+1))*'+;
'(100-inm(getIndexOfIInM("59"),i))/1E2+.6*inm(getIndexOfIInM("59"),i)/1E2'                               &&81 c
DO CIKL WITH getIndexOfIOutM("82"),'.14*(100-inm(getIndexOfIInM("59"),i))/1E2+.18*inm(getIndexOfIInM("59"),i)/1E2'     &&82 b
DO CIKL WITH getIndexOfIOutM("83"),'(oum(getIndexOfIOutM("80"),i)*oum(getIndexOfIOutM("69"),i)+oum(getIndexOfIOutM("81"),i))*'+;
'(oum(getIndexOfIOutM("79"),i)-oum(getIndexOfIOutM("69"),i)*(inm(getIndexOfIInM("31"),i)+inm(getIndexOfIInM("31.1"),i))/2/'+;
'(oum(getIndexOfIOutM("69"),i)+oum(getIndexOfIOutM("82"),i)))*(.9805+.00013*oum(getIndexOfIOutM("79"),i))*(1-.01*oum(getIndexOfIOutM("73"),i))/1E2+'+;
'.2*.95*inm(getIndexOfIInM("55"),n_blokov+1)*(100-inm(getIndexOfIInM("59"),i))/1E2*oum(getIndexOfIOutM("79"),i)/inm(getIndexOfIInM("53"),n_blokov+1)'&&83 q 2 (н)
sum=0
FOR i=1 TO n_blokov
  sum=sum+oum(getIndexOfIOutM("83"),i)*oum(getIndexOfIOutM("64"),i)
ENDFOR
oum(getIndexOfIOutM("83"),n_blokov+1)=CIKL1(getIndexOfIOutM("83"),sum/oum(getIndexOfIOutM("64"),n_blokov+1))             &&83 q 2 (н)
DO CIKL WITH getIndexOfIOutM("84"),'F2(oum(getIndexOfIOutM("65"),i),inm(getIndexOfIInM("59"),i),"2.98:2")'              &&84 q 5 (н)
DO ALTERC WITH getIndexOfIOutM("84"),5,'F1(oum(getIndexOfIOutM("65"),i),"2.99:1")'                         &&84 q 5 (н)
DO ALTERC WITH getIndexOfIOutM("84"),6,'F1(oum(getIndexOfIOutM("65"),i),"2.99:1(6)")'                      &&84 q 5 (н)
DO CIKL WITH getIndexOfIOutM("85"),'(100-inm(getIndexOfIInM("59"),i))*0.02/100'                           &&85 q 6 (н)
DO ALTERC WITH getIndexOfIOutM("86"),1,'IIF(inm(getIndexOfIInM("72"),i)>35000,.0055*(inm(getIndexOfIInM("72"),i)-35000)/'+;
'1E3,0)'                                                                       &&86 q рес (н)
DO ALTERC WITH getIndexOfIOutM("86"),2,'IIF(inm(getIndexOfIInM("72"),i)>35000,.0055*(inm(getIndexOfIInM("72"),i)-35000)/'+;
'1E3,0)'                                                                       &&86 q рес (н)
DO ALTERC WITH getIndexOfIOutM("86"),3,'IIF(inm(getIndexOfIInM("72"),i)>35000,.0055*(inm(getIndexOfIInM("72"),i)-35000)/'+;
'1E3,0)'                                                                       &&86 q рес (н)
DO ALTERC WITH getIndexOfIOutM("86"),4,'IIF(inm(getIndexOfIInM("72"),i)>35000,.0055*(inm(getIndexOfIInM("72"),i)-35000)/'+;
'1E3,0)'                                                                       &&86 q рес (н)
DO ALTERC WITH getIndexOfIOutM("86"),5,'IIF(inm(getIndexOfIInM("72"),i)>35000,.0055*(inm(getIndexOfIInM("72"),i)-35000)/'+;
'1E3,0)'                                                                       &&86 q рес (н)
DO ALTERC WITH getIndexOfIOutM("86"),6,'IIF(inm(getIndexOfIInM("72"),i)>35000,.0055*(inm(getIndexOfIInM("72"),i)-35000)/'+;
'1E3,0)'                                                                       &&86 q рес (н)
DO CIKL WITH getIndexOfIOutM("87"),'inm(getIndexOfIInM("69"),i)*64.2*7*1E2/(oum(getIndexOfIOutM("64"),i)*1E2/(100-oum(getIndexOfIOutM("73"),i)-'+;
'oum(getIndexOfIOutM("83"),i)-oum(getIndexOfIOutM("84"),i)-oum(getIndexOfIOutM("85"),i)-oum(getIndexOfIOutM("86"),i))+64.2*7)'         &&87 q пуск (н)
DO CIKL WITH getIndexOfIOutM("88"),'100-oum(getIndexOfIOutM("83"),i)-oum(getIndexOfIOutM("73"),i)-oum(getIndexOfIOutM("84"),i)-oum(getIndexOfIOutM("85"),i)-'+;
'oum(getIndexOfIOutM("86"),i)-oum(getIndexOfIOutM("87"),i)'                                                &&88 КПД к бр (ном)
sum=0
FOR i=1 TO n_blokov
  sum=sum+oum(getIndexOfIOutM("88"),i)*oum(getIndexOfIOutM("64"),i)
ENDFOR
oum(getIndexOfIOutM("88"),n_blokov+1)=CIKL1(getIndexOfIOutM("88"),sum/oum(getIndexOfIOutM("64"),n_blokov+1))             &&88 КПД к бр (ном)
DO CIKL WITH getIndexOfIOutM("89"),'100-inm(getIndexOfIInM("59"),i)'                                      &&89 alfa уг
DO CIKL WITH getIndexOfIOutM("90"),'oum(getIndexOfIOutM("64"),i)*oum(getIndexOfIOutM("89"),i)*1E3/oum(getIndexOfIOutM("88"),i)/'+;
'inm(getIndexOfIInM("53"),n_blokov+1)'                                                      &&90 B нат(ном)
oum(getIndexOfIOutM("90"),n_blokov+1)=CIKL1(getIndexOfIOutM("90"),SUM('oum(getIndexOfIOutM("90"),'))                     &&90 B нат(ном)
DO CIKL WITH getIndexOfIOutM("91"),'F2(oum(getIndexOfIOutM("65"),i),inm(getIndexOfIInM("59"),i),"2.27:2")'              &&91 Э тд(н) исх
DO ALTERC WITH getIndexOfIOutM("91"),3,'F1(oum(getIndexOfIOutM("65"),i),"2.27в:1")'                         &&91 Э тд(н) исх
DO ALTERC WITH getIndexOfIOutM("91"),4,'F1(oum(getIndexOfIOutM("65"),i),"2.27в:1")'                         &&91 Э тд(н) исх
*F BL5
*LSE
DO ALTERC WITH getIndexOfIOutM("91"),5,'F1(oum(getIndexOfIOutM("65"),i),"2.25:1")'                         &&91 Э тд(н) исх
DO ALTERC WITH getIndexOfIOutM("91"),6,'F1(oum(getIndexOfIOutM("65"),i),"2.25:1(6)")'                      &&91 Э тд(н) исх
*NDIF
DO CIKL WITH getIndexOfIOutM("92"),;
'.041*(inm(getIndexOfIInM("54"),n_blokov+1)-13.4)*(100-inm(getIndexOfIInM("59"),i))/100'                 &&92 dЭ тд (Wp)
DO ALTERC WITH getIndexOfIOutM("92"),5,'.04*(inm(getIndexOfIInM("54"),n_blokov+1)-13.4)'                  &&92 dЭ тд (Wp)
DO ALTERC WITH getIndexOfIOutM("92"),6,'.04*(inm(getIndexOfIInM("54"),n_blokov+1)-13.4)'                  &&92 dЭ тд (Wp)
DO CIKL WITH getIndexOfIOutM("93"),;
'.004*((inm(getIndexOfIInM("32"),i)+inm(getIndexOfIInM("32.1"),i))/2-30)*(100-inm(getIndexOfIInM("59"),i))/100'                 &&93 dЭ тд (t вп)
DO CIKL WITH getIndexOfIOutM("94"),'1/0'                                                     &&94 dЭ тд (t рец)
*DO CIKL WITH getIndexOfIOutM("94"),'.008*((inm(getIndexOfIInM("32"),i)+inm(getIndexOfIInM("32.1"),i))/2-'+;
*'(inm(getIndexOfIInM("31"),i)+inm(getIndexOfIInM("31.1"),i))/2)'                                        &&94 dЭ тд (t рец)
DO CIKL WITH getIndexOfIOutM("95"),'oum(getIndexOfIOutM("91"),i)+oum(getIndexOfIOutM("92"),i)'                           &&95 Э тд (ном)
sum=0
FOR i=1 TO n_blokov
  sum=sum+oum(getIndexOfIOutM("95"),i)*oum(getIndexOfIOutM("64"),i)
ENDFOR
oum(getIndexOfIOutM("95"),n_blokov+1)=CIKL1(getIndexOfIOutM("95"),sum/oum(getIndexOfIOutM("64"),n_blokov+1))             &&95 Э тд (ном)
DO CIKL WITH getIndexOfIOutM("96"),'IIF(inm(getIndexOfIInM("59"),i)=100,0,F2(oum(getIndexOfIOutM("65"),i),inm(getIndexOfIInM("59"),i),"2.26:2"))'&&96 Э пп (исх)
DO ALTERC WITH getIndexOfIOutM("96"),3,'F2(oum(getIndexOfIOutM("65"),i),0,"2.26:2")'                       &&96 Э пп (исх)
DO ALTERC WITH getIndexOfIOutM("96"),4,'F2(oum(getIndexOfIOutM("65"),i),0,"2.26:2")'                       &&96 Э пп (исх)
*O ALTERC WITH getIndexOfIOutM("96"),5,'F2(oum(getIndexOfIOutM("65"),i),0,"2.26:2")'                       &&96 Э пп (исх)
*F BL5
*O ALTERC WITH getIndexOfIOutM("96"),5,'F2(oum(getIndexOfIOutM("65"),i),0,"2.26:2")'                       &&96 Э пп (исх)
*LSE
DO ALTERC WITH getIndexOfIOutM("96"),5,'F1(oum(getIndexOfIOutM("65"),i),"2.26а:1")'                        &&96 Э пп (исх)
DO ALTERC WITH getIndexOfIOutM("96"),6,'F1(oum(getIndexOfIOutM("65"),i),"2.26:1(6)")'                      &&96 Э пп (исх)
*NDIF
DO CIKL WITH getIndexOfIOutM("97"),;
'.297*(inm(getIndexOfIInM("54"),n_blokov+1)-13.4)*(100-inm(getIndexOfIInM("59"),i))/100'                  &&97 dЭ пп (исх)
DO ALTERC WITH getIndexOfIOutM("97"),5,'.297*(inm(getIndexOfIInM("54"),n_blokov+1)-13.4)'                 &&97 dЭ пп (исх)
DO ALTERC WITH getIndexOfIOutM("97"),6,'.297*(inm(getIndexOfIInM("54"),n_blokov+1)-13.4)'                 &&97 dЭ пп (исх)
DO CIKL WITH getIndexOfIOutM("98"),'oum(getIndexOfIOutM("96"),i)+oum(getIndexOfIOutM("97"),i)'                           &&98 Э пп (н)
sum=0
FOR i=1 TO n_blokov
  sum=sum+oum(getIndexOfIOutM("98"),i)*oum(getIndexOfIOutM("90"),i)
ENDFOR
oum(getIndexOfIOutM("98"),n_blokov+1)=CIKL1(getIndexOfIOutM("98"),sum/oum(getIndexOfIOutM("90"),n_blokov+1))             &&98 Э пп (н)
DO CIKL WITH getIndexOfIOutM("99"),'F1(oum(getIndexOfIOutM("66.1"),i),"2.29:1")'                       &&99 Э пэн (н)
*O ALTERC WITH getIndexOfIOutM("99"),1,'(F1(oum(getIndexOfIOutM("66.1"),i),"2.29:1")+F1(oum(getIndexOfIOutM("66.1"),i),"2.30:1"))/'+;
*2*1.1'                                                                        &&99 Э пэн (н)
*O ALTERC WITH getIndexOfIOutM("99"),2,'(F1(oum(getIndexOfIOutM("66.1"),i),"2.29:1")+F1(oum(getIndexOfIOutM("66.1"),i),"2.30:1"))/'+;
*2*1.1'                                                                        &&99 Э пэн (н)
sum=0
FOR i=1 TO n_blokov
  sum=sum+oum(getIndexOfIOutM("99"),i)*oum(getIndexOfIOutM("66"),i)
ENDFOR
oum(getIndexOfIOutM("99"),n_blokov+1)=CIKL1(getIndexOfIOutM("99"),sum/oum(getIndexOfIOutM("66"),n_blokov+1))             &&99 Э пэн (н)
DO CIKL WITH getIndexOfIOutM("100"),'IIF(inm(getIndexOfIInM("59"),i)=100,0,IIF(inm(getIndexOfIInM("43"),n_blokov+1)>=0,F1(inm(getIndexOfIInM("43"),n_blokov+1),"2.31:1")'+;
',F1(inm(getIndexOfIInM("43"),n_blokov+1),"2.31а:1")))'                                     &&100 Э тп (н)
oum(getIndexOfIOutM("101"),n_blokov+1)=CIKL1(getIndexOfIOutM("101"),IIF(inm(getIndexOfIInM("43"),n_blokov+1)>=0,F1(inm(getIndexOfIInM("43"),n_blokov+1),"2.32:1"),;
F1(inm(getIndexOfIInM("43"),n_blokov+1),"2.32а:1")))                                        &&101 Э разг (н)
IF BL1 OR BL2 OR BL3 OR BL4 OR BL5 OR BL6
oum(getIndexOfIOutM("102"),n_blokov+1)=CIKL1(getIndexOfIOutM("102"),F1(n_blokov1,"2.33:1"))               &&102 N зшу (н)
ELSE
oum(getIndexOfIOutM("102"),n_blokov+1)=CIKL1(getIndexOfIOutM("102"),F1(inm(getIndexOfIInM("89"),n_blokov+1),;
"2.33:1"))                                                                     &&102 N зшу (н)
ENDIF
oum(getIndexOfIOutM("103"),n_blokov+1)=CIKL1(getIndexOfIOutM("103"),F1((inm(getIndexOfIInM("78"),n_blokov+1)+inm(getIndexOfIInM("79"),n_blokov+1))/;
inm(getIndexOfIInM("70"),n_blokov+1),"2.34:1"))                                             &&103 N ов (н)
oum(getIndexOfIOutM("104"),n_blokov+1)=CIKL1(getIndexOfIOutM("104"),IIF(inm(getIndexOfIInM("92"),n_blokov+1)<=0,208,F1(inm(getIndexOfIInM("92"),n_blokov+1),;
"2.34а:1")))                                                                   &&104 N маз (н)
IF BL1 OR BL2 OR BL3 OR BL4 OR BL5 OR BL6
oum(getIndexOfIOutM("105"),n_blokov+1)=CIKL1(getIndexOfIOutM("105"),F1(SUM('oum(getIndexOfIOutM("65"),')*n_blokov1,"2.51:1"))&&105 N доп.пр (н)
ELSE
oum(getIndexOfIOutM("105"),n_blokov+1)=CIKL1(getIndexOfIOutM("105"),F1(oum(getIndexOfIOutM("64"),n_blokov+1)/inm(getIndexOfIInM("70"),n_blokov+1),;
"2.51:1"))                                                                     &&105 N доп.пр (н)
ENDIF
IF BL1 OR BL2 OR BL3 OR BL4 OR BL5 OR BL6
oum(getIndexOfIOutM("106"),n_blokov+1)=CIKL1(getIndexOfIOutM("106"),(oum(getIndexOfIOutM("102"),n_blokov+1)+oum(getIndexOfIOutM("103"),n_blokov+1)+;
oum(getIndexOfIOutM("104"),n_blokov+1)+oum(getIndexOfIOutM("105"),n_blokov+1))*inm(getIndexOfIInM("70"),n_blokov+1)/1E3/n_blokov1+;
oum(getIndexOfIOutM("101"),n_blokov+1)*inm(getIndexOfIInM("88"),n_blokov+1)/1E3)                          &&106 N ов (н)
ELSE
oum(getIndexOfIOutM("106"),n_blokov+1)=CIKL1(getIndexOfIOutM("106"),(oum(getIndexOfIOutM("102"),n_blokov+1)+oum(getIndexOfIOutM("103"),n_blokov+1)+;
oum(getIndexOfIOutM("104"),n_blokov+1)+oum(getIndexOfIOutM("105"),n_blokov+1))*inm(getIndexOfIInM("70"),n_blokov+1)/1E3+;
oum(getIndexOfIOutM("101"),n_blokov+1)*inm(getIndexOfIInM("88"),n_blokov+1)/1E3)                          &&106 N ов (н)
ENDIF
DO CIKL WITH getIndexOfIOutM("107"),'6.19*inm(getIndexOfIInM("69"),i)'                                    &&107 Э пуск (н)
oum(getIndexOfIOutM("107"),n_blokov+1)=CIKL1(getIndexOfIOutM("107"),SUM('oum(getIndexOfIOutM("107"),'))                  &&107 Э пуск (н)
DO CIKL WITH getIndexOfIOutM("108"),'1.03*(oum(getIndexOfIOutM("95"),i)*oum(getIndexOfIOutM("64"),i)+oum(getIndexOfIOutM("98"),i)*oum(getIndexOfIOutM("90"),i)+'+;
'oum(getIndexOfIOutM("99"),i)*oum(getIndexOfIOutM("66"),i)+oum(getIndexOfIOutM("100"),i)*oum(getIndexOfIOutM("90"),i))/1E3+1.03*'+;
'oum(getIndexOfIOutM("106"),n_blokov+1)*oum(getIndexOfIOutM("64"),i)/oum(getIndexOfIOutM("64"),n_blokov+1)+oum(getIndexOfIOutM("107"),i)'&&108 Э к сн (н)
oum(getIndexOfIOutM("108"),n_blokov+1)=CIKL1(getIndexOfIOutM("108"),SUM('oum(getIndexOfIOutM("108"),'))                  &&108 Э к сн (н)
oum(getIndexOfIOutM("109"),n_blokov+1)=CIKL1(getIndexOfIOutM("109"),IIF(inm(getIndexOfIInM("43"),n_blokov+1)>10,0,;
F2(inm(getIndexOfIInM("43"),n_blokov+1),(6*inm(getIndexOfIInM("70"),n_blokov+1)-;
oum(getIndexOfIOutM("1"),n_blokov+1))/(6*inm(getIndexOfIInM("70"),n_blokov+1)),"2.93:2")))                &&109 Q от к (н)
oum(getIndexOfIOutM("110"),n_blokov+1)=CIKL1(getIndexOfIOutM("110"),IIF(inm(getIndexOfIInM("43"),n_blokov+1)>10,0,;
F2(inm(getIndexOfIInM("43"),n_blokov+1),oum(getIndexOfIOutM("65"),n_blokov+1)/446.1,"2.94:2")))             &&110 Q от к (н)
oum(getIndexOfIOutM("111"),n_blokov+1)=CIKL1(getIndexOfIOutM("111"),IIF(inm(getIndexOfIInM("43"),n_blokov+1)>10,0,;
F1(inm(getIndexOfIInM("43"),n_blokov+1),"2.12:1")))                                         &&111 Q от IIк (н)
oum(getIndexOfIOutM("112"),n_blokov+1)=CIKL1(getIndexOfIOutM("112"),IIF(inm(getIndexOfIInM("43"),n_blokov+1)>10,0,;
F1(inm(getIndexOfIInM("43"),n_blokov+1),"2.14:1")))                                         &&112 Q пвк (н)
oum(getIndexOfIOutM("113"),n_blokov+1)=CIKL1(getIndexOfIOutM("113"),;
F2((inm(getIndexOfIInM("78"),n_blokov+1)+inm(getIndexOfIInM("79"),n_blokov+1))/inm(getIndexOfIInM("70"),n_blokov+1),inm(getIndexOfIInM("91"),n_blokov+1),"2.15:2"))  &&113 Q об.в (н)
oum(getIndexOfIOutM("114"),n_blokov+1)=CIKL1(getIndexOfIOutM("114"),IIF(inm(getIndexOfIInM("43"),n_blokov+1)>=0,0,;
F1(inm(getIndexOfIInM("43"),n_blokov+1),"2.16:1")))                                         &&114 Q разм (н)
oum(getIndexOfIOutM("115"),n_blokov+1)=CIKL1(getIndexOfIOutM("115"),F1(inm(getIndexOfIInM("43"),n_blokov+1),"2.28:1"))  &&115 Q мх (н)
oum(getIndexOfIOutM("116"),n_blokov+1)=CIKL1(getIndexOfIOutM("116"),IIF(inm(getIndexOfIInM("61"),n_blokov+1)=0,0,;
F1(inm(getIndexOfIInM("43"),n_blokov+1),"2.52:1")))                                         &&116 Q маз сл (н)
*oum(getIndexOfIOutM("117"),n_blokov+1)=CIKL1(getIndexOfIOutM("117"),IIF(inm(getIndexOfIInM("43"),n_blokov+1)>0,0,;
F2(inm(getIndexOfIInM("62"),n_blokov+1),inm(getIndexOfIInM("43"),n_blokov+1),"2.56:2")))                 &&117 Q пр.сл (н)
oum(getIndexOfIOutM("117"),n_blokov+1)=CIKL1(getIndexOfIOutM("117"),IIF(.T.,0,;
F2(inm(getIndexOfIInM("62"),n_blokov+1),inm(getIndexOfIInM("43"),n_blokov+1),"2.56:2")))                 &&117 Q пр.сл (н)
oum(getIndexOfIOutM("118"),n_blokov+1)=CIKL1(getIndexOfIOutM("118"),F1(SUM('oum(getIndexOfIOutM("65"),'),"2.60:1"))      &&118 Q пр (н)
DO CIKL WITH getIndexOfIOutM("119"),'15.4*inm(getIndexOfIInM("69"),i)'                                    &&119 Q пуск (н)
oum(getIndexOfIOutM("119"),n_blokov+1)=CIKL1(getIndexOfIOutM("119"),SUM('oum(getIndexOfIOutM("119"),'))                  &&119 Q пуск (н)
oum(getIndexOfIOutM("120"),n_blokov+1)=CIKL1(getIndexOfIOutM("120"),(oum(getIndexOfIOutM("109"),n_blokov+1)+oum(getIndexOfIOutM("110"),n_blokov+1)+;
oum(getIndexOfIOutM("111"),n_blokov+1)+oum(getIndexOfIOutM("112"),n_blokov+1)+oum(getIndexOfIOutM("113"),n_blokov+1)+;
oum(getIndexOfIOutM("114"),n_blokov+1)+oum(getIndexOfIOutM("115"),n_blokov+1)+oum(getIndexOfIOutM("118"),n_blokov+1))*;
inm(getIndexOfIInM("70"),n_blokov+1)+oum(getIndexOfIOutM("116"),n_blokov+1)*inm(getIndexOfIInM("61"),n_blokov+1)+oum(getIndexOfIOutM("117"),n_blokov+1)+;
oum(getIndexOfIOutM("119"),n_blokov+1))                                                      &&120 Q к сн (н) гр
sum=SUM('oum(getIndexOfIOutM("119"),')
IF BL1 OR BL2 OR BL3 OR BL4 OR BL5 OR BL6
DO CIKL WITH getIndexOfIOutM("121"),'oum(getIndexOfIOutM("120"),n_blokov+1)/n_blokov1*1E2/oum(getIndexOfIOutM("64"),i)'  &&121 q к сн (н)
ELSE
DO CIKL WITH getIndexOfIOutM("121"),'IIF(inm(getIndexOfIInM("74"),i)=="1",((oum(getIndexOfIOutM("120"),n_blokov+1)-oum(getIndexOfIOutM("119"),n_blokov+1))*'+;
'oum(getIndexOfIOutM("64"),i)/oum(getIndexOfIOutM("64"),n_blokov+1)+'+;
'oum(getIndexOfIOutM("119"),i))*1E2/oum(getIndexOfIOutM("64"),i),((oum(getIndexOfIOutM("120"),n_blokov+1)-oum(getIndexOfIOutM("119"),n_blokov+1))*'+;
'(oum(getIndexOfIOutM("3"),i)+oum(getIndexOfIOutM("4"),i))/(oum(getIndexOfIOutM("3"),n_blokov+1)+oum(getIndexOfIOutM("4"),n_blokov+1))+'+;
'oum(getIndexOfIOutM("119"),i))*1E2/oum(getIndexOfIOutM("64"),i))'                                          &&121 q к сн (н)
ENDIF
oum(getIndexOfIOutM("121"),n_blokov+1)=CIKL1(getIndexOfIOutM("121"),oum(getIndexOfIOutM("120"),n_blokov+1)*1E2/oum(getIndexOfIOutM("64"),n_blokov+1))&&121 q к сн (н)
DO CIKL WITH getIndexOfIOutM("122"),'F1(inm(getIndexOfIInM("43"),n_blokov+1),"2.17:1")'                   &&122 Q псг (н)
oum(getIndexOfIOutM("123"),n_blokov+1)=CIKL1(getIndexOfIOutM("123"),F1(inm(getIndexOfIInM("43"),n_blokov+1),"2.18:1"))  &&123 Q труб (н)
oum(getIndexOfIOutM("124"),n_blokov+1)=CIKL1(getIndexOfIOutM("124"),;
F2((inm(getIndexOfIInM("50"),n_blokov+1)+inm(getIndexOfIInM("51"),n_blokov+1))*1E3/inm(getIndexOfIInM("70"),n_blokov+1),inm(getIndexOfIInM("91"),n_blokov+1),"2.19:2"))&&124 Q птс (н)
IF BL1 OR BL2 OR BL3 OR BL4 OR BL5 OR BL6
DO CIKL WITH getIndexOfIOutM("125"),'IIF(oum(getIndexOfIOutM("8"),i)=0,0,((oum(getIndexOfIOutM("123"),n_blokov+1)*inm(getIndexOfIInM("71"),n_blokov+1)+'+;
'oum(getIndexOfIOutM("124"),n_blokov+1)*inm(getIndexOfIInM("70"),n_blokov+1)+393*(inm(getIndexOfIInM("50"),n_blokov+1)+'+;
'inm(getIndexOfIInM("51"),n_blokov+1))/1E3)/n_blokov1+oum(getIndexOfIOutM("122"),i)*inm(getIndexOfIInM("71"),i))/oum(getIndexOfIOutM("8"),i)*100)'&&125 alfa пот (н)
ELSE
DO CIKL WITH getIndexOfIOutM("125"),'IIF(oum(getIndexOfIOutM("8"),i)=0,0,((oum(getIndexOfIOutM("123"),n_blokov+1)*inm(getIndexOfIInM("71"),n_blokov+1)+'+;
'(oum(getIndexOfIOutM("124"),n_blokov+1)+IIF(inm(getIndexOfIInM("85"),n_blokov+1)=0,F1(inm(getIndexOfIInM("43"),n_blokov+1),"2.12б:1")+oum(getIndexOfIOutM("112"),n_blokov+1),0))*'+;
'inm(getIndexOfIInM("70"),n_blokov+1)+393*(inm(getIndexOfIInM("50"),n_blokov+1)+'+;
'inm(getIndexOfIInM("51"),n_blokov+1))/1E3)*oum(getIndexOfIOutM("8"),i)/'+;
'oum(getIndexOfIOutM("8"),n_blokov+1)+oum(getIndexOfIOutM("122"),i)*inm(getIndexOfIInM("71"),i))/oum(getIndexOfIOutM("8"),i)*100)'    &&125 alfa пот (н)
ENDIF
sum=0
FOR i=1 TO n_blokov
  sum=sum+oum(getIndexOfIOutM("122"),i)*inm(getIndexOfIInM("71"),i)
ENDFOR
oum(getIndexOfIOutM("125"),n_blokov+1)=CIKL1(getIndexOfIOutM("125"),IIF(oum(getIndexOfIOutM("8"),n_blokov+1)=0,0,;
(oum(getIndexOfIOutM("123"),n_blokov+1)*inm(getIndexOfIInM("71"),n_blokov+1)+;
(oum(getIndexOfIOutM("124"),n_blokov+1)+IIF(inm(getIndexOfIInM("85"),n_blokov+1)=0,F1(inm(getIndexOfIInM("43"),n_blokov+1),"2.12б:1")+oum(getIndexOfIOutM("112"),n_blokov+1),0))*;
inm(getIndexOfIInM("70"),n_blokov+1)+393*(inm(getIndexOfIInM("50"),n_blokov+1)+;
inm(getIndexOfIInM("51"),n_blokov+1))/1E3+sum)/oum(getIndexOfIOutM("8"),n_blokov+1)*100))                 &&125 alfa пот (н)
DO CIKL WITH getIndexOfIOutM("138"),'IIF(inm(getIndexOfIInM("71"),i)=0,0,inm(getIndexOfIInM("47"),i)/inm(getIndexOfIInM("71"),i)*1E3)'&&138 G св
sum=0
FOR i=1 TO n_blokov
  sum=sum+oum(getIndexOfIOutM("138"),i)*oum(getIndexOfIOutM("1"),i)
ENDFOR
oum(getIndexOfIOutM("138"),n_blokov+1)=CIKL1(getIndexOfIOutM("138"),sum/oum(getIndexOfIOutM("1"),n_blokov+1))            &&138 G св
*O CIKL WITH getIndexOfIOutM("139"),'IIF(inm(getIndexOfIInM("10.1"),i)/inm(getIndexOfIInM("1"),i)>1,F1(oum(getIndexOfIOutM("138"),i),"2.39:1"),'+;
*'F1(oum(getIndexOfIOutM("138"),i),"2.39а:1"))'                                               &&139 N сет
*DO CIKL WITH getIndexOfIOutM("139"),'IIF(inm(getIndexOfIInM("10.1"),i)/inm(getIndexOfIInM("1"),i)<=1,F1(oum(getIndexOfIOutM("138"),i),"2.39б:1"),'+;
*'IIF(oum(getIndexOfIOutM("138"),i)<=4500,F1(oum(getIndexOfIOutM("138"),i),"2.39в:1"),'+;
*'F1(oum(getIndexOfIOutM("138"),i),"2.39е:1")))'                                              &&139 N сет
*DO ALTERC WITH getIndexOfIOutM("139"),1,'IIF(inm(getIndexOfIInM("10.1"),i)/inm(getIndexOfIInM("1"),i)<=1,F1(oum(getIndexOfIOutM("138"),i),"2.39б:1"),'+;
*'IIF(oum(getIndexOfIOutM("138"),i)<=4500,F1(oum(getIndexOfIOutM("138"),i),"2.39в:1"),'+;
*'F1(oum(getIndexOfIOutM("138"),i),"2.39г:1")))'                                              &&139 N сет
DO CIKL WITH getIndexOfIOutM("139"),'IIF(oum(getIndexOfIOutM("138"),i)<=4500,'+;
'(F1(oum(getIndexOfIOutM("138"),i),"2.39б:1")*(1.4-MIN(1.4,inm(getIndexOfIInM("10.1"),i)/inm(getIndexOfIInM("1"),i)))+'+;
'F1(oum(getIndexOfIOutM("138"),i),"2.39в:1")*MIN(1.4,inm(getIndexOfIInM("10.1"),i)/inm(getIndexOfIInM("1"),i)))/1.4,'+;
'F1(oum(getIndexOfIOutM("138"),i),"2.39е:1"))'                                               &&139 N сет
DO ALTERC WITH getIndexOfIOutM("139"),1,'IIF(oum(getIndexOfIOutM("138"),i)<=4500,'+;
'(F1(oum(getIndexOfIOutM("138"),i),"2.39б:1")*(1.4-MIN(1.4,inm(getIndexOfIInM("10.1"),i)/inm(getIndexOfIInM("1"),i)))+'+;
'F1(oum(getIndexOfIOutM("138"),i),"2.39в:1")*MIN(1.4,inm(getIndexOfIInM("10.1"),i)/inm(getIndexOfIInM("1"),i)))/1.4,'+;
'F1(oum(getIndexOfIOutM("138"),i),"2.39г:1"))'                                               &&139 N сет
DO CIKL WITH getIndexOfIOutM("126"),'1/1E3*860*oum(getIndexOfIOutM("139"),i)*inm(getIndexOfIInM("71"),i)*85/1E2'        &&126 Q нас (н)
oum(getIndexOfIOutM("126"),n_blokov+1)=CIKL1(getIndexOfIOutM("126"),SUM('oum(getIndexOfIOutM("126"),'))                  &&126 Q нас (н)
DO CIKL WITH getIndexOfIOutM("126.1"),'IIF(oum(getIndexOfIOutM("8"),i)=0,0,oum(getIndexOfIOutM("126"),i)/oum(getIndexOfIOutM("8"),i)*1E2)'&&126.1 alfa нас (н)
oum(getIndexOfIOutM("126.1"),n_blokov+1)=CIKL1(getIndexOfIOutM("126.1"),IIF(oum(getIndexOfIOutM("8"),n_blokov+1)=0,0,;
oum(getIndexOfIOutM("126"),n_blokov+1)/oum(getIndexOfIOutM("8"),n_blokov+1)*1E2))                          &&126.1 alfa нас (н)
DO CIKL WITH getIndexOfIOutM("127"),'IIF(inm(getIndexOfIInM("74"),i)=="1",1,(oum(getIndexOfIOutM("24"),i)*(100+oum(getIndexOfIOutM("41"),i))*'+;
'oum(getIndexOfIOutM("2"),i)/1E5+oum(getIndexOfIOutM("47"),i))/'+;
'(oum(getIndexOfIOutM("24"),i)*(100+oum(getIndexOfIOutM("41"),i))*oum(getIndexOfIOutM("2"),i)/1E5+oum(getIndexOfIOutM("47"),i)+(oum(getIndexOfIOutM("8"),i)-'+;
'oum(getIndexOfIOutM("126"),i))*(100+oum(getIndexOfIOutM("125"),i))/1E2))'                                 &&127 К э
oum(getIndexOfIOutM("127"),n_blokov+1)=CIKL1(getIndexOfIOutM("127"),IIF(SUM('inm(getIndexOfIInM("47"),')=0,1,(oum(getIndexOfIOutM("24"),n_blokov+1)*;
(100+oum(getIndexOfIOutM("41"),n_blokov+1))*oum(getIndexOfIOutM("2"),n_blokov+1)/1E5+oum(getIndexOfIOutM("47"),n_blokov+1))/;
(oum(getIndexOfIOutM("24"),n_blokov+1)*(100+oum(getIndexOfIOutM("41"),n_blokov+1))*oum(getIndexOfIOutM("2"),n_blokov+1)/1E5+;
oum(getIndexOfIOutM("47"),n_blokov+1)+(oum(getIndexOfIOutM("8"),n_blokov+1)-oum(getIndexOfIOutM("126"),n_blokov+1))*;
(100+oum(getIndexOfIOutM("125"),n_blokov+1))/1E2)))                                          &&127 К э
DO CIKL WITH getIndexOfIOutM("128"),'oum(getIndexOfIOutM("36"),i)+oum(getIndexOfIOutM("127"),i)*oum(getIndexOfIOutM("108"),i)/oum(getIndexOfIOutM("2"),i)*100'&&128 Э э сн (н)
oum(getIndexOfIOutM("128"),n_blokov+1)=CIKL1(getIndexOfIOutM("128"),oum(getIndexOfIOutM("36"),n_blokov+1)+oum(getIndexOfIOutM("127"),n_blokov+1)*;
oum(getIndexOfIOutM("108"),n_blokov+1)/oum(getIndexOfIOutM("2"),n_blokov+1)*100)                           &&128 Э э сн (н)
DO CIKL WITH getIndexOfIOutM("129"),'oum(getIndexOfIOutM("88"),i)*(100-oum(getIndexOfIOutM("121"),i))*(100-oum(getIndexOfIOutM("128"),i))/'+;
'100/(100-oum(getIndexOfIOutM("36"),i))'                                                     &&129 КПД к н (ном)
oum(getIndexOfIOutM("129"),n_blokov+1)=CIKL1(getIndexOfIOutM("129"),oum(getIndexOfIOutM("88"),n_blokov+1)*(100-oum(getIndexOfIOutM("121"),n_blokov+1))*;
(100-oum(getIndexOfIOutM("128"),n_blokov+1))/100/(100-oum(getIndexOfIOutM("36"),n_blokov+1)))              &&129 КПД к н (ном)
DO CIKL WITH getIndexOfIOutM("130"),'100-1.0*472.2/oum(getIndexOfIOutM("65"),i)'                           &&130 НЮ тп
oum(getIndexOfIOutM("130"),n_blokov+1)=CIKL1(getIndexOfIOutM("130"),100-1.0*472.2/oum(getIndexOfIOutM("65"),n_blokov+1))      &&130 НЮ тп
DO CIKL WITH getIndexOfIOutM("131"),'oum(getIndexOfIOutM("65"),i)/472.2'                              &&131 К з
oum(getIndexOfIOutM("131"),n_blokov+1)=CIKL1(getIndexOfIOutM("131"),oum(getIndexOfIOutM("65"),n_blokov+1)/472.2)    &&131 К з
DO CIKL WITH getIndexOfIOutM("132"),'IIF(F1(oum(getIndexOfIOutM("131"),i),"2.35:1")>1,1,F1(oum(getIndexOfIOutM("131"),i),"2.35:1"))'&&132 К ст
sum=0
FOR i=1 TO n_blokov
  sum=sum+oum(getIndexOfIOutM("132"),i)*oum(getIndexOfIOutM("64"),i)
ENDFOR
oum(getIndexOfIOutM("132"),n_blokov+1)=CIKL1(getIndexOfIOutM("132"),sum/oum(getIndexOfIOutM("64"),n_blokov+1))           &&132 К ст
DO CIKL WITH getIndexOfIOutM("133"),'IIF(inm(getIndexOfIInM("84"),i)=1,4,IIF(inm(getIndexOfIInM("84"),i)=2,3,'+;
'IIF(inm(getIndexOfIInM("84"),i)>2,0,1/0)))'                                                &&133 К осв э
sum=0
FOR i=1 TO n_blokov
  sum=sum+oum(getIndexOfIOutM("133"),i)*oum(getIndexOfIOutM("2"),i)
ENDFOR
oum(getIndexOfIOutM("133"),n_blokov+1)=CIKL1(getIndexOfIOutM("133"),sum/oum(getIndexOfIOutM("2"),n_blokov+1))            &&133 К осв э
DO CIKL WITH getIndexOfIOutM("134"),'IIF(inm(getIndexOfIInM("84"),i)=1,2,IIF(inm(getIndexOfIInM("84"),i)=2,1.5,'+;
'IIF(inm(getIndexOfIInM("84"),i)>2,0,1/0)))'                                                &&134 К осв т
sum=0
FOR i=1 TO n_blokov
  sum=sum+oum(getIndexOfIOutM("134"),i)*oum(getIndexOfIOutM("64"),i)
ENDFOR
oum(getIndexOfIOutM("134"),n_blokov+1)=CIKL1(getIndexOfIOutM("134"),sum/oum(getIndexOfIOutM("64"),n_blokov+1))           &&134 К осв т
DO CIKL WITH getIndexOfIOutM("135"),'1+oum(getIndexOfIOutM("47"),i)/(oum(getIndexOfIOutM("24"),i)*oum(getIndexOfIOutM("2"),i)*(100+oum(getIndexOfIOutM("41"),i))/'+;
'1E5+oum(getIndexOfIOutM("8"),i)*(100-oum(getIndexOfIOutM("126.1"),i))*(100+oum(getIndexOfIOutM("125"),i))/1E4)'         &&135 К отр (к)
oum(getIndexOfIOutM("135"),n_blokov+1)=CIKL1(getIndexOfIOutM("135"),IIF(inm(getIndexOfIInM("81"),n_blokov+1)=0,1,;
1+oum(getIndexOfIOutM("47"),n_blokov+1)/(oum(getIndexOfIOutM("24"),n_blokov+1)*;
oum(getIndexOfIOutM("2"),n_blokov+1)*(100+oum(getIndexOfIOutM("41"),n_blokov+1))/1E5+oum(getIndexOfIOutM("8"),n_blokov+1)*;
(100-inm(getIndexOfIInM("85"),n_blokov+1)/inm(getIndexOfIInM("81"),n_blokov+1)*100-oum(getIndexOfIOutM("126"),n_blokov+1)/;
oum(getIndexOfIOutM("8"),n_blokov+1)*100)*(100+oum(getIndexOfIOutM("125"),n_blokov+1))/1E4)))              &&135 К отр (к)
DO CIKL WITH getIndexOfIOutM("136"),'oum(getIndexOfIOutM("42"),i)*(100+oum(getIndexOfIOutM("132"),i)+oum(getIndexOfIOutM("133"),i))*oum(getIndexOfIOutM("48"),i)*'+;
'100/(oum(getIndexOfIOutM("129"),i)*oum(getIndexOfIOutM("130"),i)*7*oum(getIndexOfIOutM("135"),i))'                      &&136 b э (н)
oum(getIndexOfIOutM("136"),n_blokov+1)=CIKL1(getIndexOfIOutM("136"),oum(getIndexOfIOutM("42"),n_blokov+1)*(100+oum(getIndexOfIOutM("132"),n_blokov+1)+;
oum(getIndexOfIOutM("133"),n_blokov+1))*oum(getIndexOfIOutM("48"),n_blokov+1)*100/(oum(getIndexOfIOutM("129"),n_blokov+1)*;
oum(getIndexOfIOutM("130"),n_blokov+1)*7*oum(getIndexOfIOutM("135"),n_blokov+1)))                          &&136 b э (н)
DO CIKL WITH getIndexOfIOutM("137"),'oum(getIndexOfIOutM("136"),i)*(1+inm(getIndexOfIInM("64"),n_blokov+1)*'+;
'(1-inm(getIndexOfIInM("65"),n_blokov+1)))'                                                 &&137 b э (нор)
oum(getIndexOfIOutM("137"),n_blokov+1)=CIKL1(getIndexOfIOutM("137"),oum(getIndexOfIOutM("136"),n_blokov+1)*(1+inm(getIndexOfIInM("64"),n_blokov+1)*;
(1-inm(getIndexOfIInM("65"),n_blokov+1))))                                                  &&137 b э (нор)
oum(getIndexOfIOutM("140"),n_blokov+1)=CIKL1(getIndexOfIOutM("140"),(inm(getIndexOfIInM("50"),n_blokov+1)+inm(getIndexOfIInM("51"),n_blokov+1))/;
inm(getIndexOfIInM("71"),n_blokov+1)*1E3)                                                   &&140 G птс
oum(getIndexOfIOutM("141"),n_blokov+1)=CIKL1(getIndexOfIOutM("141"),F1(oum(getIndexOfIOutM("140"),n_blokov+1),"2.36:1")) &&141 N подп
oum(getIndexOfIOutM("142"),n_blokov+1)=CIKL1(getIndexOfIOutM("142"),IIF(inm(getIndexOfIInM("43"),i)<8,;
F1(oum(getIndexOfIOutM("140"),n_blokov+1),"2.37:1"),F1(oum(getIndexOfIOutM("140"),n_blokov+1),"2.37а:1")))  &&142 N хов
DO CIKL WITH getIndexOfIOutM("143"),'IIF(inm(getIndexOfIInM("74"),i)=="2" OR inm(getIndexOfIInM("74"),i)=="3",F1(oum(getIndexOfIOutM("10"),i),"2.38а:1")/1E3,'+;
'IIF(inm(getIndexOfIInM("74"),i)=="2а",F1(oum(getIndexOfIOutM("10"),i),"2.38:1")/1E3,0))'                 &&143 N кнб
IF BL1 OR BL2 OR BL3 OR BL4 OR BL5 OR BL6
IF BETWEEN(MONTH(DATE()),6,9) OR MONTH(DATE())=5 AND DAY(DATE())>15
DO CIKL WITH getIndexOfIOutM("144"),'IIF(inm(getIndexOfIInM("74"),i)=="1",0,(oum(getIndexOfIOutM("139"),i)+oum(getIndexOfIOutM("143"),i))*'+;
'inm(getIndexOfIInM("71"),i)+'+;
'(oum(getIndexOfIOutM("141"),n_blokov+1)+oum(getIndexOfIOutM("142"),n_blokov+1))*inm(getIndexOfIInM("71"),n_blokov+1)*'+;
'oum(getIndexOfIOutM("8"),i)/oum(getIndexOfIOutM("8"),n_blokov+1))'                                        &&144 Э тепл(н)
ELSE
DO CIKL WITH getIndexOfIOutM("144"),'IIF(inm(getIndexOfIInM("74"),i)=="1",0,(oum(getIndexOfIOutM("139"),i)+oum(getIndexOfIOutM("143"),i))*'+;
'inm(getIndexOfIInM("71"),i)+'+;
'(oum(getIndexOfIOutM("141"),n_blokov+1)+oum(getIndexOfIOutM("142"),n_blokov+1))*inm(getIndexOfIInM("71"),n_blokov+1)*'+;
'oum(getIndexOfIOutM("7"),i)/oum(getIndexOfIOutM("7"),n_blokov+1))'                                        &&144 Э тепл(н)
ENDIF
ELSE
DO CIKL WITH getIndexOfIOutM("144"),'IIF(inm(getIndexOfIInM("74"),i)=="1",0,(oum(getIndexOfIOutM("139"),i)+oum(getIndexOfIOutM("143"),i))*'+;
'inm(getIndexOfIInM("71"),i)+'+;
'(oum(getIndexOfIOutM("141"),n_blokov+1)+oum(getIndexOfIOutM("142"),n_blokov+1))*inm(getIndexOfIInM("71"),n_blokov+1)*'+;
'oum(getIndexOfIOutM("8"),i)/oum(getIndexOfIOutM("8"),n_blokov+1))'                                        &&144 Э тепл(н)
ENDIF
oum(getIndexOfIOutM("144"),n_blokov+1)=CIKL1(getIndexOfIOutM("144"),SUM('oum(getIndexOfIOutM("144"),'))                  &&144 Э тепл(н)
DO CIKL WITH getIndexOfIOutM("145"),'IIF(inm(getIndexOfIInM("74"),i)=="1",0,(100+oum(getIndexOfIOutM("125"),i))*'+;
'(100+oum(getIndexOfIOutM("132"),i)+oum(getIndexOfIOutM("134"),i))*1E3/'+;
'(oum(getIndexOfIOutM("129"),i)*oum(getIndexOfIOutM("130"),i)*7*oum(getIndexOfIOutM("135"),i)))'                         &&145 b тэ бл
sum=0
FOR i=1 TO n_blokov
  sum=sum+oum(getIndexOfIOutM("145"),i)*oum(getIndexOfIOutM("8"),i)
ENDFOR
oum(getIndexOfIOutM("145"),n_blokov+1)=CIKL1(getIndexOfIOutM("145"),sum/oum(getIndexOfIOutM("8"),n_blokov+1))            &&145 b тэ бл
*oum(getIndexOfIOutM("145"),n_blokov+1)=CIKL1(getIndexOfIOutM("145"),(100+oum(getIndexOfIOutM("125"),n_blokov+1))*;
(100+oum(getIndexOfIOutM("132"),n_blokov+1)+oum(getIndexOfIOutM("134"),n_blokov+1))*1E3/(oum(getIndexOfIOutM("129"),n_blokov+1)*;
oum(getIndexOfIOutM("130"),n_blokov+1)*7*oum(getIndexOfIOutM("135"),n_blokov+1)))                          &&145 b тэ бл
oum(getIndexOfIOutM("146"),n_blokov+1)=CIKL1(getIndexOfIOutM("146"),inm(getIndexOfIInM("86"),n_blokov+1))               &&146 b тэ пвк
oum(getIndexOfIOutM("146.1"),n_blokov+1)=CIKL1(getIndexOfIOutM("146.1"),inm(getIndexOfIInM("85"),n_blokov+1)/oum(getIndexOfIOutM("5"),n_blokov+1)*;
1E2)                                                                           &&146.1 alfa пвк
DO CIKL WITH getIndexOfIOutM("147"),'IIF(oum(getIndexOfIOutM("8"),i)=0,0,oum(getIndexOfIOutM("144"),i)*oum(getIndexOfIOutM("136"),i)/oum(getIndexOfIOutM("8"),i))' &&147 db тэ
sum=0
FOR i=1 TO n_blokov
  sum=sum+oum(getIndexOfIOutM("147"),i)*oum(getIndexOfIOutM("3"),i)
ENDFOR
oum(getIndexOfIOutM("147"),n_blokov+1)=CIKL1(getIndexOfIOutM("147"),sum/oum(getIndexOfIOutM("3"),n_blokov+1))            &&147 db тэ
*um(getIndexOfIOutM("147"),n_blokov+1)=CIKL1(getIndexOfIOutM("147"),oum(getIndexOfIOutM("144"),n_blokov+1)*oum(getIndexOfIOutM("136"),n_blokov+1)/;
*um(getIndexOfIOutM("8"),n_blokov+1))                                                   &&147 db тэ
DO CIKL WITH getIndexOfIOutM("148"),'oum(getIndexOfIOutM("145"),i)*(100-oum(getIndexOfIOutM("126.1"),i))/1E2+oum(getIndexOfIOutM("147"),i)'&&148 b тэ (н)
oum(getIndexOfIOutM("148"),n_blokov+1)=CIKL1(getIndexOfIOutM("148"),oum(getIndexOfIOutM("145"),n_blokov+1)*(100-;
oum(getIndexOfIOutM("126.1"),n_blokov+1))/1E2+oum(getIndexOfIOutM("147"),n_blokov+1))                      &&148 b тэ (н)
oum(getIndexOfIOutM("149"),n_blokov+1)=CIKL1(getIndexOfIOutM("149"),(oum(getIndexOfIOutM("145"),n_blokov+1)*(100-;
oum(getIndexOfIOutM("146.1"),n_blokov+1)-oum(getIndexOfIOutM("126.1"),n_blokov+1))+oum(getIndexOfIOutM("146"),n_blokov+1)*;
oum(getIndexOfIOutM("146.1"),n_blokov+1))/1E2+inm(getIndexOfIInM("87"),n_blokov+1)*1E3/oum(getIndexOfIOutM("5"),n_blokov+1)+;
oum(getIndexOfIOutM("147"),n_blokov+1))                                                      &&149 b тэ (н) ст
DO CIKL WITH getIndexOfIOutM("150"),'oum(getIndexOfIOutM("145"),i)*(100-oum(getIndexOfIOutM("126.1"),i))/1E2*(1+inm(getIndexOfIInM("66"),n_blokov+1)'+;
'*(1-inm(getIndexOfIInM("67"),n_blokov+1)))+oum(getIndexOfIOutM("147"),i)*oum(getIndexOfIOutM("137"),i)/oum(getIndexOfIOutM("136"),i)'&&150 b тэ (нр)
oum(getIndexOfIOutM("150"),n_blokov+1)=CIKL1(getIndexOfIOutM("150"),oum(getIndexOfIOutM("145"),n_blokov+1)*(100-;
oum(getIndexOfIOutM("146.1"),n_blokov+1)-oum(getIndexOfIOutM("126.1"),n_blokov+1))*(1+inm(getIndexOfIInM("66"),n_blokov+1)*;
(1-inm(getIndexOfIInM("67"),n_blokov+1)))/1E2+oum(getIndexOfIOutM("146"),n_blokov+1)*oum(getIndexOfIOutM("146.1"),n_blokov+1)/1E2+;
inm(getIndexOfIInM("87"),n_blokov+1)*1E3/oum(getIndexOfIOutM("5"),n_blokov+1)+oum(getIndexOfIOutM("147"),n_blokov+1)*;
oum(getIndexOfIOutM("137"),n_blokov+1)/oum(getIndexOfIOutM("136"),n_blokov+1))                             &&150 b тэ (нр)
DO CIKL WITH getIndexOfIOutM("151"),'oum(getIndexOfIOutM("24"),i)*oum(getIndexOfIOutM("2"),i)/1E3+(oum(getIndexOfIOutM("8"),i)-'+;
'oum(getIndexOfIOutM("126"),i))*(100+oum(getIndexOfIOutM("125"),i))/1E2+oum(getIndexOfIOutM("41"),i)*oum(getIndexOfIOutM("24"),i)*oum(getIndexOfIOutM("2"),i)/'+;
'1E3/1E2+oum(getIndexOfIOutM("121"),i)*oum(getIndexOfIOutM("64"),i)/100+472.2*oum(getIndexOfIOutM("1"),i)/1E2'      &&151 balance
oum(getIndexOfIOutM("151"),n_blokov+1)=CIKL1(getIndexOfIOutM("151"),oum(getIndexOfIOutM("24"),n_blokov+1)*;
oum(getIndexOfIOutM("2"),n_blokov+1)/1E3+(oum(getIndexOfIOutM("8"),n_blokov+1)-inm(getIndexOfIInM("85"),n_blokov+1)-;
oum(getIndexOfIOutM("126"),n_blokov+1))*(100+oum(getIndexOfIOutM("125"),n_blokov+1))/1E2+oum(getIndexOfIOutM("41"),n_blokov+1)*;
oum(getIndexOfIOutM("24"),n_blokov+1)*oum(getIndexOfIOutM("2"),n_blokov+1)/1E3/1E2+oum(getIndexOfIOutM("120"),n_blokov+1)+472.2*;
oum(getIndexOfIOutM("1"),n_blokov+1)/1E2)                                               &&151 balance
DO CIKL WITH getIndexOfIOutM("152"),'(oum(getIndexOfIOutM("64"),i)-oum(getIndexOfIOutM("151"),i))/oum(getIndexOfIOutM("64"),i)*1E2'    &&152 balance
oum(getIndexOfIOutM("152"),n_blokov+1)=CIKL1(getIndexOfIOutM("152"),(oum(getIndexOfIOutM("64"),n_blokov+1)-oum(getIndexOfIOutM("151"),n_blokov+1))/oum(getIndexOfIOutM("64"),n_blokov+1)*1E2)                                                   &&152 balance
oum(getIndexOfIOutM("153"),n_blokov+1)=CIKL1(getIndexOfIOutM("153"),oum(getIndexOfIOutM("36"),n_blokov+1)*oum(getIndexOfIOutM("2"),n_blokov+1)/1E2+oum(getIndexOfIOutM("108"),n_blokov+1)+oum(getIndexOfIOutM("144"),n_blokov+1))                        &&153 S Э сн (н)
oum(getIndexOfIOutM("154"),n_blokov+1)=CIKL1(getIndexOfIOutM("154"),inm(getIndexOfIInM("3"),n_blokov+1))           &&154 S Э сн (ф)

do calc_end

ON ERROR

**********************************************************************
FUNC CIKL1
PARAMETERS m,part,round
IF PARAM()=2
   round=.T.
ENDIF
** яч.вых.т.не пересчитывыема => вычислений не делать
RETURN(IIF(SUBSTR(outm(m,n_blokov+1),1,1)='=',VAL(oum(m,n_blokov+1)),;
IIF(round,ROUND(part,exact(m,1)),part)))
