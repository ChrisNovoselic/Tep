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

DO CIKL WITH o("1"),'inm(i("1"),i)'                                            && 1 TAU раб
oum(o("1"),n_blokov+1)=CIKL1(o("1"),SUM('inm(i("1"),'))                        && 1 TAU раб
DO CIKL WITH o("2"),'inm(i("2"),i)'                                            && 2 Э т
oum(o("2"),n_blokov+1)=CIKL1(o("2"),SUM('inm(i("2"),'))                        && 2 Э т
IF BL1 OR BL2 OR BL3 OR BL4 OR BL5 OR BL6
DO CIKL WITH o("3"),'IIF(inm(i("47"),i)/inm(i("1"),i)<0.7,0,inm(i("47"),i)*(inm(i("48"),i)'+;
'-inm(i("49"),i)))'                                                            && 3 Q то
ELSE
DO CIKL WITH o("3"),'inm(i("47"),i)*(inm(i("48"),i)-inm(i("49"),i))'           && 3 Q то
ENDIF
oum(o("3"),n_blokov+1)=CIKL1(o("3"),SUM('oum(o("3"),'))                        && 3 Q то
IF BL1 OR BL2 OR BL3 OR BL4 OR BL5 OR BL6
DO CIKL WITH o("4"),'IIF(oum(o("3"),i)=0,0,inm(i("46"),i))'                    && 4 Q пп
ELSE
DO CIKL WITH o("4"),'inm(i("46"),i)'                                           && 4 Q пп
ENDIF
oum(o("4"),n_blokov+1)=CIKL1(o("4"),SUM('oum(o("4"),'))                        && 4 Q пп
oum(o("5"),n_blokov+1)=CIKL1(o("5"),inm(i("81"),n_blokov+1))                   && 5 Q отп ст
oum(o("6"),n_blokov+1)=CIKL1(o("6"),inm(i("82"),n_blokov+1))                   && 6 Q отп роу
IF BL1 OR BL2 OR BL3 OR BL4 OR BL5 OR BL6
DO CIKL WITH o("6"),'oum(o("4"),i)/2'                                          && 6 Q отп роу
ELSE
sum=0
FOR i=1 TO n_blokov
  sum=sum+IIF(oum(o("3"),i)=0,0,oum(o("4"),i))
ENDFOR
DO CIKL WITH o("6"),'IIF(oum(o("3"),i)=0,0,IIF(oum(o("4"),i)=0,0,oum(o("6"),n_blokov+1)*'+;
'oum(o("4"),i)/sum))'                                                          && 6 Q отп роу
IF SUM('oum(o("6"),')#oum(o("6"),n_blokov+1)   && устранение ошибк.округления
   oum(o("6"),1)=oum(o("6"),1)+IIF(SUM('oum(o("6"),')<oum(o("6"),n_blokov+1),1,-1)
ENDIF
ENDIF
oum(o("7"),n_blokov+1)=CIKL1(o("7"),oum(o("5"),n_blokov+1)-oum(o("6"),n_blokov+1)-;
inm(i("85"),n_blokov+1))                                                       && 7 Q отп тепл
IF BL1 OR BL2 OR BL3 OR BL4 OR BL5 OR BL6
DO CIKL WITH o("7"),'oum(o("3"),i)*'+;
'IIF(BETWEEN(MONTH(DATE()),6,9) OR MONTH(DATE())=5 AND DAY(DATE())>15,.97,.95)'&& 7 Q отп тепл
ELSE
DO CIKL WITH o("7"),'IIF(oum(o("3"),n_blokov+1)=0,0,oum(o("7"),n_blokov+1)*oum(o("3"),i)/'+;
'oum(o("3"),n_blokov+1))'                                                      && 7 Q отп тепл
IF SUM('oum(o("7"),')#oum(o("7"),n_blokov+1)   && устранение ошибк.округления 
   oum(o("7"),1)=oum(o("7"),1)+IIF(SUM('oum(o("7"),')<oum(o("7"),n_blokov+1),1,-1)
ENDIF
ENDIF
DO CIKL WITH o("8"),'oum(o("6"),i)+oum(o("7"),i)'                              && 8 Q отп
oum(o("8"),n_blokov+1)=CIKL1(o("8"),SUM('oum(o("8"),'))                        && 8 Q отп
DO CIKL WITH o("9"),'oum(o("2"),i)/oum(o("1"),i)'                              && 9 N т
oum(o("9"),n_blokov+1)=CIKL1(o("9"),oum(o("2"),n_blokov+1)/oum(o("1"),n_blokov+1))&& 9 N т
DO CIKL WITH o("10"),'oum(o("3"),i)/oum(o("1"),i)'                             &&10 Q т ср
oum(o("10"),n_blokov+1)=CIKL1(o("10"),oum(o("3"),n_blokov+1)/oum(o("1"),n_blokov+1))&&10 Q т ср
DO CIKL WITH o("10.1"),'inm(i("37"),i)'                                        &&10.1 P вто
DO CIKL WITH o("11"),'oum(o("4"),i)/oum(o("1"),i)'                             &&11 Q роу ср
oum(o("11"),n_blokov+1)=CIKL1(o("11"),oum(o("4"),n_blokov+1)/oum(o("1"),n_blokov+1))&&11 Q роу ср
*       sss=SECON()
DO CIKL WITH o("12"),'IIF(inm(i("74"),i)=="1",F1(oum(o("9"),i),"2.40:1"),'+;
'IIF(inm(i("74"),i)=="2",F3(oum(o("9"),i),oum(o("10"),i),oum(o("10.1"),i),"2.1:3"),'+;
'IIF(inm(i("74"),i)=="2а",F3(oum(o("9"),i),oum(o("10"),i),inm(i("38"),i),"2.86:3"),'+;
'IIF(inm(i("74"),i)=="3",F2(oum(o("9"),i),oum(o("10.1"),i),"2.50:2"),1/0))))'  &&12 q т бр (исх)
*       wait window (str(SECON()-sss,10,3))
DO CIKL WITH o("13"),'IIF(inm(i("74"),i)=="1",F1(oum(o("9"),i),"2.55:1")+'+;
'inm(i("46"),i)/.7/inm(i("1"),i),'+;
'IIF(inm(i("74"),i)=="2",F3(oum(o("9"),i),oum(o("10"),i),oum(o("10.1"),i),"2.2:3")+inm(i("46"),i)/.7/inm(i("1"),i),'+;
'IIF(inm(i("74"),i)=="2а",F3(oum(o("9"),i),oum(o("10"),i),inm(i("38"),i),"2.87:3")+inm(i("46"),i)/.7/inm(i("1"),i),'+;
'IIF(inm(i("74"),i)=="3",F3(oum(o("9"),i),oum(o("10"),i),oum(o("10.1"),i),"2.2:3")+'+;
'inm(i("46"),i)/.7/inm(i("1"),i),1/0))))'                                      &&13 G o
DO CIKL WITH o("14"),'IIF(inm(i("74"),i)=="1",F1(oum(o("13"),i),"2.3:1")-inm(i("46"),i)/.7/inm(i("1"),i),'+;
'IIF(inm(i("74"),i)=="2" OR inm(i("74"),i)=="3",'+;
'F3(oum(o("13"),i),oum(o("10"),i),oum(o("10.1"),i),"2.3б:3")-inm(i("46"),i)/.7/inm(i("1"),i),'+;
'IIF(inm(i("74"),i)=="2а",F3(oum(o("13"),i),oum(o("10"),i),inm(i("38"),i),"2.3а:3")'+;
'-inm(i("46"),i)/.7/inm(i("1"),i),1/0)))'                                      &&14 G 2
DO CIKL WITH o("14"),'IIF(oum(o("14"),i)<0,0,oum(o("14"),i))'                  &&14 G 2
oum(o("14"),n_blokov+1)=CIKL1(o("14"),IIF(BL1 OR BL4 OR BL5 OR BL6,SUM('oum(o("14"),')*n_blokov1,;
SUM('oum(o("14"),')))                                                          &&14 G 2
oum(o("14.1"),n_blokov+1)=CIKL1(o("14.1"),IIF(inm(i("70"),n_blokov+1)=0,0,F2(IIF(BL1 OR BL4 OR BL5 OR BL6,;
n_blokov1,inm(i("89"),n_blokov+1)),;
ROUND(inm(i("6"),n_blokov+1)/inm(i("70"),n_blokov+1)/1.9,1),"2.4а:2")))        &&14.1 G цв
DO CIKL WITH o("15"),'F3(oum(o("14"),i),inm(i("28"),i),oum(o("14.1"),n_blokov+1),"2.4:3")'
                                                                               &&15 Р 2(н)
DO CIKL WITH o("15.1"),'F2(oum(o("15"),i),oum(o("14"),i),"2.84:2")'            &&15.1 dQ э(P2)
DO CIKL WITH o("16"),'1E3*oum(o("15.1"),i)/oum(o("9"),i)'                      &&16 dqт бр(P2)
DO CIKL WITH o("17"),'IIF(inm(i("74"),i)=="1",F1(oum(o("13"),i),"2.5а:1")*oum(o("11"),i),'+;
'IIF(inm(i("74"),i)=="2" OR inm(i("74"),i)=="2а" OR inm(i("74"),i)=="3",'+;
'F2(oum(o("13"),i),oum(o("10"),i),"2.5:2")*oum(o("11"),i),1/0))'               &&17 dqт бр(Qпп)
DO CIKL WITH o("18"),'IIF(inm(i("74"),i)=="1",1/0,'+;
'IIF(inm(i("74"),i)=="2",F1(oum(o("10.1"),i),"2.6:1"),'+;
'IIF(inm(i("74"),i)=="2а",F1(inm(i("38"),i),"2.89:1"),'+;
'IIF(inm(i("74"),i)=="3",F1(oum(o("10.1"),i),"2.6:1"),1/0))))'                 &&18 t 2(н)
DO CIKL WITH o("19"),'inm(i("49"),i)-oum(o("18"),i)'                           &&19 dt 2
DO CIKL WITH o("20"),'IIF(inm(i("74"),i)=="1" OR inm(i("74"),i)=="2а",0,1/0)'  &&20 dqт бр(t 2)   

PUBLIC o20_1(n_blokov),o20_2(n_blokov)                                         && для 4-мерного(!!!) графика
DO CIKL WITH o("20"),'IIF(inm(i("74"),i)=="2" OR inm(i("74"),i)=="3",'+;
'F3(oum(o("13"),i),oum(o("10"),i),oum(o("19"),i),'+;
'"2.7"+IIF(oum(o("10.1"),i)<0.8,"",IIF(BETWEEN(oum(o("10.1"),i),0.8,0.99),"",IIF(BETWEEN(oum(o("10.1"),i),1,1.19),"а",'+;
'IIF(BETWEEN(oum(o("10.1"),i),1.2,1.39),"б",IIF(BETWEEN(oum(o("10.1"),i),1.4,1.59),"в",'+;
'IIF(BETWEEN(oum(o("10.1"),i),1.6,1.79),"г",IIF(oum(o("10.1"),i)>=1.8,"д","")))))))'+;
'+IIF(oum(o("19"),i)<0,"-","+")+":3"),oum(o("20"),i))'                         &&20 dqт бр(t 2)   
FOR i=1 TO n_blokov
  o20_1(i)=oum(o("20"),i)
ENDFOR
DO CIKL WITH o("20"),'IIF(inm(i("74"),i)=="2" OR inm(i("74"),i)=="3",'+;
'F3(oum(o("13"),i),oum(o("10"),i),oum(o("19"),i),'+;
'"2.7"+IIF(oum(o("10.1"),i)<0.8,"",IIF(BETWEEN(oum(o("10.1"),i),0.8,0.99),"а",'+;
'IIF(BETWEEN(oum(o("10.1"),i),1,1.19),"б",IIF(BETWEEN(oum(o("10.1"),i),1.2,1.39),"в",'+;
'IIF(BETWEEN(oum(o("10.1"),i),1.4,1.59),"г",IIF(BETWEEN(oum(o("10.1"),i),1.6,1.79),"д",'+;
'IIF(oum(o("10.1"),i)>=1.8,"е","")))))))'+;
'+IIF(oum(o("19"),i)<0,"-","+")+":3"),oum(o("20"),i))'                         &&20 dqт бр(t 2)   
FOR i=1 TO n_blokov
  o20_2(i)=oum(o("20"),i)
ENDFOR
*wait wind str(o20_1(n_blokov),10,2)+" | "+str(o20_2(n_blokov),10,2)
DO CIKL WITH o("20"),'IIF(inm(i("74"),i)=="2" OR inm(i("74"),i)=="3",'+;
'IIF(oum(o("10.1"),i)<0.8,o20_1(i),'+;
'IIF(BETWEEN(oum(o("10.1"),i),0.8,0.99),(o20_1(i)*(0.99-oum(o("10.1"),i))+o20_2(i)*(oum(o("10.1"),i)-0.8))/0.19,'+;
'IIF(BETWEEN(oum(o("10.1"),i),1,1.19),(o20_1(i)*(1.19-oum(o("10.1"),i))+o20_2(i)*(oum(o("10.1"),i)-1))/0.19,'+;
'IIF(BETWEEN(oum(o("10.1"),i),1.2,1.39),(o20_1(i)*(1.39-oum(o("10.1"),i))+o20_2(i)*(oum(o("10.1"),i)-1.2))/0.19,'+;
'IIF(BETWEEN(oum(o("10.1"),i),1.4,1.59),(o20_1(i)*(1.59-oum(o("10.1"),i))+o20_2(i)*(oum(o("10.1"),i)-1.4))/0.19,'+;
'IIF(BETWEEN(oum(o("10.1"),i),1.6,1.79),(o20_1(i)*(1.79-oum(o("10.1"),i))+o20_2(i)*(oum(o("10.1"),i)-1.6))/0.19,'+;
'IIF(BETWEEN(oum(o("10.1"),i),1.8,1.99),(o20_1(i)*(1.99-oum(o("10.1"),i))+o20_2(i)*(oum(o("10.1"),i)-1.8))/0.19,'+;
'o20_2(i) ))))))),oum(o("20"),i))'                                             &&20 dqт бр(t 2)   
RELEASE o20_1,o20_2                                                            && для 4-мерного(!!!) графика
DO CIKL WITH o("20"),'IIF(oum(o("19"),i)=0,0,oum(o("20"),i))'                  &&20 dqт бр(t 2) 

DO CIKL WITH o("21"),'IIF(inm(i("25"),i)/oum(o("1"),i)=oum(o("13"),i),0,'+;
'IIF(inm(i("74"),i)=="1",F1(oum(o("13"),i),"2.8"+'+;
'IIF(inm(i("25"),i)/oum(o("1"),i)>oum(o("13"),i),"-","+")+":1"),'+;
'IIF( inm(i("74"),i)=="2" OR inm(i("74"),i)=="2а" OR inm(i("74"),i)=="3",F2(oum(o("13"),i),oum(o("10"),i),"2.8а"+'+;
'IIF(inm(i("25"),i)/oum(o("1"),i)>oum(o("13"),i),"-","+")+":2"),1/0)))'        &&21 dqт бр(Gпв)
*O ALTERC WITH o("22"),1,'IIF(inm(i("72"),i)>35000,oum(o("12"),i)*.0085*'+;
'(inm(i("72"),i)-57135)/1E5,0)'                                                &&22 dqт бр(рес)
*O ALTERC WITH o("22"),2,'IIF(inm(i("72"),i)>35000,oum(o("12"),i)*.0085*'+;
'(inm(i("72"),i)-53904)/1E5,0)'                                                &&22 dqт бр(рес)
*O ALTERC WITH o("22"),3,'IIF(inm(i("72"),i)>35000,oum(o("12"),i)*.0085*'+;
'(inm(i("72"),i)-44557)/1E5,0)'                                                &&22 dqт бр(рес)
*O ALTERC WITH o("22"),4,'IIF(inm(i("72"),i)>35000,oum(o("12"),i)*.0085*'+;
'(inm(i("72"),i)-35717)/1E5,0)'                                                &&22 dqт бр(рес)
*O ALTERC WITH o("22"),5,'IIF(inm(i("72"),i)>35000,oum(o("12"),i)*.0085*'+;
'(inm(i("72"),i)-14771)/1E5,0)'                                                &&22 dqт бр(рес)
*O ALTERC WITH o("22"),6,'IIF(inm(i("72"),i)>35000,oum(o("12"),i)*.0085*'+;
'(inm(i("72"),i)-0)/1E5,0)'                                                    &&22 dqт бр(рес)
DO ALTERC WITH o("22"),1,'IIF(inm(i("72"),i)>35000,oum(o("12"),i)*.0085*'+;
'(inm(i("72"),i)-35000)/1E5,0)'                                                &&22 dqт бр(рес)
DO ALTERC WITH o("22"),2,'IIF(inm(i("72"),i)>35000,oum(o("12"),i)*.0085*'+;
'(inm(i("72"),i)-35000)/1E5,0)'                                                &&22 dqт бр(рес)
DO ALTERC WITH o("22"),3,'IIF(inm(i("72"),i)>35000,oum(o("12"),i)*.0085*'+;
'(inm(i("72"),i)-35000)/1E5,0)'                                                &&22 dqт бр(рес)
DO ALTERC WITH o("22"),4,'IIF(inm(i("72"),i)>35000,oum(o("12"),i)*.0085*'+;
'(inm(i("72"),i)-35000)/1E5,0)'                                                &&22 dqт бр(рес)
DO ALTERC WITH o("22"),5,'IIF(inm(i("72"),i)>35000,oum(o("12"),i)*.0085*'+;
'(inm(i("72"),i)-35000)/1E5,0)'                                                &&22 dqт бр(рес)
DO ALTERC WITH o("22"),6,'IIF(inm(i("72"),i)>35000,oum(o("12"),i)*.0085*'+;
'(inm(i("72"),i)-35000)/1E5,0)'                                                    &&22 dqт бр(рес)
DO CIKL WITH o("23"),'182.3*inm(i("69"),i)*1E3/oum(o("2"),i)'                  &&23 dqт бр(пуск)
DO CIKL WITH o("24"),'oum(o("12"),i)+oum(o("16"),i)+oum(o("17"),i)+oum(o("20"),i)+'+;
'oum(o("21"),i)+oum(o("22"),i)+oum(o("23"),i)'                                 &&24 qт бр(ном)
sum=0
FOR i=1 TO n_blokov
  sum=sum+oum(o("24"),i)*oum(o("2"),i)
ENDFOR
oum(o("24"),n_blokov+1)=CIKL1(o("24"),sum/oum(o("2"),n_blokov+1))              &&24 qт бр(ном)
DO CIKL WITH o("25"),'IIF(inm(i("74"),i)=="1",0,'+;
'IIF(inm(i("74"),i)=="2",F2(oum(o("13"),i),oum(o("10.1"),i),"2.9:2"),'+;
'IIF(inm(i("74"),i)=="2а",F2(oum(o("13"),i),inm(i("38"),i),"2.9а:2"),'+;
'IIF(inm(i("74"),i)=="3",F2(oum(o("13"),i),inm(i("38"),i),"2.9б:2"),1/0))))'   &&25 W т/тф(ном)
*DO CIKL WITH o("49"),'(inm(i("17"),i)+inm(i("17.1"),i))/2-'+;
'F1((inm(i("13"),i)+inm(i("14"),i))/inm(i("1"),i),"2.22:1")+273.15',.F.        &&49 D пе
*DO CIKL WITH o("49"),'(inm(i("13"),i)+inm(i("14"),i))*SQRT(.02526/(4.7061/1E2*'+;
'(oum(o("49"),i)/1000)/((inm(i("15.2"),i)+F1((inm(i("13"),i)+inm(i("14"),i))/oum(o("1"),i),'+;
'"2.41:1"))/100)+.32371/1E3+2.5/1E4*(oum(o("49"),i)/1000)-'+;
'1.1354/1E3/(oum(o("49"),i)/1000)^2-4.381/1E4/((oum(o("49"),i)/1000)-.21)^2-(2.549/1E5/'+;
'(oum(o("49"),i)/1000)^8+1.236/1E7/(oum(o("49"),i)/1000)^14-5.5/1E5)*((inm(i("15.2"),i)+'+;
'F1((inm(i("13"),i)+inm(i("14"),i))/oum(o("1"),i),"2.41:1"))/100)))'           &&49 D пе
*DO ALTERC WITH o("49"),1,'inm(i("13"),i)+inm(i("14"),i)'                       &&49 D пе
*DO ALTERC WITH o("49"),4,'inm(i("13"),i)+inm(i("14"),i)'                       &&49 D пе
*DO ALTERC WITH o("49"),5,'inm(i("13"),i)+inm(i("14"),i)'                       &&49 D пе
DO CIKL WITH o("49"),'inm(i("13"),i)+inm(i("14"),i)'                           &&49 D пе
oum(o("49"),n_blokov+1)=CIKL1(o("49"),SUM('oum(o("49"),'))                     &&49 D пе
DO CIKL WITH o("50"),'oum(o("49"),i)/oum(o("1"),i)'                            &&50 D пе
oum(o("50"),n_blokov+1)=CIKL1(o("50"),oum(o("49"),n_blokov+1)/oum(o("1"),n_blokov+1))&&50 D пе
DO CIKL WITH o("51"),'(inm(i("17"),i)+inm(i("17.1"),i))/2'                     &&51 t пе
DO ALTERC WITH o("51"),5,'(inm(i("18"),i)+inm(i("18.1"),i))/2+'+;
'F1(oum(o("50"),i),"2.22:1")'                                                  &&51 t пе
DO CIKL WITH o("51.1"),'(inm(i("17"),i)+inm(i("17.1"),i))/2-F1(oum(o("50"),i),"2.22:1")'&&51.1 t oп
DO ALTERC WITH o("51.1"),1,'(inm(i("18"),i)+inm(i("18.1"),i))/2'               &&51.1 t oп
DO ALTERC WITH o("51.1"),4,'(inm(i("18"),i)+inm(i("18.1"),i))/2'               &&51.1 t oп
DO ALTERC WITH o("51.1"),5,'(inm(i("18"),i)+inm(i("18.1"),i))/2'               &&51.1 t oп
DO ALTERC WITH o("51.1"),6,'(inm(i("18"),i)+inm(i("18.1"),i))/2'               &&51.1 t oп
DO CIKL WITH o("54"),'IIF(inm(i("74"),i)=="1",F1(oum(o("50"),i),"2.24а:1"),'+;
'IIF(inm(i("74"),i)=="2" OR inm(i("74"),i)=="3",F1(oum(o("50"),i),"2.24:1"),'+;
'IIF(inm(i("74"),i)=="2а",F1(oum(o("50"),i),"2.24б:1"),1/0)))'                 &&54 D хпп
DO CIKL WITH o("55"),'oum(o("54"),i)*oum(o("1"),i)-inm(i("46"),i)/.7'          &&55 D гпп
DO CIKL WITH o("52"),'(inm(i("21"),i)+inm(i("21.1"),i))/2'                     &&52 t гпп
DO ALTERC WITH o("52"),5,'(inm(i("22"),i)+inm(i("22.1"),i))/2+'+;
'F1(oum(o("55"),i)/oum(o("1"),i),"2.22б:1")'                                   &&52 t гпп
DO CIKL WITH o("52.1"),'(inm(i("21"),i)+inm(i("21.1"),i))/2-'+;
'F1(oum(o("55"),i)/oum(o("1"),i),"2.22б:1")'                                   &&52.1 t гпп
DO ALTERC WITH o("52.1"),1,'(inm(i("22"),i)+inm(i("22.1"),i))/2'               &&52.1 t гпп
DO ALTERC WITH o("52.1"),4,'(inm(i("22"),i)+inm(i("22.1"),i))/2'               &&52.1 t гпп
DO ALTERC WITH o("52.1"),5,'(inm(i("22"),i)+inm(i("22.1"),i))/2'               &&52.1 t гпп
DO ALTERC WITH o("52.1"),6,'(inm(i("22"),i)+inm(i("22.1"),i))/2'               &&52.1 t гпп
DO CIKL WITH o("53"),'inm(i("19"),i)'                                          &&53 P гпп
DO CIKL WITH o("56"),'(inm(i("15"),i)+inm(i("15.1"),i))/2+'+;
'F1(oum(o("50"),i),"2.41а:1")'                                                 &&56 P пе
DO ALTERC WITH o("56"),4,'(inm(i("15"),i)+inm(i("15.1"),i))/2'                 &&56 P пе
DO ALTERC WITH o("56"),6,'(inm(i("15"),i)+inm(i("15.1"),i))/2'                 &&56 P пе
DO CIKL WITH o("56.1"),'inm(i("15.2"),i)'                                      &&56.1 Pо
*O ALTERC WITH o("56.1"),5,'(inm(i("15"),i)+inm(i("15.1"),i))/2-F1(oum(o("50"),i),"2.41:1")'&&56.1 Pо
DO CIKL WITH o("57"),'503.43+11.02849*LOG((oum(o("51"),i)+273.15)/647.27)+'+;
'229.2569*(oum(o("51"),i)+273.15)/647.27+37.93129*((oum(o("51"),i)+273.15)/647.27)**2+'+;
'(0.758195-7.97826/((oum(o("51"),i)+273.15)/1000)**2-(3.078455*'+;
'(oum(o("51"),i)+273.15)/1000-.21549)/((oum(o("51"),i)+273.15)/1000-.21)**3)*'+;
'oum(o("56"),i)/100+(.0644126-.268671/((oum(o("51"),i)+273.15)/1000)**8-'+;
'.216661/100/((oum(o("51"),i)+273.15)/1000)**14)*(oum(o("56"),i)/100)**2'      &&57 i пе
DO CIKL WITH o("57.1"),'503.43+11.02849*LOG((oum(o("51.1"),i)+273.15)/647.27)+'+;
'229.2569*(oum(o("51.1"),i)+273.15)/647.27+37.93129*((oum(o("51.1"),i)+273.15)/647.27)**2+'+;
'(0.758195-7.97826/((oum(o("51.1"),i)+273.15)/1000)**2-(3.078455*'+;
'(oum(o("51.1"),i)+273.15)/1000-.21549)/((oum(o("51.1"),i)+273.15)/1000-.21)**3)*'+;
'oum(o("56.1"),i)/100+(.0644126-.268671/((oum(o("51.1"),i)+273.15)/1000)**8-'+;
'.216661/100/((oum(o("51.1"),i)+273.15)/1000)**14)*(oum(o("56.1"),i)/100)**2'  &&57.1 i оп
DO CIKL WITH o("58"),'(49.4+402.5*inm(i("26"),i)/100+4.767*(inm(i("26"),i)/100)**2+'+;
'.0333*(inm(i("26"),i)/100)**6+(-9.25+1.67*inm(i("26"),i)/100+.00736*'+;
'(inm(i("26"),i)/100)**6-.008*(1/(inm(i("26"),i)/100+.5))**5)*(50-inm(i("16"),i)*'+;
'.0980665)/10+(-.073+.079*inm(i("26"),i)/100+.00068*(inm(i("26"),i)/100)**6)*'+;
'((50-inm(i("16"),i)*.0980665)/10)**2+3.39/1E8*(inm(i("26"),i)/100)**12*'+;
'((50-inm(i("16"),i)*.0980665)/10)**4)/4.1868'                                 &&58 i пв
DO CIKL WITH o("59"),'503.43+11.02849*LOG((oum(o("52"),i)+273.15)/647.27)+'+;
'229.2569*(oum(o("52"),i)+273.15)/647.27+37.93129*((oum(o("52"),i)+273.15)/647.27)**2+'+;
'(0.758195-7.97826/((oum(o("52"),i)+273.15)/1000)**2-(3.078455*'+;
'(oum(o("52"),i)+273.15)/1000-.21549)/((oum(o("52"),i)+273.15)/1000-.21)**3)*'+;
'oum(o("53"),i)/100+(.0644126-.268671/((oum(o("52"),i)+273.15)/1000)**8-'+;
'.216661/100/((oum(o("52"),i)+273.15)/1000)**14)*(oum(o("53"),i)/100)**2'      &&59 i гпп к
DO CIKL WITH o("59.1"),'503.43+11.02849*LOG((oum(o("52.1"),i)+273.15)/647.27)+'+;
'229.2569*(oum(o("52.1"),i)+273.15)/647.27+37.93129*((oum(o("52.1"),i)+273.15)/647.27)**2+'+;
'(0.758195-7.97826/((oum(o("52.1"),i)+273.15)/1000)**2-(3.078455*'+;
'(oum(o("52.1"),i)+273.15)/1000-.21549)/((oum(o("52.1"),i)+273.15)/1000-.21)**3)*'+;
'oum(o("53"),i)/100+(.0644126-.268671/((oum(o("52.1"),i)+273.15)/1000)**8-'+;
'.216661/100/((oum(o("52.1"),i)+273.15)/1000)**14)*(oum(o("53"),i)/100)**2'    &&59 i гпп т
DO CIKL WITH o("60"),'503.43+11.02849*LOG((inm(i("24"),i)+273.15)/647.27)+'+;
'229.2569*(inm(i("24"),i)+273.15)/647.27+37.93129*((inm(i("24"),i)+273.15)/647.27)**2+'+;
'(0.758195-7.97826/((inm(i("24"),i)+273.15)/1000)**2-(3.078455*'+;
'(inm(i("24"),i)+273.15)/1000-.21549)/((inm(i("24"),i)+273.15)/1000-.21)**3)*'+;
'inm(i("23"),i)/100+(.0644126-.268671/((inm(i("24"),i)+273.15)/1000)**8-'+;
'.216661/100/((inm(i("24"),i)+273.15)/1000)**14)*(inm(i("23"),i)/100)**2'      &&60 i хпп
* 61-ый - в 2 приема:
DO CIKL WITH o("61"),'1/(2.6864264-.20096551*LOG(inm(i("41"),i))-2.16688/1E3*'+;
'LOG(inm(i("41"),i))**2-9.480808/1E5*LOG(inm(i("41"),i))**3+6.135062/1E6*'+;
'LOG(inm(i("41"),i))**4+3.6917245/1E6*LOG(inm(i("41"),i))**5)',.F.             &&61 i пр
DO CIKL WITH o("61"),'-753.317+6959.4093*oum(o("61"),i)-29257.981*oum(o("61"),i)**2+'+;
'71285.169*oum(o("61"),i)**3-86752.84*oum(o("61"),i)**4+42641.056*'+;
'oum(o("61"),i)**5'                                                            &&61 i пр
DO CIKL WITH o("62"),'IIF(inm(i("74"),i)=="1",1/0,'+;
'IIF(inm(i("74"),i)=="2" OR inm(i("74"),i)=="3",F2(oum(o("50"),i),oum(o("10.1"),i),"2.83:2"),'+;
'IIF(inm(i("74"),i)=="2а",F2(oum(o("50"),i),inm(i("38"),i),"2.83а:2"),1/0)))'  &&62 i т
* 63-ий - написанному в инструкции не верить
* P2 = F1(inm(i("41.1"),i),"2.50:1") - f(tвых цнд1) для бл.2,3,4,5
* для блока 1 - f((tвых цнд1 + tвых цнд2)/2)
*F BL5
*O CIKL WITH o("63"),'F1(1-inm(i("30"),i)/100,"2.91:1")'                       &&63 i 2
DO CIKL WITH o("63"),'F1(inm(i("30"),i)/98.067,"2.91:1")'                      &&63 i 2
*O ALTERC WITH o("63"),1,'F1(inm(i("90"),n_blokov+1)/735.6-inm(i("30"),i),'+;
*"2.91:1")'                                                                    &&63 i 2
*O ALTERC WITH o("63"),5,'F1(inm(i("90"),n_blokov+1)/735.6-inm(i("30"),i),'+;
*"2.91:1")'                                                                    &&63 i 2
DO ALTERC WITH o("63"),6,'F1(inm(i("30"),i),"2.91:1")'                         &&63 i 2
*LSE
*O CIKL WITH o("63"),'F1(F1(inm(i("41.1"),i),"2.50:1"),"2.91:1")'              &&63 i 2
*NDIF
*O ALTERC WITH o("63"),1,'F1(F1((inm(i("41.1"),i)+inm(i("41.2"),i))/2,'+;
*'"2.50:1"),"2.91:1")'                                                         &&63 i 2
IF BL5
DO CIKL WITH o("64"),'(oum(o("49"),i)*(oum(o("57"),i)-oum(o("58"),i))+oum(o("55"),i)*'+;
'(oum(o("59"),i)-oum(o("60"),i))+inm(i("25"),i)*.004*(oum(o("61"),i)-oum(o("58"),i)))/1000'&&64 Q к бр
ELSE
DO CIKL WITH o("64"),'(oum(o("49"),i)*(oum(o("57"),i)-oum(o("58"),i))+oum(o("55"),i)*'+;
'(oum(o("59"),i)-oum(o("60"),i))+inm(i("27"),i)*(oum(o("61"),i)-oum(o("58"),i)))/1000'&&64 Q к бр
ENDIF
oum(o("64"),n_blokov+1)=CIKL1(o("64"),SUM('oum(o("64"),'))                     &&64 Q к бр
IF BL1 AND RealTime && уникальный пересчет доли газа только для параметра '/1' в реальн.врем.
inm(i("59"),ASCAN(ai,1))=MIN(100,inm(i("59"),ASCAN(ai,1))*inm(i("58"),n_blokov+1)/1E6/;
oum(o("64"),ASCAN(ai,1))*100)
inm(i("59"),ASCAN(ai,1))=IIF(inm(i("59"),ASCAN(ai,1))>=95,100,inm(i("59"),ASCAN(ai,1)))
SELECT 1 && inblok.dbf
LOCATE FOR ALLTRIM(SUBSTR(order,2))=='59'
REPLACE inblok.blok1 WITH STR(inm(i("59"),ASCAN(ai,1)),10,2)
SELECT 3 && ftabl.dbf
ENDIF               && уникальный пересчет доли газа только для параметра '/1' в реальн.врем.
IF BL2 AND RealTime && уникальный пересчет доли газа только для параметра '/2' в реальн.врем.
inm(i("59"),ASCAN(ai,2))=MIN(100,inm(i("59"),ASCAN(ai,2))*inm(i("58"),n_blokov+1)/1E6/;
oum(o("64"),ASCAN(ai,2))*100)
inm(i("59"),ASCAN(ai,2))=IIF(inm(i("59"),ASCAN(ai,2))>=95,100,inm(i("59"),ASCAN(ai,2)))
SELECT 1 && inblok.dbf
LOCATE FOR ALLTRIM(SUBSTR(order,2))=='59'
REPLACE inblok.blok2 WITH STR(inm(i("59"),ASCAN(ai,2)),10,2)
SELECT 3 && ftabl.dbf
ENDIF               && уникальный пересчет доли газа только для параметра '/2' в реальн.врем.
DO CIKL WITH o("65"),'oum(o("64"),i)/oum(o("1"),i)'                            &&65 Q к бр
oum(o("65"),n_blokov+1)=CIKL1(o("65"),oum(o("64"),n_blokov+1)/oum(o("1"),n_blokov+1))&&65 Q к бр
DO CIKL WITH o("26"),'oum(o("4"),i)*(oum(o("57.1"),i)-oum(o("60"),i))/.7/860'  &&26 Э тф п
DO CIKL WITH o("27"),'IIF(oum(o("4"),i)=0,0,F1(oum(o("13"),i),"2.9в:1"))'      &&27 W п/тф
DO CIKL WITH o("28"),'oum(o("9"),i)-oum(o("25"),i)*oum(o("10"),i)/1E3-'+;
'oum(o("27"),i)*oum(o("11"),i)/1E3'                                            &&28 N кн (ном)
sum=0
FOR i=1 TO n_blokov
  sum=sum+oum(o("28"),i)*oum(o("1"),i)
ENDFOR
oum(o("28"),n_blokov+1)=CIKL1(o("28"),sum/oum(o("1"),n_blokov+1))              &&28 N кн (ном)
STORE 0 TO sum,sum1
FOR i=1 TO n_blokov
  sum=sum+oum(o("14"),i)
  sum1=sum1+inm(i("28"),i)
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
sum1=INT(IIF(BL1 OR BL2 OR BL3 OR BL4 OR BL5 OR BL6,n_blokov1,inm(i("89"),n_blokov+1)))
sum2=CEIL(IIF(BL1 OR BL2 OR BL3 OR BL4 OR BL5 OR BL6,n_blokov1,inm(i("89"),n_blokov+1)))
sum2=IIF(sum1=sum2,sum2+1,sum2)
o29_01=EVAL('o29_'+STR(sum1,1))
o29_02=EVAL('o29_'+STR(sum2,1))
oum(o("29"),n_blokov+1)=CIKL1(o("29"),o29_01*(sum2-;
IIF(BL1 OR BL2 OR BL3 OR BL4 OR BL5 OR BL6,n_blokov1,inm(i("89"),n_blokov+1)))+;
o29_02*(IIF(BL1 OR BL2 OR BL3 OR BL4 OR BL5 OR BL6,n_blokov1,inm(i("89"),n_blokov+1))-sum1)) &&29 N цн (н) гр
DO CIKL WITH o("30"),'F1(oum(o("13"),i),"2.95:1")/1E3'                         &&30 N кэн (н)
DO CIKL WITH o("31"),'0.29'                                                    &&31 N бл(н)т
IF BL1 OR BL2 OR BL3 OR BL4 OR BL5 OR BL6
oum(o("32"),n_blokov+1)=CIKL1(o("32"),F1(SUM('oum(o("14"),')*n_blokov1,"2.11:1"))&&32 N ст(н)т гр
ELSE
oum(o("32"),n_blokov+1)=CIKL1(o("32"),F1(SUM('oum(o("14"),'),"2.11:1"))        &&32 N ст(н)т гр
ENDIF
DO CIKL WITH o("35"),'5.19*inm(i("69"),i)'                                     &&35 Э т сн(пуск)
oum(o("35"),n_blokov+1)=CIKL1(o("35"),SUM('oum(o("35"),'))                     &&35 Э т сн(пуск)
IF BL1 OR BL2 OR BL3 OR BL4 OR BL5 OR BL6
DO CIKL WITH o("36"),'(1.03*(oum(o("30"),i)*oum(o("1"),i)+oum(o("31"),i)*oum(o("1"),i)+'+;
'(oum(o("29"),n_blokov+1)+oum(o("32"),n_blokov+1))*inm(i("70"),n_blokov+1)/'+;
'n_blokov1)+oum(o("35"),i))/oum(o("2"),i)*100'                                 &&36 Э т сн/(ном)
ELSE
DO CIKL WITH o("36"),'(1.03*(oum(o("30"),i)*oum(o("1"),i)+oum(o("31"),i)*oum(o("1"),i)+'+;
'(oum(o("29"),n_blokov+1)+oum(o("32"),n_blokov+1))*inm(i("70"),n_blokov+1)*oum(o("2"),i)/'+;
'oum(o("2"),n_blokov+1))+oum(o("35"),i))/oum(o("2"),i)*100'                    &&36 Э т сн/(ном)
ENDIF
STORE 0 TO sum,sum1,sum2
FOR i=1 TO n_blokov
  sum=sum+oum(o("30"),i)*oum(o("1"),i)
  sum1=sum1+oum(o("31"),i)*oum(o("1"),i)
  sum2=sum2+oum(o("35"),i)
ENDFOR
oum(o("36"),n_blokov+1)=CIKL1(o("36"),(1.03*(sum+sum1+(oum(o("29"),n_blokov+1)+oum(o("32"),n_blokov+1))*;
inm(i("70"),n_blokov+1))+sum2)/oum(o("2"),n_blokov+1)*100)                     &&36 Э т сн/(ном)
DO CIKL WITH o("37"),'oum(o("29"),n_blokov+1)*inm(i("70"),n_blokov+1)*oum(o("2"),i)/'+;
'oum(o("2"),n_blokov+1)/oum(o("28"),i)/oum(o("1"),i)*100'                      &&37 Э цн (ном) гр
oum(o("37"),n_blokov+1)=CIKL1(o("37"),oum(o("29"),n_blokov+1)*inm(i("70"),n_blokov+1)/;
oum(o("28"),n_blokov+1)/oum(o("1"),n_blokov+1)*100)                            &&37 Э цн (ном) гр
oum(o("38"),n_blokov+1)=CIKL1(o("38"),IIF(inm(i("43"),n_blokov+1)>10,0,;
F2(inm(i("43"),n_blokov+1),(inm(i("70"),n_blokov+1)*6-oum(o("1"),n_blokov+1))/;
(6*inm(i("70"),n_blokov+1)),"2.13:2")))                                        &&38 Q т.о (отопл)
oum(o("39"),n_blokov+1)=CIKL1(o("39"),IIF(inm(i("43"),n_blokov+1)>10,0,;
F2(inm(i("43"),n_blokov+1),oum(o("65"),n_blokov+1)/446.1,"2.13а:2")))          &&39 Q т.о (вент)
DO CIKL WITH o("40"),'15.4*inm(i("69"),i)'                                     &&40 Q т сн (пуск)
oum(o("40"),n_blokov+1)=CIKL1(o("40"),SUM('oum(o("40"),'))                     &&40 Q т сн (пуск)
IF BL1 OR BL2 OR BL3 OR BL4 OR BL5 OR BL6
DO CIKL WITH o("41"),'(oum(o("38"),n_blokov+1)+oum(o("39"),n_blokov+1))*'+;
'oum(o("1"),i)*1E5/n_blokov1/(oum(o("24"),i)*oum(o("2"),i))'                   &&41 q т сн (ном)
ELSE
DO CIKL WITH o("41"),'((oum(o("38"),n_blokov+1)+oum(o("39"),n_blokov+1))*'+;
'inm(i("70"),n_blokov+1)*oum(o("1"),i)/oum(o("1"),n_blokov+1)+oum(o("40"),i))*'+;
'1E5/(oum(o("24"),i)*oum(o("2"),i))'                                           &&41 q т сн (ном)
ENDIF
oum(o("41"),n_blokov+1)=CIKL1(o("41"),((oum(o("38"),n_blokov+1)+oum(o("39"),n_blokov+1))*;
inm(i("70"),n_blokov+1)+SUM('oum(o("40"),'))*1E5/(oum(o("24"),n_blokov+1)*;
oum(o("2"),n_blokov+1)))                                                       &&41 q т сн (ном)
DO CIKL WITH o("42"),'oum(o("24"),i)*(100+oum(o("41"),i))/(100-oum(o("36"),i))'&&42 q т н (ном)
oum(o("42"),n_blokov+1)=CIKL1(o("42"),oum(o("24"),n_blokov+1)*(100+oum(o("41"),n_blokov+1))/;
(100-oum(o("36"),n_blokov+1)))                                                 &&42 q т н (ном)
DO CIKL WITH o("43"),'(oum(o("60"),i)+(oum(o("59.1"),i)-oum(o("60"),i))-oum(o("63"),i))/'+;
'(oum(o("57.1"),i)+(oum(o("59.1"),i)-oum(o("60"),i))-oum(o("63"),i))*(1+.4*(oum(o("57.1"),i)-'+;
'oum(o("60"),i))/(oum(o("57.1"),i)+(oum(o("59.1"),i)-oum(o("60"),i))-oum(o("63"),i)))'&&43 k по
DO CIKL WITH o("44"),'IIF(inm(i("74"),i)=="1",0,((oum(o("62"),i)-oum(o("63"),i))/'+;
'(oum(o("57.1"),i)+(oum(o("59.1"),i)-oum(o("60"),i))-oum(o("63"),i)))*(1+.4*(oum(o("57.1"),i)+'+;
'(oum(o("59.1"),i)-oum(o("60"),i))-oum(o("62"),i))/(oum(o("57.1"),i)+(oum(o("59.1"),i)-'+;
'oum(o("60"),i))-oum(o("63"),i))))'                                            &&44 k то
DO CIKL WITH o("45"),'IIF(oum(o("3"),i)+oum(o("4"),i)=0,0,(oum(o("4"),i)*'+;
'(1-oum(o("43"),i))*oum(o("8"),i))/(oum(o("3"),i)+oum(o("4"),i)))'             &&45 dQ э по
oum(o("45"),n_blokov+1)=CIKL1(o("45"),SUM('oum(o("45"),'))                     &&45 dQ э по
DO CIKL WITH o("46"),'IIF(oum(o("3"),i)+oum(o("4"),i)=0,0,(oum(o("3"),i)*'+;
'(1-oum(o("44"),i))*oum(o("8"),i))/(oum(o("3"),i)+oum(o("4"),i)))'             &&46 dQ э то
oum(o("46"),n_blokov+1)=CIKL1(o("46"),SUM('oum(o("46"),'))                     &&46 dQ э то
DO CIKL WITH o("47"),'(oum(o("45"),i)+oum(o("46"),i))'                         &&47 dQ э
oum(o("47"),n_blokov+1)=CIKL1(o("47"),SUM('oum(o("47"),'))                     &&47 dQ э
DO CIKL WITH o("48"),'(oum(o("24"),i)*oum(o("2"),i)*(100+oum(o("41"),i))/1E5+oum(o("47"),i))/'+;
'(oum(o("24"),i)*oum(o("2"),i)*(100+oum(o("41"),i))/1E5)'                      &&48 k отр (т)
oum(o("48"),n_blokov+1)=CIKL1(o("48"),(oum(o("24"),n_blokov+1)*oum(o("2"),n_blokov+1)*;
(100+oum(o("41"),n_blokov+1))/1E5+SUM('oum(o("47"),'))/(oum(o("24"),n_blokov+1)*;
oum(o("2"),n_blokov+1)*(100+oum(o("41"),n_blokov+1))/1E5))                     &&48 k отр (т)
IF BL5
DO CIKL WITH o("66"),'oum(o("49"),i)+inm(i("25"),i)*.004'                      &&66 D пв
ELSE
DO CIKL WITH o("66"),'oum(o("49"),i)+inm(i("27"),i)'                           &&66 D пв
ENDIF
oum(o("66"),n_blokov+1)=CIKL1(o("66"),SUM('oum(o("66"),'))                     &&66 D пв
DO CIKL WITH o("66.1"),'oum(o("66"),i)/oum(o("1"),i)'                          &&66.1 D пв ср
oum(o("66.1"),n_blokov+1)=CIKL1(o("66.1"),SUM('oum(o("66.1"),'))               &&66.1 D пв ср
DO CIKL WITH o("67"),'F2(oum(o("65"),i),inm(i("59"),i),"2.65:2")'              &&67 alfa вэк (н)
DO ALTERC WITH o("67"),3,'F1(oum(o("65"),i),"2.65в:1")'                        &&67 alfa вэк (н)
DO ALTERC WITH o("67"),4,'F1(oum(o("65"),i),"2.65в:1")'                        &&67 alfa вэк (н)
*F BL5
*LSE
DO ALTERC WITH o("67"),5,'F1(oum(o("65"),i),"2.63:1")'                         &&67 alfa вэк (н)
DO ALTERC WITH o("67"),6,'F1(oum(o("65"),i),"2.63:1(6)")'                      &&67 alfa вэк (н)
*NDIF
DO CIKL WITH o("68"),'F2(oum(o("65"),i),inm(i("59"),i),"2.82:2")'              &&68 dalfa ух (н)
*O ALTERC WITH o("68"),3,'F1(oum(o("65"),i),"2.82а:1")'                        &&68 dalfa ух (н)
*O ALTERC WITH o("68"),4,'F1(oum(o("65"),i),"2.82а:1")'                        &&68 dalfa ух (н)
*F BL5
*LSE
DO ALTERC WITH o("68"),5,'F1(oum(o("65"),i),"2.80:1")'                         &&68 dalfa ух (н)
DO ALTERC WITH o("68"),6,'F1(oum(o("65"),i),"2.80:1(6)")'                      &&68 dalfa ух (н)
*NDIF
DO CIKL WITH o("69"),'oum(o("67"),i)+oum(o("68"),i)'                           &&69 alfa ух (н)
DO CIKL WITH o("70"),'F2(oum(o("65"),i),inm(i("59"),i),"2.96:2")'              &&70 q 4 исх
*O ALTERC WITH o("70"),3,'F1(oum(o("65"),i),"2.96а:1")'                        &&70 q 4 исх
*O ALTERC WITH o("70"),4,'F1(oum(o("65"),i),"2.96а:1")'                        &&70 q 4 исх
*F BL5
DO ALTERC WITH o("70"),5,'0.912'                                               &&70 q 4 исх
DO ALTERC WITH o("70"),6,'F1(oum(o("65"),i),"2.96:1(6)")'                      &&70 q 4 исх
*LSE
*O ALTERC WITH o("70"),5,'F1(oum(o("65"),i),"2.97:1")'                         &&70 q 4 исх
*NDIF
DO CIKL WITH o("71"),;
'.064*(inm(i("55"),n_blokov+1)-14.3)*(100-inm(i("59"),i))/100'                 &&71 dq 4 (Ар)
*DO CIKL WITH o("71"),'IIF(inm(i("59"),i)<=50,'+;
*'(.063*(inm(i("55"),n_blokov+1)-14.2)*(50-inm(i("59"),i))+.022*(inm(i("55"),n_blokov+1)-15)*inm(i("59"),i))/50,'+;
*'IIF(inm(i("59"),i)>50,.022*(inm(i("55"),n_blokov+1)-15)*(100-inm(i("59"),i))/50,1/0))'&&71 dq 4 (Ар)
DO ALTERC WITH o("71"),5,'.075*(inm(i("55"),n_blokov+1)-14.3)'                 &&71 dq 4 (Ар)
DO ALTERC WITH o("71"),6,'.085*(inm(i("55"),n_blokov+1)-14.3)'                 &&71 dq 4 (Ар)
DO CIKL WITH o("72"),;
'.01*(inm(i("54"),n_blokov+1)-13.4)*(100-inm(i("59"),i))/100'                  &&72 dq 4 (Wp)
*O CIKL WITH o("72"),'IIF(inm(i("59"),i)<=50,'+;
*(.009*(inm(i("54"),n_blokov+1)-12.6)*(50-inm(i("59"),i))+.003*(inm(i("54"),n_blokov+1)-9.1)*inm(i("59"),i))/50,'+;
*IIF(inm(i("59"),i)>50,.003*(inm(i("54"),n_blokov+1)-9.1)*(100-inm(i("59"),i))/50,1/0))'&&72 dq 4 (Wp)
DO ALTERC WITH o("72"),5,'.012*(inm(i("54"),n_blokov+1)-13.4)'                 &&72 dq 4 (Wp)
DO ALTERC WITH o("72"),6,'.013*(inm(i("54"),n_blokov+1)-13.4)'                 &&72 dq 4 (Wp)
DO CIKL WITH o("73"),'oum(o("70"),i)+oum(o("71"),i)+oum(o("72"),i)'            &&73 q 4 (н)
sum=0
FOR i=1 TO n_blokov
  sum=sum+oum(o("73"),i)*oum(o("64"),i)
ENDFOR
oum(o("73"),n_blokov+1)=CIKL1(o("73"),sum/oum(o("64"),n_blokov+1))             &&73 q 4 (н)
DO CIKL WITH o("74"),'IIF(inm(i("74"),i)=="1",F1(oum(o("50"),i),"2.20:1"),'+;
'IIF(inm(i("74"),i)=="2" OR inm(i("74"),i)=="2а" OR inm(i("74"),i)=="3",F1(oum(o("50"),i),"2.20а:1"),'+;
'1/0))'                                                                        &&74 t пв (н)
sum=0
FOR i=1 TO n_blokov
  sum=sum+oum(o("74"),i)*oum(o("66"),i)
ENDFOR
oum(o("74"),n_blokov+1)=CIKL1(o("74"),sum/oum(o("66"),n_blokov+1))             &&74 t пв (н)
DO ALTERC WITH o("75"),1,'F2(oum(o("65"),i),inm(i("59"),i),"2.23:2")'          &&75 t ух исх
DO ALTERC WITH o("75"),2,'F2(oum(o("65"),i),inm(i("59"),i),"2.23:2")'          &&75 t ух исх
DO ALTERC WITH o("75"),3,'F1(oum(o("65"),i),"2.23а:1")'                        &&75 t ух исх
DO ALTERC WITH o("75"),4,'F1(oum(o("65"),i),"2.23а:1")'                        &&75 t ух исх
DO ALTERC WITH o("75"),5,'F1(oum(o("65"),i),"2.21:1")'                         &&75 t ух исх
DO ALTERC WITH o("75"),6,'F1(oum(o("65"),i),"2.21:1(6)")'                      &&75 t ух исх
DO CIKL WITH o("76"),'.2*(inm(i("26"),i)-oum(o("74"),i))'                      &&76 dt ух (t пв)
DO CIKL WITH o("77"),'.50*((inm(i("32"),i)+inm(i("32.1"),i))/2-30)'            &&77 dt ух (t вп)
DO CIKL WITH o("78"),'-.3*((inm(i("32"),i)+inm(i("32.1"),i))/2-'+;
'(inm(i("31"),i)+inm(i("31.1"),i))/2)'                                         &&78 dt ух (t рец)
DO CIKL WITH o("79"),'oum(o("75"),i)+oum(o("76"),i)+oum(o("77"),i)+oum(o("78"),i)'&&79 t ух (н)
sum=0
FOR i=1 TO n_blokov
  sum=sum+oum(o("79"),i)*oum(o("64"),i)
ENDFOR
oum(o("79"),n_blokov+1)=CIKL1(o("79"),sum/oum(o("64"),n_blokov+1))             &&79 t ух (н)
DO CIKL WITH o("80"),'(3.5+.02*1E3*inm(i("54"),n_blokov+1)/inm(i("53"),n_blokov+1))*'+;
'(100-inm(i("59"),i))/1E2+3.53*inm(i("59"),i)/1E2'                             &&80 k
DO CIKL WITH o("81"),'(.4+.04*1E3*inm(i("54"),n_blokov+1)/inm(i("53"),n_blokov+1))*'+;
'(100-inm(i("59"),i))/1E2+.6*inm(i("59"),i)/1E2'                               &&81 c
DO CIKL WITH o("82"),'.14*(100-inm(i("59"),i))/1E2+.18*inm(i("59"),i)/1E2'     &&82 b
DO CIKL WITH o("83"),'(oum(o("80"),i)*oum(o("69"),i)+oum(o("81"),i))*'+;
'(oum(o("79"),i)-oum(o("69"),i)*(inm(i("31"),i)+inm(i("31.1"),i))/2/'+;
'(oum(o("69"),i)+oum(o("82"),i)))*(.9805+.00013*oum(o("79"),i))*(1-.01*oum(o("73"),i))/1E2+'+;
'.2*.95*inm(i("55"),n_blokov+1)*(100-inm(i("59"),i))/1E2*oum(o("79"),i)/inm(i("53"),n_blokov+1)'&&83 q 2 (н)
sum=0
FOR i=1 TO n_blokov
  sum=sum+oum(o("83"),i)*oum(o("64"),i)
ENDFOR
oum(o("83"),n_blokov+1)=CIKL1(o("83"),sum/oum(o("64"),n_blokov+1))             &&83 q 2 (н)
DO CIKL WITH o("84"),'F2(oum(o("65"),i),inm(i("59"),i),"2.98:2")'              &&84 q 5 (н)
DO ALTERC WITH o("84"),5,'F1(oum(o("65"),i),"2.99:1")'                         &&84 q 5 (н)
DO ALTERC WITH o("84"),6,'F1(oum(o("65"),i),"2.99:1(6)")'                      &&84 q 5 (н)
DO CIKL WITH o("85"),'(100-inm(i("59"),i))*0.02/100'                           &&85 q 6 (н)
DO ALTERC WITH o("86"),1,'IIF(inm(i("72"),i)>35000,.0055*(inm(i("72"),i)-35000)/'+;
'1E3,0)'                                                                       &&86 q рес (н)
DO ALTERC WITH o("86"),2,'IIF(inm(i("72"),i)>35000,.0055*(inm(i("72"),i)-35000)/'+;
'1E3,0)'                                                                       &&86 q рес (н)
DO ALTERC WITH o("86"),3,'IIF(inm(i("72"),i)>35000,.0055*(inm(i("72"),i)-35000)/'+;
'1E3,0)'                                                                       &&86 q рес (н)
DO ALTERC WITH o("86"),4,'IIF(inm(i("72"),i)>35000,.0055*(inm(i("72"),i)-35000)/'+;
'1E3,0)'                                                                       &&86 q рес (н)
DO ALTERC WITH o("86"),5,'IIF(inm(i("72"),i)>35000,.0055*(inm(i("72"),i)-35000)/'+;
'1E3,0)'                                                                       &&86 q рес (н)
DO ALTERC WITH o("86"),6,'IIF(inm(i("72"),i)>35000,.0055*(inm(i("72"),i)-35000)/'+;
'1E3,0)'                                                                       &&86 q рес (н)
DO CIKL WITH o("87"),'inm(i("69"),i)*64.2*7*1E2/(oum(o("64"),i)*1E2/(100-oum(o("73"),i)-'+;
'oum(o("83"),i)-oum(o("84"),i)-oum(o("85"),i)-oum(o("86"),i))+64.2*7)'         &&87 q пуск (н)
DO CIKL WITH o("88"),'100-oum(o("83"),i)-oum(o("73"),i)-oum(o("84"),i)-oum(o("85"),i)-'+;
'oum(o("86"),i)-oum(o("87"),i)'                                                &&88 КПД к бр (ном)
sum=0
FOR i=1 TO n_blokov
  sum=sum+oum(o("88"),i)*oum(o("64"),i)
ENDFOR
oum(o("88"),n_blokov+1)=CIKL1(o("88"),sum/oum(o("64"),n_blokov+1))             &&88 КПД к бр (ном)
DO CIKL WITH o("89"),'100-inm(i("59"),i)'                                      &&89 alfa уг
DO CIKL WITH o("90"),'oum(o("64"),i)*oum(o("89"),i)*1E3/oum(o("88"),i)/'+;
'inm(i("53"),n_blokov+1)'                                                      &&90 B нат(ном)
oum(o("90"),n_blokov+1)=CIKL1(o("90"),SUM('oum(o("90"),'))                     &&90 B нат(ном)
DO CIKL WITH o("91"),'F2(oum(o("65"),i),inm(i("59"),i),"2.27:2")'              &&91 Э тд(н) исх
DO ALTERC WITH o("91"),3,'F1(oum(o("65"),i),"2.27в:1")'                         &&91 Э тд(н) исх
DO ALTERC WITH o("91"),4,'F1(oum(o("65"),i),"2.27в:1")'                         &&91 Э тд(н) исх
*F BL5
*LSE
DO ALTERC WITH o("91"),5,'F1(oum(o("65"),i),"2.25:1")'                         &&91 Э тд(н) исх
DO ALTERC WITH o("91"),6,'F1(oum(o("65"),i),"2.25:1(6)")'                      &&91 Э тд(н) исх
*NDIF
DO CIKL WITH o("92"),;
'.041*(inm(i("54"),n_blokov+1)-13.4)*(100-inm(i("59"),i))/100'                 &&92 dЭ тд (Wp)
DO ALTERC WITH o("92"),5,'.04*(inm(i("54"),n_blokov+1)-13.4)'                  &&92 dЭ тд (Wp)
DO ALTERC WITH o("92"),6,'.04*(inm(i("54"),n_blokov+1)-13.4)'                  &&92 dЭ тд (Wp)
DO CIKL WITH o("93"),;
'.004*((inm(i("32"),i)+inm(i("32.1"),i))/2-30)*(100-inm(i("59"),i))/100'                 &&93 dЭ тд (t вп)
DO CIKL WITH o("94"),'1/0'                                                     &&94 dЭ тд (t рец)
*DO CIKL WITH o("94"),'.008*((inm(i("32"),i)+inm(i("32.1"),i))/2-'+;
*'(inm(i("31"),i)+inm(i("31.1"),i))/2)'                                        &&94 dЭ тд (t рец)
DO CIKL WITH o("95"),'oum(o("91"),i)+oum(o("92"),i)'                           &&95 Э тд (ном)
sum=0
FOR i=1 TO n_blokov
  sum=sum+oum(o("95"),i)*oum(o("64"),i)
ENDFOR
oum(o("95"),n_blokov+1)=CIKL1(o("95"),sum/oum(o("64"),n_blokov+1))             &&95 Э тд (ном)
DO CIKL WITH o("96"),'IIF(inm(i("59"),i)=100,0,F2(oum(o("65"),i),inm(i("59"),i),"2.26:2"))'&&96 Э пп (исх)
DO ALTERC WITH o("96"),3,'F2(oum(o("65"),i),0,"2.26:2")'                       &&96 Э пп (исх)
DO ALTERC WITH o("96"),4,'F2(oum(o("65"),i),0,"2.26:2")'                       &&96 Э пп (исх)
*O ALTERC WITH o("96"),5,'F2(oum(o("65"),i),0,"2.26:2")'                       &&96 Э пп (исх)
*F BL5
*O ALTERC WITH o("96"),5,'F2(oum(o("65"),i),0,"2.26:2")'                       &&96 Э пп (исх)
*LSE
DO ALTERC WITH o("96"),5,'F1(oum(o("65"),i),"2.26а:1")'                        &&96 Э пп (исх)
DO ALTERC WITH o("96"),6,'F1(oum(o("65"),i),"2.26:1(6)")'                      &&96 Э пп (исх)
*NDIF
DO CIKL WITH o("97"),;
'.297*(inm(i("54"),n_blokov+1)-13.4)*(100-inm(i("59"),i))/100'                  &&97 dЭ пп (исх)
DO ALTERC WITH o("97"),5,'.297*(inm(i("54"),n_blokov+1)-13.4)'                 &&97 dЭ пп (исх)
DO ALTERC WITH o("97"),6,'.297*(inm(i("54"),n_blokov+1)-13.4)'                 &&97 dЭ пп (исх)
DO CIKL WITH o("98"),'oum(o("96"),i)+oum(o("97"),i)'                           &&98 Э пп (н)
sum=0
FOR i=1 TO n_blokov
  sum=sum+oum(o("98"),i)*oum(o("90"),i)
ENDFOR
oum(o("98"),n_blokov+1)=CIKL1(o("98"),sum/oum(o("90"),n_blokov+1))             &&98 Э пп (н)
DO CIKL WITH o("99"),'F1(oum(o("66.1"),i),"2.29:1")'                       &&99 Э пэн (н)
*O ALTERC WITH o("99"),1,'(F1(oum(o("66.1"),i),"2.29:1")+F1(oum(o("66.1"),i),"2.30:1"))/'+;
*2*1.1'                                                                        &&99 Э пэн (н)
*O ALTERC WITH o("99"),2,'(F1(oum(o("66.1"),i),"2.29:1")+F1(oum(o("66.1"),i),"2.30:1"))/'+;
*2*1.1'                                                                        &&99 Э пэн (н)
sum=0
FOR i=1 TO n_blokov
  sum=sum+oum(o("99"),i)*oum(o("66"),i)
ENDFOR
oum(o("99"),n_blokov+1)=CIKL1(o("99"),sum/oum(o("66"),n_blokov+1))             &&99 Э пэн (н)
DO CIKL WITH o("100"),'IIF(inm(i("59"),i)=100,0,IIF(inm(i("43"),n_blokov+1)>=0,F1(inm(i("43"),n_blokov+1),"2.31:1")'+;
',F1(inm(i("43"),n_blokov+1),"2.31а:1")))'                                     &&100 Э тп (н)
oum(o("101"),n_blokov+1)=CIKL1(o("101"),IIF(inm(i("43"),n_blokov+1)>=0,F1(inm(i("43"),n_blokov+1),"2.32:1"),;
F1(inm(i("43"),n_blokov+1),"2.32а:1")))                                        &&101 Э разг (н)
IF BL1 OR BL2 OR BL3 OR BL4 OR BL5 OR BL6
oum(o("102"),n_blokov+1)=CIKL1(o("102"),F1(n_blokov1,"2.33:1"))               &&102 N зшу (н)
ELSE
oum(o("102"),n_blokov+1)=CIKL1(o("102"),F1(inm(i("89"),n_blokov+1),;
"2.33:1"))                                                                     &&102 N зшу (н)
ENDIF
oum(o("103"),n_blokov+1)=CIKL1(o("103"),F1((inm(i("78"),n_blokov+1)+inm(i("79"),n_blokov+1))/;
inm(i("70"),n_blokov+1),"2.34:1"))                                             &&103 N ов (н)
oum(o("104"),n_blokov+1)=CIKL1(o("104"),IIF(inm(i("92"),n_blokov+1)<=0,208,F1(inm(i("92"),n_blokov+1),;
"2.34а:1")))                                                                   &&104 N маз (н)
IF BL1 OR BL2 OR BL3 OR BL4 OR BL5 OR BL6
oum(o("105"),n_blokov+1)=CIKL1(o("105"),F1(SUM('oum(o("65"),')*n_blokov1,"2.51:1"))&&105 N доп.пр (н)
ELSE
oum(o("105"),n_blokov+1)=CIKL1(o("105"),F1(oum(o("64"),n_blokov+1)/inm(i("70"),n_blokov+1),;
"2.51:1"))                                                                     &&105 N доп.пр (н)
ENDIF
IF BL1 OR BL2 OR BL3 OR BL4 OR BL5 OR BL6
oum(o("106"),n_blokov+1)=CIKL1(o("106"),(oum(o("102"),n_blokov+1)+oum(o("103"),n_blokov+1)+;
oum(o("104"),n_blokov+1)+oum(o("105"),n_blokov+1))*inm(i("70"),n_blokov+1)/1E3/n_blokov1+;
oum(o("101"),n_blokov+1)*inm(i("88"),n_blokov+1)/1E3)                          &&106 N ов (н)
ELSE
oum(o("106"),n_blokov+1)=CIKL1(o("106"),(oum(o("102"),n_blokov+1)+oum(o("103"),n_blokov+1)+;
oum(o("104"),n_blokov+1)+oum(o("105"),n_blokov+1))*inm(i("70"),n_blokov+1)/1E3+;
oum(o("101"),n_blokov+1)*inm(i("88"),n_blokov+1)/1E3)                          &&106 N ов (н)
ENDIF
DO CIKL WITH o("107"),'6.19*inm(i("69"),i)'                                    &&107 Э пуск (н)
oum(o("107"),n_blokov+1)=CIKL1(o("107"),SUM('oum(o("107"),'))                  &&107 Э пуск (н)
DO CIKL WITH o("108"),'1.03*(oum(o("95"),i)*oum(o("64"),i)+oum(o("98"),i)*oum(o("90"),i)+'+;
'oum(o("99"),i)*oum(o("66"),i)+oum(o("100"),i)*oum(o("90"),i))/1E3+1.03*'+;
'oum(o("106"),n_blokov+1)*oum(o("64"),i)/oum(o("64"),n_blokov+1)+oum(o("107"),i)'&&108 Э к сн (н)
oum(o("108"),n_blokov+1)=CIKL1(o("108"),SUM('oum(o("108"),'))                  &&108 Э к сн (н)
oum(o("109"),n_blokov+1)=CIKL1(o("109"),IIF(inm(i("43"),n_blokov+1)>10,0,;
F2(inm(i("43"),n_blokov+1),(6*inm(i("70"),n_blokov+1)-;
oum(o("1"),n_blokov+1))/(6*inm(i("70"),n_blokov+1)),"2.93:2")))                &&109 Q от к (н)
oum(o("110"),n_blokov+1)=CIKL1(o("110"),IIF(inm(i("43"),n_blokov+1)>10,0,;
F2(inm(i("43"),n_blokov+1),oum(o("65"),n_blokov+1)/446.1,"2.94:2")))             &&110 Q от к (н)
oum(o("111"),n_blokov+1)=CIKL1(o("111"),IIF(inm(i("43"),n_blokov+1)>10,0,;
F1(inm(i("43"),n_blokov+1),"2.12:1")))                                         &&111 Q от IIк (н)
oum(o("112"),n_blokov+1)=CIKL1(o("112"),IIF(inm(i("43"),n_blokov+1)>10,0,;
F1(inm(i("43"),n_blokov+1),"2.14:1")))                                         &&112 Q пвк (н)
oum(o("113"),n_blokov+1)=CIKL1(o("113"),;
F2((inm(i("78"),n_blokov+1)+inm(i("79"),n_blokov+1))/inm(i("70"),n_blokov+1),inm(i("91"),n_blokov+1),"2.15:2"))  &&113 Q об.в (н)
oum(o("114"),n_blokov+1)=CIKL1(o("114"),IIF(inm(i("43"),n_blokov+1)>=0,0,;
F1(inm(i("43"),n_blokov+1),"2.16:1")))                                         &&114 Q разм (н)
oum(o("115"),n_blokov+1)=CIKL1(o("115"),F1(inm(i("43"),n_blokov+1),"2.28:1"))  &&115 Q мх (н)
oum(o("116"),n_blokov+1)=CIKL1(o("116"),IIF(inm(i("61"),n_blokov+1)=0,0,;
F1(inm(i("43"),n_blokov+1),"2.52:1")))                                         &&116 Q маз сл (н)
*oum(o("117"),n_blokov+1)=CIKL1(o("117"),IIF(inm(i("43"),n_blokov+1)>0,0,;
F2(inm(i("62"),n_blokov+1),inm(i("43"),n_blokov+1),"2.56:2")))                 &&117 Q пр.сл (н)
oum(o("117"),n_blokov+1)=CIKL1(o("117"),IIF(.T.,0,;
F2(inm(i("62"),n_blokov+1),inm(i("43"),n_blokov+1),"2.56:2")))                 &&117 Q пр.сл (н)
oum(o("118"),n_blokov+1)=CIKL1(o("118"),F1(SUM('oum(o("65"),'),"2.60:1"))      &&118 Q пр (н)
DO CIKL WITH o("119"),'15.4*inm(i("69"),i)'                                    &&119 Q пуск (н)
oum(o("119"),n_blokov+1)=CIKL1(o("119"),SUM('oum(o("119"),'))                  &&119 Q пуск (н)
oum(o("120"),n_blokov+1)=CIKL1(o("120"),(oum(o("109"),n_blokov+1)+oum(o("110"),n_blokov+1)+;
oum(o("111"),n_blokov+1)+oum(o("112"),n_blokov+1)+oum(o("113"),n_blokov+1)+;
oum(o("114"),n_blokov+1)+oum(o("115"),n_blokov+1)+oum(o("118"),n_blokov+1))*;
inm(i("70"),n_blokov+1)+oum(o("116"),n_blokov+1)*inm(i("61"),n_blokov+1)+oum(o("117"),n_blokov+1)+;
oum(o("119"),n_blokov+1))                                                      &&120 Q к сн (н) гр
sum=SUM('oum(o("119"),')
IF BL1 OR BL2 OR BL3 OR BL4 OR BL5 OR BL6
DO CIKL WITH o("121"),'oum(o("120"),n_blokov+1)/n_blokov1*1E2/oum(o("64"),i)'  &&121 q к сн (н)
ELSE
DO CIKL WITH o("121"),'IIF(inm(i("74"),i)=="1",((oum(o("120"),n_blokov+1)-oum(o("119"),n_blokov+1))*'+;
'oum(o("64"),i)/oum(o("64"),n_blokov+1)+'+;
'oum(o("119"),i))*1E2/oum(o("64"),i),((oum(o("120"),n_blokov+1)-oum(o("119"),n_blokov+1))*'+;
'(oum(o("3"),i)+oum(o("4"),i))/(oum(o("3"),n_blokov+1)+oum(o("4"),n_blokov+1))+'+;
'oum(o("119"),i))*1E2/oum(o("64"),i))'                                          &&121 q к сн (н)
ENDIF
oum(o("121"),n_blokov+1)=CIKL1(o("121"),oum(o("120"),n_blokov+1)*1E2/oum(o("64"),n_blokov+1))&&121 q к сн (н)
DO CIKL WITH o("122"),'F1(inm(i("43"),n_blokov+1),"2.17:1")'                   &&122 Q псг (н)
oum(o("123"),n_blokov+1)=CIKL1(o("123"),F1(inm(i("43"),n_blokov+1),"2.18:1"))  &&123 Q труб (н)
oum(o("124"),n_blokov+1)=CIKL1(o("124"),;
F2((inm(i("50"),n_blokov+1)+inm(i("51"),n_blokov+1))*1E3/inm(i("70"),n_blokov+1),inm(i("91"),n_blokov+1),"2.19:2"))&&124 Q птс (н)
IF BL1 OR BL2 OR BL3 OR BL4 OR BL5 OR BL6
DO CIKL WITH o("125"),'IIF(oum(o("8"),i)=0,0,((oum(o("123"),n_blokov+1)*inm(i("71"),n_blokov+1)+'+;
'oum(o("124"),n_blokov+1)*inm(i("70"),n_blokov+1)+393*(inm(i("50"),n_blokov+1)+'+;
'inm(i("51"),n_blokov+1))/1E3)/n_blokov1+oum(o("122"),i)*inm(i("71"),i))/oum(o("8"),i)*100)'&&125 alfa пот (н)
ELSE
DO CIKL WITH o("125"),'IIF(oum(o("8"),i)=0,0,((oum(o("123"),n_blokov+1)*inm(i("71"),n_blokov+1)+'+;
'(oum(o("124"),n_blokov+1)+IIF(inm(i("85"),n_blokov+1)=0,F1(inm(i("43"),n_blokov+1),"2.12б:1")+oum(o("112"),n_blokov+1),0))*'+;
'inm(i("70"),n_blokov+1)+393*(inm(i("50"),n_blokov+1)+'+;
'inm(i("51"),n_blokov+1))/1E3)*oum(o("8"),i)/'+;
'oum(o("8"),n_blokov+1)+oum(o("122"),i)*inm(i("71"),i))/oum(o("8"),i)*100)'    &&125 alfa пот (н)
ENDIF
sum=0
FOR i=1 TO n_blokov
  sum=sum+oum(o("122"),i)*inm(i("71"),i)
ENDFOR
oum(o("125"),n_blokov+1)=CIKL1(o("125"),IIF(oum(o("8"),n_blokov+1)=0,0,;
(oum(o("123"),n_blokov+1)*inm(i("71"),n_blokov+1)+;
(oum(o("124"),n_blokov+1)+IIF(inm(i("85"),n_blokov+1)=0,F1(inm(i("43"),n_blokov+1),"2.12б:1")+oum(o("112"),n_blokov+1),0))*;
inm(i("70"),n_blokov+1)+393*(inm(i("50"),n_blokov+1)+;
inm(i("51"),n_blokov+1))/1E3+sum)/oum(o("8"),n_blokov+1)*100))                 &&125 alfa пот (н)
DO CIKL WITH o("138"),'IIF(inm(i("71"),i)=0,0,inm(i("47"),i)/inm(i("71"),i)*1E3)'&&138 G св
sum=0
FOR i=1 TO n_blokov
  sum=sum+oum(o("138"),i)*oum(o("1"),i)
ENDFOR
oum(o("138"),n_blokov+1)=CIKL1(o("138"),sum/oum(o("1"),n_blokov+1))            &&138 G св
*O CIKL WITH o("139"),'IIF(inm(i("10.1"),i)/inm(i("1"),i)>1,F1(oum(o("138"),i),"2.39:1"),'+;
*'F1(oum(o("138"),i),"2.39а:1"))'                                               &&139 N сет
*DO CIKL WITH o("139"),'IIF(inm(i("10.1"),i)/inm(i("1"),i)<=1,F1(oum(o("138"),i),"2.39б:1"),'+;
*'IIF(oum(o("138"),i)<=4500,F1(oum(o("138"),i),"2.39в:1"),'+;
*'F1(oum(o("138"),i),"2.39е:1")))'                                              &&139 N сет
*DO ALTERC WITH o("139"),1,'IIF(inm(i("10.1"),i)/inm(i("1"),i)<=1,F1(oum(o("138"),i),"2.39б:1"),'+;
*'IIF(oum(o("138"),i)<=4500,F1(oum(o("138"),i),"2.39в:1"),'+;
*'F1(oum(o("138"),i),"2.39г:1")))'                                              &&139 N сет
DO CIKL WITH o("139"),'IIF(oum(o("138"),i)<=4500,'+;
'(F1(oum(o("138"),i),"2.39б:1")*(1.4-MIN(1.4,inm(i("10.1"),i)/inm(i("1"),i)))+'+;
'F1(oum(o("138"),i),"2.39в:1")*MIN(1.4,inm(i("10.1"),i)/inm(i("1"),i)))/1.4,'+;
'F1(oum(o("138"),i),"2.39е:1"))'                                               &&139 N сет
DO ALTERC WITH o("139"),1,'IIF(oum(o("138"),i)<=4500,'+;
'(F1(oum(o("138"),i),"2.39б:1")*(1.4-MIN(1.4,inm(i("10.1"),i)/inm(i("1"),i)))+'+;
'F1(oum(o("138"),i),"2.39в:1")*MIN(1.4,inm(i("10.1"),i)/inm(i("1"),i)))/1.4,'+;
'F1(oum(o("138"),i),"2.39г:1"))'                                               &&139 N сет
DO CIKL WITH o("126"),'1/1E3*860*oum(o("139"),i)*inm(i("71"),i)*85/1E2'        &&126 Q нас (н)
oum(o("126"),n_blokov+1)=CIKL1(o("126"),SUM('oum(o("126"),'))                  &&126 Q нас (н)
DO CIKL WITH o("126.1"),'IIF(oum(o("8"),i)=0,0,oum(o("126"),i)/oum(o("8"),i)*1E2)'&&126.1 alfa нас (н)
oum(o("126.1"),n_blokov+1)=CIKL1(o("126.1"),IIF(oum(o("8"),n_blokov+1)=0,0,;
oum(o("126"),n_blokov+1)/oum(o("8"),n_blokov+1)*1E2))                          &&126.1 alfa нас (н)
DO CIKL WITH o("127"),'IIF(inm(i("74"),i)=="1",1,(oum(o("24"),i)*(100+oum(o("41"),i))*'+;
'oum(o("2"),i)/1E5+oum(o("47"),i))/'+;
'(oum(o("24"),i)*(100+oum(o("41"),i))*oum(o("2"),i)/1E5+oum(o("47"),i)+(oum(o("8"),i)-'+;
'oum(o("126"),i))*(100+oum(o("125"),i))/1E2))'                                 &&127 К э
oum(o("127"),n_blokov+1)=CIKL1(o("127"),IIF(SUM('inm(i("47"),')=0,1,(oum(o("24"),n_blokov+1)*;
(100+oum(o("41"),n_blokov+1))*oum(o("2"),n_blokov+1)/1E5+oum(o("47"),n_blokov+1))/;
(oum(o("24"),n_blokov+1)*(100+oum(o("41"),n_blokov+1))*oum(o("2"),n_blokov+1)/1E5+;
oum(o("47"),n_blokov+1)+(oum(o("8"),n_blokov+1)-oum(o("126"),n_blokov+1))*;
(100+oum(o("125"),n_blokov+1))/1E2)))                                          &&127 К э
DO CIKL WITH o("128"),'oum(o("36"),i)+oum(o("127"),i)*oum(o("108"),i)/oum(o("2"),i)*100'&&128 Э э сн (н)
oum(o("128"),n_blokov+1)=CIKL1(o("128"),oum(o("36"),n_blokov+1)+oum(o("127"),n_blokov+1)*;
oum(o("108"),n_blokov+1)/oum(o("2"),n_blokov+1)*100)                           &&128 Э э сн (н)
DO CIKL WITH o("129"),'oum(o("88"),i)*(100-oum(o("121"),i))*(100-oum(o("128"),i))/'+;
'100/(100-oum(o("36"),i))'                                                     &&129 КПД к н (ном)
oum(o("129"),n_blokov+1)=CIKL1(o("129"),oum(o("88"),n_blokov+1)*(100-oum(o("121"),n_blokov+1))*;
(100-oum(o("128"),n_blokov+1))/100/(100-oum(o("36"),n_blokov+1)))              &&129 КПД к н (ном)
DO CIKL WITH o("130"),'100-1.0*472.2/oum(o("65"),i)'                           &&130 НЮ тп
oum(o("130"),n_blokov+1)=CIKL1(o("130"),100-1.0*472.2/oum(o("65"),n_blokov+1))      &&130 НЮ тп
DO CIKL WITH o("131"),'oum(o("65"),i)/472.2'                              &&131 К з
oum(o("131"),n_blokov+1)=CIKL1(o("131"),oum(o("65"),n_blokov+1)/472.2)    &&131 К з
DO CIKL WITH o("132"),'IIF(F1(oum(o("131"),i),"2.35:1")>1,1,F1(oum(o("131"),i),"2.35:1"))'&&132 К ст
sum=0
FOR i=1 TO n_blokov
  sum=sum+oum(o("132"),i)*oum(o("64"),i)
ENDFOR
oum(o("132"),n_blokov+1)=CIKL1(o("132"),sum/oum(o("64"),n_blokov+1))           &&132 К ст
DO CIKL WITH o("133"),'IIF(inm(i("84"),i)=1,4,IIF(inm(i("84"),i)=2,3,'+;
'IIF(inm(i("84"),i)>2,0,1/0)))'                                                &&133 К осв э
sum=0
FOR i=1 TO n_blokov
  sum=sum+oum(o("133"),i)*oum(o("2"),i)
ENDFOR
oum(o("133"),n_blokov+1)=CIKL1(o("133"),sum/oum(o("2"),n_blokov+1))            &&133 К осв э
DO CIKL WITH o("134"),'IIF(inm(i("84"),i)=1,2,IIF(inm(i("84"),i)=2,1.5,'+;
'IIF(inm(i("84"),i)>2,0,1/0)))'                                                &&134 К осв т
sum=0
FOR i=1 TO n_blokov
  sum=sum+oum(o("134"),i)*oum(o("64"),i)
ENDFOR
oum(o("134"),n_blokov+1)=CIKL1(o("134"),sum/oum(o("64"),n_blokov+1))           &&134 К осв т
DO CIKL WITH o("135"),'1+oum(o("47"),i)/(oum(o("24"),i)*oum(o("2"),i)*(100+oum(o("41"),i))/'+;
'1E5+oum(o("8"),i)*(100-oum(o("126.1"),i))*(100+oum(o("125"),i))/1E4)'         &&135 К отр (к)
oum(o("135"),n_blokov+1)=CIKL1(o("135"),IIF(inm(i("81"),n_blokov+1)=0,1,;
1+oum(o("47"),n_blokov+1)/(oum(o("24"),n_blokov+1)*;
oum(o("2"),n_blokov+1)*(100+oum(o("41"),n_blokov+1))/1E5+oum(o("8"),n_blokov+1)*;
(100-inm(i("85"),n_blokov+1)/inm(i("81"),n_blokov+1)*100-oum(o("126"),n_blokov+1)/;
oum(o("8"),n_blokov+1)*100)*(100+oum(o("125"),n_blokov+1))/1E4)))              &&135 К отр (к)
DO CIKL WITH o("136"),'oum(o("42"),i)*(100+oum(o("132"),i)+oum(o("133"),i))*oum(o("48"),i)*'+;
'100/(oum(o("129"),i)*oum(o("130"),i)*7*oum(o("135"),i))'                      &&136 b э (н)
oum(o("136"),n_blokov+1)=CIKL1(o("136"),oum(o("42"),n_blokov+1)*(100+oum(o("132"),n_blokov+1)+;
oum(o("133"),n_blokov+1))*oum(o("48"),n_blokov+1)*100/(oum(o("129"),n_blokov+1)*;
oum(o("130"),n_blokov+1)*7*oum(o("135"),n_blokov+1)))                          &&136 b э (н)
DO CIKL WITH o("137"),'oum(o("136"),i)*(1+inm(i("64"),n_blokov+1)*'+;
'(1-inm(i("65"),n_blokov+1)))'                                                 &&137 b э (нор)
oum(o("137"),n_blokov+1)=CIKL1(o("137"),oum(o("136"),n_blokov+1)*(1+inm(i("64"),n_blokov+1)*;
(1-inm(i("65"),n_blokov+1))))                                                  &&137 b э (нор)
oum(o("140"),n_blokov+1)=CIKL1(o("140"),(inm(i("50"),n_blokov+1)+inm(i("51"),n_blokov+1))/;
inm(i("71"),n_blokov+1)*1E3)                                                   &&140 G птс
oum(o("141"),n_blokov+1)=CIKL1(o("141"),F1(oum(o("140"),n_blokov+1),"2.36:1")) &&141 N подп
oum(o("142"),n_blokov+1)=CIKL1(o("142"),IIF(inm(i("43"),i)<8,;
F1(oum(o("140"),n_blokov+1),"2.37:1"),F1(oum(o("140"),n_blokov+1),"2.37а:1")))  &&142 N хов
DO CIKL WITH o("143"),'IIF(inm(i("74"),i)=="2" OR inm(i("74"),i)=="3",F1(oum(o("10"),i),"2.38а:1")/1E3,'+;
'IIF(inm(i("74"),i)=="2а",F1(oum(o("10"),i),"2.38:1")/1E3,0))'                 &&143 N кнб
IF BL1 OR BL2 OR BL3 OR BL4 OR BL5 OR BL6
IF BETWEEN(MONTH(DATE()),6,9) OR MONTH(DATE())=5 AND DAY(DATE())>15
DO CIKL WITH o("144"),'IIF(inm(i("74"),i)=="1",0,(oum(o("139"),i)+oum(o("143"),i))*'+;
'inm(i("71"),i)+'+;
'(oum(o("141"),n_blokov+1)+oum(o("142"),n_blokov+1))*inm(i("71"),n_blokov+1)*'+;
'oum(o("8"),i)/oum(o("8"),n_blokov+1))'                                        &&144 Э тепл(н)
ELSE
DO CIKL WITH o("144"),'IIF(inm(i("74"),i)=="1",0,(oum(o("139"),i)+oum(o("143"),i))*'+;
'inm(i("71"),i)+'+;
'(oum(o("141"),n_blokov+1)+oum(o("142"),n_blokov+1))*inm(i("71"),n_blokov+1)*'+;
'oum(o("7"),i)/oum(o("7"),n_blokov+1))'                                        &&144 Э тепл(н)
ENDIF
ELSE
DO CIKL WITH o("144"),'IIF(inm(i("74"),i)=="1",0,(oum(o("139"),i)+oum(o("143"),i))*'+;
'inm(i("71"),i)+'+;
'(oum(o("141"),n_blokov+1)+oum(o("142"),n_blokov+1))*inm(i("71"),n_blokov+1)*'+;
'oum(o("8"),i)/oum(o("8"),n_blokov+1))'                                        &&144 Э тепл(н)
ENDIF
oum(o("144"),n_blokov+1)=CIKL1(o("144"),SUM('oum(o("144"),'))                  &&144 Э тепл(н)
DO CIKL WITH o("145"),'IIF(inm(i("74"),i)=="1",0,(100+oum(o("125"),i))*'+;
'(100+oum(o("132"),i)+oum(o("134"),i))*1E3/'+;
'(oum(o("129"),i)*oum(o("130"),i)*7*oum(o("135"),i)))'                         &&145 b тэ бл
sum=0
FOR i=1 TO n_blokov
  sum=sum+oum(o("145"),i)*oum(o("8"),i)
ENDFOR
oum(o("145"),n_blokov+1)=CIKL1(o("145"),sum/oum(o("8"),n_blokov+1))            &&145 b тэ бл
*oum(o("145"),n_blokov+1)=CIKL1(o("145"),(100+oum(o("125"),n_blokov+1))*;
(100+oum(o("132"),n_blokov+1)+oum(o("134"),n_blokov+1))*1E3/(oum(o("129"),n_blokov+1)*;
oum(o("130"),n_blokov+1)*7*oum(o("135"),n_blokov+1)))                          &&145 b тэ бл
oum(o("146"),n_blokov+1)=CIKL1(o("146"),inm(i("86"),n_blokov+1))               &&146 b тэ пвк
oum(o("146.1"),n_blokov+1)=CIKL1(o("146.1"),inm(i("85"),n_blokov+1)/oum(o("5"),n_blokov+1)*;
1E2)                                                                           &&146.1 alfa пвк
DO CIKL WITH o("147"),'IIF(oum(o("8"),i)=0,0,oum(o("144"),i)*oum(o("136"),i)/oum(o("8"),i))' &&147 db тэ
sum=0
FOR i=1 TO n_blokov
  sum=sum+oum(o("147"),i)*oum(o("3"),i)
ENDFOR
oum(o("147"),n_blokov+1)=CIKL1(o("147"),sum/oum(o("3"),n_blokov+1))            &&147 db тэ
*um(o("147"),n_blokov+1)=CIKL1(o("147"),oum(o("144"),n_blokov+1)*oum(o("136"),n_blokov+1)/;
*um(o("8"),n_blokov+1))                                                   &&147 db тэ
DO CIKL WITH o("148"),'oum(o("145"),i)*(100-oum(o("126.1"),i))/1E2+oum(o("147"),i)'&&148 b тэ (н)
oum(o("148"),n_blokov+1)=CIKL1(o("148"),oum(o("145"),n_blokov+1)*(100-;
oum(o("126.1"),n_blokov+1))/1E2+oum(o("147"),n_blokov+1))                      &&148 b тэ (н)
oum(o("149"),n_blokov+1)=CIKL1(o("149"),(oum(o("145"),n_blokov+1)*(100-;
oum(o("146.1"),n_blokov+1)-oum(o("126.1"),n_blokov+1))+oum(o("146"),n_blokov+1)*;
oum(o("146.1"),n_blokov+1))/1E2+inm(i("87"),n_blokov+1)*1E3/oum(o("5"),n_blokov+1)+;
oum(o("147"),n_blokov+1))                                                      &&149 b тэ (н) ст
DO CIKL WITH o("150"),'oum(o("145"),i)*(100-oum(o("126.1"),i))/1E2*(1+inm(i("66"),n_blokov+1)'+;
'*(1-inm(i("67"),n_blokov+1)))+oum(o("147"),i)*oum(o("137"),i)/oum(o("136"),i)'&&150 b тэ (нр)
oum(o("150"),n_blokov+1)=CIKL1(o("150"),oum(o("145"),n_blokov+1)*(100-;
oum(o("146.1"),n_blokov+1)-oum(o("126.1"),n_blokov+1))*(1+inm(i("66"),n_blokov+1)*;
(1-inm(i("67"),n_blokov+1)))/1E2+oum(o("146"),n_blokov+1)*oum(o("146.1"),n_blokov+1)/1E2+;
inm(i("87"),n_blokov+1)*1E3/oum(o("5"),n_blokov+1)+oum(o("147"),n_blokov+1)*;
oum(o("137"),n_blokov+1)/oum(o("136"),n_blokov+1))                             &&150 b тэ (нр)
DO CIKL WITH o("151"),'oum(o("24"),i)*oum(o("2"),i)/1E3+(oum(o("8"),i)-'+;
'oum(o("126"),i))*(100+oum(o("125"),i))/1E2+oum(o("41"),i)*oum(o("24"),i)*oum(o("2"),i)/'+;
'1E3/1E2+oum(o("121"),i)*oum(o("64"),i)/100+472.2*oum(o("1"),i)/1E2'      &&151 balance
oum(o("151"),n_blokov+1)=CIKL1(o("151"),oum(o("24"),n_blokov+1)*;
oum(o("2"),n_blokov+1)/1E3+(oum(o("8"),n_blokov+1)-inm(i("85"),n_blokov+1)-;
oum(o("126"),n_blokov+1))*(100+oum(o("125"),n_blokov+1))/1E2+oum(o("41"),n_blokov+1)*;
oum(o("24"),n_blokov+1)*oum(o("2"),n_blokov+1)/1E3/1E2+oum(o("120"),n_blokov+1)+472.2*;
oum(o("1"),n_blokov+1)/1E2)                                               &&151 balance
DO CIKL WITH o("152"),'(oum(o("64"),i)-oum(o("151"),i))/oum(o("64"),i)*1E2'    &&152 balance
oum(o("152"),n_blokov+1)=CIKL1(o("152"),(oum(o("64"),n_blokov+1)-oum(o("151"),n_blokov+1))/;
oum(o("64"),n_blokov+1)*1E2)                                                   &&152 balance
oum(o("153"),n_blokov+1)=CIKL1(o("153"),oum(o("36"),n_blokov+1)*oum(o("2"),n_blokov+1)/1E2+;
oum(o("108"),n_blokov+1)+oum(o("144"),n_blokov+1))                        &&153 S Э сн (н)
oum(o("154"),n_blokov+1)=CIKL1(o("154"),inm(i("3"),n_blokov+1))           &&154 S Э сн (ф)

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
