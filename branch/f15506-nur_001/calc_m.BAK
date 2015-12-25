PROC CALC_M
* inm,iom,outm - массив-базы INBLOK,OUTBLOK,OUTMKT
* outm1 - 1-мерн.массив дл€ GATHER в OUTMKT
* oum1 - 2-мерн.массив дл€ GATHER в OUTMKT
* exact - 2-мерн.массив кол-ва зн.после зап€той в OUTMKT
PUBLIC outm1(ALEN(outm,2)),oum(ALEN(outm,1),ALEN(outm,2))
STORE 1 TO min11,aa11,min12,aa12,min21,aa21,min22,aa22,;
           min111,aa111,min112,aa112,min121,aa121,min122,aa122,;
           min31,aa31,min32,aa32,min211,aa211,min212,aa212,min221,aa221,min222,aa222,;
           min1111,aa1111,min1112,aa1112,min1121,aa1121,min1122,aa1122,;
           min1211,aa1211,min1212,aa1212,min1221,aa1221,min1222,aa1222
FOR i=1 TO ALEN(inm,1)
FOR j=1 TO ALEN(inm,2)
    IF NOT Iinm(i)=='74'    && единственный не цифровой признак вх.базы
       inm(i,j)=IIF(VAL(inm(i,j))=0,IIF('0'$inm(i,j),0,inm(i,j)),;
       VAL(inm(i,j)))
    ELSE   
       inm(i,j)=RTRIM(inm(i,j))
    ENDIF
ENDFOR    
ENDFOR    
FOR i=1 TO ALEN(iom,1)
FOR j=1 TO ALEN(iom,2)
    IF SUBSTR(ALLTRIM(iom(i,j)),1,1)='='
       iom(i,j)=CHRTRAN(iom(i,j),'=','')
    ENDIF
    iom(i,j)=IIF(VAL(iom(i,j))=0,IIF('0'$iom(i,j),0,iom(i,j)),;
    VAL(iom(i,j)))
ENDFOR    
ENDFOR    
FOR i=1 TO ALEN(outm,1)
FOR j=1 TO ALEN(outm,2)             && отсечение первого символа
    oum(i,j)=SUBSTR(outm(i,j),2) && €вл€ющегос€ служебным
ENDFOR    
ENDFOR
******** — ѕ–ќ¬≈– ќ…  ќ––≈ “Ќќ—“» ¬’.“јЅЋ. (proc COR_IN)***************
ON ERROR i=i  && дл€ корректной обраб-ки строковых перем. в арифм.выр-€х

&& 1 Nу ср
DO CIKL WITH getIndexOfIOutM("1"),'200'
&& 1 Nу ср
DO CIKL1 WITH getIndexOfIOutM("1"),1200,'    -',1200,'    -'
&& 2 Nм
DO CIKL1 WITH getIndexOfIOutM("2"),inm(getIndexOfIInM("76"),n_blokov+1),'    -',inm(getIndexOfIInM("76"),n_blokov+1),'    -'
&& 3 Qу ср
DO CIKL WITH getIndexOfIOutM("3"),'240'
&& 3 Qу ср
DO CIKL1 WITH getIndexOfIOutM("3"),1440,'    -',1440,'    -'
&& 4 Ёвыр
DO CIKL WITH getIndexOfIOutM("4"),'iom(getIndexOfIIoM("2"),i)'
&& 4 Ёвыр
DO CIKL1 WITH getIndexOfIOutM("4"),iom(getIndexOfIIoM("2"),n_blokov+1),'    -',iom(getIndexOfIIoM("2"),n_blokov+1),'    -'
&& 5 TAU э и
DO CIKL WITH getIndexOfIOutM("5"),'oum(getIndexOfIOutM("4"),i)/oum(getIndexOfIOutM("1"),i)'
&& 5 TAU э и
DO CIKL1 WITH getIndexOfIOutM("5"),oum(getIndexOfIOutM("4"),n_blokov+1)/oum(getIndexOfIOutM("1"),n_blokov+1),'    -',oum(getIndexOfIOutM("4"),n_blokov+1)/oum(getIndexOfIOutM("1"),n_blokov+1),'    -'
&& 6 Qпо
DO CIKL WITH getIndexOfIOutM("6"),'iom(getIndexOfIIoM("4"),i)'
&& 6 Q /по
DO CIKL1 WITH getIndexOfIOutM("6"),iom(getIndexOfIIoM("4"),n_blokov+1),'    -',iom(getIndexOfIIoM("4"),n_blokov+1),'    -'
&& 7 Qто
DO CIKL WITH getIndexOfIOutM("7"),'iom(getIndexOfIIoM("3"),i)'                                            
&& 7 Qто
DO CIKL1 WITH getIndexOfIOutM("7"),iom(getIndexOfIIoM("3"),n_blokov+1),'    -',iom(getIndexOfIIoM("3"),n_blokov+1),'    -'
&& 9 Q
DO CIKL WITH getIndexOfIOutM("9"),'oum(getIndexOfIOutM("6"),i)+oum(getIndexOfIOutM("7"),i)'
&& 9 Q
DO CIKL1 WITH getIndexOfIOutM("9"),SUM('oum(getIndexOfIOutM("9"),'),'    -',SUM('oum(getIndexOfIOutM("9"),'),'    -'
&&10 TAU т и
DO CIKL WITH getIndexOfIOutM("10"),'oum(getIndexOfIOutM("9"),i)/oum(getIndexOfIOutM("3"),i)'
&&10 TAU т и
DO CIKL1 WITH getIndexOfIOutM("10"),oum(getIndexOfIOutM("9"),n_blokov+1)/oum(getIndexOfIOutM("3"),n_blokov+1),'    -',;
oum(getIndexOfIOutM("9"),n_blokov+1)/oum(getIndexOfIOutM("3"),n_blokov+1),'    -'
&&11 Qотп
DO CIKL WITH getIndexOfIOutM("11"),'iom(getIndexOfIIoM("8"),i)'
&&11 Qотп
DO CIKL1 WITH getIndexOfIOutM("11"),iom(getIndexOfIIoM("8"),n_blokov+1),'    -',iom(getIndexOfIIoM("5"),n_blokov+1),'    -'
&&12 Qот гв
DO CIKL WITH getIndexOfIOutM("12"),'oum(getIndexOfIOutM("11"),i)-inm(getIndexOfIInM("83"),i)'
&&12 Qот гв
DO CIKL1 WITH getIndexOfIOutM("12"),SUM('oum(getIndexOfIOutM("12"),'),'    -',SUM('oum(getIndexOfIOutM("12"),'),'    -'
DO CIKL WITH getIndexOfIOutM("13"),'iom(getIndexOfIIoM("8"),i)'                                           &&13 Qот отр
DO CIKL1 WITH getIndexOfIOutM("13"),iom(getIndexOfIIoM("8"),n_blokov+1),'    -',iom(getIndexOfIIoM("8"),n_blokov+1),'    -' &&13 Qот отр
DO CIKL WITH getIndexOfIOutM("15"),'(iom(getIndexOfIIoM("49"),i)*iom(getIndexOfIIoM("57.1"),i)+iom(getIndexOfIIoM("55"),i)*(iom(getIndexOfIIoM("59.1"),i)-'+;
'iom(getIndexOfIIoM("60"),i))-iom(getIndexOfIIoM("66"),i)*iom(getIndexOfIIoM("58"),i))/1E3-(iom(getIndexOfIIoM("3"),i)+iom(getIndexOfIIoM("4"),i))' &&15 Qэ
DO CIKL1 WITH getIndexOfIOutM("15"),SUM('oum(getIndexOfIOutM("15"),'),'    -',SUM('oum(getIndexOfIOutM("15"),'),'    -'  &&15 Qэ
DO CIKL WITH getIndexOfIOutM("16"),'iom(getIndexOfIIoM("41"),i)*oum(getIndexOfIOutM("15"),i)/100+15.4*(inm(getIndexOfIInM("68"),i)-'+;
'inm(getIndexOfIInM("69"),i))'                                                              &&16 Qт сн (н)
DO CIKL1 WITH getIndexOfIOutM("16"),SUM('oum(getIndexOfIOutM("16"),'),'    -',SUM('oum(getIndexOfIOutM("16"),'),'    -'  &&16 Qт сн (н)
DO CIKL WITH getIndexOfIOutM("17"),'iom(getIndexOfIIoM("64"),i)'                                          &&17 Qк бр
DO CIKL1 WITH getIndexOfIOutM("17"),SUM('oum(getIndexOfIOutM("17"),'),'    -',SUM('oum(getIndexOfIOutM("17"),'),'    -'  &&17 Qк бр
DO CIKL WITH getIndexOfIOutM("18"),'iom(getIndexOfIIoM("121"),i)*oum(getIndexOfIOutM("17"),i)/100'                      &&18 Qк сн(н)
DO CIKL1 WITH getIndexOfIOutM("18"),SUM('oum(getIndexOfIOutM("18"),'),'    -',SUM('oum(getIndexOfIOutM("18"),'),'    -'  &&18 Qк сн(н)
DO CIKL WITH getIndexOfIOutM("16.1"),'IIF(oum(getIndexOfIOutM("16"),i)+oum(getIndexOfIOutM("18"),i)=0,0,(oum(getIndexOfIOutM("9"),i)-oum(getIndexOfIOutM("11"),i))*'+;
'oum(getIndexOfIOutM("16"),i)/(oum(getIndexOfIOutM("16"),i)+oum(getIndexOfIOutM("18"),i)))'                              &&16.1 Qт сн
DO CIKL1 WITH getIndexOfIOutM("16.1"),SUM('oum(getIndexOfIOutM("16.1"),'),'    -',SUM('oum(getIndexOfIOutM("16.1"),'),'    -'&&16.1 Qт сн
DO CIKL WITH getIndexOfIOutM("19"),'(oum(getIndexOfIOutM("9"),i)-oum(getIndexOfIOutM("11"),i))-oum(getIndexOfIOutM("16.1"),i)'         &&19 Qк сн
DO CIKL1 WITH getIndexOfIOutM("19"),SUM('oum(getIndexOfIOutM("19"),'),'    -',SUM('oum(getIndexOfIOutM("19"),'),'    -'  &&19 Qк сн
DO CIKL WITH getIndexOfIOutM("20"),'iom(getIndexOfIIoM("1"),i)'                                           &&20 TAU т раб
DO CIKL1 WITH getIndexOfIOutM("20"),iom(getIndexOfIIoM("1"),n_blokov+1),'    -',iom(getIndexOfIIoM("1"),n_blokov+1),'    -'&&20 TAU т раб
DO CIKL WITH getIndexOfIOutM("21"),'inm(getIndexOfIInM("73"),i)'                                          &&21 TAU т рез
DO CIKL1 WITH getIndexOfIOutM("21"),SUM('oum(getIndexOfIOutM("21"),'),'    -',SUM('oum(getIndexOfIOutM("21"),'),'    -'  &&21 TAU т рез
DO CIKL WITH getIndexOfIOutM("24"),'inm(getIndexOfIInM("68"),i)'                                          &&24 n т
DO CIKL1 WITH getIndexOfIOutM("24"),SUM('oum(getIndexOfIOutM("24"),'),'    -',SUM('oum(getIndexOfIOutM("24"),'),'    -'  &&24 n т
DO CIKL WITH getIndexOfIOutM("25"),'inm(getIndexOfIInM("69"),i)'                                          &&25 n т (н)
DO CIKL1 WITH getIndexOfIOutM("25"),SUM('oum(getIndexOfIOutM("25"),'),'    -',SUM('oum(getIndexOfIOutM("25"),'),'    -'  &&25 n т (н)
DO CIKL WITH getIndexOfIOutM("25.1"),'64.2*(oum(getIndexOfIOutM("24"),i)-oum(getIndexOfIOutM("25"),i))'                  &&25.1 dB(n к)
DO CIKL1 WITH getIndexOfIOutM("25.1"),SUM('oum(getIndexOfIOutM("25.1"),'),'    -',SUM('oum(getIndexOfIOutM("25.1"),'),'    -'&&25.1 dB(n к)
IF BL1 OR BL2 OR BL3 OR BL4 OR BL5 OR BL6
DO CIKL WITH getIndexOfIOutM("27"),'IIF(iom(getIndexOfIIoM("8"),i)=0,0,inm(getIndexOfIInM("10"),i)+(inm(getIndexOfIInM("10.4"),n_blokov+1)+inm(getIndexOfIInM("11.1"),n_blokov+1)+'+;
'inm(getIndexOfIInM("11.2"),n_blokov+1)+inm(getIndexOfIInM("12"),n_blokov+1))/n_blokov1)'                &&27 Ётепл 
ELSE
STORE 0 TO sum,sum1
FOR i=1 TO n_blokov
    sum=sum+iom(getIndexOfIIoM("8"),i)
    sum1=sum1+inm(getIndexOfIInM("10"),i)
ENDFOR
DO CIKL WITH getIndexOfIOutM("27"),'IIF(sum=0,0,inm(getIndexOfIInM("10"),i)+(inm(getIndexOfIInM("10"),n_blokov+1)-'+;
'sum1)*inm(getIndexOfIInM("47"),i)/sum)'                                                    &&27 Ётепл 
ENDIF
DO CIKL1 WITH getIndexOfIOutM("27"),SUM('oum(getIndexOfIOutM("27"),'),'    -',SUM('oum(getIndexOfIOutM("27"),'),'    -'  &&27 Ётепл 
IF BL1 OR BL2 OR BL3 OR BL4 OR BL5 OR BL6
DO CIKL WITH getIndexOfIOutM("28"),'inm(getIndexOfIInM("4"),i)+inm(getIndexOfIInM("4"),n_blokov+1)'                    &&28 Ёт сн
ELSE
sum=0
FOR i=1 TO n_blokov
    sum=sum+inm(getIndexOfIInM("4"),i)
ENDFOR
DO CIKL WITH getIndexOfIOutM("28"),'inm(getIndexOfIInM("4"),i)+(inm(getIndexOfIInM("4"),n_blokov+1)-sum)*oum(getIndexOfIOutM("4"),i)'+;
'/oum(getIndexOfIOutM("4"),n_blokov+1)'                                                      &&28 Ёт сн
ENDIF
DO CIKL1 WITH getIndexOfIOutM("28"),SUM('oum(getIndexOfIOutM("28"),'),'    -',SUM('oum(getIndexOfIOutM("28"),'),'    -'  &&28 Ёт сн
IF BL1 OR BL2 OR BL3 OR BL4 OR BL5 OR BL6
DO CIKL WITH getIndexOfIOutM("29"),'inm(getIndexOfIInM("7"),i)+inm(getIndexOfIInM("7"),n_blokov+1)'                    &&29 Ёк сн
ELSE
sum=0
FOR i=1 TO n_blokov
    sum=sum+inm(getIndexOfIInM("7"),i)
ENDFOR
DO CIKL WITH getIndexOfIOutM("29"),'inm(getIndexOfIInM("7"),i)+(inm(getIndexOfIInM("7"),n_blokov+1)-sum)*oum(getIndexOfIOutM("4"),i)'+;
'/oum(getIndexOfIOutM("4"),n_blokov+1)'                                                      &&29 Ёк сн
ENDIF
DO CIKL1 WITH getIndexOfIOutM("29"),SUM('oum(getIndexOfIOutM("29"),'),'    -',SUM('oum(getIndexOfIOutM("29"),'),'    -'  &&29 Ёк сн
DO CIKL WITH getIndexOfIOutM("30"),'oum(getIndexOfIOutM("29"),i)+oum(getIndexOfIOutM("28"),i)+oum(getIndexOfIOutM("27"),i)'            &&30 Ёсн
DO CIKL1 WITH getIndexOfIOutM("30"),SUM('oum(getIndexOfIOutM("30"),'),'    -',SUM('oum(getIndexOfIOutM("30"),'),'    -'  &&30 Ёсн
DO CIKL WITH getIndexOfIOutM("31"),'oum(getIndexOfIOutM("4"),i)-oum(getIndexOfIOutM("30"),i)'                            &&31 Ёот
DO CIKL1 WITH getIndexOfIOutM("31"),SUM('oum(getIndexOfIOutM("31"),'),'    -',SUM('oum(getIndexOfIOutM("31"),'),'    -'  &&31 Ёот
DO CIKL WITH getIndexOfIOutM("32"),'1E-3*860*(inm(getIndexOfIInM("10.1"),i)+inm(getIndexOfIInM("10.2"),i))*85/1E2'     &&32 Qнас
DO CIKL1 WITH getIndexOfIOutM("32"),SUM('oum(getIndexOfIOutM("32"),'),'    -',SUM('oum(getIndexOfIOutM("32"),'),'    -'  &&32 Qнас
DO CIKL WITH getIndexOfIOutM("33"),'IIF(inm(getIndexOfIInM("47"),i)=0,1,(oum(getIndexOfIOutM("15"),i)+oum(getIndexOfIOutM("16.1"),i)+iom(getIndexOfIIoM("47"),i))/'+;
'(oum(getIndexOfIOutM("15"),i)+oum(getIndexOfIOutM("16.1"),i)+iom(getIndexOfIIoM("47"),i)+(oum(getIndexOfIOutM("11"),i)-oum(getIndexOfIOutM("32"),i))*'+;
'(100+iom(getIndexOfIIoM("125"),i))/1E2))'                                                  &&33 Kэ
DO CIKL1 WITH getIndexOfIOutM("33"),IIF(SUM('inm(getIndexOfIInM("47"),')=0,1,(oum(getIndexOfIOutM("15"),n_blokov+1)+;
oum(getIndexOfIOutM("16.1"),n_blokov+1)+iom(getIndexOfIIoM("47"),n_blokov+1))/;
(oum(getIndexOfIOutM("15"),n_blokov+1)+oum(getIndexOfIOutM("16.1"),n_blokov+1)+iom(getIndexOfIIoM("47"),n_blokov+1)+;
(oum(getIndexOfIOutM("11"),n_blokov+1)-oum(getIndexOfIOutM("32"),n_blokov+1))*(100+iom(getIndexOfIIoM("125"),n_blokov+1))/1E2)),'    -',;
IIF(SUM('inm(getIndexOfIInM("47"),')=0,1,(oum(getIndexOfIOutM("15"),n_blokov+1)+oum(getIndexOfIOutM("16.1"),n_blokov+1)+;
iom(getIndexOfIIoM("47"),n_blokov+1))/;
(oum(getIndexOfIOutM("15"),n_blokov+1)+oum(getIndexOfIOutM("16.1"),n_blokov+1)+iom(getIndexOfIIoM("47"),n_blokov+1)+;
(oum(getIndexOfIOutM("11"),n_blokov+1)-oum(getIndexOfIOutM("32"),n_blokov+1))*(100+iom(getIndexOfIIoM("125"),n_blokov+1))/1E2)),;
'    -'                                                                        &&33 Kэ
DO CIKL WITH getIndexOfIOutM("34"),'oum(getIndexOfIOutM("29"),i)*oum(getIndexOfIOutM("33"),i)+oum(getIndexOfIOutM("28"),i)'            &&34 Ёэ сн
DO CIKL1 WITH getIndexOfIOutM("34"),SUM('oum(getIndexOfIOutM("34"),'),'    -',SUM('oum(getIndexOfIOutM("34"),'),'    -'  &&34 Ёэ сн
DO CIKL WITH getIndexOfIOutM("35"),'oum(getIndexOfIOutM("30"),i)-oum(getIndexOfIOutM("34"),i)'                           &&35 Ётэ сн
DO CIKL1 WITH getIndexOfIOutM("35"),SUM('oum(getIndexOfIOutM("35"),'),'    -',SUM('oum(getIndexOfIOutM("35"),'),'    -'  &&35 Ётэ сн
DO CIKL WITH getIndexOfIOutM("36"),'iom(getIndexOfIIoM("36"),i)*oum(getIndexOfIOutM("4"),i)/1E2'                        &&36 Ёт сн(н)
DO CIKL1 WITH getIndexOfIOutM("36"),SUM('oum(getIndexOfIOutM("36"),'),'    -',SUM('oum(getIndexOfIOutM("36"),'),'    -'  &&36 Ёт сн(н)
DO CIKL WITH getIndexOfIOutM("37"),'iom(getIndexOfIIoM("108"),i)'                                         &&37 Ёк сн(н)
DO CIKL1 WITH getIndexOfIOutM("37"),SUM('oum(getIndexOfIOutM("37"),'),'    -',SUM('oum(getIndexOfIOutM("37"),'),'    -'  &&37 Ёк сн(н)
DO CIKL WITH getIndexOfIOutM("38"),'iom(getIndexOfIIoM("144"),i)'                                         &&38 Ётепл сн(н)
DO CIKL1 WITH getIndexOfIOutM("38"),SUM('oum(getIndexOfIOutM("38"),'),'    -',SUM('oum(getIndexOfIOutM("38"),'),'    -'  &&38 Ётепл сн(н)
DO CIKL WITH getIndexOfIOutM("39"),'iom(getIndexOfIIoM("128"),i)*oum(getIndexOfIOutM("4"),i)/1E2'                       &&39 Ёэ сн(н)
DO CIKL1 WITH getIndexOfIOutM("39"),SUM('oum(getIndexOfIOutM("39"),'),'    -',SUM('oum(getIndexOfIOutM("39"),'),'    -'  &&39 Ёэ сн(н)
DO CIKL WITH getIndexOfIOutM("40"),'oum(getIndexOfIOutM("36"),i)+oum(getIndexOfIOutM("37"),i)+oum(getIndexOfIOutM("38"),i)-oum(getIndexOfIOutM("39"),i)'&&40 Ётэ сн(н)
DO CIKL1 WITH getIndexOfIOutM("40"),SUM('oum(getIndexOfIOutM("40"),'),'    -',SUM('oum(getIndexOfIOutM("40"),'),'    -'  &&40 Ётэ сн(н)
IF BL1 OR BL2 OR BL3 OR BL4 OR BL5 OR BL6
DO CIKL WITH getIndexOfIOutM("41"),'inm(getIndexOfIInM("6"),n_blokov+1)/n_blokov1'                        &&41 Ёцн
ELSE
DO CIKL WITH getIndexOfIOutM("41"),'inm(getIndexOfIInM("6"),n_blokov+1)*oum(getIndexOfIOutM("4"),i)/oum(getIndexOfIOutM("4"),n_blokov+1)'&&41 Ёцн
ENDIF
DO CIKL1 WITH getIndexOfIOutM("41"),SUM('oum(getIndexOfIOutM("41"),'),'    -',SUM('oum(getIndexOfIOutM("41"),'),'    -'  &&41 Ёцн
DO CIKL WITH getIndexOfIOutM("42"),'iom(getIndexOfIIoM("29"),n_blokov+1)*inm(getIndexOfIInM("70"),n_blokov+1)*oum(getIndexOfIOutM("4"),i)/'+;
'oum(getIndexOfIOutM("4"),n_blokov+1)'                                                       &&42 Ёцн(н)
DO CIKL1 WITH getIndexOfIOutM("42"),SUM('oum(getIndexOfIOutM("42"),'),'    -',SUM('oum(getIndexOfIOutM("42"),'),'    -'  &&42 Ёцн(н)
DO CIKL WITH getIndexOfIOutM("43"),'inm(getIndexOfIInM("8.1"),i)'                                         &&43 Ёпэн
DO CIKL1 WITH getIndexOfIOutM("43"),SUM('oum(getIndexOfIOutM("43"),'),'    -',SUM('oum(getIndexOfIOutM("43"),'),'    -'  &&43 Ёпэн
DO CIKL WITH getIndexOfIOutM("44"),'iom(getIndexOfIIoM("66"),i)/1E3'                                      &&44 Gпв
DO CIKL1 WITH getIndexOfIOutM("44"),SUM('oum(getIndexOfIOutM("44"),'),'    -',SUM('oum(getIndexOfIOutM("44"),'),'    -'  &&44 Gпв
DO CIKL WITH getIndexOfIOutM("45"),'iom(getIndexOfIIoM("99"),i)*oum(getIndexOfIOutM("44"),i)'                           &&45 Ёпэн (н)
DO CIKL1 WITH getIndexOfIOutM("45"),SUM('oum(getIndexOfIOutM("45"),'),'    -',SUM('oum(getIndexOfIOutM("45"),'),'    -'  &&45 Ёпэн (н)
DO CIKL WITH getIndexOfIOutM("46"),'LOG(IIF(inm(getIndexOfIInM("74"),i)=="2а",inm(getIndexOfIInM("38"),i),inm(getIndexOfIInM("37"),i)))',.F.&&46 ƒт
DO CIKL WITH getIndexOfIOutM("46"),'1/(2.6864264-.20096551*oum(getIndexOfIOutM("46"),i)-2.16688/1E3*oum(getIndexOfIOutM("46"),i)^2-'+;
'9.480808/1E5*oum(getIndexOfIOutM("46"),i)^3+6.135062/1E6*oum(getIndexOfIOutM("46"),i)^4+3.6917245/1E6*'+;
'oum(getIndexOfIOutM("46"),i)^5)',.F.                                                        &&46 ƒт
DO CIKL WITH getIndexOfIOutM("46"),'-753.317+6959.4093*oum(getIndexOfIOutM("46"),i)-29257.981*oum(getIndexOfIOutM("46"),i)^2+'+;
'71285.169*oum(getIndexOfIOutM("46"),i)^3-86752.84*oum(getIndexOfIOutM("46"),i)^4+42641.056*oum(getIndexOfIOutM("46"),i)^5',.F.&&46 ƒт
DO CIKL WITH getIndexOfIOutM("46"),'oum(getIndexOfIOutM("7"),i)*1E3/(iom(getIndexOfIIoM("62"),i)-oum(getIndexOfIOutM("46"),i))'       &&46 ƒт
DO CIKL WITH getIndexOfIOutM("47"),'oum(getIndexOfIOutM("46"),i)*(iom(getIndexOfIIoM("58"),i)-107)/(728-iom(getIndexOfIIoM("58"),i))'&&47 ƒрег
DO CIKL WITH getIndexOfIOutM("48"),'iom(getIndexOfIIoM("26"),i)'                                          &&48 Ётф п
DO CIKL1 WITH getIndexOfIOutM("48"),SUM('oum(getIndexOfIOutM("48"),'),'    -',SUM('oum(getIndexOfIOutM("48"),'),'    -'  &&48 Ётф п
DO CIKL WITH getIndexOfIOutM("49"),'IIF(inm(getIndexOfIInM("74"),i)=="1",0,(oum(getIndexOfIOutM("46"),i)*(iom(getIndexOfIIoM("57.1"),i)+'+;
'(iom(getIndexOfIIoM("59.1"),i)-iom(getIndexOfIIoM("60"),i))-iom(getIndexOfIIoM("62"),i))+oum(getIndexOfIOutM("47"),i)*(iom(getIndexOfIIoM("57.1"),i)+'+;
'(iom(getIndexOfIIoM("59.1"),i)-iom(getIndexOfIIoM("60"),i))-752))/860)'                                   &&49 Ётф т
DO CIKL1 WITH getIndexOfIOutM("49"),SUM('oum(getIndexOfIOutM("49"),'),'    -',SUM('oum(getIndexOfIOutM("49"),'),'    -'  &&49 Ётф т
DO CIKL WITH getIndexOfIOutM("50"),'oum(getIndexOfIOutM("48"),i)+oum(getIndexOfIOutM("49"),i)'                           &&50 Ётф
DO CIKL1 WITH getIndexOfIOutM("50"),SUM('oum(getIndexOfIOutM("50"),'),'    -',SUM('oum(getIndexOfIOutM("50"),'),'    -'  &&50 Ётф
DO CIKL WITH getIndexOfIOutM("51"),'oum(getIndexOfIOutM("50"),i)/oum(getIndexOfIOutM("4"),i)*1E2'                        &&51 Ётф
DO CIKL1 WITH getIndexOfIOutM("51"),oum(getIndexOfIOutM("50"),n_blokov+1)/oum(getIndexOfIOutM("4"),n_blokov+1)*1E2,'    -',;
oum(getIndexOfIOutM("50"),n_blokov+1)/oum(getIndexOfIOutM("4"),n_blokov+1)*1E2,'    -'                     &&51 Ётф
DO CIKL WITH getIndexOfIOutM("52"),'oum(getIndexOfIOutM("49"),i)/oum(getIndexOfIOutM("7"),i)*1E3'                        &&52 Wтф т
DO CIKL1 WITH getIndexOfIOutM("52"),oum(getIndexOfIOutM("49"),n_blokov+1)/oum(getIndexOfIOutM("7"),n_blokov+1)*1E3,'    -',;
'    -','    -'                                                                &&52 Wтф т
DO CIKL WITH getIndexOfIOutM("53"),'iom(getIndexOfIIoM("27"),i)'                                          &&53 Wтф п
DO CIKL WITH getIndexOfIOutM("55"),'oum(getIndexOfIOutM("15"),i)/oum(getIndexOfIOutM("4"),i)*1E3'                        &&55 qт бр
DO CIKL1 WITH getIndexOfIOutM("55"),oum(getIndexOfIOutM("15"),n_blokov+1)/oum(getIndexOfIOutM("4"),n_blokov+1)*1E3,'    -',;
oum(getIndexOfIOutM("15"),n_blokov+1)/oum(getIndexOfIOutM("4"),n_blokov+1)*1E3,'    -'                     &&55 qт бр
DO CIKL WITH getIndexOfIOutM("56"),'iom(getIndexOfIIoM("24"),i)'                                          &&56 qт бр(н)
DO CIKL1 WITH getIndexOfIOutM("56"),iom(getIndexOfIIoM("24"),n_blokov+1),'    -',iom(getIndexOfIIoM("24"),n_blokov+1),'    -'&&56 qт бр(н)
DO CIKL WITH getIndexOfIOutM("57"),'(.05*inm(getIndexOfIInM("56"),i)/(100-inm(getIndexOfIInM("56"),i))+.95*inm(getIndexOfIInM("57"),i)/'+;
'(100-inm(getIndexOfIInM("57"),i)))*7800*inm(getIndexOfIInM("55"),n_blokov+1)/1E2/inm(getIndexOfIInM("53"),n_blokov+1)*'+;
'iom(getIndexOfIIoM("89"),i)'                                                               &&57 (q3+q4)
sum=0
FOR i=1 TO n_blokov
    sum=sum+oum(getIndexOfIOutM("57"),i)*oum(getIndexOfIOutM("17"),i)
ENDFOR
DO CIKL1 WITH getIndexOfIOutM("57"),sum/SUM('oum(getIndexOfIOutM("17"),'),'    -',sum/SUM('oum(getIndexOfIOutM("17"),'),'    -'&&57 (q3+q4) /н
DO CIKL WITH getIndexOfIOutM("58"),'iom(getIndexOfIIoM("73"),i)'                                          &&58 q3(н)+q4(н)
DO CIKL1 WITH getIndexOfIOutM("58"),iom(getIndexOfIIoM("73"),n_blokov+1),'    -',iom(getIndexOfIIoM("73"),n_blokov+1),'    -'&&58 q3(н)+q4(н)
DO CIKL WITH getIndexOfIOutM("59"),'(21-(.02*iom(getIndexOfIIoM("89"),i)/1E2+.1*inm(getIndexOfIInM("59"),i)/1E2)*'+;
'(inm(getIndexOfIInM("35"),i)+inm(getIndexOfIInM("36"),i))/2)/(21-(inm(getIndexOfIInM("35"),i)+inm(getIndexOfIInM("36"),i))/2)'    &&59 alfa р
sum=0
FOR i=1 TO n_blokov
    sum=sum+oum(getIndexOfIOutM("59"),i)*oum(getIndexOfIOutM("17"),i)
ENDFOR
DO CIKL1 WITH getIndexOfIOutM("59"),sum/SUM('oum(getIndexOfIOutM("17"),'),'    -',sum/SUM('oum(getIndexOfIOutM("17"),'),'    -'&&59 alfa р
DO CIKL WITH getIndexOfIOutM("60"),'iom(getIndexOfIIoM("67"),i)'                                          &&60 alfa р(н)
sum=0
FOR i=1 TO n_blokov
    sum=sum+oum(getIndexOfIOutM("60"),i)*oum(getIndexOfIOutM("17"),i)
ENDFOR
DO CIKL1 WITH getIndexOfIOutM("60"),sum/SUM('oum(getIndexOfIOutM("17"),'),'    -',sum/SUM('oum(getIndexOfIOutM("17"),'),'    -'&&60 alfa р(н)
DO CIKL WITH getIndexOfIOutM("61"),'inm(getIndexOfIInM("44"),i)*SQRT(472.2/iom(getIndexOfIIoM("65"),i))'               &&61 dalfa рух
sum=0
FOR i=1 TO n_blokov
    sum=sum+oum(getIndexOfIOutM("61"),i)*oum(getIndexOfIOutM("17"),i)
ENDFOR
DO CIKL1 WITH getIndexOfIOutM("61"),sum/SUM('oum(getIndexOfIOutM("17"),'),'    -',sum/SUM('oum(getIndexOfIOutM("17"),'),'    -'&&61 dalfa рух
DO CIKL WITH getIndexOfIOutM("62"),'iom(getIndexOfIIoM("68"),i)*1E2'                                      &&62 dalfa рух(н)
sum=0
FOR i=1 TO n_blokov
    sum=sum+oum(getIndexOfIOutM("62"),i)*oum(getIndexOfIOutM("17"),i)
ENDFOR
DO CIKL1 WITH getIndexOfIOutM("62"),sum/SUM('oum(getIndexOfIOutM("17"),'),'    -',sum/SUM('oum(getIndexOfIOutM("17"),'),'    -'&&62 dalfa рух(н)
DO CIKL WITH getIndexOfIOutM("63"),'inm(getIndexOfIInM("44.1"),i)*SQRT(446.1/iom(getIndexOfIIoM("65"),i))'             &&63 dalfa ух-д
sum=0
FOR i=1 TO n_blokov
    sum=sum+oum(getIndexOfIOutM("63"),i)*oum(getIndexOfIOutM("17"),i)
ENDFOR
DO CIKL1 WITH getIndexOfIOutM("63"),sum/SUM('oum(getIndexOfIOutM("17"),'),'    -',sum/SUM('oum(getIndexOfIOutM("17"),'),'    -'&&63 dalfa ух-д
DO CIKL WITH getIndexOfIOutM("64"),'(inm(getIndexOfIInM("31"),i)+inm(getIndexOfIInM("31.1"),i))/2'                     &&64 t хв
sum=0
FOR i=1 TO n_blokov
    sum=sum+oum(getIndexOfIOutM("64"),i)*oum(getIndexOfIOutM("17"),i)
ENDFOR
DO CIKL1 WITH getIndexOfIOutM("64"),sum/SUM('oum(getIndexOfIOutM("17"),'),'    -',sum/SUM('oum(getIndexOfIOutM("17"),'),'    -'&&64 t хв
DO CIKL WITH getIndexOfIOutM("65"),'(inm(getIndexOfIInM("32"),i)+inm(getIndexOfIInM("32.1"),i))/2'                     &&65 t 'вп
sum=0
FOR i=1 TO n_blokov
    sum=sum+oum(getIndexOfIOutM("65"),i)*oum(getIndexOfIOutM("17"),i)
ENDFOR
DO CIKL1 WITH getIndexOfIOutM("65"),sum/SUM('oum(getIndexOfIOutM("17"),'),'    -',sum/SUM('oum(getIndexOfIOutM("17"),'),'    -'&&65 t 'вп
DO CIKL WITH getIndexOfIOutM("66"),'(inm(getIndexOfIInM("39"),i)+inm(getIndexOfIInM("40"),i))/2'                       &&66 t ух
sum=0
FOR i=1 TO n_blokov
    sum=sum+oum(getIndexOfIOutM("66"),i)*oum(getIndexOfIOutM("17"),i)
ENDFOR
DO CIKL1 WITH getIndexOfIOutM("66"),sum/SUM('oum(getIndexOfIOutM("17"),'),'    -',sum/SUM('oum(getIndexOfIOutM("17"),'),'    -'&&66 t ух
DO CIKL WITH getIndexOfIOutM("67"),'iom(getIndexOfIIoM("79"),i)'                                          &&67 t ух(н)
DO CIKL1 WITH getIndexOfIOutM("67"),iom(getIndexOfIIoM("79"),n_blokov+1),'    -',iom(getIndexOfIIoM("79"),n_blokov+1),'    -'&&67 t ух(н)
DO CIKL WITH getIndexOfIOutM("68"),'(iom(getIndexOfIIoM("80"),i)*(oum(getIndexOfIOutM("59"),i)+oum(getIndexOfIOutM("61"),i)/1E2)+iom(getIndexOfIIoM("81"),i))*'+;
'(oum(getIndexOfIOutM("66"),i)-(oum(getIndexOfIOutM("59"),i)+oum(getIndexOfIOutM("61"),i)/1E2)/((oum(getIndexOfIOutM("59"),i)+oum(getIndexOfIOutM("61"),i)/1E2)+'+;
'iom(getIndexOfIIoM("82"),i))*oum(getIndexOfIOutM("64"),i))*(.9805+.00013*oum(getIndexOfIOutM("66"),i))*(1-.01*oum(getIndexOfIOutM("57"),i))/1E2+'+;
'(.2*.95*inm(getIndexOfIInM("55"),n_blokov+1)*iom(getIndexOfIIoM("89"),i)/1E2*oum(getIndexOfIOutM("66"),i))/'+;
'inm(getIndexOfIInM("53"),n_blokov+1)'                                                      &&68 q2
sum=0
FOR i=1 TO n_blokov
    sum=sum+oum(getIndexOfIOutM("68"),i)*oum(getIndexOfIOutM("17"),i)
ENDFOR
DO CIKL1 WITH getIndexOfIOutM("68"),sum/SUM('oum(getIndexOfIOutM("17"),'),'    -',sum/SUM('oum(getIndexOfIOutM("17"),'),'    -'&&68 q2
DO CIKL WITH getIndexOfIOutM("69"),'iom(getIndexOfIIoM("83"),i)'                                          &&69 q2(н)
DO CIKL1 WITH getIndexOfIOutM("69"),iom(getIndexOfIIoM("83"),n_blokov+1),'    -',iom(getIndexOfIIoM("83"),n_blokov+1),'    -'&&69 q2(н)
DO CIKL WITH getIndexOfIOutM("67.1"),'(oum(getIndexOfIOutM("69"),i)*(oum(getIndexOfIOutM("66"),i)-oum(getIndexOfIOutM("67"),i)))/(oum(getIndexOfIOutM("67"),i)-'+;
'((oum(getIndexOfIOutM("60"),i)+oum(getIndexOfIOutM("62"),i)/1E2)*oum(getIndexOfIOutM("64"),i)/(oum(getIndexOfIOutM("60"),i)+oum(getIndexOfIOutM("62"),i)/1E2)+'+;
'iom(getIndexOfIIoM("82"),i)))'                                                             &&67.1 dq2 (t ух)
DO CIKL WITH getIndexOfIOutM("70"),'iom(getIndexOfIIoM("49"),i)/1E3'                                      &&70 D0
DO CIKL1 WITH getIndexOfIOutM("70"),SUM('oum(getIndexOfIOutM("70"),'),'    -',SUM('oum(getIndexOfIOutM("70"),'),'    -'  &&70 D0
DO CIKL WITH getIndexOfIOutM("71"),'inm(getIndexOfIInM("68"),i)*64.2*7*1E2/(oum(getIndexOfIOutM("17"),i)*1E2/(100-oum(getIndexOfIOutM("68"),i)-'+;
'oum(getIndexOfIOutM("57"),i)-iom(getIndexOfIIoM("84"),i)-iom(getIndexOfIIoM("85"),i))+85.0*7)'                        &&71 q пуск
DO CIKL WITH getIndexOfIOutM("72"),'iom(getIndexOfIIoM("87"),i)'                                          &&72 q пуск(н)
DO CIKL WITH getIndexOfIOutM("73"),'100-oum(getIndexOfIOutM("68"),i)-oum(getIndexOfIOutM("57"),i)-iom(getIndexOfIIoM("84"),i)-iom(getIndexOfIIoM("85"),i)-'+;
'oum(getIndexOfIOutM("71"),i)'                                                               &&73  ѕƒк бр
sum=0
FOR i=1 TO n_blokov
    sum=sum+oum(getIndexOfIOutM("73"),i)*oum(getIndexOfIOutM("17"),i)
ENDFOR
DO CIKL1 WITH getIndexOfIOutM("73"),sum/SUM('oum(getIndexOfIOutM("17"),'),'    -',sum/SUM('oum(getIndexOfIOutM("17"),'),'    -'&&73  ѕƒк бр
DO CIKL WITH getIndexOfIOutM("74"),'iom(getIndexOfIIoM("88"),i)'                                          &&74  ѕƒк бр(н)
DO CIKL1 WITH getIndexOfIOutM("74"),iom(getIndexOfIIoM("88"),n_blokov+1),'    -',iom(getIndexOfIIoM("88"),n_blokov+1),'    -'&&74  ѕƒк бр(н)
DO CIKL WITH getIndexOfIOutM("75"),'oum(getIndexOfIOutM("17"),i)*1E2/7/oum(getIndexOfIOutM("73"),i)'                     &&75 B
DO CIKL1 WITH getIndexOfIOutM("75"),SUM('oum(getIndexOfIOutM("75"),'),'    -',SUM('oum(getIndexOfIOutM("75"),'),'    -'  &&75 B
DO CIKL WITH getIndexOfIOutM("67.2"),'oum(getIndexOfIOutM("75"),i)*oum(getIndexOfIOutM("67.1"),i)/oum(getIndexOfIOutM("74"),i)'        &&67.2 dB(t ух)
DO CIKL1 WITH getIndexOfIOutM("67.2"),SUM('oum(getIndexOfIOutM("67.2"),'),'    -',SUM('oum(getIndexOfIOutM("67.2"),'),'    -'&&67.2 dB(t ух)
DO CIKL WITH getIndexOfIOutM("76"),'oum(getIndexOfIOutM("75"),i)*inm(getIndexOfIInM("59"),i)/1E2'                       &&76 B г
DO CIKL1 WITH getIndexOfIOutM("76"),SUM('oum(getIndexOfIOutM("76"),'),'    -',SUM('oum(getIndexOfIOutM("76"),'),'    -'  &&76 B г
DO CIKL WITH getIndexOfIOutM("77"),'oum(getIndexOfIOutM("75"),i)*inm(getIndexOfIInM("60"),i)/1E2'                       &&77 B м
DO CIKL1 WITH getIndexOfIOutM("77"),SUM('oum(getIndexOfIOutM("77"),'),'    -',SUM('oum(getIndexOfIOutM("77"),'),'    -'  &&77 B м
DO CIKL WITH getIndexOfIOutM("78"),'oum(getIndexOfIOutM("75"),i)-oum(getIndexOfIOutM("76"),i)-oum(getIndexOfIOutM("77"),i)'            &&78 B тв
DO CIKL1 WITH getIndexOfIOutM("78"),SUM('oum(getIndexOfIOutM("78"),'),'    -',SUM('oum(getIndexOfIOutM("78"),'),'    -'  &&78 B тв
DO CIKL WITH getIndexOfIOutM("79"),'oum(getIndexOfIOutM("75"),i)*oum(getIndexOfIOutM("33"),i)*oum(getIndexOfIOutM("31"),i)/(oum(getIndexOfIOutM("4"),i)-'+;
'oum(getIndexOfIOutM("34"),i))'                                                              &&79 B э
DO CIKL1 WITH getIndexOfIOutM("79"),SUM('oum(getIndexOfIOutM("79"),'),'    -',SUM('oum(getIndexOfIOutM("79"),'),'    -'  &&79 B э
DO CIKL WITH getIndexOfIOutM("80"),'oum(getIndexOfIOutM("75"),i)-oum(getIndexOfIOutM("79"),i)'                           &&80 B тэ
DO CIKL1 WITH getIndexOfIOutM("80"),SUM('oum(getIndexOfIOutM("80"),'),'    -',SUM('oum(getIndexOfIOutM("80"),'),'    -'  &&80 B тэ
DO CIKL WITH getIndexOfIOutM("81"),'oum(getIndexOfIOutM("79"),i)*1E3/oum(getIndexOfIOutM("31"),i)'                       &&81 b э
DO CIKL1 WITH getIndexOfIOutM("81"),oum(getIndexOfIOutM("79"),n_blokov+1)*1E3/oum(getIndexOfIOutM("31"),n_blokov+1),'    -',;
oum(getIndexOfIOutM("79"),n_blokov+3)*1E3/oum(getIndexOfIOutM("31"),n_blokov+3),'    -'                    &&81 b э
DO CIKL WITH getIndexOfIOutM("82"),'iom(getIndexOfIIoM("136"),i)'                                         &&82 b э н
DO CASE
   CASE BL1
DO CIKL1 WITH getIndexOfIOutM("82"),oum(getIndexOfIOutM("82"),ASCAN(ai,1)),'    -',oum(getIndexOfIOutM("82"),ASCAN(ai,1)),'    -'&&82 b э н
   CASE BL2
DO CIKL1 WITH getIndexOfIOutM("82"),oum(getIndexOfIOutM("82"),ASCAN(ai,2)),'    -',oum(getIndexOfIOutM("82"),ASCAN(ai,2)),'    -'&&82 b э н
   CASE BL3
DO CIKL1 WITH getIndexOfIOutM("82"),oum(getIndexOfIOutM("82"),ASCAN(ai,3)),'    -',oum(getIndexOfIOutM("82"),ASCAN(ai,3)),'    -'&&82 b э н
   CASE BL4
DO CIKL1 WITH getIndexOfIOutM("82"),oum(getIndexOfIOutM("82"),ASCAN(ai,4)),'    -',oum(getIndexOfIOutM("82"),ASCAN(ai,4)),'    -'&&82 b э н
   CASE BL5
DO CIKL1 WITH getIndexOfIOutM("82"),oum(getIndexOfIOutM("82"),ASCAN(ai,5)),'    -',oum(getIndexOfIOutM("82"),ASCAN(ai,5)),'    -'&&82 b э н
   CASE BL6
DO CIKL1 WITH getIndexOfIOutM("82"),oum(getIndexOfIOutM("82"),ASCAN(ai,6)),'    -',oum(getIndexOfIOutM("82"),ASCAN(ai,6)),'    -'&&82 b э н
   OTHER
DO CIKL1 WITH getIndexOfIOutM("82"),iom(getIndexOfIIoM("136"),n_blokov+1),'    -',iom(getIndexOfIIoM("136"),n_blokov+1),'    -'&&82 b э н
ENDCASE
DO CIKL WITH getIndexOfIOutM("83"),'iom(getIndexOfIIoM("137"),i)'                                         &&83 b э нр
DO CASE
   CASE BL1
DO CIKL1 WITH getIndexOfIOutM("83"),oum(getIndexOfIOutM("83"),ASCAN(ai,1)),'    -',oum(getIndexOfIOutM("83"),ASCAN(ai,1)),'    -'&&83 b э нр
   CASE BL2
DO CIKL1 WITH getIndexOfIOutM("83"),oum(getIndexOfIOutM("83"),ASCAN(ai,2)),'    -',oum(getIndexOfIOutM("83"),ASCAN(ai,2)),'    -'&&83 b э нр
   CASE BL3
DO CIKL1 WITH getIndexOfIOutM("83"),oum(getIndexOfIOutM("83"),ASCAN(ai,3)),'    -',oum(getIndexOfIOutM("83"),ASCAN(ai,3)),'    -'&&83 b э нр
   CASE BL4
DO CIKL1 WITH getIndexOfIOutM("83"),oum(getIndexOfIOutM("83"),ASCAN(ai,4)),'    -',oum(getIndexOfIOutM("83"),ASCAN(ai,4)),'    -'&&83 b э нр
   CASE BL5
DO CIKL1 WITH getIndexOfIOutM("83"),oum(getIndexOfIOutM("83"),ASCAN(ai,5)),'    -',oum(getIndexOfIOutM("83"),ASCAN(ai,5)),'    -'&&83 b э нр
   CASE BL6
DO CIKL1 WITH getIndexOfIOutM("83"),oum(getIndexOfIOutM("83"),ASCAN(ai,6)),'    -',oum(getIndexOfIOutM("83"),ASCAN(ai,6)),'    -'&&83 b э нр
   OTHER
DO CIKL1 WITH getIndexOfIOutM("83"),iom(getIndexOfIIoM("137"),n_blokov+1),'    -',iom(getIndexOfIIoM("137"),n_blokov+1),'    -'&&83 b э нр
ENDCASE
DO CIKL WITH getIndexOfIOutM("84"),'oum(getIndexOfIOutM("80"),i)*1E3/oum(getIndexOfIOutM("11"),i)'                       &&84 b тэ
DO CIKL1 WITH getIndexOfIOutM("84"),oum(getIndexOfIOutM("80"),n_blokov+1)*1E3/oum(getIndexOfIOutM("11"),n_blokov+1),'    -',;
oum(getIndexOfIOutM("80"),n_blokov+1)*1E3/oum(getIndexOfIOutM("11"),n_blokov+1),'    -'                    &&84 b тэ
DO CIKL WITH getIndexOfIOutM("85"),'iom(getIndexOfIIoM("148"),i)'                                         &&85 b тэ(н)
DO CIKL1 WITH getIndexOfIOutM("85"),iom(getIndexOfIIoM("148"),n_blokov+1),'    -',iom(getIndexOfIIoM("148"),n_blokov+1),'    -'&&85 b тэ(н)
DO CIKL WITH getIndexOfIOutM("86"),'iom(getIndexOfIIoM("150"),i)'                                         &&86 b тэ нр
DO CIKL1 WITH getIndexOfIOutM("86"),iom(getIndexOfIIoM("150"),n_blokov+1),'    -',iom(getIndexOfIIoM("150"),n_blokov+1),'    -'&&86 b тэ нр
DO CIKL WITH getIndexOfIOutM("89"),'iom(getIndexOfIIoM("45"),i)'                                          &&89 dQэ по(отр)
DO CIKL1 WITH getIndexOfIOutM("89"),iom(getIndexOfIIoM("45"),n_blokov+1),'    -',iom(getIndexOfIIoM("45"),n_blokov+1),'    -'&&89 dQэ по(отр)
DO CIKL WITH getIndexOfIOutM("90"),'iom(getIndexOfIIoM("46"),i)'                                          &&90 dQэ то(отр)
DO CIKL1 WITH getIndexOfIOutM("90"),iom(getIndexOfIIoM("46"),n_blokov+1),'    -',iom(getIndexOfIIoM("46"),n_blokov+1),'    -'&&90 dQэ то(отр)
DO CIKL WITH getIndexOfIOutM("91"),'iom(getIndexOfIIoM("135"),i)'                                         &&91 Kотр(к) э
sum=0
FOR i=1 TO n_blokov
    sum=sum+oum(getIndexOfIOutM("91"),i)*oum(getIndexOfIOutM("79"),i)
ENDFOR
DO CIKL1 WITH getIndexOfIOutM("91"),sum/SUM('oum(getIndexOfIOutM("79"),'),'    -',sum/SUM('oum(getIndexOfIOutM("79"),'),'    -'&&91 Kотр(к) э
DO CIKL WITH getIndexOfIOutM("92"),'iom(getIndexOfIIoM("135"),i)'                                         &&92 Kотр(к) тэ
STORE 0 TO sum1,sum2
FOR i=1 TO n_blokov
    sum1=sum1+(oum(getIndexOfIOutM("75"),i)-oum(getIndexOfIOutM("79"),i)-oum(getIndexOfIOutM("81"),i)*oum(getIndexOfIOutM("27"),i)/1E3)*oum(getIndexOfIOutM("92"),i)
    sum2=sum2+oum(getIndexOfIOutM("75"),i)-oum(getIndexOfIOutM("79"),i)-oum(getIndexOfIOutM("81"),i)*oum(getIndexOfIOutM("27"),i)/1E3
ENDFOR
DO CIKL1 WITH getIndexOfIOutM("92"),sum1/sum2,'    -',sum1/sum2,'    -'                      &&92 Kотр(к) тэ
DO CIKL WITH getIndexOfIOutM("93"),'iom(getIndexOfIIoM("56.1"),i)'                                        &&93 Pо
sum=0
FOR i=1 TO n_blokov
    sum=sum+oum(getIndexOfIOutM("93"),i)*oum(getIndexOfIOutM("70"),i)
ENDFOR
DO CIKL1 WITH getIndexOfIOutM("93"),sum/SUM('oum(getIndexOfIOutM("70"),'),'    -',sum/SUM('oum(getIndexOfIOutM("70"),'),'    -'&&93 Pо
DO CIKL WITH getIndexOfIOutM("94"),'F1(iom(getIndexOfIIoM("50"),i),"2.65а:1")'                            &&94 Pо (н)
sum=0
FOR i=1 TO n_blokov
    sum=sum+oum(getIndexOfIOutM("94"),i)*oum(getIndexOfIOutM("70"),i)
ENDFOR
DO CIKL1 WITH getIndexOfIOutM("94"),sum/SUM('oum(getIndexOfIOutM("70"),'),'    -',sum/SUM('oum(getIndexOfIOutM("70"),'),'    -'&&94 Pо (н)
DO CIKL WITH getIndexOfIOutM("95"),'IIF(inm(getIndexOfIInM("74"),i)=="1",F2(iom(getIndexOfIIoM("50"),i),oum(getIndexOfIOutM("93"),i),"2.66:2"),'+;
'IIF(inm(getIndexOfIInM("74"),i)=="2" OR inm(getIndexOfIInM("74"),i)=="2а" OR inm(getIndexOfIInM("74"),i)=="3",'+;
'IIF(iom(getIndexOfIIoM("10"),i)<=60,IIF(iom(getIndexOfIIoM("50"),i)<=510,IIF(oum(getIndexOfIOutM("93"),i)<=130,-0.32,0.32)*ABS(oum(getIndexOfIOutM("93"),i)-oum(getIndexOfIOutM("94"),i)),'+;
                                               'IIF(oum(getIndexOfIOutM("93"),i)<=130,0.7,-0.7)*ABS(oum(getIndexOfIOutM("93"),i)-oum(getIndexOfIOutM("94"),i))'+;
                          '),'+;
                        'IIF(iom(getIndexOfIIoM("50"),i)<=510,IIF(oum(getIndexOfIOutM("93"),i)<=130,-0.37,0.37)*ABS(oum(getIndexOfIOutM("93"),i)-oum(getIndexOfIOutM("94"),i)),'+;
                                                'IIF(oum(getIndexOfIOutM("93"),i)<=125,0.76,IIF(oum(getIndexOfIOutM("93"),i)<=130,1.04,-0.8))*'+;
                                                'ABS(oum(getIndexOfIOutM("93"),i)-oum(getIndexOfIOutM("94"),i))'+;
                          ')'+;
    '),1/0))'                                                                  &&95 alfa qт(Pо)
DO CIKL WITH getIndexOfIOutM("96"),'inm(getIndexOfIInM("23"),i)'                                          &&96 Pп
sum=0
FOR i=1 TO n_blokov
    sum=sum+oum(getIndexOfIOutM("96"),i)*oum(getIndexOfIOutM("6"),i)
ENDFOR
DO CIKL1 WITH getIndexOfIOutM("96"),sum/SUM('oum(getIndexOfIOutM("6"),'),'    -',sum/SUM('oum(getIndexOfIOutM("6"),'),'    -'&&96 Pп
DO CIKL WITH getIndexOfIOutM("97"),'IIF(inm(getIndexOfIInM("74"),i)=="2а",inm(getIndexOfIInM("38"),i),inm(getIndexOfIInM("37"),i))' &&97 Pт
sum=0
FOR i=1 TO n_blokov
    sum=sum+oum(getIndexOfIOutM("97"),i)*oum(getIndexOfIOutM("7"),i)
ENDFOR
DO CIKL1 WITH getIndexOfIOutM("97"),sum/SUM('oum(getIndexOfIOutM("7"),'),'    -',sum/SUM('oum(getIndexOfIOutM("7"),'),'    -'&&97 Pт
DO CIKL WITH getIndexOfIOutM("98"),'iom(getIndexOfIIoM("51.1"),i)'                                        &&98 t o
sum=0
FOR i=1 TO n_blokov
    sum=sum+oum(getIndexOfIOutM("98"),i)*oum(getIndexOfIOutM("4"),i)
ENDFOR
DO CIKL1 WITH getIndexOfIOutM("98"),sum/SUM('oum(getIndexOfIOutM("4"),'),'    -',sum/SUM('oum(getIndexOfIOutM("4"),'),'    -'&&98 t o
DO CIKL WITH getIndexOfIOutM("99"),'540'                                                     &&99 t o (н)
DO CIKL1 WITH getIndexOfIOutM("99"),540,'    -',540,'    -'                                  &&99 t o (н)
DO CIKL WITH getIndexOfIOutM("100"),'IIF(inm(getIndexOfIInM("74"),i)=="1",F1(oum(getIndexOfIOutM("98"),i),"2.70:1"),'+;
'IIF(inm(getIndexOfIInM("74"),i)=="2" OR inm(getIndexOfIInM("74"),i)=="2а" OR inm(getIndexOfIInM("74"),i)=="3",F3(iom(getIndexOfIIoM("50"),i),iom(getIndexOfIIoM("10"),i),oum(getIndexOfIOutM("98"),i),"2.71:3")'+;
',1/0))'                                                                       &&100 alfa qт(t о)
DO CIKL WITH getIndexOfIOutM("101"),'iom(getIndexOfIIoM("52.1"),i)'                                       &&101 t цсд
sum=0
FOR i=1 TO n_blokov
    sum=sum+oum(getIndexOfIOutM("101"),i)*oum(getIndexOfIOutM("4"),i)
ENDFOR
DO CIKL1 WITH getIndexOfIOutM("101"),sum/SUM('oum(getIndexOfIOutM("4"),'),'    -',sum/SUM('oum(getIndexOfIOutM("4"),'),'    -'&&101 t цсд
DO CIKL WITH getIndexOfIOutM("102"),'540'                                                    &&102 t цсд (н)
DO CIKL1 WITH getIndexOfIOutM("102"),540,'    -',540,'    -'                                 &&102 t цсд (н)
DO CIKL WITH getIndexOfIOutM("103"),'IIF(inm(getIndexOfIInM("74"),i)=="1",F1(oum(getIndexOfIOutM("101"),i),"2.72:1"),'+;
'IIF(inm(getIndexOfIInM("74"),i)=="2" OR inm(getIndexOfIInM("74"),i)=="2а" OR inm(getIndexOfIInM("74"),i)=="3",F3(iom(getIndexOfIIoM("50"),i),iom(getIndexOfIIoM("10"),i),oum(getIndexOfIOutM("101"),i),"2.73:3")'+;
',1/0))'                                                                       &&103 alfa qт(цдс)
DO CIKL WITH getIndexOfIOutM("104"),'oum(getIndexOfIOutM("4"),i)-oum(getIndexOfIOutM("50"),i)'                           &&104 Ёконд
DO CIKL1 WITH getIndexOfIOutM("104"),oum(getIndexOfIOutM("4"),n_blokov+1)-oum(getIndexOfIOutM("50"),n_blokov+1),'    -',;
oum(getIndexOfIOutM("4"),n_blokov+1)-oum(getIndexOfIOutM("50"),n_blokov+1),'    -'                         &&104 Ёконд
*F BL5
*O CIKL WITH getIndexOfIOutM("105"),'(1-inm(getIndexOfIInM("30"),i)/100)*735.6/inm(getIndexOfIInM("90"),n_blokov+1)'   &&105 P2
*LSE
DO CIKL WITH getIndexOfIOutM("105"),'inm(getIndexOfIInM("30"),i)/98.067'                                  &&105 P2
*O ALTERC WITH getIndexOfIOutM("105"),1,'inm(getIndexOfIInM("90"),n_blokov+1)/735.6-inm(getIndexOfIInM("30"),i)'       &&105 P2
*O ALTERC WITH getIndexOfIOutM("105"),5,'inm(getIndexOfIInM("90"),n_blokov+1)/735.6-inm(getIndexOfIInM("30"),i)'       &&105 P2
DO ALTERC WITH getIndexOfIOutM("105"),6,'inm(getIndexOfIInM("30"),i)'                                     &&105 P2
*NDIF
*O ALTERC WITH getIndexOfIOutM("105"),1,'F1((inm(getIndexOfIInM("41.1"),i)+inm(getIndexOfIInM("41.2"),i))/2,"2.50:1")' &&105 P2
sum=0
FOR i=1 TO n_blokov
    sum=sum+oum(getIndexOfIOutM("105"),i)*oum(getIndexOfIOutM("104"),i)
ENDFOR
DO CIKL1 WITH getIndexOfIOutM("105"),sum/SUM('oum(getIndexOfIOutM("104"),'),'    -',sum/SUM('oum(getIndexOfIOutM("104"),'),'    -'&&105 P2
DO CIKL WITH getIndexOfIOutM("106"),'iom(getIndexOfIIoM("15"),i)'                                         &&106 P2(н)
sum=0
FOR i=1 TO n_blokov
    sum=sum+oum(getIndexOfIOutM("106"),i)*oum(getIndexOfIOutM("104"),i)
ENDFOR
DO CIKL1 WITH getIndexOfIOutM("106"),sum/SUM('oum(getIndexOfIOutM("104"),'),'    -',sum/SUM('oum(getIndexOfIOutM("104"),'),'    -'&&106 P2(н)
DO CIKL WITH getIndexOfIOutM("107"),'IIF(iom(getIndexOfIIoM("14"),i)<=100,F1(iom(getIndexOfIIoM("14"),i),"2.45:1")*(oum(getIndexOfIOutM("105"),i)-oum(getIndexOfIOutM("106"),i))/.01,'+;
'IIF(iom(getIndexOfIIoM("14"),i)>100,1.06*(oum(getIndexOfIOutM("105"),i)-oum(getIndexOfIOutM("106"),i))/.01,1/0))'      &&107 dN(P2)
DO CIKL WITH getIndexOfIOutM("108"),'1.929*oum(getIndexOfIOutM("107"),i)'                                  &&108 dQэ(P2)
DO CIKL WITH getIndexOfIOutM("109"),'inm(getIndexOfIInM("28"),i)'                                         &&109 t 1
sum=0
FOR i=1 TO n_blokov
    sum=sum+oum(getIndexOfIOutM("109"),i)*oum(getIndexOfIOutM("104"),i)
ENDFOR
DO CIKL1 WITH getIndexOfIOutM("109"),sum/SUM('oum(getIndexOfIOutM("104"),'),'    -',sum/SUM('oum(getIndexOfIOutM("104"),'),'    -'&&109 t 1
DO CIKL WITH getIndexOfIOutM("110"),'inm(getIndexOfIInM("29"),i)'                                         &&110 t 2
sum=0
FOR i=1 TO n_blokov
    sum=sum+oum(getIndexOfIOutM("110"),i)*oum(getIndexOfIOutM("104"),i)
ENDFOR
DO CIKL1 WITH getIndexOfIOutM("110"),sum/SUM('oum(getIndexOfIOutM("104"),'),'    -',sum/SUM('oum(getIndexOfIOutM("104"),'),'    -'&&110 t 2
DO CIKL WITH getIndexOfIOutM("111"),'F1(oum(getIndexOfIOutM("105"),i),"2.75:1")'                           &&111 t к
DO CIKL WITH getIndexOfIOutM("112"),'oum(getIndexOfIOutM("111"),i)-oum(getIndexOfIOutM("110"),i)'                        &&112 dt
sum=0
FOR i=1 TO n_blokov
    sum=sum+oum(getIndexOfIOutM("112"),i)*oum(getIndexOfIOutM("104"),i)
ENDFOR
DO CIKL1 WITH getIndexOfIOutM("112"),sum/SUM('oum(getIndexOfIOutM("104"),'),'    -',sum/SUM('oum(getIndexOfIOutM("104"),'),'    -'&&112 dt
DO CIKL WITH getIndexOfIOutM("113"),'F3(iom(getIndexOfIIoM("14"),i),oum(getIndexOfIOutM("109"),i),iom(getIndexOfIIoM("14.1"),n_blokov+1),"2.64:3")'&&113 dt (н)
sum=0
FOR i=1 TO n_blokov
    sum=sum+oum(getIndexOfIOutM("113"),i)*oum(getIndexOfIOutM("104"),i)
ENDFOR
DO CIKL1 WITH getIndexOfIOutM("113"),sum/SUM('oum(getIndexOfIOutM("104"),'),'    -',sum/SUM('oum(getIndexOfIOutM("104"),'),'    -'&&113 dt (н)
DO CIKL WITH getIndexOfIOutM("114"),'oum(getIndexOfIOutM("112"),i)-oum(getIndexOfIOutM("113"),i)'                        &&114 dt''
DO CIKL WITH getIndexOfIOutM("115"),'oum(getIndexOfIOutM("111"),i)-oum(getIndexOfIOutM("114"),i)'                        &&115 t к ''
DO CIKL WITH getIndexOfIOutM("116"),'F1(oum(getIndexOfIOutM("115"),i),"2.76:1")'                           &&116 P2 ''
DO CIKL WITH getIndexOfIOutM("117"),'IIF(iom(getIndexOfIIoM("14"),i)<=100,F1(iom(getIndexOfIIoM("14"),i),"2.45:1")*(oum(getIndexOfIOutM("105"),i)-oum(getIndexOfIOutM("116"),i))/.01,'+;
'IIF(iom(getIndexOfIIoM("14"),i)>100,1.06*(oum(getIndexOfIOutM("105"),i)-oum(getIndexOfIOutM("116"),i))/.01,1/0))'      &&117 dN (P2 '')
*O CIKL WITH getIndexOfIOutM("118"),'IIF(iom(getIndexOfIIoM("9"),i)<=175.4,1.91*oum(getIndexOfIOutM("117"),i),'+;
*IIF(iom(getIndexOfIIoM("9"),i)>175.4,1.831*oum(getIndexOfIOutM("117"),i),1/0))'                          &&118 dQ (P2 '') 
DO CIKL WITH getIndexOfIOutM("118"),'1.929*oum(getIndexOfIOutM("117"),i)'                                  &&118 dQ (P2 '') 
DO CIKL WITH getIndexOfIOutM("119"),'inm(getIndexOfIInM("26"),i)'                                         &&119 t пв
sum=0
FOR i=1 TO n_blokov
    sum=sum+oum(getIndexOfIOutM("119"),i)*oum(getIndexOfIOutM("44"),i)
ENDFOR
DO CIKL1 WITH getIndexOfIOutM("119"),sum/SUM('oum(getIndexOfIOutM("44"),'),'    -',sum/SUM('oum(getIndexOfIOutM("44"),'),'    -'&&119 t пв
DO CIKL WITH getIndexOfIOutM("120"),'iom(getIndexOfIIoM("74"),i)'                                         &&120 t пв (н)
DO CIKL1 WITH getIndexOfIOutM("120"),iom(getIndexOfIIoM("74"),n_blokov+1),'    -',iom(getIndexOfIIoM("74"),n_blokov+1),'    -'&&120 t пв (н)
DO CIKL WITH getIndexOfIOutM("121"),'(oum(getIndexOfIOutM("120"),i)-oum(getIndexOfIOutM("119"),i))',.F.                  &&121 alfa qт(tпв)
DO CIKL WITH getIndexOfIOutM("121"),'IIF(inm(getIndexOfIInM("74"),i)=="1",F1(oum(getIndexOfIOutM("121"),i),"2.77:1"),'+;
'IIF(inm(getIndexOfIInM("74"),i)=="2" OR inm(getIndexOfIInM("74"),i)=="2а" OR inm(getIndexOfIInM("74"),i)=="3",F2(iom(getIndexOfIIoM("50"),i),iom(getIndexOfIIoM("10"),i),"2.78:2")*oum(getIndexOfIOutM("121"),i)/2,'+;
'1/0))'                                                                        &&121 alfa qт(tпв)
DO CIKL WITH getIndexOfIOutM("123"),'IIF(inm(getIndexOfIInM("59"),i)=100,inm(getIndexOfIInM("8.2"),i)+inm(getIndexOfIInM("8.4"),i),'+;
'inm(getIndexOfIInM("8.2"),i)+inm(getIndexOfIInM("8.4"),i)/inm(getIndexOfIInM("1"),i)*(inm(getIndexOfIInM("1"),i)-inm(getIndexOfIInM("70.1"),i))*'+;
'.35+inm(getIndexOfIInM("8.4"),i)/inm(getIndexOfIInM("1"),i)*inm(getIndexOfIInM("70.1"),i))'                          &&123 Ё тд
DO CIKL1 WITH getIndexOfIOutM("123"),SUM('oum(getIndexOfIOutM("123"),'),'    -',SUM('oum(getIndexOfIOutM("123"),'),'    -'&&123 Ё тд
DO CIKL WITH getIndexOfIOutM("124"),'iom(getIndexOfIIoM("95"),i)*oum(getIndexOfIOutM("17"),i)/1E3'                      &&124 Ё тд (н)
DO CIKL1 WITH getIndexOfIOutM("124"),SUM('oum(getIndexOfIOutM("124"),'),'    -',SUM('oum(getIndexOfIOutM("124"),'),'    -'&&124 Ё тд (н)
DO CIKL WITH getIndexOfIOutM("126"),'oum(getIndexOfIOutM("78"),i)*7000/inm(getIndexOfIInM("53"),n_blokov+1)'            &&126 B уг
DO CIKL1 WITH getIndexOfIOutM("126"),SUM('oum(getIndexOfIOutM("126"),'),'    -',SUM('oum(getIndexOfIOutM("126"),'),'    -'&&126 B уг
DO CIKL WITH getIndexOfIOutM("127"),'iom(getIndexOfIIoM("98"),i)*oum(getIndexOfIOutM("126"),i)/1E3'                     &&127 Ё пп (н)
DO CIKL1 WITH getIndexOfIOutM("127"),SUM('oum(getIndexOfIOutM("127"),'),'    -',SUM('oum(getIndexOfIOutM("127"),'),'    -'&&127 Ё пп (н)
IF BL1 OR BL2 AND RealTime && уникальный пересчет доли газа только дл€ параметра '/1' и '/2' в реальн.врем.
DO CIKL WITH getIndexOfIOutM("125"),'IIF(BETW(inm(getIndexOfIInM("59"),i),90,99.99),oum(getIndexOfIOutM("127"),i),'+;
'IIF(inm(getIndexOfIInM("59"),i)=100,0,inm(getIndexOfIInM("8.3"),i)+inm(getIndexOfIInM("8.4"),i)*0.65))'              &&125 Ё пп
ELSE
DO CIKL WITH getIndexOfIOutM("125"),'IIF(inm(getIndexOfIInM("59"),i)=100,0,inm(getIndexOfIInM("8.3"),i)+inm(getIndexOfIInM("8.4"),i)/'+;
'inm(getIndexOfIInM("1"),i)*(inm(getIndexOfIInM("1"),i)-inm(getIndexOfIInM("70.1"),i))*.65)'                          &&125 Ё пп
ENDIF               && уникальный пересчет доли газа только дл€ параметра '/1' и '/2' в реальн.врем.
DO CIKL1 WITH getIndexOfIOutM("125"),SUM('oum(getIndexOfIOutM("125"),'),'    -',SUM('oum(getIndexOfIOutM("125"),'),'    -'&&125 Ё пп
DO CIKL WITH getIndexOfIOutM("129"),'iom(getIndexOfIIoM("9"),i)'                                          &&129 Qт ср
DO CIKL1 WITH getIndexOfIOutM("129"),iom(getIndexOfIIoM("9"),n_blokov+1),'    -',SUM('oum(getIndexOfIOutM("129"),'),'    -'&&129 Qт ср
DO CIKL WITH getIndexOfIOutM("130"),'iom(getIndexOfIIoM("10"),i)'                                         &&130 Qт ср
DO CIKL1 WITH getIndexOfIOutM("130"),iom(getIndexOfIIoM("10"),n_blokov+1),'    -',SUM('oum(getIndexOfIOutM("130"),'),'    -'&&130 Qт ср
DO CIKL WITH getIndexOfIOutM("131"),'iom(getIndexOfIIoM("4"),i)/iom(getIndexOfIIoM("1"),i)'                            &&131 Q пр ср
DO CIKL1 WITH getIndexOfIOutM("131"),iom(getIndexOfIIoM("4"),n_blokov+1)/iom(getIndexOfIIoM("1"),n_blokov+1),'    -',;
SUM('oum(getIndexOfIOutM("131"),'),'    -'                                                   &&131 Q пр ср
DO CIKL WITH getIndexOfIOutM("132"),'oum(getIndexOfIOutM("130"),i)+oum(getIndexOfIOutM("131"),i)'                        &&132 Q сум ср
DO CIKL1 WITH getIndexOfIOutM("132"),oum(getIndexOfIOutM("130"),n_blokov+1)+oum(getIndexOfIOutM("131"),n_blokov+1),'    -',;
SUM('oum(getIndexOfIOutM("132"),'),'    -'                                                   &&132 Q сум ср
DO CIKL1 WITH getIndexOfIOutM("133"),oum(getIndexOfIOutM("13"),n_blokov+1)/oum(getIndexOfIOutM("11"),n_blokov+3)*100,'    -','    -',;
'    -'                                                                        &&133 alfa отр
DO CIKL1 WITH getIndexOfIOutM("134"),oum(getIndexOfIOutM("12"),n_blokov+3)/oum(getIndexOfIOutM("11"),n_blokov+3)*100,'    -','    -',;
'    -'                                                                        &&134 alfa г.в.
DO CIKL WITH getIndexOfIOutM("135"),'oum(getIndexOfIOutM("30"),i)/oum(getIndexOfIOutM("4"),i)*100'                       &&135 Ёсн
DO CIKL1 WITH getIndexOfIOutM("135"),oum(getIndexOfIOutM("30"),n_blokov+1)/oum(getIndexOfIOutM("4"),n_blokov+1)*100,'    -','    -',;
'    -'                                                                        &&135 Ёсн
DO CIKL WITH getIndexOfIOutM("136"),'(oum(getIndexOfIOutM("36"),i)+oum(getIndexOfIOutM("37"),i)+oum(getIndexOfIOutM("38"),i))/oum(getIndexOfIOutM("4"),i)*100'&&136 Ёсн (н)
DO CIKL1 WITH getIndexOfIOutM("136"),(oum(getIndexOfIOutM("36"),n_blokov+1)+oum(getIndexOfIOutM("37"),n_blokov+1)+;
oum(getIndexOfIOutM("38"),n_blokov+1))/oum(getIndexOfIOutM("4"),n_blokov+1)*100,'    -','    -','    -'    &&136 Ёсн (н)
DO CIKL WITH getIndexOfIOutM("137"),'oum(getIndexOfIOutM("34"),i)/oum(getIndexOfIOutM("4"),i)*100'                       &&137
DO CIKL1 WITH getIndexOfIOutM("137"),oum(getIndexOfIOutM("34"),n_blokov+1)/oum(getIndexOfIOutM("4"),n_blokov+1)*100,'    -',;
'    -','    -'                                                                &&137
DO CIKL WITH getIndexOfIOutM("138"),'oum(getIndexOfIOutM("39"),i)/oum(getIndexOfIOutM("4"),i)*100'                       &&138 Ёэ сн(н)
DO CIKL1 WITH getIndexOfIOutM("138"),oum(getIndexOfIOutM("39"),n_blokov+1)/oum(getIndexOfIOutM("4"),n_blokov+1)*100,'    -',;
'    -','    -'                                                                &&138 Ёэ сн(н)
DO CIKL WITH getIndexOfIOutM("139"),'oum(getIndexOfIOutM("35"),i)/oum(getIndexOfIOutM("11"),i)*1E3'                      &&139 Ётэ сн
DO CIKL1 WITH getIndexOfIOutM("139"),oum(getIndexOfIOutM("35"),n_blokov+1)/oum(getIndexOfIOutM("11"),n_blokov+1)*1E3,'    -',;
'    -','    -'                                                                &&139 Ётэ сн
DO CIKL WITH getIndexOfIOutM("140"),'oum(getIndexOfIOutM("40"),i)/oum(getIndexOfIOutM("11"),i)*1E3'                      &&140 Ётэ сн(н)
DO CIKL1 WITH getIndexOfIOutM("140"),oum(getIndexOfIOutM("40"),n_blokov+1)/oum(getIndexOfIOutM("11"),n_blokov+1)*1E3,'    -',;
'    -','    -'                                                                &&140 Ётэ сн(н)
DO CIKL WITH getIndexOfIOutM("141"),'oum(getIndexOfIOutM("4"),i)/200/inm(getIndexOfIInM("70"),n_blokov+1)*100'          &&141 Kи э
DO CIKL1 WITH getIndexOfIOutM("141"),oum(getIndexOfIOutM("4"),n_blokov+1)/1200/inm(getIndexOfIInM("70"),n_blokov+1)*100,'    -',;
'    -','    -'                                                                &&141 Kи э
DO CIKL WITH getIndexOfIOutM("142"),'oum(getIndexOfIOutM("9"),i)/240/inm(getIndexOfIInM("70"),n_blokov+1)*100'          &&142 Kи тэ
DO CIKL1 WITH getIndexOfIOutM("142"),oum(getIndexOfIOutM("9"),n_blokov+1)/1440/inm(getIndexOfIInM("70"),n_blokov+1)*100,'    -',;
'    -','    -'                                                                &&142 Kи тэ
DO CIKL WITH getIndexOfIOutM("143"),'(1-oum(getIndexOfIOutM("106"),i))*100'                                &&143 V (н)
DO CIKL1 WITH getIndexOfIOutM("143"),(1-oum(getIndexOfIOutM("106"),n_blokov+1))*100,'    -',;
'    -','    -'                                                                &&143 V (н)
DO CIKL WITH getIndexOfIOutM("144"),'(1-oum(getIndexOfIOutM("105"),i))*100'                                &&144 V
DO CIKL1 WITH getIndexOfIOutM("144"),(1-oum(getIndexOfIOutM("105"),n_blokov+1))*100,'    -',;
'    -','    -'                                                                &&144 V
DO CIKL WITH getIndexOfIOutM("145"),'oum(getIndexOfIOutM("16"),i)/oum(getIndexOfIOutM("15"),i)*1E2'                    &&145 qт сн(н)
DO CIKL1 WITH getIndexOfIOutM("145"),oum(getIndexOfIOutM("16"),n_blokov+1)/oum(getIndexOfIOutM("15"),n_blokov+1)*1E2,'    -',;
'    -','    -'                                                                &&145 qт сн(н)
DO CIKL WITH getIndexOfIOutM("146"),'oum(getIndexOfIOutM("16.1"),i)/oum(getIndexOfIOutM("15"),i)*1E2'                      &&146 qт сн
DO CIKL1 WITH getIndexOfIOutM("146"),oum(getIndexOfIOutM("16.1"),n_blokov+1)/oum(getIndexOfIOutM("15"),n_blokov+1)*1E2,'    -',;
'    -','    -'                                                                &&146 qт сн
DO CIKL WITH getIndexOfIOutM("147"),'oum(getIndexOfIOutM("36"),i)/oum(getIndexOfIOutM("4"),i)*1E2'                       &&147 Ёт сн(н)
DO CIKL1 WITH getIndexOfIOutM("147"),oum(getIndexOfIOutM("36"),n_blokov+1)/oum(getIndexOfIOutM("4"),n_blokov+1)*1E2,'    -',;
'    -','    -'                                                                &&147 Ёт сн(н)
DO CIKL WITH getIndexOfIOutM("148"),'oum(getIndexOfIOutM("28"),i)/oum(getIndexOfIOutM("4"),i)*1E2'                       &&148 Ёт сн
DO CIKL1 WITH getIndexOfIOutM("148"),oum(getIndexOfIOutM("28"),n_blokov+1)/oum(getIndexOfIOutM("4"),n_blokov+1)*1E2,'    -',;
'    -','    -'                                                                &&148 Ёт сн
DO CIKL WITH getIndexOfIOutM("149"),'oum(getIndexOfIOutM("42"),i)/oum(getIndexOfIOutM("104"),i)*1E2'                     &&149 Ёцн (н)
DO CIKL1 WITH getIndexOfIOutM("149"),oum(getIndexOfIOutM("42"),n_blokov+1)/oum(getIndexOfIOutM("104"),n_blokov+1)*1E2,'    -',;
'    -','    -'                                                                &&149 Ёцн (н)
DO CIKL WITH getIndexOfIOutM("150"),'oum(getIndexOfIOutM("41"),i)/oum(getIndexOfIOutM("104"),i)*1E2'                     &&150 Ёцн
DO CIKL1 WITH getIndexOfIOutM("150"),oum(getIndexOfIOutM("41"),n_blokov+1)/oum(getIndexOfIOutM("104"),n_blokov+1)*1E2,'    -',;
'    -','    -'                                                                &&150 Ёцн
DO CIKL WITH getIndexOfIOutM("151"),'oum(getIndexOfIOutM("56"),i)*(100+oum(getIndexOfIOutM("145"),i))/(100-oum(getIndexOfIOutM("147"),i))'&&151 qт н(н)
DO CIKL1 WITH getIndexOfIOutM("151"),oum(getIndexOfIOutM("56"),n_blokov+1)*(100+oum(getIndexOfIOutM("145"),n_blokov+1))/;
(100-oum(getIndexOfIOutM("147"),n_blokov+1)),'    -','    -','    -'                         &&151 qт н(н)
DO CIKL WITH getIndexOfIOutM("152"),'oum(getIndexOfIOutM("55"),i)*(100+oum(getIndexOfIOutM("146"),i))/(100-oum(getIndexOfIOutM("148"),i))'&&152 qт н
DO CIKL1 WITH getIndexOfIOutM("152"),oum(getIndexOfIOutM("55"),n_blokov+1)*(100+oum(getIndexOfIOutM("146"),n_blokov+1))/;
(100-oum(getIndexOfIOutM("148"),n_blokov+1)),'    -','    -','    -'                         &&152 qт н
DO CIKL WITH getIndexOfIOutM("153"),'oum(getIndexOfIOutM("38"),i)/oum(getIndexOfIOutM("11"),i)*1E3'                      &&153 Ётепл (н)
DO CIKL1 WITH getIndexOfIOutM("153"),oum(getIndexOfIOutM("38"),n_blokov+1)/oum(getIndexOfIOutM("11"),n_blokov+1)*1E3,'    -',;
'    -','    -'                                                                &&153 Ётепл (н)
DO CIKL WITH getIndexOfIOutM("154"),'oum(getIndexOfIOutM("27"),i)/oum(getIndexOfIOutM("11"),i)*1E3'                      &&154 Ётепл
DO CIKL1 WITH getIndexOfIOutM("154"),oum(getIndexOfIOutM("27"),n_blokov+1)/oum(getIndexOfIOutM("11"),n_blokov+1)*1E3,'    -',;
'    -','    -'                                                                &&154 Ётепл
DO CIKL WITH getIndexOfIOutM("155"),'oum(getIndexOfIOutM("37"),i)/oum(getIndexOfIOutM("4"),i)*1E2'                       &&155 Ёк сн(н)
DO CIKL1 WITH getIndexOfIOutM("155"),oum(getIndexOfIOutM("37"),n_blokov+1)/oum(getIndexOfIOutM("4"),n_blokov+1)*1E2,'    -','    -','    -'                                                                &&155 Ёк сн(н)
DO CIKL WITH getIndexOfIOutM("156"),'oum(getIndexOfIOutM("29"),i)/oum(getIndexOfIOutM("4"),i)*1E2'                       &&156 Ёк сн
DO CIKL1 WITH getIndexOfIOutM("156"),oum(getIndexOfIOutM("29"),n_blokov+1)/oum(getIndexOfIOutM("4"),n_blokov+1)*1E2,'    -','    -','    -'                                                                &&156 Ёк сн
DO CIKL WITH getIndexOfIOutM("157"),'oum(getIndexOfIOutM("127"),i)/oum(getIndexOfIOutM("126"),i)*1E3'                    &&157 Ёпп(н)
DO CIKL1 WITH getIndexOfIOutM("157"),oum(getIndexOfIOutM("127"),n_blokov+1)/oum(getIndexOfIOutM("126"),n_blokov+1)*1E3,'    -',;
'    -','    -'                                                                &&157 Ёпп(н)
DO CIKL WITH getIndexOfIOutM("158"),'oum(getIndexOfIOutM("125"),i)/oum(getIndexOfIOutM("126"),i)*1E3'                    &&158 Ёпп 
DO CIKL1 WITH getIndexOfIOutM("158"),oum(getIndexOfIOutM("125"),n_blokov+1)/oum(getIndexOfIOutM("126"),n_blokov+1)*1E3,'    -',;
'    -','    -'                                                                &&158 Ёпп 
DO CIKL WITH getIndexOfIOutM("159"),'oum(getIndexOfIOutM("45"),i)/oum(getIndexOfIOutM("44"),i)'                          &&159 Ёпэн (н)
DO CIKL1 WITH getIndexOfIOutM("159"),oum(getIndexOfIOutM("45"),n_blokov+1)/oum(getIndexOfIOutM("44"),n_blokov+1),'    -',;
'    -','    -'                                                                &&159 Ёпэн (н)
DO CIKL WITH getIndexOfIOutM("160"),'oum(getIndexOfIOutM("43"),i)/oum(getIndexOfIOutM("44"),i)'                          &&160 Ёпэн
DO CIKL1 WITH getIndexOfIOutM("160"),oum(getIndexOfIOutM("43"),n_blokov+1)/oum(getIndexOfIOutM("44"),n_blokov+1),'    -',;
'    -','    -'                                                                &&160 Ёпэн
DO CIKL WITH getIndexOfIOutM("161"),'oum(getIndexOfIOutM("124"),i)/oum(getIndexOfIOutM("17"),i)*1E3'                     &&161 Ётд (н)
DO CIKL1 WITH getIndexOfIOutM("161"),oum(getIndexOfIOutM("124"),n_blokov+1)/oum(getIndexOfIOutM("17"),n_blokov+1)*1E3,'    -',;
'    -','    -'                                                                &&161 Ётд (н)
DO CIKL WITH getIndexOfIOutM("162"),'oum(getIndexOfIOutM("123"),i)/oum(getIndexOfIOutM("17"),i)*1E3'                     &&162 Ётд
DO CIKL1 WITH getIndexOfIOutM("162"),oum(getIndexOfIOutM("123"),n_blokov+1)/oum(getIndexOfIOutM("17"),n_blokov+1)*1E3,'    -',;
'    -','    -'                                                                &&162 Ётд
DO CIKL WITH getIndexOfIOutM("163"),'iom(getIndexOfIIoM("50"),i)'                                         &&163 Dпе
DO CIKL WITH getIndexOfIOutM("163.1"),'iom(getIndexOfIIoM("66.1"),i)'                                     &&163.1 D пв ср
DO CIKL WITH getIndexOfIOutM("164"),'iom(getIndexOfIIoM("65"),i)'                                         &&164 Qк бр
DO CIKL1 WITH getIndexOfIOutM("164"),iom(getIndexOfIIoM("65"),n_blokov+1),'    -','    -','    -'         &&164 Qк бр
DO CIKL WITH getIndexOfIOutM("165"),'iom(getIndexOfIIoM("56"),i)'                                         &&165 Pк
sum=0
FOR i=1 TO n_blokov
    sum=sum+oum(getIndexOfIOutM("165"),i)*oum(getIndexOfIOutM("17"),i)
ENDFOR
DO CIKL1 WITH getIndexOfIOutM("165"),sum/SUM('oum(getIndexOfIOutM("17"),'),'    -','    -','    -'         &&165 Pк
*O CIKL WITH getIndexOfIOutM("166"),'(inm(getIndexOfIInM("17"),i)+inm(getIndexOfIInM("17.1"),i))/2'                    &&166 t к
DO CIKL WITH getIndexOfIOutM("166"),'iom(getIndexOfIIoM("51"),i)'                                         &&166 t к
*O ALTERC WITH getIndexOfIOutM("166"),1,'iom(getIndexOfIIoM("51"),i)'                                     &&166 t к
*O ALTERC WITH getIndexOfIOutM("166"),5,'iom(getIndexOfIIoM("51"),i)'                                     &&166 t к
sum=0
FOR i=1 TO n_blokov
    sum=sum+oum(getIndexOfIOutM("166"),i)*oum(getIndexOfIOutM("17"),i)
ENDFOR
DO CIKL1 WITH getIndexOfIOutM("166"),sum/SUM('oum(getIndexOfIOutM("17"),'),'    -','    -','    -'         &&166 t к
DO CIKL WITH getIndexOfIOutM("167"),'(inm(getIndexOfIInM("33"),i)+inm(getIndexOfIInM("34"),i))/2'                      &&167 t гв
sum=0
FOR i=1 TO n_blokov
    sum=sum+oum(getIndexOfIOutM("167"),i)*oum(getIndexOfIOutM("17"),i)
ENDFOR
DO CIKL1 WITH getIndexOfIOutM("167"),sum/SUM('oum(getIndexOfIOutM("17"),'),'    -','    -','    -'         &&167 t гв
DO CIKL WITH getIndexOfIOutM("168"),'F2(oum(getIndexOfIOutM("164"),i),inm(getIndexOfIInM("59"),i),"2.41:2")'            &&168 √ун (н)
DO ALTERC WITH getIndexOfIOutM("168"),3,'3.4'                                                &&168 √ун (н)
DO ALTERC WITH getIndexOfIOutM("168"),4,'3.4'                                                &&168 √ун (н)
DO ALTERC WITH getIndexOfIOutM("168"),5,'4.1'                                                &&168 √ун (н)
*O ALTERC WITH getIndexOfIOutM("168"),5,'F1(oum(getIndexOfIOutM("164"),i),"2.42:1")'                       &&168 √ун (н)
DO ALTERC WITH getIndexOfIOutM("168"),6,'F1(oum(getIndexOfIOutM("164"),i),"2.41:1(6)")'                    &&168 √ун (н)
sum=0
FOR i=1 TO n_blokov
    sum=sum+oum(getIndexOfIOutM("168"),i)*oum(getIndexOfIOutM("78"),i)
ENDFOR
DO CIKL1 WITH getIndexOfIOutM("168"),sum/SUM('oum(getIndexOfIOutM("78"),'),'    -','    -','    -'         &&168 √ун (н)
*DO CIKL WITH getIndexOfIOutM("169"),'IIF(inm(getIndexOfIInM("59"),i)=100,0,inm(getIndexOfIInM("57"),i))'               &&169 √ун
DO CIKL WITH getIndexOfIOutM("169"),'inm(getIndexOfIInM("57"),i)*iom(getIndexOfIIoM("89"),i)/1E2'                      &&169 √ун
sum=0
FOR i=1 TO n_blokov
    sum=sum+oum(getIndexOfIOutM("169"),i)*oum(getIndexOfIOutM("78"),i)
ENDFOR
DO CIKL1 WITH getIndexOfIOutM("169"),sum/SUM('oum(getIndexOfIOutM("78"),'),'    -','    -','    -'         &&169 √ун
DO CIKL WITH getIndexOfIOutM("170"),'F2(oum(getIndexOfIOutM("164"),i),inm(getIndexOfIInM("59"),i),"2.43:2")'            &&170 √шл (н)
DO ALTERC WITH getIndexOfIOutM("170"),3,'F1(oum(getIndexOfIOutM("164"),i),"2.43:1")'                       &&170 √шл (н)
DO ALTERC WITH getIndexOfIOutM("170"),4,'F1(oum(getIndexOfIOutM("164"),i),"2.43:1")'                       &&170 √шл (н)
DO ALTERC WITH getIndexOfIOutM("170"),5,'4'                                                  &&170 √шл (н)
*O ALTERC WITH getIndexOfIOutM("170"),5,'F1(oum(getIndexOfIOutM("164"),i),"2.44:1")'                       &&170 √шл (н)
DO ALTERC WITH getIndexOfIOutM("170"),6,'F1(oum(getIndexOfIOutM("164"),i),"2.43:1(6)")'                    &&170 √шл (н)
sum=0
FOR i=1 TO n_blokov
    sum=sum+oum(getIndexOfIOutM("170"),i)*oum(getIndexOfIOutM("78"),i)
ENDFOR
DO CIKL1 WITH getIndexOfIOutM("170"),sum/SUM('oum(getIndexOfIOutM("78"),'),'    -','    -','    -'         &&170 √шл (н)
*DO CIKL WITH getIndexOfIOutM("171"),'IIF(inm(getIndexOfIInM("59"),i)=100,0,inm(getIndexOfIInM("56"),i))'              &&171 √шл
DO CIKL WITH getIndexOfIOutM("171"),'inm(getIndexOfIInM("56"),i)*iom(getIndexOfIIoM("89"),i)/1E2'                      &&171 √шл
sum=0
FOR i=1 TO n_blokov
    sum=sum+oum(getIndexOfIOutM("171"),i)*oum(getIndexOfIOutM("78"),i)
ENDFOR
DO CIKL1 WITH getIndexOfIOutM("171"),sum/SUM('oum(getIndexOfIOutM("78"),'),'    -','    -','    -'         &&171 √шл
DO CIKL WITH getIndexOfIOutM("172"),'100-oum(getIndexOfIOutM("68"),i)-oum(getIndexOfIOutM("57"),i)-oum(getIndexOfIOutM("73"),i)'       &&172 q прочие
DO CIKL1 WITH getIndexOfIOutM("172"),100-oum(getIndexOfIOutM("68"),n_blokov+1)-oum(getIndexOfIOutM("57"),n_blokov+1)-;
oum(getIndexOfIOutM("73"),n_blokov+1),'    -','    -','    -'                                &&172 q прочие
DO CIKL WITH getIndexOfIOutM("173"),'(oum(getIndexOfIOutM("62"),i)+10*SQRT(472.2/iom(getIndexOfIIoM("65"),i)))/100'     &&173 dalfa (н)
sum=0
FOR i=1 TO n_blokov
    sum=sum+oum(getIndexOfIOutM("173"),i)*oum(getIndexOfIOutM("17"),i)
ENDFOR
DO CIKL1 WITH getIndexOfIOutM("173"),sum/SUM('oum(getIndexOfIOutM("17"),'),'    -','    -','    -'         &&173 dalfa (н)
DO CIKL WITH getIndexOfIOutM("174"),'(oum(getIndexOfIOutM("61"),i)+oum(getIndexOfIOutM("63"),i))/100'                    &&174 dalfa
sum=0
FOR i=1 TO n_blokov
    sum=sum+oum(getIndexOfIOutM("174"),i)*oum(getIndexOfIOutM("17"),i)
ENDFOR
DO CIKL1 WITH getIndexOfIOutM("174"),sum/SUM('oum(getIndexOfIOutM("17"),'),'    -','    -','    -'         &&174 dalfa
DO CIKL WITH getIndexOfIOutM("175"),'oum(getIndexOfIOutM("18"),i)/oum(getIndexOfIOutM("17"),i)*100'                      &&175 q к сн (н)
DO CIKL1 WITH getIndexOfIOutM("175"),oum(getIndexOfIOutM("18"),n_blokov+1)/oum(getIndexOfIOutM("17"),n_blokov+1)*100,;
'    -','    -','    -'                                                        &&175 q к сн (н)
DO CIKL WITH getIndexOfIOutM("175.1"),'oum(getIndexOfIOutM("19"),i)/oum(getIndexOfIOutM("17"),i)*100'                    &&175.1 q к сн
DO CIKL1 WITH getIndexOfIOutM("175.1"),oum(getIndexOfIOutM("19"),n_blokov+1)/oum(getIndexOfIOutM("17"),n_blokov+1)*100,;
'    -','    -','    -'                                                        &&175.1 q к сн
DO CIKL WITH getIndexOfIOutM("176"),'oum(getIndexOfIOutM("73"),i)*(100-oum(getIndexOfIOutM("175.1"),i))/100*(100-oum(getIndexOfIOutM("137"),i))/'+;
'(100-oum(getIndexOfIOutM("148"),i))'                                                        &&176  ѕƒк"нетто"
DO CIKL1 WITH getIndexOfIOutM("176"),oum(getIndexOfIOutM("73"),n_blokov+1)*(100-oum(getIndexOfIOutM("175.1"),n_blokov+1))/100*;
(100-oum(getIndexOfIOutM("137"),n_blokov+1))/(100-oum(getIndexOfIOutM("148"),n_blokov+1)),'    -','    -','    -'&&176  ѕƒк"нетто"
DO CIKL WITH getIndexOfIOutM("177"),'oum(getIndexOfIOutM("74"),i)*(100-oum(getIndexOfIOutM("175"),i))/100*(100-oum(getIndexOfIOutM("138"),i))/'+;
'(100-oum(getIndexOfIOutM("147"),i))'                                                        &&177  ѕƒк"нетто"(н)
DO CIKL1 WITH getIndexOfIOutM("177"),oum(getIndexOfIOutM("74"),n_blokov+1)*(100-oum(getIndexOfIOutM("175"),n_blokov+1))/100*;
(100-oum(getIndexOfIOutM("138"),n_blokov+1))/(100-oum(getIndexOfIOutM("147"),n_blokov+1)),'    -','    -','    -'&&177  ѕƒк"нетто"(н)
DO CIKL1 WITH getIndexOfIOutM("178"),(inm(getIndexOfIInM("78"),n_blokov+1)+inm(getIndexOfIInM("79"),n_blokov+1))/1E3,'    -',;
'    -','    -'                                                                &&178 Gхов
DO CIKL1 WITH getIndexOfIOutM("179"),inm(getIndexOfIInM("80"),n_blokov+1),'    -','    -','    -'         &&179 Gпот (н)
DO CIKL1 WITH getIndexOfIOutM("179.1"),oum(getIndexOfIOutM("179"),n_blokov+1)*oum(getIndexOfIOutM("44"),n_blokov+1)/100,'    -','    -',;
'    -'                                                                        &&179.1 Gпот (н)
DO CIKL1 WITH getIndexOfIOutM("180"),oum(getIndexOfIOutM("178"),n_blokov+1)/oum(getIndexOfIOutM("44"),n_blokov+1)*1E2,'    -','    -',;
'    -'                                                                        &&180 Gпот
DO CIKL WITH getIndexOfIOutM("181"),'inm(getIndexOfIInM("27"),i)/1E3'                                     &&181 Gпрод
DO CIKL1 WITH getIndexOfIOutM("181"),SUM('inm(getIndexOfIInM("27"),')/1E3,'    -','    -','    -'         &&181 Gпрод
DO CIKL WITH getIndexOfIOutM("182"),'oum(getIndexOfIOutM("181"),i)/oum(getIndexOfIOutM("44"),i))*100'                    &&182 Gпрод
DO CIKL1 WITH getIndexOfIOutM("182"),oum(getIndexOfIOutM("181"),n_blokov+1)/oum(getIndexOfIOutM("44"),n_blokov+1)*100,'    -','    -',;
'    -'                                                                        &&182 Gпрод
DO CIKL WITH getIndexOfIOutM("183"),'iom(getIndexOfIIoM("130"),i)'                                        &&183 Kтп
DO CIKL1 WITH getIndexOfIOutM("183"),iom(getIndexOfIIoM("130"),n_blokov+1),'    -','    -','    -'        &&183 Kтп
DO CIKL WITH getIndexOfIOutM("95.1"),'oum(getIndexOfIOutM("95"),i)*oum(getIndexOfIOutM("4"),i)*10/(oum(getIndexOfIOutM("176"),i)*oum(getIndexOfIOutM("183"),i)*7)'&&95.1 dB(Pо)
DO CIKL1 WITH getIndexOfIOutM("95.1"),SUM('oum(getIndexOfIOutM("95.1"),'),'    -',SUM('oum(getIndexOfIOutM("95.1"),'),'    -'&&95.1 dB(Pо)
DO CIKL WITH getIndexOfIOutM("100.1"),'oum(getIndexOfIOutM("100"),i)*oum(getIndexOfIOutM("4"),i)*10/(oum(getIndexOfIOutM("176"),i)*oum(getIndexOfIOutM("183"),i)'+;
'*7)'                                                                          &&100.1 dBо(tо)
DO CIKL1 WITH getIndexOfIOutM("100.1"),SUM('oum(getIndexOfIOutM("100.1"),'),'    -',SUM('oum(getIndexOfIOutM("100.1"),'),'    -'&&100.1 dBо(tо)
DO CIKL WITH getIndexOfIOutM("103.1"),'IIF(inm(getIndexOfIInM("74"),i)=="1",oum(getIndexOfIOutM("81"),i)*oum(getIndexOfIOutM("31"),i)*oum(getIndexOfIOutM("103"),i)/'+;
'1E5,IIF(inm(getIndexOfIInM("74"),i)=="2" OR inm(getIndexOfIInM("74"),i)=="2а" OR inm(getIndexOfIInM("74"),i)=="3",oum(getIndexOfIOutM("103"),i)*'+;
'oum(getIndexOfIOutM("4"),i)*10/(oum(getIndexOfIOutM("176"),i)*oum(getIndexOfIOutM("183"),i)*7),1/0))'                   &&103.1 dB (tцсд)
DO CIKL1 WITH getIndexOfIOutM("103.1"),SUM('oum(getIndexOfIOutM("103.1"),'),'    -',SUM('oum(getIndexOfIOutM("103.1"),'),'    -'&&103.1 dB (tцсд)
DO CIKL WITH getIndexOfIOutM("108.1"),'oum(getIndexOfIOutM("108"),i)*oum(getIndexOfIOutM("20"),i)*1E4/(oum(getIndexOfIOutM("176"),i)*oum(getIndexOfIOutM("183"),i)'+;
'*7)'                                                                          &&108.1 dB(P2)
DO CIKL1 WITH getIndexOfIOutM("108.1"),SUM('oum(getIndexOfIOutM("108.1"),'),'    -',SUM('oum(getIndexOfIOutM("108.1"),'),'    -'&&108.1 dB(P2)
DO CIKL WITH getIndexOfIOutM("118.1"),'oum(getIndexOfIOutM("118"),i)*oum(getIndexOfIOutM("20"),i)*1E4/(oum(getIndexOfIOutM("176"),i)*oum(getIndexOfIOutM("183"),i)'+;
'*7)'                                                                          &&118.1 dB(dt)
DO CIKL1 WITH getIndexOfIOutM("118.1"),SUM('oum(getIndexOfIOutM("118.1"),'),'    -',SUM('oum(getIndexOfIOutM("118.1"),'),'    -'&&118.1 dB(dt)
DO CIKL WITH getIndexOfIOutM("121.1"),'oum(getIndexOfIOutM("121"),i)*oum(getIndexOfIOutM("4"),i)*10/(oum(getIndexOfIOutM("176"),i)*oum(getIndexOfIOutM("183"),i)*7)'&&121.1 dB (t пв)
DO CIKL1 WITH getIndexOfIOutM("121.1"),SUM('oum(getIndexOfIOutM("121.1"),'),'    -',SUM('oum(getIndexOfIOutM("121.1"),'),'    -'&&121.1 dB (t пв)
DO CIKL WITH getIndexOfIOutM("60.1"),'oum(getIndexOfIOutM("69"),i)*oum(getIndexOfIOutM("75"),i)/oum(getIndexOfIOutM("74"),i)*((iom(getIndexOfIIoM("80"),i)*'+;
'(oum(getIndexOfIOutM("59"),i)+oum(getIndexOfIOutM("62"),i)/1E2)+iom(getIndexOfIIoM("81"),i))*(oum(getIndexOfIOutM("67"),i)-(oum(getIndexOfIOutM("59"),i)+'+;
'oum(getIndexOfIOutM("62"),i)/1E2)*oum(getIndexOfIOutM("64"),i)/(oum(getIndexOfIOutM("59"),i)+oum(getIndexOfIOutM("62"),i)/1E2+iom(getIndexOfIIoM("82"),i)))/'+;
'(iom(getIndexOfIIoM("80"),i)*(oum(getIndexOfIOutM("60"),i)+oum(getIndexOfIOutM("62"),i)/1E2)+iom(getIndexOfIIoM("81"),i))/(oum(getIndexOfIOutM("67"),i)-'+;
'(oum(getIndexOfIOutM("60"),i)+oum(getIndexOfIOutM("62"),i)/1E2)*oum(getIndexOfIOutM("64"),i)/(oum(getIndexOfIOutM("60"),i)+oum(getIndexOfIOutM("62"),i)/1E2+'+;
'iom(getIndexOfIIoM("82"),i)))-1)'                                                          &&60.1 dB(alfa р)
DO CIKL1 WITH getIndexOfIOutM("60.1"),SUM('oum(getIndexOfIOutM("60.1"),'),'    -',SUM('oum(getIndexOfIOutM("60.1"),'),'    -'&&60.1 dB(alfa р)
DO CIKL WITH getIndexOfIOutM("62.1"),'oum(getIndexOfIOutM("69"),i)*oum(getIndexOfIOutM("75"),i)/oum(getIndexOfIOutM("74"),i)*((iom(getIndexOfIIoM("80"),i)*'+;
'(oum(getIndexOfIOutM("60"),i)+oum(getIndexOfIOutM("61"),i)/1E2)+iom(getIndexOfIIoM("81"),i))*(oum(getIndexOfIOutM("67"),i)-(oum(getIndexOfIOutM("60"),i)+'+;
'oum(getIndexOfIOutM("61"),i)/1E2)*oum(getIndexOfIOutM("64"),i)/(oum(getIndexOfIOutM("60"),i)+oum(getIndexOfIOutM("61"),i)/1E2+iom(getIndexOfIIoM("82"),i)))/'+;
'(iom(getIndexOfIIoM("80"),i)*(oum(getIndexOfIOutM("60"),i)+oum(getIndexOfIOutM("62"),i)/1E2)+iom(getIndexOfIIoM("81"),i))/(oum(getIndexOfIOutM("67"),i)-'+;
'(oum(getIndexOfIOutM("60"),i)+oum(getIndexOfIOutM("62"),i)/1E2)*oum(getIndexOfIOutM("64"),i)/(oum(getIndexOfIOutM("60"),i)+oum(getIndexOfIOutM("62"),i)/1E2+'+;
'iom(getIndexOfIIoM("82"),i)))-1)'                                                          &&62.1 dB(dalfa рух)
DO CIKL1 WITH getIndexOfIOutM("62.1"),SUM('oum(getIndexOfIOutM("62.1"),'),'    -',SUM('oum(getIndexOfIOutM("62.1"),'),'    -'&&62.1 dB(dalfa рух)
DO CIKL WITH getIndexOfIOutM("184"),'oum(getIndexOfIOutM("24"),i)-oum(getIndexOfIOutM("25"),i)'                          &&184 n вн
DO CIKL1 WITH getIndexOfIOutM("184"),SUM('oum(getIndexOfIOutM("184"),'),'    -',SUM('oum(getIndexOfIOutM("184"),'),'    -'&&184 n вн
DO CIKL WITH getIndexOfIOutM("185"),'oum(getIndexOfIOutM("95.1"),i)+oum(getIndexOfIOutM("100.1"),i)+oum(getIndexOfIOutM("103.1"),i)+'+;
'oum(getIndexOfIOutM("108.1"),i)+oum(getIndexOfIOutM("121.1"),i)'                                          &&185 —”ћ dB т
DO CIKL1 WITH getIndexOfIOutM("185"),SUM('oum(getIndexOfIOutM("185"),'),'    -','    -','    -'            &&185 —”ћ dB т
DO CIKL WITH getIndexOfIOutM("186"),'oum(getIndexOfIOutM("67.2"),i)+oum(getIndexOfIOutM("60.1"),i)+oum(getIndexOfIOutM("62.1"),i)+'+;
'oum(getIndexOfIOutM("25.1"),i)'                                                             &&186 —”ћ dB к
DO CIKL1 WITH getIndexOfIOutM("186"),SUM('oum(getIndexOfIOutM("186"),'),'    -','    -','    -'            &&186 —”ћ dB к
DO CIKL WITH getIndexOfIOutM("187"),'oum(getIndexOfIOutM("17"),i)-(oum(getIndexOfIOutM("15"),i)+(oum(getIndexOfIOutM("11"),i)-oum(getIndexOfIOutM("32"),i))*'+;
'(100+iom(getIndexOfIIoM("125"),i))/1E2+oum(getIndexOfIOutM("16.1"),i)+oum(getIndexOfIOutM("19"),i))'                   &&187 Ќебаланс
DO CIKL1 WITH getIndexOfIOutM("187"),oum(getIndexOfIOutM("17"),n_blokov+1)-(oum(getIndexOfIOutM("15"),n_blokov+1)+;
(oum(getIndexOfIOutM("11"),n_blokov+1)-oum(getIndexOfIOutM("32"),n_blokov+1))*(100+iom(getIndexOfIIoM("125"),n_blokov+1))/1E2+;
oum(getIndexOfIOutM("16.1"),n_blokov+1)+oum(getIndexOfIOutM("19"),n_blokov+1)),'    -','    -','    -'     &&187 Ќебаланс
DO CIKL WITH getIndexOfIOutM("188"),'oum(getIndexOfIOutM("187"),i)/oum(getIndexOfIOutM("17"),i)*1E2'                     &&188 Ќебаланс
DO CIKL1 WITH getIndexOfIOutM("188"),oum(getIndexOfIOutM("187"),n_blokov+1)/oum(getIndexOfIOutM("17"),n_blokov+1)*1E2,'    -','    -',;
'    -'                                                                        &&188 Ќебаланс
DO CIKL WITH getIndexOfIOutM("189"),'oum(getIndexOfIOutM("76"),i)/oum(getIndexOfIOutM("75"),i)*1E2'                      &&189 alfa газа
DO CIKL1 WITH getIndexOfIOutM("189"),oum(getIndexOfIOutM("76"),n_blokov+1)/oum(getIndexOfIOutM("75"),n_blokov+1)*1E2,'    -','    -',;
'    -'                                                                        &&189 alfa газа
DO CIKL1 WITH getIndexOfIOutM("190"),(inm(getIndexOfIInM("50"),n_blokov+1)+inm(getIndexOfIInM("51"),n_blokov+1))*1E3/;
inm(getIndexOfIInM("70"),n_blokov+1),'    -','    -','    -'                                &&190 G птс ср

do calc_end

ON ERROR

**********************************************************************
PROC CIKL1
PARAMETERS m,part1,part2,part3,part4,round
IF PARAM()=5
   round=.T.
ENDIF
PRIVATE i,part
FOR i=1 TO 4
 part='part'+STR(i,1)
 oum(m,n_blokov+i)=IIF(SUBSTR(outm(m,n_blokov+i),1,1)='=',;
 VAL(oum(m,n_blokov+i)),IIF(round,ROUND(&part,exact(m,1)),&part))
ENDFOR
*@ 0,11 SAY '+'+STR(m,3)
@ 0,11 SAY PADL('+'+Ioutm(m),6,' ')
