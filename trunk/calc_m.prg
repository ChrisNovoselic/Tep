PROC CALC_M
* inm,iom,outm - ������-���� INBLOK,OUTBLOK,OUTMKT
* outm1 - 1-����.������ ��� GATHER � OUTMKT
* oum1 - 2-����.������ ��� GATHER � OUTMKT
* exact - 2-����.������ ���-�� ��.����� ������� � OUTMKT
PUBLIC outm1(ALEN(outm,2)),oum(ALEN(outm,1),ALEN(outm,2))
STORE 1 TO min11,aa11,min12,aa12,min21,aa21,min22,aa22,;
           min111,aa111,min112,aa112,min121,aa121,min122,aa122,;
           min31,aa31,min32,aa32,min211,aa211,min212,aa212,min221,aa221,min222,aa222,;
           min1111,aa1111,min1112,aa1112,min1121,aa1121,min1122,aa1122,;
           min1211,aa1211,min1212,aa1212,min1221,aa1221,min1222,aa1222
FOR i=1 TO ALEN(inm,1)
FOR j=1 TO ALEN(inm,2)
    IF NOT Iinm(i)=='74'    && ������������ �� �������� ������� ��.����
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
FOR j=1 TO ALEN(outm,2)             && ��������� ������� �������
    oum(i,j)=SUBSTR(outm(i,j),2) && ����������� ���������
ENDFOR    
ENDFOR
******** � ��������� ������������ ��.����. (proc COR_IN)***************
ON ERROR i=i  && ��� ���������� �����-�� ��������� �����. � �����.���-��


DO CIKL WITH o("1"),'200'                                                      && 1 N� ��
DO CIKL1 WITH o("1"),1200,'    -',1200,'    -'                                 && 1 N� ��
DO CIKL1 WITH o("2"),inm(i("76"),n_blokov+1),'    -',;
inm(i("76"),n_blokov+1),'    -'                                                && 2 N�
DO CIKL WITH o("3"),'240'                                                      && 3 Q� ��
DO CIKL1 WITH o("3"),1440,'    -',1440,'    -'                                 && 3 Q� ��
DO CIKL WITH o("4"),'iom(u("2"),i)'                                            && 4 ����
DO CIKL1 WITH o("4"),iom(u("2"),n_blokov+1),'    -',iom(u("2"),n_blokov+1),'    -' && 4 ����
DO CIKL WITH o("5"),'oum(o("4"),i)/oum(o("1"),i)'                              && 5 TAU � �
DO CIKL1 WITH o("5"),oum(o("4"),n_blokov+1)/oum(o("1"),n_blokov+1),'    -',;
oum(o("4"),n_blokov+1)/oum(o("1"),n_blokov+1),'    -'                          && 5 TAU � �
DO CIKL WITH o("6"),'iom(u("4"),i)'                                            && 6 Q��
DO CIKL1 WITH o("6"),iom(u("4"),n_blokov+1),'    -',iom(u("4"),n_blokov+1),'    -' && 6 Q /��
DO CIKL WITH o("7"),'iom(u("3"),i)'                                            && 7 Q��
DO CIKL1 WITH o("7"),iom(u("3"),n_blokov+1),'    -',iom(u("3"),n_blokov+1),'    -' && 7 Q��
DO CIKL WITH o("9"),'oum(o("6"),i)+oum(o("7"),i)'                              && 9 Q
DO CIKL1 WITH o("9"),SUM('oum(o("9"),'),'    -',SUM('oum(o("9"),'),'    -'     && 9 Q
DO CIKL WITH o("10"),'oum(o("9"),i)/oum(o("3"),i)'                             &&10 TAU � �
DO CIKL1 WITH o("10"),oum(o("9"),n_blokov+1)/oum(o("3"),n_blokov+1),'    -',;
oum(o("9"),n_blokov+1)/oum(o("3"),n_blokov+1),'    -'                          &&10 TAU � �
DO CIKL WITH o("11"),'iom(u("8"),i)'                                           &&11 Q���
DO CIKL1 WITH o("11"),iom(u("8"),n_blokov+1),'    -',iom(u("5"),n_blokov+1),'    -' &&11 Q���
DO CIKL WITH o("12"),'oum(o("11"),i)-inm(i("83"),i)'                           &&12 Q�� ��
DO CIKL1 WITH o("12"),SUM('oum(o("12"),'),'    -',SUM('oum(o("12"),'),'    -'  &&12 Q�� ��
DO CIKL WITH o("13"),'iom(u("8"),i)'                                           &&13 Q�� ���
DO CIKL1 WITH o("13"),iom(u("8"),n_blokov+1),'    -',iom(u("8"),n_blokov+1),'    -' &&13 Q�� ���
DO CIKL WITH o("15"),'(iom(u("49"),i)*iom(u("57.1"),i)+iom(u("55"),i)*(iom(u("59.1"),i)-'+;
'iom(u("60"),i))-iom(u("66"),i)*iom(u("58"),i))/1E3-(iom(u("3"),i)+iom(u("4"),i))' &&15 Q�
DO CIKL1 WITH o("15"),SUM('oum(o("15"),'),'    -',SUM('oum(o("15"),'),'    -'  &&15 Q�
DO CIKL WITH o("16"),'iom(u("41"),i)*oum(o("15"),i)/100+15.4*(inm(i("68"),i)-'+;
'inm(i("69"),i))'                                                              &&16 Q� �� (�)
DO CIKL1 WITH o("16"),SUM('oum(o("16"),'),'    -',SUM('oum(o("16"),'),'    -'  &&16 Q� �� (�)
DO CIKL WITH o("17"),'iom(u("64"),i)'                                          &&17 Q� ��
DO CIKL1 WITH o("17"),SUM('oum(o("17"),'),'    -',SUM('oum(o("17"),'),'    -'  &&17 Q� ��
DO CIKL WITH o("18"),'iom(u("121"),i)*oum(o("17"),i)/100'                      &&18 Q� ��(�)
DO CIKL1 WITH o("18"),SUM('oum(o("18"),'),'    -',SUM('oum(o("18"),'),'    -'  &&18 Q� ��(�)
DO CIKL WITH o("16.1"),'IIF(oum(o("16"),i)+oum(o("18"),i)=0,0,(oum(o("9"),i)-oum(o("11"),i))*'+;
'oum(o("16"),i)/(oum(o("16"),i)+oum(o("18"),i)))'                              &&16.1 Q� ��
DO CIKL1 WITH o("16.1"),SUM('oum(o("16.1"),'),'    -',SUM('oum(o("16.1"),'),'    -'&&16.1 Q� ��
DO CIKL WITH o("19"),'(oum(o("9"),i)-oum(o("11"),i))-oum(o("16.1"),i)'         &&19 Q� ��
DO CIKL1 WITH o("19"),SUM('oum(o("19"),'),'    -',SUM('oum(o("19"),'),'    -'  &&19 Q� ��
DO CIKL WITH o("20"),'iom(u("1"),i)'                                           &&20 TAU � ���
DO CIKL1 WITH o("20"),iom(u("1"),n_blokov+1),'    -',iom(u("1"),n_blokov+1),'    -'&&20 TAU � ���
DO CIKL WITH o("21"),'inm(i("73"),i)'                                          &&21 TAU � ���
DO CIKL1 WITH o("21"),SUM('oum(o("21"),'),'    -',SUM('oum(o("21"),'),'    -'  &&21 TAU � ���
DO CIKL WITH o("24"),'inm(i("68"),i)'                                          &&24 n �
DO CIKL1 WITH o("24"),SUM('oum(o("24"),'),'    -',SUM('oum(o("24"),'),'    -'  &&24 n �
DO CIKL WITH o("25"),'inm(i("69"),i)'                                          &&25 n � (�)
DO CIKL1 WITH o("25"),SUM('oum(o("25"),'),'    -',SUM('oum(o("25"),'),'    -'  &&25 n � (�)
DO CIKL WITH o("25.1"),'64.2*(oum(o("24"),i)-oum(o("25"),i))'                  &&25.1 dB(n �)
DO CIKL1 WITH o("25.1"),SUM('oum(o("25.1"),'),'    -',SUM('oum(o("25.1"),'),'    -'&&25.1 dB(n �)
IF BL1 OR BL2 OR BL3 OR BL4 OR BL5 OR BL6
DO CIKL WITH o("27"),'IIF(iom(u("8"),i)=0,0,inm(i("10"),i)+(inm(i("10.4"),n_blokov+1)+inm(i("11.1"),n_blokov+1)+'+;
'inm(i("11.2"),n_blokov+1)+inm(i("12"),n_blokov+1))/n_blokov1)'                &&27 ����� 
ELSE
STORE 0 TO sum,sum1
FOR i=1 TO n_blokov
    sum=sum+iom(u("8"),i)
    sum1=sum1+inm(i("10"),i)
ENDFOR
DO CIKL WITH o("27"),'IIF(sum=0,0,inm(i("10"),i)+(inm(i("10"),n_blokov+1)-'+;
'sum1)*inm(i("47"),i)/sum)'                                                    &&27 ����� 
ENDIF
DO CIKL1 WITH o("27"),SUM('oum(o("27"),'),'    -',SUM('oum(o("27"),'),'    -'  &&27 ����� 
IF BL1 OR BL2 OR BL3 OR BL4 OR BL5 OR BL6
DO CIKL WITH o("28"),'inm(i("4"),i)+inm(i("4"),n_blokov+1)'                    &&28 �� ��
ELSE
sum=0
FOR i=1 TO n_blokov
    sum=sum+inm(i("4"),i)
ENDFOR
DO CIKL WITH o("28"),'inm(i("4"),i)+(inm(i("4"),n_blokov+1)-sum)*oum(o("4"),i)'+;
'/oum(o("4"),n_blokov+1)'                                                      &&28 �� ��
ENDIF
DO CIKL1 WITH o("28"),SUM('oum(o("28"),'),'    -',SUM('oum(o("28"),'),'    -'  &&28 �� ��
IF BL1 OR BL2 OR BL3 OR BL4 OR BL5 OR BL6
DO CIKL WITH o("29"),'inm(i("7"),i)+inm(i("7"),n_blokov+1)'                    &&29 �� ��
ELSE
sum=0
FOR i=1 TO n_blokov
    sum=sum+inm(i("7"),i)
ENDFOR
DO CIKL WITH o("29"),'inm(i("7"),i)+(inm(i("7"),n_blokov+1)-sum)*oum(o("4"),i)'+;
'/oum(o("4"),n_blokov+1)'                                                      &&29 �� ��
ENDIF
DO CIKL1 WITH o("29"),SUM('oum(o("29"),'),'    -',SUM('oum(o("29"),'),'    -'  &&29 �� ��
DO CIKL WITH o("30"),'oum(o("29"),i)+oum(o("28"),i)+oum(o("27"),i)'            &&30 ���
DO CIKL1 WITH o("30"),SUM('oum(o("30"),'),'    -',SUM('oum(o("30"),'),'    -'  &&30 ���
DO CIKL WITH o("31"),'oum(o("4"),i)-oum(o("30"),i)'                            &&31 ���
DO CIKL1 WITH o("31"),SUM('oum(o("31"),'),'    -',SUM('oum(o("31"),'),'    -'  &&31 ���
DO CIKL WITH o("32"),'1E-3*860*(inm(i("10.1"),i)+inm(i("10.2"),i))*85/1E2'     &&32 Q���
DO CIKL1 WITH o("32"),SUM('oum(o("32"),'),'    -',SUM('oum(o("32"),'),'    -'  &&32 Q���
DO CIKL WITH o("33"),'IIF(inm(i("47"),i)=0,1,(oum(o("15"),i)+oum(o("16.1"),i)+iom(u("47"),i))/'+;
'(oum(o("15"),i)+oum(o("16.1"),i)+iom(u("47"),i)+(oum(o("11"),i)-oum(o("32"),i))*'+;
'(100+iom(u("125"),i))/1E2))'                                                  &&33 K�
DO CIKL1 WITH o("33"),IIF(SUM('inm(i("47"),')=0,1,(oum(o("15"),n_blokov+1)+;
oum(o("16.1"),n_blokov+1)+iom(u("47"),n_blokov+1))/;
(oum(o("15"),n_blokov+1)+oum(o("16.1"),n_blokov+1)+iom(u("47"),n_blokov+1)+;
(oum(o("11"),n_blokov+1)-oum(o("32"),n_blokov+1))*(100+iom(u("125"),n_blokov+1))/1E2)),'    -',;
IIF(SUM('inm(i("47"),')=0,1,(oum(o("15"),n_blokov+1)+oum(o("16.1"),n_blokov+1)+;
iom(u("47"),n_blokov+1))/;
(oum(o("15"),n_blokov+1)+oum(o("16.1"),n_blokov+1)+iom(u("47"),n_blokov+1)+;
(oum(o("11"),n_blokov+1)-oum(o("32"),n_blokov+1))*(100+iom(u("125"),n_blokov+1))/1E2)),;
'    -'                                                                        &&33 K�
DO CIKL WITH o("34"),'oum(o("29"),i)*oum(o("33"),i)+oum(o("28"),i)'            &&34 �� ��
DO CIKL1 WITH o("34"),SUM('oum(o("34"),'),'    -',SUM('oum(o("34"),'),'    -'  &&34 �� ��
DO CIKL WITH o("35"),'oum(o("30"),i)-oum(o("34"),i)'                           &&35 ��� ��
DO CIKL1 WITH o("35"),SUM('oum(o("35"),'),'    -',SUM('oum(o("35"),'),'    -'  &&35 ��� ��
DO CIKL WITH o("36"),'iom(u("36"),i)*oum(o("4"),i)/1E2'                        &&36 �� ��(�)
DO CIKL1 WITH o("36"),SUM('oum(o("36"),'),'    -',SUM('oum(o("36"),'),'    -'  &&36 �� ��(�)
DO CIKL WITH o("37"),'iom(u("108"),i)'                                         &&37 �� ��(�)
DO CIKL1 WITH o("37"),SUM('oum(o("37"),'),'    -',SUM('oum(o("37"),'),'    -'  &&37 �� ��(�)
DO CIKL WITH o("38"),'iom(u("144"),i)'                                         &&38 ����� ��(�)
DO CIKL1 WITH o("38"),SUM('oum(o("38"),'),'    -',SUM('oum(o("38"),'),'    -'  &&38 ����� ��(�)
DO CIKL WITH o("39"),'iom(u("128"),i)*oum(o("4"),i)/1E2'                       &&39 �� ��(�)
DO CIKL1 WITH o("39"),SUM('oum(o("39"),'),'    -',SUM('oum(o("39"),'),'    -'  &&39 �� ��(�)
DO CIKL WITH o("40"),'oum(o("36"),i)+oum(o("37"),i)+oum(o("38"),i)-oum(o("39"),i)'&&40 ��� ��(�)
DO CIKL1 WITH o("40"),SUM('oum(o("40"),'),'    -',SUM('oum(o("40"),'),'    -'  &&40 ��� ��(�)
IF BL1 OR BL2 OR BL3 OR BL4 OR BL5 OR BL6
DO CIKL WITH o("41"),'inm(i("6"),n_blokov+1)/n_blokov1'                        &&41 ���
ELSE
DO CIKL WITH o("41"),'inm(i("6"),n_blokov+1)*oum(o("4"),i)/oum(o("4"),n_blokov+1)'&&41 ���
ENDIF
DO CIKL1 WITH o("41"),SUM('oum(o("41"),'),'    -',SUM('oum(o("41"),'),'    -'  &&41 ���
DO CIKL WITH o("42"),'iom(u("29"),n_blokov+1)*inm(i("70"),n_blokov+1)*oum(o("4"),i)/'+;
'oum(o("4"),n_blokov+1)'                                                       &&42 ���(�)
DO CIKL1 WITH o("42"),SUM('oum(o("42"),'),'    -',SUM('oum(o("42"),'),'    -'  &&42 ���(�)
DO CIKL WITH o("43"),'inm(i("8.1"),i)'                                         &&43 ����
DO CIKL1 WITH o("43"),SUM('oum(o("43"),'),'    -',SUM('oum(o("43"),'),'    -'  &&43 ����
DO CIKL WITH o("44"),'iom(u("66"),i)/1E3'                                      &&44 G��
DO CIKL1 WITH o("44"),SUM('oum(o("44"),'),'    -',SUM('oum(o("44"),'),'    -'  &&44 G��
DO CIKL WITH o("45"),'iom(u("99"),i)*oum(o("44"),i)'                           &&45 ���� (�)
DO CIKL1 WITH o("45"),SUM('oum(o("45"),'),'    -',SUM('oum(o("45"),'),'    -'  &&45 ���� (�)
DO CIKL WITH o("46"),'LOG(IIF(inm(i("74"),i)=="2�",inm(i("38"),i),inm(i("37"),i)))',.F.&&46 ��
DO CIKL WITH o("46"),'1/(2.6864264-.20096551*oum(o("46"),i)-2.16688/1E3*oum(o("46"),i)^2-'+;
'9.480808/1E5*oum(o("46"),i)^3+6.135062/1E6*oum(o("46"),i)^4+3.6917245/1E6*'+;
'oum(o("46"),i)^5)',.F.                                                        &&46 ��
DO CIKL WITH o("46"),'-753.317+6959.4093*oum(o("46"),i)-29257.981*oum(o("46"),i)^2+'+;
'71285.169*oum(o("46"),i)^3-86752.84*oum(o("46"),i)^4+42641.056*oum(o("46"),i)^5',.F.&&46 ��
DO CIKL WITH o("46"),'oum(o("7"),i)*1E3/(iom(u("62"),i)-oum(o("46"),i))'       &&46 ��
DO CIKL WITH o("47"),'oum(o("46"),i)*(iom(u("58"),i)-107)/(728-iom(u("58"),i))'&&47 ����
DO CIKL WITH o("48"),'iom(u("26"),i)'                                          &&48 ��� �
DO CIKL1 WITH o("48"),SUM('oum(o("48"),'),'    -',SUM('oum(o("48"),'),'    -'  &&48 ��� �
DO CIKL WITH o("49"),'IIF(inm(i("74"),i)=="1",0,(oum(o("46"),i)*(iom(u("57.1"),i)+'+;
'(iom(u("59.1"),i)-iom(u("60"),i))-iom(u("62"),i))+oum(o("47"),i)*(iom(u("57.1"),i)+'+;
'(iom(u("59.1"),i)-iom(u("60"),i))-752))/860)'                                   &&49 ��� �
DO CIKL1 WITH o("49"),SUM('oum(o("49"),'),'    -',SUM('oum(o("49"),'),'    -'  &&49 ��� �
DO CIKL WITH o("50"),'oum(o("48"),i)+oum(o("49"),i)'                           &&50 ���
DO CIKL1 WITH o("50"),SUM('oum(o("50"),'),'    -',SUM('oum(o("50"),'),'    -'  &&50 ���
DO CIKL WITH o("51"),'oum(o("50"),i)/oum(o("4"),i)*1E2'                        &&51 ���
DO CIKL1 WITH o("51"),oum(o("50"),n_blokov+1)/oum(o("4"),n_blokov+1)*1E2,'    -',;
oum(o("50"),n_blokov+1)/oum(o("4"),n_blokov+1)*1E2,'    -'                     &&51 ���
DO CIKL WITH o("52"),'oum(o("49"),i)/oum(o("7"),i)*1E3'                        &&52 W�� �
DO CIKL1 WITH o("52"),oum(o("49"),n_blokov+1)/oum(o("7"),n_blokov+1)*1E3,'    -',;
'    -','    -'                                                                &&52 W�� �
DO CIKL WITH o("53"),'iom(u("27"),i)'                                          &&53 W�� �
DO CIKL WITH o("55"),'oum(o("15"),i)/oum(o("4"),i)*1E3'                        &&55 q� ��
DO CIKL1 WITH o("55"),oum(o("15"),n_blokov+1)/oum(o("4"),n_blokov+1)*1E3,'    -',;
oum(o("15"),n_blokov+1)/oum(o("4"),n_blokov+1)*1E3,'    -'                     &&55 q� ��
DO CIKL WITH o("56"),'iom(u("24"),i)'                                          &&56 q� ��(�)
DO CIKL1 WITH o("56"),iom(u("24"),n_blokov+1),'    -',iom(u("24"),n_blokov+1),'    -'&&56 q� ��(�)
DO CIKL WITH o("57"),'(.05*inm(i("56"),i)/(100-inm(i("56"),i))+.95*inm(i("57"),i)/'+;
'(100-inm(i("57"),i)))*7800*inm(i("55"),n_blokov+1)/1E2/inm(i("53"),n_blokov+1)*'+;
'iom(u("89"),i)'                                                               &&57 (q3+q4)
sum=0
FOR i=1 TO n_blokov
    sum=sum+oum(o("57"),i)*oum(o("17"),i)
ENDFOR
DO CIKL1 WITH o("57"),sum/SUM('oum(o("17"),'),'    -',sum/SUM('oum(o("17"),'),'    -'&&57 (q3+q4) /�
DO CIKL WITH o("58"),'iom(u("73"),i)'                                          &&58 q3(�)+q4(�)
DO CIKL1 WITH o("58"),iom(u("73"),n_blokov+1),'    -',iom(u("73"),n_blokov+1),'    -'&&58 q3(�)+q4(�)
DO CIKL WITH o("59"),'(21-(.02*iom(u("89"),i)/1E2+.1*inm(i("59"),i)/1E2)*'+;
'(inm(i("35"),i)+inm(i("36"),i))/2)/(21-(inm(i("35"),i)+inm(i("36"),i))/2)'    &&59 alfa �
sum=0
FOR i=1 TO n_blokov
    sum=sum+oum(o("59"),i)*oum(o("17"),i)
ENDFOR
DO CIKL1 WITH o("59"),sum/SUM('oum(o("17"),'),'    -',sum/SUM('oum(o("17"),'),'    -'&&59 alfa �
DO CIKL WITH o("60"),'iom(u("67"),i)'                                          &&60 alfa �(�)
sum=0
FOR i=1 TO n_blokov
    sum=sum+oum(o("60"),i)*oum(o("17"),i)
ENDFOR
DO CIKL1 WITH o("60"),sum/SUM('oum(o("17"),'),'    -',sum/SUM('oum(o("17"),'),'    -'&&60 alfa �(�)
DO CIKL WITH o("61"),'inm(i("44"),i)*SQRT(472.2/iom(u("65"),i))'               &&61 dalfa ���
sum=0
FOR i=1 TO n_blokov
    sum=sum+oum(o("61"),i)*oum(o("17"),i)
ENDFOR
DO CIKL1 WITH o("61"),sum/SUM('oum(o("17"),'),'    -',sum/SUM('oum(o("17"),'),'    -'&&61 dalfa ���
DO CIKL WITH o("62"),'iom(u("68"),i)*1E2'                                      &&62 dalfa ���(�)
sum=0
FOR i=1 TO n_blokov
    sum=sum+oum(o("62"),i)*oum(o("17"),i)
ENDFOR
DO CIKL1 WITH o("62"),sum/SUM('oum(o("17"),'),'    -',sum/SUM('oum(o("17"),'),'    -'&&62 dalfa ���(�)
DO CIKL WITH o("63"),'inm(i("44.1"),i)*SQRT(446.1/iom(u("65"),i))'             &&63 dalfa ��-�
sum=0
FOR i=1 TO n_blokov
    sum=sum+oum(o("63"),i)*oum(o("17"),i)
ENDFOR
DO CIKL1 WITH o("63"),sum/SUM('oum(o("17"),'),'    -',sum/SUM('oum(o("17"),'),'    -'&&63 dalfa ��-�
DO CIKL WITH o("64"),'(inm(i("31"),i)+inm(i("31.1"),i))/2'                     &&64 t ��
sum=0
FOR i=1 TO n_blokov
    sum=sum+oum(o("64"),i)*oum(o("17"),i)
ENDFOR
DO CIKL1 WITH o("64"),sum/SUM('oum(o("17"),'),'    -',sum/SUM('oum(o("17"),'),'    -'&&64 t ��
DO CIKL WITH o("65"),'(inm(i("32"),i)+inm(i("32.1"),i))/2'                     &&65 t '��
sum=0
FOR i=1 TO n_blokov
    sum=sum+oum(o("65"),i)*oum(o("17"),i)
ENDFOR
DO CIKL1 WITH o("65"),sum/SUM('oum(o("17"),'),'    -',sum/SUM('oum(o("17"),'),'    -'&&65 t '��
DO CIKL WITH o("66"),'(inm(i("39"),i)+inm(i("40"),i))/2'                       &&66 t ��
sum=0
FOR i=1 TO n_blokov
    sum=sum+oum(o("66"),i)*oum(o("17"),i)
ENDFOR
DO CIKL1 WITH o("66"),sum/SUM('oum(o("17"),'),'    -',sum/SUM('oum(o("17"),'),'    -'&&66 t ��
DO CIKL WITH o("67"),'iom(u("79"),i)'                                          &&67 t ��(�)
DO CIKL1 WITH o("67"),iom(u("79"),n_blokov+1),'    -',iom(u("79"),n_blokov+1),'    -'&&67 t ��(�)
DO CIKL WITH o("68"),'(iom(u("80"),i)*(oum(o("59"),i)+oum(o("61"),i)/1E2)+iom(u("81"),i))*'+;
'(oum(o("66"),i)-(oum(o("59"),i)+oum(o("61"),i)/1E2)/((oum(o("59"),i)+oum(o("61"),i)/1E2)+'+;
'iom(u("82"),i))*oum(o("64"),i))*(.9805+.00013*oum(o("66"),i))*(1-.01*oum(o("57"),i))/1E2+'+;
'(.2*.95*inm(i("55"),n_blokov+1)*iom(u("89"),i)/1E2*oum(o("66"),i))/'+;
'inm(i("53"),n_blokov+1)'                                                      &&68 q2
sum=0
FOR i=1 TO n_blokov
    sum=sum+oum(o("68"),i)*oum(o("17"),i)
ENDFOR
DO CIKL1 WITH o("68"),sum/SUM('oum(o("17"),'),'    -',sum/SUM('oum(o("17"),'),'    -'&&68 q2
DO CIKL WITH o("69"),'iom(u("83"),i)'                                          &&69 q2(�)
DO CIKL1 WITH o("69"),iom(u("83"),n_blokov+1),'    -',iom(u("83"),n_blokov+1),'    -'&&69 q2(�)
DO CIKL WITH o("67.1"),'(oum(o("69"),i)*(oum(o("66"),i)-oum(o("67"),i)))/(oum(o("67"),i)-'+;
'((oum(o("60"),i)+oum(o("62"),i)/1E2)*oum(o("64"),i)/(oum(o("60"),i)+oum(o("62"),i)/1E2)+'+;
'iom(u("82"),i)))'                                                             &&67.1 dq2 (t ��)
DO CIKL WITH o("70"),'iom(u("49"),i)/1E3'                                      &&70 D0
DO CIKL1 WITH o("70"),SUM('oum(o("70"),'),'    -',SUM('oum(o("70"),'),'    -'  &&70 D0
DO CIKL WITH o("71"),'inm(i("68"),i)*64.2*7*1E2/(oum(o("17"),i)*1E2/(100-oum(o("68"),i)-'+;
'oum(o("57"),i)-iom(u("84"),i)-iom(u("85"),i))+85.0*7)'                        &&71 q ����
DO CIKL WITH o("72"),'iom(u("87"),i)'                                          &&72 q ����(�)
DO CIKL WITH o("73"),'100-oum(o("68"),i)-oum(o("57"),i)-iom(u("84"),i)-iom(u("85"),i)-'+;
'oum(o("71"),i)'                                                               &&73 ���� ��
sum=0
FOR i=1 TO n_blokov
    sum=sum+oum(o("73"),i)*oum(o("17"),i)
ENDFOR
DO CIKL1 WITH o("73"),sum/SUM('oum(o("17"),'),'    -',sum/SUM('oum(o("17"),'),'    -'&&73 ���� ��
DO CIKL WITH o("74"),'iom(u("88"),i)'                                          &&74 ���� ��(�)
DO CIKL1 WITH o("74"),iom(u("88"),n_blokov+1),'    -',iom(u("88"),n_blokov+1),'    -'&&74 ���� ��(�)
DO CIKL WITH o("75"),'oum(o("17"),i)*1E2/7/oum(o("73"),i)'                     &&75 B
DO CIKL1 WITH o("75"),SUM('oum(o("75"),'),'    -',SUM('oum(o("75"),'),'    -'  &&75 B
DO CIKL WITH o("67.2"),'oum(o("75"),i)*oum(o("67.1"),i)/oum(o("74"),i)'        &&67.2 dB(t ��)
DO CIKL1 WITH o("67.2"),SUM('oum(o("67.2"),'),'    -',SUM('oum(o("67.2"),'),'    -'&&67.2 dB(t ��)
DO CIKL WITH o("76"),'oum(o("75"),i)*inm(i("59"),i)/1E2'                       &&76 B �
DO CIKL1 WITH o("76"),SUM('oum(o("76"),'),'    -',SUM('oum(o("76"),'),'    -'  &&76 B �
DO CIKL WITH o("77"),'oum(o("75"),i)*inm(i("60"),i)/1E2'                       &&77 B �
DO CIKL1 WITH o("77"),SUM('oum(o("77"),'),'    -',SUM('oum(o("77"),'),'    -'  &&77 B �
DO CIKL WITH o("78"),'oum(o("75"),i)-oum(o("76"),i)-oum(o("77"),i)'            &&78 B ��
DO CIKL1 WITH o("78"),SUM('oum(o("78"),'),'    -',SUM('oum(o("78"),'),'    -'  &&78 B ��
DO CIKL WITH o("79"),'oum(o("75"),i)*oum(o("33"),i)*oum(o("31"),i)/(oum(o("4"),i)-'+;
'oum(o("34"),i))'                                                              &&79 B �
DO CIKL1 WITH o("79"),SUM('oum(o("79"),'),'    -',SUM('oum(o("79"),'),'    -'  &&79 B �
DO CIKL WITH o("80"),'oum(o("75"),i)-oum(o("79"),i)'                           &&80 B ��
DO CIKL1 WITH o("80"),SUM('oum(o("80"),'),'    -',SUM('oum(o("80"),'),'    -'  &&80 B ��
DO CIKL WITH o("81"),'oum(o("79"),i)*1E3/oum(o("31"),i)'                       &&81 b �
DO CIKL1 WITH o("81"),oum(o("79"),n_blokov+1)*1E3/oum(o("31"),n_blokov+1),'    -',;
oum(o("79"),n_blokov+3)*1E3/oum(o("31"),n_blokov+3),'    -'                    &&81 b �
DO CIKL WITH o("82"),'iom(u("136"),i)'                                         &&82 b � �
DO CASE
   CASE BL1
DO CIKL1 WITH o("82"),oum(o("82"),ASCAN(ai,1)),'    -',oum(o("82"),ASCAN(ai,1)),'    -'&&82 b � �
   CASE BL2
DO CIKL1 WITH o("82"),oum(o("82"),ASCAN(ai,2)),'    -',oum(o("82"),ASCAN(ai,2)),'    -'&&82 b � �
   CASE BL3
DO CIKL1 WITH o("82"),oum(o("82"),ASCAN(ai,3)),'    -',oum(o("82"),ASCAN(ai,3)),'    -'&&82 b � �
   CASE BL4
DO CIKL1 WITH o("82"),oum(o("82"),ASCAN(ai,4)),'    -',oum(o("82"),ASCAN(ai,4)),'    -'&&82 b � �
   CASE BL5
DO CIKL1 WITH o("82"),oum(o("82"),ASCAN(ai,5)),'    -',oum(o("82"),ASCAN(ai,5)),'    -'&&82 b � �
   CASE BL6
DO CIKL1 WITH o("82"),oum(o("82"),ASCAN(ai,6)),'    -',oum(o("82"),ASCAN(ai,6)),'    -'&&82 b � �
   OTHER
DO CIKL1 WITH o("82"),iom(u("136"),n_blokov+1),'    -',iom(u("136"),n_blokov+1),'    -'&&82 b � �
ENDCASE
DO CIKL WITH o("83"),'iom(u("137"),i)'                                         &&83 b � ��
DO CASE
   CASE BL1
DO CIKL1 WITH o("83"),oum(o("83"),ASCAN(ai,1)),'    -',oum(o("83"),ASCAN(ai,1)),'    -'&&83 b � ��
   CASE BL2
DO CIKL1 WITH o("83"),oum(o("83"),ASCAN(ai,2)),'    -',oum(o("83"),ASCAN(ai,2)),'    -'&&83 b � ��
   CASE BL3
DO CIKL1 WITH o("83"),oum(o("83"),ASCAN(ai,3)),'    -',oum(o("83"),ASCAN(ai,3)),'    -'&&83 b � ��
   CASE BL4
DO CIKL1 WITH o("83"),oum(o("83"),ASCAN(ai,4)),'    -',oum(o("83"),ASCAN(ai,4)),'    -'&&83 b � ��
   CASE BL5
DO CIKL1 WITH o("83"),oum(o("83"),ASCAN(ai,5)),'    -',oum(o("83"),ASCAN(ai,5)),'    -'&&83 b � ��
   CASE BL6
DO CIKL1 WITH o("83"),oum(o("83"),ASCAN(ai,6)),'    -',oum(o("83"),ASCAN(ai,6)),'    -'&&83 b � ��
   OTHER
DO CIKL1 WITH o("83"),iom(u("137"),n_blokov+1),'    -',iom(u("137"),n_blokov+1),'    -'&&83 b � ��
ENDCASE
DO CIKL WITH o("84"),'oum(o("80"),i)*1E3/oum(o("11"),i)'                       &&84 b ��
DO CIKL1 WITH o("84"),oum(o("80"),n_blokov+1)*1E3/oum(o("11"),n_blokov+1),'    -',;
oum(o("80"),n_blokov+1)*1E3/oum(o("11"),n_blokov+1),'    -'                    &&84 b ��
DO CIKL WITH o("85"),'iom(u("148"),i)'                                         &&85 b ��(�)
DO CIKL1 WITH o("85"),iom(u("148"),n_blokov+1),'    -',iom(u("148"),n_blokov+1),'    -'&&85 b ��(�)
DO CIKL WITH o("86"),'iom(u("150"),i)'                                         &&86 b �� ��
DO CIKL1 WITH o("86"),iom(u("150"),n_blokov+1),'    -',iom(u("150"),n_blokov+1),'    -'&&86 b �� ��
DO CIKL WITH o("89"),'iom(u("45"),i)'                                          &&89 dQ� ��(���)
DO CIKL1 WITH o("89"),iom(u("45"),n_blokov+1),'    -',iom(u("45"),n_blokov+1),'    -'&&89 dQ� ��(���)
DO CIKL WITH o("90"),'iom(u("46"),i)'                                          &&90 dQ� ��(���)
DO CIKL1 WITH o("90"),iom(u("46"),n_blokov+1),'    -',iom(u("46"),n_blokov+1),'    -'&&90 dQ� ��(���)
DO CIKL WITH o("91"),'iom(u("135"),i)'                                         &&91 K���(�) �
sum=0
FOR i=1 TO n_blokov
    sum=sum+oum(o("91"),i)*oum(o("79"),i)
ENDFOR
DO CIKL1 WITH o("91"),sum/SUM('oum(o("79"),'),'    -',sum/SUM('oum(o("79"),'),'    -'&&91 K���(�) �
DO CIKL WITH o("92"),'iom(u("135"),i)'                                         &&92 K���(�) ��
STORE 0 TO sum1,sum2
FOR i=1 TO n_blokov
    sum1=sum1+(oum(o("75"),i)-oum(o("79"),i)-oum(o("81"),i)*oum(o("27"),i)/1E3)*oum(o("92"),i)
    sum2=sum2+oum(o("75"),i)-oum(o("79"),i)-oum(o("81"),i)*oum(o("27"),i)/1E3
ENDFOR
DO CIKL1 WITH o("92"),sum1/sum2,'    -',sum1/sum2,'    -'                      &&92 K���(�) ��
DO CIKL WITH o("93"),'iom(u("56.1"),i)'                                        &&93 P�
sum=0
FOR i=1 TO n_blokov
    sum=sum+oum(o("93"),i)*oum(o("70"),i)
ENDFOR
DO CIKL1 WITH o("93"),sum/SUM('oum(o("70"),'),'    -',sum/SUM('oum(o("70"),'),'    -'&&93 P�
DO CIKL WITH o("94"),'F1(iom(u("50"),i),"2.65�:1")'                            &&94 P� (�)
sum=0
FOR i=1 TO n_blokov
    sum=sum+oum(o("94"),i)*oum(o("70"),i)
ENDFOR
DO CIKL1 WITH o("94"),sum/SUM('oum(o("70"),'),'    -',sum/SUM('oum(o("70"),'),'    -'&&94 P� (�)
DO CIKL WITH o("95"),'IIF(inm(i("74"),i)=="1",F2(iom(u("50"),i),oum(o("93"),i),"2.66:2"),'+;
'IIF(inm(i("74"),i)=="2" OR inm(i("74"),i)=="2�" OR inm(i("74"),i)=="3",'+;
'IIF(iom(u("10"),i)<=60,IIF(iom(u("50"),i)<=510,IIF(oum(o("93"),i)<=130,-0.32,0.32)*ABS(oum(o("93"),i)-oum(o("94"),i)),'+;
                                               'IIF(oum(o("93"),i)<=130,0.7,-0.7)*ABS(oum(o("93"),i)-oum(o("94"),i))'+;
                          '),'+;
                        'IIF(iom(u("50"),i)<=510,IIF(oum(o("93"),i)<=130,-0.37,0.37)*ABS(oum(o("93"),i)-oum(o("94"),i)),'+;
                                                'IIF(oum(o("93"),i)<=125,0.76,IIF(oum(o("93"),i)<=130,1.04,-0.8))*'+;
                                                'ABS(oum(o("93"),i)-oum(o("94"),i))'+;
                          ')'+;
    '),1/0))'                                                                  &&95 alfa q�(P�)
DO CIKL WITH o("96"),'inm(i("23"),i)'                                          &&96 P�
sum=0
FOR i=1 TO n_blokov
    sum=sum+oum(o("96"),i)*oum(o("6"),i)
ENDFOR
DO CIKL1 WITH o("96"),sum/SUM('oum(o("6"),'),'    -',sum/SUM('oum(o("6"),'),'    -'&&96 P�
DO CIKL WITH o("97"),'IIF(inm(i("74"),i)=="2�",inm(i("38"),i),inm(i("37"),i))' &&97 P�
sum=0
FOR i=1 TO n_blokov
    sum=sum+oum(o("97"),i)*oum(o("7"),i)
ENDFOR
DO CIKL1 WITH o("97"),sum/SUM('oum(o("7"),'),'    -',sum/SUM('oum(o("7"),'),'    -'&&97 P�
DO CIKL WITH o("98"),'iom(u("51.1"),i)'                                        &&98 t o
sum=0
FOR i=1 TO n_blokov
    sum=sum+oum(o("98"),i)*oum(o("4"),i)
ENDFOR
DO CIKL1 WITH o("98"),sum/SUM('oum(o("4"),'),'    -',sum/SUM('oum(o("4"),'),'    -'&&98 t o
DO CIKL WITH o("99"),'540'                                                     &&99 t o (�)
DO CIKL1 WITH o("99"),540,'    -',540,'    -'                                  &&99 t o (�)
DO CIKL WITH o("100"),'IIF(inm(i("74"),i)=="1",F1(oum(o("98"),i),"2.70:1"),'+;
'IIF(inm(i("74"),i)=="2" OR inm(i("74"),i)=="2�" OR inm(i("74"),i)=="3",F3(iom(u("50"),i),iom(u("10"),i),oum(o("98"),i),"2.71:3")'+;
',1/0))'                                                                       &&100 alfa q�(t �)
DO CIKL WITH o("101"),'iom(u("52.1"),i)'                                       &&101 t ���
sum=0
FOR i=1 TO n_blokov
    sum=sum+oum(o("101"),i)*oum(o("4"),i)
ENDFOR
DO CIKL1 WITH o("101"),sum/SUM('oum(o("4"),'),'    -',sum/SUM('oum(o("4"),'),'    -'&&101 t ���
DO CIKL WITH o("102"),'540'                                                    &&102 t ��� (�)
DO CIKL1 WITH o("102"),540,'    -',540,'    -'                                 &&102 t ��� (�)
DO CIKL WITH o("103"),'IIF(inm(i("74"),i)=="1",F1(oum(o("101"),i),"2.72:1"),'+;
'IIF(inm(i("74"),i)=="2" OR inm(i("74"),i)=="2�" OR inm(i("74"),i)=="3",F3(iom(u("50"),i),iom(u("10"),i),oum(o("101"),i),"2.73:3")'+;
',1/0))'                                                                       &&103 alfa q�(���)
DO CIKL WITH o("104"),'oum(o("4"),i)-oum(o("50"),i)'                           &&104 �����
DO CIKL1 WITH o("104"),oum(o("4"),n_blokov+1)-oum(o("50"),n_blokov+1),'    -',;
oum(o("4"),n_blokov+1)-oum(o("50"),n_blokov+1),'    -'                         &&104 �����
*F BL5
*O CIKL WITH o("105"),'(1-inm(i("30"),i)/100)*735.6/inm(i("90"),n_blokov+1)'   &&105 P2
*LSE
DO CIKL WITH o("105"),'inm(i("30"),i)/98.067'                                  &&105 P2
*O ALTERC WITH o("105"),1,'inm(i("90"),n_blokov+1)/735.6-inm(i("30"),i)'       &&105 P2
*O ALTERC WITH o("105"),5,'inm(i("90"),n_blokov+1)/735.6-inm(i("30"),i)'       &&105 P2
DO ALTERC WITH o("105"),6,'inm(i("30"),i)'                                     &&105 P2
*NDIF
*O ALTERC WITH o("105"),1,'F1((inm(i("41.1"),i)+inm(i("41.2"),i))/2,"2.50:1")' &&105 P2
sum=0
FOR i=1 TO n_blokov
    sum=sum+oum(o("105"),i)*oum(o("104"),i)
ENDFOR
DO CIKL1 WITH o("105"),sum/SUM('oum(o("104"),'),'    -',sum/SUM('oum(o("104"),'),'    -'&&105 P2
DO CIKL WITH o("106"),'iom(u("15"),i)'                                         &&106 P2(�)
sum=0
FOR i=1 TO n_blokov
    sum=sum+oum(o("106"),i)*oum(o("104"),i)
ENDFOR
DO CIKL1 WITH o("106"),sum/SUM('oum(o("104"),'),'    -',sum/SUM('oum(o("104"),'),'    -'&&106 P2(�)
DO CIKL WITH o("107"),'IIF(iom(u("14"),i)<=100,F1(iom(u("14"),i),"2.45:1")*(oum(o("105"),i)-oum(o("106"),i))/.01,'+;
'IIF(iom(u("14"),i)>100,1.06*(oum(o("105"),i)-oum(o("106"),i))/.01,1/0))'      &&107 dN(P2)
DO CIKL WITH o("108"),'1.929*oum(o("107"),i)'                                  &&108 dQ�(P2)
DO CIKL WITH o("109"),'inm(i("28"),i)'                                         &&109 t 1
sum=0
FOR i=1 TO n_blokov
    sum=sum+oum(o("109"),i)*oum(o("104"),i)
ENDFOR
DO CIKL1 WITH o("109"),sum/SUM('oum(o("104"),'),'    -',sum/SUM('oum(o("104"),'),'    -'&&109 t 1
DO CIKL WITH o("110"),'inm(i("29"),i)'                                         &&110 t 2
sum=0
FOR i=1 TO n_blokov
    sum=sum+oum(o("110"),i)*oum(o("104"),i)
ENDFOR
DO CIKL1 WITH o("110"),sum/SUM('oum(o("104"),'),'    -',sum/SUM('oum(o("104"),'),'    -'&&110 t 2
DO CIKL WITH o("111"),'F1(oum(o("105"),i),"2.75:1")'                           &&111 t �
DO CIKL WITH o("112"),'oum(o("111"),i)-oum(o("110"),i)'                        &&112 dt
sum=0
FOR i=1 TO n_blokov
    sum=sum+oum(o("112"),i)*oum(o("104"),i)
ENDFOR
DO CIKL1 WITH o("112"),sum/SUM('oum(o("104"),'),'    -',sum/SUM('oum(o("104"),'),'    -'&&112 dt
DO CIKL WITH o("113"),'F3(iom(u("14"),i),oum(o("109"),i),iom(u("14.1"),n_blokov+1),"2.64:3")'&&113 dt (�)
sum=0
FOR i=1 TO n_blokov
    sum=sum+oum(o("113"),i)*oum(o("104"),i)
ENDFOR
DO CIKL1 WITH o("113"),sum/SUM('oum(o("104"),'),'    -',sum/SUM('oum(o("104"),'),'    -'&&113 dt (�)
DO CIKL WITH o("114"),'oum(o("112"),i)-oum(o("113"),i)'                        &&114 dt''
DO CIKL WITH o("115"),'oum(o("111"),i)-oum(o("114"),i)'                        &&115 t � ''
DO CIKL WITH o("116"),'F1(oum(o("115"),i),"2.76:1")'                           &&116 P2 ''
DO CIKL WITH o("117"),'IIF(iom(u("14"),i)<=100,F1(iom(u("14"),i),"2.45:1")*(oum(o("105"),i)-oum(o("116"),i))/.01,'+;
'IIF(iom(u("14"),i)>100,1.06*(oum(o("105"),i)-oum(o("116"),i))/.01,1/0))'      &&117 dN (P2 '')
*O CIKL WITH o("118"),'IIF(iom(u("9"),i)<=175.4,1.91*oum(o("117"),i),'+;
*IIF(iom(u("9"),i)>175.4,1.831*oum(o("117"),i),1/0))'                          &&118 dQ (P2 '') 
DO CIKL WITH o("118"),'1.929*oum(o("117"),i)'                                  &&118 dQ (P2 '') 
DO CIKL WITH o("119"),'inm(i("26"),i)'                                         &&119 t ��
sum=0
FOR i=1 TO n_blokov
    sum=sum+oum(o("119"),i)*oum(o("44"),i)
ENDFOR
DO CIKL1 WITH o("119"),sum/SUM('oum(o("44"),'),'    -',sum/SUM('oum(o("44"),'),'    -'&&119 t ��
DO CIKL WITH o("120"),'iom(u("74"),i)'                                         &&120 t �� (�)
DO CIKL1 WITH o("120"),iom(u("74"),n_blokov+1),'    -',iom(u("74"),n_blokov+1),'    -'&&120 t �� (�)
DO CIKL WITH o("121"),'(oum(o("120"),i)-oum(o("119"),i))',.F.                  &&121 alfa q�(t��)
DO CIKL WITH o("121"),'IIF(inm(i("74"),i)=="1",F1(oum(o("121"),i),"2.77:1"),'+;
'IIF(inm(i("74"),i)=="2" OR inm(i("74"),i)=="2�" OR inm(i("74"),i)=="3",F2(iom(u("50"),i),iom(u("10"),i),"2.78:2")*oum(o("121"),i)/2,'+;
'1/0))'                                                                        &&121 alfa q�(t��)
DO CIKL WITH o("123"),'IIF(inm(i("59"),i)=100,inm(i("8.2"),i)+inm(i("8.4"),i),'+;
'inm(i("8.2"),i)+inm(i("8.4"),i)/inm(i("1"),i)*(inm(i("1"),i)-inm(i("70.1"),i))*'+;
'.35+inm(i("8.4"),i)/inm(i("1"),i)*inm(i("70.1"),i))'                          &&123 � ��
DO CIKL1 WITH o("123"),SUM('oum(o("123"),'),'    -',SUM('oum(o("123"),'),'    -'&&123 � ��
DO CIKL WITH o("124"),'iom(u("95"),i)*oum(o("17"),i)/1E3'                      &&124 � �� (�)
DO CIKL1 WITH o("124"),SUM('oum(o("124"),'),'    -',SUM('oum(o("124"),'),'    -'&&124 � �� (�)
DO CIKL WITH o("126"),'oum(o("78"),i)*7000/inm(i("53"),n_blokov+1)'            &&126 B ��
DO CIKL1 WITH o("126"),SUM('oum(o("126"),'),'    -',SUM('oum(o("126"),'),'    -'&&126 B ��
DO CIKL WITH o("127"),'iom(u("98"),i)*oum(o("126"),i)/1E3'                     &&127 � �� (�)
DO CIKL1 WITH o("127"),SUM('oum(o("127"),'),'    -',SUM('oum(o("127"),'),'    -'&&127 � �� (�)
IF BL1 OR BL2 AND RealTime && ���������� �������� ���� ���� ������ ��� ��������� '/1' � '/2' � ������.����.
DO CIKL WITH o("125"),'IIF(BETW(inm(i("59"),i),90,99.99),oum(o("127"),i),'+;
'IIF(inm(i("59"),i)=100,0,inm(i("8.3"),i)+inm(i("8.4"),i)*0.65))'              &&125 � ��
ELSE
DO CIKL WITH o("125"),'IIF(inm(i("59"),i)=100,0,inm(i("8.3"),i)+inm(i("8.4"),i)/'+;
'inm(i("1"),i)*(inm(i("1"),i)-inm(i("70.1"),i))*.65)'                          &&125 � ��
ENDIF               && ���������� �������� ���� ���� ������ ��� ��������� '/1' � '/2' � ������.����.
DO CIKL1 WITH o("125"),SUM('oum(o("125"),'),'    -',SUM('oum(o("125"),'),'    -'&&125 � ��
DO CIKL WITH o("129"),'iom(u("9"),i)'                                          &&129 Q� ��
DO CIKL1 WITH o("129"),iom(u("9"),n_blokov+1),'    -',SUM('oum(o("129"),'),'    -'&&129 Q� ��
DO CIKL WITH o("130"),'iom(u("10"),i)'                                         &&130 Q� ��
DO CIKL1 WITH o("130"),iom(u("10"),n_blokov+1),'    -',SUM('oum(o("130"),'),'    -'&&130 Q� ��
DO CIKL WITH o("131"),'iom(u("4"),i)/iom(u("1"),i)'                            &&131 Q �� ��
DO CIKL1 WITH o("131"),iom(u("4"),n_blokov+1)/iom(u("1"),n_blokov+1),'    -',;
SUM('oum(o("131"),'),'    -'                                                   &&131 Q �� ��
DO CIKL WITH o("132"),'oum(o("130"),i)+oum(o("131"),i)'                        &&132 Q ��� ��
DO CIKL1 WITH o("132"),oum(o("130"),n_blokov+1)+oum(o("131"),n_blokov+1),'    -',;
SUM('oum(o("132"),'),'    -'                                                   &&132 Q ��� ��
DO CIKL1 WITH o("133"),oum(o("13"),n_blokov+1)/oum(o("11"),n_blokov+3)*100,'    -','    -',;
'    -'                                                                        &&133 alfa ���
DO CIKL1 WITH o("134"),oum(o("12"),n_blokov+3)/oum(o("11"),n_blokov+3)*100,'    -','    -',;
'    -'                                                                        &&134 alfa �.�.
DO CIKL WITH o("135"),'oum(o("30"),i)/oum(o("4"),i)*100'                       &&135 ���
DO CIKL1 WITH o("135"),oum(o("30"),n_blokov+1)/oum(o("4"),n_blokov+1)*100,'    -','    -',;
'    -'                                                                        &&135 ���
DO CIKL WITH o("136"),'(oum(o("36"),i)+oum(o("37"),i)+oum(o("38"),i))/oum(o("4"),i)*100'&&136 ��� (�)
DO CIKL1 WITH o("136"),(oum(o("36"),n_blokov+1)+oum(o("37"),n_blokov+1)+;
oum(o("38"),n_blokov+1))/oum(o("4"),n_blokov+1)*100,'    -','    -','    -'    &&136 ��� (�)
DO CIKL WITH o("137"),'oum(o("34"),i)/oum(o("4"),i)*100'                       &&137
DO CIKL1 WITH o("137"),oum(o("34"),n_blokov+1)/oum(o("4"),n_blokov+1)*100,'    -',;
'    -','    -'                                                                &&137
DO CIKL WITH o("138"),'oum(o("39"),i)/oum(o("4"),i)*100'                       &&138 �� ��(�)
DO CIKL1 WITH o("138"),oum(o("39"),n_blokov+1)/oum(o("4"),n_blokov+1)*100,'    -',;
'    -','    -'                                                                &&138 �� ��(�)
DO CIKL WITH o("139"),'oum(o("35"),i)/oum(o("11"),i)*1E3'                      &&139 ��� ��
DO CIKL1 WITH o("139"),oum(o("35"),n_blokov+1)/oum(o("11"),n_blokov+1)*1E3,'    -',;
'    -','    -'                                                                &&139 ��� ��
DO CIKL WITH o("140"),'oum(o("40"),i)/oum(o("11"),i)*1E3'                      &&140 ��� ��(�)
DO CIKL1 WITH o("140"),oum(o("40"),n_blokov+1)/oum(o("11"),n_blokov+1)*1E3,'    -',;
'    -','    -'                                                                &&140 ��� ��(�)
DO CIKL WITH o("141"),'oum(o("4"),i)/200/inm(i("70"),n_blokov+1)*100'          &&141 K� �
DO CIKL1 WITH o("141"),oum(o("4"),n_blokov+1)/1200/inm(i("70"),n_blokov+1)*100,'    -',;
'    -','    -'                                                                &&141 K� �
DO CIKL WITH o("142"),'oum(o("9"),i)/240/inm(i("70"),n_blokov+1)*100'          &&142 K� ��
DO CIKL1 WITH o("142"),oum(o("9"),n_blokov+1)/1440/inm(i("70"),n_blokov+1)*100,'    -',;
'    -','    -'                                                                &&142 K� ��
DO CIKL WITH o("143"),'(1-oum(o("106"),i))*100'                                &&143 V (�)
DO CIKL1 WITH o("143"),(1-oum(o("106"),n_blokov+1))*100,'    -',;
'    -','    -'                                                                &&143 V (�)
DO CIKL WITH o("144"),'(1-oum(o("105"),i))*100'                                &&144 V
DO CIKL1 WITH o("144"),(1-oum(o("105"),n_blokov+1))*100,'    -',;
'    -','    -'                                                                &&144 V
DO CIKL WITH o("145"),'oum(o("16"),i)/oum(o("15"),i)*1E2'                    &&145 q� ��(�)
DO CIKL1 WITH o("145"),oum(o("16"),n_blokov+1)/oum(o("15"),n_blokov+1)*1E2,'    -',;
'    -','    -'                                                                &&145 q� ��(�)
DO CIKL WITH o("146"),'oum(o("16.1"),i)/oum(o("15"),i)*1E2'                      &&146 q� ��
DO CIKL1 WITH o("146"),oum(o("16.1"),n_blokov+1)/oum(o("15"),n_blokov+1)*1E2,'    -',;
'    -','    -'                                                                &&146 q� ��
DO CIKL WITH o("147"),'oum(o("36"),i)/oum(o("4"),i)*1E2'                       &&147 �� ��(�)
DO CIKL1 WITH o("147"),oum(o("36"),n_blokov+1)/oum(o("4"),n_blokov+1)*1E2,'    -',;
'    -','    -'                                                                &&147 �� ��(�)
DO CIKL WITH o("148"),'oum(o("28"),i)/oum(o("4"),i)*1E2'                       &&148 �� ��
DO CIKL1 WITH o("148"),oum(o("28"),n_blokov+1)/oum(o("4"),n_blokov+1)*1E2,'    -',;
'    -','    -'                                                                &&148 �� ��
DO CIKL WITH o("149"),'oum(o("42"),i)/oum(o("104"),i)*1E2'                     &&149 ��� (�)
DO CIKL1 WITH o("149"),oum(o("42"),n_blokov+1)/oum(o("104"),n_blokov+1)*1E2,'    -',;
'    -','    -'                                                                &&149 ��� (�)
DO CIKL WITH o("150"),'oum(o("41"),i)/oum(o("104"),i)*1E2'                     &&150 ���
DO CIKL1 WITH o("150"),oum(o("41"),n_blokov+1)/oum(o("104"),n_blokov+1)*1E2,'    -',;
'    -','    -'                                                                &&150 ���
DO CIKL WITH o("151"),'oum(o("56"),i)*(100+oum(o("145"),i))/(100-oum(o("147"),i))'&&151 q� �(�)
DO CIKL1 WITH o("151"),oum(o("56"),n_blokov+1)*(100+oum(o("145"),n_blokov+1))/;
(100-oum(o("147"),n_blokov+1)),'    -','    -','    -'                         &&151 q� �(�)
DO CIKL WITH o("152"),'oum(o("55"),i)*(100+oum(o("146"),i))/(100-oum(o("148"),i))'&&152 q� �
DO CIKL1 WITH o("152"),oum(o("55"),n_blokov+1)*(100+oum(o("146"),n_blokov+1))/;
(100-oum(o("148"),n_blokov+1)),'    -','    -','    -'                         &&152 q� �
DO CIKL WITH o("153"),'oum(o("38"),i)/oum(o("11"),i)*1E3'                      &&153 ����� (�)
DO CIKL1 WITH o("153"),oum(o("38"),n_blokov+1)/oum(o("11"),n_blokov+1)*1E3,'    -',;
'    -','    -'                                                                &&153 ����� (�)
DO CIKL WITH o("154"),'oum(o("27"),i)/oum(o("11"),i)*1E3'                      &&154 �����
DO CIKL1 WITH o("154"),oum(o("27"),n_blokov+1)/oum(o("11"),n_blokov+1)*1E3,'    -',;
'    -','    -'                                                                &&154 �����
DO CIKL WITH o("155"),'oum(o("37"),i)/oum(o("4"),i)*1E2'                       &&155 �� ��(�)
DO CIKL1 WITH o("155"),oum(o("37"),n_blokov+1)/oum(o("4"),n_blokov+1)*1E2,'    -','    -','    -'                                                                &&155 �� ��(�)
DO CIKL WITH o("156"),'oum(o("29"),i)/oum(o("4"),i)*1E2'                       &&156 �� ��
DO CIKL1 WITH o("156"),oum(o("29"),n_blokov+1)/oum(o("4"),n_blokov+1)*1E2,'    -','    -','    -'                                                                &&156 �� ��
DO CIKL WITH o("157"),'oum(o("127"),i)/oum(o("126"),i)*1E3'                    &&157 ���(�)
DO CIKL1 WITH o("157"),oum(o("127"),n_blokov+1)/oum(o("126"),n_blokov+1)*1E3,'    -',;
'    -','    -'                                                                &&157 ���(�)
DO CIKL WITH o("158"),'oum(o("125"),i)/oum(o("126"),i)*1E3'                    &&158 ��� 
DO CIKL1 WITH o("158"),oum(o("125"),n_blokov+1)/oum(o("126"),n_blokov+1)*1E3,'    -',;
'    -','    -'                                                                &&158 ��� 
DO CIKL WITH o("159"),'oum(o("45"),i)/oum(o("44"),i)'                          &&159 ���� (�)
DO CIKL1 WITH o("159"),oum(o("45"),n_blokov+1)/oum(o("44"),n_blokov+1),'    -',;
'    -','    -'                                                                &&159 ���� (�)
DO CIKL WITH o("160"),'oum(o("43"),i)/oum(o("44"),i)'                          &&160 ����
DO CIKL1 WITH o("160"),oum(o("43"),n_blokov+1)/oum(o("44"),n_blokov+1),'    -',;
'    -','    -'                                                                &&160 ����
DO CIKL WITH o("161"),'oum(o("124"),i)/oum(o("17"),i)*1E3'                     &&161 ��� (�)
DO CIKL1 WITH o("161"),oum(o("124"),n_blokov+1)/oum(o("17"),n_blokov+1)*1E3,'    -',;
'    -','    -'                                                                &&161 ��� (�)
DO CIKL WITH o("162"),'oum(o("123"),i)/oum(o("17"),i)*1E3'                     &&162 ���
DO CIKL1 WITH o("162"),oum(o("123"),n_blokov+1)/oum(o("17"),n_blokov+1)*1E3,'    -',;
'    -','    -'                                                                &&162 ���
DO CIKL WITH o("163"),'iom(u("50"),i)'                                         &&163 D��
DO CIKL WITH o("163.1"),'iom(u("66.1"),i)'                                     &&163.1 D �� ��
DO CIKL WITH o("164"),'iom(u("65"),i)'                                         &&164 Q� ��
DO CIKL1 WITH o("164"),iom(u("65"),n_blokov+1),'    -','    -','    -'         &&164 Q� ��
DO CIKL WITH o("165"),'iom(u("56"),i)'                                         &&165 P�
sum=0
FOR i=1 TO n_blokov
    sum=sum+oum(o("165"),i)*oum(o("17"),i)
ENDFOR
DO CIKL1 WITH o("165"),sum/SUM('oum(o("17"),'),'    -','    -','    -'         &&165 P�
*O CIKL WITH o("166"),'(inm(i("17"),i)+inm(i("17.1"),i))/2'                    &&166 t �
DO CIKL WITH o("166"),'iom(u("51"),i)'                                         &&166 t �
*O ALTERC WITH o("166"),1,'iom(u("51"),i)'                                     &&166 t �
*O ALTERC WITH o("166"),5,'iom(u("51"),i)'                                     &&166 t �
sum=0
FOR i=1 TO n_blokov
    sum=sum+oum(o("166"),i)*oum(o("17"),i)
ENDFOR
DO CIKL1 WITH o("166"),sum/SUM('oum(o("17"),'),'    -','    -','    -'         &&166 t �
DO CIKL WITH o("167"),'(inm(i("33"),i)+inm(i("34"),i))/2'                      &&167 t ��
sum=0
FOR i=1 TO n_blokov
    sum=sum+oum(o("167"),i)*oum(o("17"),i)
ENDFOR
DO CIKL1 WITH o("167"),sum/SUM('oum(o("17"),'),'    -','    -','    -'         &&167 t ��
DO CIKL WITH o("168"),'F2(oum(o("164"),i),inm(i("59"),i),"2.41:2")'            &&168 ��� (�)
DO ALTERC WITH o("168"),3,'3.4'                                                &&168 ��� (�)
DO ALTERC WITH o("168"),4,'3.4'                                                &&168 ��� (�)
DO ALTERC WITH o("168"),5,'4.1'                                                &&168 ��� (�)
*O ALTERC WITH o("168"),5,'F1(oum(o("164"),i),"2.42:1")'                       &&168 ��� (�)
DO ALTERC WITH o("168"),6,'F1(oum(o("164"),i),"2.41:1(6)")'                    &&168 ��� (�)
sum=0
FOR i=1 TO n_blokov
    sum=sum+oum(o("168"),i)*oum(o("78"),i)
ENDFOR
DO CIKL1 WITH o("168"),sum/SUM('oum(o("78"),'),'    -','    -','    -'         &&168 ��� (�)
*DO CIKL WITH o("169"),'IIF(inm(i("59"),i)=100,0,inm(i("57"),i))'               &&169 ���
DO CIKL WITH o("169"),'inm(i("57"),i)*iom(u("89"),i)/1E2'                      &&169 ���
sum=0
FOR i=1 TO n_blokov
    sum=sum+oum(o("169"),i)*oum(o("78"),i)
ENDFOR
DO CIKL1 WITH o("169"),sum/SUM('oum(o("78"),'),'    -','    -','    -'         &&169 ���
DO CIKL WITH o("170"),'F2(oum(o("164"),i),inm(i("59"),i),"2.43:2")'            &&170 ��� (�)
DO ALTERC WITH o("170"),3,'F1(oum(o("164"),i),"2.43:1")'                       &&170 ��� (�)
DO ALTERC WITH o("170"),4,'F1(oum(o("164"),i),"2.43:1")'                       &&170 ��� (�)
DO ALTERC WITH o("170"),5,'4'                                                  &&170 ��� (�)
*O ALTERC WITH o("170"),5,'F1(oum(o("164"),i),"2.44:1")'                       &&170 ��� (�)
DO ALTERC WITH o("170"),6,'F1(oum(o("164"),i),"2.43:1(6)")'                    &&170 ��� (�)
sum=0
FOR i=1 TO n_blokov
    sum=sum+oum(o("170"),i)*oum(o("78"),i)
ENDFOR
DO CIKL1 WITH o("170"),sum/SUM('oum(o("78"),'),'    -','    -','    -'         &&170 ��� (�)
*DO CIKL WITH o("171"),'IIF(inm(i("59"),i)=100,0,inm(i("56"),i))'              &&171 ���
DO CIKL WITH o("171"),'inm(i("56"),i)*iom(u("89"),i)/1E2'                      &&171 ���
sum=0
FOR i=1 TO n_blokov
    sum=sum+oum(o("171"),i)*oum(o("78"),i)
ENDFOR
DO CIKL1 WITH o("171"),sum/SUM('oum(o("78"),'),'    -','    -','    -'         &&171 ���
DO CIKL WITH o("172"),'100-oum(o("68"),i)-oum(o("57"),i)-oum(o("73"),i)'       &&172 q ������
DO CIKL1 WITH o("172"),100-oum(o("68"),n_blokov+1)-oum(o("57"),n_blokov+1)-;
oum(o("73"),n_blokov+1),'    -','    -','    -'                                &&172 q ������
DO CIKL WITH o("173"),'(oum(o("62"),i)+10*SQRT(472.2/iom(u("65"),i)))/100'     &&173 dalfa (�)
sum=0
FOR i=1 TO n_blokov
    sum=sum+oum(o("173"),i)*oum(o("17"),i)
ENDFOR
DO CIKL1 WITH o("173"),sum/SUM('oum(o("17"),'),'    -','    -','    -'         &&173 dalfa (�)
DO CIKL WITH o("174"),'(oum(o("61"),i)+oum(o("63"),i))/100'                    &&174 dalfa
sum=0
FOR i=1 TO n_blokov
    sum=sum+oum(o("174"),i)*oum(o("17"),i)
ENDFOR
DO CIKL1 WITH o("174"),sum/SUM('oum(o("17"),'),'    -','    -','    -'         &&174 dalfa
DO CIKL WITH o("175"),'oum(o("18"),i)/oum(o("17"),i)*100'                      &&175 q � �� (�)
DO CIKL1 WITH o("175"),oum(o("18"),n_blokov+1)/oum(o("17"),n_blokov+1)*100,;
'    -','    -','    -'                                                        &&175 q � �� (�)
DO CIKL WITH o("175.1"),'oum(o("19"),i)/oum(o("17"),i)*100'                    &&175.1 q � ��
DO CIKL1 WITH o("175.1"),oum(o("19"),n_blokov+1)/oum(o("17"),n_blokov+1)*100,;
'    -','    -','    -'                                                        &&175.1 q � ��
DO CIKL WITH o("176"),'oum(o("73"),i)*(100-oum(o("175.1"),i))/100*(100-oum(o("137"),i))/'+;
'(100-oum(o("148"),i))'                                                        &&176 ����"�����"
DO CIKL1 WITH o("176"),oum(o("73"),n_blokov+1)*(100-oum(o("175.1"),n_blokov+1))/100*;
(100-oum(o("137"),n_blokov+1))/(100-oum(o("148"),n_blokov+1)),'    -','    -','    -'&&176 ����"�����"
DO CIKL WITH o("177"),'oum(o("74"),i)*(100-oum(o("175"),i))/100*(100-oum(o("138"),i))/'+;
'(100-oum(o("147"),i))'                                                        &&177 ����"�����"(�)
DO CIKL1 WITH o("177"),oum(o("74"),n_blokov+1)*(100-oum(o("175"),n_blokov+1))/100*;
(100-oum(o("138"),n_blokov+1))/(100-oum(o("147"),n_blokov+1)),'    -','    -','    -'&&177 ����"�����"(�)
DO CIKL1 WITH o("178"),(inm(i("78"),n_blokov+1)+inm(i("79"),n_blokov+1))/1E3,'    -',;
'    -','    -'                                                                &&178 G���
DO CIKL1 WITH o("179"),inm(i("80"),n_blokov+1),'    -','    -','    -'         &&179 G��� (�)
DO CIKL1 WITH o("179.1"),oum(o("179"),n_blokov+1)*oum(o("44"),n_blokov+1)/100,'    -','    -',;
'    -'                                                                        &&179.1 G��� (�)
DO CIKL1 WITH o("180"),oum(o("178"),n_blokov+1)/oum(o("44"),n_blokov+1)*1E2,'    -','    -',;
'    -'                                                                        &&180 G���
DO CIKL WITH o("181"),'inm(i("27"),i)/1E3'                                     &&181 G����
DO CIKL1 WITH o("181"),SUM('inm(i("27"),')/1E3,'    -','    -','    -'         &&181 G����
DO CIKL WITH o("182"),'oum(o("181"),i)/oum(o("44"),i))*100'                    &&182 G����
DO CIKL1 WITH o("182"),oum(o("181"),n_blokov+1)/oum(o("44"),n_blokov+1)*100,'    -','    -',;
'    -'                                                                        &&182 G����
DO CIKL WITH o("183"),'iom(u("130"),i)'                                        &&183 K��
DO CIKL1 WITH o("183"),iom(u("130"),n_blokov+1),'    -','    -','    -'        &&183 K��
DO CIKL WITH o("95.1"),'oum(o("95"),i)*oum(o("4"),i)*10/(oum(o("176"),i)*oum(o("183"),i)*7)'&&95.1 dB(P�)
DO CIKL1 WITH o("95.1"),SUM('oum(o("95.1"),'),'    -',SUM('oum(o("95.1"),'),'    -'&&95.1 dB(P�)
DO CIKL WITH o("100.1"),'oum(o("100"),i)*oum(o("4"),i)*10/(oum(o("176"),i)*oum(o("183"),i)'+;
'*7)'                                                                          &&100.1 dB�(t�)
DO CIKL1 WITH o("100.1"),SUM('oum(o("100.1"),'),'    -',SUM('oum(o("100.1"),'),'    -'&&100.1 dB�(t�)
DO CIKL WITH o("103.1"),'IIF(inm(i("74"),i)=="1",oum(o("81"),i)*oum(o("31"),i)*oum(o("103"),i)/'+;
'1E5,IIF(inm(i("74"),i)=="2" OR inm(i("74"),i)=="2�" OR inm(i("74"),i)=="3",oum(o("103"),i)*'+;
'oum(o("4"),i)*10/(oum(o("176"),i)*oum(o("183"),i)*7),1/0))'                   &&103.1 dB (t���)
DO CIKL1 WITH o("103.1"),SUM('oum(o("103.1"),'),'    -',SUM('oum(o("103.1"),'),'    -'&&103.1 dB (t���)
DO CIKL WITH o("108.1"),'oum(o("108"),i)*oum(o("20"),i)*1E4/(oum(o("176"),i)*oum(o("183"),i)'+;
'*7)'                                                                          &&108.1 dB(P2)
DO CIKL1 WITH o("108.1"),SUM('oum(o("108.1"),'),'    -',SUM('oum(o("108.1"),'),'    -'&&108.1 dB(P2)
DO CIKL WITH o("118.1"),'oum(o("118"),i)*oum(o("20"),i)*1E4/(oum(o("176"),i)*oum(o("183"),i)'+;
'*7)'                                                                          &&118.1 dB(dt)
DO CIKL1 WITH o("118.1"),SUM('oum(o("118.1"),'),'    -',SUM('oum(o("118.1"),'),'    -'&&118.1 dB(dt)
DO CIKL WITH o("121.1"),'oum(o("121"),i)*oum(o("4"),i)*10/(oum(o("176"),i)*oum(o("183"),i)*7)'&&121.1 dB (t ��)
DO CIKL1 WITH o("121.1"),SUM('oum(o("121.1"),'),'    -',SUM('oum(o("121.1"),'),'    -'&&121.1 dB (t ��)
DO CIKL WITH o("60.1"),'oum(o("69"),i)*oum(o("75"),i)/oum(o("74"),i)*((iom(u("80"),i)*'+;
'(oum(o("59"),i)+oum(o("62"),i)/1E2)+iom(u("81"),i))*(oum(o("67"),i)-(oum(o("59"),i)+'+;
'oum(o("62"),i)/1E2)*oum(o("64"),i)/(oum(o("59"),i)+oum(o("62"),i)/1E2+iom(u("82"),i)))/'+;
'(iom(u("80"),i)*(oum(o("60"),i)+oum(o("62"),i)/1E2)+iom(u("81"),i))/(oum(o("67"),i)-'+;
'(oum(o("60"),i)+oum(o("62"),i)/1E2)*oum(o("64"),i)/(oum(o("60"),i)+oum(o("62"),i)/1E2+'+;
'iom(u("82"),i)))-1)'                                                          &&60.1 dB(alfa �)
DO CIKL1 WITH o("60.1"),SUM('oum(o("60.1"),'),'    -',SUM('oum(o("60.1"),'),'    -'&&60.1 dB(alfa �)
DO CIKL WITH o("62.1"),'oum(o("69"),i)*oum(o("75"),i)/oum(o("74"),i)*((iom(u("80"),i)*'+;
'(oum(o("60"),i)+oum(o("61"),i)/1E2)+iom(u("81"),i))*(oum(o("67"),i)-(oum(o("60"),i)+'+;
'oum(o("61"),i)/1E2)*oum(o("64"),i)/(oum(o("60"),i)+oum(o("61"),i)/1E2+iom(u("82"),i)))/'+;
'(iom(u("80"),i)*(oum(o("60"),i)+oum(o("62"),i)/1E2)+iom(u("81"),i))/(oum(o("67"),i)-'+;
'(oum(o("60"),i)+oum(o("62"),i)/1E2)*oum(o("64"),i)/(oum(o("60"),i)+oum(o("62"),i)/1E2+'+;
'iom(u("82"),i)))-1)'                                                          &&62.1 dB(dalfa ���)
DO CIKL1 WITH o("62.1"),SUM('oum(o("62.1"),'),'    -',SUM('oum(o("62.1"),'),'    -'&&62.1 dB(dalfa ���)
DO CIKL WITH o("184"),'oum(o("24"),i)-oum(o("25"),i)'                          &&184 n ��
DO CIKL1 WITH o("184"),SUM('oum(o("184"),'),'    -',SUM('oum(o("184"),'),'    -'&&184 n ��
DO CIKL WITH o("185"),'oum(o("95.1"),i)+oum(o("100.1"),i)+oum(o("103.1"),i)+'+;
'oum(o("108.1"),i)+oum(o("121.1"),i)'                                          &&185 ��� dB �
DO CIKL1 WITH o("185"),SUM('oum(o("185"),'),'    -','    -','    -'            &&185 ��� dB �
DO CIKL WITH o("186"),'oum(o("67.2"),i)+oum(o("60.1"),i)+oum(o("62.1"),i)+'+;
'oum(o("25.1"),i)'                                                             &&186 ��� dB �
DO CIKL1 WITH o("186"),SUM('oum(o("186"),'),'    -','    -','    -'            &&186 ��� dB �
DO CIKL WITH o("187"),'oum(o("17"),i)-(oum(o("15"),i)+(oum(o("11"),i)-oum(o("32"),i))*'+;
'(100+iom(u("125"),i))/1E2+oum(o("16.1"),i)+oum(o("19"),i))'                   &&187 ��������
DO CIKL1 WITH o("187"),oum(o("17"),n_blokov+1)-(oum(o("15"),n_blokov+1)+;
(oum(o("11"),n_blokov+1)-oum(o("32"),n_blokov+1))*(100+iom(u("125"),n_blokov+1))/1E2+;
oum(o("16.1"),n_blokov+1)+oum(o("19"),n_blokov+1)),'    -','    -','    -'     &&187 ��������
DO CIKL WITH o("188"),'oum(o("187"),i)/oum(o("17"),i)*1E2'                     &&188 ��������
DO CIKL1 WITH o("188"),oum(o("187"),n_blokov+1)/oum(o("17"),n_blokov+1)*1E2,'    -','    -',;
'    -'                                                                        &&188 ��������
DO CIKL WITH o("189"),'oum(o("76"),i)/oum(o("75"),i)*1E2'                      &&189 alfa ����
DO CIKL1 WITH o("189"),oum(o("76"),n_blokov+1)/oum(o("75"),n_blokov+1)*1E2,'    -','    -',;
'    -'                                                                        &&189 alfa ����
DO CIKL1 WITH o("190"),(inm(i("50"),n_blokov+1)+inm(i("51"),n_blokov+1))*1E3/;
inm(i("70"),n_blokov+1),'    -','    -','    -'                                &&190 G ��� ��

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
