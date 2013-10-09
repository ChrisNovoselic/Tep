**********************************************************************
PROC CALC_END
IF NOT nfld_arr==''
   DIMENSION outm2(6-n_blokov)
   FOR j=1 TO ALEN(outm2)
       outm2(j)='    -'            && � ���.�.- ��������(�����,�����.
   ENDFOR                          && 's')
ENDIF
SELECT 2                           && �.�. OUTBLOK.DBF,OUTMKT.DBF
GO TOP
FOR i=1 TO ALEN(outm,1)
FOR j=1 TO ALEN(outm,2)
    IF SUBSTR(outm(i,j),1,1)='='   && ��.���.�.�� �������������� =>
       outm1(j)=outm(i,j)          && ��������� ��������
    ELSE
    IF TYPE("oum(i,j)")='C'        && ��������.���
       outm1(j)='    -'
    ELSE   
       outm1(j)=IIF(ABS(oum(i,j))<1E308,STR(oum(i,j),10,exact(i,1)),'    -')
    ENDIF   
    ENDIF   
ENDFOR    
    GATHER FROM outm1 FIELDS &fld_arr
    IF NOT nfld_arr==''
       GATHER FROM outm2 FIELDS &nfld_arr
    ENDIF
    SKIP
ENDFOR
RELEASE inm,iom,outm
**********************************************************************
FUNCTION i
PARAMETER orde
SET EXACT ON
iorde=ASCAN(Iinm,orde)
SET EXACT OFF
RETURN(iorde)
**********************************************************************
FUNCTION u
PARAMETER orde
SET EXACT ON
iorde=ASCAN(Iiom,orde)
SET EXACT OFF
RETURN(iorde)
**********************************************************************
FUNCTION o
PARAMETER orde
SET EXACT ON
iorde=ASCAN(Ioutm,orde)
SET EXACT OFF
RETURN(iorde)
**********************************************************************
PROC CIKL
PARAMETERS m,part,round
IF PARAM()=2
   round=.T.
ENDIF
PRIVATE i
FOR i=1 TO n_blokov
** ��.���.�.�� �������������� => ���������� �� ������
    oum(m,i)=IIF(SUBSTR(outm(m,i),1,1)='=',VAL(oum(m,i)),;
    IIF(round,ROUND(&part,exact(m,1)),&part))
ENDFOR
*@ 0,11 SAY STR(m,4)
@ 0,11 SAY PADL(Ioutm(m),6,' ')
**********************************************************************
FUNCTION SUM
PARAMETERS part
PRIVATE i,sum
sum=0
FOR i=1 TO n_blokov
  sum=sum+IIF(1E308<EVAL(part+STR(i,1)+')'),0,EVAL(part+STR(i,1)+')'))
ENDFOR
RETURN sum
**********************************************************************
PROCEDURE ALTERC
PARAMETERS m,j,part
PRIVATE i
IF ASCAN(ai,j)>0
   i=ASCAN(ai,j)
   oum(m,i)=IIF(SUBSTR(outm(m,i),1,1)='=',VAL(oum(m,i)),;
                ROUND(&part,exact(m,1)))
ENDIF
**********************************************************************
FUNCTION F3
PARAMETERS x1,x2,x3,y     && �������� 3-�� ���������� � �������.�������
SET EXACT ON      && ������ �����
SEEK y
SET EXACT OFF
IF NOT FOUND()
   DO SEEK_Y
   RETURN 0
ENDIF
COPY TO ARRAY a WHILE TRIM(id)==y FIELDS a1,a2,a3,f   && ����-�� ������� ���-�������
usl1=.T.                  && ������� 3-���������� �������
DO MIN WITH x3,3,min31,aa31,min32,aa32,'.T.'
*IF y='2.71:3'
*answer=MESSAGEBOX('x3='+STR(x3),0)
*ENDIF
DO ADEL WITH 3,min31,min32
DO MIN WITH x2,2,min211,aa211,min212,aa212,'a(ii,3)=min31'
DO MIN WITH x2,2,min221,aa221,min222,aa222,'a(ii,3)=min32'
DO ADEL WITH 2,min211,min212,min221,min222
DO MIN WITH x1,1,min1111,aa1111,min1112,aa1112,'a(ii,3)=min31 and a(ii,2)=min211'
DO MIN WITH x1,1,min1121,aa1121,min1122,aa1122,'a(ii,3)=min31 and a(ii,2)=min212'
DO MIN WITH x1,1,min1211,aa1211,min1212,aa1212,'a(ii,3)=min32 and a(ii,2)=min221'
DO MIN WITH x1,1,min1221,aa1221,min1222,aa1222,'a(ii,3)=min32 and a(ii,2)=min222'
y11=(x1-min1111)*(aa1112-aa1111)/(min1112-min1111)+aa1111
y12=(x1-min1121)*(aa1122-aa1121)/(min1122-min1121)+aa1121
y21=(x1-min1211)*(aa1212-aa1211)/(min1212-min1211)+aa1211
y22=(x1-min1221)*(aa1222-aa1221)/(min1222-min1221)+aa1221
y1=(x2-min211)*(y12-y11)/(min212-min211)+y11
y2=(x2-min221)*(y22-y21)/(min222-min221)+y21
RETURN (x3-min31)*(y2-y1)/(min32-min31)+y1
**********************************************************************
FUNCTION F2
PARAMETERS x1,x2,y
SET EXACT ON      && ������ �����
SEEK y
SET EXACT OFF
IF NOT FOUND()
   DO SEEK_Y
   RETURN 0
ENDIF
COPY TO ARRAY a WHILE TRIM(id)==y FIELDS a1,a2,f
usl1=.T.            && ������� 2-���������� �������
DO MIN WITH x2,2,min21,aa21,min22,aa22,'.T.'
*  IF y='2.65:2'
*  wait wind "min21="+str(min21)+" | "+"min22="+str(min22)
*  endif
DO ADEL WITH 2,min21,min22
DO MIN WITH x1,1,min111,aa111,min112,aa112,'a(ii,2)=min21'
*  IF y='2.65:2'
*  wait wind "min111="+str(min111)+" | "+"min112="+str(min112)
*  endif
DO MIN WITH x1,1,min121,aa121,min122,aa122,'a(ii,2)=min22'
*  IF y='2.65:2'
*  wait wind "min121="+str(min121)+" | "+"min122="+str(min122)
*  endif
y1=(x1-min111)*(aa112-aa111)/(min112-min111)+aa111
y2=(x1-min121)*(aa122-aa121)/(min122-min121)+aa121
*  IF y='2.65:2'
*  wait wind "y1="+str(y1,10,2)+" | "+"y2="+str(y2,10,2)
*  wait wind "x1="+str(x1,10,2)+" | "+"x2="+str(x2,10,2)
*  wait wind str((x2-min21)*(y2-y1)/(min22-min21)+y1,10,2)
*  endif
RETURN (x2-min21)*(y2-y1)/(min22-min21)+y1
**********************************************************************
FUNCTION F1
PARAMETERS x1,y
SET EXACT ON      && ������ �����
SEEK y
SET EXACT OFF
IF NOT FOUND()
   DO SEEK_Y      && PROC SEEK_Y � TEP.PRG
   RETURN 0
ENDIF
COPY TO ARRAY a WHILE TRIM(id)==y FIELDS a1,f
usl1=.F.
DO MIN WITH x1,1,min11,aa11,min12,aa12,'.T.'
*  IF y=='2.63:1'
*  wait wind set("exact")
*  wait wind "x1="+str(x1,10,2)+" | "+"min11="+str(min11,10,2)+" | "+;
*  "min12="+str(min12,10,2)
*  wait wind str((x1-min11)*(aa12-aa11)/(min12-min11)+aa11,10,2)
*  endif
RETURN (x1-min11)*(aa12-aa11)/(min12-min11)+aa11
**********************************************************************
PROC MIN  && ����� ��������� �������
* �������:x,n - �������� � ����� ���������, usl - ������� �����.������ ����-�.
* ��������:minX,aaX - ������ ��� x
PARAMETERS x,n,min1,aa1,min2,aa2,usl
PRIV j,k
STORE 1E15 TO min1
STORE -1E15 TO min2
FOR ii=1 TO ALEN(a,1)      && ���� �� 1-�� ���.������� �������-�������(���-�� �����)
  IF EVAL(usl)             && ������� ����������� � ������� �����(� ���������� �������)
    IF a(ii,n)<min1             && ����������� ������ ���������
       min1=a(ii,n)             && ���������(����������� ����������)
       aa1=a(ii,ALEN(a,2))      && �� ����������� (min1) �� ���������� (min2)
    ENDIF
    IF a(ii,n)>min2
       min2=a(ii,n)
       aa2=a(ii,ALEN(a,2))
    ENDIF
  ENDIF
ENDFOR
STORE .T. TO j,k
*iii=0
IF BETW(x,min1,min2) && ������������
FOR ii=1 TO ALEN(a,1)      && ���� �� 1-�� ���.������� �������-�������(���-�� �����)
  IF EVAL(usl)             && ������� ����������� � ������� �����(� ���������� �������)
     IF (x-a(ii,n))<(x-min1) and (x-a(ii,n))>=0 AND a(ii,n)#min2
        min1=a(ii,n)       && ���������� �������� �������-�-���
        aa1=a(ii,ALEN(a,2))&& � ���������=aa1 � ���������� ���.�����
        LOOP
     ENDIF
     IF (a(ii,n)-x)<(min2-x) and (a(ii,n)-x)>=0 AND a(ii,n)#min1
        min2=a(ii,n)       && ���������� �������� �������-�-���
        aa2=a(ii,ALEN(a,2))&& � ���������=aa2 � ���������� ���.�����
        LOOP
     ENDIF
  ENDIF        && ������� ����������� � ������� �����(� ���������� �������)
ENDFOR
ELSE                 && �������������
FOR ii=1 TO ALEN(a,1)      && ���� �� 1-�� ���.������� �������-�������(���-�� �����)
  IF EVAL(usl)             && ������� ����������� � ������� �����(� ���������� �������)
*   IF (j=.T. OR k=.T.) AND usl1  && ���� ���� �� ���� �� ������� �����������, � 
*      iii=iii+1           && ���-������ 2-3-�������; ������� iii+1
*      FOR nn=1 TO ALEN(a,2)      && 
*          a(iii,nn)=a(ii,nn)     && ����������� ���.������ � ������ �������
*      ENDFOR              && ���: �������� �� �� ���� ����� ��������
**��������� �� ������� ��� �������� � ������� �����������       
*   ENDIF
    IF j=.T.                 && ���������� 1-�� �������(min1) 1-� 
       min1=a(ii,n)          && ���������� �������� �������-�-���
       aa1=a(ii,ALEN(a,2))   && � ���������=aa1 � ���������� ���.�����
       j=.F.                 && �����, ������� �� �����
       LOOP                  && �����
    ENDIF
    IF k=.T. and min1#a(ii,n) && ���������� 2-�� �������(min2) 2-�
       min2=a(ii,n)           && ���������� �������� �������-�-���
       aa2=a(ii,ALEN(a,2))    && � ���������=aa2 � ���������� ���.�����
       k=.F.                  && �����,������� �� �����
       LOOP                   && �����
    ENDIF
    IF j=.F. AND k=.F.       && ������� ���-�� 2-�� ��������� �������
*      IF usl1               && ������� �������������� ���-�������
*      IF ABS(x-a(ii,n))<=ABS(x-min1) OR ABS(x-a(ii,n))<=ABS(x-min2)
*         iii=iii+1                    && ��� ����� � �����������
*         FOR nn=1 TO ALEN(a,2)        && �������+1, ���-�� � �����
*             a(iii,nn)=a(ii,nn)       && ������������
*         ENDFOR
*      ENDIF
*      ENDIF                 && ������� �������������� ���-�������
*-------------
       IF ABS(x-a(ii,n))<ABS(x-min1) and a(ii,n)#min2
          IF ABS(x-min1)<ABS(x-min2)
             min2=min1
             aa2=aa1
          ENDIF   
          min1=a(ii,n)                    && ���� ��������� �������� ���
          aa1=a(ii,ALEN(a,2))             && ����� � �����������, ���
          LOOP                            && min1 ���� min2,���-�� 
       ELSE                               && � ����� ������������,
         IF ABS(x-a(ii,n))<ABS(x-min2) and a(ii,n)#min1
            min2=a(ii,n)                  && ������� �� ����� �����
            aa2=a(ii,ALEN(a,2))           && ��������� ��������
            LOOP
         ENDIF   
       ENDIF
*-------------        
    ENDIF      && ������� ���-�� 2-�� ��������� �������
  ENDIF        && ������� ����������� � ������� �����(� ���������� �������)
ENDFOR
ENDIF          && ������������ - �������������
*F usl1        && ������� �������������� ���-�������
*  DIMENSION a(iii,ALEN(a,2))
*  usl1=.F.
*NDIF          && ������� �������������� ���-�������
**********************************************************************
PROC ADEL      && ���������� �������-�������
PARAMETERS n,min1,min2,min3,min4
usl='a(ii,n)=min1 OR a(ii,n)=min2'
usl=usl+IIF(PARAMETERS()>3,' OR a(ii,n)=min3 OR a(ii,n)=min4','')
iii=0
FOR ii=1 TO ALEN(a,1)
    IF EVAL(usl)
       iii=iii+1
       FOR nn=1 TO ALEN(a,2)
           a(iii,nn)=a(ii,nn)
       ENDFOR
    ENDIF
ENDFOR
DIMENSION a(iii,ALEN(a,2))
