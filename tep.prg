PARAMETER BL,KTSpath,RealTime,resolut
* ��������� BL=/1 ��� BL=/5 => ������ ����� 1 ��� 5
* KTSpath - ���� � ���-������
* �������� RealTime=/RT => ������ ���������� ��� � ������.�������
* �������� resolut => ���������� ������
PUBLIC BL1,BL2,BL3,BL4,BL5,BL6,;
		DefaultDir, ProgramDir
PRIVATE tauWork, resolutWidthDefault, resolutHeightDefault� RealTimeAllBlockMenuItems

resolutWidthDefault = 1100
resolutHeightDefault = 700
RealTimeAllBlockMenuItems = .T.

DO CASE
   CASE PARAM()=1
        BL1=IIF(BL='/1',.T.,.F.)
        BL2=IIF(BL='/2',.T.,.F.)
        BL3=IIF(BL='/3',.T.,.F.)
        BL4=IIF(BL='/4',.T.,.F.)
        BL5=IIF(BL='/5',.T.,.F.)
        BL6=IIF(BL='/6',.T.,.F.)
        KTSpath='W:\KTC'
        RealTime=.F.
        resolut=resolutWidthDefault 
   CASE PARAM()=2
        BL1=IIF(BL='/1',.T.,.F.)
        BL2=IIF(BL='/2',.T.,.F.)
        BL3=IIF(BL='/3',.T.,.F.)
        BL4=IIF(BL='/4',.T.,.F.)
        BL5=IIF(BL='/5',.T.,.F.)
        BL6=IIF(BL='/6',.T.,.F.)
        * KTSpath='W:\KTC'
        RealTime=.F.
        resolut=resolutWidthDefault 
   CASE PARAM()=3
        BL1=IIF(BL='/1',.T.,.F.)
        BL2=IIF(BL='/2',.T.,.F.)
        BL3=IIF(BL='/3',.T.,.F.)
        BL4=IIF(BL='/4',.T.,.F.)
        BL5=IIF(BL='/5',.T.,.F.)
        BL6=IIF(BL='/6',.T.,.F.)
        * KTSpath='W:\KTC'
        RealTime=IIF(RealTime='/RT',.T.,.F.)
        resolut=resolutWidthDefault 
   CASE PARAM()=4
        BL1=IIF(BL='/1',.T.,.F.)
        BL2=IIF(BL='/2',.T.,.F.)
        BL3=IIF(BL='/3',.T.,.F.)
        BL4=IIF(BL='/4',.T.,.F.)
        BL5=IIF(BL='/5',.T.,.F.)
        BL6=IIF(BL='/6',.T.,.F.)
        RealTime=IIF(RealTime='/RT',.T.,.F.)
        resolut=val(resolut)
   OTHERWISE
        BL1=.F.
        BL3=.F.
        BL2=.F.
        BL4=.F.
        BL5=.F.
        BL6=.F.
        KTSpath='W:\KTC'
        RealTime=.F.
        resolut=resolutWidthDefault 
ENDCASE

DefaultDir=FULLPATH('.')
ProgramDir = SYS(5)+SYS(2003)+"\"

CLOSE ALL
SET TALK OFF
SET HEAD OFF
SET STATUS BAR OFF
SET SYSMENU OFF
ON KEY
SET DATE GERMAN
SET ESCAPE OFF
SET COLOR OF SCHEME 21 TO ,n/w*,,,,n/bg*,,,rg+/n && PERIODTI.SCX
*SET COLOR OF SCHEME 22 TO n/bg,n/bg,,,,w+/r && RS.SCX
*SET COLOR OF SCHEME 23 TO n/w,n/w,n/w,,,w/n,,/r && DIR.SCX
*SET COLOR OF SCHEME 24 TO n/bg,n/bg && CLCUL.SCX
SET CENT ON
SET HOURS TO 24
SET SAFETY OFF
if FILE (ProgramDir+"HELP.HLP")
	SET HELP TO ProgramDir+"HELP.HLP"
endif
ON KEY LABEL F1 HELP
SET PROCEDURE TO CALC_END ADDITIVE
*clear macro
CLEAR

IF RealTime=.T.
	*** inblok ���������� � bat-�����!!! ***
	COPY FILE "W:\KTC\A\TEPW_RT\ftabl.dbf" TO ProgramDir+"ftabl.dbf"
	COPY FILE "W:\KTC\A\TEPW_RT\outblok.dbf" TO ProgramDir+"outblok.dbf"
ELSE
ENDIF

WITH _Screen
     .FontName='Courier New'
     .FontSize=12
     .WindowState=0  && normal
*    .ControlBox=.F.
     .Closable=.F.
     .MaxButton=.F.
     .Height=resolutHeightDefault
     .MinHeight=resolutHeightDefault
     .Width=resolut
     .MinWidth=resolut
     .Caption="������ ���"
ENDWITH

DEFINE POPUPS MAIN FROM 05,21 IN SCREEN FONT "Courier New",11 TITLE '[������� ����]';
COLOR bg/w*,n/w*,gr+/w,gr+/w,,n/gr*
DEFINE BAR IIF(RealTime,1,1) OF MAIN PROMPT '������ ���                         ' SKIP FOR RealTime
DEFINE BAR IIF(RealTime,1,2) OF MAIN PROMPT '������ ��                          ' SKIP FOR .T.
DEFINE BAR IIF(RealTime,1,3) OF MAIN PROMPT '������ ����������� �����������     ' SKIP FOR RealTime
DEFINE BAR IIF(RealTime,1,4) OF MAIN PROMPT '������ ������ 15506-1              ' SKIP FOR RealTime
DEFINE BAR IIF(RealTime,1,5) OF MAIN PROMPT '������ � s-������                  ' SKIP FOR BL1 OR BL2 OR BL3 OR;
BL4 OR BL5 OR BL6 OR RealTime
IF RealTime = .T.
IF RealTimeAllBlockMenuItems = .F.
DEFINE BAR IIF(RealTime,1,6) OF MAIN PROMPT '������ ��� ����� '+IIF(BL1,'�1',IIF(BL2,'�2',IIF(BL3,'�3',IIF(BL4,'�4',IIF(BL5,'�5',IIF(BL6,'�6','')))))) SKIP FOR NOT RealTime
ELSE
DEFINE BAR IIF(RealTime,1,6) OF MAIN PROMPT '������ ��� ����� '+'�1' SKIP FOR NOT RealTime
DEFINE BAR IIF(RealTime,2,7) OF MAIN PROMPT '������ ��� ����� '+'�2' SKIP FOR NOT RealTime
DEFINE BAR IIF(RealTime,3,8) OF MAIN PROMPT '������ ��� ����� '+'�3' SKIP FOR NOT RealTime
DEFINE BAR IIF(RealTime,4,9) OF MAIN PROMPT '������ ��� ����� '+'�4' SKIP FOR NOT RealTime
DEFINE BAR IIF(RealTime,5,10) OF MAIN PROMPT '������ ��� ����� '+'�5' SKIP FOR NOT RealTime
DEFINE BAR IIF(RealTime,6,11) OF MAIN PROMPT '������ ��� ����� '+'�6' SKIP FOR NOT RealTime
ENDIF
ELSE
ENDIF
DEFINE BAR IIF(RealTime,IIF(RealTimeAllBlockMenuItems, 7, 2),6) OF MAIN PROMPT '�����                              '

ON SELECTION POPUPS MAIN DO tmenu WITH PROMPT()

public FormNum   && ���������� ��� table1.scx
PUBLIC TTK_L1,TTK_L2,TTK_L3,TTK_L4,TTK_L_EXCEL, TTK_P1,TTK_P2,TTK_P3  && ���������� ��� tri_tkv.scx

PUBLIC loc_date,;
	iden1, iden2,;
	imes1, imes2,;
	igod1, igod2,;
	ichas1, ichas2,;
	n_blokov, n_blokov1, Tek_N,;
	filtr,minus,;
	Show_b1, Show_b2,Show_b3,Show_b4,Show_b5,Show_b6,Show_opisanie, Show_station,Show_gruppa,Show_pk,Show_pvk,; && ��.������� �� ������ � �� ������ ����� opisanie
	begm,endm,;
	begd,endd	
	
*����� ������ �� ���������
n_blokov1=3

STORE .T. TO Show_opisanie,Show_b1,Show_b2,Show_b3,Show_b4,Show_b5,Show_b6,Show_station,;
Show_gruppa,Show_pk,Show_pvk && ��.������� �� ������ � �� ������ ����� opisanie,;
*blok1-blok6, station, gruppa, pk, pvk ��.� ���.������
STORE .T. TO Show_oppvk,Show_pvk1,Show_pvk2,Show_pvk3,Show_pvk4,Show_pvk5,Show_pvk6,Show_pvk7,Show_vsepvk && ��.������� �� ������ 
*� �� ������ ����� opisanie,pvk1-pvk7, vsepvk ��.� ���.������

public mes(12) && ������������ � PERIODTI.SCX
dimension inblok(11)
FOR i=1 TO 12
    mes(i)=SUBSTR("������  ������� ����    ������  ���     ����    "+;
           "����    ������  ��������������� ������  ������� ",(i-1)*8+1,8)
ENDFOR

USE cor_in
PUBLIC cor_in_arr(reccount(),2)
COPY TO ARRAY cor_in_arr

***sut3tec.DBF***
IF FILE('sut3tec.DBF')
	IF FILE('sut3tec.CDX')
	   USE sut3tec INDEX sut3tec.CDX
	   REINDEX
	ELSE
		USE sut3tec
		INDEX ON DATA TAG DATA of sut3tec.dbf
	ENDIF
ELSE
ENDIF

***FTABL.DBF***
IF FILE('FTABL.DBF')
	IF FILE('FTABL.CDX')
	   *elete FILE ftabl.CDX
	   USE ftabl INDEX ftabl.CDX
	   REINDEX
	ELSE
		USE ftabl
		INDEX ON id TAG id
	ENDIF
ELSE
ENDIF

***S_BLOK.DBF***
IF FILE('S_BLOK.DBF')
   USE s_blok
   IF NOT FILE('S_BLOK.CDX')
      INDEX ON DATA UNIQUE FOR S_BLOK.PERIOD="DAY" TAG DATA
      INDEX ON PERIOD UNIQUE FOR S_BLOK.PERIOD#"DAY" TAG PERIOD
   ELSE
      REINDEX
   ENDIF
ENDIF

***S_OUTBL.DBF***
IF FILE('S_OUTBL.DBF')
   USE s_outbl
   IF NOT FILE('S_OUTBL.CDX')
      INDEX ON month+STR(year,4) UNIQUE TAG month
      INDEX ON YEAR UNIQUE TAG YEAR
   ELSE
      REINDEX
   ENDIF
ENDIF
USE

DO scr WITH "","bg+/n"
SET CURSOR OFF
@ 17,27 SAY "              ���ר�       " FONT "Courier New",12
@ 19,27 SAY "������� - ������������� �����������" FONT "Courier New",12
@ 21,27 SAY "        ������������� ���-5" FONT "Courier New",12
WAIT '' TIMEOUT 2
CLEAR
SET CURSOR ON
IF NOT RealTime
   @ 16,37 SAY "         ������� ����:" FONT "Courier New",12
   @ 19,27 SAY "������ ��������������� �������" FONT "Courier New",12
   @ 20,27 SAY "����� ��������������� �������" FONT "Courier New",12
   i=78
   DO WHILE i=78 OR i=110
      @ 19,61 GET n_k_p DEFAULT {  .  .  } FONT "Courier New",12
      @ 20,61 GET k_k_p DEFAULT {  .  .  } FONT "Courier New",12
      READ
      @ 23,27 SAY "�� �������������, (Y/N) ?" FONT "Courier New",12
      WAIT ''
      @ 23,00 SAY SPACE(80) FONT "Courier New",12
      i=lastkey()
   ENDDO
ENDIF
do while .T.
  ON KEY LABEL F1 HELP ID 1
  dn=DAY(DATE())
  ms=MONTH(DATE())
  yr=2
  DO scr WITH "����� �������� � ��������� - �� [F1]","w+/w"
  IF RealTime = .T.
  	ACTIVATE POPUPS MAIN BAR IIF (BL1, 1, IIF (BL2, 2, IIF (BL3, 3, IIF (BL4, 4, IIF (BL5, 5, IIF (BL6, 6, 1))))))
  ELSE
  	ACTIVATE POPUPS MAIN
  ENDIF
enddo
*****************************************************************************
*                          Procedure   tmenu                                *
*****************************************************************************
PROCEDURE tmenu
PARAMETERS MPROMPT
DO CASE
   CASE MPROMPT="������ ���"
   		Initialize ()
      DO pvk
   CASE MPROMPT="������ ��"
   CASE MPROMPT="������ ����������� �����������"
   		Initialize ()
      DO prg3
   CASE MPROMPT="������ ������ 15506-1"
   		Initialize ()
      DO prg4
   CASE MPROMPT="������ � s-������"
   		Initialize ()
      DO sel_type
	CASE MPROMPT="������ ��� ����� �1"
		BL1 = .T.
		STORE .F. TO BL2, BL3, BL4, BL5, BL6
		Initialize ()
		DO REALTIME
	CASE MPROMPT="������ ��� ����� �2"
		BL2 = .T.
		STORE .F. TO BL1, BL3, BL4, BL5, BL6
		Initialize ()
		DO REALTIME
	CASE MPROMPT="������ ��� ����� �3"
		BL3 = .T.
		STORE .F. TO BL1, BL2, BL4, BL5, BL6
		Initialize ()
		DO REALTIME
	CASE MPROMPT="������ ��� ����� �4"
		BL4 = .T.
		STORE .F. TO BL1, BL2, BL3, BL5, BL6
		Initialize()	
		DO REALTIME
	CASE MPROMPT="������ ��� ����� �5"
		BL5 = .T.
		STORE .F. TO BL1, BL2, BL3, BL4, BL6
		Initialize ()
		DO REALTIME
	CASE MPROMPT="������ ��� ����� �6"
		BL6 = .T.
		STORE .F. TO BL1, BL2, BL3, BL4, BL5
		Initialize ()
		DO REALTIME
   CASE MPROMPT="������ ��� �����"
   		Initialize ()
      DO REALTIME
   CASE MPROMPT="�����"
      WITH _Screen
           .WindowState=2  && maximized
           .Caption="Microsoft Visual FoxPro"
           .ControlBox=.T.
           .Closable=.T.
           .MaxButton=.T.
           .MinButton=.T.
           .MousePointer=0
           .MinHeight=-1
           .MinWidth=-1
      ENDWITH
      close all
      clear
      clear memory
      clear windows
*     SET HELP TO
*     SET HELP ON
      on key
      SET STATUS BAR ON
      SET SYSMENU ON
      CANCEL
ENDCASE
@ 0,0 SAY "����� �������� � ��������� - �� [F1]"

RETURN

*****************************************************************************
*                          Procedure   init                                *
*****************************************************************************
PROCEDURE initialize

STORE .F. TO TTK_L1,TTK_L2,TTK_L3,TTK_L4, TTK_L_EXCEL
STORE .F. TO TTK_P1,TTK_P2,TTK_P3

***��� ��� � ����� ��������*** �����-��(�����������) ���.���� (�� 00:15 ���.���� ��� ����� ���������)
loc_date=IIF(VAL(SUBSTR(TIME(),4,2))<15 AND VAL(SUBSTR(TIME(),1,2))=0,date()-1,date())
STORE 10 TO igod2
STORE MONTH(loc_date) TO imes2 && ������.�����
STORE DAY(loc_date) TO iden2   && ������.����
STORE VAL(SUBSTR(TIME(),1,2))+1 TO ichas2  && ������.���. +1, �.�. array chas � ����� periodtime ������������� �� 0 �� 23 (������ �� 1 �� 24)
ichas2=IIF(VAL(SUBSTR(TIME(),4,2))<15,IIF(ichas2=1,24,ichas2-1),ichas2) && � ����.15 ���.������� ���� ���.��� ��� �� ������
ichas1=IIF(ichas2=1,24,ichas2-1) && ���.��� ������ ��������� �� 1
*ichas2=IIF(ichas2=1,24,ichas2-1) && ����� ����� �� -1 ***
*ichas1=IIF(ichas1=1,24,ichas1-1) && ����� ����� �� -1 ***
iden1=DAY(IIF(ichas1>ichas2,loc_date-1,loc_date)) && ���.����
imes1=MONTH(IIF(iden1>iden2,loc_date-1,loc_date)) && ���.�����
igod1=IIF(imes1>imes2,igod2-1,igod2)              && ���.���
RELEASE loc_date
***��� ��� � ����� ��������***
STORE date() TO begd,endd && ��� s_base.prg
STORE TRANSF(MONTH(date()),"@L 99") TO begm,endm && ��� s_base.prg

minus=.F.  && ���-�� � FUNC entr(TEP.PRG), FUNC exit(TEP.PRG)
filtr='off'          && ������� ���������� �� ��.'f' ��� '*' ����������� (� TABLE1.SCX)
*n_blokov1=3          && ����� ���������� ������ (3 - �� ������)
Tek_N=''             && ������� ����� � inblok.dbf (��� proc goto � tep.prg)

DO CASE
   CASE BL1
        STORE .F. TO Show_b2,Show_b3,Show_b4,Show_b5,Show_b6
   CASE BL2
        STORE .F. TO Show_b1,Show_b3,Show_b4,Show_b5,Show_b6
   CASE BL3
        STORE .F. TO Show_b1,Show_b2,Show_b4,Show_b5,Show_b6
   CASE BL4
        STORE .F. TO Show_b1,Show_b2,Show_b3,Show_b5,Show_b6
   CASE BL5
        STORE .F. TO Show_b1,Show_b2,Show_b3,Show_b4,Show_b6
   CASE BL6
        STORE .F. TO Show_b1,Show_b2,Show_b3,Show_b4,Show_b5
ENDCASE

*****************************************************************************
PROCEDURE scr
PARAMETERS text,clr
ACTIV SCREEN
SET COLOR TO (clr)
clear
@ 0,0 SAY SPACE(80) FONT "Courier New",12
@ 0,0 SAY text FONT "Courier New",12
*****************************************************************************
FUNCTION entr  && ����� �� TABLE1.SCX
PRIVATE x
x=varread()
IF "    -     "$&x
   minus=.T.
ENDIF
*****************************************************************************
FUNCTION exit  && ����� �� TABLE1.SCX
PARAM table
IF PARAM()=0
   PRIVATE table
   table="table1"
ENDIF
PRIVATE x,form,i
form='FORM'+FormNum
i=&table..&form..grid1.ActiveColumn
x=&table..&form..grid1.columns(i).controlsource
IF minus=.T.
   minus=.F.
   IF NOT LEFT(&x,1)='s'
   REPLACE &x WITH "    -     "
   ENDIF
ENDIF
*************************************************************
PROCEDURE S && �������/�� ������� ���� ����� �� PRG3.PRG,PRG4.PRG
PRIV i,x,y
x=LOWER(varread())
i=recno()
IF SUBSTR(x,1,4)='blok'
y=SUBSTR(&x,1,1)
REPLACE ALL &x WITH IIF(y='s',STRTRAN(&x,'s',''),'s'+RIGHT(RTRIM(&x),9))
   go i
   KEYBOARD "{TAB}{BACKTAB}"
ENDIF
*************************************************************
PROCEDURE SEEK_Y      && �� CALC.PRG � CALC_M.PRG(����� �������)
HIDE POPUP ALL
IF NOT FOUND()
   DEFINE WINDOW FX FROM 09,10 TO 14,59 '*' color w+/rb FONT "Courier New",10
   ACTIV WINDOW FX
   @ 01,8 SAY "����������� ������ "+y
   @ 02,8 SAY "������� �� ����� �������..."
   WAIT ''
   RELEASE WIND FX
ENDIF
*************************************************************
PROC MAXMIN      && �� PRG3.PRG � PRG4.PRG(��������� � �������� ��.���.)
HIDE POPUP ALL
SELECT 1
USE inblok.dbf
PUSH KEY CLEAR
ON KEY LABEL F1 HELP ID 9
ON KEY LABEL Home GO TOP
ON KEY LABEL End GO BOTTOM
ON KEY LABEL F6 DO goto     && ������� �� ����.�����.
ON KEY LABEL F7 REPL sum_norm WITH IIF(sum_norm,.F.,.T.) && �������.���� �����.sum_norm �� �������������.
DEFINE WINDOW w33 FROM 2,0 TO 27,85 ;
TITLE '������ � ������� ���������� ������� ���������� ������� �������';
color scheme 10
DO scr WITH "����� �������� � ��������� - �� [F1]","gr+/rb"
ACTIV WIND w33
brow fields order:H="�/�":W=.F.,mark:H="�����������":W=.F.,;
            mera:H="�������":W=.F.,min_z:H="�������",max_z:H="��������",;
            sn=IIF(sum_norm,"������-��","������-��"):10:H="��������":W=.F.;
            when soob() FONT "Courier New",8 IN WINDOW w33 nodelete noappend
RELEASE WIND w33
POP KEY
DO scr WITH "����� �������� � ��������� - �� [F1]","w/n"
CLOSE DATA
*************************************************************
PROC GOTO      && �� PROC MAXMIN � TEP.PRG (������� �������)
PUSH KEY CLEAR
DEFINE WINDOW goto FROM 10,25 TO 11,70 none color gr+/g,w+/n FONT "Courier New",10
Tek_N=IIF(ALLTRIM(Tek_N)=='',ALLTRIM(SUBSTR(order,2)),Tek_N)
ACTIV WINDOW goto
@ 0,4 say "������� � ���������" get Tek_N PICT '@K' size 1,5
READ
LOCATE FOR ALLTRIM(SUBSTR(order,2))==ALLTRIM(Tek_N)
RELEASE WINDOW goto
POP KEY
*****************************************************************************
FUNCTION soob   && ����� �� PROC MAXMIN (TEP.PRG) � TABLE.PRG
ACTIV SCREEN
@ 27,0 SAY opisanie FONT "Courier New",10
ACTIV WIND w33
*************************************************************************
FUNCTION RECOD      && ������������� �� 866 � 1251 
PARAMETERS char
PRIVATE tab1,tab2
tab1 = '��㪥�������뢠�஫�����ᬨ���'+'���������������������������������'
tab2 = '���������������������������������'+'���������������������������������'
RETURN CHRTRAN(char,tab1,tab2)
*****************************************************************************
FUNCTION exist    && ����� �� PROC zamena (GET &x) � ����� RS
LOCATE FOR data=&x AND recno()#ix  && �������.�� �����.������ ����
IF FOUND()
   @ 01,00 SAY "����� ���� ��� ����. ������� ����� �������" COLOR rg+/b &&ONT "Courier New",8
   wait ''
   @ 01,00 SAY "                                          " COLOR /bg &&NT "Courier New",8
   RETURN 0
ENDIF
