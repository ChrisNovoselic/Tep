PUSH KEY CLEAR
ON KEY LABEL F1 HELP ID 7
DEFINE POPUPS S_BASE FROM 06,21 TITLE'[������ � S-������]' COLOR ;
       gr+/w,n/w,b/w,gr+/w,,w+/r* FONT "Courier New",10
DEFINE BAR 01 OF s_base PROMPT '������� s-����                       ' SKIP FOR .T. &&FILE('s_blok.dbf')
DEFINE BAR 02 OF s_base PROMPT '���������� s-����                    ' SKIP FOR .T. &&.NOT.FILE('s_blok.dbf')
DEFINE BAR 03 OF s_base PROMPT '����������� s-���� � ���������� SBASE' SKIP FOR .T. &&.NOT.FILE('s_blok.dbf')
DEFINE BAR 04 OF s_base PROMPT '��������� s-���� �� �����            ' SKIP FOR .NOT.FILE('s_blok.dbf').OR..NOT.FILE('inblok.dbf')
DEFINE BAR 05 OF s_base PROMPT '��������� s-���� �� �����            ' SKIP FOR .NOT.FILE('s_blok.dbf').OR..NOT.FILE('inblok.dbf')
DEFINE BAR 06 OF s_base PROMPT '�������� s-����                      ' SKIP FOR .NOT.FILE('s_blok.dbf')
DEFINE BAR 07 OF s_base PROMPT '������ ������� ����                  ' SKIP FOR .NOT.FILE('s_blok.dbf')
DEFINE BAR 08 OF s_base PROMPT '������� ����                         '
ON SELECTION POPUPS s_base DO tsb WITH PROMPT()
DO scr WITH "����� �������� � ��������� - �� [F1]","w+/w"
*IDE POPUPS main
HIDE POPUPS ALL
ACTIVATE POPUPS s_base
*****************************************************************************
*                          Procedure   tsb
*****************************************************************************
PROCEDURE tsb
PARAMETERS MPROMPT
DO CASE
   CASE MPROMPT="������� s-����"
      _Screen.MousePointer=11
      COPY FILE SBASE\s_blok.dbf TO s_blok.dbf
      _Screen.MousePointer=0
      KEYBOARD "{DNARROW}"
   CASE MPROMPT="���������� s-����"
      HIDE POPUP ALL
      DEFINE WINDOW pri FROM 0,0 TO 24,79 NONE color n/w,w+/r FONT "Courier New",10
      ACTIV WINDOW pri
      yn='A'
      DO WHILE .NOT.(UPPER(yn)='Y' OR UPPER(yn)='N')
        yn='N'
        @ 10,15 SAY "�� �������, ��� ��� ��� �����,(Y/N) " GET yn
        READ
      ENDDO
      RELEASE WIND pri
      IF UPPER(yn)='Y'
         DELETE FILE s_blok.dbf
         KEYBOARD "{UPARROW}"
      ENDIF
   CASE MPROMPT="����������� s-���� � ���������� SBASE"
      _Screen.MousePointer=11
      COPY FILE s_blok.dbf TO SBASE\s_blok.dbf
      _Screen.MousePointer=0
   CASE MPROMPT="��������� s-���� �� �����"
      HIDE POPUP ALL
      DEFINE WINDOW pri FROM 0,0 TO 24,79 NONE color n/w,w+/r FONT "Courier New",10
      DEFINE WINDOW a_r_e FROM 10,07 TO 14,72 color w+/r FONT "Courier New",10
      USE s_blok
      set order to data
      ACTIV WINDOW pri
      yn=2
      dt=date()
      DO WHILE yn=2
        @ 04,05 SAY "������� ���� ��� ���������� s-���" GET dt;
        valid BETWEEN(dt,{^1980-01-01},{^2100-01-01}) error '�������� ����'
        READ
        exdt=.F.       && ������� �������-�� ��������� ����
        IF SEEK(dt)
           exdt=.T.
           @ 17,05 SAY "����� ���� ��� ���� ��������" color w+/n
        ENDIF
        ACTIV WINDOW a_r_e
        @ 2,02 GET yn PICTURE "@*IHT ;;" SIZE 1,12,2 COLOR w+/r,,,,,w+/b,,,w+/r
        @ 2,04 SAY "��������" SIZE 1,8, 0
        @ 2,17 SAY "���������" SIZE 1,9, 0
        @ 2,32 SAY "���������" SIZE 1,9, 0
        READ CYCLE
        DEACTIV WINDOW a_r_e
        CLEAR
      ENDDO
      RELEASE WIND pri,a_r_e
      IF yn=3
         _Screen.MousePointer=11
         IF exdt        && �������.��������
            SET ORDER TO
            DELETE FOR data=dt
            pack
         ELSE
            CALCULATE CNT(),MIN(data) TO cnt,mindata && ��������� ��� � ���.����
            SET ORDER TO
            IF cnt=40      && ������ ���������� �������� ���
               DELETE FOR data=mindata
               pack
            ENDIF
         ENDIF
         APPEND FROM inblok FIELDS order,blok1,blok2,blok3,blok4,blok5,;
         blok6,station
         REPLACE data WITH dt,period with 'DAY' FOR data={  .  .  }
         _Screen.MousePointer=0
      ENDIF
      USE
   CASE MPROMPT="��������� s-���� �� �����"
      HIDE POPUP ALL
      dimension mes(12)
      DEFINE WINDOW pri FROM 0,0 TO 24,79 NONE color n/w,w+/r FONT "Courier New",10
      DEFINE WINDOW a_r_e FROM 10,07 TO 14,72 color w+/r FONT "Courier New",10
      FOR i=1 TO 12
      mes(i)=SUBSTR("01������  02������� 03����    04������  05���     06����    "+;
           "07����    08������  09��������10������� 11������  12������� ",(i-1)*10+1,10)
      ENDFOR
      USE s_blok
      set order to period
      ACTIV WINDOW pri
      yn=2
      dt=date()
      ms=MONTH(date())
      DO WHILE yn=2
        @ 05,05 SAY "������� ����� ������ ��� ���������� "
        @ 04,05 SAY "������� ������� ���� " GET dt;
        valid BETWEEN(dt,{^1980-01-01},{^2100-01-01}) error '�������� ����'
        @ 05,41 GET ms SIZE 1,2 valid BETWEEN(ms,1,12) error '�������� � ���.'
        READ
        ACTIV WINDOW a_r_e
        @ 2,02 GET yn PICTURE "@*IHT ;;" SIZE 1,12,2 COLOR w+/r,,,,,w+/b,,,w+/r
        @ 2,04 SAY "��������" SIZE 1,8, 0
        @ 2,17 SAY "���������" SIZE 1,9, 0
        @ 2,32 SAY "���������" SIZE 1,9, 0
        READ CYCLE
        DEACTIV WINDOW a_r_e
        CLEAR
      ENDDO
      RELEASE WIND pri,a_r_e
      IF yn=3
         _Screen.MousePointer=11
         IF SEEK(mes(ms)) && �������.��������
            SET ORDER TO
            DELETE FOR period=mes(ms)
            pack
         ELSE
            SET ORDER TO
         ENDIF
         APPEND FROM inblok FIELDS order,blok1,blok2,blok3,blok4,blok5,;
         blok6,station
         REPLACE data WITH dt,period with mes(ms) FOR data={  .  .  }
         _Screen.MousePointer=0
      ENDIF
      REINDEX
      USE
   CASE MPROMPT="�������� s-����"
      HIDE POPUP ALL
      DO FORM RS
   CASE MPROMPT="������ ������� ����"
      PRIV i,i1,i2,j,yn
      SET EXACT ON
      HIDE POPUP ALL
      DEFINE WINDOW pri FROM 0,0 TO 24,79 NONE color n/w,w+/r FONT "Courier New",10
      DEFINE WINDOW a_r_e FROM 10,14 TO 14,55 color w+/r FONT "Courier New",10
      DEFINE WINDOW a_r_e1 FROM 10,08 TO 14,61 color w+/r FONT "Courier New",10
      USE inblok && sum_norm - ������ ������� �������������-������������������
      COPY TO ARRAY sum_norm FIELDS order,sum_norm
      *iarray - ������ �����.�������-���, rez,rez1 - ��������������
      DIMENSION iarray(alen(sum_norm,1),8),rez(alen(sum_norm,1),8),rez1(7)
      STORE 0 TO iarray
      STORE '' TO rez
      FOR i=1 TO ALEN(sum_norm,1)
          sum_norm(i,1)=SUBS(sum_norm(i,1),2)
          iarray(i,1)=sum_norm(i,1)
          rez(i,1)=sum_norm(i,1)
      ENDFOR
      ACTIV WINDOW pri
      ACTIV WINDOW a_r_e
      yn=1
      @ 0,12 SAY "������"
      @ 2,02 GET yn PICTURE "@*IHT ;" SIZE 1,12,2 COLOR w+/r,,,,,w+/b,,,w+/r
      @ 2,03 SAY "� �������     � ��������"
      READ CYCLE
      DEACT WINDOW a_r_e
      DO CASE
         CASE yn=1 && ������ � �������
           USE s_blok
           set order to data && DATA UNIQUE
           yn=1
           DO WHILE yn=1
              @ 04,05 SAY "��������� ����" GET begd
              @ 05,05 SAY "��������  ����" GET endd
              READ
              IF SEEK(begd) AND SEEK(endd) AND begd<=endd && ����� ���������
                ACTIV WINDOW a_r_e1
                @ 2,02 GET yn PICTURE "@*IHT ;;" SIZE 1,11,2;
                COLOR w+/r,,,,,w+/b,,,w+/r
                @ 2,03 SAY "���������    ���������    ��������"
                READ CYCLE
                DEACT WIND a_r_e1
                IF yn=1
                  EXIT
                ENDIF
                yn=yn-1
              ELSE
                ACTIV WINDOW a_r_e
                @ 0,03 SAY "������� �������� ����"
                @ 2,02 GET yn PICTURE "@*IHT ;" SIZE 1,12,2;
                COLOR w+/r,,,,,w+/b,,,w+/r
                @ 2,03 SAY "���������     ��������"
                READ CYCLE
                DEACT WIND a_r_e
              ENDIF
           ENDDO
           IF yn=1
              COPY TO ARRAY ar_data FIELDS data FOR BETW(data,begd,endd) && �������� ������� ���
              filter='period="DAY" AND data=ar_data(i,1)'
           ENDIF
         CASE yn=2 && ������ � ��������
           USE s_blok
           set order to period  && PERIOD UNIQUE
           yn=1
           DO WHILE yn=1
              @ 04,05 SAY "��������� ����� (�����)" GET begm SIZE 1,2
              @ 05,05 SAY "��������  ����� (�����)" GET endm SIZE 1,2
              READ
              begm=RIGHT('0'+ALLTRIM(begm),2)
              endm=RIGHT('0'+ALLTRIM(endm),2)
*wait wind 'begm='+begm+ALLTRIM(mes(VAL(begm)))+IIF(SEEK(begm+ALLTRIM(mes(VAL(begm)))),'+','-')
              IF SEEK(begm+ALLTRIM(mes(VAL(begm)))) AND SEEK(endm+ALLTRIM(mes(VAL(endm))));
              AND begm<=endm && ����� ���������
                ACTIV WINDOW a_r_e1
                @ 2,02 GET yn PICTURE "@*IHT ;;" SIZE 1,11,2 COLOR w+/r,,,,,w+/b,,,w+/r
                @ 2,03 SAY "���������    ���������    ��������"
                READ CYCLE
                DEACT WIND a_r_e1
                IF yn=1
                  EXIT
                ENDIF
                yn=yn-1
              ELSE
                ACTIV WINDOW a_r_e
                @ 0,03 SAY "������� �������� ����"
                @ 2,02 GET yn PICTURE "@*IHT ;" SIZE 1,12,2;
                COLOR w+/r,,,,,w+/b,,,w+/r
                @ 2,03 SAY "���������     ��������"
                READ CYCLE
                DEACT WIND a_r_e
              ENDIF
           ENDDO
           IF yn=1
              COPY TO ARRA ar_data FIEL period ;
              FOR BETW(period,begm+ALLTRIM(mes(VAL(begm))),endm+ALLTRIM(mes(VAL(endm)))) && ������.������� ���-�
              filter='period=ar_data(i,1)'
           ENDIF
      ENDCASE
      RELEASE WIND pri,a_r_e,a_r_e1
      _Screen.MousePointer=11
      SET ORDER TO
      FOR i=1 to IIF(type('ar_data')='U',0,ALEN(ar_data,1)) && � ��������� ��������� �������
        SET FILTER TO &filter
        COPY TO ARRAY add FIELDS order,blok1,blok2,blok3,blok4,blok5,blok6,station
        FOR i1=1 TO ALEN(add,1)
            STORE SUBS(add(i1,1),2) TO add(i1,1)
        ENDFOR
        FOR i1=1 TO alen(add,1)
            IF ASCAN(sum_norm,add(i1,1))>0 && � rez ������� ����������� add(i1,1)
               j=ASUBSCRIPT(sum_norm,ASCAN(sum_norm,add(i1,1)),1)
               FOR i2=2 TO 8
                   IF ALEN(ar_data,1)=1 && ���-�� ��.������=1 => �� ���� �� PROC ADD
                      rez(j,i2)=add(i1,i2)
                   ELSE 
                      STORE STRTRAN(add(i1,i2),'s    -    ','    -     ') TO add(i1,i2)
                      IF 's'$add(i1,i2) OR (add(i1,i2)='    -    ' AND i>1) OR ;
                      (ALLTR(SUBS(add(i1,1),2))=='74' AND i>1) && �� ���� �� PROC ADD
                      ELSE
                         iarray(j,i2)=iarray(j,i2)+1 && ���� �� PROC ADD �
                         DO add WITH iarray(j,i2)    && �������� - ������� �����������
                      ENDIF
                   ENDIF
               ENDFOR
            ENDIF
        ENDFOR
      ENDFOR
      _Screen.MousePointer=0
      SET FILTER TO
      IF NOT type('ar_data')='U'
      USE inblok
      FOR i1=1 TO alen(rez,1)
      FOR i2=2 TO alen(rez,2)
          rez1(i2-1)=rez(i1,i2)
      ENDFOR
         LOCATE FOR SUBSTR(order,2)==rez(i1,1)
         GATHER FROM rez1 FIELDS blok1,blok2,blok3,blok4,blok5,blok6,station
      ENDFOR
      ENDIF
      USE
      SET EXACT OFF
   CASE MPROMPT="������� ����"
      DO scr WITH "����� �������� � ��������� - �� [F1]","w+/w"
      POP KEY
      RETURN TO MASTER
ENDCASE
*****************************************************************************
PROCEDURE add   && ����� �� PROC tsb,MPROMPT="������ ������� ����"
PARAMETER i
PRIVATE n,x,x1,x2
IF i=1
   rez(j,i2)=add(i1,i2)
   RETURN
ENDIF
n=MAX(IIF(AT('.',rez(j,i2))=0,0,LEN(TRIM(rez(j,i2)))-AT('.',rez(j,i2))),;
IIF(AT('.',add(i1,i2))=0,0,LEN(TRIM(add(i1,i2)))-AT('.',add(i1,i2))))
x1=VAL(rez(j,i2))
x2=VAL(add(i1,i2))
IF sum_norm(j,2)  && �������� - �����������
  x=x1+x2
ELSE              && �������� - ������������������
  x=(x1*(i-1)+x2)/i
ENDIF
rez(j,i2)=LTRIM(STR(x,20,n))