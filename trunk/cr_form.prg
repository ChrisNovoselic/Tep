* CR_FORM  && ��������� �������� ���� ������
PROC CR_FORM
PARAMETER NumForm,ITypeForm,OTypeForm
IF ITypeForm='IN3_TEX'
   IF MESSAGEBOX('��������� 3tec.DBF �� �����?',4+32+256,'')=6
      DO FORM tri_tec
   ENDIF
ENDIF
PRIVATE i,in,out,instri,outstr,pstr,num_poz,xdb,ydb,field,yesno
yesno=.F.
SELECT 3
USE outblok.dbf
SELECT 2
USE inblok.dbf
SELECT 1
USE outmkt.dbf
IF ITypeForm='IN15506'
   HIDE POPUP ALL
   IF MESSAGEBOX('��������� ast_'+right(DTOC(Date()), 2)+'.DBF ?'+CHR(13)+CHR(13)+;
                 '���� ��, �� ���������� ���� ast_'+right(DTOC(Date()), 2)+'.DBF �� �������� 3���-01!',4+32+256,'')=6
      yesno=.T.
   ENDIF
   * m.kgo � m.stn - ������� � ast_'+right(DTOC(Date()), 2)+'.DBF
   PRIV m.kgo,m.stn,m.mes1,r,rr,value,NewLine,found
   m.mes1=IIF(ms=1,12,ms-1)
   r=0 && ������� ����� ���� ast_'+right(DTOC(Date()), 2)+'.R1,ast_'+right(DTOC(Date()), 2)+'.R2,...ast_'+right(DTOC(Date()), 2)+'.R9,ast_'+right(DTOC(Date()), 2)+'.R10,...
   USE 'ast_'+right(DTOC(Date()), 2)IN 5
   @ 4,0 GET m.mes1 PICTURE "@&T" FROM mes SIZE 14,12 FONT "Courier New",10
   READ CYCLE
ENDIF
clear
FOR i=1 TO NumForm
 DO CR_1 WITH i
ENDFOR
IF ITypeForm='IN15506'
   USE IN 5
ENDIF
USE IN 3
USE IN 2
USE IN 1
*****************************************************************
PROC CR_1
PARAMETER file
PRIVATE file1,file2
file1='FORMA\'+ITypeForm+'.'+STR(file,1) && ��.����
file2='FORMA\'+OTypeForm+'.'+STR(file,1) && ���.����
in = FOPEN(file1,0)
out = FCREATE(file2,0)
DO WHILE NOT FEOF(in)
 instri=FGETS(in)   &&���������� ��.������
 outstr=instri
 IF ITypeForm='IN15506'
    NewLine=.T.
 ENDIF
 DO WHILE .T.
  pstr=AT('^',outstr) && ������� ������ �� outmkt.dbf
  IF pstr=0
   EXIT
  ENDIF
  IF ITypeForm='IN15506' AND NewLine
     NewLine=.F.
     rr=r
     * ����������� ����� � ast_'+right(DTOC(Date()), 2)+'.DBF , ���� ����������� ������
     m.kgo=CHRTRAN(ALLTRIM(SUBSTR(outstr,AT(':',outstr)+1,;
     AT(':',outstr,2)-AT(':',outstr)-1)),CHR(9),'')  && CHR(9) - ���� ���������
     m.stn=VAL(CHRTRAN(ALLTRIM(SUBSTR(outstr,AT(':',outstr,3)+1,;
     AT(':',outstr,4)-AT(':',outstr,3)-1)),CHR(9),''))
     SELECT 5
     LOCATE FOR mes=TRANSF(m.mes1,'@L 99') AND kgo=m.kgo AND stn=m.stn;
                AND kpo='357313'
     found=IIF(FOUND(),.T.,.F.)
     SELECT 1
  ENDIF
  IF ITypeForm='IN15506'
     rr=rr+1
  ENDIF
  num_poz=IIF(AT(':',SUBSTR(outstr,pstr))=0,;
  AT('�',SUBSTR(outstr,pstr)),AT(':',SUBSTR(outstr,pstr)))-1 && ���-�� ���.
  xdb=VAL(SUBSTR(outstr,pstr+1,2)) && ����������� � ���� � outmkt.dbf, ������ ������� ������
  ydb=SUBSTR(outstr,pstr+3,5)
  DO CASE
     CASE BETWEEN(VAL(ydb),1,500)   && ������ �� outmkt.dbf
          SELECT 1
     CASE BETWEEN(VAL(ydb),801,999) && ������ �� inblok.dbf
          ydb=CHRTRAN(SUBSTR(ydb,1,1),'89','01')+SUBSTR(ydb,2)
          SELECT 2
     CASE BETWEEN(VAL(ydb),501,800) && ������ �� outblok.dbf
          ydb=CHRTRAN(SUBSTR(ydb,1,1),'5678','0123')+SUBSTR(ydb,2)
          SELECT 3
  ENDCASE
  LOCATE FOR VAL(SUBSTR(order,2))==VAL(ydb) && ����������� � ������ � outmkt.dbf,
* ������ ������� ������
  field=field(xdb)
  IF '    -     '$&field OR 's'$&field
   outstr=SUBSTR(outstr,1,pstr-1)+PADL('   -   ',num_poz,' ')+SUBSTR(outstr,pstr+num_poz)
  ELSE
   outstr=SUBSTR(outstr,1,pstr-1)+' '+;
   PADR((ALLTRIM(STRTRAN(&field,'=',''))),num_poz-1,' ')+;
   SUBSTR(outstr,pstr+num_poz)
  ENDIF
  IF ITypeForm='IN15506' AND found AND yesno
     IF '    -     '$&field OR 's'$&field
     ELSE
        value=VAL(STRTRAN(&field,'=',''))
        SELECT 5
        ON ERROR DO ERROR
        REPLACE ('ast_'+right(DTOC(Date()), 2)+'.R'+ALLTRIM(STR(rr))) WITH value
        ON ERROR
        SELECT 1
     ENDIF
  ENDIF
 ENDDO
= FPUTS(out,outstr)
ENDDO
= FCLOSE(in)
= FCLOSE(out)
IF ITypeForm='IN15506'
   r=rr
ENDIF
*****************************************************************
PROC ERROR
MESSAGEBOX('������� ������ � ���� ast_'+right(DTOC(Date()), 2)+'.DBF � ���� R'+ALLTRIM(STR(rr))+' ��������'+CHR(13)+;
           '��������� '+ALLTRIM(ydb)+'='+ALLTRIM(STR(value))+'(�� ������), ����������� ��������'+CHR(13)+;
           '�� ��������������� (��������� ���������� � ��������� ast_'+right(DTOC(Date()), 2)+'.DBF)',48)

