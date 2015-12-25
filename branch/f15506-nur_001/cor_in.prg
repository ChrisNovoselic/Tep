PRIV x,i0,j,k,i1
LOCAL ARRAY NumbHours(n_blokov+1)
i1=0
select inblok
FOR k=1 TO n_blokov+1
    NumbHours(k)=inm(getIndexOfIInM("1"),k)
ENDFOR
FOR i0=1 TO ALEN(cor_in_arr,1) && массив cor_in_arr из БД cor_in (в TEP.PRG)
    j=getIndexOfIInM(TRIM(cor_in_arr(i0,1)))
    FOR k=1 TO n_blokov+1
        IF cor_in_arr(i0,2)='b' AND k<n_blokov+1 OR cor_in_arr(i0,2)='s' AND k=n_blokov+1
        IF (ai(k)#7 AND inm(getIndexOfIInM("74"),k)=='1' AND ;
           (cor_in_arr(i0,1)='48    ' OR cor_in_arr(i0,1)='49    ')) OR ;
           (ai(k)=1 AND ;
           (cor_in_arr(i0,1)='17    ' OR cor_in_arr(i0,1)='17.1  ')) OR ;
           (BETW(ai(k),2,4) AND ;
           (cor_in_arr(i0,1)='15    ' OR cor_in_arr(i0,1)='15.1  ' OR ;
            cor_in_arr(i0,1)='18    ' OR cor_in_arr(i0,1)='18.1  ' OR ;
            cor_in_arr(i0,1)='22    ' OR cor_in_arr(i0,1)='22.1  ' OR ;
            cor_in_arr(i0,1)='41.2  ')) OR ;
           (ai(k)=5 AND ;
           (cor_in_arr(i0,1)='15.2  ' OR cor_in_arr(i0,1)='17    ' OR ;
            cor_in_arr(i0,1)='21    ' OR cor_in_arr(i0,1)='21.1  ' OR ;
            cor_in_arr(i0,1)='17.1  ' OR cor_in_arr(i0,1)='41.2  '))
        LOOP
        ENDIF
        x=inm(j,k)
        IF TYPE('x')='C' OR x<minmax(j,1)*IIF(minmax(j,3) AND k<n_blokov+1,NumbHours(k),1) OR ;
        x>minmax(j,2)*IIF(minmax(j,3) AND k<n_blokov+1,NumbHours(k),1)
           i1=i1+1
           DIMENSION cor_in1(i1,7)
           cor_in1(i1,1)=cor_in_arr(i0,1)
           LOCATE FOR ALLTRIM(SUBSTR(order,2))==ALLTRIM(cor_in1(i1,1))
           cor_in1(i1,2)=PADR(TRIM(mark)+',  '+TRIM(mera)+IIF(minmax(j,3),'/ч',''),20,' ')
           cor_in1(i1,3)=IIF(k=n_blokov+1,'станция','блок '+STR(ai(k),1)+' ')
           cor_in1(i1,4)=IIF(TYPE('x')='C',PADL(x,14,' '),;
           STR(IIF(minmax(j,3),x/NumbHours(k),x),14,3))
           cor_in1(i1,5)=STR(min_z,14,3)
           cor_in1(i1,6)=STR(max_z,14,3)
           cor_in1(i1,7)=opisanie
        ENDIF
        ENDIF
    ENDFOR
ENDFOR
select ftabl
IF i1=0
   return to calc
ELSE
   CREATE TABLE cor_in1 (f1 C(6),f2 C(20),f3 C(7),f4 C(14),f5 C(14),f6 C(14),f7 M)
   SELECT 4
   USE cor_in1
   DIMENSION cor_in3(7)
   FOR i0=1 TO ALEN(cor_in1,1)
   FOR j=1 TO 7
       cor_in3(j)=cor_in1(i0,j)
   ENDFOR
       APPEND FROM ARRAY cor_in3
       REPLACE f7 WITH cor_in3(7)
   ENDFOR
   go top
   DO FORM cor_in
ENDIF