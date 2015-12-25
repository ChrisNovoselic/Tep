SET TALK OFF
ON KEY
SET DATE GERMAN
SET ESCAPE OFF
SET COLOR TO
SET SAFETY OFF
SET CARRY ON
SET BLINK OFF
= INSMODE(.F.)
CLOSE ALL
CLEAR ALL
clear macro
CLEAR

ON KEY LABEL Home GO TOP
ON KEY LABEL End GO BOTTOM
ON KEY LABEL F4 insert blank
ON KEY LABEL F6 DO SEARCH
ON KEY LABEL F7 delete
ON KEY LABEL Ctrl-F7 recall
ON KEY LABEL F8 do pack
ON KEY LABEL F9 do form mean
ON KEY LABEL F10 do end
WITH _Screen
     .FontName='Arial Cyr'
     .FontSize=10
     .WindowState=2
ENDWITH
DEFINE WINDOW ftabl FROM 0,0 TO 24,60 FLOAT TITLE '' COLOR n/w,w+/r,w/n,,,,w+/b FONT "Arial Cyr"
psk1=''
SELECT 1
USE ftabl
DELETE TAG ALL
REPLACE ALL ftabl.id WITH STRTRAN(ftabl.id,':','/')
SORT TO FTABL1.DBF ON FTABL.ID
USE
COPY FILE FTABL1.DBF TO FTABL.DBF
DELETE FILE FTABL1.DBF
USE ftabl
REPLACE ALL ftabl.id WITH STRTRAN(ftabl.id,'/',':')
GO TOP

@ 1,61 say 'Home - на начало' FONT "Arial Cyr"
@ 2,61 say 'End  - на конец' FONT "Arial Cyr"
@ 3,61 say 'F4   - вставка строки' FONT "Arial Cyr"
@ 4,61 say 'F6   - поиск' FONT "Arial Cyr"
@ 5,61 say 'F7   - отметка для удаления' FONT "Arial Cyr"
@ 6,61 say 'Ctrl-F7' FONT "Arial Cyr"
@ 7,61 say '     - убрать отметку' FONT "Arial Cyr"
@ 8,61 say 'F8   - удаление' FONT "Arial Cyr"
@ 9,61 say 'F9   - вычислить пропорцию(ступенька)' FONT "Arial Cyr"
@10,61 say 'F10  - конец работы' FONT "Arial Cyr"

activ window ftabl
DO WHILE .T.
brows field id,a1,a2:W=(':2'$id OR ':3'$id),a3:W=(':3'$id),f in window ftabl;
nodelete noappend
ENDDO
**************************************
PROCEDURE end
releas window
close all
clear
SET CARRY OFF
SET BLINK ON
on key
CANCEL
**************************************
PROCEDURE search
i=RECNO()
DEFINE WINDOW psk FROM 8, 20 TO 10,60 ;
       COLOR w+/rb,w+/r,w+/rb,w+/rb,,w+/r,,,w+/rb FONT "Courier New",10
ACTIVATE WINDOW psk 
@ 0,3 SAY "Перейти к графику:" 
@ 0,21 GET psk1 SIZE 1,10 DEFAULT " "
READ
release window psk
LOCATE FOR RTRIM(psk1)$ftabl.id REST
IF .NOT.FOUND() 
   GO i   
ENDIF   
**************************************
PROCEDURE pack
i=RECNO()
pack
SET NEAR ON
LOCATE RECORD i
SET NEAR OFF   
