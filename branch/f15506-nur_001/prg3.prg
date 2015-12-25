PUSH KEY CLEAR
ON KEY LABEL F1 HELP ID 2
DEFINE POPUPS POP3 FROM 03,21 IN SCREEN TITLE'[ћ≈Ќё –ј—„≈“ј Ќќ–ћј“»¬Ќџ’ ѕќ ј«ј“≈Ћ≈…]' COLOR ;
       bg/w*,n/w*,gr+/w,gr+/w,,w+/r* FONT "Courier New",11
DEFINE BAR 01 OF POP3 PROMPT '—оздать входную таблицу                ' SKIP FOR .T. &&FILE('inblok.dbf')
DEFINE BAR 02 OF POP3 PROMPT '”ничтожить входную таблицу             ' SKIP FOR .T. &&.NOT.FILE('inblok.dbf')
DEFINE BAR 03 OF POP3 PROMPT '¬вести во входную таблицу выработку э/э' SKIP FOR .NOT.FILE('inblok.dbf')
DEFINE BAR 04 OF POP3 PROMPT '¬ыработка э/э - автоматически          ' SKIP FOR .NOT.FILE('inblok.dbf')
DEFINE BAR 05 OF POP3 PROMPT 'ќтредактировать входную таблицу        ' SKIP FOR .NOT.FILE('inblok.dbf')
DEFINE BAR 06 OF POP3 PROMPT 'ѕровести расчет                        ' SKIP FOR .NOT.FILE('inblok.dbf')
DEFINE BAR 07 OF POP3 PROMPT 'ѕросмотреть выходную таблицу           '
DEFINE BAR 08 OF POP3 PROMPT '–аспечатать входную таблицу            ' SKIP FOR .NOT.FILE('inblok.dbf')
DEFINE BAR 09 OF POP3 PROMPT '–аспечатать выходную таблицу           '
DEFINE BAR 10 OF POP3 PROMPT 'MAXимумы и MINимумы входной таблицы    ' SKIP FOR .NOT.FILE('inblok.dbf')
DEFINE BAR 11 OF POP3 PROMPT '√лавное меню                           '
ON SELECTION POPUPS POP3 DO tmenu3 WITH PROMPT()
DO scr WITH "Ѕолее подробно о программе - по [F1]","w/n"
ACTIVATE POPUPS POP3
*****************************************************************************
*                          Procedure   tmenu3                               *
*****************************************************************************
PROCEDURE tmenu3
PARAMETERS MPROMPT
DO CASE
   CASE MPROMPT="—оздать входную таблицу" && аналог.п.в PRG4.PRG
      COPY FILE BASE\inblok.dbf TO inblok.dbf
      KEYBOARD "{DNARROW}"
   CASE MPROMPT="”ничтожить входную таблицу" && аналог.п.в PRG4.PRG
      HIDE POPUP ALL
      DEFINE WINDOW pri FROM 0,0 TO 24,79 NONE color n/w,w+/r FONT "Courier New",10
      ACTIV WINDOW pri
      yn='A'
      DO WHILE .NOT.(UPPER(yn)='Y' OR UPPER(yn)='N')
        yn='N'
        @ 10,15 SAY "¬ы уверены, что это ¬ам нужно,(Y/N) " GET yn
        READ
      ENDDO
      RELEASE WIND pri
      IF UPPER(yn)='Y'
         DELETE FILE inblok.dbf
         KEYBOARD "{UPARROW}"
      ENDIF
   CASE MPROMPT="ќтредактировать входную таблицу" && аналог.п.в PRG4.PRG
      PUSH KEY CLEAR
      ON KEY LABEL S do s
      FormNum="1"
      DO FORM TABLE1 WITH "inblok",.T.
      ON KEY LABEL S
      POP KEY
   CASE MPROMPT="¬вести во входную таблицу выработку э/э" && аналог.п.в PRG4.PRG
      HIDE POPUP ALL
      _Screen.MousePointer=11
      error=0
      on error store error() to error
      SET DEFAULT TO (KTSpath)
      on error 
      SET DEFAULT TO (DefaultDir)
      IF error=0
         USE inblok
         file_name='par_el'
         do forma dir
         file_name='par_el5msc'
         do forma dir
         file_name='par_e6'
         do forma dir
         file_name='par_e6msc'
         do forma dir
         file_name='tep0'
         do forma dir
         file_name='par_tp'
         do forma dir
         file_name='par_tp5msc'
         do forma dir
         file_name='par_t6'
         do forma dir
         file_name='par_t6msc'
         do forma dir
         file_name='par_b1'
         do forma dir
         file_name='par_b1dms'
         do forma dir
*!*	         file_name='par_b2'
*!*	         do forma dir
*!*	         file_name='par_b3'
*!*	         do forma dir
         file_name='par_b6'
         do forma dir
         file_name='par_b6dms'
         do forma dir
         USE
      ELSE
         wait window KTSpath + 'Ќет доступа к источнику. Ќажмите любую клавишу.'  
      ENDIF
      _Screen.MousePointer=0
	&& ƒобавлен 20.06.2012 - автоматический ввод
	&& Ѕлоки єє1,6 мск
	&&  “— Ёлектричество 6-€ верси€ мск
	&&  “— “епло 6-€ верси€ мск
	CASE MPROMPT="¬ыработка э/э - автоматически" && Ќ≈“ аналогј .п.в PRG4.PRG
      HIDE POPUP ALL
      _Screen.MousePointer = 11
      error=0
      on error store error() to error
      SET DEFAULT TO (KTSpath)
      on error 
      SET DEFAULT TO (DefaultDir)
      IF error=0
         USE inblok

         && »з 'frsDir1.Unload'
         && SET TALK ON
         && SET COMPATIBLE FOXPLUS
         && SET COMPATIBLE ON
         && Ќо предџƒущ. разработчик в 'frsDir1.Unload' их ¬—≈√о-Ћ»Ўь ¬ќ——“јЌј¬ливает из сохрјЌ-х в 'frsDir1.Load'
         && поЁ“ќћу: 'frsDir1' Ќ≈ »—ѕќЋ№зу≈ћ «Ќј„ит и ¬ќ——“јЌј¬Ћиват№ Ќ≈„≈√ќ

         && 1-е присвоение значени€ 'file_name' — ќѕ»–ќ¬јно из предыдущего пунк“а меню

         && ѕри 2-м присваиван»и значени€ 'file_name'
         && «десь (так же как и в 'DIR.load') при назначении 'file_name' крайЌя€ буква м.б. не назначенј
		 && т.к. к маске при вызов≈ 'ADIR' добавл€ет—€ '?'

         file_name='par_el'
         && Ќ≈ »—ѕќЋ№«”≈“—€
         && do forma dir

         file_name='par_el5msc'
		 && Ќ≈ »—ѕќЋ№«”≈“—€
         && do forma dir

         file_name='par_e6'
         && Ќ≈ »—ѕќЋ№«”≈“—€
         && do forma dir

		file_name='par_e6msc'
		&& из DIR.load
		folder=KTSpath+'\ZTU_E6\'
		file_name='par_eldms'
		file_res = find_lastmodify (folder, file_name)
		IF NOT EMPTY (file_res) then
			do import with folder + file_res, 1, 1
		 ELSE
		 endif

		file_name = 'tep0'
		folder = KTSpath + '\ZTU_E6\'
		&& file_name = ѕ–≈∆Ќее
		file_res = find_lastmodify (folder, file_name)
		IF NOT EMPTY (file_res) then
		do import with folder + file_res, 1, 1
		ELSE
		ENDIF
		
		file_name = 'par_tp'
		&& Ќ≈ »—ѕќЋ№«”≈“—€
		&& do forma dir

		file_name = 'par_tp5msc'
		&& Ќ≈ »—ѕќЋ№«”≈“—€
		&& do forma dir

		file_name = 'par_t6'
		&& Ќ≈ »—ѕќЋ№«”≈“—€
		&& do forma dir

		file_name = 'par_t6msc'
		&& из DIR.load
		folder = KTSpath + '\ZTU_T6\'
		file_name = 'par_tpdms'
		file_res = find_lastmodify (folder, file_name)
		IF NOT EMPTY (file_res) then
			do import with folder + file_res, 1, 1
		ELSE
		endif

		file_name='par_b1'
		&& Ќ≈ »—ѕќЋ№«”≈“—€
		&& do forma dir

		file_name='par_b1dms'
		&& из DIR.load
		folder=KTSpath+'\ZTU_B1\'
		&& file_name = ѕ–≈∆Ќее
		file_res = find_lastmodify (folder, file_name)
		IF NOT EMPTY (file_res) then
		do import with folder + file_res, 1, 1
		ELSE
		endif

		file_name='par_b2'
		&& ¬ќќЅў≈ Ќ≈ »—ѕќЋ№«”≈“—€
		&& do forma dir

		file_name='par_b3'
		&& ¬ќќЅў≈ Ќ≈ »—ѕќЋ№«”≈“—€
		&& do forma dir

		file_name='par_b6'
		&& Ќ≈ »—ѕќЋ№«”≈“—€
		&& do forma dir

		file_name='par_b6dms'
		&& из DIR.load
		folder=KTSpath+'\ZTU_B6\'
		&& file_name = ѕ–≈∆Ќее
		file_res = find_lastmodify (folder, file_name)
		IF NOT EMPTY (file_res) then
		do import with folder + file_res, 1, 1
		ELSE
		ENDIF
		
		&& SET COMPATIBLE ON

		USE
	ELSE
		wait window KTSpath + 'Ќет доступа к источнику. Ќажмите любую клавишу.'  
	ENDIF
	_Screen.MousePointer=0

	CASE MPROMPT="ѕровести расчет" && аналог.п.в PRG4.PRG
		do precalc.prg  with 'outblok','calc.prg'
	CASE MPROMPT="ѕросмотреть выходную таблицу" && аналог.п.в PRG4.PRG
		PUSH KEY CLEAR
		ON KEY LABEL = do ravno
		FormNum="2"
		DO FORM TABLE1 WITH "outblok", .T.
		ON KEY LABEL =
		POP KEY
	CASE MPROMPT="–аспечатать входную таблицу"
		DO PRINT WITH "inblok", '¬ходна€ таблица “Ёѕ блоков'
	CASE MPROMPT="–аспечатать выходную таблицу"
		DO PRINT WITH "outblok",'¬ыходна€ таблица “Ёѕ блоков'
	CASE MPROMPT="MAXимумы и MINимумы входной таблицы"
		DO MAXMIN  && proc maxmin находитс€ в tep.prg
	CASE MPROMPT="√лавное меню"
		DO scr WITH "Ѕолее подробно о программе - по [F1]","w+/w"
	POP KEY
	RETURN TO MASTER
ENDCASE
*************************************************************
PROCEDURE ravno  && вычисл€ть/не вычисл€ть блок/станцию ;
похожий текст данной процедуры также в PRG4.PRG
	x=LOWER(varread())
	y=SUBSTR(&x,1,1)
	DO CASE
		CASE x='order'
			IF recno () = 1
				IF MESSAGEBOX('”брать все защиты?',4+32+256,'')=6
					REPL order WITH STRTRAN(order,'=',' '),blok1 WITH STRTRAN(blok1,'=',''),;
					blok2 WITH STRTRAN(blok2,'=',''),blok3 WITH STRTRAN(blok3,'=',''),;
					blok4 WITH STRTRAN(blok4,'=',''),blok5 WITH STRTRAN(blok5,'=',''),;
					blok6 WITH STRTRAN(blok6,'=',''),station WITH STRTR(station,'=','') ALL
					GOTO 1
				ELSE
					REPL order WITH IIF(y='=',STRTRAN(order,'=',' '),'='+RIGHT(order,5)),;
					blok1 WITH IIF(y='=',STRTRAN(blok1,'=',''),'='+RIGHT(RTRIM(blok1),9)),;
					blok2 WITH IIF(y='=',STRTRAN(blok2,'=',''),'='+RIGHT(RTRIM(blok2),9)),;
					blok3 WITH IIF(y='=',STRTRAN(blok3,'=',''),'='+RIGHT(RTRIM(blok3),9)),;
					blok4 WITH IIF(y='=',STRTRAN(blok4,'=',''),'='+RIGHT(RTRIM(blok4),9)),;
					blok5 WITH IIF(y='=',STRTRAN(blok5,'=',''),'='+RIGHT(RTRIM(blok5),9)),;
					blok6 WITH IIF(y='=',STRTRAN(blok6,'=',''),'='+RIGHT(RTRIM(blok6),9)),;
					station WITH IIF(y='=',STRTR(station,'=',''),'='+RIGHT(RTRIM(station),9))
					KEYBOARD "{TAB}{BACKTAB}"
				ENDIF
			ELSE
				REPL order WITH IIF(y='=',STRTRAN(order,'=',' '),'='+RIGHT(order,5)),;
				blok1 WITH IIF(y='=',STRTRAN(blok1,'=',''),'='+RIGHT(RTRIM(blok1),9)),;
				blok2 WITH IIF(y='=',STRTRAN(blok2,'=',''),'='+RIGHT(RTRIM(blok2),9)),;
				blok3 WITH IIF(y='=',STRTRAN(blok3,'=',''),'='+RIGHT(RTRIM(blok3),9)),;
				blok4 WITH IIF(y='=',STRTRAN(blok4,'=',''),'='+RIGHT(RTRIM(blok4),9)),;
				blok5 WITH IIF(y='=',STRTRAN(blok5,'=',''),'='+RIGHT(RTRIM(blok5),9)),;
				blok6 WITH IIF(y='=',STRTRAN(blok6,'=',''),'='+RIGHT(RTRIM(blok6),9)),;
				station WITH IIF(y='=',STRTR(station,'=',''),'='+RIGHT(RTRIM(station),9))
				KEYBOARD "{TAB}{BACKTAB}"
			ENDIF
		CASE SUBSTR(x,1,4)='blok' OR x='station'
			REPL &x WITH IIF(y='=',STRTRAN(&x,'=',''),'='+RIGHT(RTRIM(&x),9)),order WITH IIF(y='=',order,'='+RIGHT(order,5))
			KEYBOARD "{TAB}{BACKTAB}"
	ENDCASE
endproc

*************************************************************
**20-06-2102*************************************************
**ѕроцедура нахождени€ файла с саћџм ранним дата/врем€ модификации
PROCEDURE find_lastmodify
PARAMETERS path_dir, mask
	n = ADIR(ls, path_dir + mask + '?.*')
	indxRes = 1
	dateNow = DATE ()
	FOR i = indxRes TO n
		IF i > 1 THEN
			IF ls (i, 3) < ls (i - 1, 3) THEN
				indxRes = i - 1
			ELSE
			ENDIF
		ELSE
		ENDIF
	ENDFOR

	IF DAY (FDATE(path_dir + ls (indxRes, 1))) = DAY (DATE ()) then
		wait WINDOW "find_lastmodify: " + ls (indxRes, 1) + CHR(32) + DTOC (ls (indxRes, 3)) TIMEOUT 1
		RETURN ls (indxRes, 1)		
	ELSE
		wait WINDOW "«а текущую ƒ≈Ќь: " + trim (STR(DAY (DATE ()))) + ", в " + path_dir + CHR(13) + ;
			"входной файл по маске: " + mask + '?.*' + " Ќ≈ —‘ќ–ћ»–ќ¬јЌ!" TIMEOUT 1
		RETURN ""
	endif
ENDPROC