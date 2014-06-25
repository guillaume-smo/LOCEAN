
print, '' & print, 'DEF_MODEL_TIME...'


;-------------------------------------------------------------------------------------------------
; MODEL DATES DEFINITION
;-------------------------------------------------------------------------------------------------

; DATES PAR DEFAUT POUR WRF ET NEMO
test = WHERE(STRMATCH(exp_list, '*_erai*', /FOLD_CASE) EQ 1 OR STRMATCH(exp_list, '*_quik*', /FOLD_CASE) EQ 1, nbok)
IF nbok EQ 0 THEN yearini_mod = 1989 ELSE IF data_type EQ 'c1m' THEN yearini_mod = 2001 ELSE yearini_mod = 2000
IF nbok EQ 0 THEN yearend_mod = 2009 ELSE yearend_mod = 2008

; EXCEPTIONS
IF WHERE( STRMATCH( exp_list, 'tr12_bmj01ml60', /FOLD_CASE) ) EQ 1 THEN yearend_mod = 1993
IF WHERE( STRMATCH( exp_list, 'tr075_ra12L60' , /FOLD_CASE) ) EQ 1 THEN yearend_mod = 1998

; CLIM NOW c1m
IF data_type EQ 'c1m' AND model EQ 'now' THEN yearini_mod = 1990

; TROP025 NOW DATES
IF grid EQ 'trop025' AND model EQ 'now' THEN BEGIN
  yearini_mod = 1990 & yearend_mod = 2009
ENDIF

; TROP075 NOW DATES
IF grid EQ 'trop075' AND model EQ 'now' THEN BEGIN
  yearini_mod = 1990 & yearend_mod = 2009
  IF exp_name EQ 'tr075_cpl11L60' THEN yearend_mod = 1993
  IF exp_name EQ 'tr075_cpl12L60' THEN yearend_mod = 1993
  IF exp_name EQ 'tr075_cpl15L60' THEN yearend_mod = 1993
  IF exp_name EQ 'tr075_cpl33L60' THEN yearend_mod = 1993
  IF exp_name EQ 'tr075_cpl44L60' THEN yearend_mod = 1993
  IF exp_name EQ 'tr075_cpl55L60' THEN yearend_mod = 1993
  IF exp_name EQ 'tr075_cpl12L60_sol094albdamp' THEN yearend_mod = 1991
  IF exp_name EQ 'tr075_cpl12L60_sol094alb' THEN yearend_mod = 2005
  IF exp_name EQ 'tr075_cpl12L60_sol093alb' THEN yearend_mod = 2005
  IF exp_name EQ 'tr075_cpl12L60_sol098' THEN yearend_mod = 1993
  IF exp_name EQ 'tr075_cpl12L60_sol096' THEN yearend_mod = 1993
  IF exp_name EQ 'tr075_cpl12L60_sol092' THEN yearend_mod = 1993
  IF exp_name EQ 'tr075_cpl12L60_sol096lev' THEN yearend_mod = 1993
  IF exp_name EQ 'tr075_cpl1l60_nocrt' THEN yearend_mod = 1993
  IF exp_name EQ 'tr075_cpl1l60' THEN yearend_mod = 1993
  IF STRMATCH( exp_name, 'tr075now_long_sol09?') THEN BEGIN & yearini_mod = 1980 & yearend_mod = 2012 & ENDIF
ENDIF

; TROP075_WRF DATES
IF grid EQ 'trop075' AND model EQ 'wrf' THEN BEGIN
  yearini_mod = 1989 & yearend_mod = 2009
  IF exp_name EQ 'tr075_ra12L60_wsm3' THEN yearend_mod = 1989
  IF exp_name EQ 'tr075_ra12L60' THEN yearend_mod = 1998
  IF exp_name EQ 'tr075_ra12L60_alb' THEN yearend_mod = 1989
  IF exp_name EQ 'tr075_ra12L60_kf' THEN yearend_mod = 1989
  IF exp_name EQ 'tr075_ra12L60_sol094era234'    THEN yearend_mod = 1989
  IF exp_name EQ 'tr075_ra12L60_sol094erafullt'  THEN yearend_mod = 1989
  IF exp_name EQ 'tr075_ra12L60_sol094eratw'     THEN yearend_mod = 1989
  IF exp_name EQ 'tr075_ra12L60_sol094erafulltw' THEN yearend_mod = 1989
  IF exp_name EQ 'tr075_ra12L60_sol090'          THEN yearend_mod = 1989
  IF exp_name EQ 'tr075_ra12L60_sol096'          THEN yearend_mod = 1989
  IF exp_name EQ 'tr075_ra12L60_sol098'          THEN yearend_mod = 1989
  IF exp_name EQ 'tr075_ra12L60_g3d'             THEN yearend_mod = 1989
  IF exp_name EQ 'tr075_ra12L60_tdk'             THEN yearend_mod = 1989
  IF exp_name EQ 'tr075_ra12L60_fairall'         THEN yearend_mod = 1989
  IF exp_name EQ 'tr075_ra12L60_sol093fairall'   THEN yearend_mod = 1989
  IF exp_name EQ 'tr075_ra11L60' THEN yearend_mod = 1998
  IF exp_name EQ 'tr075_ra15L60' THEN yearend_mod = 1998
  IF exp_name EQ 'tr075_ra33L60' THEN yearend_mod = 1998
  IF exp_name EQ 'tr075_ra44L60' THEN yearend_mod = 1998
  IF exp_name EQ 'tr075_ra55L60' THEN yearend_mod = 1998
  IF exp_name EQ 'tr075_ra11ysuL60'      THEN yearend_mod = 1989
  IF exp_name EQ 'tr075_ra11L60_wsm3'    THEN yearend_mod = 1989
  IF exp_name EQ 'tr075_ra11ysuL60_wsm3' THEN yearend_mod = 1989
  IF exp_name EQ 'tr075_bmj01m'          THEN yearend_mod = 2009
  IF exp_name EQ 'tr075_bmj02sfc'        THEN yearend_mod = 2009

  IF exp_name EQ 'tr075_ra12L60_sol094sstnow'  THEN yearend_mod = 1998
  IF exp_name EQ 'tr075_ra12L60_sol094sstclim' THEN yearend_mod = 1998
  IF exp_name EQ 'tr075_ra12L60_sol094sstbb' THEN yearend_mod = 1998
  IF exp_name EQ 'tr075_ra12L60_sol094sstwi' THEN yearend_mod = 1998
  IF exp_name EQ 'tr075_ra12L60_sol094sstbbr' THEN yearend_mod = 1998
  IF exp_name EQ 'tr075_ra12L60_sol094sstwir' THEN yearend_mod = 1998
  IF exp_name EQ 'tr075_ra12L60_mynnsfc5' THEN yearend_mod = 1998
  IF exp_name EQ 'tr075_ra12L60_mynn' THEN yearend_mod = 1998
  IF exp_name EQ 'tr075_ra11ysuL60' THEN yearend_mod = 1998

ENDIF

; TROP025_WRF DATES
IF grid EQ 'trop025' AND model EQ 'wrf' THEN BEGIN
  IF exp_name EQ 'tr025_ra12L60' THEN yearend_mod = 1998
  IF exp_name EQ 'tr025_kf01m' THEN yearend_mod = 1989
  IF exp_name EQ 'tr025_bmj02' THEN yearend_mod = 1993
  IF exp_name EQ 'tr025_bmj02ysu' THEN yearend_mod = 1993
  ;IF exp_name EQ 'tr025_bmjang' THEN yearend_mod = 1989
  IF exp_name EQ 'tr025_bmjpbl5' THEN yearend_mod = 1993
  IF exp_name EQ 'tr025_bmj02sfc5' THEN yearend_mod = 2007
  IF exp_name EQ 'tr025_cam01' THEN yearend_mod = 1993
  IF exp_name EQ 'tr025_kf02m' THEN yearend_mod = 1989
  IF exp_name EQ 'tr025_kftemf01' THEN yearend_mod = 1989
  IF exp_name EQ 'tr025_ra11L60' THEN yearend_mod = 1989
  IF exp_name EQ 'tr025_ra15L60' THEN yearend_mod = 1989
  IF exp_name EQ 'tr025_ra33L60' THEN yearend_mod = 1989
  IF exp_name EQ 'tr025_ra44L60' THEN yearend_mod = 1989
  IF exp_name EQ 'tr025_ra55L60'  THEN yearend_mod = 1989
ENDIF

; TROP12_WRF DATES
IF grid EQ 'trop12' AND model EQ 'wrf' THEN BEGIN
  IF exp_name EQ 'tr12_bmj01'     THEN yearend_mod = 1990
  IF exp_name EQ 'tr12_bmj01m'    THEN BEGIN yearini_mod = 1991 & yearend_mod = 1993 & ENDIF
  IF exp_name EQ 'tr12_bmj01ml60' THEN yearend_mod = 1993
  IF exp_name EQ 'tr12_kf01'      THEN yearend_mod = 1991
  IF exp_name EQ 'tr12_kf02'      THEN yearend_mod = 1989
ENDIF

; NEMO DATES
IF STRMATCH(exp_name, '*_erai*', /FOLD_CASE) EQ 1 OR STRMATCH(exp_name, '*_quik*', /FOLD_CASE) EQ 1 THEN BEGIN
  IF data_type EQ 'c1m' THEN BEGIN
    yearini_mod = 2001 
  ENDIF ELSE BEGIN 
    yearini_mod = 2000
    IF exp_name EQ 'tr075_erainocrt' THEN yearini_mod = 1997
  ENDELSE
  yearend_mod = 2008
ENDIF

; IND025 DATES
IF ( WHERE( STRMATCH(exp_list, 'ind*', /FOLD_CASE) EQ 1))[0] NE -1 THEN yearini_mod = 1990

help, yearini_mod, yearend_mod

; SAUVEGARDE
yearini_modfile = yearini_mod
yearend_modfile = yearend_mod


;-------------------------------------------------------------------------------------------------
; GESTION DATES
;-------------------------------------------------------------------------------------------------

IF data_type EQ 'c1m' THEN BEGIN & fym_only = 0 & fyo_only = 0 & fyc_only = 0 & ayc_only = 0 & fmr_only = 0 & ENDIF

; EXCEPTION NEMO TROP075
IF e GE 1 THEN BEGIN
  test = WHERE(STRMATCH(exp_name, 'tr075_erai*', /FOLD_CASE) EQ 1 OR STRMATCH(exp_name, 'tr075_quik*', /FOLD_CASE) EQ 1, nbok)
  IF fym_only AND nbok EQ 0 THEN yearend_mod = yearini_mod
ENDIF
nbyear_mod   = yearend_mod-yearini_mod+1 ;& help, nbyear_mod
listyear_mod = yearini_mod + indgen(nbyear_mod) & help, listyear_mod

IF fyc_only THEN BEGIN
  ind_fyc = listmatch(listyear_mod,listyear_obs)
  IF ind_fyc[0,0] NE -1 THEN BEGIN
    IF nbok EQ 0 THEN yearini_mod = listyear_mod[ind_fyc[0,0]] ;& help, yearini_mod
    IF nbok EQ 0 THEN yearend_mod = MIN([yearend_mod, yearini_mod+nbyear_load]) ;& help, yearend_mod
  ENDIF ELSE STOP
ENDIF

IF fyo_only THEN BEGIN
  ind_fyo = listmatch(listyear_mod,listyear_obs)
  IF ind_fyo[0,0] NE -1 THEN BEGIN
    IF nbok EQ 0 THEN yearini_mod = listyear_mod[ind_fyo[0,0]] ;& help, yearini_mod
    IF nbok EQ 0 THEN yearend_mod = yearini_mod
  ENDIF ELSE BEGIN
    yearini_mod = listyear_mod[0]
    yearend_mod = yearini_mod
  ENDELSE
ENDIF

IF ayc_only THEN BEGIN
  ind_ayc = listmatch(listyear_mod,listyear_obs)
  IF ind_ayc[0,0] NE -1 THEN BEGIN
    yearini_mod = listyear_mod[ind_ayc[0,0]] ;& help, yearini_mod
    yearend_mod = listyear_mod[ind_ayc[n_elements(ind_ayc[*,0])-1,0]] ;& help, yearend_mod
  ENDIF ELSE STOP
ENDIF

IF fmr_only THEN BEGIN
  yearini_mod = MIN( manual_range)
  yearend_mod = MAX( manual_range)
ENDIF

nbyear_mod    = yearend_mod - yearini_mod + 1 ;& help, nbyear_mod
listyear_mod  = yearini_mod + indgen(nbyear_mod) ;& help, listyear_mod
listmonth_mod = INDGEN(nbyear_mod*12) MOD 12
nbyear_modfile    = yearend_modfile - yearini_modfile + 1 ;& help, nbyear_mod
listyear_modfile  = yearini_modfile + indgen(nbyear_modfile) ;& help, listyear_mod
listmonth_modfile = INDGEN(nbyear_modfile*12) MOD 12


;-------------------------------------------------------------------------------------------------
; TIME AXIS
;-------------------------------------------------------------------------------------------------

; DATES DEFINITION IF NOT ALREADY DONE IN "def_obs_dates"
IF WHERE( STRMATCH( exp_list[0], 'tr*')  EQ 0) EQ -1 OR $
   WHERE( STRMATCH( exp_list[0], 'ind*') EQ 0) EQ -1 THEN BEGIN

  IF data_type EQ 'c1m' THEN dates = [0,11]
  IF data_type EQ '1d' OR data_type EQ '1m' THEN cmd = execute( 'dates = ['+strtrim(yearini_mod,2)+'0101.00d,'+strtrim(yearend_mod,2)+'1231.99d ]' )
  IF data_type EQ '6h' THEN cmd = execute( 'dates = ['+strtrim(yearini_mod,2)+'0101.00d,'+strtrim(yearend_mod,2)+'0131.99d ]' )
  print, dates

ENDIF


; 6h CASE
IF data_type EQ '6h' THEN BEGIN
  nbyear_mod  = 1
  nbmonth_mod = 1
  listmonth_mod = 0
  nbday_mod = long(date2jul(double(strtrim(yearend_mod,2)+'0131')) - date2jul(double(strtrim(yearini_mod,2)+'0101')) + 1) & help, nbday_mod
  nbdt_mod  = nbday_mod * 4.
  time_mod = jul2date(date2jul(double(strtrim(yearini_mod,2)+'0101')) + dindgen(nbdt_mod)/4.) & help, time_mod
  ind_mean6h = !NULL
  FOR i = 0, n_elements(ind_period)-1 DO ind_mean6h = [ ind_mean6h, WHERE(FIX((time_mod-FIX(time_mod/10000.)*10000.)/100.) EQ ind_period[i]+1 )]
  help, ind_mean6h
ENDIF

; 1d CASE
IF data_type EQ '1d' THEN BEGIN
  nbmonth_mod = nbyear_mod*12 & help, nbmonth_mod
  nbday_mod = long(date2jul(double(strtrim(yearend_mod,2)+'1231')) -  date2jul(double(strtrim(yearini_mod,2)+'0101')) + 1) & help, nbday_mod
  time_mod  = jul2date(date2jul(double(strtrim(yearini_mod,2)+'0101')) + dindgen(nbday_mod)) & help, time_mod
  ind_mean1d = !NULL
  FOR i = 0, n_elements(ind_period)-1 DO ind_mean1d = [ ind_mean1d, WHERE(FIX((time_mod-FIX(time_mod/10000.)*10000.)/100.) EQ ind_period[i]+1 )]
  help, ind_mean1d
ENDIF

; 1m CASE
IF data_type EQ '1m' THEN BEGIN

  nbmonth_mod     = nbyear_mod*12     & help, nbmonth_mod
  nbmonth_modfile = nbyear_modfile*12 & help, nbmonth_modfile
  IF exp_name EQ 'tr12_kf02' THEN nbmonth_mod = 5
  listmonth_mod = INDGEN( nbmonth_mod) MOD 12

  ; 1m TIME AXIS
  time_modfile = double( strtrim( yearini_modfile,2)+'0115') + (dindgen( nbmonth_modfile) MOD 12)*100.00d  & help, time_modfile
  time_mod = double( strtrim( yearini_mod,2)+'0115') + (dindgen( nbmonth_mod) MOD 12)*100.00d  & help, time_mod
  FOR m = 0, nbmonth_modfile-1 DO time_modfile[m] = time_modfile[m] + CEIL(m/12)*10000.00d
  FOR m = 0, nbmonth_mod-1     DO time_mod[m]     = time_mod[m]     + CEIL(m/12)*10000.00d

  ; TOTAL MEAN INDEX
  ind_mean1m = !NULL
  FOR i = 0, n_elements(ind_period)-1 DO ind_mean1m = [ ind_mean1m, WHERE(listmonth_mod EQ ind_period[i]) ]
  help, ind_mean1m

ENDIF

; c1m CASE
IF data_type EQ 'c1m' THEN BEGIN
  nbyear_mod  = 1
  nbmonth_mod = 12
  listmonth_mod = INDGEN(12)
  time_mod = double(strtrim(yearini_mod,2)+'0115') + (dindgen(nbmonth_mod) MOD 12)*100.00d & help, time_mod
  ind_mean1m = ind_period
ENDIF


; SAUVEGARDE POUR INTERPOLATION
IF data_type EQ  '1d' THEN cmd = execute(   'nbday_'+STRTRIM(e,2)+' =   nbday_mod' )
IF data_type EQ 'c1m' OR data_type EQ '1m' THEN cmd = execute( 'nbmonth_'+STRTRIM(e,2)+' = nbmonth_mod' )


print, 'DEF_MODEL_TIME OK' & print, ''
IF debug THEN STOP
