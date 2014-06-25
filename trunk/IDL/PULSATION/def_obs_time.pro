
;-------------------------------------------------------------------------------------------------
; YEARS SELECTION
;-------------------------------------------------------------------------------------------------

IF data_type EQ 'c1m' OR data_type EQ 'c1d' THEN BEGIN & fym_only = 0 & fyc_only = 0 & ayc_only = 0 & fmr_only = 0 & ENDIF

IF fyo_only THEN yearend_obs = yearini_obs
nbyear_obs   = yearend_obs-yearini_obs+1 ;& help, nbyear_obs
listyear_obs = yearini_obs+indgen(nbyear_obs) ;& help, listyear_obs

IF (WHERE( STRMATCH( exp_list, 'tr*')))[0] NE -1 OR (WHERE( STRMATCH( exp_list, 'ind*')))[0] NE -1 THEN BEGIN
  @def_model_time
ENDIF ELSE BEGIN
  listyear_mod = listyear_obs
  nbyear_mod   = nbyear_obs
  yearini_mod  = yearini_obs
  yearend_mod  = yearend_obs
ENDELSE

IF fyc_only THEN BEGIN
  ind_fyc = listmatch(listyear_mod,listyear_obs)
  IF ind_fyc[0,0] NE -1 THEN BEGIN
    yearini_obs = listyear_obs[ind_fyc[0,1]] ;& help, yearini_obs
    yearend_obs = MIN([yearend_obs,yearini_obs+nbyear_load]) ;& help, yearend_obs
  ENDIF ELSE STOP
ENDIF

IF fym_only THEN BEGIN
  ind_fymg = listmatch(listyear_mod,listyear_obs)
  IF ind_fymg[0,0] NE -1 THEN BEGIN
    yearini_obs = listyear_obs[ind_fymg[0,1]] ;& help, yearini_obs
    yearend_obs = yearini_obs ;& help, yearend_obs
  ENDIF ELSE STOP
ENDIF

IF ayc_only THEN BEGIN
  ind_ayc = listmatch(listyear_mod,listyear_obs)
  IF ind_ayc[0,0] NE -1 THEN BEGIN
    yearini_obs = listyear_obs[ind_ayc[0,1]] ;& help, yearini_obs
    yearend_obs = listyear_obs[ind_ayc[n_elements(ind_ayc[*,0])-1,1]] ;& help, yearend_obs
  ENDIF ELSE STOP
ENDIF

IF fmr_only THEN BEGIN
  yearini_obs = MIN( manual_range)
  yearend_obs = MAX( manual_range)
ENDIF

nbyear_obs    = yearend_obs - yearini_obs + 1 ;& help, nbyear_obs
listyear_obs  = yearini_obs + INDGEN(nbyear_obs) ;& help, listyear_obs
listmonth_obs = INDGEN(nbyear_obs*12) MOD 12
help, yearini_obs, yearend_obs, nbyear_obs, listyear_obs

;IF (WHERE( STRMATCH( exp_list, 'tr*')))[0] NE -1 OR (WHERE( STRMATCH( exp_list, 'ind*')))[0] NE -1 THEN BEGIN
  yearini_mod  = yearini_obs
  yearend_mod  = yearend_obs
  nbyear_mod    = nbyear_obs
  listyear_mod  = listyear_obs
  listmonth_mod = listmonth_obs
  help, yearini_mod, yearend_mod, nbyear_mod, listyear_mod, listmonth_mod
;ENDIF


;-------------------------------------------------------------------------------------------------
; TIME AXIS
;-------------------------------------------------------------------------------------------------

; DATES DEFINITION
IF data_type EQ 'c1m' THEN dates = [0, 11]
IF data_type EQ 'c1d' THEN dates = [0,365]
IF data_type EQ '1d' OR data_type EQ '1m' THEN cmd = execute( 'dates = ['+strtrim(yearini_obs,2)+'0101.00d,'+strtrim(yearend_obs,2)+'1231.99d ]' )
IF data_type EQ '6h' THEN cmd = execute( 'dates = ['+strtrim(yearini_obs,2)+'0101.00d,'+strtrim(yearend_obs,2)+'0131.99d ]' )
print, dates

; 6h CASE
IF data_type EQ '6h' THEN BEGIN
  nbyear_obs  = 1
  nbmonth_obs = 1
  listmonth_obs = 0
  nbday_obs = long(date2jul(double(dates[1])) - date2jul(double(dates[0])) + 1) & help, nbday_obs
  nbdt_obs  = nbday_obs * 4.
  time_obs = jul2date(date2jul(double(dates[0])) + dindgen(nbdt_obs)/4.) & help, time_obs
  ind_mean6h = !NULL
  FOR i = 0, n_elements(ind_period)-1 DO ind_mean6h = [ ind_mean6h, WHERE(FIX((time_obs-FIX(time_obs/10000.)*10000.)/100.) EQ ind_period[i]+1 )]
  help, ind_mean6h
ENDIF

; 1d CASE
IF data_type EQ '1d' THEN BEGIN
  nbmonth_obs = nbyear_obs / 12.
  nbday_obs   = long(date2jul(double(dates[1])) - date2jul(double(dates[0])) + 1) & help, nbday_obs
  time_obs    = jul2date(date2jul(double(dates[0])) + dindgen(nbday_obs)) & help, time_obs
  ind_mean1d = !NULL
  FOR i = 0, n_elements(ind_period)-1 DO ind_mean1d = [ ind_mean1d, WHERE(FIX((time_obs-FIX(time_obs/10000.)*10000.)/100.) EQ ind_period[i]+1 )]
  help, ind_mean1d
ENDIF

; 1m CASE
IF data_type EQ '1m' THEN BEGIN
  nbmonth_obs = nbyear_obs*12 & help, nbmonth_obs
  time_obs = double(strtrim(yearini_obs,2)+'0115') + (dindgen(nbmonth_obs) MOD 12)*100.00d & help, time_obs
  FOR m = 0, nbmonth_obs-1 DO time_obs[m] = time_obs[m] + CEIL(m/12)*10000.00d
  ind_mean1m = !NULL
  FOR i = 0, n_elements(ind_period)-1 DO ind_mean1m = [ ind_mean1m, where(listmonth_obs EQ ind_period[i]) ]
  help, ind_mean1m
ENDIF

; c1m CASE
IF data_type EQ 'c1m' THEN BEGIN
  nbyear_obs  = 1
  nbmonth_obs = 12
  listmonth_obs = INDGEN(nbmonth_obs)
  time_obs = double( strtrim( yearini_obs,2)+'0115') + (dindgen(nbmonth_obs) MOD 12)*100.00d & help, time_obs
  ind_mean1m = ind_period
  help, ind_mean1m
ENDIF

; c1d CASE
IF data_type EQ 'c1d' THEN BEGIN
  nbyear_obs  = 1
  nbmonth_obs = 12
  nbday_obs   = 365
  listday_obs = INDGEN(nbday_obs)
  time_obs    = jul2date(date2jul(double(dates[0])) + dindgen(nbday_obs)) & help, time_obs
  ind_mean1d = !NULL
  FOR i = 0, n_elements(ind_period)-1 DO ind_mean1d = [ ind_mean1d, WHERE(FIX((time_obs-FIX(time_obs/10000.)*10000.)/100.) EQ ind_period[i]+1 )]
  help, ind_mean1d & STOP
ENDIF

; SAUVEGARDE
IF data_type EQ '1d' THEN cmd = execute( 'nbday_'+STRTRIM(e,2)+' = nbday_obs' )

IF debug THEN STOP
