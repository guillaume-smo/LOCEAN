

; OBS CASE
IF STRMATCH( exp_name, 'tr*') EQ 0 AND STRMATCH( exp_name, 'ind*') EQ 0 THEN BEGIN
  save_data    = 0 & load_data    = 0
  save_gridobs = 0 & load_gridobs = 0
ENDIF


; ORIGINAL DATA
;IF (STRMATCH( exp_name, 'tr*') OR STRMATCH( exp_name, 'ind*')) AND save_data AND load_data EQ 0 THEN BEGIN
IF flag_obs EQ 0 AND save_data AND load_data EQ 0 THEN BEGIN

  print, 'WRITING DATA TO IDL FILE...'

  save_list = !NULL

  ; c1m+1m+1d VAR
  ;IF data_type EQ 'c1m' OR data_type EQ '1m' OR (data_type EQ '1d' AND (fym_only OR fyo_only OR fyc_only)) THEN BEGIN
  IF data_type NE '6h' THEN BEGIN
    save_list = [ save_list, 'var']
    IF STRMATCH(var_name, 'UV*') OR var_name EQ 'STRESS' OR var_name EQ 'RAIN' THEN save_list = [ save_list, 'varx', 'vary']
  ENDIF
  ; c1m+1m VAR_Z*
  IF flag_z THEN save_list = [ save_list, 'var_zp', 'var_zx', 'var_zy']
  ; 1m VAR_YEAR
  IF data_type EQ '1m' THEN save_list = [ save_list, 'var_year']
  ; 1m+1d+6h VAR_MEAN
  IF data_type EQ '1m' OR data_type EQ 'c1m' OR data_type EQ '1d' $
  ;OR (data_type EQ '1d' AND (fym_only OR fyo_only OR fyc_only) ) $
  OR (data_type EQ '6h' AND (fym_only OR fyo_only OR fyc_only) ) THEN BEGIN
    save_list = [ save_list, 'var_mean', 'ano_mean']
    IF STRMATCH(var_name, 'UV*') OR var_name EQ 'STRESS' OR var_name EQ 'RAIN' THEN save_list = [ save_list, 'varx_mean', 'vary_mean']
    IF flag_z THEN save_list = [ save_list, 'var_zp_mean', 'var_zx_mean', 'var_zy_mean']
  ENDIF
  ; 1m+1d+6h VAR_TS
  IF data_type NE 'c1m' THEN save_list = [ save_list, 'var_ts', 'ano_ts']
  ; 1m+c1m VAR_SC1m
  IF data_type EQ '1m' OR data_type EQ 'c1m' THEN save_list = [ save_list, 'var_sc1m']
  ; 1d VAR_SC1d
  IF data_type EQ '1d' THEN save_list = [ save_list, 'var_sc1d']
  ; TIME VARIABLES
  IF data_type NE 'c1m' THEN BEGIN
    save_list = [ save_list, 'time_mod', 'nbyear_mod', 'listyear_mod', 'yearini_mod', 'yearend_mod', 'listmonth_mod', 'nbmonth_mod']
  ENDIF
  IF data_type EQ '1d' THEN save_list = [ save_list, 'nbday_mod' , 'ind_mean1d']
  ; OTHER
  save_list = [ save_list, 'landmask', 'seamask', 'highmask', 'grid_path', 'grid_file', 'lon_mod', 'lat_mod']
  IF flag_z THEN BEGIN
    z_mod = gdept
    save_list = [ save_list, 'z_mod']
  ENDIF

  cmd = execute( 'SAVE, '+STRJOIN(save_list,', ', /SINGLE)+', FILENAME = "'+ save_dir + save_file +'", /VERBOSE')
  print, 'SAVE_LIST: ', save_list
  help, save_dir, save_file
  print, 'OK' & print, ''

ENDIF



; INTERPOLATED DATA
IF (STRMATCH( exp_name, 'tr*') OR STRMATCH( exp_name, 'ind*')) AND save_gridobs AND load_gridobs EQ 0 AND e GT 0 THEN BEGIN

  print, 'WRITING INTERPOLATED DATA TO IDL FILE...'

  save_list = !NULL

  IF flag_interp EQ 0 THEN BEGIN
    seamask_gridobs  = seamask
    landmask_gridobs = landmask
    highmask_gridobs = highmask
    var_gridobs      = var
    var_mean_gridobs = var_mean
    var_year_gridobs = var_year
    IF STRMATCH(var_name, 'UV*') OR var_name EQ 'STRESS' OR var_name EQ 'RAIN' THEN BEGIN
      varx_gridobs = varx
      vary_gridobs = varyu
      varx_mean_gridobs = varx_mean
      vary_mean_gridobs = vary_mean
    ENDIF
  ENDIF

  ;IF data_type NE '6h' AND e GT 0 THEN errvar = var_gridobs - var

  save_list = [ save_list, 'seamask_gridobs', 'landmask_gridobs', 'highmask_gridobs']
  save_list = [ save_list, 'var_gridobs', 'var_mean_gridobs']
  IF data_type EQ '1m' THEN save_list = [ save_list, 'var_year_gridobs']
  IF STRMATCH(var_name, 'UV*') OR var_name EQ 'STRESS' OR var_name EQ 'RAIN' THEN BEGIN
    save_list = [ save_list, 'varx_mean_gridobs', 'vary_mean_gridobs']
    save_list = [ save_list, 'varx_gridobs', 'vary_gridobs']
  ENDIF

  cmd = execute( 'SAVE, '+STRJOIN(save_list,', ', /SINGLE)+', FILENAME = "'+ save_dir + gridobs_file +'", /VERBOSE')
  print, 'SAVE_LIST: ', save_list
  help, save_dir, save_file
  print, 'OK' & print, ''

ENDIF


; CLEANING
;tmp = TEMPORARY( var)
;tmp = TEMPORARY( var_mean)
;IF STRMATCH(var_name, 'UV*') OR var_name EQ 'STRESS' OR var_name EQ 'RAIN' THEN BEGIN
;  tmp = TEMPORARY( varx)
;  tmp = TEMPORARY( vary)
;  tmp = TEMPORARY( varx_mean)
;  tmp = TEMPORARY( vary_mean)
;ENDIF
