; SAVE USEFULL TEMPORARY VARIABLES BY ADDING "_n" AT THEIR NAMES
; (n is the experience number)


  print, '' & print, 'SAUVEGARDE...'


  ; c1m+1m+1d VAR
  IF data_type NE '6h' THEN BEGIN
    cmd = execute( 'var_'+strtrim(e,2)+' = var' )
    IF STRMATCH(var_name, 'UV*') OR var_name EQ 'STRESS' OR var_name EQ 'RAIN' THEN BEGIN
      cmd = execute( 'varx_'+strtrim(e,2)+' = varx' )
      cmd = execute( 'vary_'+strtrim(e,2)+' = vary' )
      cmd = execute( 'help, varx_'+strtrim(e,2)+', vary_'+strtrim(e,2) )
    ENDIF
    cmd = execute( 'help, var_'+strtrim(e,2) )
  ENDIF


  ; c1m+1m+1d VAR ON GRIDOBS
  ;IF data_type EQ 'c1m' OR data_type EQ '1m' OR (data_type EQ '1d' AND (fym_only OR fyo_only OR fyc_only)) THEN BEGIN
  IF data_type NE '6h' AND e GT 0 THEN BEGIN
    IF flag_interp THEN BEGIN
      cmd = execute( 'var_'+strtrim(e,2)+'_gridobs = TEMPORARY(var_gridobs)' )
      IF STRMATCH(var_name, 'UV*') OR var_name EQ 'STRESS' OR var_name EQ 'RAIN' THEN BEGIN
        cmd = execute( 'varx_'+strtrim(e,2)+'_gridobs = TEMPORARY(varx_gridobs)' )
        cmd = execute( 'vary_'+strtrim(e,2)+'_gridobs = TEMPORARY(vary_gridobs)' )
        cmd = execute( 'help, varx_'+strtrim(e,2)+'_gridobs, vary_'+strtrim(e,2)+'_gridobs' )
      ENDIF 
    ENDIF ELSE BEGIN
       cmd = execute( 'var_'+strtrim(e,2)+'_gridobs = TEMPORARY(var)' )
      IF STRMATCH(var_name, 'UV*') OR var_name EQ 'STRESS' OR var_name EQ 'RAIN' THEN BEGIN
        cmd = execute( 'varx_'+strtrim(e,2)+'_gridobs = TEMPORARY(varx)' )
        cmd = execute( 'vary_'+strtrim(e,2)+'_gridobs = TEMPORARY(vary)' )
        cmd = execute( 'help, varx_'+strtrim(e,2)+'_gridobs, vary_'+strtrim(e,2)+'_gridobs' )
      ENDIF
    ENDELSE
    cmd = execute( 'help, var_'+strtrim(e,2)+'_gridobs' )
  ENDIF


  ; Z-VAR
  IF flag_z THEN BEGIN
    cmd = execute( 'var_zp_'+strtrim(e,2)+' = TEMPORARY(var_zp)' )
    cmd = execute( 'var_zx_'+strtrim(e,2)+' = TEMPORARY(var_zx)' )
    cmd = execute( 'var_zy_'+strtrim(e,2)+' = TEMPORARY(var_zy)' )
  ENDIF


  ; INTERP_LIST
  IF data_type EQ '1m' THEN BEGIN
    IF e EQ 0 OR flag_interp EQ 0 THEN BEGIN
      FOR l = 0, n_elements( interp_list)-1 DO cmd = execute( interp_list[l]+'_'+strtrim(e,2)+' = TEMPORARY('+interp_list[l]+')' )
    ENDIF
    IF flag_interp THEN BEGIN
      FOR l = 0, n_elements( interp_list)-1 DO cmd = execute( interp_list[l]+'_'+strtrim(e,2)+' = TEMPORARY('+interp_list[l]+'_gridobs)' )
    ENDIF
    FOR l = 0, n_elements( interp_list)-1 DO cmd = execute( 'help, '+interp_list[l]+'_'+strtrim(e,2) )
  ENDIF


  IF data_type EQ '1m' OR data_type EQ 'c1m' OR data_type EQ '1d' OR data_type EQ 'c1d' $
  ;OR (data_type EQ '1d' AND (fym_only OR fyo_only OR fyc_only) ) $
  OR (data_type EQ '6h' AND (fym_only OR fyo_only OR fyc_only) ) THEN BEGIN

    ; VAR_MEAN ON GRID OBS
    IF e GT 0 THEN BEGIN
      IF flag_interp THEN BEGIN
        cmd = execute( 'var_mean_'+strtrim(e,2)+'_gridobs = TEMPORARY(var_mean_gridobs)' )
        cmd = execute( 'ano_mean_'+strtrim(e,2)+'_gridobs = TEMPORARY(ano_mean_gridobs)' )
        IF STRMATCH(var_name, 'UV*') OR var_name EQ 'STRESS' OR var_name EQ 'RAIN' THEN BEGIN
          cmd = execute( 'varx_mean_'+strtrim(e,2)+'_gridobs = TEMPORARY(varx_mean_gridobs)' )
          cmd = execute( 'vary_mean_'+strtrim(e,2)+'_gridobs = TEMPORARY(vary_mean_gridobs)' )
          cmd = execute( 'help, varx_mean_'+strtrim(e,2)+'_gridobs, vary_mean_'+strtrim(e,2)+'_gridobs' )
        ENDIF
      ENDIF ELSE BEGIN
        cmd = execute( 'var_mean_'+strtrim(e,2)+'_gridobs = var_mean' )
        cmd = execute( 'ano_mean_'+strtrim(e,2)+'_gridobs = ano_mean' )
        IF STRMATCH(var_name, 'UV*') OR var_name EQ 'STRESS' OR var_name EQ 'RAIN' THEN BEGIN
          cmd = execute( 'varx_mean_'+strtrim(e,2)+'_gridobs = varx_mean' )
          cmd = execute( 'vary_mean_'+strtrim(e,2)+'_gridobs = vary_mean' )
          cmd = execute( 'help, varx_mean_'+strtrim(e,2)+'_gridobs, vary_mean_'+strtrim(e,2)+'_gridobs' )
        ENDIF
      ENDELSE
      cmd = execute( 'help, var_mean_'+strtrim(e,2)+'_gridobs, ano_mean_'+strtrim(e,2)+'_gridobs' )
    ENDIF

    ; VAR_MEAN
    ;IF e EQ 0 THEN BEGIN
      cmd = execute( 'var_mean_'+strtrim(e,2)+' = TEMPORARY(var_mean)' )
      cmd = execute( 'ano_mean_'+strtrim(e,2)+' = TEMPORARY(ano_mean)' )
      IF STRMATCH(var_name, 'UV*') OR var_name EQ 'STRESS' OR var_name EQ 'RAIN' THEN BEGIN
        cmd = execute( 'varx_mean_'+strtrim(e,2)+' = TEMPORARY(varx_mean)' )
        cmd = execute( 'vary_mean_'+strtrim(e,2)+' = TEMPORARY(vary_mean)' )
        cmd = execute( 'help, varx_mean_'+strtrim(e,2)+', vary_mean_'+strtrim(e,2) )
      ENDIF
      cmd = execute( 'help, var_mean_'+strtrim(e,2)+', ano_mean_'+strtrim(e,2) )
    ;ENDIF

    ; VAR_MEAN_Z
    IF flag_z THEN BEGIN
      cmd = execute( 'var_zp_mean_'+strtrim(e,2)+' = TEMPORARY(var_zp_mean)' )
      cmd = execute( 'var_zx_mean_'+strtrim(e,2)+' = TEMPORARY(var_zx_mean)' )
      cmd = execute( 'var_zy_mean_'+strtrim(e,2)+' = TEMPORARY(var_zy_mean)' )
    ENDIF

  ENDIF


  ; TS VARIABLES
  IF data_type NE 'c1m' THEN BEGIN
    cmd = execute( 'var_ts_'+strtrim(e,2)+' = TEMPORARY(var_ts)' )
    cmd = execute( 'ano_ts_'+strtrim(e,2)+' = TEMPORARY(ano_ts)' )
    cmd = execute( 'help, var_ts_'+strtrim(e,2)+', ano_ts_'+strtrim(e,2) )
  ENDIF
  IF data_type EQ '1m' THEN BEGIN
    cmd = execute( 'var_ts1y_'+strtrim(e,2)+' = TEMPORARY(var_ts1y)' )
    cmd = execute( 'ano_ts1y_'+strtrim(e,2)+' = TEMPORARY(ano_ts1y)' )
    cmd = execute( 'ano_sc1m_ts1m_'+strtrim(e,2)+' = TEMPORARY(ano_sc1m_ts1m)' )
    cmd = execute( 'ano_sc1m_ts1y_'+strtrim(e,2)+' = TEMPORARY(ano_sc1m_ts1y)' )
    cmd = execute( 'help, var_ts1y_'+strtrim(e,2)+', ano_ts1y_'+strtrim(e,2)+', ano_sc1m_ts1m_'+strtrim(e,2)+', ano_sc1m_ts1y_'+strtrim(e,2) )
  END


  ; VAR + STDDEV SC1M
  IF data_type EQ '1m' OR data_type EQ 'c1m' THEN BEGIN
    cmd = execute( 'var_sc1m_'+strtrim(e,2)+'  = TEMPORARY(var_sc1m)' )
    cmd = execute( 'var_sdc1m_'+strtrim(e,2)+' = TEMPORARY(var_sdc1m)' )
    cmd = execute( 'std_ano_sc1m_'+strtrim(e,2)+' = TEMPORARY(std_ano_sc1m)' )
    cmd = execute( 'help, var_sc1m_'+strtrim(e,2)+', var_sdc1m_'+strtrim(e,2)+', std_ano_sc1m_'+strtrim(e,2))
  ENDIF


  ; VAR SC1D
  IF data_type EQ '1d' THEN BEGIN
    cmd = execute( 'var_sc1d_'+strtrim(e,2)+' = TEMPORARY(var_sc1d)' )
    cmd = execute( 'help, var_sc1d_'+strtrim(e,2) )
  ENDIF


  ; TIME VARIABLES
  ;IF data_type NE 'c1m' THEN BEGIN
    IF (STRCMP(exp_name, 'tr', 2) OR STRCMP(exp_name, 'ind', 3)) THEN BEGIN
      cmd = execute( 'time_'+strtrim(e,2)+' = time_mod' )
      cmd = execute( 'nbyear_'+strtrim(e,2)+' = nbyear_mod' )
      cmd = execute( 'listyear_'+strtrim(e,2)+' = listyear_mod' )
      cmd = execute( 'yearini_'+strtrim(e,2)+' = yearini_mod' )
      cmd = execute( 'yearend_'+strtrim(e,2)+' = yearend_mod' )
      cmd = execute( 'listmonth_'+strtrim(e,2)+' = listmonth_mod' )
      cmd = execute( 'nbmonth_'+strtrim(e,2)+' = nbmonth_mod' )
    ENDIF ELSE BEGIN
      cmd = execute( 'time_'+strtrim(e,2)+' = time_obs' )
      cmd = execute( 'nbyear_'+strtrim(e,2)+' = nbyear_obs' )
      cmd = execute( 'listyear_'+strtrim(e,2)+' = listyear_obs' )
      cmd = execute( 'yearini_'+strtrim(e,2)+' = yearini_obs' )
      cmd = execute( 'yearend_'+strtrim(e,2)+' = yearend_obs' )
      cmd = execute( 'listmonth_'+strtrim(e,2)+' = listmonth_obs' )
      cmd = execute( 'nbmonth_'+strtrim(e,2)+' = nbmonth_obs' )
    ENDELSE

  ;ENDIF
  IF data_type EQ '1d' THEN BEGIN
    IF (STRCMP(exp_name, 'tr', 2) OR STRCMP(exp_name, 'ind', 3)) THEN cmd = execute( 'nbday_'+strtrim(e,2)+' = nbday_mod' )
    cmd = execute( 'ind_mean1d_'+strtrim(e,2)+' = ind_mean1d' )
  ENDIF
  IF data_type EQ '1m' THEN cmd = execute( 'ind_mean1m_'+strtrim(e,2)+' = ind_mean1m' )


  ; MASKS
  IF flag_mask THEN BEGIN

    cmd = execute( 'landmask_'+strtrim(e,2)+' = landmask' )
    cmd = execute( 'seamask_'+strtrim(e,2)+' = seamask' )
    cmd = execute( 'highmask_'+strtrim(e,2)+' = highmask' )

    IF e GT 0 AND flag_interp THEN BEGIN
      cmd = execute( 'landmask_'+strtrim(e,2)+'_gridobs = landmask_gridobs' )
      cmd = execute( 'seamask_'+strtrim(e,2)+'_gridobs  = seamask_gridobs' )
      cmd = execute( 'highmask_'+strtrim(e,2)+'_gridobs = highmask_gridobs' )
    ENDIF

  ENDIF ; MASKS

  IF STRCMP(exp_name, 'tr', 2) OR STRCMP(exp_name, 'ind', 3) THEN BEGIN
    cmd = execute( 'path_'+strtrim(e,2)+' = grid_path' )
    cmd = execute( 'file_'+strtrim(e,2)+' = grid_file' )
  ENDIF ELSE BEGIN
    cmd = execute( 'path_'+strtrim(e,2)+' = path' )
    cmd = execute( 'file_'+strtrim(e,2)+' = file' )
  ENDELSE
  cmd = execute( 'lon_'+strtrim(e,2)+' = glamt[firstxt:lastxt,0]' )
  cmd = execute( 'lat_'+strtrim(e,2)+' = REFORM(gphit[0,firstyt:lastyt])' )
  IF flag_z THEN cmd = execute( 'z_'+strtrim(e,2)+' = gdept' )


  print, 'SAUVEGARDE OK' & print, ''
  IF debug THEN STOP
