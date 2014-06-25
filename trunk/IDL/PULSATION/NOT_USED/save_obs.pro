      print, '' & print, 'SAUVEGARDE...'


      ; c1m+1m+1d VAR
      ;IF data_type EQ 'c1m' OR data_type EQ '1m' OR (data_type EQ '1d' AND (fym_only OR fyo_only OR fyc_only)) THEN BEGIN
      IF data_type EQ 'c1m' OR data_type EQ '1m' OR data_type EQ '1d' THEN BEGIN
        cmd = execute( 'var_'+strtrim(e,2)+' = TEMPORARY(var)' )
        IF STRMATCH(var_name, 'UV*') OR var_name EQ 'STRESS' OR var_name EQ 'RAIN' THEN BEGIN
          cmd = execute( 'varx_'+strtrim(e,2)+' = TEMPORARY(varx)' )
          cmd = execute( 'vary_'+strtrim(e,2)+' = TEMPORARY(vary)' )
          cmd = execute( 'help, varx_'+strtrim(e,2)+', vary_'+strtrim(e,2) )
        ENDIF
        cmd = execute( 'help, var_'+strtrim(e,2) )
      ENDIF
      IF flag_z THEN BEGIN
        cmd = execute( 'var_zp_'+strtrim(e,2)+' = TEMPORARY(var_zp)' )
        cmd = execute( 'var_zx_'+strtrim(e,2)+' = TEMPORARY(var_zx)' )
        cmd = execute( 'var_zy_'+strtrim(e,2)+' = TEMPORARY(var_zy)' )
      ENDIF
      ; VAR_YEAR
      IF data_type EQ '1m' THEN BEGIN
        cmd = execute( 'var_year_'+strtrim(e,2)+' = TEMPORARY(var_year)' )
        ;IF var_name EQ 'UV10' OR var_name EQ 'STRESS' OR var_name EQ 'RAIN' THEN BEGIN
        ;  cmd = execute( 'varx_year_'+strtrim(e,2)+' = TEMPORARY(varx_year)' )
        ;  cmd = execute( 'vary_year_'+strtrim(e,2)+' = TEMPORARY(vary_year)' )
        ;  cmd = execute( 'help, varx_year_'+strtrim(e,2)+', vary_year_'+strtrim(e,2) )
        ;ENDIF
        cmd = execute( 'help, var_year_'+strtrim(e,2) )
      ENDIF

      ; VAR_MEAN
      ;IF data_type EQ '1m' OR data_type EQ 'c1m' $
      ;OR (data_type EQ '1d' AND (fym_only OR fyo_only OR fyc_only) ) $
      ;OR (data_type EQ '6h' AND (fym_only OR fyo_only OR fyc_only) ) THEN BEGIN
        cmd = execute( 'var_mean_'+strtrim(e,2)+' = TEMPORARY(var_mean)' )
        cmd = execute( 'ano_mean_'+strtrim(e,2)+' = TEMPORARY(ano_mean)' )
        IF STRMATCH(var_name, 'UV*') OR var_name EQ 'STRESS' OR var_name EQ 'RAIN' THEN BEGIN
          cmd = execute( 'varx_mean_'+strtrim(e,2)+' = TEMPORARY(varx_mean)' )
          cmd = execute( 'vary_mean_'+strtrim(e,2)+' = TEMPORARY(vary_mean)' )
          cmd = execute( 'help, varx_mean_'+strtrim(e,2)+', vary_mean_'+strtrim(e,2) )
        ENDIF
        cmd = execute( 'help, var_mean_'+strtrim(e,2)+', ano_mean_'+strtrim(e,2) )
        IF flag_z THEN BEGIN
          cmd = execute( 'var_zp_mean_'+strtrim(e,2)+' = TEMPORARY(var_zp_mean)' )
          cmd = execute( 'var_zx_mean_'+strtrim(e,2)+' = TEMPORARY(var_zx_mean)' )
          cmd = execute( 'var_zy_mean_'+strtrim(e,2)+' = TEMPORARY(var_zy_mean)' )
        ENDIF
      ;ENDIF

      ; VAR_TS
      IF data_type NE 'c1m' THEN BEGIN
        cmd = execute( 'var_ts_'+strtrim(e,2)+' = TEMPORARY(var_ts)' )
        cmd = execute( 'ano_ts_'+strtrim(e,2)+' = TEMPORARY(ano_ts)' )
        cmd = execute( 'help, var_ts_'+strtrim(e,2)+', ano_ts_'+strtrim(e,2) )
      ENDIF

      ; VAR SC1M
      IF data_type EQ '1m' OR data_type EQ 'c1m' THEN BEGIN
        cmd = execute( 'var_sc1m_'+strtrim(e,2)+' = TEMPORARY(var_sc1m)' )
        cmd = execute( 'help, var_sc1m_'+strtrim(e,2) )
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
          cmd = execute( 'listmonth_'+strtrim(e,2)+' = listmonth_obs' )
          cmd = execute( 'yearini_'+strtrim(e,2)+' = yearini_obs' )
          cmd = execute( 'yearend_'+strtrim(e,2)+' = yearend_obs' )
          IF data_type EQ '1m' THEN cmd = execute( 'nbmonth_'+strtrim(e,2)+' = nbmonth_obs' )
        ENDELSE
      ;ENDIF
      IF data_type EQ '1d' THEN BEGIN
        IF (STRCMP(exp_name, 'tr', 2) OR STRCMP(exp_name, 'ind', 3)) THEN cmd = execute( 'nbday_'+strtrim(e,2)+' = nbday_mod' ) $
        ELSE cmd = execute( 'nbday_'+strtrim(e,2)+' = nbday_obs' )
        cmd = execute( 'ind_mean1d_'+strtrim(e,2)+' = ind_mean1d' )
      ENDIF
      IF data_type EQ '1m' THEN cmd = execute( 'ind_mean1m_'+strtrim(e,2)+' = ind_mean1m' )

      ; OTHER
      cmd = execute( 'landmask_'+strtrim(e,2)+' = landmask' )
      cmd = execute( 'seamask_'+strtrim(e,2)+' = seamask' )
      cmd = execute( 'highmask_'+strtrim(e,2)+' = highmask' )
      cmd = execute( 'path_'+strtrim(e,2)+' = path' )
      cmd = execute( 'file_'+strtrim(e,2)+' = file' )
      cmd = execute( 'lon_'+strtrim(e,2)+' = glamt[firstxt:lastxt,0]' )
      cmd = execute( 'lat_'+strtrim(e,2)+' = REFORM(gphit[0,firstyt:lastyt])' )
      IF flag_z THEN cmd = execute( 'z_'+strtrim(e,2)+' = gdept' )

      ; SPECIAL CASE
      IF e GE 1 AND (WHERE(STRCMP(exp_list, 'tr', 2) OR STRCMP(exp_name, 'ind', 3) EQ 1))[0] EQ -1 THEN BEGIN
        ; MEAN
        cmd = execute( 'var_'+strtrim(e,2)+'_gridobs = var_'+strtrim(e,2) )
        cmd = execute( 'var_mean_'+strtrim(e,2)+'_gridobs = var_mean_'+strtrim(e,2) )
        cmd = execute( 'ano_mean_'+strtrim(e,2)+'_gridobs = ano_mean_'+strtrim(e,2) )
        IF STRMATCH(var_name, 'UV*') OR var_name EQ 'STRESS' OR var_name EQ 'RAIN' THEN BEGIN
          cmd = execute( 'varx_'+strtrim(e,2)+'_gridobs = varx_'+strtrim(e,2) )
          cmd = execute( 'vary_'+strtrim(e,2)+'_gridobs = vary_'+strtrim(e,2) )
          cmd = execute( 'varx_mean_'+strtrim(e,2)+'_gridobs = varx_mean_'+strtrim(e,2) )
          cmd = execute( 'vary_mean_'+strtrim(e,2)+'_gridobs = vary_mean_'+strtrim(e,2) )
        ENDIF
      ENDIF

      print, 'OK' & print, ''
      IF debug THEN STOP
