      print, '' & print, 'SAUVEGARDE...'


      ; c1m+1m+1d VAR
      IF data_type EQ 'c1m' OR data_type EQ '1m' OR (data_type EQ '1d' AND (fym_only OR fyo_only OR fyc_only)) THEN BEGIN
        cmd = execute( 'var_'+strtrim(e,2)+' = TEMPORARY(var)' )
        IF var_name EQ 'UV10' OR var_name EQ 'UV' OR var_name EQ 'STRESS' OR var_name EQ 'RAIN' THEN BEGIN
          cmd = execute( 'varx_'+strtrim(e,2)+' = TEMPORARY(varx)' )
          cmd = execute( 'vary_'+strtrim(e,2)+' = TEMPORARY(vary)' )
          cmd = execute( 'help, varx_'+strtrim(e,2)+', vary_'+strtrim(e,2) )
        ENDIF
        cmd = execute( 'help, var_'+strtrim(e,2) )
      ENDIF
      IF flag_z THEN  cmd = execute( 'var_zp_'+strtrim(e,2)+' = TEMPORARY(var_zp)' )

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
      IF data_type EQ '1m' OR data_type EQ 'c1m' $
      OR (data_type EQ '1d' AND (fym_only OR fyo_only OR fyc_only) ) $
      OR (data_type EQ '6h' AND (fym_only OR fyo_only OR fyc_only) ) THEN BEGIN
        cmd = execute( 'var_mean_'+strtrim(e,2)+' = TEMPORARY(var_mean)' )
        IF var_name EQ 'UV10' OR var_name EQ 'UV' OR var_name EQ 'STRESS' OR var_name EQ 'RAIN' THEN BEGIN
          cmd = execute( 'varx_mean_'+strtrim(e,2)+' = TEMPORARY(varx_mean)' )
          cmd = execute( 'vary_mean_'+strtrim(e,2)+' = TEMPORARY(vary_mean)' )
          cmd = execute( 'help, varx_mean_'+strtrim(e,2)+', vary_mean_'+strtrim(e,2) )
        ENDIF
        cmd = execute( 'help, var_mean_'+strtrim(e,2) )
        IF flag_z THEN cmd = execute( 'var_zp_mean_'+strtrim(e,2)+' = TEMPORARY(var_zp_mean)' )
      ENDIF

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

      ; TIME VARIABLES
      IF data_type NE 'c1m' THEN BEGIN
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
      ENDIF
      IF data_type EQ '1d' THEN BEGIN
        IF (STRCMP(exp_name, 'tr', 2) OR STRCMP(exp_name, 'ind', 3)) THEN cmd = execute( 'nbday_'+strtrim(e,2)+' = nbday_mod' ) $
        ELSE cmd = execute( 'nbday_'+strtrim(e,2)+' = nbday_obs' )
        cmd = execute( 'ind_mean1d_'+strtrim(e,2)+' = ind_mean1d' )
      ENDIF

      ; OTHER
      IF STRCMP(exp_name, 'tr', 2) OR STRCMP(exp_name, 'ind', 3) THEN BEGIN
        cmd = execute( 'landmask_'+strtrim(e,2)+' = landmask' )
        cmd = execute( 'seamask_'+strtrim(e,2)+' = seamask' )
        cmd = execute( 'highmask_'+strtrim(e,2)+' = highmask' )
        cmd = execute( 'path_'+strtrim(e,2)+' = grid_path' )
        cmd = execute( 'file_'+strtrim(e,2)+' = grid_file' )
      ENDIF ELSE BEGIN
        cmd = execute( 'path_'+strtrim(e,2)+' = path' )
        cmd = execute( 'file_'+strtrim(e,2)+' = file' )
      ENDELSE
      cmd = execute( 'lon_'+strtrim(e,2)+' = glamt[firstxt:lastxt,0]' )
      cmd = execute( 'lat_'+strtrim(e,2)+' = REFORM(gphit[0,firstyt:lastyt])' )
      IF flag_z THEN cmd = execute( 'z_'+strtrim(e,2)+' = gdept' )

      print, 'OK' & print, ''
      IF debug THEN STOP
