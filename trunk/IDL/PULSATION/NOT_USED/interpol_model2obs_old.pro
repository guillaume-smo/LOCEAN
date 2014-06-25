    print, '' & print, 'INTERPOLATION...'

    
    FOR e = 1, n_elements(exp_list)-1 DO BEGIN

      ; MASK
      cmd = execute( 'seamask_'+strtrim(e,2)+'_gridobs  = fromreg("bilinear",seamask_' +strtrim(e,2)+',lon_'+strtrim(e,2)+',lat_'+strtrim(e,2)+',lon_0,lat_0)' )
      cmd = execute( 'landmask_'+strtrim(e,2)+'_gridobs = fromreg("bilinear",landmask_'+strtrim(e,2)+',lon_'+strtrim(e,2)+',lat_'+strtrim(e,2)+',lon_0,lat_0)' )
      cmd = execute( 'highmask_'+strtrim(e,2)+'_gridobs = fromreg("bilinear",highmask_'+strtrim(e,2)+',lon_'+strtrim(e,2)+',lat_'+strtrim(e,2)+',lon_0,lat_0)' )
      cmd = execute( 'help, seamask_'+strtrim(e,2)+'_gridobs, landmask_'+strtrim(e,2)+'_gridobs, highmask_'+strtrim(e,2)+'_gridobs' )

      ; SEASONAL AVERAGE
      cmd = execute( 'var_mean_'+strtrim(e,2)+'_gridobs = fromreg("bilinear",TEMPORARY(var_mean_'+strtrim(e,2)+'),lon_'+strtrim(e,2)+',lat_'+strtrim(e,2)+',lon_0,lat_0)' )
      cmd = execute( 'help, var_mean_'+strtrim(e,2)+'_gridobs' )
      IF var_name EQ 'UV10' OR var_name EQ 'UV' OR var_name EQ 'STRESS' OR var_name EQ 'RAIN' THEN BEGIN
        cmd = execute( 'varx_mean_'+strtrim(e,2)+'_gridobs = fromreg("bilinear", TEMPORARY(varx_mean_'+strtrim(e,2)+'),lon_'+strtrim(e,2)+',lat_'+strtrim(e,2)+',lon_0,lat_0)' )
        cmd = execute( 'vary_mean_'+strtrim(e,2)+'_gridobs = fromreg("bilinear", TEMPORARY(vary_mean_'+strtrim(e,2)+'),lon_'+strtrim(e,2)+',lat_'+strtrim(e,2)+',lon_0,lat_0)' )
      ENDIF

      ; c1m+1m+1d DATA
      IF data_type EQ 'c1m' OR data_type EQ '1m' OR (data_type EQ '1d' AND (fyc_only OR fym_only OR fyo_only)) THEN BEGIN

        cmd = execute( 'nbmonth = nbmonth_'+strtrim(e,2) )
        IF data_type EQ 'c1m' OR data_type EQ '1m' THEN cmd = execute( 'nt = nbmonth_'+strtrim(e,2) )
        IF data_type EQ '1d' THEN cmd = execute( 'nt = nbday_'+strtrim(e,2) )
        cmd = execute( 'var_'+strtrim(e,2)+'_gridobs = fltarr(n_elements(lon_0), n_elements(lat_0), nt)' )
        cmd = execute( 'errvar_'+strtrim(e,2)+' = fltarr(n_elements(lon_0), n_elements(lat_0), nt)' )
        FOR t = 0, nt-1 DO BEGIN
          cmd = execute( 'var_'+strtrim(e,2)+'_gridobs[*,*,t] = fromreg("bilinear",'+'var_'+strtrim(e,2)+'[*,*,t],lon_'+strtrim(e,2)+',lat_'+strtrim(e,2)+',lon_0,lat_0)' )
          cmd = execute( 'errvar_'+strtrim(e,2)+'[*,*,t] = var_'+strtrim(e,2)+'_gridobs[*,*,t] - var_0[*,*,t]' )
        ENDFOR
        cmd = execute( 'help, var_'+strtrim(e,2)+'_gridobs, errvar_'+strtrim(e,2) )
        cmd = execute( 'tmp = TEMPORARY(var_'+strtrim(e,2)+')' )
        IF var_name EQ 'UV10' OR var_name EQ 'UV' OR var_name EQ 'STRESS' OR var_name EQ 'RAIN' THEN BEGIN
          cmd = execute( 'varx_'+strtrim(e,2)+'_gridobs = fltarr(n_elements(lon_0), n_elements(lat_0), nt)' )
          cmd = execute( 'vary_'+strtrim(e,2)+'_gridobs = fltarr(n_elements(lon_0), n_elements(lat_0), nt)' )
          FOR t = 0, nt-1 DO BEGIN
            cmd = execute( 'varx_'+strtrim(e,2)+'_gridobs[*,*,t] = fromreg("bilinear",'+'varx_'+strtrim(e,2)+'[*,*,t],lon_'+strtrim(e,2)+',lat_'+strtrim(e,2)+',lon_0,lat_0)' )
            cmd = execute( 'vary_'+strtrim(e,2)+'_gridobs[*,*,t] = fromreg("bilinear",'+'vary_'+strtrim(e,2)+'[*,*,t],lon_'+strtrim(e,2)+',lat_'+strtrim(e,2)+',lon_0,lat_0)' )
          ENDFOR
          cmd = execute( 'help, varx_'+strtrim(e,2)+'_gridobs, vary_'+strtrim(e,2)+'_gridobs' )
          cmd = execute( 'tmp = TEMPORARY(varx_'+strtrim(e,2)+')' )
          cmd = execute( 'tmp = TEMPORARY(vary_'+strtrim(e,2)+')' )
        ENDIF

        ; 1m INTERANNUAL SEASONAL
        IF data_type EQ '1m' THEN BEGIN
          cmd = execute( 'var_year_'+strtrim(e,2)+'_gridobs = FLTARR(n_elements(lon_0), n_elements(lat_0), nbyear_'+strtrim(e,2)+')' )
          cmd = execute( 'FOR y = 0, nbyear_'+strtrim(e,2)+' -1 DO var_year_'+strtrim(e,2)+'_gridobs[*,*,y] = fromreg("bilinear",'+'var_year_'+strtrim(e,2)+'[*,*,y],lon_'+strtrim(e,2)+',lat_'+strtrim(e,2)+',lon_0,lat_0)' )
          cmd = execute( 'help, var_year_'+strtrim(e,2)+'_gridobs' )
        ENDIF

      ENDIF ; c1m+1m+1d DATA

    ENDFOR ; EXPS

    print, 'OK' & print, ''
    IF debug THEN STOP
