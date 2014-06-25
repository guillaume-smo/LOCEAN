
;-------------------------------------------------------------------------------------------------
; READ OBS NETCDF DATA FILES & COMPUTE DIAGS IF NECESSARY
;-------------------------------------------------------------------------------------------------

  flag_c1 = (data_type EQ 'c1m' OR data_type EQ 'c1d')

  CASE var_name OF

    'HEIGHT': BEGIN
       IF data_type EQ 'c1m' THEN var = REPLICATE_ARRAY( read_ncdf( 'z', filename=path+'HEIGHT_ERAI.nc', /ALLRECORDS, /NOSTRUCT) / 9.81 + 2., 12) ; m
     END

    'UV10Q2': BEGIN
      q2   = read_ncdf( 'Q2', dates[0], dates[1], filename=path+'Q2_'+data_type+'_'+obs_name+'_1989-2011.nc', TIMESTEP=flag_c1, /NOSTRUCT)*1000.
      varx = read_ncdf('u10', dates[0], dates[1], filename=path+file, TIMESTEP=flag_c1, /NOSTRUCT)*q2
      vary = read_ncdf('v10', dates[0], dates[1], filename=path+file, TIMESTEP=flag_c1, /NOSTRUCT)*q2
      var  = SQRT(varx^2 + vary^2)
    END

    'ROT_UV10': BEGIN
      IF obs_name EQ 'CERSAT-QSCAT' THEN BEGIN
        ;var  = read_ncdf('wind_speed', filename=path+file, /ALLRECORDS, /NOSTRUCT)
        varx = read_ncdf('zonal_wind_speed', dates[0], dates[1], filename=path+file, TIMESTEP=flag_c1, /NOSTRUCT)
        vary = read_ncdf('meridional_wind_speed', dates[0], dates[1], filename=path+file, TIMESTEP=flag_c1, /NOSTRUCT)
      ENDIF
      IF obs_name EQ 'ERAI' THEN BEGIN
        varx = read_ncdf('u10', dates[0], dates[1], filename=path+file, TIMESTEP=flag_c1, /NOSTRUCT)
        vary = read_ncdf('v10', dates[0], dates[1], filename=path+file, TIMESTEP=flag_c1, /NOSTRUCT)
      ENDIF
      IF obs_name EQ 'TROPFLUX' THEN STOP
      var = curl( smooth( varx, 1, /NAN), smooth( vary, 1, /NAN), /MILLION)
    END

    'UV10': BEGIN
      IF obs_name EQ 'CERSAT-QSCAT' THEN BEGIN
        ;var  = read_ncdf('wind_speed', filename=path+file, /ALLRECORDS, /NOSTRUCT)
        varx = read_ncdf('zonal_wind_speed', dates[0], dates[1], filename=path+file, TIMESTEP=flag_c1, /NOSTRUCT)
        vary = read_ncdf('meridional_wind_speed', dates[0], dates[1], filename=path+file, TIMESTEP=flag_c1, /NOSTRUCT)
        var  = SQRT(varx^2 + vary^2)
      ENDIF
      IF obs_name EQ 'ERAI' THEN BEGIN
        varx = read_ncdf('u10', dates[0], dates[1], filename=path+file, TIMESTEP=flag_c1, /NOSTRUCT)
        vary = read_ncdf('v10', dates[0], dates[1], filename=path+file, TIMESTEP=flag_c1, /NOSTRUCT)
        var  = SQRT(varx^2 + vary^2)
      ENDIF
      IF obs_name EQ 'TROPFLUX' THEN BEGIN
        var  = read_ncdf('ws', dates[0], dates[1], filename=path+file, TIMESTEP=flag_c1, /NOSTRUCT)
        varx = var * 0.
        vary = var * 0.
      ENDIF
      help, varx, vary
    END

    'UV': BEGIN
      IF obs_name EQ 'ERAI' THEN BEGIN
        varx = read_ncdf('u', dates[0], dates[1], filename=path+file, TIMESTEP=flag_c1, /NOSTRUCT)
        vary = read_ncdf('v', dates[0], dates[1], filename=path+file, TIMESTEP=flag_c1, /NOSTRUCT)
        var  = SQRT(varx^2 + vary^2)
        psfc = read_ncdf(    'sp', dates[0], dates[1], filename=path+'PSFC_'+data_type+'_'+obs_name+'_1989-2009.nc', TIMESTEP=flag_c1, /NOSTRUCT)/100.
        FOR l = 0, jpt-1 DO BEGIN
          FOR j = 0, nyt-1 DO BEGIN
            FOR i = 0, nxt-1 DO BEGIN
              kbad = WHERE( gdept GT psfc[i,j,l] )
              IF kbad[0] NE -1 THEN BEGIN
                var[i,j,kbad,l]  = !VALUES.F_NAN
                varx[i,j,kbad,l] = !VALUES.F_NAN
                vary[i,j,kbad,l] = !VALUES.F_NAN
              ENDIF
            ENDFOR
          ENDFOR
        ENDFOR
      ENDIF ELSE STOP
      help, TEMPORARY(psfc)
      help, varx, vary
    END

    'STRESS': BEGIN
;      var  = read_ncdf('wind_stress', filename=path+file, /ALLRECORDS, /NOSTRUCT)
      varx = read_ncdf('zonal_wind_stress', filename=path+file, /ALLRECORDS, /NOSTRUCT)
      vary = read_ncdf('meridional_wind_stress', filename=path+file, /ALLRECORDS, /NOSTRUCT)
      var  = SQRT(varx^2 + vary^2)
      help, varx, vary
    END

    'U10': BEGIN
      IF obs_name EQ 'CERSAT-QSCAT' THEN var = read_ncdf('zonal_wind_speed', dates[0], dates[1], filename=path+file, TIMESTEP=flag_c1, /NOSTRUCT)
      IF obs_name EQ 'ERAI'         THEN var = read_ncdf('u10', dates[0], dates[1], filename=path+file, TIMESTEP=flag_c1, /NOSTRUCT)
      IF obs_name EQ 'TROPFLUX'     THEN STOP
    END

    'RAIN': BEGIN
      ;IF exp_name EQ 'GPCP' THEN var = (reverse(ncdf_lec(path+file, VAR=var_name),2))[firstxt:lastxt,firstyt:lastyt,*] $
      var  = read_ncdf(var_file, dates[0], dates[1], filename=path+file, TIMESTEP=flag_c1, /NOSTRUCT)
      varx = var ;* 0.
      vary = var ;* 0.
    END

    'SWDOWN': BEGIN
      var = read_ncdf('GSW', dates[0], dates[1], filename=path+'GSW_'+data_type+'_ERAI_1989-2009.nc', TIMESTEP=flag_c1, /NOSTRUCT) $
          / (1 - read_ncdf('ALBEDO', dates[0], dates[1], filename=path+'ALBEDO_'+data_type+'_ERAI_1989-2009.nc', TIMESTEP=flag_c1, /NOSTRUCT))
    END

    ;'GLW': BEGIN
    ;  var = read_ncdf('LWR', dates[0], dates[1], filename=path+'LWR_'+data_type+'_ERAI_1989-2009.nc', TIMESTEP=flag_c1, /NOSTRUCT) $
    ;      + emiss * stebolt * read_ncdf('SKT', dates[0], dates[1], filename=path+'SKT_'+data_type+'_ERAI_1989-2009.nc', TIMESTEP=flag_c1, /NOSTRUCT)^4
    ;END

    ; GEOPOTENTIEL m2/s2 -> m
    'GHT': BEGIN
      IF obs_name EQ 'ERAI' THEN BEGIN
        var  = read_ncdf(var_file, dates[0], dates[1], filename=path+file, TIMESTEP=flag_c1, /NOSTRUCT) / g
        psfc = read_ncdf(    'sp', dates[0], dates[1], filename=path+'PSFC_'+data_type+'_'+obs_name+'_1989-2009.nc', TIMESTEP=flag_c1, /NOSTRUCT) / 100. ; Pa->hPa
        FOR l = 0, jpt-1 DO BEGIN
          FOR j = 0, nyt-1 DO BEGIN
            FOR i = 0, nxt-1 DO BEGIN
              kbad = WHERE( gdept GT psfc[i,j,l] )
              IF kbad[0] NE -1 THEN var[i,j,kbad,l]  = !VALUES.F_NAN
            ENDFOR
          ENDFOR
        ENDFOR
      ENDIF
    END
    
    'Q2': BEGIN
      IF (data_type EQ '6h' OR data_type EQ '1d') AND obs_name EQ 'ERAI' THEN BEGIN
        tmp1 = read_ncdf('sp', dates[0], dates[1], filename=path+'PSFC_'+data_type+'_'+obs_name+'_'+STRTRIM(year,2)+'.nc', TIMESTEP=flag_c1, /NOSTRUCT)/100. ; Pa->mb
        tmp2 = read_ncdf('d2', dates[0], dates[1], filename=path+'D2_'+data_type+'_'+obs_name+'_'+STRTRIM(year,2)+'.nc', TIMESTEP=flag_c1, /NOSTRUCT)-273.15 ; K->degC
        ew = 6.112*exp((17.67*tmp2)/(tmp2 + 243.5))
        var = (0.622 * ew)/(TEMPORARY(tmp1) - (0.378 * ew))
        help, TEMPORARY(tmp2), TEMPORARY(ew)
      ENDIF ELSE var = read_ncdf(var_file, dates[0], dates[1], filename=path+file, TIMESTEP=flag_c1, /NOSTRUCT)
    END

    'MSLP': BEGIN
      IF data_type EQ 'c1m' OR data_type EQ '1m' THEN BEGIN
        p_sfc  = read_ncdf('sp', dates[0], dates[1], filename=path+'PSFC_'+data_type+'_'+obs_name+'_1989-2009.nc', TIMESTEP=flag_c1, /NOSTRUCT)/100. ; Pa->mb
        hgt    = REPLICATE_ARRAY(read_ncdf('z', filename=path+'HEIGHT_ERAI.nc', /ALLRECORDS, /NOSTRUCT) / 9.81 + 2., 12) ; m
        t_sfc  = read_ncdf('T2', dates[0], dates[1], filename=path+'T2_'+data_type+'_ERAI_1989-2011.nc', TIMESTEP=flag_c1, /NOSTRUCT) ; K
        q_sfc  = read_ncdf('Q2', dates[0], dates[1], filename=path+'Q2_'+data_type+'_ERAI_1989-2011.nc', TIMESTEP=flag_c1, /NOSTRUCT) ; kg/kg
        tv_sfc = t_sfc*(1.+0.608*q_sfc) ; virtual temperature (K)
        tv_msl = tv_sfc + gamma * hgt
        var    = p_sfc * exp( hgt / (rd/g * (tv_sfc+tv_msl)/2. ) )
        help, p_sfc, hgt, t_sfc, q_sfc, var
      ENDIF
    END

    'GRAD_MSLP': BEGIN
      IF data_type EQ 'c1m' OR data_type EQ '1m' THEN BEGIN
        p_sfc  = read_ncdf('sp', dates[0], dates[1], filename=path+'PSFC_'+data_type+'_'+obs_name+'_1989-2009.nc', TIMESTEP=flag_c1, /NOSTRUCT)/100. ; Pa->mb
        hgt    = REPLICATE_ARRAY(read_ncdf('z', filename=path+'HEIGHT_ERAI.nc', /ALLRECORDS, /NOSTRUCT) / 9.81 + 2., 12) ; m
        t_sfc  = read_ncdf('T2', dates[0], dates[1], filename=path+'T2_'+data_type+'_ERAI_1989-2011.nc', TIMESTEP=flag_c1, /NOSTRUCT) ; K
        q_sfc  = read_ncdf('Q2', dates[0], dates[1], filename=path+'Q2_'+data_type+'_ERAI_1989-2011.nc', TIMESTEP=flag_c1, /NOSTRUCT) ; kg/kg
        tv_sfc = t_sfc*(1.+0.608*q_sfc) ; virtual temperature (K)
        tv_msl = tv_sfc + gamma * hgt
        var    = p_sfc * exp( hgt / (rd/g * (tv_sfc+tv_msl)/2. ) )
        var    = NORM( GRAD( var, "x", /MILLION), GRAD( var, "y", /MILLION) )
        help, p_sfc, hgt, t_sfc, q_sfc, var
        ;hgt_0 = hgt
        ;lon_0 = glamt[firstxt:lastxt,firstyt:lastyt]
        ;lat_0 = gphit[firstxt:lastxt,firstyt:lastyt]
        ;path_0 = path
        ;file_0 = file
      ENDIF
    END

    ; calcul theta-e d'apres Bolton 1980 et Holland 1997
    'THETAE': BEGIN
      IF data_type EQ '6h' OR data_type EQ '1d' THEN BEGIN
        ; potential temperature at 2m (K)
        psfc  = read_ncdf('sp', dates[0], dates[1], filename=path+'PSFC_'+data_type+'_'+obs_name+'_'+STRTRIM(year,2)+'.nc', TIMESTEP=flag_c1, /NOSTRUCT)/100. ; Pa->mb
        t2    = read_ncdf('t2', dates[0], dates[1], filename=path+'T2_'+data_type+'_'+obs_name+'_'+STRTRIM(year,2)+'.nc', TIMESTEP=flag_c1, /NOSTRUCT) ; K
        tpot2 = TEMPORARY(t2) * ( 1000. / psfc )^(2./7.) ; K
        ; vapor pressure (mb) from dew point temperature (degC) at 2m
        d2    = read_ncdf('d2', dates[0], dates[1], filename=path+'D2_'+data_type+'_'+obs_name+'_'+STRTRIM(year,2)+'.nc', TIMESTEP=flag_c1, /NOSTRUCT)-273.15 ; K->degC
        e2    = 6.112 * exp( (17.67 * d2) / (TEMPORARY(d2) + 243.5) )
        ; specific humidity at 2m (kg/kg)
        q2    = (0.622 * e2)/(TEMPORARY(psfc) - (0.378 * e2))
        ; temperature at the lifting condensation level (K)
        tl    = (2840 / (3.5 * alog(tpot2[*,*,*,0]) - alog(TEMPORARY(e2)) - 4.805)) + 55
        ; equivalent potential temperature at 2m (K)
        te2   = TEMPORARY(tpot2) * exp( (3.376 / TEMPORARY(tl) - 0.00254) * 1000 * q2 * (1 + 0.81 * TEMPORARY(q2)) )
        var   = TEMPORARY(te2)
      ENDIF
    END

    'T': BEGIN
      IF obs_name EQ 'ERAI' AND data_type EQ '1m' OR data_type EQ 'c1m' THEN BEGIN
        var  = read_ncdf(var_file, dates[0], dates[1], filename=path+file, TIMESTEP=flag_c1, /NOSTRUCT)
        psfc = read_ncdf(    'sp', dates[0], dates[1], filename=path+'PSFC_'+data_type+'_'+obs_name+'_1989-2009.nc', TIMESTEP=flag_c1, /NOSTRUCT)/100.
        FOR l = 0, jpt-1 DO BEGIN
        FOR j = 0, nyt-1 DO BEGIN
        FOR i = 0, nxt-1 DO BEGIN
          kbad = WHERE( gdept GT psfc[i,j,l] )
          IF kbad[0] NE -1 THEN var[i,j,kbad,l] = !VALUES.F_NAN 
        ENDFOR
        ENDFOR
        ENDFOR
        help, TEMPORARY(psfc)
      ENDIF ELSE STOP
    END

    'Q': BEGIN
      IF obs_name EQ 'ERAI' AND (data_type EQ '1m' OR data_type EQ 'c1m') THEN BEGIN
        var  = read_ncdf(var_file, dates[0], dates[1], filename=path+file, TIMESTEP=flag_c1, /NOSTRUCT)*1000. ; kg/kg -> g/kg
        psfc = read_ncdf(    'sp', dates[0], dates[1], filename=path+'PSFC_'+data_type+'_'+obs_name+'_1989-2009.nc', TIMESTEP=flag_c1, /NOSTRUCT)/100.
        FOR l = 0, jpt-1 DO BEGIN
          FOR j = 0, nyt-1 DO BEGIN
            FOR i = 0, nxt-1 DO BEGIN
              kbad = WHERE( gdept GT psfc[i,j,l] )
              IF kbad[0] NE -1 THEN var[i,j,kbad,l] = !VALUES.F_NAN
            ENDFOR
          ENDFOR
        ENDFOR
      ENDIF ELSE STOP
      help, TEMPORARY(psfc)
    END

    'UVQ': BEGIN
      IF obs_name EQ 'ERAI' AND (data_type EQ '1m' OR data_type EQ 'c1m') THEN BEGIN
        q    = read_ncdf(var_file, dates[0], dates[1], filename=path+file, TIMESTEP=flag_c1, /NOSTRUCT)*1000. ; kg/kg -> g/kg
        psfc = read_ncdf(    'sp', dates[0], dates[1], filename=path+'PSFC_'+data_type+'_'+obs_name+'_1989-2009.nc', TIMESTEP=flag_c1, /NOSTRUCT)/100.
        varx = read_ncdf('u', dates[0], dates[1], filename=path+'uv_'+data_type+'_1989-2009.apmei.GLOBAL_075.nc', TIMESTEP=flag_c1, /NOSTRUCT) ;* q
        vary = read_ncdf('v', dates[0], dates[1], filename=path+'uv_'+data_type+'_1989-2009.apmei.GLOBAL_075.nc', TIMESTEP=flag_c1, /NOSTRUCT) ;* TEMPORARY(q)
        var   = SQRT(varx^2 + vary^2) * TEMPORARY(q)
        FOR l = 0, jpt-1 DO BEGIN
          FOR j = 0, nyt-1 DO BEGIN
            FOR i = 0, nxt-1 DO BEGIN
              kbad = WHERE( gdept GT psfc[i,j,l] )
              IF kbad[0] NE -1 THEN var[i,j,kbad,l]  = !VALUES.F_NAN
              IF kbad[0] NE -1 THEN varx[i,j,kbad,l] = !VALUES.F_NAN
              IF kbad[0] NE -1 THEN vary[i,j,kbad,l] = !VALUES.F_NAN
            ENDFOR
          ENDFOR
        ENDFOR
      ENDIF ELSE STOP
      help, TEMPORARY(psfc)
    END

    'RHOD': BEGIN
      IF obs_name EQ 'ERAI' AND (data_type EQ '1m' OR data_type EQ 'c1m') THEN BEGIN
        psfc = read_ncdf( 'sp', dates[0], dates[1], filename=path+'PSFC_'+data_type+'_'+obs_name+'_1989-2009.nc', TIMESTEP=flag_c1, /NOSTRUCT) / 100.   ; hPa
        t    = read_ncdf( 'ta', dates[0], dates[1], filename=path+'ta_'+data_type+'_1989-2009.apmei.GLOBAL_075.nc', TIMESTEP=flag_c1, /NOSTRUCT)        ; K
        var  = t
        FOR k = 0, nzt-1 DO var[*,*,k,*] = REPLICATE( gdept[k]*100., nxt, nyt, jpt) / ( RD * t[*,*,k,*] ) ; dry air density (kg/m3)
        FOR l = 0, jpt-1 DO BEGIN
          FOR j = 0, nyt-1 DO BEGIN
            FOR i = 0, nxt-1 DO BEGIN
              kbad = WHERE( gdept GT psfc[i,j,l] )
              IF kbad[0] NE -1 THEN var[i,j,kbad,l]  = !VALUES.F_NAN
            ENDFOR
          ENDFOR
        ENDFOR
      ENDIF ELSE STOP
      help, TEMPORARY(psfc), TEMPORARY(t)
    END

    'RHO': BEGIN
      IF obs_name EQ 'ERAI' AND (data_type EQ '1m' OR data_type EQ 'c1m') THEN BEGIN
        psfc = read_ncdf( 'sp', dates[0], dates[1], filename=path+'PSFC_'+data_type+'_'+obs_name+'_1989-2009.nc', TIMESTEP=flag_c1, /NOSTRUCT) / 100.  ; hPa
        t    = read_ncdf( 'ta', dates[0], dates[1], filename=path+'ta_'+data_type+'_1989-2009.apmei.GLOBAL_075.nc', TIMESTEP=flag_c1, /NOSTRUCT)       ; K
        q    = read_ncdf(  'q', dates[0], dates[1], filename=path+ 'q_'+data_type+'_1989-2009.apmei.GLOBAL_075.nc', TIMESTEP=flag_c1, /NOSTRUCT)       ; kg/kg
        var  = t
        ; formula from: http://www.engineeringtoolbox.com/density-air-d_680.html
        FOR k = 0, nzt-1 DO var[*,*,k,*] = REPLICATE( gdept[k]*100., nxt, nyt, jpt) / ( RD * t[*,*,k,*] ) * (1.+q[*,*,k,*]) / (1.+1.609*q[*,*,k,*])
        FOR l = 0, jpt-1 DO BEGIN
          FOR j = 0, nyt-1 DO BEGIN
            FOR i = 0, nxt-1 DO BEGIN
              kbad = WHERE( gdept GT psfc[i,j,l] )
              IF kbad[0] NE -1 THEN var[i,j,kbad,l]  = !VALUES.F_NAN
            ENDFOR
          ENDFOR
        ENDFOR
      ENDIF ELSE STOP
      help, TEMPORARY(psfc), TEMPORARY(t), TEMPORARY(q)
    END

    'W': BEGIN
      IF obs_name EQ 'ERAI' AND (data_type EQ '1m' OR data_type EQ 'c1m') THEN BEGIN
        omega  = read_ncdf(var_file, dates[0], dates[1], filename=path+file, TIMESTEP=flag_c1, /NOSTRUCT) ; Pa/s
        psfc   = read_ncdf(    'sp', dates[0], dates[1], filename=path+'PSFC_'+data_type+'_'+obs_name+'_1989-2009.nc', TIMESTEP=flag_c1, /NOSTRUCT) / 100.   ; hPa
        t      = read_ncdf(    'ta', dates[0], dates[1], filename=path+'ta_'+data_type+'_1989-2009.apmei.GLOBAL_075.nc', TIMESTEP=flag_c1, /NOSTRUCT)        ; K
        rhodry = t
        FOR k = 0, nzt-1 DO rhodry[*,*,k,*] = REPLICATE( gdept[k]*100., nxt, nyt, jpt) / ( RD * t[*,*,k,*] ) ; dry air density (kg/m3)
        var = TEMPORARY(omega) / (-1. * G * TEMPORARY(rhodry)) ; vertical speed (m/s)
        FOR l = 0, jpt-1 DO BEGIN
          FOR j = 0, nyt-1 DO BEGIN
            FOR i = 0, nxt-1 DO BEGIN
              kbad = WHERE( gdept GT psfc[i,j,l] )
              IF kbad[0] NE -1 THEN var[i,j,kbad,l]  = !VALUES.F_NAN
            ENDFOR
          ENDFOR
        ENDFOR
      ENDIF ELSE STOP
      help, TEMPORARY(psfc), TEMPORARY(t)
    END

    'WV': BEGIN
      IF obs_name EQ 'ERAI' AND (data_type EQ '1m' OR data_type EQ 'c1m') THEN BEGIN
        omega  = read_ncdf(var_file, dates[0], dates[1], filename=path+file, TIMESTEP=flag_c1, /NOSTRUCT) ; Pa/s
        psfc   = read_ncdf(    'sp', dates[0], dates[1], filename=path+'PSFC_'+data_type+'_'+obs_name+'_1989-2009.nc', TIMESTEP=flag_c1, /NOSTRUCT) / 100.   ; hPa
        t      = read_ncdf(    'ta', dates[0], dates[1], filename=path+'ta_'+data_type+'_1989-2009.apmei.GLOBAL_075.nc', TIMESTEP=flag_c1, /NOSTRUCT)        ; K
        rhodry = t
        FOR k = 0, nzt-1 DO rhodry[*,*,k,*] = REPLICATE( gdept[k]*100., nxt, nyt, jpt) / ( RD * t[*,*,k,*] ) ; dry air density (kg/m3)
        varx = TEMPORARY(omega) / (-1. * G * TEMPORARY(rhodry)) ; vertical speed (m/s)
        vary = read_ncdf('v', dates[0], dates[1], filename=path+file, TIMESTEP=flag_c1, /NOSTRUCT)
        FOR l = 0, jpt-1 DO BEGIN
          FOR j = 0, nyt-1 DO BEGIN
            FOR i = 0, nxt-1 DO BEGIN
              kbad = WHERE( gdept GT psfc[i,j,l] )
              IF kbad[0] NE -1 THEN var[i,j,kbad,l]  = !VALUES.F_NAN
            ENDFOR
          ENDFOR
        ENDFOR
      ENDIF ELSE STOP
      help, TEMPORARY(psfc), TEMPORARY(t)
    END

    'WU': BEGIN
      IF obs_name EQ 'ERAI' AND (data_type EQ '1m' OR data_type EQ 'c1m') THEN BEGIN
        omega  = read_ncdf(var_file, dates[0], dates[1], filename=path+file, TIMESTEP=flag_c1, /NOSTRUCT) ; Pa/s
        psfc   = read_ncdf(    'sp', dates[0], dates[1], filename=path+'PSFC_'+data_type+'_'+obs_name+'_1989-2009.nc', TIMESTEP=flag_c1, /NOSTRUCT) / 100.   ; hPa
        t      = read_ncdf(    'ta', dates[0], dates[1], filename=path+'ta_'+data_type+'_1989-2009.apmei.GLOBAL_075.nc', TIMESTEP=flag_c1, /NOSTRUCT)        ; K
        rhodry = t
        FOR k = 0, nzt-1 DO rhodry[*,*,k,*] = REPLICATE( gdept[k]*100., nxt, nyt, jpt) / ( RD * t[*,*,k,*] ) ; dry air density (kg/m3)
        var = TEMPORARY(omega) / (-1. * G * TEMPORARY(rhodry)) ; vertical speed (m/s)
        FOR l = 0, jpt-1 DO BEGIN
          FOR j = 0, nyt-1 DO BEGIN
            FOR i = 0, nxt-1 DO BEGIN
              kbad = WHERE( gdept GT psfc[i,j,l] )
              IF kbad[0] NE -1 THEN var[i,j,kbad,l]  = !VALUES.F_NAN
            ENDFOR
          ENDFOR
        ENDFOR
      ENDIF ELSE STOP
      help, TEMPORARY(psfc), TEMPORARY(t)
    END

    'WQ': BEGIN
      IF obs_name EQ 'ERAI' AND (data_type EQ '1m' OR data_type EQ 'c1m') THEN BEGIN
        q      = read_ncdf(var_file, dates[0], dates[1], filename=path+file, TIMESTEP=flag_c1, /NOSTRUCT) * 1000.                                 ; kg/kg -> g/kg
        psfc   = read_ncdf(    'sp', dates[0], dates[1], filename=path+'PSFC_'+data_type+'_'+obs_name+'_1989-2009.nc', TIMESTEP=flag_c1, /NOSTRUCT) / 100.   ; hPa
        omega  = read_ncdf(     'w', dates[0], dates[1], filename=path+'w_'+data_type+'_1989-2009.apmei.GLOBAL_075.nc', TIMESTEP=flag_c1, /NOSTRUCT)         ; Pa/s
        t      = read_ncdf(    'ta', dates[0], dates[1], filename=path+'ta_'+data_type+'_1989-2009.apmei.GLOBAL_075.nc', TIMESTEP=flag_c1, /NOSTRUCT)        ; K
        rhodry = t
        FOR k = 0, nzt-1 DO rhodry[*,*,k,*] = REPLICATE( gdept[k]*100., nxt, nyt, jpt) / ( RD * t[*,*,k,*] ) ; dry air density (kg / m3)
        w      = TEMPORARY(omega) / (-1. * G * TEMPORARY(rhodry))
        var    = TEMPORARY(w) * TEMPORARY(q)
        FOR l = 0, jpt-1 DO BEGIN
          FOR j = 0, nyt-1 DO BEGIN
            FOR i = 0, nxt-1 DO BEGIN
              kbad = WHERE( gdept GT psfc[i,j,l] )
              IF kbad[0] NE -1 THEN var[i,j,kbad,l]  = !VALUES.F_NAN
            ENDFOR
          ENDFOR
        ENDFOR
      ENDIF ELSE STOP
      help, TEMPORARY(psfc), TEMPORARY(t)
    END

    'RH': BEGIN
      IF obs_name EQ 'ERAI' AND (data_type EQ '1m' OR data_type EQ 'c1m') THEN BEGIN
        var  = read_ncdf(var_file, dates[0], dates[1], filename=path+file, TIMESTEP=flag_c1, /NOSTRUCT)
        psfc = read_ncdf(    'sp', dates[0], dates[1], filename=path+'PSFC_'+data_type+'_'+obs_name+'_1989-2009.nc', TIMESTEP=flag_c1, /NOSTRUCT)/100.
        FOR l = 0, jpt-1 DO BEGIN
          FOR j = 0, nyt-1 DO BEGIN
            FOR i = 0, nxt-1 DO BEGIN
              kbad = WHERE( gdept GT psfc[i,j,l] )
              IF kbad[0] NE -1 THEN var[i,j,kbad,l] = !VALUES.F_NAN
            ENDFOR
          ENDFOR
        ENDFOR
      ENDIF ELSE STOP
      help, TEMPORARY(psfc)
    END

    'TPOT': BEGIN
      IF obs_name EQ 'ERAI' AND data_type EQ '1m' OR data_type EQ 'c1m' THEN BEGIN
        var  = read_ncdf(var_file, dates[0], dates[1], filename=path+file, TIMESTEP=flag_c1, /NOSTRUCT)
        psfc = read_ncdf(    'sp', dates[0], dates[1], filename=path+'PSFC_'+data_type+'_'+obs_name+'_1989-2009.nc', TIMESTEP=flag_c1, /NOSTRUCT)/100.
        FOR l = 0, jpt-1 DO BEGIN
          FOR j = 0, nyt-1 DO BEGIN
            FOR i = 0, nxt-1 DO BEGIN
              kbad = WHERE( gdept GT psfc[i,j,l] )
              IF kbad[0] NE -1 THEN var[i,j,kbad,l] = !VALUES.F_NAN
            ENDFOR
          ENDFOR
        ENDFOR
        FOR k = 0, nzt-1 DO var[*,*,k,*] = var[*,*,k,*] * ( 1000. / replicate( gdept[k], nxt, nyt, jpt))^(2./7. )
      ENDIF ELSE STOP
      help, TEMPORARY(psfc)
    END

    'TPOTE': BEGIN
      IF obs_name EQ 'ERAI' AND (data_type EQ '1m' OR data_type EQ 'c1m') THEN BEGIN

        ; potential temperature (K)
        print, path+'PSFC_'+data_type+'_'+obs_name+'_'+STRTRIM(yearini_obs,2)+'-'+STRTRIM(yearend_obs,2)+'.nc'
        psfc = read_ncdf( 'sp', dates[0], dates[1], filename=path+'PSFC_'+data_type+'_'+obs_name+'_'+STRTRIM(yearini_obs,2)+'-*.nc', TIMESTEP=flag_c1, /NOSTRUCT)/100. ; Pa->mb
        print, path+   'T_'+data_type+'_'+obs_name+'_'+STRTRIM(yearini_obs,2)+'-'+STRTRIM(yearend_obs,2)+'.nc'
        tpot = read_ncdf( 'ta', dates[0], dates[1], filename=path+   'T_'+data_type+'_'+obs_name+'_'+STRTRIM(yearini_obs,2)+'-*.nc', TIMESTEP=flag_c1, /NOSTRUCT) ; K
        FOR l = 0, jpt-1 DO BEGIN
          FOR j = 0, nyt-1 DO BEGIN
            FOR i = 0, nxt-1 DO BEGIN
              kbad = WHERE( gdept GT psfc[i,j,l] )
              IF kbad[0] NE -1 THEN tpot[i,j,kbad,l] = !VALUES.F_NAN
            ENDFOR
          ENDFOR
        ENDFOR
        FOR k = 0, nzt-1 DO tpot[*,*,k,*] = tpot[*,*,k,*] * ( 1000. / replicate( gdept[k], nxt, nyt, jpt))^(2./7. ) ; K
        help, tpot, TEMPORARY(psfc)

        ; vapor pressure (mb) from specific humidity (kg/kg)
        q = read_ncdf('q', dates[0], dates[1], filename=path+'Q_'+data_type+'_'+obs_name+'_'+STRTRIM(yearini_obs,2)+'-*.nc', TIMESTEP=flag_c1, /NOSTRUCT) ; kg/kg
        ew = q & FOR k = 0, nzt-1 DO ew[*,*,k,*] = replicate( gdept[k], nxt, nyt, jpt) / ( 0.622 / q + 0.378 )
        help, ew, q
        ; temperature at the lifting condensation level (K)
        tl  = (2840 / (3.5 * alog(tpot) - alog(TEMPORARY(ew)) - 4.805)) + 55
        ; equivalent potential temperature (K)
        var = TEMPORARY(tpot) * exp( (3.376 / TEMPORARY(tl) - 0.00254) * 1000 * q * (1 + 0.81 * TEMPORARY(q)) )
        help, var
      ENDIF
    END

    'D20': BEGIN
      IF obs_name NE 'SODA 2.2.4' THEN BEGIN
        thetao = read_ncdf( var_file, dates[0], dates[1], filename=path+file, TIMESTEP=flag_c1, /NOSTRUCT) & help, thetao
        IF obs_name EQ 'GLORYS2V1' THEN thetao = thetao - 273.15
        var    = FLTARR( nxt, nyt, jpt) + !VALUES.F_NAN
        FOR l = 0, jpt-1 DO BEGIN
          FOR j = 0, nyt-1 DO BEGIN
            FOR i = 0, nxt-1 DO BEGIN
              IF FINITE( thetao[i,j,0,l]) EQ 1 THEN BEGIN
                IF (WHERE( thetao[i,j,*,l] LE 20.))[0] NE -1 AND ( WHERE( thetao[i,j,*,l] GE 20.))[0] NE -1 THEN BEGIN
                  kbeg = MAX( WHERE( thetao[i,j,*,l] GE 20.))
                  kend = MIN( WHERE( thetao[i,j,*,l] LE 20.))
                  var[i,j,l] = INTERPOL( [ gdept[kbeg], gdept[kend]], [ thetao[i,j,kbeg,l], thetao[i,j,kend,l]], 20.)
                ENDIF
              ENDIF
            ENDFOR
          ENDFOR
        ENDFOR
        help, var, TEMPORARY(thetao)
      ENDIF ELSE var = REFORM( read_ncdf(var_file, dates[0], dates[1], filename=path+file, TIMESTEP=flag_c1, /NOSTRUCT))
    END

    ELSE: var = REFORM( read_ncdf(var_file, dates[0], dates[1], filename=path+file, TIMESTEP=flag_c1, /NOSTRUCT))

  ENDCASE
