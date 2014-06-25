
;-------------------------------------------------------------------------------------------------
; READ MODEL NETCDF DATA FILES & COMPUTE DIAGS IF NECESSARY
;-------------------------------------------------------------------------------------------------


;-------------------------------------------------------------------------------------------------
; TIME SETUP & EXCEPTIONS
;-------------------------------------------------------------------------------------------------

help, file

; c1m CASE
IF data_type EQ 'c1m' OR (data_type EQ '1m' AND grid EQ 'trop12') THEN tbeg = 0
IF data_type EQ 'c1m' OR (data_type EQ '1m' AND grid EQ 'trop12') THEN tend = 11 

; 1m CASE
IF data_type EQ  '1m' AND grid NE 'trop12' THEN tbeg = (WHERE( time_modfile GE dates[0]))[0]
IF data_type EQ  '1m' AND grid NE 'trop12' THEN tend = (WHERE( time_modfile LE dates[1]))[-1]

; 1d CASE
;IF data_type EQ '1d' AND (grid EQ 'trop025' OR grid EQ 'trop12') THEN BEGIN
IF data_type EQ '1d' AND grid EQ 'trop12' THEN BEGIN
  tbeg = 0
  tend = nbday_month-1
ENDIF
;IF data_type EQ '1d' AND grid EQ 'trop075' THEN BEGIN
IF data_type EQ '1d' AND (grid EQ 'trop075' OR grid EQ 'trop025' OR grid EQ 'ind025') THEN BEGIN
  tbeg = 0
  tend = nbday_year-1
ENDIF

; EXPS WITH NO MEAN VAR
IF exp_name EQ 'tr025_cam01' OR exp_name EQ 'tr025_kftemf01' OR exp_name EQ 'tr12_bmj01' OR $
   exp_name EQ 'tr12_kf01' OR exp_name EQ 'tr12_kf02' THEN flag_nomean = 1 ELSE flag_nomean = 0 
help, flag_nomean

; RUNNING EXPS...
IF data_type EQ  '1m' $
  AND (exp_name EQ 'tr075_ra12L60_sol094sstnow' $
  OR   exp_name EQ 'tr075_ra12L60_sol094sstclim') THEN BEGIN
  tbeg = 0
  tend = 11
ENDIF

; CHECK
help, tbeg, tend
If tbeg EQ -1 OR tend EQ -1 OR tend EQ 0 THEN STOP



;-------------------------------------------------------------------------------------------------
; READ NETCDF WRF MEAN VARIABLES
;-------------------------------------------------------------------------------------------------

IF (model EQ 'wrf' OR model EQ 'now') AND force_nomean_wrf EQ 0 AND flag_nemo EQ 0 AND flag_nomean EQ 0 THEN BEGIN

  print, '' & print, 'READ MEAN WRF DATA...'
  CASE var_name OF

    'HEIGHT': BEGIN
       IF data_type EQ 'c1m' THEN var = REPLICATE_ARRAY( orog, 12)
     END

    'SLOPE': BEGIN
       IF data_type EQ 'c1m' THEN BEGIN
         tmpx = (NCDF_LEC( 'geo_em.d01.nc', VAR='SLPX', IODIR=geog_path))[firstxt:lastxt, firstyt:lastyt] & help, tmpx
         tmpy = (NCDF_LEC( 'geo_em.d01.nc', VAR='SLPY', IODIR=geog_path))[firstxt:lastxt, firstyt:lastyt] & help, tmpy
         var  = SQRT( tmpx^2 + tmpy^2)
         var  = REPLICATE_ARRAY( var, 12)
       ENDIF
    END

    'LANDUSE': BEGIN
       IF data_type EQ 'c1m' THEN BEGIN
         tmp = ncdf_lec( 'geo_em.d01.nc', VAR='LANDUSEF', IODIR=geog_path)
         tmp = tmp[ firstxt:lastxt, firstyt:lastyt, *]
         var = FLTARR( n_elements( tmp[*,0,0]), n_elements( tmp[0,*,0]))
         FOR i = 0, n_elements( tmp[*,0,0])-1 DO FOR j = 0, n_elements( tmp[0,*,0])-1 DO BEGIN
           var[i,j] = WHERE( tmp[i,j,*] EQ MAX( tmp[i,j,*]))
         ENDFOR
         IF data_type EQ 'c1m' THEN var = REPLICATE_ARRAY( var, 12)
       ENDIF
     END

    'SOILCAT': BEGIN
       IF data_type EQ 'c1m' THEN BEGIN
         tmp = ncdf_lec( 'geo_em.d01.nc', VAR='SOILCTOP', IODIR=geog_path)
         tmp = tmp[ firstxt:lastxt, firstyt:lastyt, *]
         var = FLTARR( n_elements( tmp[*,0,0]), n_elements( tmp[0,*,0]))
         FOR i = 0, n_elements( tmp[*,0,0])-1 DO FOR j = 0, n_elements( tmp[0,*,0])-1 DO BEGIN
           var[i,j] = (WHERE( tmp[i,j,*] EQ MAX( tmp[i,j,*])))[0]
         ENDFOR
         IF data_type EQ 'c1m' THEN var = REPLICATE_ARRAY( var, 12)
       ENDIF

     END

    ; WARNING: ERAI 6h non-cumulated fields must be compared to WRF instantaneous fields
    ; WARNING: BUG WRF OCEAN SKINTEMPMEAN-> USE SKINTEMP INSTEAD
    'SKT': IF data_type EQ '6h' AND obs_name EQ 'ERAI' THEN var = read_ncdf('SKINTEMP', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT) $
           ELSE var = read_ncdf('SKINTEMP', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT)
    'T2' : IF data_type EQ '6h' AND obs_name EQ 'ERAI' THEN var = read_ncdf('T2', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT)-273.15 $
           ELSE var = read_ncdf('T2MEAN', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT)
    'TPOT2' : BEGIN
      ; potential temperature at 2m (K)
      psfc = read_ncdf(   'PSFC', tbeg, tend, filename=path+file, /TIMESTEP, /NOSTRUCT)/100. ; Pa->mb
      t2   = read_ncdf( 'T2MEAN', tbeg, tend, filename=path+file, /TIMESTEP, /NOSTRUCT)      ; K
      ;var  = t2 * ( 1000. / psfc)^(2./7.) ; K
      var  = TEMPORARY(t2) * ( 1000. / TEMPORARY(psfc) )^(2./7.) ; K
    END

    'Q2' : IF data_type EQ '6h' AND obs_name EQ 'ERAI' THEN var = read_ncdf('Q2', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT)*1000. $
           ELSE var = read_ncdf('Q2MEAN', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT)*1000.
    'UV10': BEGIN
      IF data_type EQ '6h' AND obs_name EQ 'ERAI' THEN BEGIN
        varx = read_ncdf('U10', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT)
        vary = read_ncdf('V10', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT)
        var = SQRT( varx^2 + vary^2)
      ENDIF ELSE BEGIN
        varx = read_ncdf('U10MEAN', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT)
        vary = read_ncdf('V10MEAN', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT)
        var = SQRT( varx^2 + vary^2)
      ENDELSE
    END
    'ROT_UV10': BEGIN
      varx = read_ncdf('U10MEAN', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT)
      vary = read_ncdf('V10MEAN', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT)
      var  = curl( smooth( varx, 1, /NAN), smooth( vary, 1, /NAN), /MILLION)
    END
    'UV10Q2': BEGIN
      q2   = read_ncdf( 'Q2MEAN', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT)*1000.
      varx = read_ncdf('U10MEAN', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT)*q2
      vary = read_ncdf('V10MEAN', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT)*q2
      var = SQRT( varx^2 + vary^2)
    END

    'U10': IF obs_name EQ 'ERAI' THEN var = read_ncdf('U10MEAN', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT)
    'DELTAT': IF obs_name EQ 'ERAI' THEN var = read_ncdf('T2', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT)-read_ncdf('SKINTEMP', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT)
    'DELTAQ': BEGIN
      IF obs_name EQ 'ERAI' THEN BEGIN
        tmp = read_ncdf('SKINTEMP', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT)-273.15 ; K -> degC
        tmp = 6.1094*exp((17.625*tmp)/(243.04+tmp))
        var = (read_ncdf('Q2', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT) - 0.62197*(tmp/(1020.-0.378*tmp)))*1000.
      ENDIF
    END

    ; calcul theta-e d'apres Bolton 1980 et Holland 1997
    'THETAE': BEGIN
      IF data_type EQ '6h' OR data_type EQ '1d' THEN BEGIN
        ; potential temperature at 2m (K)
        psfc  = read_ncdf('PSFC', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT)/100. ; Pa->mb
        t2    = read_ncdf(  'T2', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT) ; K
        tpot2 = TEMPORARY(t2) * ( 1000. / psfc )^(2./7.) ; K
        ; vapor pressure (mb) from specific humidity (kg/kg) at 2m
        q2    = read_ncdf(  'Q2', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT) ; kg/kg
        e2    = TEMPORARY(psfc) / ( 0.622 / q2 + 0.378 )
        ; temperature at the lifting condensation level (K)
        tl    = (2840 / (3.5 * alog(tpot2[*,*,*,0]) - alog(TEMPORARY(e2)) - 4.805)) + 55
        ; equivalent potential temperature at 2m (K)
        te2   = TEMPORARY(tpot2) * exp( (3.376 / TEMPORARY(tl) - 0.00254) * 1000 * q2 * (1 + 0.81 * TEMPORARY(q2)) )
        var   = TEMPORARY(te2)
      ENDIF
    END

    ; WARNING: ERAI cumulated fields must be compared to WRF mean fields
    'RAIN': BEGIN
              varx = read_ncdf('RAINCVMEAN' , filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT)*86400. ;mm/s -> mm/day
              vary = read_ncdf('RAINNCVMEAN', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT)*86400. ;mm/s -> mm/day
              var  = varx + vary 
            END
    'LH' : var = read_ncdf('LHMEAN' , filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT)*(-1.)
    'HFX': var = read_ncdf('HFXMEAN', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT)*(-1.)
    'OLR': var = read_ncdf('OLRMEAN', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT)
    ; surface net shortwave
    'GSW': var = read_ncdf('GSWMEAN', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT)
    ; surface downward longwave
    'GLW': var = read_ncdf('GLWMEAN', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT)
    ; surface net longwave
    'LWR': BEGIN
      IF force_seamask THEN tmp = read_ncdf('SKINTEMPMEAN', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT) ELSE $
                            tmp = read_ncdf('SKINTEMP', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT)
      IF STRMATCH(exp_name, '*sol094era*', /FOLD_CASE) OR STRMATCH(exp_name, 'tr*now_*') THEN tmp = tmp + 273.15 ; degC -> K
      var = read_ncdf('GLWMEAN' , filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT) - tmp^4*emiss*stebolt
    END
    ; surface radiative budget
    'RAD': BEGIN
      IF force_seamask THEN tmp = read_ncdf('SKINTEMPMEAN', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT) ELSE $
                            tmp = read_ncdf('SKINTEMP', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT)
      IF STRMATCH(exp_name, '*sol094era*', /FOLD_CASE) OR STRMATCH(exp_name, 'tr*now_*') THEN tmp = tmp + 273.15 ; degC -> K
      var = read_ncdf('GLWMEAN' , filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT) + $
            read_ncdf('GSWMEAN' , filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT) - tmp^4*emiss*stebolt
    END
    ; surface turbulent heat flux
    'TURB': var = read_ncdf('LHMEAN' , filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT)*(-1.) $
                - read_ncdf('HFXMEAN', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT)
    ; surface total heat flux budget
    'NET': BEGIN
      IF force_seamask THEN tmp = read_ncdf('SKINTEMPMEAN', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT) ELSE $
                            tmp = read_ncdf('SKINTEMP', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT)
      IF STRMATCH(exp_name, '*sol094era*', /FOLD_CASE) OR STRMATCH(exp_name, 'tr*now_*') THEN tmp = tmp + 273.15 ; degC -> K
      var = read_ncdf('GLWMEAN', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT) + $
            read_ncdf('GSWMEAN', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT) - tmp^4*emiss*stebolt - $
            read_ncdf('LHMEAN' , filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT) - $
            read_ncdf('HFXMEAN', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT)
    END
    ;'ALBEDO': var = 1. - read_ncdf( 'GSW' , filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT) / read_ncdf('SWDOWN' , filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT)
    'ALBEDO': BEGIN
      var = ncdf_lec( 'geo_em.d01.nc', VAR='ALBEDO12M', IODIR=geog_path)
      var = var[ firstxt:lastxt, firstyt:lastyt, *]
    END

;-------------------------------------------------------------------------------------------------
; VARIABLES 3D
;-------------------------------------------------------------------------------------------------

    'RHOD': BEGIN
      IF data_type EQ '1m' OR data_type EQ 'c1m' THEN BEGIN
        psfc = read_ncdf( 'PSFC', tbeg, tend, /TIMESTEP, filename=path+file, /NOSTRUCT)/100. ; hPa
        t    = read_ncdf(   'TT', tbeg, tend, /TIMESTEP, filename=path+file, /NOSTRUCT)      ; K
        t[WHERE(t GT 1.e+30)] = !VALUES.F_NAN
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
      IF data_type EQ '1m' OR data_type EQ 'c1m' THEN BEGIN
        psfc = read_ncdf(  'PSFC', tbeg, tend, /TIMESTEP, filename=path+file, /NOSTRUCT)/100. ; hPa
        t    = read_ncdf(    'TT', tbeg, tend, /TIMESTEP, filename=path+file, /NOSTRUCT)      ; K
        q    = read_ncdf('QVAPOR', tbeg, tend, /TIMESTEP, filename=path+file, /NOSTRUCT)      ; kg/kg
        t[WHERE(t GT 1.e+30)] = !VALUES.F_NAN
        q[WHERE(q GT 1.e+30)] = !VALUES.F_NAN
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

    'T': BEGIN
      flag_z    = 1
      flag_mask = 1
      IF data_type EQ '1m' OR data_type EQ 'c1m' THEN BEGIN
        var  = read_ncdf(   'TT', tbeg, tend, /TIMESTEP, filename=path+file, /NOSTRUCT)
        var[WHERE(var GT 1.e+30)] = !VALUES.F_NAN
        psfc = read_ncdf( 'PSFC', tbeg, tend, /TIMESTEP, filename=path+file, /NOSTRUCT)/100. ; Pa -> hPa
        FOR l = 0, jpt-1 DO BEGIN
        FOR j = 0, nyt-1 DO BEGIN
        FOR i = 0, nxt-1 DO BEGIN
          kbad = WHERE( gdept GT psfc[i,j,l])
          IF kbad[0] NE -1 THEN var[i,j,kbad,l] = !VALUES.F_NAN
        ENDFOR
        ENDFOR
        ENDFOR
      ENDIF ELSE STOP
      help, TEMPORARY(psfc)
    END

    'Q': BEGIN
      flag_z    = 1
      flag_mask = 1
      IF data_type EQ '1m'  OR data_type EQ 'c1m' THEN BEGIN
        var  = read_ncdf(   'QVAPOR', tbeg, tend, /TIMESTEP, filename=path+file, /NOSTRUCT)*1000. ; kg/kg -> g/kg
        var[WHERE(var GT 1.e+30)] = !VALUES.F_NAN
        psfc = read_ncdf( 'PSFC', tbeg, tend, /TIMESTEP, filename=path+file, /NOSTRUCT)/100. ; Pa -> hPa
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

    'QCLOUD': BEGIN
      flag_z    = 1
      flag_mask = 1
      IF data_type EQ '1m'  OR data_type EQ 'c1m' THEN BEGIN
        var  = read_ncdf(   'QCLOUD', tbeg, tend, /TIMESTEP, filename=path+file, /NOSTRUCT)*1000. ; kg/kg -> g/kg
        var[WHERE(var GT 1.e+30)] = !VALUES.F_NAN
        psfc = read_ncdf( 'PSFC', tbeg, tend, /TIMESTEP, filename=path+file, /NOSTRUCT)/100. ; Pa -> hPa
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

    'QALL': BEGIN
      flag_z    = 1
      flag_mask = 1
      IF data_type EQ '1m' OR data_type EQ 'c1m' THEN BEGIN
        var  = read_ncdf( 'QCLOUD', tbeg, tend, /TIMESTEP, filename=path+file, /NOSTRUCT) + $
               read_ncdf(  'QRAIN', tbeg, tend, /TIMESTEP, filename=path+file, /NOSTRUCT) + $
               read_ncdf(   'QICE', tbeg, tend, /TIMESTEP, filename=path+file, /NOSTRUCT) + $
               read_ncdf(  'QSNOW', tbeg, tend, /TIMESTEP, filename=path+file, /NOSTRUCT) + $
               read_ncdf( 'QGRAUP', tbeg, tend, /TIMESTEP, filename=path+file, /NOSTRUCT)
        var = var * 1000. ; kg/kg -> g/kg
        var[WHERE(var GT 1.e+30)] = !VALUES.F_NAN
        psfc = read_ncdf( 'PSFC', tbeg, tend, /TIMESTEP, filename=path+file, /NOSTRUCT)/100. ; Pa -> hPa
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
      IF data_type EQ '1m' OR data_type EQ 'c1m' THEN BEGIN
        var  = read_ncdf(   'QVAPOR', tbeg, tend, /TIMESTEP, filename=path+file, /NOSTRUCT)*1000. ; kg/kg -> g/kg
        var[WHERE(var GT 1.e+30)] = !VALUES.F_NAN
        psfc = read_ncdf( 'PSFC', tbeg, tend, /TIMESTEP, filename=path+file, /NOSTRUCT)/100. ; Pa -> hPa
        varx = read_ncdf('UU', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT); * var
        vary = read_ncdf('VV', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT); * var
        varx[WHERE(varx GT 1.e+30)] = !VALUES.F_NAN
        vary[WHERE(vary GT 1.e+30)] = !VALUES.F_NAN
        var  = SQRT( varx^2 + vary^2) * TEMPORARY(var)
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

    'W': BEGIN
      IF data_type EQ '1m' OR data_type EQ 'c1m' THEN BEGIN
        var = read_ncdf('W', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT) ; m/s
        var[WHERE(var GT 1.e+30)] = !VALUES.F_NAN
        psfc = read_ncdf( 'PSFC', tbeg, tend, /TIMESTEP, filename=path+file, /NOSTRUCT)/100. ; Pa -> hPa
        FOR l = 0, jpt-1 DO BEGIN
        FOR j = 0, nyt-1 DO BEGIN
        FOR i = 0, nxt-1 DO BEGIN
          kbad = WHERE( gdept GT psfc[i,j,l] )
          IF kbad[0] NE -1 THEN var[i,j,kbad,l]  = !VALUES.F_NAN
        ENDFOR
        ENDFOR
        ENDFOR
      ENDIF ELSE STOP
      help, TEMPORARY(psfc)
    END

    'WQ': BEGIN
      IF data_type EQ '1m' OR data_type EQ 'c1m' THEN BEGIN
        var  = read_ncdf( 'QVAPOR', tbeg, tend, /TIMESTEP, filename=path+file, /NOSTRUCT)*1000. ; kg/kg -> g/kg
        var[WHERE(var GT 1.e+30)] = !VALUES.F_NAN
        psfc = read_ncdf( 'PSFC', tbeg, tend, /TIMESTEP, filename=path+file, /NOSTRUCT)/100. ; Pa -> hPa
        w = read_ncdf('W', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT) ; m/s
        w[WHERE(w GT 1.e+30)] = !VALUES.F_NAN
        var  = TEMPORARY(w) * TEMPORARY(var)
        FOR l = 0, jpt-1 DO BEGIN
        FOR j = 0, nyt-1 DO BEGIN
        FOR i = 0, nxt-1 DO BEGIN
          kbad = WHERE( gdept GT psfc[i,j,l] )
          IF kbad[0] NE -1 THEN var[i,j,kbad,l]  = !VALUES.F_NAN
        ENDFOR
        ENDFOR
        ENDFOR
      ENDIF ELSE STOP
      help, TEMPORARY(psfc)
    END

    'RH': BEGIN
      IF data_type EQ '1m' OR data_type EQ 'c1m' THEN BEGIN
        var  = read_ncdf( 'RH', tbeg, tend, /TIMESTEP, filename=path+file, /NOSTRUCT)
        var[WHERE(var GT 1.e+30)] = !VALUES.F_NAN
        psfc = read_ncdf( 'PSFC', tbeg, tend, /TIMESTEP, filename=path+file, /NOSTRUCT)/100. ; Pa -> hPa
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
      flag_z    = 1
      flag_mask = 1
      IF data_type EQ '1m' OR data_type EQ 'c1m' THEN BEGIN
        var  = read_ncdf(   'TT', tbeg, tend, /TIMESTEP, filename=path+file, /NOSTRUCT)
        var[WHERE(var GT 1.e+30)] = !VALUES.F_NAN
        psfc = read_ncdf( 'PSFC', tbeg, tend, /TIMESTEP, filename=path+file, /NOSTRUCT) / 100.
        FOR l = 0, jpt-1 DO BEGIN
          FOR j = 0, nyt-1 DO BEGIN
            FOR i = 0, nxt-1 DO BEGIN
              kbad = WHERE( gdept GT psfc[i,j,l] )
              IF kbad[0] NE -1 THEN var[i,j,kbad,l] = !VALUES.F_NAN
            ENDFOR
          ENDFOR
        ENDFOR
        FOR k = 0, nzt-1 DO var[*,*,k,*] = var[*,*,k,*] * (1000. / replicate( gdept[k], nxt, nyt, jpt) )^(2./7.)
      ENDIF ELSE STOP
      help, TEMPORARY(psfc)
    END

    'TPOTE': BEGIN
      IF data_type EQ '1m' OR data_type EQ 'c1m' THEN BEGIN
        ; potential temperature (K
        psfc = read_ncdf( 'PSFC', tbeg, tend, /TIMESTEP, filename=path+file, /NOSTRUCT) / 100.
        tpot = read_ncdf(   'TT', tbeg, tend, /TIMESTEP, filename=path+file, /NOSTRUCT)
        tpot[WHERE(tpot GT 1.e+30)] = !VALUES.F_NAN
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
        q = read_ncdf( 'QVAPOR', tbeg, tend, /TIMESTEP, filename=path+file, /NOSTRUCT) ; kg
        q[WHERE(q GT 1.e+30)] = !VALUES.F_NAN
        ew = q & FOR k = 0, nzt-1 DO ew[*,*,k,*] = replicate( gdept[k], nxt, nyt, jpt) / ( 0.622 / q + 0.378 )
        help, e, q
        ; temperature at the lifting condensation level (K)
        tl  = (2840 / (3.5 * alog(tpot) - alog(TEMPORARY(ew)) - 4.805)) + 55
        ; equivalent potential temperature (K)
        var = TEMPORARY(tpot) * exp( (3.376 / TEMPORARY(tl) - 0.00254) * 1000 * q * (1 + 0.81 * TEMPORARY(q)) )
        help, var
      ENDIF
    END

    'UV': BEGIN
      varx = read_ncdf('UU', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT)
      vary = read_ncdf('VV', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT)
      var  = SQRT( varx^2 + vary^2)
      IF data_type EQ '1m' OR data_type EQ 'c1m' THEN BEGIN
        psfc = read_ncdf( 'PSFC', tbeg, tend, /TIMESTEP, filename=path+file, /NOSTRUCT)/100. ; Pa -> hPa
        varx = read_ncdf('UU', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT); * var
        vary = read_ncdf('VV', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT); * var
        varx[WHERE(varx GT 1.e+30)] = !VALUES.F_NAN
        vary[WHERE(vary GT 1.e+30)] = !VALUES.F_NAN
        var = SQRT( varx^2 + vary^2)
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

    'U': BEGIN
      var = read_ncdf('UU', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT)
    END
    'V': BEGIN
      var = read_ncdf('VV', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT)
    END

    'MSLP': BEGIN
      p_sfc  = read_ncdf( 'PSFC', tbeg, tend, /TIMESTEP, filename=path+file, /NOSTRUCT)/100. ; Pa -> hPa
      hgt    = REPLICATE_ARRAY(read_ncdf('HGT_M', 0, 0, filename=geog_path+'geo_em.d01.nc', /TIMESTEP, /NOSTRUCT),12)+2. ; m
      t_sfc  = read_ncdf('T2MEAN', tbeg, tend, filename=path+file, /TIMESTEP, /NOSTRUCT)+273.15 ; degC -> K
      q_sfc  = read_ncdf('Q2MEAN', tbeg, tend, filename=path+file, /TIMESTEP, /NOSTRUCT) ; kg/kg
      q_sfc  = q_sfc / (q_sfc + 1.) ; conversion water vapor mixing ratio -> specific humidity
      tv_sfc = t_sfc*(1.+0.608*q_sfc) ; virtual temperature (K)
      tv_msl = tv_sfc + gamma * hgt
      var    = p_sfc * exp( hgt / (rd/g * (tv_sfc+tv_msl)/2. ) )
      help, p_sfc, hgt, t_sfc, q_sfc, var
      ;hgt_1_gridobs = fromreg('bilinear', hgt, glamt[firstxt:lastxt,firstyt:lastyt], gphit[firstxt:lastxt,firstyt:lastyt], lon_0, lat_0)
    END

    'GRAD_MSLP': BEGIN
      p_sfc  = read_ncdf( 'PSFC', tbeg, tend, /TIMESTEP, filename=path+file, /NOSTRUCT)/100. ; Pa -> hPa
      hgt    = REPLICATE_ARRAY(read_ncdf('HGT_M', 0, 0, filename=geog_path+'geo_em.d01.nc', /TIMESTEP, /NOSTRUCT),12)+2. ; m
      t_sfc  = read_ncdf('T2MEAN', tbeg, tend, filename=path+file, /TIMESTEP, /NOSTRUCT)+273.15 ; degC -> K
      q_sfc  = read_ncdf('Q2MEAN', tbeg, tend, filename=path+file, /TIMESTEP, /NOSTRUCT) ; kg/kg
      q_sfc  = q_sfc / (q_sfc + 1.) ; conversion water vapor mixing ratio -> specific humidity
      tv_sfc = t_sfc*(1.+0.608*q_sfc) ; virtual temperature (K)
      tv_msl = tv_sfc + gamma * hgt
      var    = p_sfc * exp( hgt / (rd/g * (tv_sfc+tv_msl)/2. ) )
      var    = NORM( GRAD( var, "x", /MILLION), GRAD( var, "y", /MILLION) )
      help, p_sfc, hgt, t_sfc, q_sfc, var
      ;hgt_1_gridobs = fromreg('bilinear', hgt, glamt[firstxt:lastxt,firstyt:lastyt], gphit[firstxt:lastxt,firstyt:lastyt], lon_0, lat_0)
    END

    ELSE : var = read_ncdf(var_name, filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT)

  ENDCASE

ENDIF


;-------------------------------------------------------------------------------------------------
; WRF INSTANT VARIABLES
;-------------------------------------------------------------------------------------------------

IF (model EQ 'wrf' OR model EQ 'now') AND ((force_nomean_wrf AND flag_nemo EQ 0) OR flag_nomean) THEN BEGIN

  print, '' & print, 'READ INSTANT WRF DATA...'
  CASE var_name OF
    
    'RAIN': BEGIN
              IF grid NE 'ind025' THEN BEGIN
                varx = read_ncdf('PREC_ACC_C' , filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT)*4. ;mm/6h -> mm/day
                vary = read_ncdf('PREC_ACC_NC', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT)*4. ;mm/6h -> mm/day
                var  = varx + vary
              END ELSE BEGIN
                varx = read_ncdf('RAINC' , filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT)*4. ;mm/6h -> mm/day
                vary = read_ncdf('RAINNC', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT)*4. ;mm/6h -> mm/day
                var  = varx + vary
              ENDELSE
            END

    'UV10': BEGIN
            var = SQRT(read_ncdf('U10', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT)^2 + $
                       read_ncdf('V10', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT)^2)
            varx = read_ncdf('U10', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT)
            vary = read_ncdf('V10', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT)
            END

    'ROT_UV10': BEGIN
      varx = read_ncdf('U10', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT)
      vary = read_ncdf('V10', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT)
      var  = curl( smooth( varx, 1, /NAN), smooth( vary, 1, /NAN), /MILLION)
    END

    'SKT': var = read_ncdf('SKINTEMP', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT)
    'LH' : var = read_ncdf('LH' , filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT)*(-1.)
    'HFX': var = read_ncdf('HFX', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT)*(-1.)
    'OLR': var = read_ncdf('OLR', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT)
    'Q2' : var = read_ncdf('Q2', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT) * 1000.  ; kg/kg -> g/kg
    'T2' : var = read_ncdf('T2', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT) - 273.15 ; K -> degC

    'DELTAT': var = read_ncdf('T2', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT)-(read_ncdf('SKINTEMP', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT)-274.15)

    'DELTAQ': BEGIN
              tmp = read_ncdf('SKINTEMP', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT)-273.15 ; K -> degC
              tmp = 6.1094*exp((17.625*tmp)/(243.04+tmp)) & help, tmp
              var = (read_ncdf('Q2', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT) - 0.62197*(tmp/(1020.-0.378*tmp)))*1000.
              END

    ; surface net shortwave
    'GSW': var = read_ncdf('GSW', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT)
    ; surface downward longwave
    'GLW': var = read_ncdf('GLW', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT)
    ; surface net longwave
    'LWR': var = read_ncdf('GLW', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT) - $
                 read_ncdf('SKINTEMP', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT)^4*emiss*stebolt
    ; surface radiative budget
    'RAD': var = read_ncdf('GLW', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT) + $
                 read_ncdf('GSW', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT) - $
                 read_ncdf('SKINTEMP', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT)^4*emiss*stebolt
    ; surface total heat flux budget
    'NET': var = read_ncdf('GLW', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT) + $
                 read_ncdf('GSW', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT) - $
                 read_ncdf('SKINTEMP', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT)^4*emiss*stebolt - $
                 read_ncdf('LH' , filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT) - $
                 read_ncdf('HFX', filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT)

    ELSE : var = read_ncdf(var_name, filename=path+file, tbeg, tend, /TIMESTEP, /NOSTRUCT)

  ENDCASE

ENDIF


;-------------------------------------------------------------------------------------------------
; NEMO VARIABLES
;-------------------------------------------------------------------------------------------------

IF model EQ 'nemo' OR flag_nemo THEN BEGIN

  print, '' & print, 'READ NEMO DATA...'
  CASE var_name OF

    'SST': var = read_ncdf('tos', tbeg, tend, filename=path+file, /TIMESTEP, /NOSTRUCT)
    'SKINTEMP': var = read_ncdf('tos', tbeg, tend, filename=path+file, /TIMESTEP, /NOSTRUCT)

    'STRESS': BEGIN
              ;var  = read_ncdf('taum', tbeg, tend, filename=path+file, /TIMESTEP, /NOSTRUCT, GRID='T') & help, var
              varx = read_ncdf('tauuo', tbeg, tend, filename=path+STRJOIN(STRSPLIT(file, 'T', /EXTRACT), 'U'), /TIMESTEP, /NOSTRUCT, GRID='U') & help, varx
              vary = read_ncdf('tauvo', tbeg, tend, filename=path+STRJOIN(STRSPLIT(file, 'T', /EXTRACT), 'V'), /TIMESTEP, /NOSTRUCT, GRID='V') & help, vary
              var  = NORM( varx, vary)
              END

    'NET' : var = read_ncdf('tohfls', tbeg, tend, filename=path+file, /TIMESTEP, /NOSTRUCT)
    'GSW' : var = read_ncdf('rsntds', tbeg, tend, filename=path+file, /TIMESTEP, /NOSTRUCT)
    'MLDT': var = read_ncdf('mld_dt02', tbeg, tend, filename=path+file, /TIMESTEP, /NOSTRUCT)
    'MLDR': var = read_ncdf('mldr10_3', tbeg, tend, filename=path+file, /TIMESTEP, /NOSTRUCT) 
    'BLT' : var = read_ncdf('blt', tbeg, tend, filename=path+file, /TIMESTEP, /NOSTRUCT)
    'SLA' : var = read_ncdf('sla', tbeg, tend, filename=path+file, /TIMESTEP, /NOSTRUCT)*100. ; m -> cm
    'THETAO' : var = read_ncdf('thetao', tbeg, tend, filename=path+file, /TIMESTEP, /NOSTRUCT)
    'SO'  : var = read_ncdf('so', tbeg, tend, filename=path+file, /TIMESTEP, /NOSTRUCT)
    'D20': BEGIN
      thetao = read_ncdf( 'thetao', tbeg, tend, filename=path+file, /TIMESTEP, /NOSTRUCT)
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
    END

    ELSE  : var = read_ncdf(var_name, filename=path+file, /TIMESTEP, /NOSTRUCT)

  ENDCASE

  ; MASK MISSING VALUES
  var[WHERE(var EQ 0.)] = !VALUES.F_NAN

ENDIF


;-------------------------------------------------------------------------------------------------
; CONVERSIONS
;-------------------------------------------------------------------------------------------------

IF model NE 'nemo' AND flag_nemo EQ 0 THEN $
;IF exp_name NE 'tr075_ra12L60_sol094era234' AND exp_name NE 'tr075_ra12L60_sol094erafullt' THEN $
IF var_name EQ 'SST' OR var_name EQ 'SKT' OR var_name EQ 'SKINTEMP' THEN var = var - 273.15 ; K -> degC

IF var_name EQ 'PSFC' THEN var = var / 100. ; Pa -> hPa
