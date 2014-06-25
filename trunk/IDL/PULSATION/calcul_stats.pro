
print, 'CALCUL STATISTICS...'
IF data_type EQ 'c1m' THEN flag_sig = 0

flag_lagcor = 0 ; compute 1m lagged correlation betwenn obs & exps


;-------------------------------------------------------------------------------------------------
; c1m STATS
;-------------------------------------------------------------------------------------------------

    IF data_type EQ 'c1m' OR data_type EQ '1m' THEN BEGIN

      ; sc1m CORRELATION 1D+2D + BIAS + RMSE
      FOR e = 1, n_elements(exp_list)-1 DO BEGIN
        cmd = execute( 'corr_0'+strtrim(e,2)+' = LINCORR( var_sc1m_0, var_sc1m_'+strtrim(e,2)+' )' )
        cmd = execute( 'IF corr_0'+strtrim(e,2)+'[1] LE 0.05 THEN corr_0'+strtrim(e,2)+'[1] = 1')
        cmd = execute( 'indok = WHERE( FINITE('+'var_mean_0) EQ 1 AND FINITE('+'var_mean_'+strtrim(e,2)+'_gridobs) EQ 1, nbok)' )
        cmd = execute( 'corrpat_0'+strtrim(e,2)+' = LINCORR( (REFORM('+'var_mean_0, n_elements('+'var_mean_0)))[indok], (REFORM('+'var_mean_'+strtrim(e,2)+'_gridobs, n_elements('+'var_mean_'+strtrim(e,2)+'_gridobs)))[indok] )' )
        cmd = execute( 'bias_'+strtrim(e,2)+' = (MEAN( var_sc1m_'+strtrim(e,2)+', /NAN)-MEAN( var_sc1m_0, /NAN)) / MEAN( var_sc1m_0, /NAN)*100.' )
        cmd = execute( 'rmse_'+strtrim(e,2)+' = SQRT( TOTAL( (var_sc1m_'+strtrim(e,2)+' - var_sc1m_0)^2, /NAN) / n_elements(var_sc1m_0)  )' )
      ENDFOR
      print, 'CORR1D + CORR2D + BIAS + RMSE OK'

      IF n_elements(exp_list) EQ 3 THEN corr_12 = LINCORR( var_sc1m_1, var_sc1m_2)
      IF n_elements(exp_list) EQ 3 THEN IF corr_12[1] LE 0.05 THEN corr_12[1] = 1
      IF n_elements(exp_list) EQ 4 THEN corr_13 = LINCORR( var_sc1m_1, var_sc1m_3)
      IF n_elements(exp_list) EQ 4 THEN IF corr_13[1] LE 0.05 THEN corr_13[1] = 1
      IF n_elements(exp_list) EQ 4 THEN corr_23 = LINCORR( var_sc1m_2, var_sc1m_3)
      IF n_elements(exp_list) EQ 4 THEN IF corr_23[1] LE 0.05 THEN corr_23[1] = 1

      ; sc1m STDDEV ERROR
      ;std_errvar_sc1m = fltarr(12)
      ;FOR m = 0, 12-1 DO BEGIN
      ;  all_errvar = !NULL
      ;  FOR e = 1, n_elements(exp_list)-1 DO cmd = execute( 'all_errvar =  [[[all_errvar]], [[errvar_'+strtrim(e,2)+'[*,*,m]]]]' )
      ;  IF n_elements(exp_list) GT 2 THEN std_errvar_sc1m[m] = MEAN( STDDEV( all_errvar, DIMENSION=3, /NAN), /NAN)
      ;ENDFOR
      ;print, 'STDDEV ERROR OK'

      ; sc1m MONTHLY CORR PATTERN
      IF data_type EQ 'c1m' THEN BEGIN
        FOR e = 1, n_elements(exp_list)-1 DO BEGIN
          cmd = execute( 'cor2D1m_'+strtrim(e,2)+' = FLTARR(12)' )
          FOR m = 0, 12-1 DO BEGIN
            cmd = execute( 'indok = WHERE( FINITE('+'var_0[*,*,m]) EQ 1 AND FINITE('+'var_'+strtrim(e,2)+'_gridobs[*,*,m]) EQ 1, nbok)' )
            tmp0 = var_0[*,*,m]
            cmd = execute( 'tmp = var_'+strtrim(e,2)+'_gridobs[*,*,m]' )
            cmd = execute( 'cor2D1m_'+strtrim(e,2)+'[m] = CORRELATE( TEMPORARY(tmp0[indok]), TEMPORARY(tmp[indok]))' )
          ENDFOR
        ENDFOR
        ;var_plot = 'cor2D1m'
        ;@plot_1D_t
        print, 'sc1m MONTHLY COR2D OK'
      ENDIF

      ; sc1m LAG CORRELATION
      IF flag_lagcor AND data_type EQ 'c1m' THEN BEGIN
        ;lag1m = [-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6] ; 1m lag definition
        lag1m = [0,1,2,3,4,5,6,7,8,9]
        FOR e = 1, n_elements(exp_list)-1 DO BEGIN
          cmd = execute( 'lagcor_0'+strtrim(e,2)+' = FLTARR( n_elements(lon_0), n_elements(lat_0), n_elements(lag1m))' )
          cmd = execute( 'maxcor_0'+strtrim(e,2)+' = FLTARR( n_elements(lon_0), n_elements(lat_0))' )
          cmd = execute( 'lag_0'+strtrim(e,2)+'    = FLTARR( n_elements(lon_0), n_elements(lat_0))' )
          FOR j = 0, n_elements(lat_0)-1 DO BEGIN
            FOR i = 0, n_elements(lon_0)-1 DO BEGIN
              cmd = execute( 'tst = FINITE(var_0[i,j,0]) AND FINITE(var_'+strtrim(e,2)+'_gridobs[i,j,0])' )
              IF tst EQ 1 THEN BEGIN
                cmd = execute( 'lagcor_0'+strtrim(e,2)+'[i,j,*] = C_CORRELATE( var_0[i,j,*], var_'+strtrim(e,2)+'_gridobs[i,j,*], lag1m)' )
                cmd = execute( 'maxcor_0'+strtrim(e,2)+'[i,j,*] = MAX( lagcor_0'+strtrim(e,2)+'[i,j,*], /NAN)' )
                cmd = execute( 'lag_0'+strtrim(e,2)+'[i,j]      = lag1m[ WHERE( lagcor_0'+strtrim(e,2)+'[i,j,*] EQ maxcor_0'+strtrim(e,2)+'[i,j])]' )
                cmd = execute( 'tmp = MAX( var_'+strtrim(e,2)+'_gridobs[i,j,*], /NAN)' )
                IF var_name EQ 'RAIN' AND (MAX( var_0[i,j,*], /NAN) LT 1. OR tmp LT 1. ) THEN BEGIN
                  cmd = execute( 'lagcor_0'+strtrim(e,2)+'[i,j,*] = !VALUES.F_NAN' )
                  cmd = execute( 'maxcor_0'+strtrim(e,2)+'[i,j,*] = !VALUES.F_NAN' )
                  cmd = execute( 'lag_0'+strtrim(e,2)+'[i,j,*]    = !VALUES.F_NAN' )
                ENDIF
              ENDIF ELSE BEGIN
                cmd = execute( 'lagcor_0'+strtrim(e,2)+'[i,j,*] = !VALUES.F_NAN' )
                cmd = execute( 'maxcor_0'+strtrim(e,2)+'[i,j,*] = !VALUES.F_NAN' )
                cmd = execute( 'lag_0'+strtrim(e,2)+'[i,j,*]    = !VALUES.F_NAN' )
              ENDELSE
            ENDFOR
          ENDFOR
        ENDFOR
        var_plot = 'maxcor_0'
        @plot_2D
        var_plot = 'lag_0'
        @plot_2D
        print, '1m LAG CORR OK'
      ENDIF

    ENDIF


;-------------------------------------------------------------------------------------------------
; 1m STATS
;-------------------------------------------------------------------------------------------------

    IF data_type EQ '1m' THEN BEGIN

      ; 1m SEASONAL CORRELATION 1D+2D
      nbyear_max = !NULL
      FOR e = 0, n_elements(exp_list)-1 DO cmd = execute( 'nbyear_max = MAX( [ nbyear_max, nbyear_'+strtrim(e,2)+'] )' ) & help, nbyear_max
      IF nbyear_max GT 1 THEN BEGIN
        FOR e = 1, n_elements(exp_list)-1 DO BEGIN
          cmd = execute( 'indok = WHERE( FINITE(var_ts1y_0) EQ 1 AND FINITE(var_ts1y_'+strtrim(e,2)+') EQ 1, nbok)' )
          cmd = execute( 'corrind_0'+strtrim(e,2)+' = LINCORR( (REFORM(var_ts1y_0, n_elements(var_ts1y_0)))[indok], (REFORM(var_ts1y_'+strtrim(e,2)+', n_elements(var_ts1y_'+strtrim(e,2)+')))[indok] )' )
          cmd = execute( 'indok = WHERE( FINITE('+'var_mean_0) EQ 1 AND FINITE('+'var_mean_'+strtrim(e,2)+'_gridobs) EQ 1, nbok)' )
          cmd = execute( 'corrpat_0'+strtrim(e,2)+' = LINCORR( (REFORM('+'var_mean_0, n_elements('+'var_mean_0)))[indok], (REFORM('+'var_mean_'+strtrim(e,2)+'_gridobs, n_elements('+'var_mean_'+strtrim(e,2)+'_gridobs)))[indok] )' )
        ENDFOR
      ENDIF

      ; MONTHLY TM_TEST (90%)
      IF nbyear_max GT 1 THEN BEGIN

        tmp = FLTARR( n_elements(exp_list), 12, nbyear_max) * !VALUES.F_NAN
        FOR e = 1, n_elements(exp_list)-1 DO cmd = execute( 'sig1D_0'+strtrim(e,2)+' = FLTARR(12) ')
        FOR m = 0, 12-1 DO BEGIN
          FOR e = 0, n_elements(exp_list)-1 DO BEGIN
            cmd = execute( 'ind_month = WHERE( listmonth_'+strtrim(e,2)+' EQ m, nbmonth)' )
            cmd = execute( 'tmp[ e, m, 0:nbmonth-1 ] = var_ts_'+strtrim(e,2)+'[ind_month]' )
          ENDFOR
          FOR e = 1, n_elements(exp_list)-1 DO cmd = execute( 'sig1D_0'+strtrim(e,2)+'[m] = ( TM_TEST( tmp[ 0, m, *], tmp[ e, m, *]) )[1]' )
        ENDFOR
        FOR e = 1, n_elements(exp_list)-1 DO cmd = execute( 'sig1D_0'+strtrim(e,2)+'[ WHERE( sig1D_0'+strtrim(e,2)+' LE 0.10 ) ] = 1. &'+ $
        'sig1D_0'+strtrim(e,2)+'[ WHERE( sig1D_0'+strtrim(e,2)+' NE 1. ) ] = 0.')

        IF n_elements(exp_list) GT 2 THEN BEGIN
          sig1D_12 = FLTARR(12)
          IF n_elements(exp_list) GE 4 THEN sig1D_13 = FLTARR(12)
          IF n_elements(exp_list) GE 4 THEN sig1D_23 = FLTARR(12)
          FOR m = 0, 12-1 DO BEGIN
            tmp1 = REFORM(tmp[ 1, m, WHERE( FINITE( tmp[ 1, m, *]) EQ 1)]) ;& help, tmp1
            tmp2 = REFORM(tmp[ 2, m, WHERE( FINITE( tmp[ 2, m, *]) EQ 1)]) ;& help, tmp2
            sig1D_12[m] = (TM_TEST( tmp1, tmp2))[1]
            IF n_elements(exp_list) GE 4 THEN tmp3 = REFORM(tmp[ 3, m, WHERE( FINITE( tmp[ 3, m, *]) EQ 1)])
            IF n_elements(exp_list) GE 4 THEN sig1D_13[m] = (TM_TEST( tmp1, tmp3))[1]
            IF n_elements(exp_list) GE 4 THEN sig1D_23[m] = (TM_TEST( tmp2, tmp3))[1]
          ENDFOR
          sig1D_12[ WHERE( sig1D_12 LE 0.10 ) ] = 1. & sig1D_12[ WHERE( sig1D_12 NE 1. ) ] = 0.
          IF n_elements(exp_list) GE 4 THEN BEGIN & sig1D_13[ WHERE( sig1D_13 LE 0.10 ) ] = 1. & sig1D_13[ WHERE( sig1D_13 NE 1. ) ] = 0. & ENDIF
          IF n_elements(exp_list) GE 4 THEN BEGIN & sig1D_23[ WHERE( sig1D_23 LE 0.10 ) ] = 1. & sig1D_23[ WHERE( sig1D_23 NE 1. ) ] = 0. & ENDIF
        ENDIF

        ; SPATIAL TM_TEST (90%)
        IF flag_sig THEN BEGIN
          nbyear_min = !NULL
          FOR e = 1, n_elements(exp_list)-1 DO cmd = execute( 'nbyear_min = MIN( [nbyear_min, nbyear_'+STRTRIM(e,2)+'] )' ) & help, nbyear_min
          IF n_elements(exp_list) GE 2 AND nbyear_0 EQ nbyear_min THEN BEGIN
            sig2D_01 = FLTARR( n_elements(lon_0), n_elements(lat_0) )
            FOR j = 0, n_elements(lat_0)-1 DO BEGIN
              FOR i = 0, n_elements(lon_0)-1 DO BEGIN
                sig2D_01[i,j] = ( TM_TEST( var_0[i,j,ind_mean1m], var_1_gridobs[i,j,ind_mean1m]))[1]
              ENDFOR
            ENDFOR
            sig2D_01[ WHERE( sig2D_01 LE 0.10 ) ] = 1. & sig2D_01[ WHERE( sig2D_01 NE 1. ) ] = !VALUES.F_NAN
            help, sig2D_01
          ENDIF
          IF n_elements(exp_list) GE 3 AND nbyear_0 EQ nbyear_min THEN BEGIN
            sig2D_02 = FLTARR( n_elements(lon_0), n_elements(lat_0) )
            FOR j = 0, n_elements(lat_0)-1 DO BEGIN
              FOR i = 0, n_elements(lon_0)-1 DO BEGIN
                sig2D_02[i,j] = ( TM_TEST( var_0[i,j,ind_mean1m], var_2_gridobs[i,j,ind_mean1m]))[1]
              ENDFOR
            ENDFOR
            sig2D_02[ WHERE( sig2D_02 LE 0.10 ) ] = 1. & sig2D_02[ WHERE( sig2D_02 NE 1. ) ] = !VALUES.F_NAN
            help, sig2D_02
            sig2D_12 = FLTARR( n_elements(lon_0), n_elements(lat_0) )
            FOR j = 0, n_elements(lat_0)-1 DO BEGIN
              FOR i = 0, n_elements(lon_0)-1 DO BEGIN
                sig2D_12[i,j] = ( TM_TEST( var_1_gridobs[i,j,ind_mean1m], var_2_gridobs[i,j,ind_mean1m]))[1]
              ENDFOR
            ENDFOR
            sig2D_12[ WHERE( sig2D_12 LE 0.10 ) ] = 1. & sig2D_12[ WHERE( sig2D_12 NE 1. ) ] = !VALUES.F_NAN
            help, sig2D_12
          ENDIF
          IF n_elements(exp_list) GE 4 AND nbyear_0 EQ nbyear_min THEN BEGIN
            sig2D_03 = FLTARR( n_elements(lon_0), n_elements(lat_0) )
            FOR j = 0, n_elements(lat_0)-1 DO BEGIN
              FOR i = 0, n_elements(lon_0)-1 DO BEGIN
                sig2D_03[i,j] = ( TM_TEST( var_0[i,j,ind_mean1m], var_3_gridobs[i,j,ind_mean1m]))[1]
              ENDFOR
            ENDFOR
            sig2D_03[ WHERE( sig2D_03 LE 0.10 ) ] = 1. & sig2D_03[ WHERE( sig2D_03 NE 1. ) ] = !VALUES.F_NAN
            help, sig2D_03
            sig2D_23 = FLTARR( n_elements(lon_0), n_elements(lat_0) )
            FOR j = 0, n_elements(lat_0)-1 DO BEGIN
              FOR i = 0, n_elements(lon_0)-1 DO BEGIN
                sig2D_23[i,j] = ( TM_TEST( var_2_gridobs[i,j,ind_mean1m], var_3_gridobs[i,j,ind_mean1m]))[1]
              ENDFOR
            ENDFOR
            sig2D_23[ WHERE( sig2D_23 LE 0.10 ) ] = 1. & sig2D_23[ WHERE( sig2D_23 NE 1. ) ] = !VALUES.F_NAN
            help, sig2D_23
            sig2D_13 = FLTARR( n_elements(lon_0), n_elements(lat_0) )
            FOR j = 0, n_elements(lat_0)-1 DO BEGIN
              FOR i = 0, n_elements(lon_0)-1 DO BEGIN
                sig2D_13[i,j] = ( TM_TEST( var_1_gridobs[i,j,ind_mean1m], var_3_gridobs[i,j,ind_mean1m]))[1]
              ENDFOR
            ENDFOR
            sig2D_13[ WHERE( sig2D_13 LE 0.10 ) ] = 1. & sig2D_13[ WHERE( sig2D_13 NE 1. ) ] = !VALUES.F_NAN
            help, sig2D_13
          ENDIF
        ENDIF ; spatial sig test
      ENDIF ; nbyear > 1
    ENDIF ; 1m DATA


;-------------------------------------------------------------------------------------------------
; 1m / 1d / 6h STATS
;-------------------------------------------------------------------------------------------------

    ; CORRELATION 1D + BIAS + RMSE
    IF data_type NE 'c1m' THEN BEGIN
      FOR e = 1, n_elements(exp_list)-1 DO BEGIN
        cmd = execute( 'indok = listmatch(time_0,time_'+strtrim(e,2)+')' )
        cmd = execute( 'corrts_0'+strtrim(e,2)+' = LINCORR( var_ts_0[indok[*,0]], var_ts_'+strtrim(e,2)+'[indok[*,1]] )' )
        cmd = execute( 'bias_'+strtrim(e,2)+' = (MEAN( var_ts_'+strtrim(e,2)+'[indok[*,1]], /NAN)-MEAN( var_ts_0[indok[*,0]], /NAN)) / MEAN( var_ts_0[indok[*,0]], /NAN)*100.' )
        cmd = execute( 'rmse_'+strtrim(e,2)+' = SQRT( TOTAL( (var_ts_'+strtrim(e,2)+'[indok[*,1]] - var_ts_0[indok[*,0]])^2, /NAN) / n_elements(var_ts_0[indok[*,0]])  )' )
        cmd = execute( 'indok = WHERE( FINITE('+'var_mean_0) EQ 1 AND FINITE('+'var_mean_'+strtrim(e,2)+'_gridobs) EQ 1, nbok)' )
        cmd = execute( 'corrpat_0'+strtrim(e,2)+' = LINCORR( (REFORM('+'var_mean_0, n_elements('+'var_mean_0)))[indok], (REFORM('+'var_mean_'+strtrim(e,2)+'_gridobs, n_elements('+'var_mean_'+strtrim(e,2)+'_gridobs)))[indok] )' )
        cmd = execute( 'help, corrts_0'+strtrim(e,2)+', corrpat_0'+strtrim(e,2))

      ENDFOR
      print, 'CORR1D + CORR2D + BIAS + RMSE OK'
    ENDIF


;-------------------------------------------------------------------------------------------------
; WRITE STATS TO ASCII FILE
;-------------------------------------------------------------------------------------------------

    IF write_ascii AND data_type NE '1d' AND data_type NE '6h' THEN BEGIN
      IF force_seamask EQ 0 AND force_landmask EQ 0 THEN mask_name = 'NO'
      IF force_seamask AND force_highmask EQ 0 THEN mask_name = 'SEA'
      IF force_seamask AND force_highmask      THEN mask_name = 'SEA+HIGH'
      IF force_landmask                        THEN mask_name = 'LAND'
      PRINTF, file_lun, 'STATISTICS FOR VAR:'+var_name+' DOMAIN:'+zone+' PERIOD:'+period+' DATA_TYPE:'+data_type+' MASK:'+mask_name
      PRINTF, file_lun, 'EXP CORR1D BIAS(%) RMSE CORR2D'
      FOR e = 1, n_elements(exp_list)-1 DO cmd = execute( 'PRINTF, file_lun, exp_list[e], corrts_0'+strtrim(e,2)+', bias_'+strtrim(e,2)+', rmse_'+strtrim(e,2)+', corrpat_0'+strtrim(e,2) )
      PRINTF, file_lun, 'OK'
    ENDIF


print, 'OK' & print, ''
