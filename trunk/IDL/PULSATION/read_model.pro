
flag_obs = 0 & help, flag_obs



;-------------------------------------------------------------------------------------------------
; FILE & TIME DEFINITIONS + INIT
;-------------------------------------------------------------------------------------------------

@def_model_time
@def_model_gridmask
; TC SPECIAL CASE
IF plot_TC THEN BEGIN
  @read_TC_tracker
ENDIF



;-------------------------------------------------------------------------------------------------
; LOAD DATA FROM IDL FILE
;-------------------------------------------------------------------------------------------------

@def_save_dir
@restore_data


; SAUVEGARDE (NECESSAIRE POUR INTERPOLATION)
  lon_mod   = glamt[firstxt:lastxt,0] & help, lon_mod
  lat_mod   = REFORM(gphit[0,firstyt:lastyt]) & help, lat_mod
  nblon_mod = nxt
  nblat_mod = nyt
  cmd = execute( 'lon_'+STRTRIM(e,2)+'     = glamt[firstxt:lastxt,0] & help, lon_'+STRTRIM(e,2) )
  cmd = execute( 'lat_'+STRTRIM(e,2)+'     = REFORM(gphit[0,firstyt:lastyt]) & help, lat_'+STRTRIM(e,2) )
  cmd = execute( 'grid_'+STRTRIM(e,2)+'    = "mod"' )
  cmd = execute( 'nbmonth_'+STRTRIM(e,2)+' = nbmonth_mod' )



;-------------------------------------------------------------------------------------------------
; c1m DATA
;-------------------------------------------------------------------------------------------------

IF load_data EQ 0 AND load_gridobs EQ 0 THEN BEGIN

  IF data_type EQ 'c1m' THEN BEGIN

    ; FILE DEFINITION + DATA
    IF model EQ 'wrf'  OR model EQ 'now'  THEN file = exp_name+'_c1m_'+strtrim(yearini_modfile,2)+'_'+strtrim(yearend_modfile,2)+'_PLEV.nc'
    IF model EQ 'nemo' OR flag_nemo THEN BEGIN
      IF var_name EQ 'THETAO' OR var_name EQ 'SO'  THEN file = exp_name+'_c1m_'+strtrim(yearini_modfile,2)+'_'+strtrim(yearend_modfile,2)+'_grid_T_3D.nc' $
                                                   ELSE file = exp_name+'_c1m_'+strtrim(yearini_modfile,2)+'_'+strtrim(yearend_modfile,2)+'_grid_T_2D.nc'
      IF grid EQ 'ind025' THEN file = exp_name+'_c1m_'+strtrim(yearini_modfile,2)+'01_'+strtrim(yearend_modfile,2)+'12_grid_T_3D.nc'
    ENDIF
    help, file
    @get_model_data

    ; VERTICAL CALCUL
    IF flag_z THEN BEGIN
      IF flag_nemo EQ 0 THEN domdef, [box, 0, 20], /ZINDEX, /MEMEINDICES
      var[WHERE(var GT 1.e+30)] = !VALUES.F_NAN
      @calcul_vertical
    ENDIF

    ; 2D MASK
    @apply_monthly_mask
    help, var

    ; 4D MASK + VERTICAL PROFILE
    IF flag_z THEN BEGIN
      IF flag_mask AND force_landmask THEN FOR m = 0, nbmonth_mod-1 DO FOR z = 0, nzt-1 DO var4D[*,*,z,m] = var4D[*,*,z,m] * landmask
      IF flag_mask AND force_seamask  THEN FOR m = 0, nbmonth_mod-1 DO FOR z = 0, nzt-1 DO var4D[*,*,z,m] = var4D[*,*,z,m] * seamask
      IF               force_highmask THEN FOR m = 0, nbmonth_mod-1 DO FOR z = 0, nzt-1 DO var4D[*,*,z,m] = var4D[*,*,z,m] * highmask
      jpt = nbmonth_mod
      var_zx = GROSSEMOYENNE( var4D, 'y', /NAN)
      var_zy = GROSSEMOYENNE( var4D, 'x', /NAN)
      var_zp = GROSSEMOYENNE( TEMPORARY(var4D), 'xy', /NAN)
      help, var_zp, var_zx, var_zy
      IF var_name EQ 'UV' THEN BEGIN
        varx_zp = GROSSEMOYENNE( TEMPORARY(varx4D), 'xy', /NAN)
        vary_zp = GROSSEMOYENNE( TEMPORARY(vary4D), 'xy', /NAN)
        help, varx_zp, vary_zp
      ENDIF
    ENDIF

    ; SEASONAL AVERAGE
    IF n_elements(ind_period) GT 1 THEN BEGIN
      var_mean = MEANT(var, ind_period)
      IF STRMATCH(var_name, 'UV*') OR var_name EQ 'STRESS' OR var_name EQ 'RAIN' THEN BEGIN
        varx_mean = MEANT(varx, ind_period)
        vary_mean = MEANT(vary, ind_period)
        help, varx_mean, vary_mean
        IF force_highmask THEN BEGIN
          varx_mean = varx_mean * highmask
          vary_mean = vary_mean * highmask
        ENDIF
      ENDIF
      IF flag_z THEN BEGIN
        var_zp_mean = MEAN( var_zp[*,ind_period], DIMENSION=2, /NAN)
        var_zx_mean = MEAN( var_zx[*,*,ind_period], DIMENSION=3, /NAN)
        var_zy_mean = MEAN( var_zy[*,*,ind_period], DIMENSION=3, /NAN)
        IF var_name EQ 'UV' THEN BEGIN
          varx_zp_mean = MEAN( varx_zp[*,ind_period], DIMENSION=2, /NAN)
          vary_zp_mean = MEAN( vary_zp[*,ind_period], DIMENSION=2, /NAN)
        ENDIF
      ENDIF
    ENDIF ELSE BEGIN
      var_mean = var[*,*,ind_period]
      IF STRMATCH(var_name, 'UV*') OR var_name EQ 'STRESS' OR var_name EQ 'RAIN' THEN BEGIN
        varx_mean = varx[*,*,ind_period]
        vary_mean = vary[*,*,ind_period]
        help, varx_mean, vary_mean
      ENDIF
      IF flag_z THEN BEGIN
        var_zp_mean = var_zp[*,ind_period]
        var_zx_mean = var_zx[*,ind_period]
        var_zy_mean = var_zy[*,ind_period]
        IF var_name EQ 'UV' THEN BEGIN
          varx_zp_mean = varx_zp[*,ind_period]
          vary_zp_mean = vary_zp[*,ind_period]
        ENDIF
      ENDIF
    ENDELSE
    ano_mean = var_mean - MEAN(var, DIMENSION=3, /NAN)
    help, var_mean, ano_mean
    IF flag_z THEN help, var_zp_mean, var_zx_mean, var_zy_mean

    ; CALCUL SC1M OBS
    var_sc1m = fltarr(12)
    FOR m = 0, 12-1 DO var_sc1m[m] = MEAN(var[*,*,m], /NAN)

  ENDIF ; c1m DATA


;-------------------------------------------------------------------------------------------------
; 1m+1d+6h DATA
;-------------------------------------------------------------------------------------------------

  IF data_type NE 'c1m' THEN BEGIN

    ; DECLARATIONS
    IF data_type EQ '6h' THEN BEGIN
      var_ts = FLTARR(nbdt_mod)*!VALUES.F_NAN
      ano_ts = FLTARR(nbdt_mod)*!VALUES.F_NAN
    ENDIF
    IF data_type EQ '1d' THEN BEGIN
      var_ts = FLTARR(nbday_mod)*!VALUES.F_NAN
      ano_ts = FLTARR(nbday_mod)*!VALUES.F_NAN
    ENDIF
    IF data_type EQ '1m' THEN BEGIN
      var_ts     = FLTARR( nbmonth_mod)*!VALUES.F_NAN
      ano_ts     = FLTARR( nbmonth_mod)*!VALUES.F_NAN
      var_year   = FLTARR( nxt, nyt, nbyear_mod)
      var_sdc1m  = FLTARR( 12)
      var_sc1m   = FLTARR( 12)
      var2D_sc1m = FLTARR( nxt, nyt, 12)
    ENDIF
    var_mean = !NULL
    IF STRMATCH(var_name, 'UV*') OR var_name EQ 'STRESS' OR var_name EQ 'RAIN' THEN BEGIN
      varx_mean = !NULL
      vary_mean = !NULL
    ENDIF
    IF flag_z THEN BEGIN
      var_zp_mean = !NULL
      var_zx_mean = !NULL
      var_zy_mean = !NULL
      IF var_name EQ 'UV' THEN BEGIN
        varx_zp_mean = !NULL
        vary_zp_mean = !NULL
      ENDIF
    ENDIF


;-------------------------------------------------------------------------------------------------
; 1m FULL FILES (TROP075+TROP025)
;-------------------------------------------------------------------------------------------------

    IF (model EQ 'wrf' OR model EQ 'now') AND data_type EQ '1m' AND grid NE 'trop12' $
    ; EXCEPTIONS & RUN EN COURS...
    AND exp_name NE 'tr025_bmj02sfc5' $
    AND exp_name NE 'tr075_ra12L60_sol094sstnow' $
    AND exp_name NE 'tr075_ra12L60_sol094sstclim' $
    THEN BEGIN

      ; READ DATA
      IF flag_nemo EQ 0 THEN file = exp_name+'_'+data_type+'_'+strtrim(yearini_modfile,2)+'01_'+strtrim(yearend_modfile,2)+'12_PLEV.nc' $
                        ELSE file = exp_name+'_'+data_type+'_'+strtrim(yearini_modfile,2)+'01_'+strtrim(yearend_modfile,2)+'12_grid_T_3D.nc'
      @get_model_data
      help, var, flag_z

      ; VERTICAL AVERAGE
      IF flag_z THEN BEGIN
        domdef, [box, 0, 20], /ZINDEX, /MEMEINDICES
        var[WHERE(var GT 1.e+30)] = !VALUES.F_NAN
        @calcul_vertical
      ENDIF

      ; APPLY MASK 2D
      help, force_landmask, force_seamask, force_highmask
      IF force_landmask THEN FOR m = 0, nbmonth_mod-1 DO var[*,*,m] = var[*,*,m] * landmask
      IF force_seamask  THEN FOR m = 0, nbmonth_mod-1 DO var[*,*,m] = var[*,*,m] * seamask
      IF force_highmask THEN FOR m = 0, nbmonth_mod-1 DO var[*,*,m] = var[*,*,m] * highmask
      IF var_name EQ 'SST' THEN FOR m = 0, nbmonth_mod-1 DO var[*,*,m] = var[*,*,m] * landmask

      ; 4D MASK + VERTICAL PROFILE
      IF flag_z THEN BEGIN
        IF flag_mask AND force_landmask THEN FOR m = 0, nbmonth_obs-1 DO FOR z = 0, nzt-1 DO var4D[*,*,z,m] = var4D[*,*,z,m] * landmask
        IF flag_mask AND force_seamask  THEN FOR m = 0, nbmonth_obs-1 DO FOR z = 0, nzt-1 DO var4D[*,*,z,m] = var4D[*,*,z,m] * seamask
        IF               force_highmask THEN FOR m = 0, nbmonth_obs-1 DO FOR z = 0, nzt-1 DO var4D[*,*,z,m] = var4D[*,*,z,m] * highmask
        jpt = nbmonth_mod
        var_zx = GROSSEMOYENNE( var4D, 'y', /NAN)
        var_zy = GROSSEMOYENNE( var4D, 'x', /NAN)
        var_zp = GROSSEMOYENNE( TEMPORARY(var4D), 'xy', /NAN)
        help, var_zp, var_zx, var_zy
        IF var_name EQ 'UV' THEN BEGIN
          varx_zp = GROSSEMOYENNE( TEMPORARY(varx4D), 'xy', /NAN)
          vary_zp = GROSSEMOYENNE( TEMPORARY(vary4D), 'xy', /NAN)
          help, varx_zp, vary_zp
        ENDIF
      ENDIF

      ; SEASONAL CLIMATOLOGY
      IF n_elements(ind_mean1m) GT 1 THEN BEGIN
        ind_mean1m = ind_mean1m[ WHERE( ind_mean1m NE -1)] & help, ind_mean1m
        var_mean = MEAN(var[*,*,ind_mean1m], DIMENSION=3, /NAN)
        IF STRMATCH(var_name, 'UV*') OR var_name EQ 'STRESS' OR var_name EQ 'RAIN' THEN BEGIN
          varx_mean = MEAN(varx[*,*,ind_mean1m], DIMENSION=3, /NAN)
          vary_mean = MEAN(vary[*,*,ind_mean1m], DIMENSION=3, /NAN)
          help, varx_mean, vary_mean
        ENDIF
        IF flag_z THEN BEGIN
          var_zp_mean = MEAN( var_zp[*,ind_mean1m], DIMENSION=2, /NAN)
          var_zx_mean = MEAN( var_zx[*,*,ind_mean1m], DIMENSION=3, /NAN)
          var_zy_mean = MEAN( var_zy[*,*,ind_mean1m], DIMENSION=3, /NAN)
        ENDIF
        IF flag_z AND var_name EQ 'UV' THEN BEGIN
          varx_zp_mean = MEAN( varx_zp[*,ind_mean1m], DIMENSION=2, /NAN)
          vary_zp_mean = MEAN( vary_zp[*,ind_mean1m], DIMENSION=2, /NAN)
        ENDIF
      ENDIF ELSE BEGIN
        var_mean = var[*,*,ind_mean1m]
        IF STRMATCH(var_name, 'UV*') OR var_name EQ 'STRESS' OR var_name EQ 'RAIN' THEN BEGIN
          varx_mean = varx[*,*,ind_mean1m]
          vary_mean = vary[*,*,ind_mean1m]
          help, varx_mean, vary_mean
        ENDIF
        IF flag_z THEN BEGIN
          var_zp_mean = var_zp[*,ind_mean1m]
          var_zx_mean = var_zx[*,ind_mean1m]
          var_zy_mean = var_zy[*,ind_mean1m]
        ENDIF
        IF flag_z AND var_name EQ 'UV' THEN BEGIN
          varx_zp_mean = varx_zp[*,ind_mean1m]
          vary_zp_mean = vary_zp[*,ind_mean1m]
        ENDIF
      ENDELSE
      ano_mean = var_mean - MEAN(var, DIMENSION=3, /NAN)
      help, var_mean, ano_mean
      IF flag_z THEN help, var_zp_mean, var_zx_mean, var_zy_mean

      ; MONTHLY TIME SERIES
      FOR m = 0, nbmonth_mod-1 DO var_ts[m] = MEAN(var[*,*,m], /NAN)
      FOR m = 0, nbmonth_mod-1 DO ano_ts[m] = var_ts[m] - MEAN(var_ts, /NAN)
      help, var_ts, ano_ts

      ; MONTHLY STANDARD DEVIATION + SEASONAL CYCLE
      FOR m = 0, 12-1 DO BEGIN
        ind_month = findgen(nbyear_mod)*12+m
        var_sdc1m[m]      = STDDEV( var_ts[ind_month], /NAN)
        var_sc1m[m]       = MEAN( var_ts[ind_month], /NAN)
        var2D_sc1m[*,*,m] = MEAN( var[*,*,ind_month], DIMENSION=3, /NAN)
      ENDFOR
      help, var_sc1m, var_sdc1m, var2D_sc1m

      @calcul_1m_diags

    ENDIF ; 1m FULL FILES


;-------------------------------------------------------------------------------------------------
; 1m+1d+6h YEARLY FILES
;-------------------------------------------------------------------------------------------------

    IF  ( (model EQ 'wrf' OR model EQ 'now') $
    ;AND ( (data_type EQ '1d' AND (grid EQ 'trop075' OR grid EQ 'ind025') ) $
    AND ( (data_type EQ '1d' AND (grid EQ 'trop075' OR grid EQ 'trop025' OR grid EQ 'ind025') ) $
    OR    (data_type EQ '1m' AND grid EQ 'trop12'  )      $
    OR    (data_type EQ '1m' AND grid EQ 'trop025' AND exp_name EQ 'tr025_bmj02sfc5' ) $
    OR    (data_type EQ '6h' AND grid EQ 'trop075' ) ) ) $
    ;OR  (  model EQ 'nemo'   AND grid NE 'trop075' )  THEN BEGIN

    ; RUN EN COURS...
    OR    (data_type EQ '1m' AND grid EQ 'trop075' AND exp_name EQ 'tr075_ra12L60_sol094sstnow' ) $
    OR    (data_type EQ '1m' AND grid EQ 'trop075' AND exp_name EQ 'tr075_ra12L60_sol094sstclim' ) $
    THEN BEGIN

      IF data_type EQ '1d' THEN cur_nbday = 0
      IF data_type EQ '6h' THEN cur_nbdt  = 0
      IF data_type EQ '1m' OR data_type EQ '1d' THEN BEGIN
        tmp = !NULL & tmpx = !NULL & tmpy = !NULL
      ENDIF


      FOR y = 0, nbyear_mod-1 DO BEGIN

        ; LECTURE 1m+1d+6h YEARLY
        year = yearini_mod + y & help, year
        IF model EQ 'wrf'  OR model EQ 'now'  THEN BEGIN
          IF data_type EQ '1d' OR data_type EQ '6h' THEN file = exp_name+'_'+data_type+'_'+strtrim(year,2)+'0101_'+strtrim(year,2)+'1231_PLEV.nc'
          IF data_type EQ '1m' THEN file = exp_name+'_'+data_type+'_'+strtrim(year,2)+'01_'+strtrim(year,2)+'12_PLEV.nc'
          IF data_type EQ '1m' AND exp_name EQ 'tr12_kf02' THEN file = exp_name+'_'+data_type+'_'+strtrim(year,2)+'01_'+strtrim(year,2)+'05_PLEV.nc'
          IF data_type EQ '1d' THEN nbday_year = N_ELEMENTS( WHERE( FIX( time_mod / 10000.00d) EQ year) )
        ENDIF
        IF model EQ 'nemo' OR flag_nemo THEN BEGIN
          IF data_type EQ '1d' THEN file = exp_name+'_'+data_type+'_'+strtrim(year,2)+'0101_'+strtrim(year,2)+'1231_grid_T.nc'
          IF data_type EQ '1m' THEN file = exp_name+'_'+data_type+'_'+strtrim(year,2)+'01_'+strtrim(year,2)+'12_grid_T_2D.nc'
        ENDIF
        @get_model_data
        help, var


;-------------------------------------------------------------------------------------------------
; 1m YEARLY FILES (TROP12 + RUNNING EXPS)
;-------------------------------------------------------------------------------------------------

        ; WRF 1m YEARLY FILES
        IF data_type EQ '1m' THEN BEGIN

          ; VERTICAL CALCUL
          IF flag_z THEN BEGIN
            domdef, [box, 0, 20], /ZINDEX, /MEMEINDICES
            var[WHERE(var GT 1.e+30)] = !VALUES.F_NAN
            @calcul_vertical
          ENDIF

          ; MASK 2D
          IF data_type EQ '1m' OR data_type EQ '1d' THEN BEGIN
            @apply_monthly_mask
          ENDIF

          ; 4D MASK + VERTICAL PROFILE
          IF flag_z THEN BEGIN
            IF flag_mask AND force_landmask THEN FOR m = 0, nbmonth_obs-1 DO FOR z = 0, nzt-1 DO var4D[*,*,z,m] = var4D[*,*,z,m] * landmask
            IF flag_mask AND force_seamask  THEN FOR m = 0, nbmonth_obs-1 DO FOR z = 0, nzt-1 DO var4D[*,*,z,m] = var4D[*,*,z,m] * seamask
            IF force_highmask               THEN FOR m = 0, nbmonth_obs-1 DO FOR z = 0, nzt-1 DO var4D[*,*,z,m] = var4D[*,*,z,m] * highmask
            jpt = nbmonth_obs
            var_zx = GROSSEMOYENNE( var4D, 'y', /NAN)
            var_zy = GROSSEMOYENNE( var4D, 'x', /NAN)
            var_zp = GROSSEMOYENNE( TEMPORARY(var4D), 'xy', /NAN)
            help, var_zp, var_zx, var_zy
            IF var_name EQ 'UV' THEN BEGIN
              varx_zp = GROSSEMOYENNE( TEMPORARY(varx4D), 'xy', /NAN)
              vary_zp = GROSSEMOYENNE( TEMPORARY(vary4D), 'xy', /NAN)
              help, varx_zp, vary_zp
            ENDIF
          ENDIF

          ; SEASONAL AVERAGE + INTERANNUAL SEASONAL AVERAGE (1m ONLY)
          IF ind_mean1m[0] NE -1 THEN BEGIN
            IF n_elements(ind_period) GT 1 THEN BEGIN
               varmean = MEAN(var[*,*,ind_period], DIMENSION=3, /NAN)
               IF STRMATCH(var_name, 'UV*') OR var_name EQ 'STRESS' OR var_name EQ 'RAIN' THEN BEGIN
                varxmean = MEAN(varx[*,*,ind_period], DIMENSION=3, /NAN)
                varymean = MEAN(vary[*,*,ind_period], DIMENSION=3, /NAN)
              ENDIF
              IF flag_z THEN BEGIN
                varzpmean = MEAN( var_zp[*,ind_period], DIMENSION=2, /NAN)
                varzxmean = MEAN( var_zx[*,*,ind_period], DIMENSION=3, /NAN)
                varzymean = MEAN( var_zy[*,*,ind_period], DIMENSION=3, /NAN)
              ENDIF
              IF flag_z AND var_name EQ 'UV' THEN BEGIN
                varxzpmean = MEAN( varx_zp[*,ind_period], DIMENSION=2, /NAN)
                varyzpmean = MEAN( vary_zp[*,ind_period], DIMENSION=2, /NAN)
              ENDIF
            ENDIF ELSE BEGIN
              varmean = var[*,*,ind_period]
              IF STRMATCH(var_name, 'UV*') OR var_name EQ 'STRESS' OR var_name EQ 'RAIN' THEN BEGIN
                varxmean = varx[*,*,ind_period]
                varymean = vary[*,*,ind_period]
              ENDIF
              IF flag_z THEN BEGIN
                varzpmean = var_zp[*,ind_period]
                varzxmean = var_zx[*,*,ind_period]
                varzymean = var_zy[*,*,ind_period]
              ENDIF
              IF flag_z AND var_name EQ 'UV' THEN BEGIN
                varxzpmean = varx_zp[*,ind_period]
                varyzpmean = vary_zp[*,ind_period]
              ENDIF
            ENDELSE
            IF n_elements(ind_period) GT 1 THEN var_year[*,*,y] = MEAN(var[*,*,ind_period], DIMENSION=3, /NAN) $
            ELSE var_year[*,*,y] = var[*,*,ind_period]
            help, var_year, varmean
            IF flag_z THEN help, varzpmean
          ENDIF

          ; MONTHLY TIME SERIES
          FOR m = 0, MIN([nbmonth_mod,12])-1 DO var_ts[y*12+m] = MEAN(var[*,*,m], /NAN)
          help, var_ts

          ; CONCATENATE VAR
          tmp = [ [[tmp]], [[var]] ] & help, tmp
          IF STRMATCH(var_name, 'UV*') OR var_name EQ 'STRESS' OR var_name EQ 'RAIN' THEN BEGIN
            tmpx = [ [[tmpx]], [[varx]] ]
            tmpy = [ [[tmpy]], [[vary]] ]
          ENDIF

        ENDIF ; 1m


;-------------------------------------------------------------------------------------------------
;WRF 1d YEARLY FILES (= TROP075 & TROP025)
;-------------------------------------------------------------------------------------------------

        IF data_type EQ '1d' THEN BEGIN

          ; MASK
          nbday_year = n_elements(var[0,0,*]) & help, nbday_year
          ;IF (fym_only OR fyc_only OR fyo_only) THEN BEGIN
            IF force_landmask THEN FOR d = 0, nbday_year-1 DO var[*,*,d] = var[*,*,d]*landmask
            IF force_seamask  THEN FOR d = 0, nbday_year-1 DO var[*,*,d] = var[*,*,d]* seamask
            IF force_highmask THEN FOR d = 0, nbday_year-1 DO var[*,*,d] = var[*,*,d]* highmask
            IF var_name EQ 'UV10' OR var_name EQ 'STRESS' OR var_name EQ 'RAIN' THEN BEGIN
              IF force_landmask THEN FOR d = 0, nbday_year-1 DO BEGIN
                varx[*,*,d] = varx[*,*,d]*landmask
                vary[*,*,d] = vary[*,*,d]*landmask
              ENDFOR
              IF force_seamask  THEN FOR d = 0, nbday_year-1 DO BEGIN
                varx[*,*,d] = varx[*,*,d]*seamask
                vary[*,*,d] = vary[*,*,d]*seamask
              ENDFOR
              IF force_highmask  THEN FOR d = 0, nbday_year-1 DO BEGIN
                varx[*,*,d] = varx[*,*,d]*highmask
                vary[*,*,d] = vary[*,*,d]*highmask
              ENDFOR
            ENDIF
          ;ENDIF

          ; TIME_SERIES + MASK
          IF force_landmask THEN FOR d = 0, nbday_year-1 DO var_ts[cur_nbday+d] = MEAN(var[*,*,d]*landmask, /NAN)
          IF force_seamask  THEN FOR d = 0, nbday_year-1 DO var_ts[cur_nbday+d] = MEAN(var[*,*,d]* seamask, /NAN)
          IF force_highmask THEN FOR d = 0, nbday_year-1 DO var_ts[cur_nbday+d] = MEAN(var[*,*,d]* highmask, /NAN)
          IF force_landmask EQ 0 AND force_seamask EQ 0 AND force_highmask EQ 0 THEN FOR d = 0, nbday_year-1 DO var_ts[cur_nbday+d] = MEAN(var[*,*,d], /NAN)
          help, var_ts

          ; MEAN_2D + MASK
          IF n_elements(ind_mean1d) GT 1 THEN BEGIN
            ind_mean1d_cur = ind_mean1d[ WHERE( FIX( time_mod[ind_mean1d] / 10000.) EQ year)] - cur_nbday & help, ind_mean1d_cur
            varmean = MEAN(var[*,*,ind_mean1d_cur], DIMENSION=3, /NAN)
            IF var_name EQ 'UV10' OR var_name EQ 'STRESS' OR var_name EQ 'RAIN' THEN BEGIN
              varxmean = MEAN(varx[*,*,ind_mean1d_cur], DIMENSION=3, /NAN)
              varymean = MEAN(vary[*,*,ind_mean1d_cur], DIMENSION=3, /NAN)
            ENDIF
            IF force_landmask THEN varmean = varmean * landmask
            IF force_seamask  THEN varmean = varmean * seamask
            IF force_highmask THEN varmean = varmean * highmask
            help, varmean
          ENDIF ELSE BEGIN
            plot_2D = 0 
            varmean = !NULL
            IF var_name EQ 'UV10' OR var_name EQ 'STRESS' OR var_name EQ 'RAIN' THEN BEGIN
              varxmean = !NULL
              varymean = !NULL
            ENDIF
          ENDELSE
          cur_nbday  = cur_nbday + nbday_year

          ; CONCATENATE VAR
          tmp = [ [[tmp]], [[var]] ] & help, tmp
          IF STRMATCH(var_name, 'UV*') OR var_name EQ 'STRESS' OR var_name EQ 'RAIN' THEN BEGIN
            tmpx = [ [[tmpx]], [[varx]] ]
            tmpy = [ [[tmpy]], [[vary]] ]
          ENDIF

        ENDIF ; WRF 1d YEARLY FILES


;-------------------------------------------------------------------------------------------------
;WRF 6h YEARLY FILES (= TROP075)
;-------------------------------------------------------------------------------------------------

        IF data_type EQ '6h' THEN BEGIN

          ; MASK
          nbdt_year = n_elements(var[0,0,*]) & help, nbdt_year
          IF (fym_only OR fyc_only OR fyo_only) THEN BEGIN
            IF force_landmask THEN FOR d = 0, nbdt_year-1 DO var[*,*,d] = var[*,*,d]*landmask
            IF force_seamask  THEN FOR d = 0, nbdt_year-1 DO var[*,*,d] = var[*,*,d]* seamask
            IF var_name EQ 'UV10' OR var_name EQ 'STRESS' OR var_name EQ 'RAIN' THEN BEGIN
              IF force_landmask THEN FOR d = 0, nbdt_year-1 DO BEGIN
                varx[*,*,d] = varx[*,*,d]*landmask
                vary[*,*,d] = vary[*,*,d]*landmask
              ENDFOR
              IF force_seamask  THEN FOR d = 0, nbdt_year-1 DO BEGIN
                varx[*,*,d] = varx[*,*,d]*seamask
                vary[*,*,d] = vary[*,*,d]*seamask
              ENDFOR
            ENDIF
          ENDIF

          ; TIME_SERIES + MASK
          IF force_landmask THEN FOR d = 0, nbdt_year-1 DO var_ts[cur_nbdt+d] = MEAN(var[*,*,d]*landmask, /NAN)
          IF force_seamask  THEN FOR d = 0, nbdt_year-1 DO var_ts[cur_nbdt+d] = MEAN(var[*,*,d]* seamask, /NAN)
          IF force_landmask EQ 0 AND force_seamask EQ 0 THEN FOR d = 0, nbdt_year-1 DO var_ts[cur_nbdt+d] = MEAN(var[*,*,d], /NAN)
          help, var_ts
          cur_nbdt  = cur_nbdt + nbdt_year

          ; MEAN_2D + MASK
          IF (fym_only OR fyo_only) AND ind_mean6h[0] NE -1 THEN BEGIN
            varmean = MEAN(var[*,*,ind_mean6h], DIMENSION=3, /NAN)
            IF var_name EQ 'UV10' OR var_name EQ 'STRESS' OR var_name EQ 'RAIN' THEN BEGIN
              varxmean = MEAN(varx[*,*,ind_mean6h], DIMENSION=3, /NAN)
              varymean = MEAN(vary[*,*,ind_mean6h], DIMENSION=3, /NAN)
            ENDIF
            IF force_landmask THEN varmean = varmean * landmask
            IF force_seamask  THEN varmean = varmean * seamask
            IF force_highmask THEN varmean = varmean * highmask
            help, varmean
          ENDIF ELSE plot_2D = 0

        ENDIF ; WRF 6h YEARLY FILES


        ; CONCATENATION VAR_MEAN PAR ANNEE
        ;IF plot_2D THEN BEGIN
          IF flag_z THEN BEGIN
            var_zp_mean = [ [[var_zp_mean]], [[varzpmean]] ]
            var_zx_mean = [ [[var_zx_mean]], [[varzxmean]] ]
            var_zy_mean = [ [[var_zy_mean]], [[varzymean]] ]
          ENDIF
          IF flag_z AND var_name EQ 'UV' THEN BEGIN
            varx_zp_mean = [ [[varx_zp_mean]], [[varxzpmean]] ]
            vary_zp_mean = [ [[vary_zp_mean]], [[varyzpmean]] ]
          ENDIF
          var_mean = [ [[var_mean]], [[varmean]] ]
          IF STRMATCH(var_name, 'UV*') OR var_name EQ 'STRESS' OR var_name EQ 'RAIN' THEN BEGIN
            varx_mean = [ [[varx_mean]], [[varxmean]] ]
            vary_mean = [ [[vary_mean]], [[varymean]] ]
          ENDIF
        ;ENDIF

      ENDFOR ; LOOP YEAR

      IF data_type EQ '1m' OR data_type EQ '1d' THEN BEGIN
        var = TEMPORARY(tmp)
        IF STRMATCH(var_name, 'UV*') OR var_name EQ 'STRESS' OR var_name EQ 'RAIN' THEN BEGIN
          varx = TEMPORARY(tmpx)
          vary = TEMPORARY(tmpy)
        ENDIF
      ENDIF

      IF data_type EQ '1d' THEN BEGIN
        FOR d = 0, nbday_mod-1 DO ano_ts[d] = var_ts[d] - MEAN(var_ts, /NAN)
        help, ano_ts
        ; DAILY SEASONAL CYCLE (sc1d)
        var_sc1d = FLTARR(365)
        FOR d = 0, 365-1 DO BEGIN
          ind_day     = FINDGEN( nbyear_mod)*365 + d
          var_sc1d[d] = MEAN( var_ts[ind_day], /NAN)
        ENDFOR
        help, var_sc1d
      ENDIF

      IF data_type EQ '6h' THEN BEGIN
        FOR d = 0, nbdt_mod-1 DO ano_ts[d] = var_ts[d] - MEAN(var_ts, /NAN)
        help, ano_ts
      ENDIF

;-------------------------------------------------------------------------------------------------
; 1m YEARLY FILES (TROP12 + RUNNING EXPS)
;-------------------------------------------------------------------------------------------------

      ; TIME SERIES ANOMALIES
      IF data_type EQ '1m' THEN BEGIN
        FOR m = 0, nbmonth_mod-1 DO ano_ts[m] = var_ts[m] - MEAN(var_ts, /NAN)
        help, ano_ts

      ; SEASONAL AVERAGE
      IF nbyear_mod GT 1 THEN BEGIN
        var_mean = MEAN( TEMPORARY(var_mean), DIMENSION=3, /NAN)
        IF flag_z THEN BEGIN
          var_zp_mean = MEAN( TEMPORARY(var_zp_mean), DIMENSION=3, /NAN)
          var_zx_mean = MEAN( TEMPORARY(var_zx_mean), DIMENSION=3, /NAN)
          var_zy_mean = MEAN( TEMPORARY(var_zy_mean), DIMENSION=3, /NAN)
        ENDIF
        IF flag_z AND var_name EQ 'UV' THEN BEGIN
          varx_zp_mean = MEAN( TEMPORARY(varx_zp_mean), DIMENSION=3, /NAN)
          vary_zp_mean = MEAN( TEMPORARY(vary_zp_mean), DIMENSION=3, /NAN)
        ENDIF
        IF var_name EQ 'UV10' OR var_name EQ 'STRESS' OR var_name EQ 'RAIN' THEN BEGIN
          varx_mean = MEAN( TEMPORARY(varx_mean), DIMENSION=3, /NAN)
          vary_mean = MEAN( TEMPORARY(vary_mean), DIMENSION=3, /NAN)
          tmp = TEMPORARY(varxmean)
          tmp = TEMPORARY(varymean)
        ENDIF
      tmp = TEMPORARY(varmean)
      ENDIF
      help, var_mean
      IF flag_z THEN help, var_zp_mean, var_zx_mean, var_zy_mean
      IF var_name EQ 'UV10' OR var_name EQ 'STRESS' OR var_name EQ 'RAIN' THEN help, varx_mean, vary_mean

      ; MONTHLY STANDARD DEVIATION + SEASONAL CYCLE
      FOR m = 0, MIN([nbmonth_mod,12])-1 DO BEGIN
        ind_month    = FINDGEN(nbyear_mod)*12+m
        var_sdc1m[m] = STDDEV(var_ts[ind_month], /NAN)
        var_sc1m[m]  =   MEAN(var_ts[ind_month], /NAN)
      ENDFOR
      help, var_sc1m, var_sdc1m

      ENDIF

    ENDIF ; WRF+NEMO


;-------------------------------------------------------------------------------------------------
; WRF 1d MONTHLY FILES (TROP025 + TROP12)
;-------------------------------------------------------------------------------------------------

    ;IF (model EQ 'wrf' OR model EQ 'now') AND data_type EQ '1d' AND (grid EQ 'trop12' OR grid EQ 'trop025') THEN BEGIN
    IF (model EQ 'wrf' OR model EQ 'now') AND data_type EQ '1d' AND grid EQ 'trop12' THEN BEGIN

      cur_nbday = 0
      tmp = FLTARR(nxt,nyt,nbday_mod) & help, tmp
      IF var_name EQ 'UV10' OR var_name EQ 'STRESS' OR var_name EQ 'RAIN' THEN BEGIN
        tmpx = FLTARR(nxt,nyt,nbday_mod) & help, tmpx
        tmpy = FLTARR(nxt,nyt,nbday_mod) & help, tmpy
      ENDIF

      FOR y = 0, nbyear_mod-1 DO BEGIN
        year = yearini_mod + y & help, year
        FOR m = 1, 12 DO BEGIN
          IF m LT 10 THEN month = '0'+STRING(m, FORMAT='(I1)') ELSE month = STRING(m, FORMAT='(I2)')
          file = exp_name+'_'+data_type+'_'+strtrim(year,2)+month+'01_'+strtrim(year,2)+month+'**_PLEV.nc' & help, file
          nbday_month = N_ELEMENTS( WHERE( FIX( (time_mod - 19890000.00d) / 100.) EQ m))
          @get_model_data
          help, var
          IF var_name EQ 'UV10' OR var_name EQ 'STRESS' OR var_name EQ 'RAIN' THEN help, varx, vary
          ;nbday_month = n_elements(var[0,0,*]) & help, nbday_month
          ;IF (fym_only OR fyc_only OR fyo_only) THEN BEGIN
            IF force_landmask THEN FOR d = 0, nbday_month-1 DO var[*,*,d] = var[*,*,d]*landmask
            IF force_seamask  THEN FOR d = 0, nbday_month-1 DO var[*,*,d] = var[*,*,d]* seamask
          ;ENDIF
          tmp[*,*,cur_nbday:cur_nbday+nbday_month-1] = var
          IF var_name EQ 'UV10' OR var_name EQ 'STRESS' OR var_name EQ 'RAIN' THEN BEGIN
            ;IF (fym_only OR fyc_only OR fyo_only) THEN BEGIN
              IF force_landmask THEN FOR d = 0, nbday_month-1 DO BEGIN
                varx[*,*,d] = varx[*,*,d]*landmask
                vary[*,*,d] = vary[*,*,d]*landmask
              ENDFOR
              IF force_seamask THEN FOR d = 0, nbday_month-1 DO BEGIN
                varx[*,*,d] = varx[*,*,d]*seamask
                vary[*,*,d] = vary[*,*,d]*seamask
              ENDFOR
            ;ENDIF
            tmpx[*,*,cur_nbday:cur_nbday+nbday_month-1] = varx
            tmpy[*,*,cur_nbday:cur_nbday+nbday_month-1] = vary
          ENDIF
          cur_nbday = cur_nbday + nbday_month

        ENDFOR ; MONTH LOOP
      ENDFOR ; YEAR LOOP

      var = TEMPORARY(tmp)
      IF var_name EQ 'UV10' OR var_name EQ 'STRESS' OR var_name EQ 'RAIN' THEN BEGIN
        varx = TEMPORARY(tmpx)
        vary = TEMPORARY(tmpy)
      ENDIF

      ; TIME SERIES
      var_ts = MEAN(MEAN(var, DIMENSION=1, /NAN), DIMENSION=1, /NAN) & help, var_ts
      FOR d = 0, nbday_mod-1 DO ano_ts[d] = var_ts[d] - MEAN(var_ts, /NAN) & help, ano_ts

      ; SEASONAL AVERAGE
      ;IF (fym_only OR fyo_only OR fyc_only) AND n_elements(ind_mean1d) GT 1 THEN BEGIN
      IF n_elements(ind_mean1d) GT 1 THEN BEGIN
        var_mean = MEAN(var[*,*,ind_mean1d], DIMENSION=3, /NAN) & help, var_mean
        IF var_name EQ 'UV10' OR var_name EQ 'STRESS' OR var_name EQ 'RAIN' THEN BEGIN
          varx_mean = MEAN(varx[*,*,ind_mean1d], DIMENSION=3, /NAN) & help, varx_mean
          vary_mean = MEAN(vary[*,*,ind_mean1d], DIMENSION=3, /NAN) & help, vary_mean
        ENDIF
      ENDIF ELSE STOP

    ENDIF ; WRF 1d MONTHLY FILES


;-------------------------------------------------------------------------------------------------
; EXCEPTION NEMO TROP075
;-------------------------------------------------------------------------------------------------

    IF grid EQ 'trop0755' AND (model EQ 'nemo' OR flag_nemo) THEN BEGIN

      IF data_type EQ '1m' THEN file = exp_name+'_'+data_type+'_'+strtrim(yearini_mod,2)+'01_'+strtrim(yearend_mod,2)+'12_grid_T_2D.nc'
      IF data_type EQ '1d' THEN file = exp_name+'_'+data_type+'_'+strtrim(yearini_mod,2)+'0101_'+strtrim(yearend_mod,2)+'1231_grid_T.nc'
      help, file

      @get_model_data
      IF fym_only THEN nbmonth_mod = 12
      var = var[*,*,0:nbmonth_mod-1]
      help, var

      ; APPLY MASK
      IF data_type EQ '1m' THEN BEGIN
        @apply_monthly_mask
      ENDIF

      ; TIME SERIES 1D
      IF force_landmask THEN FOR m = 0, nbmonth_mod-1 DO var_ts[m] = MEAN(var[*,*,m]*landmask, /NAN)
      IF force_seamask  THEN FOR m = 0, nbmonth_mod-1 DO var_ts[m] = MEAN(var[*,*,m]* seamask, /NAN)
      IF force_landmask EQ 0 AND force_seamask EQ 0 THEN FOR m = 0, nbmonth_mod-1 DO var_ts[m] = MEAN(var[*,*,m], /NAN)

      ; MEAN 2D
      IF n_elements(ind_period) GT 1 THEN BEGIN
        var_mean = MEAN(var[*,*,ind_period], DIMENSION=3, /NAN)
        IF var_name EQ 'UV10' OR var_name EQ 'STRESS' OR var_name EQ 'RAIN' THEN BEGIN
          varx_mean = MEAN(varx[*,*,ind_period], DIMENSION=3, /NAN)
          vary_mean = MEAN(vary[*,*,ind_period], DIMENSION=3, /NAN)
        ENDIF
      ENDIF ELSE var_mean = var
      IF force_landmask THEN var_mean = var_mean * landmask
      IF force_seamask  THEN var_mean = var_mean * seamask
      help, var_mean

    ENDIF ; NEMO files

  ENDIF ; 1m+1d DATA

ENDIF ; RESTORE DATA FROM IDL FILE

print, 'READ_MODEL OK' & print, ''
IF debug THEN STOP
