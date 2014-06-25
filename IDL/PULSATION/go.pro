PRO go
COMPILE_OPT STRICTARR, STRICTARRSUBS



print, '' & print, 'SETUP...'
;-------------------------------------------------------------------------------------------------
; DATA TYPE: c1m / 1m / 1d / 6h / (c1d)
;-------------------------------------------------------------------------------------------------

data_type = '1m' & help, data_type


;-------------------------------------------------------------------------------------------------
; VARIABLES LIST
;-------------------------------------------------------------------------------------------------

; VARS 2D WRF     : RAIN / SST / UV10 / ROT_UV10 / T2 / TPOT2 / Q2 / OLR / LH / HFX / SKT / DELTAT / DELTAQ / THETAE / MSLP
;                   GSW=SW NET / SWDOWN / GLW=LW DOWN / LWR=LW NET / RAD=RADIATIVE BUDGET / NET=NET HEAT FLUX
; SURFACE BUDGET  : GSW, LWR, LH, HFX, NET / RAIN, SKT, Q2, T2
; VARS 3D WRF     : T, TPOT, TPOTE, RH, Q, UV, U, V, W, UVQ, WQ, UV10Q2
; VARS 2D NEMO    : SST(TOS), SOS, NET(TOHFLS), STRESS(TAUM), GSW(RSNTDS), MLDT, MLDR, BLT, SLA(ZOS), D20
; VARS 3D NEMO    : THETAO, SO
; VARS WRF STATIC : HEIGHT, LANDUSE, SOILCAT, SLOPE

var_list = [ 'SST', 'D20']
help, var_list & print, var_list


;-------------------------------------------------------------------------------------------------
; ZONES LIST
;-------------------------------------------------------------------------------------------------

zone_list = ['XIE']
help, zone_list & print, zone_list


;-------------------------------------------------------------------------------------------------
; PERIODS LIST: DJFM / JFM / JJA / JJAS / J2D / SONDJF / MONTH (3 first letters)
;-------------------------------------------------------------------------------------------------

period_list = ['DJFMA']
help, period_list & print, period_list


;-------------------------------------------------------------------------------------------------
; EXPERIENCES LIST
;-------------------------------------------------------------------------------------------------

;@def_exp_trop075
;@def_exp_trop025
;@def_exp_trop12

;exp_list = [ 'TROPFLUX', 'AVHRR', 'GLORYS2V1', 'ind025_cpl12_kf', 'ind025_cpl12_bmj']
exp_list = [ 'OBS'];, 'ind025_cpl12_bmj', 'ind025_ra12_bmj', 'ind025_cpl12_kf', 'ind025_ra12_kf']
help, exp_list & print, exp_list


;-------------------------------------------------------------------------------------------------
; TIME OPTIONS (6h / 1d / 1m ONLY)
;-------------------------------------------------------------------------------------------------

ayc_only       = 0  ; load ALL COMMON years
fym_only       = 0  ; load ONLY the first MODEL year
fyo_only       = 0  ; load ONLY the first OBS year
fyc_only       = 0  ; load the first x COMMON years
nbyear_load    = 5  ; define (x-1) number of years
fmr_only       = 1  ; define year range manually
manual_range   = [ 1982, 2012]


;-------------------------------------------------------------------------------------------------
; GLOBAL OPTIONS
;-------------------------------------------------------------------------------------------------

; i/o options
force_norestore  = 1      ; force la lecture netcdf (even if an idl data file already exists)
force_nomean_wrf = 0      ; lecture des variables "instant" wrf (meme si les "mean" sont dispos)
; mask options
force_landmask   = 1
force_seamask    = 0
force_highmask   = 0
highmask_height  = 2000.  ; height in meters
;zlayer_def       = [ 900., 500.] ; [1000/975 hPa - 200 hPa] ; DELTA_TT Xavier 8507: 600-850 hPa; USHEAR: 850-850hPa
zlayer_def       = [ 0., 200.] ; m (1500m max pour NIOA)
; vertical options
force_zmean      = 1     ; vertical average
force_zdiff      = 0     ; vertical shear
; debug
debug            = 0


;-------------------------------------------------------------------------------------------------
; DIAGS & PLOTS OPTIONS
;-------------------------------------------------------------------------------------------------

plot_2D     = 1 ; spatial plots
flag_sig    = 0 ; plot signifitive (90%) stuff only
plot_sc1m   = 1 ; 1m seasonal cycle
plot_ts     = 0 ; time series (1m/1d/6h)
smooth_coef = 5 ; 1d ONLY
plot_index  = 0 ; seasonal time series
plot_dt     = 0 ; monthly tendency
plot_hov    = 0 ; hovmuller
plot_zp     = 0 ; xy-mean vertical profile (c1m + 1m ONLY)

; experimental
plot_TC     = 1 ; TC diags
plot_sp     = 0 ; spectrum (1d ONLY)
plot_timefilt = 0 ; 1d time filtered data
iso_filt      = [10., 90.] ; intraseasonal definition (in days)
syn_filt      = [ 2., 10.] ; synoptic definition (in days)

; write plot files
write_gif   = 0
write_ps    = 0
write_ascii = 0
plot_dir    = 'FIGS_tr075now_long_sol0XX'


;-------------------------------------------------------------------------------------------------
; INITIALIZATION
;-------------------------------------------------------------------------------------------------

@all_cm
@def_plot_dir
@def_constants
;@def_ascii_file
print, 'SETUP OK' & print, ''
IF debug THEN STOP


;-------------------------------------------------------------------------------------------------
; LECTURE NETCDF
;-------------------------------------------------------------------------------------------------

FOR p = 0, n_elements(period_list)-1 DO BEGIN

period = period_list[p] & print, 'LOOP_PER: ', period
@def_periods

FOR z = 0, n_elements(zone_list)-1 DO BEGIN
  
  zone = zone_list[z] & print, 'LOOP_ZON: ', zone
  @def_zones

  FOR v = 0, n_elements(var_list)-1 DO BEGIN

    var_name = var_list[v] & print, 'LOOP_VAR: ', var_name

    FOR e = 0, n_elements(exp_list)-1 DO BEGIN
    
      exp_name = exp_list[e] & print, 'LOOP_EXP: ', exp_name & print, ''

      IF STRCMP(exp_name, 'tr', 2) OR STRCMP(exp_name, 'ind', 3) THEN BEGIN

        ; MODEL DATA
        @read_model

      ENDIF ELSE BEGIN

        ; OBS DATA
        @read_obs

      ENDELSE

        ; INTERPOLATION GRID_e - > GRID_0
        @calcul_interpol

        ; ECRITURE FICHIER IDL
        @save_data

        ; SAUVEGARDE VARIABLES
        @save_vars

    ENDFOR ; EXPS


;-------------------------------------------------------------------------------------------------
; STATS
;-------------------------------------------------------------------------------------------------

    @calcul_stats


;-------------------------------------------------------------------------------------------------
; PLOTS
;-------------------------------------------------------------------------------------------------

    IF plot_TC THEN BEGIN
      var_plot = 'nbcg_1y'
      @plot_1D_t
      var_plot = 'nbtcday_1y'
      @plot_1D_t
    ENDIF

    ;IF flag_z THEN BEGIN
      ;@plot_2D_xz
    ;ENDIF

    ;var_plot = 'var_xmean'
    ;@plot_1D_x
    ;var_plot = 'var_ymean'
    ;@plot_1D_y

    ; SEASONAL CYCLE
    IF plot_sc1m AND (data_type EQ '1m' OR data_type EQ 'c1m') THEN BEGIN
      var_plot = 'var_sc1m'
      @plot_1D_c1m
    ENDIF

    ; MONTHLY TENDENCIES
    IF plot_dt and data_type EQ 'c1m' and flag_z EQ 0 THEN BEGIN
      @calcul_dt
      var_plot = 'vardt_1D'
      @plot_1D_t
      ;var_plot = 'vardt_mean'     
      ;@plot_2D
      var_plot = 'vardt_cum'
      @plot_2D
      var_plot = 'errdt_cum'
      @plot_2D
    ENDIF

    ; SEASONAL AVERAGE
    IF plot_2D THEN BEGIN

      var_plot = 'var_mean'
      @plot_2D
      ;var_plot = 'ano_mean'
      ;@plot_2D

      @plot_2D_errmean

      IF data_type EQ '1m' THEN IF nbyear_max GT 1 THEN BEGIN
        FOR e = 0, n_elements(exp_list)-1 DO cmd = execute( 'stddev_varyear_'+STRTRIM(e,2)+' = STDDEV( var_year_'+STRTRIM(e,2)+', DIMENSION=3, /NAN)' )
        var_plot = 'stddev_varyear'
        @plot_2D
        FOR e = 0, n_elements(exp_list)-1 DO cmd = execute( 'stddev_anoyearsc1m_'+STRTRIM(e,2)+' = STDDEV( anoyear_sc1m_2D1y_'+STRTRIM(e,2)+', DIMENSION=3, /NAN)' )
        var_plot = 'stddev_anoyearsc1m'
        @plot_2D
      ENDIF


;-------------------------------------------------------------------------------------------------
; PLOTS RAIN
;-------------------------------------------------------------------------------------------------

      IF var_name EQ 'RAIN' THEN BEGIN
        ;IF period EQ 'JJAS' AND data_type EQ '1d' THEN BEGIN
        ;  @calcul_wetspell
        ;ENDIF
        ;var_plot = 'varx_mean'
        ;@plot_2D
        ;var_plot = 'vary_mean'
        ;@plot_2D
        ratx_mean_0 = varx_mean_0
        FOR e = 1, n_elements(exp_list)-1 DO cmd = execute( 'ratx_mean_'+strtrim(e,2)+' = varx_mean_'+strtrim(e,2)+'_gridobs / var_mean_'+strtrim(e,2)+'_gridobs * 100.' )
        FOR e = 1, n_elements(exp_list)-1 DO cmd = execute( 'ratx_mean_'+strtrim(e,2)+'[WHERE( var_mean_'+strtrim(e,2)+'_gridobs LT 1.)] = !VALUES.F_NAN' )
        var_plot = 'ratx_mean'
        ;@plot_2D
        raty_mean_0 = vary_mean_0
        FOR e = 1, n_elements(exp_list)-1 DO cmd = execute( 'raty_mean_'+strtrim(e,2)+' = vary_mean_'+strtrim(e,2)+'_gridobs / var_mean_'+strtrim(e,2)+'_gridobs * 100.' )
        FOR e = 1, n_elements(exp_list)-1 DO cmd = execute( 'raty_mean_'+strtrim(e,2)+'[WHERE( var_mean_'+strtrim(e,2)+'_gridobs LT 1.)] = !VALUES.F_NAN' )
        var_plot = 'raty_mean'
        ;@plot_2D
      ENDIF


;-------------------------------------------------------------------------------------------------
; PLOTS DIV, ROT & GRAD
;-------------------------------------------------------------------------------------------------

      IF STRMATCH(var_name, 'UV*') OR var_name EQ 'STRESS' THEN BEGIN
        @calcul_divcurl
        var_plot = 'div'
        @plot_2D
        var_plot = 'errdiv'
        @plot_2D
        var_plot = 'rot'
        @plot_2D
      ENDIF

      IF var_name EQ 'PSFC' OR var_name EQ 'MSLP' THEN BEGIN
        @calcul_grad
        var_plot = 'grad'
        @plot_2D
        var_plot = 'errgrad'
        @plot_2D
        var_plot = 'diffgrad'
        @plot_2D
      ENDIF

    ENDIF


;-------------------------------------------------------------------------------------------------
; PLOTS OROGRAPHIE
;-------------------------------------------------------------------------------------------------

    IF zone EQ 'HYM' THEN BEGIN
      rotdeg = -25
      FOR e = 0, n_elements(exp_list)-1 DO BEGIN
        cmd = execute( 'var_meanrot_'+strtrim(e,2)+'   =  ROT(   var_mean_'+strtrim(e,2)+', rotdeg, MISSING=!VALUES.F_NAN)' )
        cmd = execute( 'var_meanrot1D_'+strtrim(e,2)+' = MEAN(var_meanrot_'+strtrim(e,2)+', DIMENSION=1, /NAN)' )
        IF e GT 0 THEN cmd = execute( 'var_meanrot1D_'+strtrim(e,2)+'_gridobs = INTERPOL( var_meanrot1D_'+strtrim(e,2)+', n_elements(var_meanrot1D_0))' )
      ENDFOR
      var_plot = 'var_meanrot1D'
      @plot_1D_rot
    ENDIF

    IF zone EQ 'GAT' THEN BEGIN
      rotdeg = 20
      FOR e = 0, n_elements(exp_list)-1 DO BEGIN
        cmd = execute( 'var_meanrot_'+strtrim(e,2)+'   =  ROT(   var_mean_'+strtrim(e,2)+', rotdeg, MISSING=!VALUES.F_NAN)' )
        cmd = execute( 'var_meanrot1D_'+strtrim(e,2)+' = MEAN(var_meanrot_'+strtrim(e,2)+', DIMENSION=2, /NAN)' )
        IF e GT 0 THEN cmd = execute( 'var_meanrot1D_'+strtrim(e,2)+'_gridobs = INTERPOL( var_meanrot1D_'+strtrim(e,2)+', n_elements(var_meanrot1D_0))' )
      ENDFOR
      var_plot = 'var_meanrot1D'
      @plot_1D_rot
    ENDIF

    IF flag_z and plot_zp THEN BEGIN
      @plot_1D_z
    ENDIF

    IF plot_ts THEN BEGIN
      var_plot = 'var_ts'
      @plot_1D_t
      var_plot = 'ano_ts'
      @plot_1D_t
      var_plot = 'ano_sc1m_ts1m'
      @plot_1D_t
      var_plot = 'ano_sc1m_ts1y'
      @plot_1D_t
    ENDIF

    IF plot_index and nbyear_0 GT 1 THEN BEGIN
      var_plot = 'var_ts1y'
      @plot_1D_t
      var_plot = 'ano_ts1y'
      @plot_1D_t
    ENDIF

    IF plot_timefilt THEN BEGIN
      @plot_timefilt
    ENDIF

    IF plot_sp THEN BEGIN
      @plot_sp
    ENDIF

    IF var_name EQ 'D20' OR var_name EQ 'SST' THEN BEGIN
      @plot_TRIO
    ENDIF


;-------------------------------------------------------------------------------------------------
; SAUVEGARDE + SPECIAL PLOTS
;-------------------------------------------------------------------------------------------------

    ; 1m D20-SST
    IF data_type EQ '1m' AND (var_name EQ 'D20' OR var_name EQ 'SST') THEN BEGIN
      FOR e = 0, n_elements(exp_list)-1 DO BEGIN
        cmd = execute( var_name+'_anosc1m_ts1m_'+strtrim(e,2)+' = ano_sc1m_ts1m_'+strtrim(e,2) )
        cmd = execute( var_name+'_anoyearsc1m_2D1y_'+strtrim(e,2)+' = anoyear_sc1m_2D1y_'+strtrim(e,2) )
        cmd = execute( var_name+'_var_ts1y_d20xie_'+strtrim(e,2)+' = var_ts1y_d20xie_'+strtrim(e,2) )
        cmd = execute( var_name+'_anosc1m_ts1y_d20xie_'+strtrim(e,2)+' = anosc1m_ts1y_d20xie_'+strtrim(e,2) )
        cmd = execute( 'help, '+var_name+'_var_ts1y_d20xie_'+strtrim(e,2)+', '+var_name+'_anosc1m_ts1y_d20xie_'+strtrim(e,2)+', '+var_name+'_anosc1m_ts1m_'+strtrim(e,2) )
      ENDFOR
      cmd = execute( 'lon_'+var_name+' = lon_0' )
      cmd = execute( 'lat_'+var_name+' = lat_0' )
    ENDIF
    STOP

    ; 1m RAIN-SST
    IF data_type EQ '1m' AND (var_name EQ 'RAIN' OR var_name EQ 'SST') THEN BEGIN

      cmd = execute( 'path_'+var_name+' = path_0' )
      cmd = execute( 'file_'+var_name+' = file_0' )
      cmd = execute( 'lon_'+var_name+' = lon_0' )
      cmd = execute( 'lat_'+var_name+' = lat_0' )

      FOR e = 0, n_elements(exp_list)-1 DO BEGIN
        cmd = execute( var_name+'_ts_'+strtrim(e,2)+' = var_ts_'+strtrim(e,2) )
        IF e EQ 0 THEN cmd = execute( var_name+'_'+strtrim(e,2)+' = var_'+strtrim(e,2) ) $
                  ELSE cmd = execute( var_name+'_'+strtrim(e,2)+' = var_'+strtrim(e,2)+'_gridobs' )
        IF var_name EQ 'RAIN' THEN BEGIN
          cmd = execute( var_name+'P_ts_'+strtrim(e,2)+' = varx_ts_'+strtrim(e,2) )
          cmd = execute( var_name+'E_ts_'+strtrim(e,2)+' = vary_ts_'+strtrim(e,2) )
          IF e EQ 0 THEN cmd = execute( var_name+'P_'+strtrim(e,2)+' = varx_'+strtrim(e,2) ) $
                    ELSE cmd = execute( var_name+'P_'+strtrim(e,2)+' = varx_'+strtrim(e,2)+'_gridobs' )
          IF e EQ 0 THEN cmd = execute( var_name+'E_'+strtrim(e,2)+' = vary_'+strtrim(e,2) ) $
                    ELSE cmd = execute( var_name+'E_'+strtrim(e,2)+' = vary_'+strtrim(e,2)+'_gridobs' )
        ENDIF
      ENDFOR

    ENDIF


    ; 1d RAIN-SST
    IF data_type EQ '1d' AND (var_name EQ 'RAIN' OR var_name EQ 'SST') THEN BEGIN
      FOR e = 0, n_elements(exp_list)-1 DO BEGIN
        IF e EQ 0 THEN cmd = execute( var_name+'_'+strtrim(e,2)+' = var_'+strtrim(e,2) ) $
                  ELSE cmd = execute( var_name+'_'+strtrim(e,2)+' = var_'+strtrim(e,2)+'_gridobs' )
      ENDFOR
    ENDIF


    ; SAUVEGARDE POUR PLOTS "RAIN + UV10"
    IF var_name EQ 'RAIN' OR var_name EQ 'UV10' THEN BEGIN
      cmd = execute( 'path_'+var_name+' = path_0' )
      cmd = execute( 'file_'+var_name+' = file_0' )
      cmd = execute( 'lon_'+var_name+' = lon_0' )
      cmd = execute( 'lat_'+var_name+' = lat_0' )
      e=0
      cmd = execute( var_name+'_mean_'+strtrim(e,2)+' = var_mean_'+strtrim(e,2) )
      IF var_name EQ 'UV10' THEN cmd = execute( 'u10_mean_'+strtrim(e,2)+' = varx_mean_'+strtrim(e,2)+' & v10_mean_'+strtrim(e,2)+' = vary_mean_'+strtrim(e,2) )
      FOR e = 1, n_elements(exp_list)-1 DO BEGIN
        cmd = execute( var_name+'_mean_'+strtrim(e,2)+' = var_mean_'+strtrim(e,2)+'_gridobs' )
        IF var_name EQ 'UV10' THEN cmd = execute( 'u10_mean_'+strtrim(e,2)+' = varx_mean_'+strtrim(e,2)+'_gridobs & v10_mean_'+strtrim(e,2)+' = vary_mean_'+strtrim(e,2)+'_gridobs' )
      ENDFOR
      IF var_name EQ 'UV10' THEN BEGIN
         FOR e = 0, n_elements(exp_list)-1 DO BEGIN
           cmd = execute( 'uv10_mean_'+strtrim(e,2)+'_gridrain = fromreg("bilinear", uv10_mean_'+strtrim(e,2)+', lon_uv10, lat_uv10, lon_rain, lat_rain)' )
           cmd = execute( 'u10_mean_'+strtrim(e,2)+'_gridrain = fromreg("bilinear", u10_mean_'+strtrim(e,2)+', lon_uv10, lat_uv10, lon_rain, lat_rain)' )
           cmd = execute( 'v10_mean_'+strtrim(e,2)+'_gridrain = fromreg("bilinear", v10_mean_'+strtrim(e,2)+', lon_uv10, lat_uv10, lon_rain, lat_rain)' )
         ENDFOR
      ENDIF
    ENDIF

    ; SAUVEGARDE POUR PLOTS "FLUX NET vs SST"
    IF var_name EQ 'SST' OR var_name EQ 'NET' THEN $
    FOR e = 0, n_elements(exp_list)-1 DO cmd = execute( var_name+'_sc1m_'+strtrim(e,2)+' = MEAN(var_sc1m_'+strtrim(e,2)+', /NAN)' )

    ; SAUVEGARDE POUR PLOTS "WIND vs LH"
    IF var_name EQ 'UV10' OR var_name EQ 'LH' OR var_name EQ 'HFX' OR var_name EQ 'DELTAQ' THEN $
    IF data_type EQ '1d' THEN FOR e = 0, n_elements(exp_list)-1 DO cmd = execute( var_name+'_'+data_type+'_'+strtrim(e,2)+' = var_'+strtrim(e,2) )

    ; SAUVEGARDE POUR PLOTS BILAN SURFACE
    IF var_name EQ 'GSW' OR var_name EQ 'LWR' OR var_name EQ 'LH' OR var_name EQ 'HFX' OR var_name EQ 'NET' $
    OR var_name EQ 'SST' OR var_name EQ 'SKT' OR var_name EQ 'T2'  OR var_name EQ 'Q2' OR var_name EQ 'RAIN' THEN BEGIN
      IF data_type EQ 'c1m' THEN BEGIN
        FOR e = 0, n_elements(exp_list)-1 DO cmd = execute( var_name+'_xytmean_'+strtrim(e,2)+' = MEAN(var_sc1m_'+strtrim(e,2)+'[ind_period], /NAN)' )
        FOR e = 0, n_elements(exp_list)-1 DO cmd = execute( var_name+'_xytmean_'+strtrim(e,2)+' = MEAN(var_sc1m_'+strtrim(e,2)+'[ind_period], /NAN)' )
      ENDIF
      IF data_type EQ '1m' THEN BEGIN
        FOR e = 0, n_elements(exp_list)-1 DO cmd = execute( var_name+'_xytmean_'+strtrim(e,2)+' = MEAN(var_ts1y_'+strtrim(e,2)+', /NAN)' )
        FOR e = 0, n_elements(exp_list)-1 DO cmd = execute( var_name+'_ts1y_'+strtrim(e,2)+'  = var_ts1y_'+strtrim(e,2) )
      ENDIF
      IF data_type EQ '1d' THEN BEGIN
        FOR e = 0, n_elements(exp_list)-1 DO cmd = execute( var_name+'_xytmean_'+strtrim(e,2)+' = MEAN(var_ts_'+strtrim(e,2)+'[ind_mean1d], /NAN)' )
      ENDIF
    ENDIF

    ; SAUVEGARDE POUR PLOTS "DELTA_TT"
    IF data_type EQ 'c1m' AND ( STRMATCH( zone, '*1') OR STRMATCH( zone, '*2')) THEN BEGIN
      IF STRMATCH( zone, '*1') THEN boxn = 'box1'
      IF STRMATCH( zone, '*2') THEN boxn = 'box2'
      FOR e = 0, n_elements(exp_list)-1 DO cmd = execute( var_name+'_'+boxn+'_'+strtrim(e,2)+' = var_sc1m_'+strtrim(e,2) )
    ENDIF

    ; PLOTS SPECIAL UPWELLING
    IF (zone EQ 'UPW' AND var_name EQ 'SST') OR (zone EQ 'UPW' AND var_name EQ 'UV10') OR (zone EQ 'UPW' AND var_name EQ 'STRESS') THEN BEGIN
      @plot_diag_upw
    ENDIF

    ; PLOT HOVMULLER MONSOON
    IF var_name EQ 'RAIN' AND plot_hov THEN BEGIN
      @plot_hov
    ENDIF

    ; PLOT SPECIAL RAIN
    IF var_name EQ 'RAIN' AND data_type EQ '1d' THEN BEGIN
      @plot_rain2D
      ;@plot_rain1D
    ENDIF

  ENDFOR ; VARS LOOP

  ; CLOSE ASCII FILE
  IF data_type NE '1d' AND data_type NE '6h' THEN CLOSE, /ALL

;-------------------------------------------------------------------------------------------------
; END VARS LOOP
;-------------------------------------------------------------------------------------------------

  ; sc1m SST-D20 CORRELATION
  FOR e = 0, n_elements(exp_list)-1 DO BEGIN

    cmd = execute( 'help, SST_var_ts1y_d20xie_'+strtrim(e,2)+', SST_anosc1m_ts1y_d20xie_'+strtrim(e,2)+', SST_anosc1m_ts1m_'+strtrim(e,2) )
    cmd = execute( 'help, D20_var_ts1y_d20xie_'+strtrim(e,2)+', D20_anosc1m_ts1y_d20xie_'+strtrim(e,2)+', D20_anosc1m_ts1m_'+strtrim(e,2) )

    ;cmd = execute( 'cor1m_anosc1m_'+strtrim(e,2)+' = FLTARR(12)' ) ; correlation mois par mois
    ;FOR m = 0, 12-1 DO BEGIN
    ;  ind_month = FINDGEN( nbyear_obs) * 12 + m
    ;  cmd = execute( 'cor1m_anosc1m_'+strtrim(e,2)+'[m] = CORRELATE( SST_anosc1m_ts1m_'+strtrim(e,2)+'[ind_month], D20_anosc1m_ts1m_'+strtrim(e,2)+'[ind_month])' )
    ;ENDFOR
    ; correlation moyenne sur "period"
    ;cmd = execute( 'cor_anosc1m_'+strtrim(e,2)+' = MEAN( cor1m_anosc1m_'+strtrim(e,2)+'[ind_period MOD 12])' )
    cmd = execute( 'print, exp_list[e], " CORRELATION D20-SST SUR TOUTE LANNEE: ", CORRELATE( D20_anosc1m_ts1m_'+strtrim(e,2)+', SST_anosc1m_ts1m_'+strtrim(e,2)+' )' )
    ;cmd = execute( 'print, exp_list[e], " CORRELATION D20-SST SUR LA PERIODE '+period+': ", cor_anosc1m_'+strtrim(e,2) )

    cmd = execute( 'print, exp_list[e], " CORRELATION VAR_TS1Y     D20XIE:", CORRELATE( D20_var_ts1y_d20xie_'+strtrim(e,2)+', SST_var_ts1y_d20xie_'+strtrim(e,2)+')' )
    cmd = execute( 'print, exp_list[e], " CORRELATION ANOSC1M_TS1Y D20XIE:", CORRELATE( D20_anosc1m_ts1y_d20xie_'+strtrim(e,2)+', SST_anosc1m_ts1y_d20xie_'+strtrim(e,2)+')' )

  ENDFOR
  STOP
  var_plot = 'cor1m_anosc1m'
  @plot_1D_c1m

  ; 2D 1m SST-D20 CORRELATION
  FOR e = 0, n_elements(exp_list)-1 DO BEGIN
    cmd = execute( 'cor2D_anoyearsc1m_'+strtrim(e,2)+' = FLTARR( n_elements(lon_sst), n_elements(lat_sst))' )
    cmd = execute( 'nbyear = nbyear_'+strtrim(e,2) )
    cmd = execute( 'D20_anoyearsc1m_2D1y_'+strtrim(e,2)+'_gridsst = FLTARR( n_elements(lon_sst), n_elements(lat_sst), nbyear)' )
    FOR k = 0, nbyear-1 DO cmd = execute( 'D20_anoyearsc1m_2D1y_'+strtrim(e,2)+'_gridsst[*,*,k] = FROMREG( "bilinear", D20_anoyearsc1m_2D1y_'+strtrim(e,2)+'[*,*,k], lon_d20, lat_d20, lon_sst, lat_sst)' ) 
    FOR j = 0, n_elements( lat_sst)-1 DO BEGIN
      FOR i = 0, n_elements( lon_sst)-1 DO BEGIN
        cmd = execute( 'cor2D_anoyearsc1m_'+strtrim(e,2)+'[i,j] = CORRELATE( SST_anoyearsc1m_2D1y_'+strtrim(e,2)+'[i,j,*], D20_anoyearsc1m_2d1y_'+strtrim(e,2)+'_gridsst[i,j,*])' )
      ENDFOR
    ENDFOR
    cmd = execute( 'help, cor2D_anoyearsc1m_'+strtrim(e,2) )
    cmd = execute( 'print, "VERIF: ", MEAN( cor2D_anoyearsc1m_'+strtrim(e,2)+', /NAN)' )
  ENDFOR

  var_plot = 'cor2D_anoyearsc1m'
  @plot_2D
  STOP

  ; 1m SST-RAIN PLOTS
  IF data_type EQ '1m' AND WHERE(var_list EQ 'RAIN') NE -1 AND WHERE(var_list EQ 'SST') NE -1 THEN BEGIN

    ; CORRELATION 1m DURING JJAS
    FOR e = 0, n_elements(exp_list)-1 DO BEGIN

      ; 1m CORRELATION 2D-MEAN
      cmd = execute( ' rain_periodts_'+strtrim(e,2)+' =  rain_ts_'+strtrim(e,2)+'[ind_mean1m_'+strtrim(e,2)+'[SORT( ind_mean1m_'+strtrim(e,2)+')]]' )
      cmd = execute( 'rainp_periodts_'+strtrim(e,2)+' = rainp_ts_'+strtrim(e,2)+'[ind_mean1m_'+strtrim(e,2)+'[SORT( ind_mean1m_'+strtrim(e,2)+')]]' )
      cmd = execute( 'raine_periodts_'+strtrim(e,2)+' = raine_ts_'+strtrim(e,2)+'[ind_mean1m_'+strtrim(e,2)+'[SORT( ind_mean1m_'+strtrim(e,2)+')]]' )
      cmd = execute( '  sst_periodts_'+strtrim(e,2)+' =  sst_ts_'+strtrim(e,2)+'[ind_mean1m_'+strtrim(e,2)+'[SORT( ind_mean1m_'+strtrim(e,2)+')]]' )
      cmd = execute( ' corr_periodts_'+strtrim(e,2)+' = CORRELATE( rain_periodts_'+strtrim(e,2)+', sst_periodts_'+strtrim(e,2)+')' ) 
      cmd = execute( 'help, rain_periodts_'+strtrim(e,2)+', sst_periodts_'+strtrim(e,2)+', corr_periodts_'+strtrim(e,2))

      ; 1m CORRELATION 2D
      cmd = execute( 'rain_period_'+strtrim(e,2)+' = rain_'+strtrim(e,2)+'[*,*,ind_mean1m_'+strtrim(e,2)+'[SORT( ind_mean1m_'+strtrim(e,2)+')]]' )
      cmd = execute( 'rainp_period_'+strtrim(e,2)+' = rainp_'+strtrim(e,2)+'[*,*,ind_mean1m_'+strtrim(e,2)+'[SORT( ind_mean1m_'+strtrim(e,2)+')]]' )
      cmd = execute( 'raine_period_'+strtrim(e,2)+' = raine_'+strtrim(e,2)+'[*,*,ind_mean1m_'+strtrim(e,2)+'[SORT( ind_mean1m_'+strtrim(e,2)+')]]' )

      cmd = execute( ' sst_period_'+strtrim(e,2)+' =  sst_'+strtrim(e,2)+'[*,*,ind_mean1m_'+strtrim(e,2)+'[SORT( ind_mean1m_'+strtrim(e,2)+')]]' )
      cmd = execute( ' sst_period_gridrain_'+strtrim(e,2)+' = rain_period_'+strtrim(e,2)+' * 0.' )
      FOR t = 0, n_elements(ind_mean1m)-1 DO $
      cmd = execute( ' sst_period_gridrain_'+strtrim(e,2)+'[*,*,t] = fromreg("bilinear", sst_period_'+strtrim(e,2)+'[*,*,t], lon_sst, lat_sst, lon_rain, lat_rain)' )
      cmd = execute( 'corr2D_period_'+strtrim(e,2)+' = rain_period_'+strtrim(e,2)+'[*,*,0] * 0.' )
      cmd = execute( 'help, rain_period_'+strtrim(e,2)+', sst_period_gridrain_'+strtrim(e,2)+', corr2D_period_'+strtrim(e,2) )
      FOR i = 0, n_elements( rain_period_0[*,0,0])-1 DO BEGIN
        FOR j = 0, n_elements( rain_period_0[0,*,0])-1 DO BEGIN
          cmd = execute( 'corr2D_period_'+strtrim(e,2)+'[i,j] = CORRELATE( rain_period_'+strtrim(e,2)+'[i,j,*], sst_period_gridrain_'+strtrim(e,2)+'[i,j,*])' )
        ENDFOR
      ENDFOR
    ENDFOR

    ; PLOTS 2D RAIN
    FOR e = 0, n_elements(exp_list)-1 DO BEGIN
      cmd = execute( 'rainp_period_ratio_'+strtrim(e,2)+' = MEAN( rainp_period_'+strtrim(e,2)+', DIMENSION=3, /NAN) / MEAN( rain_period_'+strtrim(e,2)+', DIMENSION=3, /NAN) * 100.' )
      cmd = execute( 'raine_period_ratio_'+strtrim(e,2)+' = MEAN( raine_period_'+strtrim(e,2)+', DIMENSION=3, /NAN) / MEAN( rain_period_'+strtrim(e,2)+', DIMENSION=3, /NAN) * 100.' )
    ENDFOR
  
    var_plot = 'rainp_period_ratio'
    @plot_2D_rain

    var_plot = 'raine_period_ratio'
    @plot_2D_rain


    ; 1m SST vs PRECIPS
    var_plotx = 'sst_period_gridrain'
    var_ploty = 'rain_period'
    @plot_sst_vs_rain

  ENDIF


  ; PLOT SST vs RAIN
  IF data_type EQ '1d' AND WHERE(var_list EQ 'RAIN') NE -1 AND WHERE(var_list EQ 'SST') NE -1 THEN BEGIN
    @plot_sst_vs_rain
  ENDIF

  ; PLOT RAIN + UV10
  IF WHERE(var_list EQ 'RAIN') NE -1 AND WHERE(var_list EQ 'UV10') NE -1 THEN BEGIN
    @plot_rain_uv10
  ENDIF

  ; PLOT BILAN SURFACE
  IF WHERE(var_list EQ 'GSW') NE -1 AND WHERE(var_list EQ 'LWR') NE -1 AND $
     WHERE(var_list EQ  'LH') NE -1 AND WHERE(var_list EQ 'HFX') NE -1 AND WHERE(var_list EQ 'NET') NE -1 THEN BEGIN
    terms = [ 'GSW', 'LWR', 'LH', 'HFX', 'NET' ]
    @plot_surf_budget
  ENDIF
  IF WHERE(var_list EQ 'SKT') NE -1 AND WHERE(var_list EQ   'T2') NE -1 AND $
     WHERE(var_list EQ  'Q2') NE -1 AND WHERE(var_list EQ 'RAIN') NE -1 THEN BEGIN
    terms = [ 'SKT', 'T2', 'Q2', 'RAIN' ]
    @plot_surf_budget
  ENDIF
  IF WHERE(var_list EQ 'SST') NE -1 AND WHERE(var_list EQ   'T2') NE -1 AND $
     WHERE(var_list EQ  'Q2') NE -1 AND WHERE(var_list EQ 'RAIN') NE -1 AND WHERE(var_list EQ 'OLR') NE -1 THEN BEGIN
    terms = [ 'SST', 'T2', 'Q2', 'RAIN', 'OLR']
    @plot_surf_budget
  ENDIF

  ; PLOT "FLUX NET vs SST"
  IF WHERE(var_list EQ 'NET') NE -1 AND WHERE(var_list EQ 'SST') NE -1 THEN BEGIN
    @plot_net_vs_sst
  ENDIF

  ; PLOT WIND vs LH
  IF data_type EQ '1d' THEN BEGIN
    IF (WHERE(var_list EQ 'UV10') NE -1 AND WHERE(var_list EQ 'LH')  NE -1) OR $
       (WHERE(var_list EQ 'UV10') NE -1 AND WHERE(var_list EQ 'HFX') NE -1) THEN BEGIN
      IF WHERE(var_list EQ 'LH')  NE -1 THEN BEGIN
        var_name = 'LH'
        @plot_hist_flux
        var_name = 'LHDELTAQ'
        @plot_hist_flux
        var_name = 'LHDELTAQM'
        @plot_hist_flux
      ENDIF
      IF WHERE(var_list EQ 'DELTAQ') NE -1 THEN BEGIN
        var_name = 'DELTAQ'
        @plot_hist_flux
      ENDIF
    ENDIF
  ENDIF

ENDFOR ; ZONES
print, 'END LOOP ZONES'

; PLOT "DELTA_TT"
IF data_type EQ 'c1m' AND ( WHERE( STRMATCH( zone_list, '*1')) NE -1 OR WHERE( STRMATCH( zone_list, '*2')) NE -1 ) THEN BEGIN
  FOR e = 0, n_elements(exp_list)-1 DO cmd = execute( 'delta_box_'+strtrim(e,2)+' = '+var_name+'_box1_'+strtrim(e,2)+' - '+var_name+'_box2_'+strtrim(e,2) )
  var_plot = 'delta_box_'
  @plot_1D_t
ENDIF

ENDFOR ; PERIODS
print, 'END LOOP PERIODS'


;-------------------------------------------------------------------------------------------------
; ARCHIVAGE
;-------------------------------------------------------------------------------------------------

IF write_ps OR write_gif THEN BEGIN
  FILE_DELETE, 'TAR_FILES/'+(STRSPLIT( plot_dir, '/', /EXTRACT))[1]+'.tar', /RECURSIVE, /ALLOW_NONEXISTENT
  SPAWN, 'tar -cvf TAR_FILES/'+(STRSPLIT( plot_dir, '/', /EXTRACT))[1]+'.tar '+plot_dir
  print, 'TARF FILE OK'
ENDIF


print, 'END OF PROGRAM' & STOP

END
