
;-------------------------------------------------------------------------------------------------
; WARNING:
; VAR  = TOTAL RAIN
; VARX = RAINC  (CONVECTIVE)
; VARY = RAINNC (EXPLICIT)
;-------------------------------------------------------------------------------------------------


;-------------------------------------------------------------------------------------------------
; RAIN BINS DEFINITION (mm/day)
min_rain  = 0.
max_rain  = 300.
bin_rain  = 10.
nbin_rain = (max_rain-min_rain) / bin_rain
axe_rain  = min_rain + indgen(nbin_rain+1) * bin_rain
;-------------------------------------------------------------------------------------------------


;-------------------------------------------------------------------------------------------------
; CALCULS
;-------------------------------------------------------------------------------------------------

FOR e = 0, n_elements(exp_list)-1 DO BEGIN

  ; RENAME
  IF e EQ 0 THEN cmd = execute( 'var  = var_'+strtrim(e,2) )  ELSE cmd = execute( 'var  = var_'+strtrim(e,2)+'_gridobs' )

  cmd = execute( 'quar_rain_'+strtrim(e,2)+'   = PERCENTILE( var[ WHERE( var GT 0.)], 0.25)' )

  cmd = execute( 'rain25_'+strtrim(e,2)+' = var * !VALUES.F_NAN' )
  cmd = execute( 'rain75_'+strtrim(e,2)+' = var * !VALUES.F_NAN' )
  IF e GT 0 THEN cmd = execute( 'rain25obs_'+strtrim(e,2)+' = var * !VALUES.F_NAN' )
  IF e GT 0 THEN cmd = execute( 'rain75obs_'+strtrim(e,2)+' = var * !VALUES.F_NAN' )

  cmd = execute( 'rain25_'+strtrim(e,2)+'[ WHERE( var LE quar_rain_'+strtrim(e,2)+'[0])] = var[ WHERE( var LE quar_rain_'+strtrim(e,2)+'[0])]' )
  cmd = execute( 'rain75_'+strtrim(e,2)+'[ WHERE( var GE quar_rain_'+strtrim(e,2)+'[1])] = var[ WHERE( var GE quar_rain_'+strtrim(e,2)+'[1])]' )
  IF e GT 0 THEN cmd = execute( 'rain25obs_'+strtrim(e,2)+'[ WHERE( var LE quar_rain_0[0])] = var[ WHERE( var LE quar_rain_0[0])]' )
  IF e GT 0 THEN cmd = execute( 'rain75obs_'+strtrim(e,2)+'[ WHERE( var GE quar_rain_0[1])] = var[ WHERE( var GE quar_rain_0[1])]' )

  cmd = execute( 'rain25_mean_'+strtrim(e,2)+' = MEAN( rain25_'+strtrim(e,2)+'[*,*,ind_mean1d_'+strtrim(e,2)+'], DIMENSION=3, /NAN)' )
  cmd = execute( 'rain75_mean_'+strtrim(e,2)+' = MEAN( rain75_'+strtrim(e,2)+'[*,*,ind_mean1d_'+strtrim(e,2)+'], DIMENSION=3, /NAN)' )
  IF e GT 0 THEN cmd = execute( 'rain25obs_mean_'+strtrim(e,2)+' = MEAN( rain25obs_'+strtrim(e,2)+'[*,*,ind_mean1d_'+strtrim(e,2)+'], DIMENSION=3, /NAN)' )
  IF e GT 0 THEN cmd = execute( 'rain75obs_mean_'+strtrim(e,2)+' = MEAN( rain75obs_'+strtrim(e,2)+'[*,*,ind_mean1d_'+strtrim(e,2)+'], DIMENSION=3, /NAN)' )

  ;cmd = execute( 'ratio_r25_mean_'+strtrim(e,2)+' = rain25_mean_'+strtrim(e,2)+' / rain_mean_'+strtrim(e,2)+' * 100.' )
  ;cmd = execute( 'ratio_r75_mean_'+strtrim(e,2)+' = rain75_mean_'+strtrim(e,2)+' / rain_mean_'+strtrim(e,2)+' * 100.' )
  ;IF e GT 0 THEN cmd = execute( 'ratio_r25obs_mean_'+strtrim(e,2)+' = rain25obs_mean_'+strtrim(e,2)+' / rain_mean_'+strtrim(e,2)+' * 100.' )
  ;IF e GT 0 THEN cmd = execute( 'ratio_r75obs_mean_'+strtrim(e,2)+' = rain75obs_mean_'+strtrim(e,2)+' / rain_mean_'+strtrim(e,2)+' * 100.' )

ENDFOR


;-------------------------------------------------------------------------------------------------
; PLOT SETUP
;-------------------------------------------------------------------------------------------------

lct, 60 & fmt='(F6.1)'
IF force_landmask THEN mask_title = 'OCEAN'
IF force_seamask  THEN mask_title = 'LAND'
IF force_landmask EQ 0 AND force_seamask EQ 0 THEN mask_title = 'OCEAN+LAND'
IF write_ps THEN thc = 6 ELSE thc = 2
IF write_ps THEN chs = 1 ELSE chs = 1.5
color_list = [0, 50, 250, 150, 200, 100, 25]


;-------------------------------------------------------------------------------------------------
; PLOT QUARTILES
;-------------------------------------------------------------------------------------------------

var_plot = 'rain25_mean' & help, var_plot
@plot_2D

var_plot = 'rain75_mean' & help, var_plot
@plot_2D

rain25obs_mean_0 = rain25_mean_0
var_plot = 'rain25obs_mean' & help, var_plot
@plot_2D

rain75obs_mean_0 = rain75_mean_0
var_plot = 'rain75obs_mean' & help, var_plot
@plot_2D

;var_plot = 'ratio_r25_mean' & help, var_plot
;@plot_2D

;var_plot = 'ratio_r75_mean' & help, var_plot
;@plot_2D

;ratio_r25obs_mean_0 = ratio_r25_mean_0
;var_plot = 'ratio_r25obs_mean' & help, var_plot
;@plot_2D

;ratio_r75obs_mean_0 = ratio_r75_mean_0
;var_plot = 'ratio_r75obs_mean' & help, var_plot
;@plot_2D
