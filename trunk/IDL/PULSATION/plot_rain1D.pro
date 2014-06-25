
;-------------------------------------------------------------------------------------------------
; WARNING:
; VAR  = TOTAL RAIN
; VARX = RAINC
; VARY = RAINNC
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
  IF e EQ 0 THEN cmd = execute( 'varx = varx_'+strtrim(e,2) ) ELSE cmd = execute( 'varx = varx_'+strtrim(e,2)+'_gridobs' )
  IF e EQ 0 THEN cmd = execute( 'vary = vary_'+strtrim(e,2) ) ELSE cmd = execute( 'vary = vary_'+strtrim(e,2)+'_gridobs' )
  tmpx = varx / var
  tmpy = vary / var

  ; DECLARATIONS
  dist_bin  = lonarr(nbin_rain) ; total distribution per bin (%)
  distx_bin = lonarr(nbin_rain) ; convective distribution per bin (%)
  disty_bin = lonarr(nbin_rain) ; explicit distribution per bin (%)
  tot_bin   = fltarr(nbin_rain) ; total rain per bin (mm)
  conv_bin  = fltarr(nbin_rain) ; convective rain per bin (mm)
  exp_bin   = fltarr(nbin_rain) ; explicit rain per bin (mm)
  ratx_bin  = fltarr(nbin_rain) ; convective ratio per bin (%)
  raty_bin  = fltarr(nbin_rain) ; convective ratio per bin (%)

  nbcheck = 0

  ; CALCULS
  FOR i = 0, nbin_rain-1 DO BEGIN
    indok  = WHERE( var  GE axe_rain[i] AND var  LT axe_rain[i+1], nbok)
    indokx = WHERE( varx GE axe_rain[i] AND varx LT axe_rain[i+1], nbokx)
    indoky = WHERE( vary GE axe_rain[i] AND vary LT axe_rain[i+1], nboky)
    IF nbok  GE 10 THEN dist_bin[i]  = nbok  ELSE dist_bin[i]  = 0
    IF nbokx GE 10 THEN distx_bin[i] = nbokx ELSE distx_bin[i] = 0
    IF nboky GE 10 THEN disty_bin[i] = nboky ELSE disty_bin[i] = 0
    IF nbok  GE 10 THEN tot_bin[i]   = TOTAL( var[indok],   /NAN) ELSE tot_bin[i]  = !VALUES.F_NAN
    IF nbokx GE 10 THEN conv_bin[i]  = TOTAL( varx[indokx], /NAN) ELSE conv_bin[i] = !VALUES.F_NAN
    IF nboky GE 10 THEN exp_bin[i]   = TOTAL( vary[indoky], /NAN) ELSE exp_bin[i]  = !VALUES.F_NAN
    IF nbok  GE 10 THEN ratx_bin[i]  =  MEAN(  tmpx[indok], /NAN) ELSE ratx_bin[i] = !VALUES.F_NAN
    IF nbok  GE 10 THEN raty_bin[i]  =  MEAN(  tmpy[indok], /NAN) ELSE raty_bin[i] = !VALUES.F_NAN
    nbcheck = nbcheck + nbok
  ENDFOR
  dist_bin[ WHERE(dist_bin  LT 0)] = 0
  distx_bin[WHERE(distx_bin LT 0)] = 0
  disty_bin[WHERE(disty_bin LT 0)] = 0

  ; SAVE RESULTS
  cmd = execute( 'dist_rain_'+strtrim(e,2)+'   = FLOAT( dist_bin)  / TOTAL( dist_bin,  /NAN) * 100.' ) ; %
  cmd = execute( 'distx_rain_'+strtrim(e,2)+'  = FLOAT( distx_bin) / TOTAL( distx_bin, /NAN) * 100.' ) ; %
  cmd = execute( 'disty_rain_'+strtrim(e,2)+'  = FLOAT( disty_bin) / TOTAL( disty_bin, /NAN) * 100.' ) ; %
  cmd = execute( 'ratiox_rain_'+strtrim(e,2)+' = FLOAT( distx_bin) / FLOAT( dist_bin) * 100.' ) ; %
  cmd = execute( 'ratioy_rain_'+strtrim(e,2)+' = FLOAT( disty_bin) / FLOAT( dist_bin) * 100.' ) ; %
  cmd = execute( 'tot_rain_' +strtrim(e,2)+'   = TEMPORARY(tot_bin)  / nbyear_'+strtrim(e,2) ) ; mm/year
  cmd = execute( 'conv_rain_'+strtrim(e,2)+'   = TEMPORARY(conv_bin) / nbyear_'+strtrim(e,2) ) ; mm/year
  cmd = execute( 'exp_rain_' +strtrim(e,2)+'   = TEMPORARY(exp_bin)  / nbyear_'+strtrim(e,2) ) ; mm/year
  cmd = execute( 'cum_rain_' +strtrim(e,2)+'   = TOTAL( tot_rain_' +strtrim(e,2)+', /CUMULATIVE, /NAN)' ) ; mm/year
  cmd = execute( 'cumx_rain_'+strtrim(e,2)+'   = TOTAL( conv_rain_'+strtrim(e,2)+', /CUMULATIVE, /NAN)' ) ; mm/year
  cmd = execute( 'cumy_rain_'+strtrim(e,2)+'   = TOTAL( exp_rain_' +strtrim(e,2)+', /CUMULATIVE, /NAN)' ) ; mm/year
  cmd = execute( 'ratx_rain_'+strtrim(e,2)+'   = TEMPORARY( ratx_bin) * 100.' ) ; %
  cmd = execute( 'raty_rain_'+strtrim(e,2)+'   = TEMPORARY( raty_bin) * 100.' ) ; %

  ; ERRORS
  IF e GE 1 THEN BEGIN
    cmd = execute( 'errcum_rain_'+strtrim(e,2)+' = cum_rain_'+strtrim(e,2)+' - cum_rain_0' )
    cmd = execute( 'errtot_rain_'+strtrim(e,2)+' = tot_rain_'+strtrim(e,2)+' - tot_rain_0' )
  ENDIF

  ; VERIFS
  cmd = execute( 'print, "VERIF mm/day: ", TOTAL( tot_rain_'+strtrim(e,2)+', /NAN) / (nbday_'+strtrim(e,2)+'*n_elements(where(finite(var[*,*,0]) EQ 1))) * nbyear_'+strtrim(e,2)+', " mm/day ", TOTAL( var, /NAN) / (nbday_'+strtrim(e,2)+'*n_elements( where( finite(var[*,*,0]) EQ 1))), " mm/day" ' )
  cmd = execute( 'print, "VERIF mm/year: ", TOTAL( tot_rain_'+strtrim(e,2)+', /NAN) / n_elements(where(finite(var[*,*,0]) EQ 1)), " mm/year ", TOTAL( var, /NAN) / (nbyear_'+strtrim(e,2)+'*n_elements( where( finite(var[*,*,0]) EQ 1))), " mm/year ", cum_rain_'+strtrim(e,2)+'[-1] / n_elements(where(finite(var[*,*,0]) EQ 1)), " mm/year"' )
  print, 'VERIF NB DATA: ', nbcheck, ' /', n_elements( WHERE( FINITE( var) EQ 1) ) & print, ''

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
; PLOT TOTAL RAIN DISTRIBUTION (%)
;-------------------------------------------------------------------------------------------------

var_plot = 'dist_rain' & help, var_plot
fig_name = var_plot+'_'+data_type+'_'+zone+'_'+mask_title
IF write_ps THEN openps, filename=plot_dir+'/'+STRUPCASE(data_type)+'/'+zone+'/'+var_name+'/'+fig_name
axe_x = axe_rain[0:nbin_rain-1] + bin_rain/2.
cmd = execute( 'var_y = '+var_plot+'_0' )
splot, axe_x, var_y, xtitle='RAIN BINS (mm/day)', title=mask_title+' TOTAL '+var_name+' DISTRIBUTION - '+zone, charsize=1.5, ystyle=1, ytitle='TOTAL RAIN DISTRIBUTION (%)', lct=39, charthick=1.5, XTicklen=1, YTicklen=1, XGridStyle=1, YGridStyle=1, xrange=[min_rain,max_rain], /nodata, PSYM=10, YRANGE=[0.0001,100.], /YLOG
FOR e = 0, n_elements(exp_list)-1 DO BEGIN
  cmd = execute( 'var_y = '+var_plot+'_'+strtrim(e,2) )
  oplot, axe_x, var_y, thick=thc, color=color_list[e], PSYM=10
  IF exp_list[e] EQ 'OBS' THEN xyouts, 0.08, 0.175-e*0.025, obs_name, /NORMAL, charsize=chs, color=color_list[e], charthick=1 ELSE xyouts, 0.08, 0.175-e*0.025, exp_list[e], /NORMAL, charsize=chs, color=color_list[e], charthick=1
ENDFOR
IF write_ps THEN closeps ELSE STOP


;-------------------------------------------------------------------------------------------------
; PLOT CONVECTIVE RAIN DISTRIBUTION (%)
;-------------------------------------------------------------------------------------------------

var_plot = 'distx_rain' & help, var_plot
fig_name = var_plot+'_'+data_type+'_'+zone+'_'+mask_title
IF write_ps THEN openps, filename=plot_dir+'/'+STRUPCASE(data_type)+'/'+zone+'/'+var_name+'/'+fig_name
axe_x = axe_rain[0:nbin_rain-1] + bin_rain/2.
cmd = execute( 'var_y = '+var_plot+'_1' )
splot, axe_x, var_y, xtitle='RAIN BINS (mm/day)', title=mask_title+' CONVECTIVE '+var_name+' DISTRIBUTION - '+zone, charsize=1.5, ystyle=1, ytitle='CONVECTIVE RAIN DISTRIBUTION (%)', lct=39, charthick=1.5, XTicklen=1, YTicklen=1, XGridStyle=1, YGridStyle=1, xrange=[min_rain,max_rain], /nodata, PSYM=10, YRANGE=[0.0001,100.], /YLOG
FOR e = 1, n_elements(exp_list)-1 DO BEGIN
  cmd = execute( 'var_y = '+var_plot+'_'+strtrim(e,2) )
  oplot, axe_x, var_y, thick=thc, color=color_list[e], PSYM=10
  IF exp_list[e] EQ 'OBS' THEN xyouts, 0.08, 0.175-e*0.025, obs_name, /NORMAL, charsize=chs, color=color_list[e], charthick=1 ELSE xyouts, 0.08, 0.175-e*0.025, exp_list[e], /NORMAL, charsize=chs, color=color_list[e], charthick=1
ENDFOR
IF write_ps THEN closeps ELSE STOP


;-------------------------------------------------------------------------------------------------
; PLOT EXPLICIT RAIN DISTRIBUTION (%)
;-------------------------------------------------------------------------------------------------

var_plot = 'disty_rain' & help, var_plot
fig_name = var_plot+'_'+data_type+'_'+zone+'_'+mask_title
IF write_ps THEN openps, filename=plot_dir+'/'+STRUPCASE(data_type)+'/'+zone+'/'+var_name+'/'+fig_name
axe_x = axe_rain[0:nbin_rain-1] + bin_rain/2.
cmd = execute( 'var_y = '+var_plot+'_1' )
splot, axe_x, var_y, xtitle='RAIN BINS (mm/day)', title=mask_title+' EXPLICIT '+var_name+' DISTRIBUTION - '+zone, charsize=1.5, ystyle=1, ytitle='EXPLICIT RAIN DISTRIBUTION (%)', lct=39, charthick=1.5, XTicklen=1, YTicklen=1, XGridStyle=1, YGridStyle=1, xrange=[min_rain,max_rain], /nodata, PSYM=10, YRANGE=[0.0001,100.], /YLOG
FOR e = 1, n_elements(exp_list)-1 DO BEGIN
  cmd = execute( 'var_y = '+var_plot+'_'+strtrim(e,2) )
  oplot, axe_x, var_y, thick=thc, color=color_list[e], PSYM=10
  IF exp_list[e] EQ 'OBS' THEN xyouts, 0.08, 0.175-e*0.025, obs_name, /NORMAL, charsize=chs, color=color_list[e], charthick=1 ELSE xyouts, 0.08, 0.175-e*0.025, exp_list[e], /NORMAL, charsize=chs, color=color_list[e], charthick=1
ENDFOR
IF write_ps THEN closeps ELSE STOP


;-------------------------------------------------------------------------------------------------
; PLOT CONVECTIVE RAIN RATIO (%)
;-------------------------------------------------------------------------------------------------

var_plot = 'ratiox_rain' & help, var_plot
fig_name = var_plot+'_'+data_type+'_'+zone+'_'+mask_title
IF write_ps THEN openps, filename=plot_dir+'/'+STRUPCASE(data_type)+'/'+zone+'/'+var_name+'/'+fig_name
axe_x = axe_rain[0:nbin_rain-1] + bin_rain/2.
cmd = execute( 'var_y = '+var_plot+'_1' )
splot, axe_x, var_y, xtitle='RAIN BINS (mm/day)', title=mask_title+' CONVECTIVE '+var_name+' RATIO - '+zone, charsize=1.5, ystyle=1, ytitle='CONVECTIVE RAIN RATIO (%)', lct=39, charthick=1.5, XTicklen=1, YTicklen=1, XGridStyle=1, YGridStyle=1, xrange=[min_rain,max_rain], /nodata, PSYM=10, YRANGE=[0.0001,100.];, /YLOG
FOR e = 1, n_elements(exp_list)-1 DO BEGIN
  cmd = execute( 'var_y = '+var_plot+'_'+strtrim(e,2) )
  oplot, axe_x, var_y, thick=thc, color=color_list[e], PSYM=10
  IF exp_list[e] EQ 'OBS' THEN xyouts, 0.08, 0.175-e*0.025, obs_name, /NORMAL, charsize=chs, color=color_list[e], charthick=1 ELSE xyouts, 0.08, 0.175-e*0.025, exp_list[e], /NORMAL, charsize=chs, color=color_list[e], charthick=1
ENDFOR
;FOR i = 0, nbin_rain-1 DO xyouts, axe_rain[0:nbin_rain-1], maxvar, STRTRIM(dist_bin,2), /DATA
IF write_ps THEN closeps ELSE STOP


;-------------------------------------------------------------------------------------------------
; PLOT CONVECTIVE RAIN RATIO (%)
;-------------------------------------------------------------------------------------------------

var_plot = 'ratx_rain' & help, var_plot
fig_name = var_plot+'_'+data_type+'_'+zone+'_'+mask_title
IF write_ps THEN openps, filename=plot_dir+'/'+STRUPCASE(data_type)+'/'+zone+'/'+var_name+'/'+fig_name
axe_x = axe_rain[0:nbin_rain-1] + bin_rain/2.
cmd = execute( 'var_y = '+var_plot+'_1' )
splot, axe_x, var_y, xtitle='RAIN BINS (mm/day)', title=mask_title+' CONVECTIVE '+var_name+' RATIO - '+zone, charsize=1.5, ystyle=1, ytitle='CONVECTIVE RAIN RATIO (%)', lct=39, charthick=1.5, XTicklen=1, YTicklen=1, XGridStyle=1, YGridStyle=1, xrange=[min_rain,max_rain], /nodata, PSYM=10, YRANGE=[0.0001,100.];, /YLOG
FOR e = 1, n_elements(exp_list)-1 DO BEGIN
  cmd = execute( 'var_y = '+var_plot+'_'+strtrim(e,2) )
  oplot, axe_x, var_y, thick=thc, color=color_list[e], PSYM=10
  IF exp_list[e] EQ 'OBS' THEN xyouts, 0.08, 0.175-e*0.025, obs_name, /NORMAL, charsize=chs, color=color_list[e], charthick=1 ELSE xyouts, 0.08, 0.175-e*0.025, exp_list[e], /NORMAL, charsize=chs, color=color_list[e], charthick=1
ENDFOR
;FOR i = 0, nbin_rain-1 DO xyouts, axe_rain[0:nbin_rain-1], maxvar, STRTRIM(dist_bin,2), /DATA
IF write_ps THEN closeps ELSE STOP


;-------------------------------------------------------------------------------------------------
; PLOT EXPLICIT RAIN RATIO (%)
;-------------------------------------------------------------------------------------------------

var_plot = 'ratioy_rain' & help, var_plot
fig_name = var_plot+'_'+data_type+'_'+zone+'_'+mask_title
IF write_ps THEN openps, filename=plot_dir+'/'+STRUPCASE(data_type)+'/'+zone+'/'+var_name+'/'+fig_name
axe_x = axe_rain[0:nbin_rain-1] + bin_rain/2.
cmd = execute( 'var_y = '+var_plot+'_1' )
splot, axe_x, var_y, xtitle='RAIN BINS (mm/day)', title=mask_title+' EXPLICIT '+var_name+' RATIO - '+zone, charsize=1.5, ystyle=1, ytitle='EXPLICIT RAIN RATIO (%)', lct=39, charthick=1.5, XTicklen=1, YTicklen=1, XGridStyle=1, YGridStyle=1, xrange=[min_rain,max_rain], /nodata, PSYM=10, YRANGE=[0.0001,100.];, /YLOG
FOR e = 1, n_elements(exp_list)-1 DO BEGIN
  cmd = execute( 'var_y = '+var_plot+'_'+strtrim(e,2) )
  oplot, axe_x, var_y, thick=thc, color=color_list[e], PSYM=10
  IF exp_list[e] EQ 'OBS' THEN xyouts, 0.08, 0.175-e*0.025, obs_name, /NORMAL, charsize=chs, color=color_list[e], charthick=1 ELSE xyouts, 0.08, 0.175-e*0.025, exp_list[e], /NORMAL, charsize=chs, color=color_list[e], charthick=1
ENDFOR
;FOR i = 0, nbin_rain-1 DO xyouts, axe_rain[0:nbin_rain-1], maxvar, STRTRIM(dist_bin,2), /DATA
IF write_ps THEN closeps ELSE STOP


;-------------------------------------------------------------------------------------------------
; PLOT EXPLICIT RAIN RATIO (%)
;-------------------------------------------------------------------------------------------------

var_plot = 'raty_rain' & help, var_plot
fig_name = var_plot+'_'+data_type+'_'+zone+'_'+mask_title
IF write_ps THEN openps, filename=plot_dir+'/'+STRUPCASE(data_type)+'/'+zone+'/'+var_name+'/'+fig_name
axe_x = axe_rain[0:nbin_rain-1] + bin_rain/2.
cmd = execute( 'var_y = '+var_plot+'_1' )
splot, axe_x, var_y, xtitle='RAIN BINS (mm/day)', title=mask_title+' EXPLICIT '+var_name+' RATIO - '+zone, charsize=1.5, ystyle=1, ytitle='EXPLICIT RAIN RATIO (%)', lct=39, charthick=1.5, XTicklen=1, YTicklen=1, XGridStyle=1, YGridStyle=1, xrange=[min_rain,max_rain], /nodata, PSYM=10, YRANGE=[0.0001,100.];, /YLOG
FOR e = 1, n_elements(exp_list)-1 DO BEGIN
  cmd = execute( 'var_y = '+var_plot+'_'+strtrim(e,2) )
  oplot, axe_x, var_y, thick=thc, color=color_list[e], PSYM=10
  IF exp_list[e] EQ 'OBS' THEN xyouts, 0.08, 0.175-e*0.025, obs_name, /NORMAL, charsize=chs, color=color_list[e], charthick=1 ELSE xyouts, 0.08, 0.175-e*0.025, exp_list[e], /NORMAL, charsize=chs, color=color_list[e], charthick=1
ENDFOR
IF write_ps THEN closeps ELSE STOP


;-------------------------------------------------------------------------------------------------
; PLOT TOTAL RAIN DISTRIBUTION (mm/y)
;-------------------------------------------------------------------------------------------------

var_plot = 'tot_rain' & help, var_plot
fig_name = var_plot+'_'+data_type+'_'+zone+'_'+mask_title
IF write_ps THEN openps, filename=plot_dir+'/'+STRUPCASE(data_type)+'/'+zone+'/'+var_name+'/'+fig_name
@def_plot_minmax
maxvar = 5.0e+7
axe_x = axe_rain[0:nbin_rain-1] + bin_rain/2.
cmd = execute( 'var_y = '+var_plot+'_0' )
splot, axe_x, var_y, xtitle='RAIN BINS (mm/day)', title=mask_title+' TOTAL '+var_name+' DISTRIBUTION - '+zone, charsize=1.5, ystyle=1, ytitle='TOTAL RAIN (mm/year)', lct=39, charthick=1.5, XTicklen=1, YTicklen=1, XGridStyle=1, YGridStyle=1, xrange=[min_rain,max_rain], /nodata, PSYM=10, YRANGE=[1000,maxvar], /YLOG, linestyle=2
FOR e = 0, n_elements(exp_list)-1 DO BEGIN
  cmd = execute( 'var_y = '+var_plot+'_'+strtrim(e,2) )
  oplot, axe_x, var_y, thick=thc, color=color_list[e], PSYM=10
  IF exp_list[e] EQ 'OBS' THEN xyouts, 0.08, 0.175-e*0.025, obs_name, /NORMAL, charsize=chs, color=color_list[e], charthick=1 ELSE xyouts, 0.08, 0.175-e*0.025, exp_list[e], /NORMAL, charsize=chs, color=color_list[e], charthick=1
ENDFOR
IF write_ps THEN closeps ELSE STOP


;-------------------------------------------------------------------------------------------------
; PLOT CONVECTIVE RAIN DISTRIBUTION (mm/y)
;-------------------------------------------------------------------------------------------------

var_plot = 'conv_rain' & help, var_plot
fig_name = var_plot+'_'+data_type+'_'+zone+'_'+mask_title
IF write_ps THEN openps, filename=plot_dir+'/'+STRUPCASE(data_type)+'/'+zone+'/'+var_name+'/'+fig_name
@def_plot_minmax
maxvar = 5.0e+7
axe_x = axe_rain[0:nbin_rain-1] + bin_rain/2.
cmd = execute( 'var_y = '+var_plot+'_1' )
splot, axe_x, var_y, xtitle='RAIN BINS (mm/day)', title=mask_title+' CONVECTIVE '+var_name+' DISTRIBUTION - '+zone, charsize=1.5, ystyle=1, ytitle='CONVECTIVE RAIN (mm/year)', lct=39, charthick=1.5, XTicklen=1, YTicklen=1, XGridStyle=1, YGridStyle=1, xrange=[min_rain,max_rain], /nodata, PSYM=10, YRANGE=[1000,maxvar], /YLOG, linestyle=2
FOR e = 1, n_elements(exp_list)-1 DO BEGIN
  cmd = execute( 'var_y = '+var_plot+'_'+strtrim(e,2) )
  oplot, axe_x, var_y, thick=thc, color=color_list[e], PSYM=10
  IF exp_list[e] EQ 'OBS' THEN xyouts, 0.08, 0.175-e*0.025, obs_name, /NORMAL, charsize=chs, color=color_list[e], charthick=1 ELSE xyouts, 0.08, 0.175-e*0.025, exp_list[e], /NORMAL, charsize=chs, color=color_list[e], charthick=1
ENDFOR
IF write_ps THEN closeps ELSE STOP


;-------------------------------------------------------------------------------------------------
; PLOT EXPLICIT RAIN DISTRIBUTION (mm/y)
;-------------------------------------------------------------------------------------------------

var_plot = 'exp_rain' & help, var_plot
fig_name = var_plot+'_'+data_type+'_'+zone+'_'+mask_title
IF write_ps THEN openps, filename=plot_dir+'/'+STRUPCASE(data_type)+'/'+zone+'/'+var_name+'/'+fig_name
@def_plot_minmax
maxvar = 5.0e+7
axe_x = axe_rain[0:nbin_rain-1] + bin_rain/2.
cmd = execute( 'var_y = '+var_plot+'_1' )
splot, axe_x, var_y, xtitle='RAIN BINS (mm/day)', title=mask_title+' EXPLICIT '+var_name+' DISTRIBUTION - '+zone, charsize=1.5, ystyle=1, ytitle='EXPLICIT RAIN (mm/year)', lct=39, charthick=1.5, XTicklen=1, YTicklen=1, XGridStyle=1, YGridStyle=1, xrange=[min_rain,max_rain], /nodata, PSYM=10, YRANGE=[1000,maxvar], /YLOG, linestyle=2
FOR e = 1, n_elements(exp_list)-1 DO BEGIN
  cmd = execute( 'var_y = '+var_plot+'_'+strtrim(e,2) )
  oplot, axe_x, var_y, thick=thc, color=color_list[e], PSYM=10
  IF exp_list[e] EQ 'OBS' THEN xyouts, 0.08, 0.175-e*0.025, obs_name, /NORMAL, charsize=chs, color=color_list[e], charthick=1 ELSE xyouts, 0.08, 0.175-e*0.025, exp_list[e], /NORMAL, charsize=chs, color=color_list[e], charthick=1
ENDFOR
IF write_ps THEN closeps ELSE STOP


;-------------------------------------------------------------------------------------------------
; PLOT EXPLICIT RAIN DISTRIBUTION (mm/y)
;-------------------------------------------------------------------------------------------------

FOR e = 1, n_elements(exp_list)-1 DO cmd = execute( 'expli_rain_'+STRTRIM(e,2)+' = raty_rain_'+STRTRIM(e,2)+'/100. * conv_rain_'+STRTRIM(e,2) )
var_plot = 'expli_rain' & help, var_plot
fig_name = var_plot+'_'+data_type+'_'+zone+'_'+mask_title
IF write_ps THEN openps, filename=plot_dir+'/'+STRUPCASE(data_type)+'/'+zone+'/'+var_name+'/'+fig_name
@def_plot_minmax
maxvar = 5.0e+7
axe_x = axe_rain[0:nbin_rain-1] + bin_rain/2.
cmd = execute( 'var_y = '+var_plot+'_1' )
splot, axe_x, var_y, xtitle='RAIN BINS (mm/day)', title=mask_title+' EXPLICIT '+var_name+' DISTRIBUTION - '+zone, charsize=1.5, ystyle=1, ytitle='EXPLICIT RAIN (mm/year)', lct=39, charthick=1.5, XTicklen=1, YTicklen=1, XGridStyle=1, YGridStyle=1, xrange=[min_rain,max_rain], /nodata, PSYM=10, YRANGE=[1000,maxvar], /YLOG, linestyle=2
FOR e = 1, n_elements(exp_list)-1 DO BEGIN
  cmd = execute( 'var_y = '+var_plot+'_'+strtrim(e,2) )
  oplot, axe_x, var_y, thick=thc, color=color_list[e], PSYM=10
  IF exp_list[e] EQ 'OBS' THEN xyouts, 0.08, 0.175-e*0.025, obs_name, /NORMAL, charsize=chs, color=color_list[e], charthick=1 ELSE xyouts, 0.08, 0.175-e*0.025, exp_list[e], /NORMAL, charsize=chs, color=color_list[e], charthick=1
ENDFOR
IF write_ps THEN closeps ELSE STOP


;-------------------------------------------------------------------------------------------------
; PLOT EXPLICIT RAIN DISTRIBUTION (mm/y)
;-------------------------------------------------------------------------------------------------

FOR e = 1, n_elements(exp_list)-1 DO cmd = execute( 'expli_rain_'+STRTRIM(e,2)+' = ratioy_rain_'+STRTRIM(e,2)+'/100. * conv_rain_'+STRTRIM(e,2) )
var_plot = 'expli_rain' & help, var_plot
fig_name = var_plot+'_'+data_type+'_'+zone+'_'+mask_title
IF write_ps THEN openps, filename=plot_dir+'/'+STRUPCASE(data_type)+'/'+zone+'/'+var_name+'/'+fig_name
@def_plot_minmax
maxvar = 5.0e+7
axe_x = axe_rain[0:nbin_rain-1] + bin_rain/2.
cmd = execute( 'var_y = '+var_plot+'_1' )
splot, axe_x, var_y, xtitle='RAIN BINS (mm/day)', title=mask_title+' EXPLICIT '+var_name+' DISTRIBUTION - '+zone, charsize=1.5, ystyle=1, ytitle='EXPLICIT RAIN (mm/year)', lct=39, charthick=1.5, XTicklen=1, YTicklen=1, XGridStyle=1, YGridStyle=1, xrange=[min_rain,max_rain], /nodata, PSYM=10, YRANGE=[1000,maxvar], /YLOG, linestyle=2
FOR e = 1, n_elements(exp_list)-1 DO BEGIN
  cmd = execute( 'var_y = '+var_plot+'_'+strtrim(e,2) )
  oplot, axe_x, var_y, thick=thc, color=color_list[e], PSYM=10
  IF exp_list[e] EQ 'OBS' THEN xyouts, 0.08, 0.175-e*0.025, obs_name, /NORMAL, charsize=chs, color=color_list[e], charthick=1 ELSE xyouts, 0.08, 0.175-e*0.025, exp_list[e], /NORMAL, charsize=chs, color=color_list[e], charthick=1
ENDFOR
;FOR i = 0, nbin_rain-1 DO xyouts, axe_rain[0:nbin_rain-1], maxvar, STRTRIM(dist_bin,2), /DATA
IF write_ps THEN closeps ELSE STOP



;-------------------------------------------------------------------------------------------------
; PLOT RAIN ERROR DISTRIBUTION (mm/y)
;-------------------------------------------------------------------------------------------------

var_plot = 'errtot_rain' & help, var_plot
fig_name = var_plot+'_'+data_type+'_'+zone+'_'+mask_title
IF write_ps THEN openps, filename=plot_dir+'/'+STRUPCASE(data_type)+'/'+zone+'/'+var_name+'/'+fig_name
@def_plot_minmax
axe_x = axe_rain[0:nbin_rain-1] + bin_rain/2.
cmd = execute( 'var_y = '+var_plot+'_1' )
splot, axe_x, var_y, xtitle='RAIN BINS (mm/day)', title=mask_title+' '+var_name+' ERROR DISTRIBUTION - '+zone, charsize=1.5, ystyle=1, ytitle='RAIN ERROR (mm/year)', lct=39, charthick=1.5, XTicklen=1, YTicklen=1, XGridStyle=1, YGridStyle=1, xrange=[min_rain,max_rain], /nodata, PSYM=10, YRANGE=[1000,maxvar], /YLOG, linestyle=2
FOR e = 0, n_elements(exp_list)-1 DO BEGIN
  cmd = execute( 'var_y = '+var_plot+'_'+strtrim(e,2) )
  oplot, axe_x, var_y, thick=thc, color=color_list[e], PSYM=10
  IF exp_list[e] EQ 'OBS' THEN xyouts, 0.08, 0.175-e*0.025, obs_name, /NORMAL, charsize=chs, color=color_list[e], charthick=1 ELSE xyouts, 0.08, 0.175-e*0.025, exp_list[e], /NORMAL, charsize=chs, color=color_list[e], charthick=1
ENDFOR
IF write_ps THEN closeps ELSE STOP


;-------------------------------------------------------------------------------------------------
; PLOT CUMULATIVE TOTAL RAIN DISTRIBUTION (mm/y)
;-------------------------------------------------------------------------------------------------

var_plot = 'cum_rain' & help, var_plot
fig_name = var_plot+'_'+data_type+'_'+zone+'_'+mask_title
IF write_ps THEN openps, filename=plot_dir+'/'+STRUPCASE(data_type)+'/'+zone+'/'+var_name+'/'+fig_name
@def_plot_minmax
;minvar = 1.0e+6
;maxvar = 1.4e+8
axe_x = axe_rain[0:nbin_rain-1] + bin_rain/2.
cmd = execute( 'var_y = '+var_plot+'_0' )
splot, axe_x, var_y, xtitle='RAIN BINS (mm/day)', title='CUMULATIVE '+mask_title+' '+var_name+' DISTRIBUTION - '+zone, charsize=1.5, ystyle=1, ytitle='TOTAL RAIN (mm/year)', lct=39, charthick=1.5, XTicklen=1, YTicklen=1, XGridStyle=1, YGridStyle=1, xrange=[min_rain,max_rain], /nodata, PSYM=10, YRANGE=[minvar,maxvar], linestyle=2;, /YLOG
FOR e = 0, n_elements(exp_list)-1 DO BEGIN
  cmd = execute( 'var_y = '+var_plot+'_'+strtrim(e,2) )
  oplot, axe_x, var_y, thick=thc, color=color_list[e], PSYM=10
  IF exp_list[e] EQ 'OBS' THEN xyouts, 0.08, 0.175-e*0.025, obs_name, /NORMAL, charsize=chs, color=color_list[e], charthick=1 ELSE xyouts, 0.08, 0.175-e*0.025, exp_list[e], /NORMAL, charsize=chs, color=color_list[e], charthick=1
ENDFOR
IF write_ps THEN closeps ELSE STOP


;-------------------------------------------------------------------------------------------------
; PLOT CUMULATIVE CONVECTIVE RAIN DISTRIBUTION (mm/y)
;-------------------------------------------------------------------------------------------------

var_plot = 'cumx_rain' & help, var_plot
fig_name = var_plot+'_'+data_type+'_'+zone+'_'+mask_title
IF write_ps THEN openps, filename=plot_dir+'/'+STRUPCASE(data_type)+'/'+zone+'/'+var_name+'/'+fig_name
@def_plot_minmax
;minvar = 1.0e+6
;maxvar = 1.4e+8
axe_x = axe_rain[0:nbin_rain-1] + bin_rain/2.
cmd = execute( 'var_y = '+var_plot+'_1' )
splot, axe_x, var_y, xtitle='RAIN BINS (mm/day)', title='CUMULATIVE '+mask_title+' '+var_name+' DISTRIBUTION - '+zone, charsize=1.5, ystyle=1, ytitle='CONVECTIVE RAIN (mm/year)', lct=39, charthick=1.5, XTicklen=1, YTicklen=1, XGridStyle=1, YGridStyle=1, xrange=[min_rain,max_rain], /nodata, PSYM=10, YRANGE=[minvar,maxvar], linestyle=2;, /YLOG
FOR e = 1, n_elements(exp_list)-1 DO BEGIN
  cmd = execute( 'var_y = '+var_plot+'_'+strtrim(e,2) )
  oplot, axe_x, var_y, thick=thc, color=color_list[e], PSYM=10
  IF exp_list[e] EQ 'OBS' THEN xyouts, 0.08, 0.175-e*0.025, obs_name, /NORMAL, charsize=chs, color=color_list[e], charthick=1 ELSE xyouts, 0.08, 0.175-e*0.025, exp_list[e], /NORMAL, charsize=chs, color=color_list[e], charthick=1
ENDFOR
IF write_ps THEN closeps ELSE STOP


;-------------------------------------------------------------------------------------------------
; PLOT CUMULATIVE EXPLICIT RAIN DISTRIBUTION (mm/y)
;-------------------------------------------------------------------------------------------------

var_plot = 'cumy_rain' & help, var_plot
fig_name = var_plot+'_'+data_type+'_'+zone+'_'+mask_title
IF write_ps THEN openps, filename=plot_dir+'/'+STRUPCASE(data_type)+'/'+zone+'/'+var_name+'/'+fig_name
@def_plot_minmax
;minvar = 1.0e+6
;maxvar = 1.4e+8
axe_x = axe_rain[0:nbin_rain-1] + bin_rain/2.
cmd = execute( 'var_y = '+var_plot+'_1' )
splot, axe_x, var_y, xtitle='RAIN BINS (mm/day)', title='CUMULATIVE '+mask_title+' '+var_name+' DISTRIBUTION - '+zone, charsize=1.5, ystyle=1, ytitle='EXPLICIT RAIN (mm/year)', lct=39, charthick=1.5, XTicklen=1, YTicklen=1, XGridStyle=1, YGridStyle=1, xrange=[min_rain,max_rain], /nodata, PSYM=10, YRANGE=[minvar,maxvar], linestyle=2;, /YLOG
FOR e = 1, n_elements(exp_list)-1 DO BEGIN
  cmd = execute( 'var_y = '+var_plot+'_'+strtrim(e,2) )
  oplot, axe_x, var_y, thick=thc, color=color_list[e], PSYM=10
  IF exp_list[e] EQ 'OBS' THEN xyouts, 0.08, 0.175-e*0.025, obs_name, /NORMAL, charsize=chs, color=color_list[e], charthick=1 ELSE xyouts, 0.08, 0.175-e*0.025, exp_list[e], /NORMAL, charsize=chs, color=color_list[e], charthick=1
ENDFOR
IF write_ps THEN closeps ELSE STOP


;-------------------------------------------------------------------------------------------------
; PLOT CUMULATIVE EXPLICIT RAIN DISTRIBUTION (mm/y)
;-------------------------------------------------------------------------------------------------

FOR e = 1, n_elements(exp_list)-1 DO cmd = execute( 'cumexp_rain_'+strtrim(e,2)+'= TOTAL( expli_rain_' +strtrim(e,2)+', /CUMULATIVE, /NAN)' ) ; mm/year
var_plot = 'cumexp_rain' & help, var_plot
fig_name = var_plot+'_'+data_type+'_'+zone+'_'+mask_title
IF write_ps THEN openps, filename=plot_dir+'/'+STRUPCASE(data_type)+'/'+zone+'/'+var_name+'/'+fig_name
@def_plot_minmax
;minvar = 1.0e+6
;maxvar = 1.4e+8
axe_x = axe_rain[0:nbin_rain-1] + bin_rain/2.
cmd = execute( 'var_y = '+var_plot+'_1' )
splot, axe_x, var_y, xtitle='RAIN BINS (mm/day)', title='CUMULATIVE '+mask_title+' '+var_name+' DISTRIBUTION - '+zone, charsize=1.5, ystyle=1, ytitle='EXPLICIT RAIN (mm/year)', lct=39, charthick=1.5, XTicklen=1, YTicklen=1, XGridStyle=1, YGridStyle=1, xrange=[min_rain,max_rain], /nodata, PSYM=10, YRANGE=[minvar,maxvar], linestyle=2;, /YLOG
FOR e = 1, n_elements(exp_list)-1 DO BEGIN
  cmd = execute( 'var_y = '+var_plot+'_'+strtrim(e,2) )
  oplot, axe_x, var_y, thick=thc, color=color_list[e], PSYM=10
  IF exp_list[e] EQ 'OBS' THEN xyouts, 0.08, 0.175-e*0.025, obs_name, /NORMAL, charsize=chs, color=color_list[e], charthick=1 ELSE xyouts, 0.08, 0.175-e*0.025, exp_list[e], /NORMAL, charsize=chs, color=color_list[e], charthick=1
ENDFOR
IF write_ps THEN closeps ELSE STOP


;-------------------------------------------------------------------------------------------------
; PLOT CUMULATIVE TOTAL RAIN DISTRIBUTION (%)
;-------------------------------------------------------------------------------------------------

FOR e = 0, n_elements(exp_list)-1 DO cmd = execute( 'cumpc_rain_'+STRTRIM(e,2)+' = cum_rain_'+STRTRIM(e,2)+' / cum_rain_'+STRTRIM(e,2)+'[-1] * 100.' )
var_plot = 'cumpc_rain' & help, var_plot
fig_name = var_plot+'_'+data_type+'_'+zone+'_'+mask_title
IF write_ps THEN openps, filename=plot_dir+'/'+STRUPCASE(data_type)+'/'+zone+'/'+var_name+'/'+fig_name
@def_plot_minmax
;minvar = 10.
;maxvar = 200.
axe_x = axe_rain[0:nbin_rain-1] + bin_rain/2.
cmd = execute( 'var_y = '+var_plot+'_0' )
splot, axe_x, var_y, xtitle='RAIN BINS (mm/day)', title='CUMULATIVE '+mask_title+' '+var_name+' DISTRIBUTION - '+zone, charsize=1.5, ystyle=1, ytitle='CUMULATIVE TOTAL RAIN RATIO (%)', lct=39, charthick=1.5, XTicklen=1, YTicklen=1, XGridStyle=1, YGridStyle=1, xrange=[min_rain,max_rain], /nodata, PSYM=10, YRANGE=[minvar,maxvar], linestyle=2, /YLOG
FOR e = 0, n_elements(exp_list)-1 DO BEGIN
  cmd = execute( 'var_y = '+var_plot+'_'+strtrim(e,2) )
  oplot, axe_x, var_y, thick=thc, color=color_list[e], PSYM=10
  IF exp_list[e] EQ 'OBS' THEN xyouts, 0.08, 0.175-e*0.025, obs_name, /NORMAL, charsize=chs, color=color_list[e], charthick=1 ELSE xyouts, 0.08, 0.175-e*0.025, exp_list[e], /NORMAL, charsize=chs, color=color_list[e], charthick=1
ENDFOR
IF write_ps THEN closeps ELSE STOP


;-------------------------------------------------------------------------------------------------
; PLOT CUMULATIVE RAIN ERROR DISTRIBUTION (mm/y)
;-------------------------------------------------------------------------------------------------

var_plot = 'errcum_rain' & help, var_plot
fig_name = var_plot+'_'+data_type+'_'+zone+'_'+mask_title
IF write_ps THEN openps, filename=plot_dir+'/'+STRUPCASE(data_type)+'/'+zone+'/'+var_name+'/'+fig_name
@def_plot_minmax
axe_x = axe_rain[0:nbin_rain-1] + bin_rain/2.
cmd = execute( 'var_y = '+var_plot+'_1' )
splot, axe_x, var_y, xtitle='RAIN BINS (mm/day)', title='CUMULATIVE '+mask_title+' '+var_name+' ERROR DISTRIBUTION - '+zone, charsize=1.5, ystyle=1, ytitle='RAIN ERROR (mm/year)', lct=39, charthick=1.5, XTicklen=1, YTicklen=1, XGridStyle=1, YGridStyle=1, xrange=[min_rain,max_rain], /nodata, PSYM=10, YRANGE=[minvar,maxvar], linestyle=2;, /YLOG
FOR e = 1, n_elements(exp_list)-1 DO BEGIN
  cmd = execute( 'var_y = '+var_plot+'_'+strtrim(e,2) )
  oplot, axe_x, var_y, thick=thc, color=color_list[e], PSYM=10
  IF exp_list[e] EQ 'OBS' THEN xyouts, 0.08, 0.175-e*0.025, obs_name, /NORMAL, charsize=chs, color=color_list[e], charthick=1 ELSE xyouts, 0.08, 0.175-e*0.025, exp_list[e], /NORMAL, charsize=chs, color=color_list[e], charthick=1
ENDFOR
;FOR i = 0, nbin_rain-1 DO xyouts, axe_rain[0:nbin_rain-1], maxvar, STRTRIM(dist_bin,2), /DATA
IF write_ps THEN closeps ELSE STOP


;-------------------------------------------------------------------------------------------------
; PLOT RAIN RATIO MODEL / OBS
;-------------------------------------------------------------------------------------------------

var_plot = 'ratio_rain' & help, var_plot
fig_name = var_plot+'_'+data_type+'_'+zone+'_'+mask_title
IF write_ps THEN openps, filename=plot_dir+'/'+STRUPCASE(data_type)+'/'+zone+'/'+var_name+'/'+fig_name
FOR e = 1, n_elements(exp_list)-1 DO cmd = execute( var_plot+'_'+strtrim(e,2)+'  = tot_rain_'+strtrim(e,2)+' / tot_rain_0' )
@def_plot_minmax
axe_x = axe_rain[0:nbin_rain-1] + bin_rain/2.
cmd = execute( 'var_y = '+var_plot+'_1' )
splot, axe_x, var_y, xtitle='RAIN BINS (mm/day)', title=mask_title+' '+var_name+' RATIO DISTRIBUTION - '+zone, charsize=1.5, ystyle=1, ytitle='RAIN RATIO (model/obs)', lct=39, charthick=1.5, XTicklen=1, YTicklen=1, XGridStyle=1, YGridStyle=1, xrange=[min_rain,max_rain], PSYM=10, YRANGE=[minvar,maxvar], /NODATA
FOR e = 1, n_elements(exp_list)-1 DO BEGIN
  cmd = execute( 'var_y = '+var_plot+'_'+strtrim(e,2) )
  oplot, axe_x, var_y, thick=thc, color=color_list[e], PSYM=10
  IF exp_list[e] EQ 'OBS' THEN xyouts, 0.08, 0.175-e*0.025, obs_name, /NORMAL, charsize=chs, color=color_list[e], charthick=1 ELSE xyouts, 0.08, 0.175-e*0.025, exp_list[e], /NORMAL, charsize=chs, color=color_list[e], charthick=1
ENDFOR
oplot, [min_rain,max_rain], [1,1], thick=thc 
;FOR i = 0, nbin_rain-1 DO xyouts, axe_rain[0:nbin_rain-1], maxvar, STRTRIM(dist_bin,2), /DATA
IF write_ps THEN closeps ELSE STOP


;-------------------------------------------------------------------------------------------------
; PLOT MODEL RAIN RATIO PARAM / TOTAL
;-------------------------------------------------------------------------------------------------

var_plot = 'conv_rain' & help, var_plot
fig_name = var_plot+'_'+data_type+'_'+zone+'_'+mask_title
IF write_ps THEN openps, filename=plot_dir+'/'+STRUPCASE(data_type)+'/'+zone+'/'+var_name+'/'+fig_name
axe_x = axe_rain[0:nbin_rain-1] + bin_rain/2.
cmd = execute( 'var_y = '+var_plot+'_1' )
splot, axe_x, var_y, xtitle='RAIN BINS (mm/day)', title=mask_title+' CONVECTIVE '+var_name+' RATIO DISTRIBUTION - '+zone, charsize=1.5, ystyle=1, ytitle='CONVECTIVE RAIN RATIO (param/total)', lct=39, charthick=1.5, XTicklen=1, YTicklen=1, XGridStyle=1, YGridStyle=1, xrange=[min_rain,max_rain], /nodata, PSYM=10, YRANGE=[0,1]
FOR e = 1, n_elements(exp_list)-1 DO BEGIN
  cmd = execute( 'var_y = '+var_plot+'_'+strtrim(e,2) )
  oplot, axe_x, var_y, thick=thc, color=color_list[e], PSYM=10
  IF exp_list[e] EQ 'OBS' THEN xyouts, 0.08, 0.175-e*0.025, obs_name, /NORMAL, charsize=chs, color=color_list[e], charthick=1 ELSE xyouts, 0.08, 0.175-e*0.025, exp_list[e], /NORMAL, charsize=chs, color=color_list[e], charthick=1
ENDFOR
;FOR i = 0, nbin_rain-1 DO xyouts, axe_rain[0:nbin_rain-1], maxvar, STRTRIM(dist_bin,2), /DATA
IF write_ps THEN closeps ELSE STOP
