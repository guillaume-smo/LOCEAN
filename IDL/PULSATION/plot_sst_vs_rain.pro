
;-------------------------------------------------------------------------------------------------
; PLOT SETUP
;-------------------------------------------------------------------------------------------------

  key_portrait = 1
  fig_name ='SST_vs_RAIN_'+data_type+'_'+zone+'_'+STRTRIM(yearini_mod,2)+'-'+STRTRIM(yearend_mod,2)
  IF write_ps THEN openps, filename=plot_dir+'/'+STRUPCASE(data_type)+'/'+zone+'/'+var_name+'/'+fig_name
  IF write_ps THEN thc = 6 ELSE thc = 2
  IF write_ps THEN chs = 1 ELSE chs = 1.5
  color_list = [0, 50, 250, 150, 200, 100, 25, 75, 175, 225, 125, 60, 260, 160, 210]
  IF force_landmask THEN mask_title = 'OCEAN'
  IF force_seamask  THEN mask_title = 'LAND'
  IF force_landmask EQ 0 AND force_seamask EQ 0 THEN mask_title = 'OCEAN+LAND'

  sst_bin = 0.5
  sst_min = 26.
  sst_max = 31.
  rain_bin = 2.
  rain_min = 0.
  rain_max = 20.

  nbc = 10  ; nombre de contours
  dtc = 1.  ; delta entre chaque contour


;-------------------------------------------------------------------------------------------------
; PLOT
;-------------------------------------------------------------------------------------------------

FOR e = 0, n_elements(exp_list)-1 DO BEGIN

  cmd = execute( 'varx = '+var_plotx+'_'+STRTRIM(e,2) )
  cmd = execute( 'vary = '+var_ploty+'_'+STRTRIM(e,2) )
  cmd = execute( 'indok = WHERE( FINITE( varx) EQ 1 AND FINITE( vary) EQ 1)' )
  help, indok
  cmd = execute( 'varx = varx[indok]' )
  cmd = execute( 'vary = vary[indok]' )
  help, varx, vary

  hist2D = HIST_2D( varx, vary, min1=sst_min, max1=sst_max, bin1=sst_bin, min2=rain_min, max2=rain_max, bin2=rain_bin)
  print, 'MIN/MAX  :', MIN( hist2D, /NAN), MAX( hist2D, /NAN)
  print, 'MIN/MAX %:', MIN( hist2D/TOTAL( hist2D, /NAN)*100., /NAN), MAX( hist2D/TOTAL( hist2D, /NAN)*100., /NAN)


  axex = sst_min  + FINDGEN( ( sst_max- sst_min) /  sst_bin + 1) * sst_bin
  axey = rain_min + FINDGEN( (rain_max-rain_min) / rain_bin + 1) * rain_bin
  help, axex, axey

  IF e EQ 0 THEN $
  scontour, hist2D/total(hist2D)*100., axex, axey, levels=INDGEN(nbc)*dtc, c_labels=intarr(nbc)+1., c_thick=thc, $
  xtitle = 'SST (degC)', ytitle = 'RAIN (mm/day)', title=data_type+' SST-RAIN Relationship - '+mask_title+' '+zone, charsize=chs $
  ELSE $
  scontour, hist2D/total(hist2D)*100., axex, axey, levels=INDGEN(nbc)*dtc, c_labels=INTARR(nbc)+1, c_colors=INTARR(nbc)+color_list[e], c_thick=thc, /OVERPLOT
  xyouts, 0.05, 0.190-e*0.030, exp_list[e], /NORMAL, charsize=chs, color=color_list[e], charthick=1

ENDFOR
STOP
