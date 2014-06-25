
print, '' & print, 'plot_1D_y:', var_plot


;-------------------------------------------------------------------------------------------------
; PLOT SETUP
;-------------------------------------------------------------------------------------------------

  key_portrait = 1
  @def_plot_mask
  lct, 60 & fmt='(F6.1)'
  IF write_ps THEN thc = 6 ELSE thc = 2
  IF write_ps THEN chs = 1 ELSE chs = 1.5
  color_list = [0, 50, 250, 150, 200, 100, 25]


;-------------------------------------------------------------------------------------------------
; PLOT ZONAL MEAN + ERROR
;-------------------------------------------------------------------------------------------------

  var_xmean_0 = MEAN( var_mean_0, DIMENSION=1, /NAN)
  FOR e = 1, n_elements(exp_list) -1 DO cmd = execute( 'var_xmean_'+strtrim(e,2)+' = MEAN( var_mean_'+strtrim(e,2)+'_gridobs, DIMENSION=1, /NAN)' )

  @def_plot_minmax

  fig_name = var_name+'_'+var_plot+'_'+data_type+'_'+zone+'_'+period
  IF write_ps THEN openps, filename=plot_dir+'/'+STRUPCASE(data_type)+'/'+zone+'/'+var_name+'/'+fig_name

  axe_x = lat_0
  cmd = execute( 'var_y = '+var_plot+'_0' )
  splot, axe_x, var_y, xtitle='LATITUDE', title=mask_title+' '+var_name+' ZONAL AVERAGE - '+zone+' - '+period, charsize=1.5, ystyle=1, yrange=[minvar,maxvar], ytitle=var_name, lct=39, charthick=1.5, XTicklen=1, YTicklen=1, XGridStyle=1, YGridStyle=1, /nodata
  FOR e = 0, n_elements(exp_list)-1 DO BEGIN
    cmd = execute( 'var_y = '+var_plot+'_'+strtrim(e,2) )
    oplot, axe_x, var_y, thick=thc, color=color_list[e]
    IF exp_list[e] EQ 'OBS' THEN xyouts, 0.05, 0.175-e*0.025, obs_name, /NORMAL, charsize=chs, color=color_list[e], charthick=1 ELSE xyouts, 0.05, 0.175-e*0.025, exp_list[e], /NORMAL, charsize=chs, color=color_list[e], charthick=1
  ENDFOR

  IF write_gif THEN saveimage, plot_dir+'/'+STRUPCASE(data_type)+'/'+zone+'/'+var_name+'/'+fig_name+'.gif'
  IF write_ps  THEN closeps
  IF write_ps EQ 0 AND write_gif EQ 0 THEN STOP


; ERROR
IF n_elements(exp_list) GT 1 THEN BEGIN

  FOR e = 1, n_elements(exp_list) -1 DO cmd = execute( 'err_xmean_'+strtrim(e,2)+' = var_xmean_'+strtrim(e,2)+' - var_xmean_0' )

  var_plot = 'err_xmean'
  @def_plot_minmax

  fig_name = var_name+'_'+var_plot+'_'+data_type+'_'+zone+'_'+period
  IF write_ps THEN openps, filename=plot_dir+'/'+STRUPCASE(data_type)+'/'+zone+'/'+var_name+'/'+fig_name

  axe_x = lat_0
  cmd = execute( 'var_y = '+var_plot+'_1' )
  splot, axe_x, var_y, xtitle='LATITUDE', title=mask_title+' '+var_name+' ZONAL AVERAGE ERROR - '+zone+' - '+period, charsize=1.5, ystyle=1, yrange=[minvar,maxvar], ytitle=var_name, lct=39, charthick=1.5, XTicklen=1, YTicklen=1, XGridStyle=1, YGridStyle=1, /nodata
  FOR e = 1, n_elements(exp_list)-1 DO BEGIN
    cmd = execute( 'var_y = '+var_plot+'_'+strtrim(e,2) )
    oplot, axe_x, var_y, thick=thc, color=color_list[e]
    oplot, axe_x, MEAN( var_y, /NAN)+axe_x*0., thick=thc, color=color_list[e], line=2
    oplot, axe_x, axe_x*0., color=0, thick=thc*2.
    IF exp_list[e] EQ 'OBS' THEN xyouts, 0.05, 0.175-e*0.025, obs_name, /NORMAL, charsize=chs, color=color_list[e], charthick=1 ELSE xyouts, 0.05, 0.175-e*0.025, exp_list[e], /NORMAL, charsize=chs, color=color_list[e], charthick=1
  ENDFOR

  IF write_gif THEN saveimage, plot_dir+'/'+STRUPCASE(data_type)+'/'+zone+'/'+var_name+'/'+fig_name+'.gif'
  IF write_ps  THEN closeps
  IF write_ps EQ 0 AND write_gif EQ 0 THEN STOP

ENDIF
