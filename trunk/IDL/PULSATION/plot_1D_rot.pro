
print, '' & print, 'plot_1D_rot: ', var_plot


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
; PLOT 1D
;-------------------------------------------------------------------------------------------------

  @def_plot_minmax

  fig_name = var_name+'_'+var_plot+'_'+data_type+'_'+zone+'_'+period
  IF write_ps THEN openps, filename=plot_dir+'/'+STRUPCASE(data_type)+'/'+zone+'/'+var_name+'/'+fig_name

  cmd = execute( 'axe_x = INDGEN( n_elements('+var_plot+'_0))' )
  cmd = execute( 'var_y = '+var_plot+'_0' )
  splot, axe_x, var_y, xtitle='NB PTS', title=mask_title+' '+var_name+' CROSS AVERAGE - '+zone+' - '+period, charsize=1.5, ystyle=1, yrange=[minvar,maxvar], ytitle=var_name, lct=39, charthick=1.5, XTicklen=1, YTicklen=1, XGridStyle=1, YGridStyle=1, /nodata
  FOR e = 0, n_elements(exp_list)-1 DO BEGIN
    IF e EQ 0 THEN cmd = execute( 'var_y = '+var_plot+'_0' ) ELSE cmd = execute( 'var_y = '+var_plot+'_'+strtrim(e,2)+'_gridobs' )
    oplot, axe_x, var_y, thick=thc, color=color_list[e]
    IF exp_list[e] EQ 'OBS' THEN xyouts, 0.05, 0.175-e*0.025, obs_name, /NORMAL, charsize=chs, color=color_list[e], charthick=1 ELSE xyouts, 0.05, 0.175-e*0.025, exp_list[e], /NORMAL, charsize=chs, color=color_list[e], charthick=1
  ENDFOR

  IF write_gif THEN saveimage, plot_dir+'/'+STRUPCASE(data_type)+'/'+zone+'/'+var_name+'/'+fig_name+'.gif'
  IF write_ps  THEN closeps
  IF write_ps EQ 0 AND write_gif EQ 0 THEN STOP
