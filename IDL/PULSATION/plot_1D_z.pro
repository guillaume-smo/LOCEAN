
print, '' & print, 'PLOT_ZP: '


;-------------------------------------------------------------------------------------------------
; PLOT SETUP
;-------------------------------------------------------------------------------------------------

  var_plot = 'var_zp_mean' & help, var_plot
  key_portrait = 1
  fig_name = var_name+'_'+var_plot+'_'+data_type+'_'+zone+'_'+STRTRIM(yearini_mod,2)+'-'+STRTRIM(yearend_mod,2)
  IF write_ps THEN openps, filename=plot_dir+'/'+STRUPCASE(data_type)+'/'+zone+'/'+var_name+'/'+fig_name
  IF write_ps THEN thc = 6 ELSE thc = 2
  IF write_ps THEN chs = 1 ELSE chs = 1.5
  color_list = [0, 50, 250, 150, 200, 100, 25]
  IF force_landmask THEN mask_title = 'OCEAN'
  IF force_seamask  THEN mask_title = 'LAND'
  IF force_landmask EQ 0 AND force_seamask EQ 0 THEN mask_title = 'OCEAN+LAND'
  zrange = [975., 200.] ; bornes: 1000hPa - 200hPa
  @def_plot_minmax


;-------------------------------------------------------------------------------------------------
; PLOT
;-------------------------------------------------------------------------------------------------

  cmd = execute( 'var_x = '+var_plot+'_0')
  var_y = Z_0

  splot, var_x, var_y,  ytitle='pressure (hPa)', xtitle=var_name, charsize=1.5, ystyle=1, yrange=zrange, xrange=[minvar,maxvar], title=mask_title+' '+var_name+' '+data_type+' '+period+' VERTICAL PROFILE - ZONE: '+zone, lct=39, charthick=1.5, /nodata, XTicklen=1, YTicklen=1, XGridStyle=1, YGridStyle=1

  FOR e = 0, n_elements(exp_list)-1 DO BEGIN
    cmd = execute( 'var_x = '+var_plot+'_'+strtrim(e,2) )
    cmd = execute( 'var_y = Z_'+strtrim(e,2) )
    oplot, var_x, var_y, thick=thc, color=color_list[e]
    IF exp_list[e] EQ 'OBS' THEN xyouts, 0.05, 0.190-e*0.030, obs_name, /NORMAL, charsize=chs, color=color_list[e], charthick=1 $
    ELSE xyouts, 0.05, 0.190-e*0.030, exp_list[e], /NORMAL, charsize=chs, color=color_list[e], charthick=1
  ENDFOR

  IF write_ps THEN closeps ELSE STOP



;-------------------------------------------------------------------------------------------------
; PLOT SETUP
;-------------------------------------------------------------------------------------------------
IF n_elements(exp_list) GT 1 THEN BEGIN

  FOR e = 1, n_elements(exp_list)-1 DO cmd = execute( 'err_zp_mean_'+STRTRIM(e,2)+' = REVERSE(var_zp_mean_'+STRTRIM(e,2)+'[(listmatch(z_0, z_1))[*,1]] - var_zp_mean_0[(listmatch(z_0, z_'+STRTRIM(e,2)+'))[*,0]] )' )

  var_plot = 'err_zp_mean' & help, var_plot
  key_portrait = 1
  fig_name = var_name+'_'+var_plot+'_'+data_type+'_'+zone+'_'+STRTRIM(yearini_mod,2)+'-'+STRTRIM(yearend_mod,2)
  IF write_ps THEN openps, filename=plot_dir+'/'+STRUPCASE(data_type)+'/'+zone+'/'+var_name+'/'+fig_name
  IF write_ps THEN thc = 6 ELSE thc = 2
  IF write_ps THEN chs = 1 ELSE chs = 1.5
  color_list = [0, 50, 250, 150, 200, 100, 25]
  IF force_landmask THEN mask_title = 'OCEAN'
  IF force_seamask  THEN mask_title = 'LAND'
  IF force_landmask EQ 0 AND force_seamask EQ 0 THEN mask_title = 'OCEAN+LAND'
  @def_plot_minmax


;-------------------------------------------------------------------------------------------------
; PLOT
;-------------------------------------------------------------------------------------------------

  cmd = execute( 'var_x = '+var_plot+'_0')
  var_y = Z_0

  splot, var_x, var_y,  ytitle='pressure (hPa)', xtitle=var_name+' ERROR', charsize=1.5, ystyle=1, yrange=zrange, xrange=[minvar,maxvar], title=mask_title+' '+var_name+' '+data_type+' VERTICAL PROFILE - ZONE: '+zone, lct=39, charthick=1.5, /nodata, XTicklen=1, YTicklen=1, XGridStyle=1, YGridStyle=1

  oplot, [0.,0.], zrange, color=0, thick=thc

  FOR e = 1, n_elements(exp_list)-1 DO BEGIN
    cmd = execute( 'var_x = '+var_plot+'_'+strtrim(e,2) )
    cmd = execute( 'var_y = Z_'+strtrim(e,2) )
    oplot, var_x, var_y, thick=thc, color=color_list[e]
    IF exp_list[e] EQ 'OBS' THEN xyouts, 0.05, 0.190-e*0.030, obs_name, /NORMAL, charsize=chs, color=color_list[e], charthick=1 $
    ELSE xyouts, 0.05, 0.190-e*0.030, exp_list[e], /NORMAL, charsize=chs, color=color_list[e], charthick=1
  ENDFOR

  IF write_ps THEN closeps ELSE STOP

ENDIF
