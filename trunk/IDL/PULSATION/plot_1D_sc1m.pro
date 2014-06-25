
print, 'plot 1D sc1m: var_sc1m'


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
; PLOT SC1m
;-------------------------------------------------------------------------------------------------

  fig_name = var_name+'_sc1m_'+zone
  IF write_ps THEN openps, filename=plot_dir+'/'+STRUPCASE(data_type)+'/'+zone+'/'+var_name+'/'+fig_name

  minvar = MIN(var_sc1m_0, /NAN) & maxvar = MAX(var_sc1m_0, /NAN)
  FOR e = 1, n_elements(exp_list) -1 DO BEGIN
    cmd = execute( 'var_plot = var_sc1m_'+strtrim(e,2) )
    minvar = MIN([minvar,var_plot], /NAN)
    maxvar = MAX([maxvar,var_plot], /NAN)
  ENDFOR
  maxvar = maxvar + 0.05*(maxvar-minvar)
  minvar = minvar - 0.05*(maxvar-minvar)
  print, 'MINPLOT= ', minvar, ' MAXPLOT= ', maxvar

  splot, indgen(12)+1.5, var_sc1m_0, xtitle='MONTHS', title=mask_title+' '+var_name+' SC1m - '+zone, charsize=1.5, ystyle=1, yrange=[minvar,maxvar], ytitle=var_name, lct=39, charthick=1.5, XTicklen=1, YTicklen=1, XGridStyle=1, YGridStyle=1, /nodata, xrange=[1., 13.]
  FOR e = 0, n_elements(exp_list)-1 DO BEGIN
    cmd = execute( 'oplot, indgen(12)+1.5, var_sc1m_'+strtrim(e,2)+', thick=thc, color=color_list[e]' )
    cmd = execute( 'oplot, indgen(12)+1.5, mean(var_sc1m_'+strtrim(e,2)+',/nan)+indgen(12)*0., thick=thc, color=color_list[e], line=2' )
    IF exp_list[e] EQ 'OBS' THEN xyouts, 0.05, 0.175-e*0.025, obs_name, /NORMAL, charsize=chs, color=color_list[e], charthick=1 ELSE xyouts, 0.05, 0.175-e*0.025, exp_list[e], /NORMAL, charsize=chs, color=color_list[e], charthick=1
    cmd = execute( 'xyouts, 0.3, 0.175-e*0.025, "MEAN="+string(mean(var_sc1m_'+strtrim(e,2)+',/nan), format="(F7.2)"), /NORMAL, charsize=chs, color=color_list[e], charthick=1' )
    cmd = execute( 'xyouts, 0.45, 0.175-e*0.025, "STD="+string(stddev(var_sc1m_'+strtrim(e,2)+',/nan), format="(F7.2)"), /NORMAL, charsize=chs, color=color_list[e], charthick=1' )
    IF e GT 0 THEN cmd = execute( 'xyouts, 0.60, 0.175-e*0.025, "COR_1D="+string(corr_0'+strtrim(e,2)+'[0], format="(F5.2)"), /NORMAL, charsize=chs, color=color_list[e], charthick=1' )
    IF e GT 0 THEN cmd = execute( 'xyouts, 0.80, 0.175-e*0.025, "COR_2D="+string(corrpat_0'+strtrim(e,2)+'[0], format="(F5.2)"), /NORMAL, charsize=chs, color=color_list[e], charthick=1' )

  IF data_type EQ 'c1m' THEN nbyear_max = 1
  IF data_type EQ '1m' AND nbyear_max GT 1 THEN BEGIN
;maxvar+2-e*0.25
    tmp = maxvar+(maxvar-minvar)/3.
    IF e GE 1 THEN cmd = execute( 'xyouts, indgen(13), tmp-e*(maxvar-minvar)/32., ["TST0'+STRTRIM(e,2)+':",STRTRIM(FIX(sig1D_0'+STRTRIM(e,2)+'),2)], /DATA, charsize=chs, color=0, charthick=1' )
    IF e EQ 2 THEN xyouts, indgen(13), tmp-(e+n_elements(exp_list)-1)*(maxvar-minvar)/32., ['TST12:',STRTRIM(FIX(sig1D_12),2)], /DATA, charsize=chs, color=0, charthick=1
    IF e EQ 3 THEN xyouts, indgen(13), tmp-(e+n_elements(exp_list)-1)*(maxvar-minvar)/32., ['TST13:',STRTRIM(FIX(sig1D_13),2)], /DATA, charsize=chs, color=0, charthick=1
    IF e EQ 3 THEN xyouts, indgen(13), tmp-(e+n_elements(exp_list))*(maxvar-minvar)/32., ['TST23:',STRTRIM(FIX(sig1D_23),2)], /DATA, charsize=chs, color=0, charthick=1
  ENDIF

  ENDFOR

  IF write_ps THEN closeps ELSE STOP


;-------------------------------------------------------------------------------------------------
; PLOT SC1m ERRROR (MODEL-OBS)
;-------------------------------------------------------------------------------------------------

IF n_elements(exp_list) GT 1 THEN BEGIN

  fig_name = var_name+'_sc1m_ERROR_'+zone
  IF write_ps THEN openps, filename=plot_dir+'/'+STRUPCASE(data_type)+'/'+zone+'/'+var_name+'/'+fig_name

  FOR e = 1, n_elements(exp_list)-1 DO cmd = execute( 'err_sc1m_'+STRTRIM(e,2)+' = var_sc1m_'+STRTRIM(e,2)+' - var_sc1m_0' )
  minvar = !NULL & maxvar = !NULL
  FOR e = 1, n_elements(exp_list) -1 DO BEGIN
    cmd = execute( 'var_plot = err_sc1m_'+strtrim(e,2) )
    minvar = MIN([minvar,var_plot], /NAN)
    maxvar = MAX([maxvar,var_plot], /NAN)
  ENDFOR
  maxvar = maxvar + 0.05*(maxvar-minvar)
  minvar = minvar - 0.05*(maxvar-minvar)
  print, 'MINPLOT= ', minvar, ' MAXPLOT= ', maxvar

  splot, indgen(12)+1.5, err_sc1m_1, xtitle='MONTHS', title=mask_title+' '+var_name+' SC1m - '+zone, charsize=1.5, ystyle=1, yrange=[minvar,maxvar], ytitle=var_name, lct=39, charthick=1.5, XTicklen=1, YTicklen=1, XGridStyle=1, YGridStyle=1, /nodata, xrange=[1., 13.]
  FOR e = 1, n_elements(exp_list)-1 DO BEGIN
    cmd = execute( 'oplot, indgen(12)+1.5, err_sc1m_'+strtrim(e,2)+', thick=thc, color=color_list[e]' )
    cmd = execute( 'oplot, indgen(12)+1.5, indgen(12)*0., thick=thc, color=0' )
    IF exp_list[e] EQ 'OBS' THEN xyouts, 0.05, 0.175-e*0.025, obs_name, /NORMAL, charsize=chs, color=color_list[e], charthick=1 ELSE xyouts, 0.05, 0.175-e*0.025, exp_list[e], /NORMAL, charsize=chs, color=color_list[e], charthick=1
    cmd = execute( 'xyouts, 0.3, 0.175-e*0.025, "MEAN="+string(mean(err_sc1m_'+strtrim(e,2)+',/nan), format="(F7.2)"), /NORMAL, charsize=chs, color=color_list[e], charthick=1' )
    cmd = execute( 'xyouts, 0.45, 0.175-e*0.025, "STD="+string(stddev(err_sc1m_'+strtrim(e,2)+',/nan), format="(F7.2)"), /NORMAL, charsize=chs, color=color_list[e], charthick=1' )
  ENDFOR

  IF write_ps THEN closeps ELSE STOP

ENDIF


;-------------------------------------------------------------------------------------------------
; PLOT STDDEV SC1m
;-------------------------------------------------------------------------------------------------

  fig_name = var_name+'_sc1m_STDDEV_'+zone
  IF write_ps THEN openps, filename=plot_dir+'/'+STRUPCASE(data_type)+'/'+zone+'/'+var_name+'/'+fig_name

  minvar = !NULL & maxvar = !NULL
  FOR e = 0, n_elements(exp_list) -1 DO BEGIN
    cmd = execute( 'var_plot = var_sdc1m_'+strtrim(e,2) )
    minvar = MIN([minvar,var_plot], /NAN)
    maxvar = MAX([maxvar,var_plot], /NAN)
  ENDFOR
  maxvar = maxvar + 0.05*(maxvar-minvar)
  minvar = minvar - 0.05*(maxvar-minvar)
  print, 'MINPLOT= ', minvar, ' MAXPLOT= ', maxvar

  splot, indgen(12)+1.5, var_sdc1m_0, xtitle='MONTHS', title=mask_title+' '+var_name+' STDDEV 1m - '+zone, charsize=1.5, ystyle=1, yrange=[minvar,maxvar], ytitle=var_name, lct=39, charthick=1.5, XTicklen=1, YTicklen=1, XGridStyle=1, YGridStyle=1, /nodata, xrange=[1., 13.]
  FOR e = 0, n_elements(exp_list)-1 DO BEGIN
    cmd = execute( 'oplot, indgen(12)+1.5, var_sdc1m_'+strtrim(e,2)+', thick=thc, color=color_list[e]' )
    cmd = execute( 'oplot, indgen(12)+1.5, indgen(12)*0., thick=thc, color=0' )
    IF exp_list[e] EQ 'OBS' THEN xyouts, 0.05, 0.175-e*0.025, obs_name, /NORMAL, charsize=chs, color=color_list[e], charthick=1 ELSE xyouts, 0.05, 0.175-e*0.025, exp_list[e], /NORMAL, charsize=chs, color=color_list[e], charthick=1
    cmd = execute( 'xyouts, 0.3, 0.175-e*0.025, "MEAN="+string(mean(var_sdc1m_'+strtrim(e,2)+',/nan), format="(F7.2)"), /NORMAL, charsize=chs, color=color_list[e], charthick=1' )
    cmd = execute( 'xyouts, 0.45, 0.175-e*0.025, "STD="+string(stddev(var_sdc1m_'+strtrim(e,2)+',/nan), format="(F7.2)"), /NORMAL, charsize=chs, color=color_list[e], charthick=1' )
  ENDFOR

  IF write_ps THEN closeps ELSE STOP


;-------------------------------------------------------------------------------------------------
; PLOT STDDEV ANO SC1m
;-------------------------------------------------------------------------------------------------

  fig_name = var_name+'_STDDEV_ANOSC1M_'+zone
  IF write_ps THEN openps, filename=plot_dir+'/'+STRUPCASE(data_type)+'/'+zone+'/'+var_name+'/'+fig_name

  minvar = !NULL & maxvar = !NULL
  FOR e = 0, n_elements(exp_list) -1 DO BEGIN
    cmd = execute( 'var_plot = std_ano_sc1m_'+strtrim(e,2) )
    minvar = MIN([minvar,var_plot], /NAN)
    maxvar = MAX([maxvar,var_plot], /NAN)
  ENDFOR
  maxvar = maxvar + 0.05*(maxvar-minvar)
  minvar = minvar - 0.05*(maxvar-minvar)
  print, 'MINPLOT= ', minvar, ' MAXPLOT= ', maxvar

  splot, indgen(12)+1.5, std_ano_sc1m_0, xtitle='MONTHS', title=mask_title+' '+var_name+' STDDEV ANO 1m - '+zone, charsize=1.5, ystyle=1, yrange=[minvar,maxvar], ytitle=var_name, lct=39, charthick=1.5, XTicklen=1, YTicklen=1, XGridStyle=1, YGridStyle=1, /nodata, xrange=[1., 13.]
  FOR e = 0, n_elements(exp_list)-1 DO BEGIN
    cmd = execute( 'oplot, indgen(12)+1.5, std_ano_sc1m_'+strtrim(e,2)+', thick=thc, color=color_list[e]' )
    cmd = execute( 'oplot, indgen(12)+1.5, indgen(12)*0., thick=thc, color=0' )
    IF exp_list[e] EQ 'OBS' THEN xyouts, 0.05, 0.175-e*0.025, obs_name, /NORMAL, charsize=chs, color=color_list[e], charthick=1 ELSE xyouts, 0.05, 0.175-e*0.025, exp_list[e], /NORMAL, charsize=chs, color=color_list[e], charthick=1
    cmd = execute( 'xyouts, 0.3, 0.175-e*0.025, "MEAN="+string(mean(std_ano_sc1m_'+strtrim(e,2)+',/nan), format="(F7.2)"), /NORMAL, charsize=chs, color=color_list[e], charthick=1' )
    cmd = execute( 'xyouts, 0.45, 0.175-e*0.025, "STD="+string(stddev(std_ano_sc1m_'+strtrim(e,2)+',/nan), format="(F7.2)"), /NORMAL, charsize=chs, color=color_list[e], charthick=1' )
  ENDFOR

  IF write_ps THEN closeps ELSE STOP
