key_portrait = 1
lct, 39
IF terms[0] EQ 'GSW' THEN fig_name = 'SURFACE_HEAT_BUDGET_'+data_type+'_'+zone+'_'+period+'_'+STRTRIM(yearini_mod,2)+'-'+STRTRIM(yearend_mod,2)
IF terms[0] EQ 'SKT' THEN fig_name = 'SURFACE_VAR_'+data_type+'_'+zone+'_'+period+'_'+STRTRIM(yearini_mod,2)+'-'+STRTRIM(yearend_mod,2)
IF write_ps THEN openps, filename=plot_dir+'/'+STRUPCASE(data_type)+'/'+zone+'/'+fig_name
IF write_ps THEN chs = 1 ELSE chs = 1.5
IF write_ps THEN thc = 6 ELSE thc = 2
IF write_gif THEN SET_PLOT, 'Z'

;color_list = [0, 25, 50, 250, 150, 200, 100, 25]
color_list = [0, 50, 250, 150, 200, 100, 25]


ymin = !NULL & ymax = !NULL
FOR i = 0, N_ELEMENTS(exp_list)-1 DO BEGIN
  FOR j = 0, n_elements(terms)-1 DO cmd = execute( 'ymin = MIN([ymin,  '+terms[j]+'_xytmean_'+strtrim(i,2)+'], /NAN)' )
  FOR j = 0, n_elements(terms)-1 DO cmd = execute( 'ymax = MAX([ymax,  '+terms[j]+'_xytmean_'+strtrim(i,2)+'], /NAN)' )
ENDFOR
ymax = ymax + 0.05*(ymax-ymin)
ymin = MIN([ymin - 0.05*(ymax-ymin),0])


FOR i = 0, N_ELEMENTS(terms)-1 DO BEGIN
  data  = !NULL
  names = !NULL
  FOR j = 0, N_ELEMENTS(exp_list)-1 DO BEGIN
    cmd = execute( 'data = [ data,'+terms[i]+'_xytmean_'+strtrim(j,2)+']' )
    IF j EQ 0 THEN names = [ names, terms[i]] ELSE names = [ names, ' ']
  ENDFOR
  SBAR_PLOT, data, COLORS=color_list[0:N_ELEMENTS(exp_list)-1], BACKGROUND=255, $
  BARWIDTH=0.75, BARSPACE=0.0001, BAROFFSET=i*(1.35*N_ELEMENTS(exp_list))+0.75, BARNAMES=names, $
  OVER=(i GT 0), BASERANGE=1./N_ELEMENTS(terms), YMIN=ymin, YMAX=ymax, $
  XTITLE='BUDGET TERMS', YTITLE='HEAT FLUX (W/m2)', CHARSIZE=1.5, CHARTHICK=1.5, $
  XTicklen=1, YTicklen=1, XGridStyle=1, YGridStyle=1, TITLE=zone+' - '+period
ENDFOR
FOR j = 0, N_ELEMENTS(exp_list)-1 DO $
;IF exp_list[j] EQ 'OBS' THEN xyouts, 0.12, 0.200-j*0.025, obs_name, /NORMAL, charsize=chs, color=color_list[j], charthick=1 $
xyouts, 0.12, 0.200-j*0.025, exp_list[j], /NORMAL, charsize=chs, color=color_list[j], charthick=1
OPLOT, indgen(N_ELEMENTS(exp_list))*0., thick=thc


IF write_gif THEN saveimage, plot_dir+'/'+STRUPCASE(data_type)+'/'+zone+'/'+fig_name+'.gif'
IF write_ps  THEN closeps
IF write_ps EQ 0 AND write_gif EQ 0 THEN STOP
