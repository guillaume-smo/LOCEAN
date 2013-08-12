print, '' & print, 'PLOT ENSEMBLE ERROR...'

lct, 60
key_portrait = 1
color_factor = 70
;write_ps = 0
IF write_ps THEN thc = 6 ELSE thc = 2
color_offset = 3



; plots erreurs moyennes par modele
time_alad = indgen(maxnbt_alad)*6; & help, time_alad
time_arom = indgen(maxnbt_arom)*6; & help, time_arom



; ERREURS DISTANCE
maxplot = max([errdist_alad,errdist_arom],/nan)
minplot = min([errdist_alad,errdist_arom],/nan)
FOR i = 0, n_elements(par_list)-1 DO BEGIN
  cmd = execute('var = errdist_aro'+strtrim(i,2)+'[*]')
  cmd = execute('std = std_errdist_aro'+strtrim(i,2)+'[*]')
  maxplot=max([maxplot,var+std],/nan)
  minplot=min([minplot,var-std],/nan)
ENDFOR
maxplot = maxplot + 0.05*(maxplot-minplot)
minplot = minplot - 0.05*(maxplot-minplot)
minplot = 0

IF write_ps THEN openps, filename=plt_path+'MEAN_ERR_DIST_1DTC'
splot, time_alad, errdist_alad, XTICKINTERVAL=12, xminor=2, yrange=[minplot,maxplot], xrange=[0,96], title='ENSEMBLE MEAN ERROR: TRACK', xtitle='FORECAST TIME (hours)', thick=1, win=0, ytitle='DISTANCE (km)', xgridstyle=2, xticklen=1, ygridstyle=2, yticklen=1, charsize=1.5, charthick=2
IF finite(errdist_alad[0]) EQ 1 THEN oplot, time_alad, errdist_alad, color=color_factor*1, thick=thc
IF finite(errdist_alad[0]) EQ 1 THEN xyouts, 0.125, 0.200-0.020*0, 'ALADIN', /normal, charsize=1, charthick=2, color=1*color_factor MOD 256
;IF finite(errdist_arom[0]) EQ 1 THEN oplot, time_arom, errdist_arom, color=color_factor*2, thick=thc
;IF finite(errdist_arom[0]) EQ 1 THEN xyouts, 0.125, 0.200-0.020*1, 'AROME', /normal, charsize=1, charthick=2, color=2*color_factor MOD 256

FOR i = 0, n_elements(par_list)-1 DO BEGIN
  cmd = execute('var = errdist_aro'+strtrim(i,2))
  cmd = execute('std = std_errdist_aro'+strtrim(i,2)+'[*]')    
  min = strtrim(round( min(var,/nan)),2)
  max = strtrim(round( max(var,/nan)),2)
  ave = strtrim(round(mean(abs(var),/nan)),2)
  oplot, time_arom, var, color=(i+color_offset)*color_factor MOD 256, thick=thc
  oplot, time_arom, var*0., thick=thc, linestyle=0, color=0
  errplot, time_arom, var-std, var+std, color=color_factor*(i+color_offset) MOD 256
  xyouts, 0.075, 0.125-0.020*i, par_list[i], /normal, charsize=1.25, charthick=2, color=(i+color_offset)*color_factor MOD 256
  xyouts, 0.525, 0.125-0.020*i, 'MIN/AVE/MAX ERROR: '+min+'/'+ave+'/'+max+' km', /normal, charsize=1.25, charthick=2, color=(i+color_offset)*color_factor MOD 256
  IF nb_par EQ 2 AND nb_date GE 4 THEN xyouts, time_arom, minplot-0.150*(maxplot-minplot), strtrim(tmtest_errdist_arom,2), charsize=1.25, charthick=2 $
  ELSE xyouts, time_arom[0], minplot-0.150*(maxplot-minplot), 'SIGNIFICANCE TEST NOT POSSIBLE', charsize=1.25, charthick=2
  xyouts, time_arom, minplot-0.180*(maxplot-minplot), strtrim(nb_errdist_arom,2), charsize=1.25, charthick=2
  xyouts, time_arom[0]-6, minplot-0.150*(maxplot-minplot), 'SIG: ', charsize=1.25, charthick=2, alignment=1
  xyouts, time_arom[0]-6, minplot-0.180*(maxplot-minplot), 'NB: ', charsize=1.25, charthick=2, alignment=1
ENDFOR
IF write_ps THEN closeps ELSE saveimage, plt_path+''+'MEAN_ERR_DIST_1DTC.gif', quality=100



; ERREURS VENT MAX
maxplot = max([errwind_alad,errwind_arom],/nan)
minplot = min([errwind_alad,errwind_arom],/nan)
FOR i = 0, n_elements(par_list)-1 DO BEGIN
  cmd = execute('var = errwind_aro'+strtrim(i,2)+'[*]')
  cmd = execute('std = std_errwind_aro'+strtrim(i,2)+'[*]')
  maxplot=max([maxplot,var+std],/nan)
  minplot=min([minplot,var-std],/nan)
ENDFOR
maxplot = maxplot + 0.05*(maxplot-minplot)
minplot = minplot - 0.05*(maxplot-minplot)

IF write_ps THEN openps, filename=plt_path+'MEAN_ERR_W10M_1DTC'
splot, time_alad, errwind_alad, XTICKINTERVAL=12, xminor=2, yrange=[minplot,maxplot], xrange=[0,96], title='ENSEMBLE MEAN ERROR: MAX 10M-WIND', xtitle='FORECAST TIME (hours)', thick=1, win=0, ytitle='WIND ERROR (m/s)', xgridstyle=2, xticklen=1, ygridstyle=2, yticklen=1, charsize=1.5, charthick=2
IF finite(errwind_alad[0]) EQ 1 THEN oplot, time_alad, errwind_alad, color=color_factor*1, thick=thc
IF finite(errwind_alad[0]) EQ 1 THEN xyouts, 0.125, 0.200-0.020*0, 'ALADIN', /normal, charsize=1, charthick=2, color=1*color_factor MOD 256
;IF finite(errwind_arom[0]) EQ 1 THEN oplot, time_arom, errwind_arom, color=color_factor*2, thick=thc
;IF finite(errwind_arom[0]) EQ 1 THEN xyouts, 0.125, 0.200-0.020*1, 'AROME', /normal, charsize=1, charthick=2, color=2*color_factor MOD 256

FOR i = 0, n_elements(par_list)-1 DO BEGIN
  cmd = execute('var = errwind_aro'+strtrim(i,2))
  cmd = execute('std = std_errwind_aro'+strtrim(i,2)+'[*]')
  min = strtrim(round( min(var,/nan)),2)
  max = strtrim(round( max(var,/nan)),2)
  ave = strtrim(round(mean(abs(var),/nan)),2)
  oplot, time_arom, var, color=(i+color_offset)*color_factor MOD 256, thick=thc
  oplot, time_arom, var*0., thick=thc, linestyle=0, color=0  
  errplot, time_arom, var-std, var+std, color=color_factor*(i+color_offset) MOD 256
  xyouts, 0.075, 0.125-0.020*i, par_list[i], /normal, charsize=1.25, charthick=2, color=(i+color_offset)*color_factor MOD 256
  xyouts, 0.525, 0.125-0.020*i, 'MIN/AVE/MAX ERROR: '+min+'/'+ave+'/'+max+' m/s', /normal, charsize=1.25, charthick=2, color=(i+color_offset)*color_factor MOD 256
  IF nb_par EQ 2 AND nb_date GE 4 THEN xyouts, time_arom, minplot-0.150*(maxplot-minplot), strtrim(tmtest_errwind_arom,2), charsize=1.25, charthick=2 $
  ELSE xyouts, time_arom[0], minplot-0.150*(maxplot-minplot), 'SIGNIFICANCE TEST NOT POSSIBLE', charsize=1.25, charthick=2
  xyouts, time_arom, minplot-0.175*(maxplot-minplot), strtrim(nb_errwind_arom,2), charsize=1.25, charthick=2
  xyouts, time_arom[0]-6, minplot-0.150*(maxplot-minplot), 'SIG: ', charsize=1.25, charthick=2, alignment=1
  xyouts, time_arom[0]-6, minplot-0.180*(maxplot-minplot), 'NB: ', charsize=1.25, charthick=2, alignment=1
ENDFOR
IF write_ps THEN closeps ELSE saveimage, plt_path+''+'MEAN_ERR_W10M_1DTC.gif', quality=100



; ERREURS VENT MAX (AZIMUTHAL AVERAGE)
maxplot = max([errwrad_alad,errwrad_arom],/nan)
minplot = min([errwrad_alad,errwrad_arom],/nan)
FOR i = 0, n_elements(par_list)-1 DO BEGIN
  cmd = execute('var = errwrad_aro'+strtrim(i,2)+'[*]')
  cmd = execute('std = std_errwrad_aro'+strtrim(i,2)+'[*]')
  maxplot=max([maxplot,var+std],/nan)
  minplot=min([minplot,var-std],/nan)
ENDFOR
maxplot = maxplot + 0.05*(maxplot-minplot)
minplot = minplot - 0.05*(maxplot-minplot)

IF write_ps THEN openps, filename=plt_path+'MEAN_ERR_W10MRAD_1DTC'
splot, time_alad, errwrad_alad, XTICKINTERVAL=12, xminor=2, yrange=[minplot,maxplot], xrange=[0,96], title='ENSEMBLE MEAN ERROR: MAX 10M-WIND ERROR', xtitle='FORECAST TIME (hours)', thick=1, win=0, ytitle='WIND ERROR (m/s)', xgridstyle=2, xticklen=1, charsize=1.5, charthick=2 , ygridstyle=2, yticklen=1
IF finite(errwrad_alad[0]) EQ 1 THEN oplot, time_alad, errwrad_alad, color=color_factor*1, thick=thc
IF finite(errwrad_alad[0]) EQ 1 THEN xyouts, 0.125, 0.200-0.020*0, 'ALADIN', /normal, charsize=1, charthick=2, color=1*color_factor MOD 256
;IF finite(errwrad_arom[0]) EQ 1 THEN oplot, time_arom, errwrad_arom, color=color_factor*2, thick=thc
;IF finite(errwrad_arom[0]) EQ 1 THEN xyouts, 0.125, 0.200-0.020*1, 'AROME', /normal, charsize=1, charthick=2, color=2*color_factor MOD 256

FOR i = 0, n_elements(par_list)-1 DO BEGIN
  cmd = execute('var = errwrad_aro'+strtrim(i,2))
  cmd = execute('std = std_errwrad_aro'+strtrim(i,2)+'[*]')
  min = strtrim(round( min(var,/nan)),2)
  max = strtrim(round( max(var,/nan)),2)
  ave = strtrim(round(mean(abs(var),/nan)),2)
  oplot, time_arom, var, color=(i+color_offset)*color_factor MOD 256, thick=thc
  oplot, time_arom, var*0., thick=thc, linestyle=0, color=0  
  errplot, time_arom, var-std, var+std, color=color_factor*(i+color_offset) MOD 256
  xyouts, 0.075, 0.125-0.020*i, par_list[i], /normal, charsize=1.25, charthick=2, color=(i+color_offset)*color_factor MOD 256
  xyouts, 0.525, 0.125-0.020*i, 'MIN/AVE/MAX ERROR: '+min+'/'+ave+'/'+max+' m/s', /normal, charsize=1.25, charthick=2, color=(i+color_offset)*color_factor MOD 256
  IF nb_par EQ 2 AND nb_date GE 4 THEN xyouts, time_arom, minplot-0.150*(maxplot-minplot), strtrim(tmtest_errwrad_arom,2), charsize=1.25, charthick=2 $
  ELSE xyouts, time_arom[0], minplot-0.150*(maxplot-minplot), 'SIGNIFICANCE TEST NOT POSSIBLE', charsize=1.25, charthick=2
  xyouts, time_arom, minplot-0.175*(maxplot-minplot), strtrim(nb_errwrad_arom,2), charsize=1.25, charthick=2
  xyouts, time_arom[0]-6, minplot-0.150*(maxplot-minplot), 'SIG: ', charsize=1.25, charthick=2, alignment=1
  xyouts, time_arom[0]-6, minplot-0.180*(maxplot-minplot), 'NB: ', charsize=1.25, charthick=2, alignment=1
ENDFOR
IF write_ps THEN closeps ELSE saveimage, plt_path+''+'MEAN_ERR_W10MRAD_1DTC.gif', quality=100



; ERREURS MSLP
maxplot = max([errmslp_alad,errmslp_arom],/nan)
minplot = min([errmslp_alad,errmslp_arom],/nan)
FOR i = 0, n_elements(par_list)-1 DO BEGIN
  cmd = execute('var = errmslp_aro'+strtrim(i,2)+'[*]')
  cmd = execute('std = std_errmslp_aro'+strtrim(i,2)+'[*]')
  maxplot=max([maxplot,var+std],/nan)
  minplot=min([minplot,var-std],/nan)
ENDFOR
maxplot = maxplot + 0.05*(maxplot-minplot)
minplot = minplot - 0.05*(maxplot-minplot)

IF write_ps THEN openps, filename=plt_path+'MEAN_ERR_MSLP_1DTC'
splot, time_alad, errmslp_alad, XTICKINTERVAL=12, xminor=2, yrange=[minplot,maxplot], xrange=[0,96], title='ENSEMBLE MEAN ERROR: MIN MSLP', xtitle='FORECAST TIME (hours)', thick=1, win=0, ytitle='MSLP ERROR (hPa)', xgridstyle=2, xticklen=1, charsize=1.5, charthick=2, ygridstyle=2, yticklen=1
IF finite(errmslp_alad[0]) EQ 1 THEN oplot, time_alad, errmslp_alad, color=color_factor*1, thick=thc
IF finite(errmslp_alad[0]) EQ 1 THEN xyouts, 0.125, 0.200-0.020*0, 'ALADIN', /normal, charsize=1, charthick=2, color=1*color_factor MOD 256
;IF finite(errmslp_arom[0]) EQ 1 THEN oplot, time_arom, errmslp_arom, color=color_factor*2, thick=thc
;IF finite(errmslp_arom[0]) EQ 1 THEN xyouts, 0.125, 0.200-0.020*1, 'AROME', /normal, charsize=1, charthick=2, color=2*color_factor MOD 256

FOR i = 0, n_elements(par_list)-1 DO BEGIN
  cmd = execute('var = errmslp_aro'+strtrim(i,2))
  cmd = execute('std = std_errmslp_aro'+strtrim(i,2)+'[*]')
  min = strtrim(round( min(var,/nan)),2)
  max = strtrim(round( max(var,/nan)),2)
  ave = strtrim(round(mean(abs(var),/nan)),2)
  oplot, time_arom, var, color=(i+color_offset)*color_factor MOD 256, thick=thc
  oplot, time_arom, var*0., thick=thc, linestyle=0, color=0
  errplot, time_arom, var-std, var+std, color=color_factor*(i+color_offset) MOD 256  
  xyouts, 0.075, 0.125-0.020*i, par_list[i], /normal, charsize=1.25, charthick=2, color=(i+color_offset)*color_factor MOD 256
  xyouts, 0.525, 0.125-0.020*i, 'MIN/AVE/MAX ERROR: '+min+'/'+ave+'/'+max+' hPa', /normal, charsize=1.25, charthick=2, color=(i+color_offset)*color_factor MOD 256
  IF nb_par EQ 2 AND nb_date GE 4 THEN xyouts, time_arom, minplot-0.150*(maxplot-minplot), strtrim(tmtest_errmslp_arom,2), charsize=1.25, charthick=2 $
  ELSE xyouts, time_arom[0], minplot-0.150*(maxplot-minplot), 'SIGNIFICANCE TEST NOT POSSIBLE', charsize=1.25, charthick=2
  xyouts, time_arom, minplot-0.175*(maxplot-minplot), strtrim(nb_errmslp_arom,2), charsize=1.25, charthick=2
  xyouts, time_arom[0]-6, minplot-0.150*(maxplot-minplot), 'SIG: ', charsize=1.25, charthick=2, alignment=1
  xyouts, time_arom[0]-6, minplot-0.180*(maxplot-minplot), 'NB: ', charsize=1.25, charthick=2, alignment=1
ENDFOR
IF write_ps THEN closeps ELSE saveimage, plt_path+''+'MEAN_ERR_MSLP_1DTC.gif', quality=100



; ERREURS RMW
maxplot = max([err_rmw_alad,err_rmw_arom],/nan)
minplot = min([err_rmw_alad,err_rmw_arom],/nan)
FOR i = 0, n_elements(par_list)-1 DO BEGIN
  cmd = execute('var = err_rmw_aro'+strtrim(i,2)+'[*]')
  cmd = execute('std = std_err_rmw_aro'+strtrim(i,2)+'[*]')
  maxplot=max([maxplot,var+std],/nan)
  minplot=min([minplot,var-std],/nan)
ENDFOR
maxplot = maxplot + 0.05*(maxplot-minplot)
minplot = minplot - 0.05*(maxplot-minplot)

IF write_ps THEN openps, filename=plt_path+'MEAN_ERR_RMW_1DTC'
splot, time_alad, err_rmw_alad, XTICKINTERVAL=12, xminor=2, yrange=[minplot,maxplot], xrange=[0,96], title='ENSEMBLE MEAN ERROR: RMW', xtitle='FORECAST TIME (hours)', thick=1, win=0, ytitle='RADIUS ERROR (km)', xgridstyle=2, xticklen=1, charsize=1.5, charthick=2, ygridstyle=2, yticklen=1
IF finite(err_rmw_alad[0]) EQ 1 THEN oplot, time_alad, err_rmw_alad, color=color_factor*1, thick=thc
IF finite(err_rmw_alad[0]) EQ 1 THEN xyouts, 0.125, 0.200-0.020*0, 'ALADIN', /normal, charsize=1, charthick=2, color=1*color_factor MOD 256
;IF finite(err_rmw_arom[0]) EQ 1 THEN oplot, time_arom, err_rmw_arom, color=color_factor*2, thick=thc
;IF finite(err_rmw_arom[0]) EQ 1 THEN xyouts, 0.125, 0.200-0.020*1, 'AROME', /normal, charsize=1, charthick=2, color=2*color_factor MOD 256

FOR i = 0, n_elements(par_list)-1 DO BEGIN
  cmd = execute('var = err_rmw_aro'+strtrim(i,2))
  cmd = execute('std = std_err_rmw_aro'+strtrim(i,2)+'[*]')
  min = strtrim(round( min(var,/nan)),2)
  max = strtrim(round( max(var,/nan)),2)
  ave = strtrim(round(mean(abs(var),/nan)),2)
  oplot, time_arom, var, color=(i+color_offset)*color_factor MOD 256, thick=thc
  oplot, time_arom, var*0., thick=thc, linestyle=0, color=0  
  errplot, time_arom, var-std, var+std, color=color_factor*(i+color_offset) MOD 256
  xyouts, 0.075, 0.125-0.020*i, par_list[i], /normal, charsize=1.25, charthick=2, color=(i+color_offset)*color_factor MOD 256
  xyouts, 0.525, 0.125-0.020*i, 'MIN/AVE/MAX ERROR: '+min+'/'+ave+'/'+max+' km', /normal, charsize=1.25, charthick=2, color=(i+color_offset)*color_factor MOD 256
  IF nb_par EQ 2 AND nb_date GE 4 THEN xyouts, time_arom, minplot-0.150*(maxplot-minplot), strtrim(tmtest_err_rmw_arom,2), charsize=1.25, charthick=2 $
  ELSE xyouts, time_arom[0], minplot-0.150*(maxplot-minplot), 'SIGNIFICANCE TEST NOT POSSIBLE', charsize=1.25, charthick=2
  xyouts, time_arom, minplot-0.175*(maxplot-minplot), strtrim(nb_err_rmw_arom,2), charsize=1.25, charthick=2
  xyouts, time_arom[0]-6, minplot-0.150*(maxplot-minplot), 'SIG: ', charsize=1.25, charthick=2, alignment=1
  xyouts, time_arom[0]-6, minplot-0.180*(maxplot-minplot), 'NB: ', charsize=1.25, charthick=2, alignment=1
ENDFOR
oplot, time_arom, err_rmw_arom*0., thick=1, linestyle=2
IF write_ps THEN closeps ELSE saveimage, plt_path+''+'MEAN_ERR_RMW_1DTC.gif', quality=100



; ERREURS SST
maxplot = max([err_sst_alad,err_sst_arom],/nan)
minplot = min([err_sst_alad,err_sst_arom],/nan)
FOR i = 0, n_elements(par_list)-1 DO BEGIN
  cmd = execute('var = err_sst_aro'+strtrim(i,2)+'[*]')
  cmd = execute('std = std_err_sst_aro'+strtrim(i,2)+'[*]')
  maxplot=max([maxplot,var+std],/nan)
  minplot=min([minplot,var-std],/nan)
ENDFOR
maxplot = maxplot + 0.05*(maxplot-minplot)
minplot = minplot - 0.05*(maxplot-minplot)

IF write_ps THEN openps, filename=plt_path+'MEAN_ERR_SST_1DTC'
splot, time_alad, err_sst_alad, XTICKINTERVAL=12, xminor=2, yrange=[minplot,maxplot], xrange=[0,96], title='ENSEMBLE MEAN ERROR: SST', xtitle='FORECAST TIME (hours)', thick=1, win=0, ytitle='SST ERROR (K)', xgridstyle=2, xticklen=1, charsize=1.5, charthick=2, ygridstyle=2, yticklen=1
IF finite(err_sst_alad[0]) EQ 1 THEN oplot, time_alad, err_sst_alad, color=color_factor*1, thick=thc
IF finite(err_sst_alad[0]) EQ 1 THEN xyouts, 0.125, 0.200-0.020*0, 'ALADIN', /normal, charsize=1, charthick=2, color=1*color_factor MOD 256
;IF finite(err_sst_arom[0]) EQ 1 THEN oplot, time_arom, err_sst_arom, color=color_factor*2, thick=thc
;IF finite(err_sst_arom[0]) EQ 1 THEN xyouts, 0.125, 0.200-0.020*1, 'AROME', /normal, charsize=1, charthick=2, color=2*color_factor MOD 256

FOR i = 0, n_elements(par_list)-1 DO BEGIN
  cmd = execute('var = err_sst_aro'+strtrim(i,2))
  cmd = execute('std = std_err_sst_aro'+strtrim(i,2)+'[*]')
  min = string( min(var,/nan), format='(F4.1)')
  max = string( max(var,/nan), format='(F4.1)')
  ave = string(mean(abs(var),/nan), format='(F4.1)')
  oplot, time_arom, var, color=(i+color_offset)*color_factor MOD 256, thick=thc
  oplot, time_arom, var*0., thick=thc, linestyle=0, color=0  
  errplot, time_arom, var-std, var+std, color=color_factor*(i+color_offset) MOD 256
  xyouts, 0.075, 0.125-0.020*i, par_list[i], /normal, charsize=1.25, charthick=2, color=(i+color_offset)*color_factor MOD 256
  xyouts, 0.525, 0.125-0.020*i, 'MIN/AVE/MAX ERROR: '+min+'/'+ave+'/'+max+' K', /normal, charsize=1.25, charthick=2, color=(i+color_offset)*color_factor MOD 256
  IF nb_par EQ 2 AND nb_date GE 4 THEN xyouts, time_arom, minplot-0.150*(maxplot-minplot), strtrim(tmtest_err_sst_arom,2), charsize=1.25, charthick=2 $
  ELSE xyouts, time_arom[0], minplot-0.150*(maxplot-minplot), 'SIGNIFICANCE TEST NOT POSSIBLE', charsize=1.25, charthick=2
  xyouts, time_arom, minplot-0.175*(maxplot-minplot), strtrim(nb_err_sst_arom,2), charsize=1.25, charthick=2
  xyouts, time_arom[0]-6, minplot-0.150*(maxplot-minplot), 'SIG: ', charsize=1.25, charthick=2, alignment=1
  xyouts, time_arom[0]-6, minplot-0.180*(maxplot-minplot), 'NB: ', charsize=1.25, charthick=2, alignment=1
ENDFOR
oplot, time_arom, err_sst_arom*0., thick=1, linestyle=2
IF write_ps THEN closeps ELSE saveimage, plt_path+''+'MEAN_ERR_SST_1DTC.gif', quality=100

print, 'PLOT ENSEMBLE ERROR OK' & print, ''
