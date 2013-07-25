print, '' & print, 'PLOT ENSEMBLE MEAN...'

lct,60
color_factor = 70
key_portrait = 1
;write_ps = 0
IF write_ps THEN thc = 6 ELSE thc = 2



; PLOT 1D VENT MAX
FOR i = 0, nb_par-1 DO BEGIN
  cmd = execute('var = ave_max_w10m_aro'+strtrim(i,2)+'[*]')
  cmd = execute('std = std_max_w10m_aro'+strtrim(i,2)+'[*]')  
  IF i EQ 0 THEN maxplot=max([max_w10m_0,var+std],/nan) ELSE maxplot=max([maxplot,var+std],/nan)
  IF i EQ 0 THEN minplot=min([max_w10m_0,var-std],/nan) ELSE minplot=min([minplot,var-std],/nan)
ENDFOR
maxplot = maxplot + 0.05*(maxplot-minplot)
minplot = minplot - 0.05*(maxplot-minplot)

IF write_ps THEN openps, filename=plt_path+'MEAN_MAXW10M'
jpt = n_elements(juld_0) & time = juld_0 + 0.50d
pltt, max_w10m_0, 't', minplot, maxplot, xminor=4, title='ENSEMBLE MEAN: MAX 10M-WIND', subtitle='', $
ytitle='10M-WIND (m/s)', thick=thc, charsize=1.5, charthick=2
xyouts, 0.125, 0.180-0.020*0, exp_list[0], /normal, charsize=1.5, charthick=2, color=color_factor*0 MOD 256
xyouts, 0.375, 0.180-0.020*0, '('+alt_list[0]+')', /normal, charsize=1.5, charthick=2, color=color_factor*0

FOR i = 0, nb_par-1 DO BEGIN
  cmd = execute('var = ave_max_w10m_aro'+strtrim(i,2)+'[*]')
  cmd = execute('std = std_max_w10m_aro'+strtrim(i,2)+'[*]')
  pltt, var, 't', color=color_factor*(i+1) MOD 256, thick=thc, /ov1D
;  oplot, [0,time[0]], [0,time[0]], psym=1, thick=2, symsize=2, color=color_factor*(i+1) MOD 256
  xyouts, 0.125, 0.180-0.020*(i+1), par_list[i], /normal, charsize=1.5, charthick=2, color=color_factor*(i+1) MOD 256
  errplot, time, var-std, var+std, color=color_factor*(i+1) MOD 256
ENDFOR
IF write_ps THEN closeps ELSE saveimage, plt_path+'MEAN_MAXW10M.gif', quality=100



; PLOT 1D VENT MAX + MOY AZIMTUH
maxdate=max(date_0,/nan)
mindate=min(date_0,/nan)
FOR i = 0, nb_par-1 DO BEGIN
  cmd = execute('var = ave_max_w10m_radtc_aro'+strtrim(i,2)+'[*]')
  cmd = execute('std = std_max_w10m_radtc_aro'+strtrim(i,2)+'[*]')  
  IF i EQ 0 THEN maxplot=max([max_w10m_radtc_0,var+std],/nan) ELSE maxplot=max([maxplot,var+std],/nan)
  IF i EQ 0 THEN minplot=min([max_w10m_radtc_0,var-std],/nan) ELSE minplot=min([minplot,var-std],/nan)
ENDFOR
maxplot = maxplot + 0.05*(maxplot-minplot)
minplot = minplot - 0.05*(maxplot-minplot)

IF write_ps THEN openps, filename=plt_path+'MEAN_MAXW10M_RADTC'
jpt = n_elements(juld_0) & time = juld_0 + 0.50d
pltt, max_w10m_radtc_0, 't', minplot, maxplot, xminor=4, title='ENSEMBLE MEAN: MAX 10M-WIND', subtitle='', $
ytitle='10M-WIND (m/s)', thick=thc, charsize=1.5, charthick=2
xyouts, 0.125, 0.180-0.020*0, exp_list[0], /normal, charsize=1.5, charthick=2, color=color_factor*0 MOD 256
xyouts, 0.375, 0.180-0.020*0, '('+alt_list[0]+')', /normal, charsize=1.5, charthick=2, color=color_factor*0

FOR i = 0, nb_par-1 DO BEGIN
  cmd = execute('var = ave_max_w10m_radtc_aro'+strtrim(i,2)+'[*]')
  cmd = execute('std = std_max_w10m_radtc_aro'+strtrim(i,2)+'[*]')
  pltt, var, 't', color=color_factor*(i+1) MOD 256, thick=thc, /ov1D
  xyouts, 0.125, 0.180-0.020*(i+1), par_list[i], /normal, charsize=1.5, charthick=2, color=color_factor*(i+1) MOD 256
  errplot, time, var-std, var+std, color=color_factor*(i+1) MOD 256
ENDFOR
IF write_ps THEN closeps ELSE saveimage, plt_path+'MEAN_MAXW10M_RADTC.gif', quality=100



; PLOT 1D MSLP MIN
maxdate=max(date_0,/nan)
mindate=min(date_0,/nan)
FOR i = 0, nb_par-1 DO BEGIN
  cmd = execute('var = ave_min_mslp_aro'+strtrim(i,2)+'[*]')
  cmd = execute('std = std_min_mslp_aro'+strtrim(i,2)+'[*]')  
  IF i EQ 0 THEN maxplot=max([min_mslp_0,var+std],/nan) ELSE maxplot=max([maxplot,var+std],/nan)
  IF i EQ 0 THEN minplot=min([min_mslp_0,var-std],/nan) ELSE minplot=min([minplot,var-std],/nan)
ENDFOR
maxplot = maxplot + 0.05*(maxplot-minplot)
minplot = minplot - 0.05*(maxplot-minplot)

IF write_ps THEN openps, filename=plt_path+'MEAN_MINMSLP'
jpt = n_elements(juld_0) & time = juld_0 + 0.50d
pltt, min_mslp_0, 't', minplot, maxplot, xminor=4, title='ENSEMBLE MEAN: MIN MSLP', subtitle='', $
ytitle='MSLP (hPa)', thick=thc, charsize=1.5, charthick=2
xyouts, 0.125, 0.180-0.020*0, exp_list[0], /normal, charsize=1.5, charthick=2, color=color_factor*0 MOD 256
xyouts, 0.375, 0.180-0.020*0, '('+alt_list[0]+')', /normal, charsize=1.5, charthick=2, color=color_factor*0

FOR i = 0, nb_par-1 DO BEGIN
  cmd = execute('var = ave_min_mslp_aro'+strtrim(i,2)+'[*]')
  cmd = execute('std = std_min_mslp_aro'+strtrim(i,2)+'[*]')
  pltt, var, 't', color=color_factor*(i+1) MOD 256, thick=thc, /ov1D
  xyouts, 0.125, 0.180-0.020*(i+1), par_list[i], /normal, charsize=1.5, charthick=2, color=color_factor*(i+1) MOD 256
  errplot, time, var-std, var+std, color=color_factor*(i+1) MOD 256
ENDFOR
IF write_ps THEN closeps ELSE saveimage, plt_path+'MEAN_MINMSLP.gif', quality=100



; PLOT 1D RVM
maxdate=max(date_0,/nan)
mindate=min(date_0,/nan)
FOR i = 0, nb_par-1 DO BEGIN
  cmd = execute('var = ave_RVM_1DTC_aro'+strtrim(i,2)+'[*]')
  cmd = execute('std = std_RVM_1DTC_aro'+strtrim(i,2)+'[*]')
  IF i EQ 0 THEN maxplot=max([RVM_1DTC_0,var+std],/nan) ELSE maxplot=max([maxplot,var+std],/nan)
  IF i EQ 0 THEN minplot=min([RVM_1DTC_0,var-std],/nan) ELSE minplot=min([minplot,var-std],/nan)
ENDFOR
maxplot = maxplot + 0.05*(maxplot-minplot)
minplot = minplot - 0.05*(maxplot-minplot)

IF write_ps THEN openps, filename=plt_path+'MEAN_RVM'
jpt = n_elements(juld_0) & time = juld_0 + 0.50d
pltt, RVM_1DTC_0, 't', minplot, maxplot, xminor=4, title='ENSEMBLE MEAN: RMW', subtitle='', $
ytitle='RADIUS (km)', thick=thc, charsize=1.5, charthick=2
xyouts, 0.125, 0.180-0.020*0, exp_list[0], /normal, charsize=1.5, charthick=2, color=color_factor*0 MOD 256
xyouts, 0.375, 0.180-0.020*0, '('+alt_list[0]+')', /normal, charsize=1.5, charthick=2, color=color_factor*0

FOR i = 0, nb_par-1 DO BEGIN
  cmd = execute('var = ave_RVM_1DTC_aro'+strtrim(i,2)+'[*]')
  cmd = execute('std = std_RVM_1DTC_aro'+strtrim(i,2)+'[*]')
  pltt, var, 't', color=color_factor*(i+1) MOD 256, thick=thc, /ov1D
  xyouts, 0.125, 0.180-0.020*(i+1), par_list[i], /normal, charsize=1.5, charthick=2, color=color_factor*(i+1) MOD 256
  errplot, time, var-std, var+std, color=color_factor*(i+1) MOD 256
ENDFOR
IF write_ps THEN closeps ELSE saveimage, plt_path+'MEAN_RVM.gif', quality=100



; PLOT SST 
maxplot = max([ssti_1Dtc_0,sst_1Dtc_1,ave_sst_1DTC_alad,ave_sst_1DTC_arom], /nan)-273.15
minplot = min([ssti_1Dtc_0,sst_1Dtc_1,ave_sst_1DTC_alad,ave_sst_1DTC_arom], /nan)-273.15
maxplot = maxplot + 0.05*(maxplot-minplot)
minplot = minplot - 0.05*(maxplot-minplot)

IF write_ps THEN openps, filename=plt_path+'MEAN_SST_1DTC'
jpt = n_elements(juld_0) & time = juld_0 + 0.50d
pltt, ssti_1Dtc_0-273.15, 't', minplot, maxplot, xminor=4, title='ENSEMBLE MEAN: TC SST', subtitle='', $
ytitle='SST (degC)', thick=thc, charsize=1.5, charthick=2
xyouts, 0.125, 0.180-0.020*0, exp_list[0], /normal, charsize=1.5, charthick=2, color=color_factor*0 MOD 256
xyouts, 0.375, 0.180-0.020*0, '('+alt_list[0]+')', /normal, charsize=1.5, charthick=2, color=color_factor*0

FOR i = 0, n_elements(par_list)-1 DO BEGIN
  cmd = execute('var = ave_SST_1DTC_aro'+strtrim(i,2)+'[*]-273.15')
  cmd = execute('std = std_SST_1DTC_aro'+strtrim(i,2)+'[*]')  
  pltt, var, 't', color=color_factor*(i+1) MOD 256, thick=thc, /ov1D
  xyouts, 0.125, 0.180-0.020*(i+1), par_list[i], /normal, charsize=1.5, charthick=2, color=color_factor*(i+1) MOD 256
  errplot, time, var-std, var+std, color=color_factor*(i+1) MOD 256
ENDFOR
IF write_ps THEN closeps ELSE saveimage, plt_path+'MEAN_SST_1DTC.gif', quality=100



; PLOTS DES VARIABLE "SUP_LIST"
FOR j = 0, n_elements(sup_list)-1 DO BEGIN

  var = sup_list[j]
  unt = usp_list[j]
  print, var
  cmd = execute('maxplot = max([ave_'+var+'_1DTC_alad, ave_'+var+'_1DTC_arom], /nan)')
  cmd = execute('minplot = min([ave_'+var+'_1DTC_alad, ave_'+var+'_1DTC_arom], /nan)')
  FOR i = 0, nb_par-1 DO BEGIN
    cmd = execute('varplot = ave_'+var+'_1DTC_aro'+strtrim(i,2)+'[*]')
    maxplot=max([maxplot,varplot],/nan)
    minplot=min([minplot,varplot],/nan)
  ENDFOR
  maxplot = maxplot + 0.05*(maxplot-minplot)
  minplot = minplot - 0.05*(maxplot-minplot)

  IF write_ps THEN openps, filename=plt_path+'MEAN_'+var+'_1DTC'
  jpt = n_elements(juld_0) & time = juld_0 + 0.50d
  cmd = execute('pltt, ave_'+var+'_1DTC_alad, "t", minplot, maxplot, xminor=4, title="ENSEMBLE MEAN: '+var $ 
  +'", subtitle="", ytitle="'+var+' ('+unt+')", thick=thc, charsize=1.5, charthick=2')

  FOR i = 0, nb_par-1 DO BEGIN
    cmd = execute('varplot = ave_'+var+'_1DTC_aro'+strtrim(i,2)+'[*]')
    cmd = execute('stdplot = std_'+var+'_1DTC_aro'+strtrim(i,2)+'[*]')
    pltt, varplot, 't', color=color_factor*(i+1) MOD 256, thick=thc, /ov1D
    xyouts, 0.125, 0.200-0.025*(1+i), par_list[i], /normal, charsize=1.5, charthick=2, color=color_factor*(1+i) MOD 256
    errplot, time, varplot-stdplot, varplot+stdplot, color=color_factor*(i+1) MOD 256
  ENDFOR
  IF write_ps THEN closeps ELSE saveimage, plt_path+'MEAN_'+var+'_1DTC.gif', quality=100

ENDFOR



; PLOT 2D TRAJ
key_portrait = 0
IF write_ps THEN openps, filename=plt_path+'MEAN_TRACK'
indbold = (listmatch(date_0,round(date_0)))[*,0]
plt, glamt, /nodata, /realcont, title='ENSEMBLE MEAN: TRACK', subtitle='', /no_cb, charsize=1.5, charthick=2
oplot, lon_mslp_0, lat_mslp_0, psym=1, color=0, thick=1, symsize=1
oplot, lon_mslp_0[indbold], lat_mslp_0[indbold], psym=1, color=0, thick=thc, symsize=2
oplot, lon_mslp_0,lat_mslp_0, linestyle=0, color=0, thick=thc
xyouts, 0.100, 0.125-0.025*0, exp_list[0], /normal, charsize=1.5, charthick=2, color=color_factor*0 MOD 256
xyouts, 0.350, 0.125-0.025*0, '('+alt_list[0]+')', /normal, charsize=1.5, charthick=2, color=color_factor*0

FOR i = 0, nb_par-1 DO BEGIN
  cmd = execute('oplot, ave_lon_mslp_aro'+strtrim(i,2)+', ave_lat_mslp_aro'+strtrim(i,2)+', psym=1, color=color_factor*(i+1) MOD 256, thick=1, symsize=1')
  cmd = execute('oplot, ave_lon_mslp_aro'+strtrim(i,2)+'[indbold], ave_lat_mslp_aro'+strtrim(i,2)+'[indbold], psym=1, color=color_factor*(i+1) MOD 256, thick=thc, symsize=2')
  cmd = execute('oplot, ave_lon_mslp_aro'+strtrim(i,2)+', ave_lat_mslp_aro'+strtrim(i,2)+', linestyle=0, color=color_factor*(i+1) MOD 256, thick=thc')
  xyouts, 0.100, 0.125-0.025*(1+i), par_list[i], /normal, charsize=1.5, charthick=2, color=color_factor*(i+1) MOD 256
ENDFOR
;xyouts, 0.125, 0.200-0.020*1, 'ALADIN-OPER', /normal, charsize=1.5, charthick=2, color=color_factor*1
;xyouts, 0.125, 0.200-0.020*2, 'AROME-ALL'  , /normal, charsize=1.5, charthick=2, color=color_factor*2
IF write_ps THEN closeps ELSE saveimage, plt_path+'MEAN_TRACK.gif', quality=100

print, 'PLOT ENSEMBLE MEAN OK' & print, ''
