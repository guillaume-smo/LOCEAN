print, '' & print, 'PLOT ALL...'

lct,60
color_factor=70
key_portrait=1
;write_ps = 0
IF write_ps THEN thc = 6 ELSE thc = 2


; PLOT 1D VENT MAX
FOR i = 0, nb_exp-1 DO BEGIN
  cmd = execute('var = max_w10m_'+strtrim(i,2)+'[*]')
  IF i EQ 0 THEN maxplot=max(var,/nan) ELSE maxplot=max([maxplot,var],/nan)
  IF i EQ 0 THEN minplot=min(var,/nan) ELSE minplot=min([minplot,var],/nan)
ENDFOR
maxplot = maxplot + 0.05*(maxplot-minplot)
minplot = minplot - 0.05*(maxplot-minplot)
help, minplot, maxplot

IF write_ps THEN openps, filename=plt_path+'ALL_MAXW10M'
FOR i = 0, nb_exp-1 DO BEGIN
  cmd = execute(' var = max_w10m_'+strtrim(i,2))
  cmd = execute('juld = juld_'+strtrim(i,2))
  jpt = n_elements(juld) & time = juld + 0.50d
  IF i EQ 0 THEN pltt, var, 't', minplot, maxplot, xminor=4, title='TC MAX 10M-WIND', subtitle='', ytitle='10M-WIND (m/s)', thick=thc, charsize=1.5, charthick=2
;  IF exp_list[i] EQ 'BEST-TRACK' OR exp_list[i] EQ 'ALADIN-ANA' THEN thc=2 ELSE thc=2
  pltt, var, 't', color=color_factor*i MOD 256, thick=thc, /ov1D
  oplot, [0,time[0]], [0,var[0]], psym=1, thick=thc, symsize=2, color=color_factor*i MOD 256
  IF i LE 8 THEN BEGIN
    xyouts, 0.125, 0.180-0.020*i, exp_list[i], /normal, charsize=1, charthick=2, color=color_factor*i MOD 256
    xyouts, 0.380, 0.180-0.020*i, '('+alt_list[i]+')', /normal, charsize=1, charthick=2, color=color_factor*i MOD 256
  ENDIF ELSE BEGIN
    xyouts, 0.525, 0.180-0.020*(i-8), exp_list[i], /normal, charsize=1, charthick=2, color=color_factor*i MOD 256
    xyouts, 0.780, 0.180-0.020*(i-8), '('+alt_list[i]+')', /normal, charsize=1, charthick=2, color=color_factor*i MOD 256
  ENDELSE
ENDFOR
IF write_ps THEN closeps ELSE saveimage, plt_path+'ALL_MAXW10M.gif', quality=100

FOR i = 0, nb_par-1 DO BEGIN
  IF write_ps THEN openps, filename=plt_path+'ALL_MAXW10M_'+par_list[i]
  var  = max_w10m_0 & juld = juld_0
  jpt = n_elements(juld) & time = juld + 0.50d
  pltt, var, 't', minplot, maxplot, xminor=4, title='TC MAX 10M-WIND', subtitle='', ytitle='10M-WIND (m/s)', thick=thc, charsize=1.5, charthick=2
  xyouts, 0.125, 0.180-0.020*0, exp_list[0], /normal, charsize=1, charthick=2, color=color_factor*0 MOD 256
  xyouts, 0.380, 0.180-0.020*0, '('+alt_list[0]+')', /normal, charsize=1, charthick=2, color=color_factor*0 MOD 256
  FOR j = 0, nb_date-1 DO BEGIN
    k = i * nb_date + j + 1
    cmd = execute(' var = max_w10m_'+strtrim(k,2))
    cmd = execute('juld = juld_'+strtrim(k,2))
    jpt = n_elements(juld) & time = juld + 0.50d
    pltt, var, 't', color=color_factor*(j+1) MOD 256, thick=thc, /ov1D
    oplot, [0,time[0]], [0,var[0]], psym=1, thick=thc, symsize=2, color=color_factor*(j+1) MOD 256
    IF j LE 8 THEN BEGIN
      xyouts, 0.125, 0.180-0.020*(j+1), exp_list[k], /normal, charsize=1, charthick=2, color=color_factor*(j+1) MOD 256
      xyouts, 0.380, 0.180-0.020*(j+1), '('+alt_list[k]+')', /normal, charsize=1, charthick=2, color=color_factor*(j+1) MOD 256
    ENDIF ELSE BEGIN
      xyouts, 0.525, 0.180-0.020*(j+1-8), exp_list[k], /normal, charsize=1, charthick=2, color=color_factor*(j+1) MOD 256
      xyouts, 0.780, 0.180-0.020*(j+1-8), '('+alt_list[k]+')', /normal, charsize=1, charthick=2, color=color_factor*(j+1) MOD 256
    ENDELSE
  ENDFOR
  IF write_ps THEN closeps ELSE saveimage, plt_path+'ALL_MAXW10M_'+par_list[i]+'.gif', quality=100
ENDFOR



; PLOT 1D VENT MAX+ MOY AZIMTUH
FOR i = 0, nb_exp-1 DO BEGIN
  cmd = execute('var = max_w10m_radtc_'+strtrim(i,2)+'[*]')
  IF i EQ 0 THEN maxplot=max(var,/nan) ELSE maxplot=max([maxplot,var],/nan)
  IF i EQ 0 THEN minplot=min(var,/nan) ELSE minplot=min([minplot,var],/nan)
ENDFOR
maxplot = maxplot + 0.05*(maxplot-minplot)
minplot = minplot - 0.05*(maxplot-minplot)
help, minplot, maxplot

IF write_ps THEN openps, filename=plt_path+'ALL_MAXW10M_RADTC'
FOR i = 0, nb_exp-1 DO BEGIN
  cmd = execute(' var = max_w10m_radtc_'+strtrim(i,2)+'[*]')
  cmd = execute('juld = juld_'+strtrim(i,2))
  jpt = n_elements(juld) & time = juld + 0.50d
  IF i EQ 0 THEN pltt, var, 't', minplot, maxplot, xminor=4, title='TC MAX 10M-WIND (AZIMUTHAL AVERAGE)', subtitle='', ytitle='10M-WIND (m/s)', thick=thc, charsize=1.5, charthick=2
;  IF exp_list[i] EQ 'BEST-TRACK' OR exp_list[i] EQ 'ALADIN-ANA' THEN thc=2 ELSE thc=2
  pltt, var, 't', color=color_factor*i MOD 256, thick=thc, /ov1D
  oplot, [0,time[0]], [0,var[0]], psym=1, thick=thc, symsize=2, color=color_factor*i MOD 256
  IF i LE 8 THEN BEGIN
    xyouts, 0.125, 0.180-0.020*i, exp_list[i], /normal, charsize=1, charthick=2, color=color_factor*i MOD 256
    xyouts, 0.380, 0.180-0.020*i, '('+alt_list[i]+')', /normal, charsize=1, charthick=2, color=color_factor*i MOD 256
  ENDIF ELSE BEGIN
    xyouts, 0.525, 0.180-0.020*(i-8), exp_list[i], /normal, charsize=1, charthick=2, color=color_factor*i MOD 256
    xyouts, 0.780, 0.180-0.020*(i-8), '('+alt_list[i]+')', /normal, charsize=1, charthick=2, color=color_factor*i MOD 256
  ENDELSE
ENDFOR
IF write_ps THEN closeps ELSE saveimage, plt_path+'ALL_MAXW10M_RADTC.gif', quality=100

FOR i = 0, nb_par-1 DO BEGIN
  IF write_ps THEN openps, filename=plt_path+'ALL_MAXW10M_RADTC_'+par_list[i]
  var  = max_w10m_radtc_0 & juld = juld_0
  jpt = n_elements(juld) & time = juld + 0.50d
  pltt, var, 't', minplot, maxplot, xminor=4, title='TC MAX 10M-WIND (AZIMUTHAL AVERAGE)', subtitle='', ytitle='10M-WIND (m/s)', thick=thc, charsize=1.5, charthick=2
  xyouts, 0.125, 0.180-0.020*0, exp_list[0], /normal, charsize=1, charthick=2, color=color_factor*0 MOD 256
  xyouts, 0.380, 0.180-0.020*0, '('+alt_list[0]+')', /normal, charsize=1, charthick=2, color=color_factor*0 MOD 256
  FOR j = 0, nb_date-1 DO BEGIN
    k = i * nb_date + j + 1
    cmd = execute(' var = max_w10m_radtc_'+strtrim(k,2))
    cmd = execute('juld = juld_'+strtrim(k,2))
    jpt = n_elements(juld) & time = juld + 0.50d
    pltt, var, 't', color=color_factor*(j+1) MOD 256, thick=thc, /ov1D
    oplot, [0,time[0]], [0,var[0]], psym=1, thick=thc, symsize=2, color=color_factor*(j+1) MOD 256
    IF j LE 8 THEN BEGIN
      xyouts, 0.125, 0.180-0.020*(j+1), exp_list[k], /normal, charsize=1, charthick=2, color=color_factor*(j+1) MOD 256
      xyouts, 0.380, 0.180-0.020*(j+1), '('+alt_list[k]+')', /normal, charsize=1, charthick=2, color=color_factor*(j+1) MOD 256
    ENDIF ELSE BEGIN
      xyouts, 0.525, 0.180-0.020*(j+1-8), exp_list[k], /normal, charsize=1, charthick=2, color=color_factor*(j+1) MOD 256
      xyouts, 0.780, 0.180-0.020*(j+1-8), '('+alt_list[k]+')', /normal, charsize=1, charthick=2, color=color_factor*(j+1) MOD 256
    ENDELSE
  ENDFOR
  IF write_ps THEN closeps ELSE saveimage, plt_path+'ALL_MAXW10M_RADTC_'+par_list[i]+'.gif', quality=100
ENDFOR



; PLOT 1D MSLP MIN
FOR i = 0, nb_exp-1 DO BEGIN
  cmd = execute('var = min_mslp_'+strtrim(i,2)+'[*]')
  IF i EQ 0 THEN maxplot=max(var,/nan) ELSE maxplot=max([maxplot,var],/nan)
  IF i EQ 0 THEN minplot=min(var,/nan) ELSE minplot=min([minplot,var],/nan)
ENDFOR
maxplot = maxplot + 0.05*(maxplot-minplot)
minplot = minplot - 0.05*(maxplot-minplot)
help, minplot, maxplot

IF write_ps THEN openps, filename=plt_path+'ALL_MINMSLP'
FOR i = 0, nb_exp-1 DO BEGIN
  cmd = execute('var = min_mslp_'+strtrim(i,2)+'[*]')
  cmd = execute('juld = juld_'+strtrim(i,2))
  jpt = n_elements(juld) & time = juld + 0.50d
  IF i EQ 0 THEN pltt, var, 't', minplot, maxplot, xminor=4, title='TC MIN MSLP', subtitle='', ytitle='MSLP (hPa)', thick=thc, charsize=1.5, charthick=2
;  IF exp_list[i] EQ 'BEST-TRACK' OR exp_list[i] EQ 'ALADIN-ANA' THEN thc=2 ELSE thc=2  
  pltt, var, 't', color=color_factor*i MOD 256, thick=thc, /ov1D
  oplot, [0,time[0]], [0,var[0]], psym=1, thick=thc, symsize=2, color=color_factor*i MOD 256
  IF i LE 8 THEN BEGIN
    xyouts, 0.125, 0.180-0.020*i, exp_list[i], /normal, charsize=1, charthick=2, color=color_factor*i MOD 256
    xyouts, 0.380, 0.180-0.020*i, '('+alt_list[i]+')', /normal, charsize=1, charthick=2, color=color_factor*i MOD 256
  ENDIF ELSE BEGIN
    xyouts, 0.525, 0.180-0.020*(i-8), exp_list[i], /normal, charsize=1, charthick=2, color=color_factor*i MOD 256
    xyouts, 0.780, 0.180-0.020*(i-8), '('+alt_list[i]+')', /normal, charsize=1, charthick=2, color=color_factor*i MOD 256
  ENDELSE
ENDFOR
IF write_ps THEN closeps ELSE saveimage, plt_path+'ALL_MINMSLP.gif', quality=100

FOR i = 0, nb_par-1 DO BEGIN
  IF write_ps THEN openps, filename=plt_path+'ALL_MINMSLP_'+par_list[i]
  var  = min_mslp_0 & juld = juld_0
  jpt = n_elements(juld) & time = juld + 0.50d
  pltt, var, 't', minplot, maxplot, xminor=4, title='TC MIN MSLP', subtitle='', ytitle='MSLP (hPa)', thick=thc, charsize=1.5, charthick=2
  xyouts, 0.125, 0.180-0.020*0, exp_list[0], /normal, charsize=1, charthick=2, color=color_factor*0 MOD 256
  xyouts, 0.380, 0.180-0.020*0, '('+alt_list[0]+')', /normal, charsize=1, charthick=2, color=color_factor*0 MOD 256
  FOR j = 0, nb_date-1 DO BEGIN
    k = i * nb_date + j + 1
    cmd = execute(' var = min_mslp_'+strtrim(k,2))
    cmd = execute('juld = juld_'+strtrim(k,2))
    jpt = n_elements(juld) & time = juld + 0.50d
    pltt, var, 't', color=color_factor*(j+1) MOD 256, thick=thc, /ov1D
    oplot, [0,time[0]], [0,var[0]], psym=1, thick=thc, symsize=2, color=color_factor*(j+1) MOD 256
    IF j LE 8 THEN BEGIN
      xyouts, 0.125, 0.180-0.020*(j+1), exp_list[k], /normal, charsize=1, charthick=2, color=color_factor*(j+1) MOD 256
      xyouts, 0.380, 0.180-0.020*(j+1), '('+alt_list[k]+')', /normal, charsize=1, charthick=2, color=color_factor*(j+1) MOD 256
    ENDIF ELSE BEGIN
      xyouts, 0.525, 0.180-0.020*(j+1-8), exp_list[k], /normal, charsize=1, charthick=2, color=color_factor*(j+1) MOD 256
      xyouts, 0.780, 0.180-0.020*(j+1-8), '('+alt_list[k]+')', /normal, charsize=1, charthick=2, color=color_factor*(j+1) MOD 256
    ENDELSE
  ENDFOR
  IF write_ps THEN closeps ELSE saveimage, plt_path+'ALL_MINMSLP_'+par_list[i]+'.gif', quality=100
ENDFOR



; PLOT 1D RVM
FOR i = 0, nb_exp-1 DO BEGIN
;  cmd = execute('var = rvm_'+strtrim(i,2)+'[*]')
  cmd = execute('var = RVM_1DTC_'+strtrim(i,2)+'[*]')
  IF i EQ 0 THEN maxplot=max(var,/nan) ELSE maxplot=max([maxplot,var],/nan)
  IF i EQ 0 THEN minplot=min(var,/nan) ELSE minplot=min([minplot,var],/nan)
ENDFOR
maxplot = maxplot + 0.05*(maxplot-minplot)
minplot = minplot - 0.05*(maxplot-minplot)
help, minplot, maxplot

IF write_ps THEN openps, filename=plt_path+'ALL_RVM_RADTC'
FOR i = 0, nb_exp-1 DO BEGIN
  cmd = execute('var  = RVM_1DTC_'+strtrim(i,2)+'[*]')
  cmd = execute('juld = juld_'+strtrim(i,2))
  jpt = n_elements(juld) & time = juld + 0.50d
  IF i EQ 0 THEN pltt, var, 't', minplot, maxplot, xminor=4, title='TC RMW', subtitle='', ytitle='RADIUS (km)', thick=thc, charsize=1.5, charthick=2
;  IF exp_list[i] EQ 'BEST-TRACK' OR exp_list[i] EQ 'ALADIN-ANA' THEN thc=2 ELSE thc=2
  pltt, var, 't', color=color_factor*i MOD 256, thick=thc, /ov1D
  oplot, [0,time[0]], [0,var[0]], psym=1, thick=thc, symsize=2, color=color_factor*i MOD 256
  IF i LE 8 THEN BEGIN
    xyouts, 0.125, 0.180-0.020*i, exp_list[i], /normal, charsize=1, charthick=2, color=color_factor*i MOD 256
    xyouts, 0.380, 0.180-0.020*i, '('+alt_list[i]+')', /normal, charsize=1, charthick=2, color=color_factor*i MOD 256
  ENDIF ELSE BEGIN
    xyouts, 0.525, 0.180-0.020*(i-8), exp_list[i], /normal, charsize=1, charthick=2, color=color_factor*i MOD 256
    xyouts, 0.780, 0.180-0.020*(i-8), '('+alt_list[i]+')', /normal, charsize=1, charthick=2, color=color_factor*i MOD 256
  ENDELSE
ENDFOR
IF write_ps THEN closeps ELSE saveimage, plt_path+'ALL_RVM_RADTC.gif', quality=100

FOR i = 0, nb_par-1 DO BEGIN
  IF write_ps THEN openps, filename=plt_path+'ALL_RVM_RADTC_'+par_list[i]
  var  = RVM_1DTC_0 & juld = juld_0
  jpt = n_elements(juld) & time = juld + 0.50d
  pltt, var, 't', minplot, maxplot, xminor=4, title='TC RMW', subtitle='', ytitle='RADIUS (km)', thick=thc, charsize=1.5, charthick=2
  xyouts, 0.125, 0.180-0.020*0, exp_list[0], /normal, charsize=1, charthick=2, color=color_factor*0 MOD 256
  xyouts, 0.380, 0.180-0.020*0, '('+alt_list[0]+')', /normal, charsize=1, charthick=2, color=color_factor*0 MOD 256
  FOR j = 0, nb_date-1 DO BEGIN
    k = i * nb_date + j + 1
    cmd = execute(' var = RVM_1DTC_'+strtrim(k,2))
    cmd = execute('juld = juld_'+strtrim(k,2))
    jpt = n_elements(juld) & time = juld + 0.50d
    pltt, var, 't', color=color_factor*(j+1) MOD 256, thick=thc, /ov1D
    oplot, [0,time[0]], [0,var[0]], psym=1, thick=thc, symsize=2, color=color_factor*(j+1) MOD 256
    IF j LE 8 THEN BEGIN
      xyouts, 0.125, 0.180-0.020*(j+1), exp_list[k], /normal, charsize=1, charthick=2, color=color_factor*(j+1) MOD 256
      xyouts, 0.380, 0.180-0.020*(j+1), '('+alt_list[k]+')', /normal, charsize=1, charthick=2, color=color_factor*(j+1) MOD 256
    ENDIF ELSE BEGIN
      xyouts, 0.525, 0.180-0.020*(j+1-8), exp_list[k], /normal, charsize=1, charthick=2, color=color_factor*(j+1) MOD 256
      xyouts, 0.780, 0.180-0.020*(j+1-8), '('+alt_list[k]+')', /normal, charsize=1, charthick=2, color=color_factor*(j+1) MOD 256
    ENDELSE
  ENDFOR
  IF write_ps THEN closeps ELSE saveimage, plt_path+'ALL_RVM_RADTC_'+par_list[i]+'.gif', quality=100
ENDFOR



; PLOT 1D SST
maxplot = !Values.F_NAN & minplot = !Values.F_NAN & var = !Values.F_NAN
FOR i = 0, nb_exp-1 DO BEGIN
  IF i EQ 0 AND sst_list[0] NE '' THEN BEGIN
    FOR j = 0, n_elements(sst_list)-1 DO BEGIN
      cmd = execute('maxplot=max([maxplot,SST'+strtrim(j,2)+'_1DTC_0-273.15],/nan)')
      cmd = execute('minplot=min([minplot,SST'+strtrim(j,2)+'_1DTC_0-273.15],/nan)')
    ENDFOR
  ENDIF ELSE BEGIN
    cmd = execute('var = SST_1DTC_'+strtrim(i,2)+'[*]-273.15')
    maxplot=max([maxplot,var],/nan)
    minplot=min([minplot,var],/nan)
  ENDELSE
ENDFOR
maxplot = maxplot + 0.05*(maxplot-minplot)
minplot = minplot - 0.05*(maxplot-minplot)
help, minplot, maxplot

IF write_ps THEN openps, filename=plt_path+'ALL_SST_1DTC'
IF sst_list[0] NE '' THEN time = juld_0 + 0.50d ELSE time = juld_1 + 0.50d
IF sst_list[0] NE '' THEN var = SST0_1DTC_0-273.15 ELSE var = SST_1DTC_1-273.15
jpt = n_elements(time)
pltt, var, 't', minplot, maxplot, xminor=4, title='TC SST', subtitle='', ytitle='SST (degC)', thick=thc, charsize=1.5, charthick=2

IF sst_list[0] NE '' THEN BEGIN & l = 0
  FOR i = 0, n_elements(sst_list)-1 DO BEGIN
    cmd = execute('pltt, SST'+strtrim(i,2)+'_1DTC_0-273.15, "t", color=color_factor*i MOD 256, thick=thc, /ov1D')
    cmd = execute('oplot, [0,time[0]], [0,SST'+strtrim(i,2)+'_1DTC_0[0]-273.15], psym=1, thick=thc, symsize=2, color=color_factor*i MOD 256')
    xyouts, 0.125, 0.180-0.020*i, sst_list[i], /normal, charsize=1, charthick=2, color=color_factor*i
    l = l+1
  ENDFOR
ENDIF

FOR i = 1, nb_exp-1 DO BEGIN
  icol=l+i-1
  cmd = execute('var = SST_1DTC_'+strtrim(i,2)+'[*]-273.15')
  cmd = execute('juld = juld_'+strtrim(i,2))
  jpt = n_elements(juld) & time = juld + 0.50d
  pltt, var, 't', color=color_factor*icol MOD 256, thick=thc, /ov1D
  oplot, [0,time[0]], [0,var[0]], psym=1, thick=thc, symsize=2, color=color_factor*icol MOD 256
  IF i LE 8 THEN BEGIN
    xyouts, 0.125, 0.180-0.020*icol, exp_list[i], /normal, charsize=1, charthick=2, color=color_factor*i MOD 256
    xyouts, 0.380, 0.180-0.020*icol, '('+alt_list[i]+')', /normal, charsize=1, charthick=2, color=color_factor*i MOD 256
  ENDIF ELSE BEGIN
    xyouts, 0.525, 0.180-0.020*(icol-8), exp_list[i], /normal, charsize=1, charthick=2, color=color_factor*i MOD 256
    xyouts, 0.780, 0.180-0.020*(icol-8), '('+alt_list[i]+')', /normal, charsize=1, charthick=2, color=color_factor*i MOD 256
  ENDELSE
ENDFOR
IF write_ps THEN closeps ELSE saveimage, plt_path+'ALL_SST_1DTC.gif', quality=100

FOR i = 0, nb_par-1 DO BEGIN
  IF write_ps THEN openps, filename=plt_path+'ALL_SST_'+par_list[i]
  var  = sst0_1dtc_0-273.15 & juld = juld_0
  jpt = n_elements(juld) & time = juld + 0.50d
  pltt, var, 't', minplot, maxplot, xminor=4, title='TC SST', subtitle='', ytitle='SST (degC)', thick=thc, charsize=1.5, charthick=2
  xyouts, 0.125, 0.180-0.020*0, exp_list[0], /normal, charsize=1, charthick=2, color=color_factor*0 MOD 256
  xyouts, 0.380, 0.180-0.020*0, '('+alt_list[0]+')', /normal, charsize=1, charthick=2, color=color_factor*0 MOD 256
  FOR j = 0, nb_date-1 DO BEGIN
    k = i * nb_date + j + 1
    cmd = execute(' var = sst_1dtc_'+strtrim(k,2)+'-273.15')
    cmd = execute('juld = juld_'+strtrim(k,2))
    jpt = n_elements(juld) & time = juld + 0.50d
    pltt, var, 't', color=color_factor*(j+1) MOD 256, thick=thc, /ov1D
    oplot, [0,time[0]], [0,var[0]], psym=1, thick=thc, symsize=2, color=color_factor*(j+1) MOD 256
    IF j LE 8 THEN BEGIN
      xyouts, 0.125, 0.180-0.020*(j+1), exp_list[k], /normal, charsize=1, charthick=2, color=color_factor*(j+1) MOD 256
      xyouts, 0.380, 0.180-0.020*(j+1), '('+alt_list[k]+')', /normal, charsize=1, charthick=2, color=color_factor*(j+1) MOD 256
    ENDIF ELSE BEGIN
      xyouts, 0.525, 0.180-0.020*(j+1-8), exp_list[k], /normal, charsize=1, charthick=2, color=color_factor*(j+1) MOD 256
      xyouts, 0.780, 0.180-0.020*(j+1-8), '('+alt_list[k]+')', /normal, charsize=1, charthick=2, color=color_factor*(j+1) MOD 256
    ENDELSE
  ENDFOR
  IF write_ps THEN closeps ELSE saveimage, plt_path+'ALL_SST_'+par_list[i]+'.gif', quality=100
ENDFOR

print, 'PLOT ALL OK' & print, ''
