; plots "time vs distance error"
FOR i = 1, nb_exp-1 DO BEGIN
  cmd = execute('var  = errdist_'+strtrim(i,2))
  cmd = execute('date = date_err_'+strtrim(i,2))
  IF i EQ 1 THEN maxplot=max(var,/nan) ELSE maxplot=max([maxplot,var],/nan)
  IF i EQ 1 THEN minplot=min(var,/nan) ELSE minplot=min([minplot,var],/nan)
  IF i EQ 1 THEN maxdate=max(date,/nan) ELSE maxdate=max([maxdate,date],/nan)
  IF i EQ 1 THEN mindate=min(date,/nan) ELSE mindate=min([mindate,date],/nan)
  IF i EQ 1 THEN max_nbt = n_elements(var) ELSE max_nbt = max([max_nbt,n_elements(var)],/nan)
ENDFOR
maxplot = maxplot + 0.05*(maxplot-minplot)
minplot = minplot - 0.05*(maxplot-minplot)
help, minplot, maxplot

IF write_ps THEN openps, filename=plt_path+'DATE_ERRDIST_1D'
FOR i = 1, nb_exp-1 DO BEGIN
  cmd = execute('var = errdist_'+strtrim(i,2))
  cmd = execute('date = date_err_'+strtrim(i,2))
  IF i EQ 1 THEN splot, date, var, XTICKFORMAT='(I8)', XTICKINTERVAL=1, xminor=4, yrange=[minplot,maxplot], xrange=[mindate,maxdate], title='DISTANCE ERROR (km)', xtitle='date', thick=1, win=0, xgridstyle=2, xticklen=1.0, ytitle='DISTANCE ERROR (km)', charsize=1.5, charthick=2
  IF exp_list[i] EQ 'ALADIN-ANA' THEN oplot, date, var, color=color_factor*(i-1), thick=2.5 $
  ELSE oplot, date, var, color=color_factor*(i-1), thick=1
  oplot, [0,date[0]], [0,var[0]], psym=1, thick=2, symsize=2, color=color_factor*(i-1)
  xyouts, 0.125, 0.200-0.020*i, exp_list[i], /normal, charsize=1, charthick=2, color=color_factor*(i-1)
  xyouts, 0.300, 0.200-0.020*i, '('+alt_list[i]+')', /normal, charsize=1, charthick=2, color=color_factor*(i-1)
  IF i EQ 1 THEN oplot, date, var*0., thick=1, linestyle=2
ENDFOR
IF write_ps THEN closeps ELSE saveimage, plt_path+''+'DATE_ERRDIST_1D.gif', quality=100

IF write_ps THEN openps, filename=plt_path+'ECH_ERRDIST_1D'
FOR i = 2, nb_exp-1 DO BEGIN
  cmd = execute('var = errdist_'+strtrim(i,2))
  cmd = execute('date = date_err_'+strtrim(i,2))
  lead_time = indgen(n_elements(date))*6
  IF i EQ 2 THEN splot, lead_time, var, XTICKINTERVAL=12, xminor=2, yrange=[minplot,maxplot], xrange=[0,96], title='DISTANCE ERROR (km)', xtitle='LEAD TIME (hours)', thick=1, win=0, xgridstyle=2, xticklen=1.0, ytitle='DISTANCE ERROR (km)'
  oplot, lead_time, var, color=color_factor*(i-1), thick=1
  xyouts, 0.125, 0.200-0.020*i, exp_list[i], /normal, charsize=1, charthick=2, color=color_factor*(i-1)
  xyouts, 0.300, 0.200-0.020*i, '('+alt_list[i]+')', /normal, charsize=1, charthick=2, color=color_factor*(i-1)
  IF i EQ 2 THEN oplot, indgen(96), indgen(96)*0., thick=1, linestyle=2
ENDFOR
IF write_ps THEN closeps ELSE saveimage, plt_path+''+'ECH_ERRDIST_1D.gif', quality=100



; calcul min+max du plot "time vs wind error"
FOR i = 1, nb_exp-1 DO BEGIN
  cmd = execute('var = errwind_'+strtrim(i,2))
  cmd = execute('date = date_err_'+strtrim(i,2))
  IF i EQ 1 THEN maxplot=max(var,/nan) ELSE maxplot=max([maxplot,var],/nan)
  IF i EQ 1 THEN minplot=min(var,/nan) ELSE minplot=min([minplot,var],/nan)
  IF i EQ 1 THEN maxdate=max(date,/nan) ELSE maxdate=max([maxdate,date],/nan)
  IF i EQ 1 THEN mindate=min(date,/nan) ELSE mindate=min([mindate,date],/nan)
ENDFOR
maxplot = maxplot + 0.05*(maxplot-minplot)
minplot = minplot - 0.05*(maxplot-minplot)
help, minplot, maxplot

; plots "time vs wind error"
IF write_ps THEN openps, filename=plt_path+'DATE_ERRW10M_1D'
FOR i = 1, nb_exp-1 DO BEGIN
  cmd = execute('var = errwind_'+strtrim(i,2))
  cmd = execute('date = date_err_'+strtrim(i,2))
  IF i EQ 1 THEN splot, date, var, XTICKFORMAT='(I8)', XTICKINTERVAL=1, xminor=4, yrange=[minplot,maxplot], xrange=[mindate,maxdate], title='MAX W10M ERROR (m/s)', xtitle='date', thick=1, win=0, xgridstyle=2, xticklen=1.0, ytitle='MAX W10M ERROR (m/s)'
  IF exp_list[i] EQ 'ALADIN-ANA' THEN oplot, date, var, color=color_factor*(i-1), thick=2.5 $
  ELSE oplot, date, var, color=color_factor*(i-1), thick=1
  oplot, [0,date[0]], [0,var[0]], psym=1, thick=2, symsize=2, color=color_factor*(i-1)
  xyouts, 0.125, 0.200-0.020*i, exp_list[i], /normal, charsize=1, charthick=2, color=color_factor*(i-1)
  xyouts, 0.300, 0.200-0.020*i, '('+alt_list[i]+')', /normal, charsize=1, charthick=2, color=color_factor*(i-1)
  IF i EQ 1 THEN oplot, date, var*0., thick=1, linestyle=2
ENDFOR
IF write_ps THEN closeps ELSE saveimage, plt_path+''+'DATE_ERRW10M_1D.gif', quality=100

IF write_ps THEN openps, filename=plt_path+'ECH_ERRW10M_1D'
FOR i = 2, nb_exp-1 DO BEGIN
  cmd = execute('var = errwind_'+strtrim(i,2))
  cmd = execute('date = date_err_'+strtrim(i,2))
  lead_time = indgen(n_elements(date))*6
  IF i EQ 2 THEN splot, lead_time, var, XTICKINTERVAL=12, xminor=2, yrange=[minplot,maxplot], xrange=[0,96], title='MAX W10M ERROR (m/s)', xtitle='LEAD TIME (hours)', thick=1, win=0, ytitle='MAX W10M ERROR (m/s)', xgridstyle=2, xticklen=1.0
  oplot, lead_time, var, color=color_factor*(i-1), thick=1
  xyouts, 0.125, 0.200-0.020*i, exp_list[i], /normal, charsize=1, charthick=2, color=color_factor*(i-1)
  xyouts, 0.300, 0.200-0.020*i, '('+alt_list[i]+')', /normal, charsize=1, charthick=2, color=color_factor*(i-1)
  IF i EQ 2 THEN oplot, indgen(96), indgen(96)*0., thick=1, linestyle=2
ENDFOR
IF write_ps THEN closeps ELSE saveimage, plt_path+''+'ECH_ERRW10M_1D.gif', quality=100



; calcul min+max du plot "time vs mslp error"
FOR i = 1, nb_exp-1 DO BEGIN
  cmd = execute('var = errmslp_'+strtrim(i,2))
  cmd = execute('date = date_err_'+strtrim(i,2))
  IF i EQ 1 THEN maxplot=max(var,/nan) ELSE maxplot=max([maxplot,var],/nan)
  IF i EQ 1 THEN minplot=min(var,/nan) ELSE minplot=min([minplot,var],/nan)
  IF i EQ 1 THEN maxdate=max(date,/nan) ELSE maxdate=max([maxdate,date],/nan)
  IF i EQ 1 THEN mindate=min(date,/nan) ELSE mindate=min([mindate,date],/nan)
ENDFOR
maxplot = maxplot + 0.05*(maxplot-minplot)
minplot = minplot - 0.05*(maxplot-minplot)
help, minplot, maxplot

; plots "time vs mslp error"
IF write_ps THEN openps, filename=plt_path+'DATE_ERRMSLP_1D'
FOR i = 1, nb_exp-1 DO BEGIN
  cmd = execute('var = errmslp_'+strtrim(i,2))
  cmd = execute('date = date_err_'+strtrim(i,2))
  IF i EQ 1 THEN splot, date, var, XTICKFORMAT='(I8)', XTICKINTERVAL=1, xminor=4, yrange=[minplot,maxplot], xrange=[mindate,maxdate], title='MSLP ERROR (hPa)', xtitle='date', thick=1, win=0, ytitle='MSLP ERROR (hPa)', xgridstyle=2, xticklen=1.0
  IF exp_list[i] EQ 'ALADIN-ANA' THEN oplot, date, var, color=color_factor*(i-1), thick=2.5 $
  ELSE oplot, date, var, color=color_factor*(i-1), thick=1
  oplot, [0,date[0]], [0,var[0]], psym=1, thick=2, symsize=2, color=color_factor*(i-1)
  IF i EQ 1 THEN oplot, date, var*0., thick=1, linestyle=2
  xyouts, 0.125, 0.200-0.020*i, exp_list[i], /normal, charsize=1, charthick=2, color=color_factor*(i-1)
  xyouts, 0.300, 0.200-0.020*i, '('+alt_list[i]+')', /normal, charsize=1, charthick=2, color=color_factor*(i-1)
ENDFOR
IF write_ps THEN closeps ELSE saveimage, plt_path+''+'DATE_ERRMSLP_1D.gif', quality=100


IF write_ps THEN openps, filename=plt_path+'ECH_ERRMSLP_1D'
FOR i = 2, nb_exp-1 DO BEGIN
  cmd = execute('var = errmslp_'+strtrim(i,2))
  cmd = execute('date = date_err_'+strtrim(i,2))
  lead_time = indgen(n_elements(date))*6
  IF i EQ 2 THEN splot, lead_time, var, XTICKINTERVAL=12, xminor=2, yrange=[minplot,maxplot], xrange=[0,96], title='MSLP ERROR (hPa)', xtitle='LEAD TIME (hours)', thick=1, win=0, ytitle='MSLP ERROR (hPa)', xgridstyle=2, xticklen=1.0
  oplot, lead_time, var, color=color_factor*(i-1), thick=1
  IF i EQ 2 THEN oplot, indgen(96), indgen(96)*0., thick=1, linestyle=2
  xyouts, 0.125, 0.200-0.020*i, exp_list[i], /normal, charsize=1, charthick=2, color=color_factor*(i-1)
  xyouts, 0.300, 0.200-0.020*i, '('+alt_list[i]+')', /normal, charsize=1, charthick=2, color=color_factor*(i-1)
ENDFOR
IF write_ps THEN closeps ELSE saveimage, plt_path+''+'ECH_ERRMSLP_1D.gif', quality=100
