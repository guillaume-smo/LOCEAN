; PLOT 1D MSLP vs WIND
lct,60
color_factor=70
key_portrait=1
IF write_ps THEN openps, filename=plt_path+'MSLP_vs_WIND_1DTC'


j = 0 & k = 0 & l = 0
FOR i = 0, nb_exp-1 DO BEGIN

  cmd = execute('var = min_mslp_'+strtrim(i,2)+'[*]')
  cmd = execute('date = max_w10m_'+strtrim(i,2)+'[*]')

  IF i EQ 0 THEN maxdate=max(date,/nan) ELSE maxdate=max([maxdate,date],/nan)
  IF i EQ 0 THEN mindate=min(date,/nan) ELSE mindate=min([mindate,date],/nan)
  IF i EQ 0 THEN maxplot=max(var,/nan) ELSE maxplot=max([maxplot,var],/nan)
  IF i EQ 0 THEN minplot=min(var,/nan) ELSE minplot=min([minplot,var],/nan)

  IF exp_list[i] EQ 'ALADIN-OPER' THEN BEGIN
    IF j EQ 0 THEN ypts_alao = var  ELSE ypts_alao = [ ypts_alao , var ]
    IF j EQ 0 THEN xpts_alao = date ELSE xpts_alao = [ xpts_alao , date ]
    j = j+1
  ENDIF

  IF exp_list[i] EQ 'ALADIN-ANA' THEN BEGIN
    IF l EQ 0 THEN ypts_alaa = var  ELSE ypts_alaa = [ ypts_alaa , var ]
    IF l EQ 0 THEN xpts_alaa = date ELSE xpts_alaa = [ xpts_alaa , date ]
    l = l+1
  ENDIF

  ; AROME
  IF STRMID(exp_list[i],0,1) EQ '9' THEN BEGIN
    IF k EQ 0 THEN ypts_aro = var  ELSE ypts_aro = [ ypts_aro , var ]
    IF k EQ 0 THEN xpts_aro = date ELSE xpts_aro = [ xpts_aro , date ]
    k = k+1
  ENDIF

ENDFOR

;maxplot = maxplot + 0.05*(maxplot-minplot)
;minplot = minplot - 0.05*(maxplot-minplot)
minplot = 890.
maxplot = 1000.
mindate = 15.
maxdate = 100.


FOR i = 0, nb_exp-1 DO BEGIN
  cmd = execute('date = max_w10m_'+strtrim(i,2)+'[*]')
  cmd = execute('var = min_mslp_'+strtrim(i,2)+'[*]')
;  a = (regress(date,var,const=b))[0]
;  x = findgen(100)
;  y = a*x + b
  IF i EQ 0 THEN splot, date, var, XTICKFORMAT='(F10.1)', xminor=8, yrange=[minplot,maxplot], xrange=[mindate,maxdate], title='WIND-PRESSURE RELATIONSHIP', xtitle='MAX 10M-WIND (m/s)', ytitle='MIN MSLP (hPa)', thick=1, psym=1, symsize=1.0, win=0
  IF exp_list[i] EQ 'IBTRACS' THEN l = 0
  IF exp_list[i] EQ 'ALADIN-OPER' THEN l = 1
  IF exp_list[i] EQ 'ALADIN-ANA'  THEN l = 2
  IF STRMID(exp_list[i],0,2) EQ '9A' THEN l = 3
  oplot, date, var, color=color_factor*l, thick=1, psym=1, symsize=1.0
;  oplot, x, y, color=color_factor*i
;  xyouts, 0.125, 0.200-0.020*i, exp_list[i], /normal, charsize=1.5, charthick=2, color=color_factor*l
;  xyouts, 0.300, 0.200-0.020*i, '('+alt_list[i]+')', /normal, charsize=1.5, charthick=2, color=color_factor*l
ENDFOR


; OBS IBTRACS
a = (regress(max_w10m_0,min_mslp_0,const=b))[0]
x = findgen(100)
y = a*x + b
oplot, x, y, color=color_factor*0, thick=2.
xyouts, 0.15, 0.400, 'coef IBTRACS:'+strtrim(a,2), /normal, charsize=1.5, charthick=1, color=color_factor*0

; ALADIN ANALYSE
a = (regress(xpts_alaa,ypts_alaa,const=b))[0]
x = findgen(100)
y = a*x + b
oplot, x, y, color=color_factor*1, thick=2.
xyouts, 0.15, 0.375, 'coef ALADIN-ANA:'+strtrim(a,2), /normal, charsize=1.5, charthick=1, color=color_factor*1

; ALADIN FORECAST
a = (regress(xpts_alao,ypts_alao,const=b))[0]
x = findgen(100)
y = a*x + b
oplot, x, y, color=color_factor*2, thick=2.
xyouts, 0.15, 0.350, 'coef ALADIN-FCT:'+strtrim(a,2), /normal, charsize=1.5, charthick=1, color=color_factor*2

; AROME
a = (regress(xpts_aro,ypts_aro,const=b))[0]
x = findgen(100)
y = a*x + b
oplot, x, y, color=color_factor*3, thick=2.
xyouts, 0.15, 0.325, 'coef AROME:'+strtrim(a,2), /normal, charsize=1.5, charthick=1, color=color_factor*3

@read_ascii_cyclade

IF write_ps THEN closeps ELSE saveimage, plt_path+'MSLP_vs_WIND_1DTC.gif', quality=100
