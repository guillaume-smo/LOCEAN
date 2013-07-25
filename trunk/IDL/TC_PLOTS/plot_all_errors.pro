print, '' & print, 'PLOT ALL ERRORS...'

lct,60
color_factor = 70
key_portrait = 1
;write_ps = 0
IF write_ps THEN thc = 6 ELSE thc = 2


err_list = [ 'errdist', 'errwind', 'errwrad', 'errmslp', 'err_rmw', 'err_sst' ]
eru_list = [ '(km)', '(m/s)', '(m/s)', '(hPa)', '(km)', '(K)' ]
nb_err   = n_elements(err_list)
lead_time_arom = indgen(maxnbt_arom)*6; & help, lead_time_arom


FOR l = 0, nb_err-1 DO BEGIN

  FOR i = 1, nb_exp-1 DO BEGIN
    cmd = execute('var = '+err_list[l]+'_'+strtrim(i,2)+'[*]')
    IF i EQ 1 THEN maxplot=max(var,/nan) ELSE maxplot=max([maxplot,var],/nan)
    IF i EQ 1 THEN minplot=min(var,/nan) ELSE minplot=min([minplot,var],/nan)
  ENDFOR
  maxplot = maxplot + 0.05*(maxplot-minplot)
  minplot = minplot - 0.05*(maxplot-minplot)
  help, minplot, maxplot

  FOR i = 0, nb_par-1 DO BEGIN
    IF write_ps THEN openps, filename=plt_path+'ALL_'+err_list[l]+'_'+par_list[i]
    splot, lead_time_arom, fltarr(maxnbt_arom), XTICKINTERVAL=12, xminor=2, yrange=[minplot,maxplot], xrange=[0,96], title=err_list[l], xtitle='FORECAST TIME (hours)', ytitle=err_list[l]+' '+eru_list[l], xgridstyle=2, xticklen=1.0, charsize=1.5, charthick=2, thick=thc/2, linestyle=2
    FOR j = 0, nb_date-1 DO BEGIN
      k = i * nb_date + j + 1      
;      print, err_list[l], ' ', par_list[i], ' ', date_list[j]
      cmd = execute( 'var = '+err_list[l]+'_'+strtrim(k,2) )
      oplot, lead_time_arom, var, color=color_factor*(j+1) MOD 256, thick=thc
      IF j LE 8 THEN BEGIN
        xyouts, 0.125, 0.180-0.020*(j+1), exp_list[k], /normal, charsize=1, charthick=2, color=color_factor*(j+1) MOD 256
        xyouts, 0.380, 0.180-0.020*(j+1), '('+alt_list[k]+')', /normal, charsize=1, charthick=2, color=color_factor*(j+1) MOD 256
      ENDIF ELSE BEGIN
        xyouts, 0.525, 0.180-0.020*(j+1-8), exp_list[k], /normal, charsize=1, charthick=2, color=color_factor*(j+1) MOD 256
        xyouts, 0.780, 0.180-0.020*(j+1-8), '('+alt_list[k]+')', /normal, charsize=1, charthick=2, color=color_factor*(j+1) MOD 256
      ENDELSE
    ENDFOR
  ENDFOR

ENDFOR

print, 'PLOT ALL ERRORS OK' & print, ''
