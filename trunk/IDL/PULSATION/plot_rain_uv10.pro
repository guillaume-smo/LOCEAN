

var_name = 'RAIN'
IF zone EQ 'TROP' OR zone EQ 'ALL' THEN key_portrait = 0 ELSE key_portrait = 1
IF zone EQ 'TROP' OR zone EQ 'ALL' OR zone EQ 'IO' OR zone EQ 'NIO' THEN uvs = [10,10] ELSE uvs = [5,5]
IF zone EQ 'AIMR' THEN uvs = [3,3]
IF var_name EQ 'SKT' OR var_name EQ 'RAIN' THEN clf = 2 ELSE clf = 0


;-------------------------------------------------------------------------------------------------
; PLOT 2D
;-------------------------------------------------------------------------------------------------

IF n_elements(exp_list) LE 6 THEN BEGIN
initncdf, path_rain+file_rain, glam=[20,380], /fullcgrid & domdef, box, /MEMEINDICES
nb_plot = n_elements(exp_list)

; MIN/MAX
var_plot = rain_mean_0
minvar = MIN(var_plot, /NAN) & maxvar = MAX(var_plot, /NAN)
FOR e = 1, n_elements(exp_list)-1 DO BEGIN
  cmd = execute( 'var_plot = rain_mean_'+strtrim(e,2) )
  minvar = MIN([minvar,MIN(var_plot, /NAN)], /NAN)
  maxvar = MAX([maxvar,MAX(var_plot, /NAN)], /NAN)
ENDFOR
;maxvar = maxvar + 0.05*(maxvar-minvar)
;minvar = minvar - 0.05*(maxvar-minvar)
intvar=(maxvar-minvar)/20. & lct, 22 ; 60
IF var_name EQ 'STRESS' THEN fmt='(F7.2)' ELSE fmt='(F6.1)'
IF var_name EQ 'SST' THEN BEGIN & minvar = 20 & maxvar = 30 & intvar = 0.5 & ENDIF
IF var_name EQ 'RAIN' THEN BEGIN 
  IF grid EQ 'ind025' THEN BEGIN 
    IF period EQ 'DJFM' THEN BEGIN minvar = 0 & maxvar = 20 & intvar = 2 & lct,15 & ENDIF
    IF period EQ 'JJAS' THEN BEGIN minvar = 0 & maxvar = 30 & intvar = 3 & lct,15 & ENDIF 
  ENDIF ELSE BEGIN minvar = 0 & maxvar = 30 & intvar = 2 & ENDELSE
ENDIF

@def_plot_win
fig_name = 'RAIN+UV10_c1m_'+zone+'_'+period
IF write_ps THEN openps, filename=plot_dir+'/'+STRUPCASE(data_type)+'/'+zone+'/'+var_name+'/'+fig_name
var_plot = rain_mean_0
plt, var_plot, min=minvar, max=maxvar, int=intvar, realcont=2, /nocont, title='RAIN+UV10 - '+zone+' - '+period+' - '+obs_name, subtitle='', xtitle='', ytitle='', small=[win, 1], /nocolorbar, COLNUMB=colnumb, NCONTOUR=ncontour, charsize=1, cell_fill=clf;, INV=(var_name EQ 'RAIN')
ajoutvect, {u:{a:u10_mean_0_gridrain, g:'T'}, v:{a:v10_mean_0_gridrain, g:'T'}}, unvectsur=uvs, normeref=12, cmref=1, vectthick=2
FOR e = 1, n_elements(exp_list)-1 DO BEGIN
  cmd = execute( 'var_plot = rain_mean_'+strtrim(e,2) )
  plt, var_plot, min=minvar, max=maxvar, int=intvar, realcont=2, /nocont, title='RAIN+UV10 - '+exp_list[e], subtitle='', xtitle='', ytitle='', small=[win, e+1], /noerase, /nocolorbar, charsize=1, cell_fill=clf;, INV=(var_name EQ 'RAIN')
  cmd = execute( 'xyouts, xy[0], xy[1], "CORR_PAT="+string(corrpat_0'+strtrim(e,2)+', format="(F6.3)"), /DATA, charsize=1' )
  cmd = execute( 'ajoutvect, {u:{a:u10_mean_'+strtrim(e,2)+'_gridrain, g:"T"}, v:{a:v10_mean_'+strtrim(e,2)+'_gridrain, g:"T"}}, unvectsur=uvs, normeref=12, cmref=1, vectthick=2' )
ENDFOR

@def_plot_cb
IF write_gif THEN saveimage, plot_dir+'/'+STRUPCASE(data_type)+'/'+zone+'/'+var_name+'/'+fig_name+'.gif'
IF write_ps  THEN closeps
IF write_ps EQ 0 AND write_gif EQ 0 THEN STOP

ENDIF


;-------------------------------------------------------------------------------------------------
; PLOT MODEL-OBS aka "MODEL ERROR"
;-------------------------------------------------------------------------------------------------

initncdf, path_rain+file_rain, glam=[20,380], /fullcgrid & domdef, box, /MEMEINDICES
nb_plot = n_elements(exp_list)-1 

; ERROR CALCUL
FOR e = 1, n_elements(exp_list) -1 DO cmd = execute( 'errrain_mean_'+strtrim(e,2)+' = rain_mean_'+strtrim(e,2)+' - rain_mean_0')
FOR e = 1, n_elements(exp_list) -1 DO cmd = execute( 'erruv10_mean_'+strtrim(e,2)+' = uv10_mean_'+strtrim(e,2)+'_gridrain - uv10_mean_0_gridrain')
FOR e = 1, n_elements(exp_list) -1 DO cmd = execute( 'erru10_mean_'+strtrim(e,2)+' = u10_mean_'+strtrim(e,2)+'_gridrain - u10_mean_0_gridrain')
FOR e = 1, n_elements(exp_list) -1 DO cmd = execute( 'errv10_mean_'+strtrim(e,2)+' = v10_mean_'+strtrim(e,2)+'_gridrain - v10_mean_0_gridrain')

; MIN/MAX PLOT
minrain = MIN(errrain_mean_1, /NAN) & maxrain = MAX(errrain_mean_1, /NAN)
FOR e = 1, n_elements(exp_list) -1 DO BEGIN
  cmd = execute( 'var_plot = errrain_mean_'+strtrim(e,2) )
  minvar = MIN([minvar,MIN(var_plot, /NAN)], /NAN)
  maxvar = MAX([maxvar,MAX(var_plot, /NAN)], /NAN)
ENDFOR
; maxvar = maxvar + 0.05*(maxvar-minvar) & print, maxvar
; minvar = minvar - 0.05*(maxvar-minvar) & print, minvar
IF ABS(minvar) GT maxvar THEN maxvar = ABS(minvar)
IF maxvar GT ABS(minvar) THEN minvar = -1. * maxvar
intvar=(maxvar-minvar)/20. & lct, 64
var_name = 'RAIN'
IF var_name EQ 'STRESS' THEN fmt='(F7.2)' ELSE fmt='(F6.1)'
IF var_name EQ 'SST' THEN BEGIN & minvar = -2 & maxvar = 2 & intvar = 0.1 & ENDIF
IF var_name EQ 'STRESS' THEN BEGIN & minvar = -0.05 & maxvar = 0.05 & intvar = 0.005 & ENDIF
IF var_name EQ 'RAIN' THEN BEGIN & minvar = -20. & maxvar = 20. & intvar = 1. & ENDIF
IF var_name EQ 'SKT' THEN BEGIN & minvar = -20. & maxvar = 20. & intvar = 1. & ENDIF

@def_plot_win
fig_name = 'RAIN+UV10_ERROR_c1m_'+zone+'_'+period
IF write_ps  THEN openps, filename=plot_dir+'/'+STRUPCASE(data_type)+'/'+zone+'/'+var_name+'/'+fig_name
IF write_gif THEN SET_PLOT, 'Z'

var_plot = errrain_mean_1
plt, var_plot, min=minvar, max=maxvar, int=intvar, realcont=2, /nocont, title='RAIN+UV10 ERROR - '+exp_list[1]+' - '+zone+' - '+period, subtitle='', xtitle='', ytitle='', small=[win, 1], /nocolorbar, COLNUMB=colnumb, NCONTOUR=ncontour, charsize=1, cell_fill=clf
e=1
cmd = execute( 'ajoutvect, {u:{a:erru10_mean_'+strtrim(e,2)+', g:"T"}, v:{a:errv10_mean_'+strtrim(e,2)+', g:"T"}}, unvectsur=uvs, normeref=6, cmref=1, vectthick=2' )
FOR e = 2, n_elements(exp_list)-1 DO BEGIN
  cmd = execute( 'var_plot = errrain_mean_'+strtrim(e,2) )
  plt, var_plot, min=minvar, max=maxvar, int=intvar, realcont=2, /nocont, title='RAIN+UV10 ERROR - '+exp_list[e], subtitle='', xtitle='', ytitle='', small=[win, e], /noerase, /nocolorbar, charsize=1, cell_fill=clf
  cmd = execute( 'ajoutvect, {u:{a:erru10_mean_'+strtrim(e,2)+', g:"T"}, v:{a:errv10_mean_'+strtrim(e,2)+', g:"T"}}, unvectsur=uvs, normeref=6, cmref=1, vectthick=2' )
ENDFOR

@def_plot_cb
IF write_gif THEN saveimage, plot_dir+'/'+STRUPCASE(data_type)+'/'+zone+'/'+var_name+'/'+fig_name+'.gif'
IF write_ps  THEN closeps
IF write_ps EQ 0 AND write_gif EQ 0 THEN STOP


;-------------------------------------------------------------------------------------------------
; PLOT MODEL-MODEL
;-------------------------------------------------------------------------------------------------

initncdf, path_0+file_0, glam=[20,380], /fullcgrid & domdef, box, /MEMEINDICES
nb_plot = n_elements(exp_list)-2 
IF nb_plot GE 1 THEN BEGIN

; MIN/MAX
FOR e = 2, n_elements(exp_list) -1 DO cmd = execute( 'var_mean_'+strtrim(e,2)+'m'+strtrim(e-1,2)+' = var_mean_'+strtrim(e,2)+'_gridobs - var_mean_'+strtrim(e-1,2)+'_gridobs')
minvar = MIN(var_mean_2m1, /NAN) & maxvar = MAX(var_mean_2m1, /NAN)
FOR e = 3, n_elements(exp_list) -1 DO BEGIN
  cmd = execute( 'var_plot = var_mean_'+strtrim(e,2)+'m'+strtrim(e-1,2) )
  minvar = MIN([minvar,MIN(var_plot, /NAN)], /NAN)
  maxvar = MAX([maxvar,MAX(var_plot, /NAN)], /NAN)
ENDFOR
IF ABS(minvar) GT maxvar THEN maxvar = ABS(minvar)
IF maxvar GT ABS(minvar) THEN minvar = -1. * maxvar
intvar=(maxvar-minvar)/20. & lct, 64
IF var_name EQ 'STRESS' THEN fmt='(F7.2)' ELSE fmt='(F6.1)'
IF var_name EQ 'SST' THEN BEGIN & minvar = -2 & maxvar = 2 & intvar = 0.1 & ENDIF
;IF var_name EQ 'STRESS' THEN BEGIN & minvar = -0.01 & maxvar = 0.01 & intvar = 0.001 & ENDIF

@def_plot_win
fig_name = var_name+'_MODELDIFF_c1m_'+zone+'_'+period
IF write_ps THEN openps, filename=plot_dir+'/'+STRUPCASE(data_type)+'/'+zone+'/'+var_name+'/'+fig_name

var_plot = var_mean_2m1
plt, var_plot, min=minvar, max=maxvar, int=intvar, realcont=2, /nocont, title=var_name+' DIFFERENCE: '+exp_list[2]+' - '+exp_list[1]+' - '+zone+' - '+period, subtitle='', xtitle='', ytitle='', small=[win, 1], /nocolorbar, COLNUMB=colnumb, NCONTOUR=ncontour, charsize=1, cell_fill=clf
FOR e = 3, n_elements(exp_list)-1 DO BEGIN
  cmd = execute( 'var_plot = var_mean_'+strtrim(e,2)+'m'+strtrim(e-1,2) )
  plt, var_plot, min=minvar, max=maxvar, int=intvar, realcont=2, /nocont, title=var_name+' DIFFERENCE: '+exp_list[e]+' - '+exp_list[e-1], subtitle='', xtitle='', ytitle='', small=[win,e-1], /noerase, /nocolorbar, charsize=1, cell_fill=clf
ENDFOR

@def_plot_cb
IF write_ps THEN closeps ELSE STOP


;-------------------------------------------------------------------------------------------------
; PLOT ERREUR MODEL-MODEL
;-------------------------------------------------------------------------------------------------

initncdf, path_0+file_0, glam=[20,380], /fullcgrid & domdef, box, /MEMEINDICES
nb_plot = n_elements(exp_list)-2

; MIN/MAX
FOR e = 2, n_elements(exp_list) -1 DO cmd = execute( 'errvar_mean_'+strtrim(e-1,2)+'m'+strtrim(e,2)+' = ABS(errvar_mean_'+strtrim(e-1,2)+') - ABS(errvar_mean_'+strtrim(e,2)+')' )
minvar = MIN(errvar_mean_1m2, /NAN) & maxvar = MAX(errvar_mean_1m2, /NAN)
FOR e = 3, n_elements(exp_list) -1 DO BEGIN
  cmd = execute( 'var_plot = errvar_mean_'+strtrim(e-1,2)+'m'+strtrim(e,2) )
  minvar = MIN([minvar,MIN(var_plot, /NAN)], /NAN)
  maxvar = MAX([maxvar,MAX(var_plot, /NAN)], /NAN)
ENDFOR
;maxvar = maxvar + 0.05*(maxvar-minvar) & print, maxvar
;minvar = minvar - 0.05*(maxvar-minvar) & print, minvar
IF ABS(minvar) GT maxvar THEN maxvar = ABS(minvar)
IF maxvar GT ABS(minvar) THEN minvar = -1. * maxvar
intvar=(maxvar-minvar)/20. & lct, 64
IF var_name EQ 'STRESS' THEN fmt='(F7.2)' ELSE fmt='(F6.1)'
IF var_name EQ 'SST' THEN BEGIN & minvar = -2 & maxvar = 2 & intvar = 0.1 & ENDIF
;IF var_name EQ 'STRESS' THEN BEGIN & minvar = -0.01 & maxvar = 0.01 & intvar = 0.001 & ENDIF

@def_plot_win
fig_name = var_name+'_ABSERRORDIFF_c1m_'+zone+'_'+period
IF write_ps THEN openps, filename=plot_dir+'/'+STRUPCASE(data_type)+'/'+zone+'/'+var_name+'/'+fig_name

var_plot = errvar_mean_1m2
plt, var_plot, min=minvar, max=maxvar, int=intvar, realcont=2, /nocont, title=var_name+' ABS(ERROR) DIFFERENCE: '+exp_list[1]+' - '+exp_list[2]+' - '+zone+' - '+period, subtitle='', xtitle='', ytitle='', small=[win, 1], /nocolorbar, COLNUMB=colnumb, NCONTOUR=ncontour, charsize=1, cell_fill=clf
FOR e = 3, n_elements(exp_list)-1 DO BEGIN
  cmd = execute( 'var_plot = errvar_mean_'+strtrim(e-1,2)+'m'+strtrim(e,2) )
  plt, var_plot, min=minvar, max=maxvar, int=intvar, realcont=2, /nocont, title=var_name+' ABS(ERROR) DIFFERENCE: '+exp_list[e-1]+' - '+exp_list[e], subtitle='', xtitle='', ytitle='', small=[win,e-1], /noerase, /nocolorbar, charsize=1, cell_fill=clf
ENDFOR

;      IF n_elements(exp_list)-1 EQ 3 THEN BEGIN
;        var_plot = errvar_mean_1m2 + errvar_mean_2m3
;        plt, var_plot, min=minvar, max=maxvar, int=intvar, realcont=2, /nocont, title=var_name+' ABSOLUTE ERROR DIFFERENCE: '+exp_list[1]+' - '+exp_list[3], subtitle='', xtitle='', ytitle='', small=[win,3], /noerase, /nocolorbar, charsize=1
;      ENDIF

@def_plot_cb

IF write_ps THEN closeps ELSE STOP


;-------------------------------------------------------------------------------------------------
; DISPERSION INTER-MODELES (ERROR STD DEV)
;-------------------------------------------------------------------------------------------------

all_errvar_mean = !NULL
FOR e = 1, n_elements(exp_list)-1 DO cmd = execute( 'all_errvar_mean =  [[[all_errvar_mean]], [[errvar_mean_'+strtrim(e,2)+']]]' )
std_errvar_mean =  STDDEV(all_errvar_mean, DIMENSION=3, /NAN)
help, std_errvar_mean

fig_name = var_name+'_ERROR_STDDEV_c1m_'+zone+'_'+period
IF write_ps THEN openps, filename=plot_dir+'/'+STRUPCASE(data_type)+'/'+zone+'/'+var_name+'/'+fig_name

var_plot = std_errvar_mean
minvar = MIN(var_plot, /NAN) & maxvar = MAX(var_plot, /NAN)
maxvar = maxvar - 0.10*(maxvar-minvar)
intvar=(maxvar-minvar)/20. & lct, 22
IF var_name EQ 'STRESS' THEN fmt='(F7.2)' ELSE fmt='(F6.1)'

plt, var_plot, min=minvar, max=maxvar, int=intvar, realcont=2, /nocont, title=var_name+' ERROR STD DEV - '+zone+' - '+period, subtitle='', xtitle='', ytitle='', charsize=1, cell_fill=clf

IF write_ps THEN closeps ELSE STOP

ENDIF ; nb_exp > 2
