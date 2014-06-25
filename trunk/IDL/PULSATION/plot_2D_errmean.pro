
IF n_elements(exp_list) GT 1 THEN BEGIN


;-------------------------------------------------------------------------------------------------
; PLOT SETUP
;-------------------------------------------------------------------------------------------------

IF zone EQ 'TROP' OR zone EQ 'ALL' OR zone EQ 'INDPAC' THEN key_portrait = 0 ELSE key_portrait = 1
IF zone EQ 'TROP' OR zone EQ 'ALL' OR zone EQ 'IO' OR zone EQ 'NIO' OR zone EQ 'INDPAC' THEN uvs = [10,10] ELSE uvs = [5,5]
IF zone EQ 'AIMR' OR zone EQ 'IND' THEN uvs = [2,2]
IF var_name EQ 'SST' OR var_name EQ 'SKT' OR var_name EQ 'RAIN' OR $
   var_name EQ 'GSW' OR var_name EQ 'GRAD_MSLP' OR var_name EQ 'HEIGHT' OR $
   var_name EQ 'LANDUSE' OR var_name EQ 'ROT_UV10' THEN clf = 1 ELSE clf = 0


;-------------------------------------------------------------------------------------------------
; PLOT MODEL-OBS aka "ERROR"
;-------------------------------------------------------------------------------------------------

initncdf, path_0+file_0, glam=[20,380], /fullcgrid & domdef, box, /MEMEINDICES
nb_plot = n_elements(exp_list)-1 

; ERROR CALCUL
FOR e = 1, n_elements(exp_list) -1 DO cmd = execute( 'errvar_mean_'+strtrim(e,2)+' = var_mean_'+strtrim(e,2)+'_gridobs - var_mean_0')
IF STRMATCH( var_name, 'UV*') OR var_name EQ 'STRESS' THEN BEGIN
  FOR e = 1, n_elements(exp_list) -1 DO cmd = execute( 'errvarx_mean_'+strtrim(e,2)+' = varx_mean_'+strtrim(e,2)+'_gridobs - varx_mean_0')
  FOR e = 1, n_elements(exp_list) -1 DO cmd = execute( 'errvary_mean_'+strtrim(e,2)+' = vary_mean_'+strtrim(e,2)+'_gridobs - vary_mean_0')
ENDIF

; MIN/MAX PLOT
minvar = MIN(errvar_mean_1, /NAN) & maxvar = MAX(errvar_mean_1, /NAN)
FOR e = 1, n_elements(exp_list) -1 DO BEGIN
  cmd = execute( 'var_plot = errvar_mean_'+strtrim(e,2) )
  minvar = MIN([minvar,MIN(var_plot, /NAN)], /NAN)
  maxvar = MAX([maxvar,MAX(var_plot, /NAN)], /NAN)
ENDFOR
; maxvar = maxvar + 0.05*(maxvar-minvar) & print, maxvar
; minvar = minvar - 0.05*(maxvar-minvar) & print, minvar
IF ABS(minvar) GT maxvar THEN maxvar = ABS(minvar)
IF maxvar GT ABS(minvar) THEN minvar = -1. * maxvar
intvar=(maxvar-minvar)/20. 
IF flag_sig THEN lct, 56 ELSE lct, 64
fmt='(F6.1)'
IF var_name EQ 'SST' THEN BEGIN & minvar = -2 & maxvar = 2 & intvar = 0.1 & ENDIF
IF var_name EQ 'THETAO' THEN BEGIN & minvar = -5 & maxvar = 5 & intvar = 0.25 & ENDIF
IF var_name EQ 'STRESS' THEN BEGIN & minvar = -0.10 & maxvar = 0.10 & intvar = 0.01 & fmt='(F7.2)' & ENDIF
IF var_name EQ 'RAIN' THEN BEGIN & minvar = -15.   & maxvar = 15. & intvar = 1. & ENDIF
IF var_name EQ 'SKT'  THEN BEGIN & minvar = -15.   & maxvar = 15. & intvar = 1. & ENDIF
IF var_name EQ 'W'    THEN BEGIN & minvar = -0.015 & maxvar = 0.015 & intvar = 0.0015 & fmt='(F6.3)'& ENDIF
IF var_name EQ 'PSFC' THEN BEGIN & minvar = -10.   & maxvar = 10. & intvar = 1. & ENDIF
;IF var_name EQ 'MSLP' THEN BEGIN & minvar = -10.   & maxvar = 10. & intvar = 1. & ENDIF
IF var_name EQ 'MSLP' THEN BEGIN & minvar = -2.    & maxvar = 2.  & intvar = 0.25 & ENDIF
IF var_name EQ 'UV10' THEN BEGIN & minvar = -5.    & maxvar = 5.  & intvar = 0.5 & ENDIF
IF var_name EQ 'HEIGHT' THEN BEGIN & minvar = -200. & maxvar = 200. & intvar = 10. & ENDIF
IF var_name EQ 'T2'    THEN BEGIN & minvar = -5. & maxvar = 5. & intvar = 0.5 & ENDIF
IF var_name EQ 'TPOT2' THEN BEGIN & minvar = -5. & maxvar = 5. & intvar = 0.5 & ENDIF
IF var_name EQ 'ROT_UV10' THEN BEGIN & minvar = -10. & maxvar = 10. & intvar = 1. & ENDIF
IF var_name EQ 'D20' THEN BEGIN & minvar = -50. & maxvar = 50. & intvar = 5. & ENDIF

@def_plot_win
fig_name = var_name+'_ERROR_'+data_type+'_'+zone+'_'+period
IF write_ps  THEN openps, filename=plot_dir+'/'+STRUPCASE(data_type)+'/'+zone+'/'+var_name+'/'+fig_name
IF write_gif THEN SET_PLOT, 'Z'

IF flag_sig THEN var_plot = errvar_mean_1*sig2D_01 ELSE var_plot = errvar_mean_1
plt, var_plot, min=minvar, max=maxvar, int=intvar, realcont=2, /nocont, title=var_name+' ERROR - '+exp_list[1]+' - '+zone+' - '+period, subtitle='', xtitle='', ytitle='', small=[win, 1], NOCOLORBAR=(nb_plot GT 1), COLNUMB=colnumb, NCONTOUR=ncontour, charsize=1, cell_fill=clf
e=1
IF STRMATCH( var_name, 'UV*') OR var_name EQ 'STRESS' THEN cmd = execute( 'ajoutvect, {u:{a:errvarx_mean_'+strtrim(e,2)+', g:"T"}, v:{a:errvary_mean_'+strtrim(e,2)+', g:"T"}}, unvectsur=uvs/2., normeref=vref/2., cmref=1, vectthick=2' )
FOR e = 2, n_elements(exp_list)-1 DO BEGIN
  IF flag_sig THEN cmd = execute( 'var_plot = errvar_mean_'+strtrim(e,2)+'*sig2D_0'+strtrim(e,2) ) $
                     ELSE cmd = execute( 'var_plot = errvar_mean_'+strtrim(e,2) )
  plt, var_plot, min=minvar, max=maxvar, int=intvar, realcont=2, /nocont, title=var_name+' ERROR - '+exp_list[e], subtitle='', xtitle='', ytitle='', small=[win, e], /noerase, /nocolorbar, charsize=1, cell_fill=clf
  IF STRMATCH( var_name, 'UV*') OR var_name EQ 'STRESS' THEN cmd = execute( 'ajoutvect, {u:{a:errvarx_mean_'+strtrim(e,2)+', g:"T"}, v:{a:errvary_mean_'+strtrim(e,2)+', g:"T"}}, unvectsur=uvs/2., normeref=vref/2., cmref=1, vectthick=2' )
ENDFOR

@def_plot_cb
IF write_gif THEN saveimage, plot_dir+'/'+STRUPCASE(data_type)+'/'+zone+'/'+var_name+'/'+fig_name+'.gif'
IF write_ps  THEN closeps
IF write_ps EQ 0 AND write_gif EQ 0 THEN STOP



;-------------------------------------------------------------------------------------------------
; PLOT MODEL-MODEL aka "DIFF"
;-------------------------------------------------------------------------------------------------

initncdf, path_0+file_0, glam=[20,380], /fullcgrid & domdef, box, /MEMEINDICES
IF n_elements(exp_list) EQ 4 THEN nb_plot = n_elements(exp_list)-1 ELSE nb_plot = n_elements(exp_list)-2 
IF nb_plot GE 1 THEN BEGIN

  ; CALCUL
  FOR e = 2, n_elements(exp_list)-1 DO cmd = execute( 'var_mean_'+strtrim(e,2)+'m'+strtrim(e-1,2)+' = var_mean_'+strtrim(e,2)+'_gridobs - var_mean_'+strtrim(e-1,2)+'_gridobs')
  IF n_elements(exp_list) EQ 4 THEN var_mean_3m1 = var_mean_3_gridobs - var_mean_1_gridobs
  IF STRMATCH( var_name, 'UV*') OR var_name EQ 'STRESS' OR var_name EQ 'RAIN' THEN BEGIN
    FOR e = 2, n_elements(exp_list)-1 DO cmd = execute( 'varx_mean_'+strtrim(e,2)+'m'+strtrim(e-1,2)+' = varx_mean_'+strtrim(e,2)+'_gridobs - varx_mean_'+strtrim(e-1,2)+'_gridobs')
    FOR e = 2, n_elements(exp_list)-1 DO cmd = execute( 'vary_mean_'+strtrim(e,2)+'m'+strtrim(e-1,2)+' = vary_mean_'+strtrim(e,2)+'_gridobs - vary_mean_'+strtrim(e-1,2)+'_gridobs')
    IF n_elements(exp_list) EQ 4 THEN BEGIN
      varx_mean_3m1 = varx_mean_3_gridobs - varx_mean_1_gridobs
      vary_mean_3m1 = vary_mean_3_gridobs - vary_mean_1_gridobs
    ENDIF
  ENDIF

  ; MIN/MAX
  IF n_elements(exp_list) EQ 4 THEN BEGIN 
     minvar = MIN(var_mean_3m1, /NAN) & maxvar = MAX(var_mean_3m1, /NAN)
  ENDIF ELSE BEGIN 
    minvar = !NULL & maxvar = !NULL
  ENDELSE
  FOR e = 2, n_elements(exp_list) -1 DO BEGIN
    cmd = execute( 'var_plot = var_mean_'+strtrim(e,2)+'m'+strtrim(e-1,2) )
    minvar = MIN([minvar,MIN(var_plot, /NAN)], /NAN)
    maxvar = MAX([maxvar,MAX(var_plot, /NAN)], /NAN)
  ENDFOR
  IF ABS(minvar) GT maxvar THEN maxvar = ABS(minvar)
  IF maxvar GT ABS(minvar) THEN minvar = -1. * maxvar
  intvar=(maxvar-minvar)/20.
  IF flag_sig THEN lct, 56 ELSE lct, 64
  IF var_name EQ 'STRESS' THEN fmt='(F7.2)' ELSE fmt='(F6.1)'
  IF var_name EQ 'SST' THEN BEGIN & minvar = -2 & maxvar = 2 & intvar = 0.1 & ENDIF
IF var_name EQ 'THETAO' THEN BEGIN & minvar = -1.5 & maxvar = 1.5 & intvar = 0.1 & ENDIF
  ;IF var_name EQ 'STRESS' THEN BEGIN & minvar = -0.01 & maxvar = 0.01 & intvar = 0.001 & ENDIF
  IF var_name EQ 'STRESS' THEN BEGIN & minvar = -0.10 & maxvar = 0.10 & intvar = 0.005 & ENDIF
  IF var_name EQ 'RAIN' THEN BEGIN & minvar = -10. & maxvar = 10. & intvar = 1.  & ENDIF
  IF var_name EQ 'SKT'  THEN BEGIN & minvar = -5.  & maxvar = 5.  & intvar = 0.5 & ENDIF
  IF var_name EQ 'OLR'  THEN BEGIN & minvar = -30. & maxvar = 30. & intvar = 2.  & ENDIF
  IF var_name EQ 'UV'   THEN BEGIN & minvar = -6.  & maxvar = 6.  & intvar = 0.5 & ENDIF
  IF var_name EQ 'UV10' THEN BEGIN & minvar = -3.  & maxvar = 3.  & intvar = 0.25 & ENDIF
  IF var_name EQ 'W' THEN BEGIN & minvar = -0.005 & maxvar = 0.005 & intvar = 0.001 & fmt='(F6.3)'& ENDIF
  IF var_name EQ 'MSLP' THEN BEGIN & minvar = -5 & maxvar = 5 & intvar = 0.5 & ENDIF
  IF var_name EQ 'HEIGHT' THEN BEGIN & minvar = -200. & maxvar = 200. & intvar = 10. & ENDIF
  IF var_name EQ 'T2'    THEN BEGIN & minvar = -5. & maxvar = 5. & intvar = 0.5 & ENDIF
  IF var_name EQ 'T2POT' THEN BEGIN & minvar = -5. & maxvar = 5. & intvar = 0.5 & ENDIF

  @def_plot_win
  fig_name = var_name+'_MODELDIFF_'+data_type+'_'+zone+'_'+period
  IF write_ps THEN openps, filename=plot_dir+'/'+STRUPCASE(data_type)+'/'+zone+'/'+var_name+'/'+fig_name
  IF flag_sig THEN var_plot = var_mean_2m1*sig2D_12 ELSE var_plot = var_mean_2m1

  plt, var_plot, min=minvar, max=maxvar, int=intvar, realcont=2, /nocont, title=var_name+' DIFFERENCE: '+exp_list[2]+' - '+exp_list[1]+' - '+zone+' - '+period, subtitle='', xtitle='', ytitle='', small=[win, 1], NOCOLORBAR=(nb_plot GT 1), COLNUMB=colnumb, NCONTOUR=ncontour, charsize=1, cell_fill=clf
  IF STRMATCH( var_name, 'UV*') OR var_name EQ 'STRESS' THEN cmd = execute( 'ajoutvect, {u:{a:varx_mean_2m1, g:"T"}, v:{a:vary_mean_2m1, g:"T"}}, unvectsur=uvs/2., normeref=2., cmref=1, vectthick=2' ) ; normeref=CEIL(maxvar)

  FOR e = 3, n_elements(exp_list)-1 DO BEGIN
    IF flag_sig THEN cmd = execute( 'var_plot = var_mean_'+strtrim(e,2)+'m'+strtrim(e-1,2)+'*sig2D_'+strtrim(e-1,2)+strtrim(e,2) ) $
                ELSE cmd = execute( 'var_plot = var_mean_'+strtrim(e,2)+'m'+strtrim(e-1,2) )
    plt, var_plot, min=minvar, max=maxvar, int=intvar, realcont=2, /nocont, title=var_name+' DIFFERENCE: '+exp_list[e]+' - '+exp_list[e-1], subtitle='', xtitle='', ytitle='', small=[win,e-1], /noerase, /nocolorbar, charsize=1, cell_fill=clf
    IF STRMATCH( var_name, 'UV*') OR var_name EQ 'STRESS' THEN cmd = execute( 'ajoutvect, {u:{a:varx_mean_'+strtrim(e,2)+'m'+strtrim(e-1,2)+', g:"T"}, v:{a:vary_mean_'+strtrim(e,2)+'m'+strtrim(e-1,2)+', g:"T"}}, unvectsur=uvs/6., normeref=CEIL(maxvar), cmref=1, vectthick=2' )

  ENDFOR
  IF n_elements(exp_list) EQ 4 THEN BEGIN
    IF flag_sig THEN var_plot = var_mean_3m1*sig2D_13 ELSE var_plot = var_mean_3m1
    plt, var_plot, min=minvar, max=maxvar, int=intvar, realcont=2, /nocont, title=var_name+' DIFFERENCE: '+exp_list[3]+' - '+exp_list[1], subtitle='', xtitle='', ytitle='', small=[win,3], /noerase, /nocolorbar, charsize=1, cell_fill=clf
    IF STRMATCH( var_name, 'UV*') OR var_name EQ 'STRESS' THEN cmd = execute( 'ajoutvect, {u:{a:varx_mean_3m1, g:"T"}, v:{a:vary_mean_3m1, g:"T"}}, unvectsur=uvs/2., normeref=vref/2., cmref=1, vectthick=2' )
  ENDIF

  @def_plot_cb
  IF write_ps THEN closeps ELSE STOP


  IF var_name EQ 'RAIN' THEN BEGIN

    @def_plot_win
    fig_name = var_name+'E_MODELDIFF_'+data_type+'_'+zone+'_'+period
    IF write_ps THEN openps, filename=plot_dir+'/'+STRUPCASE(data_type)+'/'+zone+'/'+var_name+'/'+fig_name
    IF flag_sig THEN var_plot = varx_mean_2m1*sig2D_12 ELSE var_plot = varx_mean_2m1
    minvar = -10. & maxvar = 10.

    plt, var_plot, min=minvar, max=maxvar, int=intvar, realcont=2, /nocont, title='CONVECTIVE SCHEME '+var_name+' DIFFERENCE: '+exp_list[2]+' - '+exp_list[1]+' - '+zone+' - '+period, subtitle='', xtitle='', ytitle='', small=[win, 1], NOCOLORBAR=(nb_plot GT 1), COLNUMB=colnumb, NCONTOUR=ncontour, charsize=1, cell_fill=clf

    FOR e = 3, n_elements(exp_list)-1 DO BEGIN
      IF flag_sig THEN cmd = execute( 'var_plot = varx_mean_'+strtrim(e,2)+'m'+strtrim(e-1,2)+'*sig2D_'+strtrim(e-1,2)+strtrim(e,2) ) $
                  ELSE cmd = execute( 'var_plot = varx_mean_'+strtrim(e,2)+'m'+strtrim(e-1,2) )
      plt, var_plot, min=minvar, max=maxvar, int=intvar, realcont=2, /nocont, title='CONVECTIVE SCHEME '+var_name+' DIFFERENCE: '+exp_list[e]+' - '+exp_list[e-1], subtitle='', xtitle='', ytitle='', small=[win,e-1], /noerase, /nocolorbar, charsize=1, cell_fill=clf
    ENDFOR
    IF n_elements(exp_list) EQ 4 THEN BEGIN
      IF flag_sig THEN var_plot = varx_mean_3m1*sig2D_13 ELSE var_plot = varx_mean_3m1
      plt, var_plot, min=minvar, max=maxvar, int=intvar, realcont=2, /nocont, title='CONVECTIVE SCHEME '+var_name+' DIFFERENCE: '+exp_list[3]+' - '+exp_list[1], subtitle='', xtitle='', ytitle='', small=[win,3], /noerase, /nocolorbar, charsize=1, cell_fill=clf
    ENDIF
    @def_plot_cb
    IF write_ps THEN closeps ELSE STOP

    @def_plot_win
    fig_name = var_name+'E_MODELDIFF_'+data_type+'_'+zone+'_'+period
    IF write_ps THEN openps, filename=plot_dir+'/'+STRUPCASE(data_type)+'/'+zone+'/'+var_name+'/'+fig_name
    IF flag_sig THEN var_plot = vary_mean_2m1*sig2D_12 ELSE var_plot = vary_mean_2m1

    plt, var_plot, min=minvar, max=maxvar, int=intvar, realcont=2, /nocont, title='EXPLICIT '+var_name+' DIFFERENCE: '+exp_list[2]+' - '+exp_list[1]+' - '+zone+' - '+period, subtitle='', xtitle='', ytitle='', small=[win, 1], NOCOLORBAR=(nb_plot GT 1), COLNUMB=colnumb, NCONTOUR=ncontour, charsize=1, cell_fill=clf

    FOR e = 3, n_elements(exp_list)-1 DO BEGIN
      IF flag_sig THEN cmd = execute( 'var_plot = vary_mean_'+strtrim(e,2)+'m'+strtrim(e-1,2)+'*sig2D_'+strtrim(e-1,2)+strtrim(e,2) ) $
                  ELSE cmd = execute( 'var_plot = vary_mean_'+strtrim(e,2)+'m'+strtrim(e-1,2) )
      plt, var_plot, min=minvar, max=maxvar, int=intvar, realcont=2, /nocont, title='EXPLICIT '+var_name+' DIFFERENCE: '+exp_list[e]+' - '+exp_list[e-1], subtitle='', xtitle='', ytitle='', small=[win,e-1], /noerase, /nocolorbar, charsize=1, cell_fill=clf
    ENDFOR
    IF n_elements(exp_list) EQ 4 THEN BEGIN
      IF flag_sig THEN var_plot = vary_mean_3m1*sig2D_13 ELSE var_plot = vary_mean_3m1
      plt, var_plot, min=minvar, max=maxvar, int=intvar, realcont=2, /nocont, title='EXPLICIT '+var_name+' DIFFERENCE: '+exp_list[3]+' - '+exp_list[1], subtitle='', xtitle='', ytitle='', small=[win,3], /noerase, /nocolorbar, charsize=1, cell_fill=clf
    ENDIF
    @def_plot_cb
    IF write_ps THEN closeps ELSE STOP

ENDIF


  ;@plot_2D_abs
  ;@plot_2D_stddev


ENDIF ; nb_exp > 2

ENDIF ; nb_exp > 1
