;-------------------------------------------------------------------------------------------------
; PLOT ABSOLUTE ERREUR MODEL-MODEL
;-------------------------------------------------------------------------------------------------

  initncdf, path_0+file_0, glam=[20,380], /fullcgrid & domdef, box, /MEMEINDICES
  IF n_elements(exp_list) EQ 4 THEN nb_plot = n_elements(exp_list)-1 ELSE nb_plot = n_elements(exp_list)-2

  ; CALCUL
  FOR e = 2, n_elements(exp_list) -1 DO cmd = execute( 'errvar_mean_'+strtrim(e-1,2)+'m'+strtrim(e,2)+' = ABS(errvar_mean_'+strtrim(e-1,2)+') - ABS(errvar_mean_'+strtrim(e,2)+')' )
  IF n_elements(exp_list) EQ 4 THEN errvar_mean_1m3 = ABS(errvar_mean_1) - ABS(errvar_mean_3)

  ;FOR e = 2, n_elements(exp_list) -1 DO cmd = execute( 'errvar_mean_'+strtrim(e-1,2)+'m'+strtrim(e,2)+' = (ABS(errvar_mean_'+strtrim(e-1,2)+') - ABS(errvar_mean_'+strtrim(e,2)+')) / ABS(errvar_mean_'+strtrim(e-1,2)+') * 100.' )
  ;IF n_elements(exp_list) EQ 4 THEN errvar_mean_1m3 = (ABS(errvar_mean_1) - ABS(errvar_mean_3)) / ABS(errvar_mean_1) * 100.

  ; MIN/MAX
  IF n_elements(exp_list) EQ 4 THEN BEGIN
    minvar = MIN(errvar_mean_1m3, /NAN) & maxvar = MAX(errvar_mean_1m3, /NAN)
  ENDIF ELSE BEGIN
    minvar = !NULL & maxvar = !NULL
  ENDELSE
  FOR e = 2, n_elements(exp_list) -1 DO BEGIN
    cmd = execute( 'var_plot = errvar_mean_'+strtrim(e-1,2)+'m'+strtrim(e,2) )
    minvar = MIN([minvar,MIN(var_plot, /NAN)], /NAN)
    maxvar = MAX([maxvar,MAX(var_plot, /NAN)], /NAN)
  ENDFOR
  IF ABS(minvar) GT maxvar THEN maxvar = ABS(minvar)
  IF maxvar GT ABS(minvar) THEN minvar = -1. * maxvar
  intvar=(maxvar-minvar)/20.
  IF flag_sig THEN lct, 56 ELSE lct, 64
  IF var_name EQ 'STRESS' THEN fmt='(F7.2)' ELSE fmt='(F6.1)'
  IF var_name EQ 'SST' THEN BEGIN & minvar = -2 & maxvar = 2 & intvar = 0.1 & ENDIF
  ;IF var_name EQ 'STRESS' THEN BEGIN & minvar = -0.01 & maxvar = 0.01 & intvar = 0.001 & ENDIF
  IF var_name EQ 'RAIN' THEN BEGIN & minvar = -5. & maxvar = 5. & intvar = 0.5 & ENDIF
  ;IF var_name EQ 'RAIN' THEN BEGIN & minvar = -100. & maxvar = 100. & intvar = 10. & ENDIF
  IF var_name EQ 'SKT' THEN BEGIN & minvar = -5. & maxvar = 5. & intvar = 0.5 & ENDIF
  IF var_name EQ 'W' THEN BEGIN & minvar = -0.02 & maxvar = 0.02 & intvar = 0.002 & ENDIF

  @def_plot_win
  fig_name = var_name+'_ABSERRORDIFF_'+data_type+'_'+zone+'_'+period
  IF write_ps THEN openps, filename=plot_dir+'/'+STRUPCASE(data_type)+'/'+zone+'/'+var_name+'/'+fig_name

  IF flag_sig THEN var_plot = errvar_mean_1m2*sig2D_12 ELSE var_plot = errvar_mean_1m2
  plt, var_plot, min=minvar, max=maxvar, int=intvar, realcont=2, /nocont, title=var_name+' ABS(ERROR) DIFF: '+exp_list[1]+' - '+exp_list[2]+' - '+zone+' - '+period, subtitle='', xtitle='', ytitle='', small=[win, 1], NOCOLORBAR=(nb_plot GT 1), COLNUMB=colnumb, NCONTOUR=ncontour, charsize=1, cell_fill=clf
  FOR e = 3, n_elements(exp_list)-1 DO BEGIN
    IF flag_sig THEN cmd = execute( 'var_plot = errvar_mean_'+strtrim(e-1,2)+'m'+strtrim(e,2)+'*sig2D_'+strtrim(e-1,2)+strtrim(e,2) ) $
                       ELSE cmd = execute( 'var_plot = errvar_mean_'+strtrim(e-1,2)+'m'+strtrim(e,2) )
    plt, var_plot, min=minvar, max=maxvar, int=intvar, realcont=2, /nocont, title=var_name+' ABS(ERROR) DIFFERENCE: '+exp_list[e-1]+' - '+exp_list[e], subtitle='', xtitle='', ytitle='', small=[win,e-1], /noerase, /nocolorbar, charsize=1, cell_fill=clf
  ENDFOR

  IF n_elements(exp_list) EQ 4 THEN BEGIN
    IF flag_sig THEN var_plot = errvar_mean_1m3*sig2D_13 ELSE var_plot = errvar_mean_1m3
    plt, var_plot, min=minvar, max=maxvar, int=intvar, realcont=2, /nocont, title=var_name+' ABS(ERROR) DIFFERENCE: '+exp_list[1]+' - '+exp_list[3], subtitle='', xtitle='', ytitle='', small=[win,3], /noerase, /nocolorbar, charsize=1, cell_fill=clf
  ENDIF

  @def_plot_cb

  IF write_ps THEN closeps ELSE STOP
