

;-------------------------------------------------------------------------------------------------
; PLOT SETUP
;-------------------------------------------------------------------------------------------------

  var_plot = 'var_zx_mean'
  print, 'plot_2D_xz: ', var_plot
  key_portrait = 0
  fig_name = var_name+'_'+var_plot+'_'+data_type+'_'+zone+'_'+period
  IF write_ps THEN openps, filename=plot_dir+'/'+STRUPCASE(data_type)+'/'+zone+'/'+var_name+'/'+fig_name
  lct, 22
  IF var_name EQ 'WQ' OR var_name EQ 'W' THEN BEGIN
    lct,64
    fmt = '(F6.3)'
  ENDIF
  nb_plot  = n_elements(exp_list)
  @def_plot_mask
  @def_plot_minmax
  @def_plot_win
  IF flag_nemo THEN zbox = [  0, 1000] ELSE zbox = [800,  200]


;-------------------------------------------------------------------------------------------------
; PLOT XZ MEAN (NATIVE GRID)
;-------------------------------------------------------------------------------------------------

  FOR e = 0, n_elements(exp_list)-1 DO BEGIN

    IF e EQ 0 THEN cmd = execute( 'initncdf, path_'+STRTRIM(e,2)+'+file_'+STRTRIM(e,2) ) $
              ELSE cmd = execute( 'initncdf, path_'+STRTRIM(e,2)+'+file_'+STRTRIM(e,2)+', ZAXISNAME="pressure"' )
    domdef, box, glam=[20,380]
    gdept = REVERSE(gdept, /OVERWRITE)
    cmd = execute( 'var = REVERSE(var_zx_mean_'+STRTRIM(e,2)+', 2)' )
    cmd = execute( 'var = var_zx_mean_'+STRTRIM(e,2) )

    pltz, var, min=minvar, max=maxvar, int=intvar, boxzoom=[ box[0], box[1], 0, 0 , zbox], /yindex, small=[win, e+1], noerase=(e GT 0), /nocolorbar, /nocontour, NCONTOUR=ncontour, COLNUMB=colnumb, subtitle='', xtitle='', ytitle='', title=mask_title+' '+var_name+' XZ - '+exp_list[e], /REMPLI

  ENDFOR

  @def_plot_cb
  IF write_gif THEN saveimage, plot_dir+'/'+STRUPCASE(data_type)+'/'+zone+'/'+var_name+'/'+fig_name+'.gif'
  IF write_ps  THEN closeps
  IF write_ps EQ 0 AND write_gif EQ 0 THEN STOP



;-------------------------------------------------------------------------------------------------
; PLOT SETUP
;-------------------------------------------------------------------------------------------------

  var_plot = 'var_zx_mean'
  print, 'plot_2D_xz: ', var_plot
  key_portrait = 0
  fig_name = var_name+'_'+var_plot+'_gridobs_'+data_type+'_'+zone+'_'+period
  IF write_ps THEN openps, filename=plot_dir+'/'+STRUPCASE(data_type)+'/'+zone+'/'+var_name+'/'+fig_name
  lct, 22
  IF var_name EQ 'WQ' OR var_name EQ 'W' THEN BEGIN
    lct,64
    fmt = '(F6.3)'
  ENDIF
  nb_plot  = n_elements(exp_list)
  @def_plot_win


;-------------------------------------------------------------------------------------------------
; PLOT XZ MEAN (GRID OBS)
;-------------------------------------------------------------------------------------------------

  initncdf, path_0+file_0 & domdef, box, glam=[20,380]
  FOR e = 0, n_elements(exp_list)-1 DO BEGIN

    IF e GT 0 THEN cmd = execute( 'var_zx_mean_'+STRTRIM(e,2)+'_gridobs = fromreg("bilinear", var_zx_mean_'+strtrim(e,2)+', lon_'+STRTRIM(e,2)+', z_'+STRTRIM(e,2)+', lon_0, z_0)' )
    IF e EQ 0 THEN cmd = execute( 'var = var_zx_mean_'+STRTRIM(e,2) ) ELSE cmd = execute( 'var = var_zx_mean_'+STRTRIM(e,2)+'_gridobs' )
    
   pltz, var, min=minvar, max=maxvar, int=intvar, boxzoom=[ box[0], box[1], 0, 0 , zbox], /yindex, small=[win, e+1], noerase=(e GT 0), /nocolorbar, /nocontour, NCONTOUR=ncontour, COLNUMB=colnumb, subtitle='', xtitle='', ytitle='', title=mask_title+' '+var_name+' XZ - '+exp_list[e], /REMPLI

  ENDFOR

  @def_plot_cb
  IF write_gif THEN saveimage, plot_dir+'/'+STRUPCASE(data_type)+'/'+zone+'/'+var_name+'/'+fig_name+'.gif'
  IF write_ps  THEN closeps
  IF write_ps EQ 0 AND write_gif EQ 0 THEN STOP


IF n_elements(exp_list) GT 1 THEN BEGIN
;-------------------------------------------------------------------------------------------------
; PLOT SETUP
;-------------------------------------------------------------------------------------------------

  var_plot = 'err_zx_mean'
  print, 'plot_2D_xz: ', var_plot
  key_portrait = 0
  fig_name = 'err'+var_name+'_'+var_plot+'_gridobs_'+data_type+'_'+zone+'_'+period
  IF write_ps THEN openps, filename=plot_dir+'/'+STRUPCASE(data_type)+'/'+zone+'/'+var_name+'/'+fig_name
  lct, 22
  IF var_name EQ 'WQ' OR var_name EQ 'W' THEN fmt = '(F6.3)'
  lct,64
  nb_plot  = n_elements(exp_list)-1
  @def_plot_win
  FOR e = 1, n_elements(exp_list)-1 DO cmd = execute( 'err_zx_mean_'+STRTRIM(e,2)+' =  var_zx_mean_'+STRTRIM(e,2)+'_gridobs - var_zx_mean_0' )
  @def_plot_minmax


;-------------------------------------------------------------------------------------------------
; PLOT XZ MEAN ERROR
;-------------------------------------------------------------------------------------------------

  initncdf, path_0+file_0 & domdef, box, glam=[20,380]
  FOR e = 1, n_elements(exp_list)-1 DO BEGIN

    cmd = execute( 'var = err_zx_mean_'+STRTRIM(e,2) )
    pltz, var, min=minvar, max=maxvar, int=intvar, boxzoom=[ box[0], box[1], 0, 0 , zbox], /yindex, small=[win, e], noerase=(e GT 1), nocolorbar=(nb_plot GT 1), /nocontour, NCONTOUR=ncontour, COLNUMB=colnumb, subtitle='', xtitle='', ytitle='', title=mask_title+' '+var_name+' ERROR XZ - '+exp_list[e], /REMPLI

  ENDFOR

  @def_plot_cb
  IF write_gif THEN saveimage, plot_dir+'/'+STRUPCASE(data_type)+'/'+zone+'/'+var_name+'/'+fig_name+'.gif'
  IF write_ps  THEN closeps
  IF write_ps EQ 0 AND write_gif EQ 0 THEN STOP

ENDIF
