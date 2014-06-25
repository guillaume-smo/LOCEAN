
print, '' & print, 'plot_2D_rain: '+var_plot


;-------------------------------------------------------------------------------------------------
; PLOT SETUP
;-------------------------------------------------------------------------------------------------

IF zone EQ 'TROP' OR zone EQ 'ALL' OR zone EQ 'INDPAC' THEN key_portrait = 0 ELSE key_portrait = 1
IF zone EQ 'TROP' OR zone EQ 'ALL' OR zone EQ 'IO' OR zone EQ 'NIO' OR zone EQ 'IOB' OR zone EQ 'INDPAC' THEN uvs = [10,10] $
ELSE uvs = [5,5]
IF zone EQ 'AIMR' THEN uvs = [3,3]
IF var_name EQ 'SKT' OR var_name EQ 'RAIN' OR var_name EQ 'GSW' THEN clf = 1 ELSE clf = 0
nb_plot = n_elements(exp_list)
edeb = 0
IF STRMATCH( var_plot, 'raine*') OR STRMATCH( var_plot, 'rainp*') THEN BEGIN
  nb_plot = n_elements(exp_list) - 1
  edeb = 1
ENDIF
lct, 22 ; 60
fig_name = var_name+'_'+var_plot+'_'+data_type+'_'+zone+'_'+period
IF write_ps THEN openps, filename=plot_dir+'/'+STRUPCASE(data_type)+'/'+zone+'/'+var_name+'/'+fig_name
help, nb_plot, edeb


;-------------------------------------------------------------------------------------------------
; PLOT 2D
;-------------------------------------------------------------------------------------------------

IF n_elements(exp_list) LE 6 THEN BEGIN

  @def_plot_minmax

  IF var_plot EQ 'var_mean' THEN BEGIN
    IF var_name EQ 'STRESS' THEN fmt='(F7.2)' ELSE fmt='(F6.1)'
    IF var_name EQ 'SST' THEN BEGIN & minvar = 20 & maxvar = 30 & intvar = 0.5 & ENDIF
    IF var_name EQ 'UV10' THEN BEGIN & minvar = 0. & maxvar = 10. & intvar = 0.5 & ENDIF
    IF var_name EQ 'RAIN' THEN BEGIN 
      IF grid EQ 'ind025' THEN BEGIN 
        IF period EQ 'DJFM' THEN BEGIN minvar = 0 & maxvar = 20 & intvar = 2 & lct,15 & ENDIF
        IF period EQ 'JJAS' THEN BEGIN minvar = 0 & maxvar = 30 & intvar = 3 & lct,15 & ENDIF 
      ENDIF ELSE BEGIN minvar = 0 & maxvar = 20 & intvar = 1 & ENDELSE
    ENDIF
  ENDIF

  IF var_plot EQ 'var_ft1d' THEN BEGIN
    IF var_name EQ 'RAIN' THEN BEGIN
      minvar = 0 & intvar = 2
    ENDIF
  ENDIF

  IF var_plot EQ 'stddev_year' THEN BEGIN
    IF var_name EQ 'RAIN' THEN BEGIN
      minvar = 0 & maxvar = 5 & intvar = 0.25
    ENDIF
  ENDIF

  @def_plot_win

  initncdf, path_0+file_0, glam=[20,380], /fullcgrid & domdef, box, /MEMEINDICES
  IF STRMATCH( var_plot, 'rain*period*ratio') THEN initncdf, path_rain+file_rain, glam=[20,380], /fullcgrid & domdef, box, /MEMEINDICES

  cmd = execute( 'var_xy = '+var_plot+'_'+STRTRIM(edeb,2) )
  cmd = execute( 'var_ct = rain_mean_'+STRTRIM(edeb,2) )
  plt, var_xy, min=minvar, max=maxvar, int=intvar, realcont=2, title=var_name+' '+var_plot+' - '+zone+' - '+period+' - '+exp_list[edeb], subtitle='', xtitle='', ytitle='', small=[win, 1], NOCOLORBAR=(nb_plot GT 1), COLNUMB=colnumb, NCONTOUR=ncontour, charsize=1, cell_fill=clf, CONTOUR=var_ct
  IF var_name EQ 'UV10' OR var_name EQ 'UV' THEN ajoutvect, {u:{a:varx_mean_0, g:'T'}, v:{a:vary_mean_0, g:'T'}}, unvectsur=uvs, normeref=12, cmref=1, vectthick=2

  ewin = 0
  FOR e = edeb+1, n_elements(exp_list)-1 DO BEGIN
    ewin = ewin + 1
    IF var_plot EQ 'var_mean' THEN cmd = execute( 'var_xy = '+var_plot+'_'+strtrim(e,2)+'_gridobs' ) $
                              ELSE cmd = execute( 'var_xy = '+var_plot+'_'+strtrim(e,2) )
    cmd = execute( 'var_ct = rain_mean_'+STRTRIM(e,2) )
    plt, var_xy, min=minvar, max=maxvar, int=intvar, realcont=2, title=var_name+' - '+exp_list[e], subtitle='', xtitle='', ytitle='', small=[win, ewin+1], /noerase, /NOCOLORBAR, charsize=1, cell_fill=clf, CONTOUR=var_ct
    cmd = execute( 'xyouts, xy[0], xy[1], "CORR_PAT="+string(corrpat_0'+strtrim(e,2)+'[0], format="(F6.3)"), /DATA, charsize=1, ALIGNMENT=0.5' )
    IF var_name EQ 'UV10' OR var_name EQ 'UV' THEN cmd = execute( 'ajoutvect, {u:{a:varx_mean_'+strtrim(e,2)+'_gridobs, g:"T"}, v:{a:vary_mean_'+strtrim(e,2)+'_gridobs, g:"T"}}, unvectsur=uvs, normeref=12, cmref=1, vectthick=2' )
  ENDFOR

  @def_plot_cb

  IF write_gif THEN saveimage, plot_dir+'/'+STRUPCASE(data_type)+'/'+zone+'/'+var_name+'/'+fig_name+'.gif'
  IF write_ps  THEN closeps
  IF write_ps EQ 0 AND write_gif EQ 0 THEN STOP

ENDIF
