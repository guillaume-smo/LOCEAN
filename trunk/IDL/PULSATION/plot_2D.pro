
print, '' & print, 'plot_2D: '+var_plot


;-------------------------------------------------------------------------------------------------
; PLOT SETUP
;-------------------------------------------------------------------------------------------------

IF zone EQ 'TROP' OR zone EQ 'ALL' OR zone EQ 'INDPAC' THEN key_portrait = 0 ELSE key_portrait = 1
IF zone EQ 'TROP' OR zone EQ 'ALL' OR  $
   zone EQ 'IO' OR zone EQ 'NIO' OR zone EQ 'IOB' OR $
   zone EQ 'INDPAC' OR zone EQ 'AFRASIA' THEN uvs = [10,10] $
ELSE uvs = [5,5]
IF zone EQ 'AIMR' THEN uvs = [3,3]
IF var_name EQ 'PSFC' OR var_name EQ 'SKT' OR var_name EQ 'RAIN' OR $
   var_name EQ 'GSW' OR var_name EQ 'SST' OR var_name EQ 'GRAD_MSLP' OR $
   var_name EQ 'HEIGHT' OR var_name EQ 'LANDUSE' OR $
   STRMATCH( var_name, '*rot*', /FOLD_CASE) THEN clf = 1 ELSE clf = 0
IF STRMATCH( var_plot, '*div*', /FOLD_CASE) OR STRMATCH( var_plot, '*rot*', /FOLD_CASE)  OR STRMATCH( var_plot, '*grad*', /FOLD_CASE) THEN clf = 1
IF STRMATCH( var_name, 'UV*') OR var_name EQ 'STRESS' THEN flag_vec = 1 ELSE flag_vec = 0
IF var_plot EQ 'div' OR var_plot EQ 'rot' OR STRMATCH( var_plot, 'maxcor_*') OR STRMATCH( var_plot, 'lag_*') THEN flag_vec = 0

; NOMBRE DE PLOTS
nb_plot = n_elements(exp_list)
edeb = 0
IF STRMATCH( var_plot, 'raine*') OR STRMATCH( var_plot, 'rainp*') OR STRMATCH( var_plot, 'err*') OR $
  STRMATCH( var_plot, 'maxcor_*') OR STRMATCH( var_plot, 'lag_*') THEN BEGIN
  nb_plot = n_elements(exp_list)-1
  edeb = 1
ENDIF
IF STRMATCH( var_plot, 'diff*') THEN BEGIN
  nb_plot = n_elements(exp_list)-2
  edeb = 2
ENDIF
help, nb_plot, edeb

; PALETTE COULEUR
lct, 22 ; 60
IF STRMATCH(var_name, 'W*') THEN BEGIN
  lct,64
  fmt = '(F6.3)'
ENDIF
IF var_plot EQ 'div' OR var_plot EQ 'rot' OR var_plot EQ 'lag_0' THEN lct, 64

; FIGURE NAME
fig_name = var_name+'_'+(STRSPLIT( var_plot, '_0', /EXTRACT))[0]+'_'+data_type+'_'+zone+'_'+period
IF write_ps THEN openps, filename=plot_dir+'/'+STRUPCASE(data_type)+'/'+zone+'/'+var_name+'/'+fig_name


;-------------------------------------------------------------------------------------------------
; PLOT 2D
;-------------------------------------------------------------------------------------------------

IF n_elements(exp_list) LE 6 THEN BEGIN

  @def_plot_minmax
  @def_plot_win

  initncdf, path_0+file_0, glam=[20,380], /fullcgrid & domdef, box, /MEMEINDICES
  IF STRMATCH( var_plot, 'rain*period*ratio') THEN initncdf, path_rain+file_rain, glam=[20,380], /fullcgrid & domdef, box, /MEMEINDICES
  IF STRMATCH( var_plot, '*_') EQ 0 AND STRMATCH( var_plot, '*_0') EQ 0 THEN var_plot = var_plot + '_'

  cmd = execute( 'var_xy = '+var_plot+STRTRIM(edeb,2) )
  plt, var_xy, min=minvar, max=maxvar, int=intvar, realcont=2, /nocont, title=var_name+' '+var_plot+' - '+zone+' - '+period+' - '+exp_list[edeb], subtitle='', xtitle='', ytitle='', small=[win, 1], NOCOLORBAR=(nb_plot GT 1), COLNUMB=colnumb, NCONTOUR=ncontour, charsize=1, cell_fill=clf, INV=(var_name EQ 'D20' AND STRMATCH( var_name, 'var*'))
  IF flag_vec THEN ajoutvect, {u:{a:varx_mean_0, g:'T'}, v:{a:vary_mean_0, g:'T'}}, unvectsur=uvs, normeref=vref, cmref=1, vectthick=2

  ewin = 0
  FOR e = edeb+1, n_elements(exp_list)-1 DO BEGIN
    ewin = ewin + 1
    IF STRMATCH( var_plot, '???*_mean*') THEN cmd = execute( 'var_xy = '+var_plot+strtrim(e,2)+'_gridobs' ) $
                                         ELSE cmd = execute( 'var_xy = '+var_plot+strtrim(e,2) )
    IF STRMATCH( var_plot, 'vardt_*')    THEN cmd = execute( 'var_xy = '+var_plot+strtrim(e,2) )
    plt, var_xy, min=minvar, max=maxvar, int=intvar, realcont=2, /nocont, title=var_name+' - '+exp_list[e], subtitle='', xtitle='', ytitle='', small=[win, ewin+1], /noerase, /NOCOLORBAR, charsize=1, cell_fill=clf, INV=(var_name EQ 'D20' AND STRMATCH( var_name, 'var*'))
    cmd = execute( 'xyouts, xy[0], xy[1], "CORR_PAT="+string(corrpat_0'+strtrim(e,2)+'[0], format="(F6.3)"), /DATA, charsize=1, ALIGNMENT=0.5' )
    IF flag_vec THEN cmd = execute( 'ajoutvect, {u:{a:varx_mean_'+strtrim(e,2)+'_gridobs, g:"T"}, v:{a:vary_mean_'+strtrim(e,2)+'_gridobs, g:"T"}}, unvectsur=uvs, normeref=vref, cmref=1, vectthick=2' )
  ENDFOR

  @def_plot_cb

  IF write_gif THEN saveimage, plot_dir+'/'+STRUPCASE(data_type)+'/'+zone+'/'+var_name+'/'+fig_name+'.gif'
  IF write_ps  THEN closeps
  IF write_ps EQ 0 AND write_gif EQ 0 THEN STOP

ENDIF
