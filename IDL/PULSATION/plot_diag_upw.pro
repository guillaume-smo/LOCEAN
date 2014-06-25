lon = glamt[firstxt:lastxt,firstyt:lastyt]
lat = gphit[firstxt:lastxt,firstyt:lastyt]

FOR e = 0, n_elements(exp_list)-1 DO BEGIN

  IF e EQ 0 THEN BEGIN
    IF var_name EQ 'UV10' OR var_name EQ 'STRESS' THEN BEGIN
      cmd = execute( 'varx_mean = varx_mean_'+strtrim(e,2) )
      cmd = execute( 'vary_mean = vary_mean_'+strtrim(e,2) )
    ENDIF
    cmd = execute( 'var_mean = var_mean_'+strtrim(e,2) )
  ENDIF ELSE BEGIN
    IF var_name EQ 'UV10' OR var_name EQ 'STRESS' THEN BEGIN
      cmd = execute( 'varx_mean = varx_mean_'+strtrim(e,2)+'_gridobs' )
      cmd = execute( 'vary_mean = vary_mean_'+strtrim(e,2)+'_gridobs' )
    ENDIF
    cmd = execute( 'var_mean = var_mean_'+strtrim(e,2)+'_gridobs' )
  ENDELSE

  ; COTE INCLINEE DE +30DEG
  var_meanrot = ROT(var_mean, -30, 1., MISSING=!VALUES.F_NAN, /INTERP)
  cmd = execute( 'var_meanrot_'+strtrim(e,2)+' = var_meanrot')

  ; PROFIL MOYEN CROSSSHORE
  var_meanrot_1D = MEAN(var_meanrot, DIMENSION=2, /NAN)
  cmd = execute( 'var_meanrot1D_'+strtrim(e,2)+' = var_meanrot_1D')

  IF var_name EQ 'UV10' THEN BEGIN

    ; COMPOSANTE PERPENDICULAIRE
    ;varx_meanper  = varx_mean * cos(330*!dtor)
    ;vary_meanper  = -1. * vary_mean * sin(330*!dtor)
    ;uvary_meanper = SQRT(varx_meanper^2 + vary_meanper^2)

    ; COMPOSANTE //
    varx_meanpar = varx_mean * sin(30*!dtor)
    vary_meanpar = vary_mean * cos(30*!dtor)
    var_meanpar  = SQRT(varx_meanpar^2 + vary_meanpar^2)

    ; PROFIL DE VENT MAX // A LA COTE
    var_meanparrot = ROT(var_meanpar, -30, 1., MISSING=!VALUES.F_NAN, /INTERP)
    cmd = execute( 'var_meanparrot_'+strtrim(e,2)+' = var_meanparrot' )
    cmd = execute( 'var_meanparrot1D_'+strtrim(e,2)+' = MEAN(var_meanparrot, DIMENSION=2, /NAN)')

    ; GRADIENT DU PROFIL DE VENT MAX // A LA COTE
    ;grad = var_meanparrot[icoast:iend,ijmax[1]] - SHIFT(var_meanparrot[icoast:iend,ijmax[1]],-1)

  ENDIF

ENDFOR


; PLOTS

IF var_name EQ 'UV10' THEN BEGIN  

  indmax = WHERE(var_meanparrot_0 EQ MAX(var_meanparrot_0, /NAN))
  ijmax  = ARRAY_INDICES(var_meanparrot_0, indmax)
  icoast = ( REVERSE( WHERE( FINITE( var_meanparrot_0[*,ijmax[1]]) NE 1)))[0]
  iend   = n_elements(var_meanparrot_0[*,ijmax[1]])-1

  minvar = MIN(var_meanparrot_0[icoast:iend,ijmax[1]], /NAN) & maxvar = MAX(var_meanparrot_0[icoast:iend,ijmax[1]], /NAN)
  FOR e = 1, n_elements(exp_list) -1 DO BEGIN
    cmd = execute( 'var_plot = var_meanparrot_'+strtrim(e,2) )
    minvar = MIN([minvar,MIN(var_plot[icoast:iend,ijmax[1]],/NAN)], /NAN)
    maxvar = MAX([maxvar,MAX(var_plot[icoast:iend,ijmax[1]], /NAN)], /NAN)
  ENDFOR
  maxvar = maxvar + 0.05*(maxvar-minvar)
  minvar = minvar - 0.05*(maxvar-minvar)
  intvar = (maxvar-minvar)/20. & lct, 60 & fmt='(F6.1)'

  fig_name = 'V10_CROSSSHORE_c1m_'+zone+'_'+period
  IF write_ps THEN openps, filename=plot_dir+'/'+STRUPCASE(data_type)+'/'+zone+'/'+var_name+'/'+fig_name
  IF write_ps THEN thc = 6 ELSE thc = 2
  IF write_ps THEN chs = 1 ELSE chs = 1.5
  color_list = [0, 50, 250, 150, 200, 100, 25]

  indmax = WHERE(var_meanparrot_0 EQ MAX(var_meanparrot_0, /NAN))
  ijmax  = ARRAY_INDICES(var_meanparrot_0, indmax)
  icoast = ( REVERSE( WHERE( FINITE( var_meanparrot_0[*,ijmax[1]]) NE 1)))[0]
  iend   = n_elements(var_meanparrot_0[*,ijmax[1]])-1

  splot, var_meanparrot_0[icoast:iend,ijmax[1]], xtitle='DISTANCE FROM THE COAST (km)', title='CROSS-SHORCROSS- PROFILE - '+period, charsize=1.5, ystyle=1, yrange=[minvar,maxvar], ytitle='UV10 (m/s)', lct=39, charthick=1.5, XTicklen=1, YTicklen=1, XGridStyle=1, YGridStyle=1, /nodata

  FOR e = 0, n_elements(exp_list)-1 DO BEGIN

    cmd = execute( 'var_plot = var_meanparrot_'+strtrim(e,2) )
    indmax = WHERE(var_plot EQ MAX(var_plot, /NAN))
;    ijmax  = ARRAY_INDICES(var_plot, indmax)
;    icoast = ( REVERSE( WHERE( FINITE( var_plot[*,ijmax[1]]) NE 1)))[0]
;    iend   = n_elements(var_plot[*,ijmax[1]])-1
    lonmax = lon[indmax]
    latmax = lat[indmax]

    oplot, var_plot[icoast:iend,ijmax[1]], thick=thc, color=color_list[e]
    cmd = execute( 'oplot, var_meanparrot1D_'+strtrim(e,2)+'[icoast:iend], thick=thc, color=color_list[e], line=2' )
    xyouts, 0.08, 0.175-e*0.025, exp_list[e]+' (MAX @ '+string(lonmax, format='(F5.2)')+'E '+string(latmax, format='(F5.2)')+'N)', /NORMAL, charsize=chs, color=color_list[e], charthick=1

  ENDFOR

  IF write_ps THEN closeps ELSE STOP

ENDIF ELSE BEGIN

  icoast = ( WHERE( FINITE( var_meanrot_0[*,0]) EQ 1))[0]
  iend   = n_elements(var_meanrot1D_0)-1

  minvar = MIN(var_meanrot1D_0[icoast:iend], /NAN) & maxvar = MAX(var_meanrot1D_0[icoast:iend], /NAN)
  FOR e = 1, n_elements(exp_list) -1 DO BEGIN
    cmd = execute( 'var_plot = var_meanrot1D_'+strtrim(e,2)+'[icoast:iend]' )
    minvar = MIN([minvar,var_plot], /NAN)
    maxvar = MAX([maxvar,var_plot], /NAN)
  ENDFOR
  maxvar = maxvar + 0.05*(maxvar-minvar)
  minvar = minvar - 0.05*(maxvar-minvar)
  intvar = (maxvar-minvar)/20. & lct, 60 & fmt='(F6.1)'

  fig_name = var_name+'_CROSSSHORE_c1m_'+zone+'_'+period
  IF write_ps THEN openps, filename=plot_dir+'/'+STRUPCASE(data_type)+'/'+zone+'/'+var_name+'/'+fig_name
  IF write_ps THEN thc = 6 ELSE thc = 2
  IF write_ps THEN chs = 1 ELSE chs = 1.5
  color_list = [0, 50, 250, 150, 200, 100, 25]

  splot, var_meanrot1D_0[icoast:iend], xtitle='DISTANCE FROM THE COAST (km)', title='CROSS-SHORE '+var_name+' PROFILE - '+period, charsize=1.5, ystyle=1, yrange=[minvar,maxvar], ytitle=var_name, lct=39, charthick=1.5, XTicklen=1, YTicklen=1, XGridStyle=1, YGridStyle=1, /nodata
  
  FOR e = 0, n_elements(exp_list)-1 DO BEGIN

    cmd = execute( 'var_plot = var_meanrot1D_'+strtrim(e,2) )
;    cmd = execute( 'icoast = ( WHERE( FINITE( var_meanrot_'+strtrim(e,2)+'[*,0]) EQ 1))[0]' )
;    iend   = n_elements(var_plot)-1
    
    oplot, var_plot[icoast:iend], thick=thc, color=color_list[e]
    xyouts, 0.08, 0.175-e*0.025, exp_list[e], /NORMAL, charsize=chs, color=color_list[e], charthick=1

  ENDFOR

  IF write_ps THEN closeps ELSE STOP


  ; ERRORS
  FOR e = 1, n_elements(exp_list)-1 DO cmd = execute( 'error_meanrot1D_'+strtrim(e,2)+' = var_meanrot1D_'+strtrim(e,2)+' - var_meanrot1D_0' )

  minvar = MIN(error_meanrot1D_1[icoast:iend], /NAN) & maxvar = MAX(error_meanrot1D_1[icoast:iend], /NAN)
  FOR e = 2, n_elements(exp_list) -1 DO BEGIN
    cmd = execute( 'var_plot = error_meanrot1D_'+strtrim(e,2)+'[icoast:iend]' )
    minvar = MIN([minvar,var_plot], /NAN)
    maxvar = MAX([maxvar,var_plot], /NAN)
  ENDFOR
  maxvar = maxvar + 0.05*(maxvar-minvar)
  minvar = minvar - 0.05*(maxvar-minvar)
  intvar = (maxvar-minvar)/20. & lct, 60 & fmt='(F6.1)'

  fig_name = var_name+'_ERROR_CROSSSHORE_c1m_'+zone+'_'+period
  IF write_ps THEN openps, filename=plot_dir+'/'+STRUPCASE(data_type)+'/'+zone+'/'+var_name+'/'+fig_name
  IF write_ps THEN thc = 6 ELSE thc = 2
  IF write_ps THEN chs = 1 ELSE chs = 1.5
  color_list = [0, 50, 250, 150, 200, 100, 25]

  splot, error_meanrot1D_1[icoast:iend], xtitle='DISTANCE FROM THE COAST (km)', title='CROSS-SHORE '+var_name+' ERROR PROFILE - '+period, charsize=1.5, ystyle=1, yrange=[minvar,maxvar], ytitle=var_name+' ERROR', lct=39, charthick=1.5, XTicklen=1, YTicklen=1, XGridStyle=1, YGridStyle=1, /nodata

  FOR e = 1, n_elements(exp_list)-1 DO BEGIN

    cmd = execute( 'var_plot = error_meanrot1D_'+strtrim(e,2) )
    oplot, var_plot[icoast:iend], thick=thc, color=color_list[e]
    xyouts, 0.08, 0.175-e*0.025, exp_list[e], /NORMAL, charsize=chs, color=color_list[e], charthick=1

  ENDFOR

  IF write_ps THEN closeps ELSE STOP


ENDELSE
