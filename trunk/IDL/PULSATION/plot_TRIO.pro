print, '' & print, 'PLOT_TRIO...'

FOR e = 0, n_elements(exp_list)-1 DO BEGIN

  exp_name = exp_list[e]


  ; MANUAL SELECTION
  ;IF exp_name EQ 'GLORYS' THEN BEGIN
  ;  year_p1std  = [1994,1997,2006]
  ;  year_m1std  = [1998,2005]
  ;  year_p05std = [1994,1997,2002,2006]
  ;  year_m05std = [1996,1998,2005,2008]
  ;ENDIF
  ;IF exp_name EQ 'ind025_cpl12_kf' THEN BEGIN
  ;  year_p1std  = [1997,2002]
  ;  year_m1std  = [2007]
  ;  year_p05std = [1997,2002,2004,2006,2009]
  ;  year_m05std = [1995,1996,1998,1999,2001,2003,2007]
  ;ENDIF
  ;IF exp_name EQ 'ind025_cpl12_bmj' THEN BEGIN
  ;  year_p1std  = [1997,2002,2006,2009]
  ;  year_m1std  = [1996,1998,1999]
  ;  year_p05std = [1997,2002,2006,2009]
  ;  year_m05std = [1993,1996,1998,1999,2000,2003]
  ;ENDIF
  ;cmd = execute( 'ind_p1std  = (listmatch( listyear_'+strtrim(e,2)+', year_p1std))[*,0]' )
  ;cmd = execute( 'ind_m1std  = (listmatch( listyear_'+strtrim(e,2)+', year_m1std))[*,0]' )
  ;cmd = execute( 'ind_p05std = (listmatch( listyear_'+strtrim(e,2)+', year_p05std))[*,0]' )
  ;cmd = execute( 'ind_m05std = (listmatch( listyear_'+strtrim(e,2)+', year_m05std))[*,0]' )


  ; D20 BOX
  box_d20xie = [ 50., 70.,-12., -8.]
  IF box[0] GT box_d20xie[0] OR $
     box[1] LT box_d20xie[1] OR $
     box[2] GT box_d20xie[2] OR $
     box[3] LT box_d20xie[3] THEN STOP

  cmd = execute( 'initncdf, path_'+STRTRIM(e,2)+'+file_'+STRTRIM(e,2)+', glam=[20,380]' ) & domdef, box
  glamt_old = glamt[firstxt:lastxt,firstyt:lastyt]
  gphit_old = gphit[firstxt:lastxt,firstyt:lastyt]
  cmd = execute( 'initncdf, path_'+STRTRIM(e,2)+'+file_'+STRTRIM(e,2)+',XMINMESH=firstxt,YMINMESH=firstyt,XMAXMESH=lastxt,YMAXMESH=lastyt' )
  glamt = glamt_old
  gphit = gphit_old
  domdef, box_d20xie

  cmd = execute( 'var_year = var_year_'+STRTRIM(e,2) )
  cmd = execute( 'anoyear_sc1m_2D1y = anoyear_sc1m_2D1y_'+STRTRIM(e,2) )
  var_year_d20xie = var_year[ firstxt:lastxt, firstyt:lastyt, *]
  anoyear_sc1m_2D1y_d20xie = anoyear_sc1m_2D1y[ firstxt:lastxt, firstyt:lastyt, *]
  help, var_year, var_year_d20xie, anoyear_sc1m_2D1y, anoyear_sc1m_2D1y_d20xie

  cmd = execute( 'nbyear  = nbyear_'+STRTRIM(e,2) )
  cmd = execute( 'nbmonth = nbmonth_'+STRTRIM(e,2) )
  cmd = execute( 'listyear  = listyear_'+STRTRIM(e,2) )

  var_ts1y_d20xie     = FLTARR( nbyear)
  anosc1m_ts1y_d20xie = FLTARR( nbyear)
  ano_ts1y_d20xie     = FLTARR( nbyear)
  FOR y = 0, nbyear-1 DO BEGIN
    var_ts1y_d20xie[y]     = MEAN( var_year_d20xie[*,*,y], /NAN)
    anosc1m_ts1y_d20xie[y] = MEAN( anoyear_sc1m_2D1y_d20xie[*,*,y], /NAN)
  ENDFOR
  FOR y = 0, nbyear-1 DO ano_ts1y_d20xie[y] = var_ts1y_d20xie[y] - MEAN(var_ts1y_d20xie, /NAN)
  help, var_ts1y_d20xie, anosc1m_ts1y_d20xie, ano_ts1y_d20xie

  cmd = execute( 'var_ts1y_d20xie_'+STRTRIM(e,2)+' = var_ts1y_d20xie' )
  cmd = execute( 'ano_ts1y_d20xie_'+STRTRIM(e,2)+' = ano_ts1y_d20xie' )
  cmd = execute( 'anosc1m_ts1y_d20xie_'+STRTRIM(e,2)+' = anosc1m_ts1y_d20xie' )


  ; STD DEV
  cmd = execute( 'ind_p1std  = WHERE( anosc1m_ts1y_d20xie GE  1.  * STDDEV( anosc1m_ts1y_d20xie))')
  cmd = execute( 'ind_m1std  = WHERE( anosc1m_ts1y_d20xie LE -1.  * STDDEV( anosc1m_ts1y_d20xie))')
  cmd = execute( 'ind_p075std = WHERE( anosc1m_ts1y_d20xie GE  0.75 * STDDEV( anosc1m_ts1y_d20xie))')
  cmd = execute( 'ind_m075std = WHERE( anosc1m_ts1y_d20xie LE -0.75 * STDDEV( anosc1m_ts1y_d20xie))')
  cmd = execute( 'ind_p05std = WHERE( anosc1m_ts1y_d20xie GE  0.5 * STDDEV( anosc1m_ts1y_d20xie))')
  cmd = execute( 'ind_m05std = WHERE( anosc1m_ts1y_d20xie LE -0.5 * STDDEV( anosc1m_ts1y_d20xie))')
  cmd = execute( 'ind_p0std  = WHERE( anosc1m_ts1y_d20xie GT  0.  * STDDEV( anosc1m_ts1y_d20xie))')
  cmd = execute( 'ind_m0std  = WHERE( anosc1m_ts1y_d20xie LT -0.  * STDDEV( anosc1m_ts1y_d20xie))')


  ; CHECK
  cmd = execute( 'print, exp_list[e]," >   1*STD: ", listyear_'+strtrim(e,2)+'[ind_p1std]' )
  cmd = execute( 'print, exp_list[e]," <   1*STD: ", listyear_'+strtrim(e,2)+'[ind_m1std]' )
  cmd = execute( 'print, exp_list[e]," > 0.75*STD: ", listyear_'+strtrim(e,2)+'[ind_p075std]' )
  cmd = execute( 'print, exp_list[e]," < 0.75*STD: ", listyear_'+strtrim(e,2)+'[ind_m075std]' )
  cmd = execute( 'print, exp_list[e]," > 0.5*STD: ", listyear_'+strtrim(e,2)+'[ind_p05std]' )
  cmd = execute( 'print, exp_list[e]," < 0.5*STD: ", listyear_'+strtrim(e,2)+'[ind_m05std]' )
  cmd = execute( 'print, exp_list[e]," >   0*STD: ", listyear_'+strtrim(e,2)+'[ind_p0std]' )
  cmd = execute( 'print, exp_list[e]," <   0*STD: ", listyear_'+strtrim(e,2)+'[ind_m0std]' )


  ; COMPOSITES CYCLONES 075STD
  cmd = execute( 'yearcg = yearcg_'+STRTRIM(e,2) )
  cmd = execute( 'yeartc = yeartc_'+STRTRIM(e,2) )
  cmd = execute( 'listyear = listyear_'+STRTRIM(e,2) )
  indcg_p075std = !NULL
  indcg_m075std = !NULL
  FOR y = 0, n_elements( ind_p075std)-1 DO indcg_p075std = [ indcg_p075std, WHERE( seasoncg EQ FLOAT( listyear[ind_p075std[y]]), /NULL) ]
  FOR y = 0, n_elements( ind_m075std)-1 DO indcg_m075std = [ indcg_m075std, WHERE( seasoncg EQ FLOAT( listyear[ind_m075std[y]]), /NULL) ]
  help, ind_p075std, ind_m075std
  help, indcg_p075std, indcg_m075std

  cgd_p075std = HIST_2D( loncg[indcg_p075std,0], latcg[indcg_p075std,0], bin1=bin_size, min1=box[0], max1=box[1], bin2=bin_size, min2=box[2], max2=box[3]) / float(nbyear_obs-1)
  tcd_p075std = HIST_2D( loncg[indcg_p075std,*], latcg[indcg_p075std,*], bin1=bin_size, min1=box[0], max1=box[1], bin2=bin_size, min2=box[2], max2=box[3]) / (float(nbyear_obs-1) * 4.)
  cgd_m075std = HIST_2D( loncg[indcg_m075std,0], latcg[indcg_m075std,0], bin1=bin_size, min1=box[0], max1=box[1], bin2=bin_size, min2=box[2], max2=box[3]) / float(nbyear_obs-1)
  tcd_m075std = HIST_2D( loncg[indcg_m075std,*], latcg[indcg_m075std,*], bin1=bin_size, min1=box[0], max1=box[1], bin2=bin_size, min2=box[2], max2=box[3]) / (float(nbyear_obs-1) * 4.)

  computegrid,box[0], box[2], bin_size, bin_size, (box[1]-box[0]) / bin_size+1, (box[3]-box[2]) / bin_size+1
  plt, cgd_p075std-cgd_m075std, -0.25, 0.25, /realcont, /nocont, lct=64, xtitle='', ytitle='', subtitle='CG/YEAR',title='IBTRACS P075STD-M075STD CG DENSITY' & STOP
  plt, tcd_p075std-tcd_m075std, -0.5, 0.5, /realcont, /nocont, lct=64, xtitle='', ytitle='', subtitle='CG/YEAR',title='IBTRACS P075STD-M075STD TC DENSITY' & STOP


  ; COMPOSITES CYCLONES 075STD
  indcg_p05std = !NULL
  indcg_m05std = !NULL
  FOR y = 0, n_elements( ind_p05std)-1 DO indcg_p05std = [ indcg_p05std, WHERE( seasoncg EQ FLOAT( listyear[ind_p05std[y]]), /NULL) ]
  FOR y = 0, n_elements( ind_m05std)-1 DO indcg_m05std = [ indcg_m05std, WHERE( seasoncg EQ FLOAT( listyear[ind_m05std[y]]), /NULL) ]
  help, ind_p05std, ind_m05std
  help, indcg_p05std, indcg_m05std

  cgd_p05std = HIST_2D( loncg[indcg_p05std,0], latcg[indcg_p05std,0], bin1=bin_size, min1=box[0], max1=box[1], bin2=bin_size, min2=box[2], max2=box[3]) / float(nbyear_obs-1)
  tcd_p05std = HIST_2D( loncg[indcg_p05std,*], latcg[indcg_p05std,*], bin1=bin_size, min1=box[0], max1=box[1], bin2=bin_size, min2=box[2], max2=box[3]) / (float(nbyear_obs-1) * 4.)
  cgd_m05std = HIST_2D( loncg[indcg_m05std,0], latcg[indcg_m05std,0], bin1=bin_size, min1=box[0], max1=box[1], bin2=bin_size, min2=box[2], max2=box[3]) / float(nbyear_obs-1)
  tcd_m05std = HIST_2D( loncg[indcg_m05std,*], latcg[indcg_m05std,*], bin1=bin_size, min1=box[0], max1=box[1], bin2=bin_size, min2=box[2], max2=box[3]) / (float(nbyear_obs-1) * 4.)

  computegrid,box[0], box[2], bin_size, bin_size, (box[1]-box[0]) / bin_size+1, (box[3]-box[2]) / bin_size+1
  plt, cgd_p05std-cgd_m05std, -0.25, 0.25, /realcont, /nocont, lct=64, xtitle='', ytitle='', subtitle='CG/YEAR',title='IBTRACS P05STD-M05STD CG DENSITY' & STOP
  plt, tcd_p05std-tcd_m05std, -0.5, 0.5, /realcont, /nocont, lct=64, xtitle='', ytitle='', subtitle='CG/YEAR',title='IBTRACS P05STD-M05STD TC DENSITY' & STOP


  ; CYC BOX
  box_cycxie = [ 50., 70.,-22.,-12.]
  IF box[0] GT box_cycxie[0] OR $
     box[1] LT box_cycxie[1] OR $
     box[2] GT box_cycxie[2] OR $
     box[3] LT box_cycxie[3] THEN STOP

  cmd = execute( 'nbmonthcg = nbmonthcg_'+STRTRIM(e,2) )
  cmd = execute( 'nbmonthtc = nbmonthtc_'+STRTRIM(e,2) )
  cmd = execute( 'loncg     = loncg_'+STRTRIM(e,2) )
  cmd = execute( 'latcg     = latcg_'+STRTRIM(e,2) )
  cmd = execute( 'lontc     = lontc_'+STRTRIM(e,2) )
  cmd = execute( 'lattc     = lattc_'+STRTRIM(e,2) )
  help, nbmonthcg, loncg, latcg
  help, nbmonthtc, lontc, lattc

  indtc = WHERE( lontc GE box_cycxie[0] AND lontc LE box_cycxie[1] AND lattc GE box_cycxie[2] AND lattc LE box_cycxie[3])
  ijtc  = ARRAY_INDICES( lontc, indtc)
  itc   = REFORM( ijtc[0,*])
  itc   = itc[ UNIQ( itc, SORT( itc))]
  nbmonthtc = nbmonthtc[ itc, *]
  help, nbmonthtc

  indcg = WHERE( loncg GE box_cycxie[0] AND loncg LE box_cycxie[1] AND latcg GE box_cycxie[2] AND latcg LE box_cycxie[3])
  ijcg  = ARRAY_INDICES( loncg, indcg)
  icg   = REFORM( ijcg[0,*])  
  icg   = icg[ UNIQ( icg, SORT( icg))]
  nbmonthcg = nbmonthcg[ icg, *]
  help, nbmonthcg

  @calcul_TC_stats

  nbcg_1y_cycxie = nbcg_1y
  nbct_1y_cycxie = nbct_1y
  nbtcday_1y_cycxie = nbtcday_1y
  nbtgday_1y_cycxie = nbtgday_1y
  ano_nbcg_1y_cycxie = ano_nbcg_1y
  ano_nbct_1y_cycxie = ano_nbct_1y
  ano_nbtcday_1y_cycxie = ano_nbtcday_1y
  ano_nbtgday_1y_cycxie = ano_nbtgday_1y

  cmd = execute( 'nbcg_1y_cycxie_'+STRTRIM(e,2)+' = nbcg_1y_cycxie' )
  cmd = execute( 'nbtcday_1y_cycxie_'+STRTRIM(e,2)+' = nbtcday_1y_cycxie' )



	; CORRELATIONS
  iokcg = WHERE( FINITE(nbcg_1y_cycxie) EQ 1, /NULL)
  iokct = WHERE( FINITE(nbct_1y_cycxie) EQ 1, /NULL)
  ioktc = WHERE( FINITE(nbtcday_1y_cycxie) EQ 1, /NULL)
  ioktg = WHERE( FINITE(nbtgday_1y_cycxie) EQ 1, /NULL)

  print, ''
  print, C_CORRELATE( nbcg_1y_cycxie[iokcg], nbtgday_1y_cycxie[iokcg], [-1,0,1])
  print, C_CORRELATE( nbct_1y_cycxie[iokct], nbtcday_1y_cycxie[iokct], [-1,0,1])
  print, ''
  print, C_CORRELATE( var_ts1y_d20xie[iokcg], ano_nbcg_1y_cycxie[iokcg], [-1,0,1])
  print, C_CORRELATE( anosc1m_ts1y_d20xie[iokcg], ano_nbcg_1y_cycxie[iokcg], [-1,0,1])
  print, ''
  print, C_CORRELATE( var_ts1y_d20xie[ioktc], ano_nbtcday_1y_cycxie[ioktc], [-1,0,1])
  print, C_CORRELATE( anosc1m_ts1y_d20xie[ioktc], ano_nbtcday_1y_cycxie[ioktc], [-1,0,1])
  print, ''
  print, anosc1m_ts1y_d20xie[ind_p1std], ano_nbtcday_1y_cycxie[ind_p1std], ano_nbcg_1y_cycxie[ind_p1std]
  print, ''
  print, anosc1m_ts1y_d20xie[ind_m1std], ano_nbtcday_1y_cycxie[ind_m1std], ano_nbcg_1y_cycxie[ind_m1std]
STOP

  ; DIAGS
  IF n_elements(ind_p1std)  GT 1 THEN cmd = execute( 'var_p1std_'+strtrim(e,2)+'  = MEAN( var_year_'+strtrim(e,2)+'[*,*,ind_p1std], DIMENSION=3, /NAN)' )
  IF n_elements(ind_m1std)  GT 1 THEN cmd = execute( 'var_m1std_'+strtrim(e,2)+'  = MEAN( var_year_'+strtrim(e,2)+'[*,*,ind_m1std], DIMENSION=3, /NAN)' )
  IF n_elements(ind_p05std) GT 1 THEN cmd = execute( 'var_p05std_'+strtrim(e,2)+' = MEAN( var_year_'+strtrim(e,2)+'[*,*,ind_p05std], DIMENSION=3, /NAN)' )
  IF n_elements(ind_m05std) GT 1 THEN cmd = execute( 'var_m05std_'+strtrim(e,2)+' = MEAN( var_year_'+strtrim(e,2)+'[*,*,ind_m05std], DIMENSION=3, /NAN)' )

  IF n_elements(ind_p1std)  EQ 1 THEN cmd = execute( 'var_p1std_'+strtrim(e,2)+'  = var_year_'+strtrim(e,2)+'[*,*,ind_p1std]' )
  IF n_elements(ind_m1std)  EQ 1 THEN cmd = execute( 'var_m1std_'+strtrim(e,2)+'  = var_year_'+strtrim(e,2)+'[*,*,ind_m1std]' )
  IF n_elements(ind_p05std) EQ 1 THEN cmd = execute( 'var_p05std_'+strtrim(e,2)+'  = var_year_'+strtrim(e,2)+'[*,*,ind_p05std]' )
  IF n_elements(ind_m05std) EQ 1 THEN cmd = execute( 'var_m05std_'+strtrim(e,2)+'  = var_year_'+strtrim(e,2)+'[*,*,ind_m05std]' )

  cmd = execute( 'ano_p1std_'+strtrim(e,2)+'  = var_p1std_'+strtrim(e,2)+'  - var_mean_'+strtrim(e,2) )
  cmd = execute( 'ano_m1std_'+strtrim(e,2)+'  = var_m1std_'+strtrim(e,2)+'  - var_mean_'+strtrim(e,2) )
  cmd = execute( 'ano_p05std_'+strtrim(e,2)+' = var_p05std_'+strtrim(e,2)+' - var_mean_'+strtrim(e,2) )
  cmd = execute( 'ano_m05std_'+strtrim(e,2)+' = var_m05std_'+strtrim(e,2)+' - var_mean_'+strtrim(e,2) )

  cmd = execute( 'diff_p1std_m1std_'+strtrim(e,2)+'   = var_p1std_' +strtrim(e,2)+'  - var_m1std_' +strtrim(e,2) )
  cmd = execute( 'diff_p05std_m05std_'+strtrim(e,2)+' = var_p05std_'+strtrim(e,2)+'  - var_m05std_'+strtrim(e,2) )

ENDFOR

zone_old = zone
zone = 'TRIO'

;var_plot = 'var_ts1y_d20xie'
;@plot_1D_t

;var_plot = 'anosc1m_ts1y_d20xie'
;@plot_1D_t

;var_plot = 'nbcg_1y_cycxie'
;@plot_1D_t

;var_plot = 'nbtcday_1y_cycxie'
;@plot_1D_t

;var_plot = 'var_p1std'
;@plot_2D

;var_plot = 'var_m1std'
;@plot_2D

;var_plot = 'var_p05std'
;@plot_2D

;var_plot = 'var_m05std'
;@plot_2D

;var_plot = 'diff_p1std_m1std'
;@plot_2D

;var_plot = 'diff_p05std_m05std'
;@plot_2D

print, 'PLOT_STD OK' & print, ''
