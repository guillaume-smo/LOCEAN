PRO plot_pdf_1var
 
  datebeg  = '19980101' & dateend = '20100101'
  expname  = 'IBTRACS' & help, expname
  bassin   = ['SIO']
  freq     = '1D'

  pathin  = '/usr/adonis/varclim/gslod/IDL/TRACKER/EXP_'+ expname +'/DATA/'

  var_name = ['ci']
  var_unit = ['()']
  bin_ini  = 0
  bin_size = 5
  bin_num  = 12

;  var_name = ['RMW-ANA']
;  var_unit = ['(km)']
;  bin_ini  = 0
;  bin_size = 50
;  bin_num  = 6
;  var = D1_RMW_ANA & help, var

;  var_name = ['RMW-WRF']
;  var_unit = ['(km)']
;  bin_ini  = 0
;  bin_size = 50
;  bin_num  = 6 
;  var = D1_RMW_WRF & help, d1_var

;  var_name = ['RMW-IBTRACS']
;  var_unit = ['(KM)']
;  bin_ini  = 0
;  bin_size = 50
;  bin_num  = 6
;  var = D1_RMW & help, var

;  var_name = ['WPI+RMW-ANA']
;  var_unit = ['(SI)']
;  bin_ini  = 0
;  bin_size = 1
;  bin_num  = 8 
;  var = (1./3.)*D1_NRJ_V3_ANA^(1./3.) & help, var

;  var_name = ['WPI+RMW-WRF']
;  var_unit = ['(SI)']
;  bin_ini  = 0
;  bin_size = 1
;  bin_num  = 8 
;  var = (1./3.)*D1_NRJ_V3_WRF^(1./3.) & help, var

;  var_name = ['WPI-WRF']
;  var_unit = ['(SI)']
;  bin_ini  = 0
;  bin_size = 1
;  bin_num  = 8 
;  var = (1./3.)*D1_NRJ_V3_WND^(1./3.) & help, var

;  var_name = ['WPI-IBTRACS']
;  var_unit = ['(SI)']
;  bin_ini  = 0
;  bin_size = 1
;  bin_num  = 8
;  var = (1./3.)*D1_NRJ_V3^(1./3.) & help, var


  IF expname EQ 'IBTRACS' THEN BEGIN
  restore, pathin + 'd0_TRACKS_'+expname+'_'+bassin+'_'+datebeg+'-'+dateend+'.dat', /VERBOSE
  restore, pathin + 'd1_TRACKS_'+expname+'_'+bassin+'_'+datebeg+'-'+dateend+'.dat', /VERBOSE  
  ENDIF ELSE BEGIN
  restore, pathin + 'd0_TRACKS_'+expname+'_IO_'+datebeg+'-'+dateend+'.dat', /VERBOSE
  restore, pathin + 'd1_TRACKS_'+expname+'_IO_'+datebeg+'-'+dateend+'.dat', /VERBOSE
  restore, pathin + var_name+'_200km_'+ expname+'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat', /VERBOSE
  ENDELSE

  var = D1_VAR_BEFORE & help, var

  bin_deb = bin_ini+findgen(bin_num)*bin_size
  bin_mid = bin_ini+findgen(bin_num)*bin_size+bin_size/2.
  bin_fin = bin_ini+findgen(bin_num)*bin_size+bin_size
  print, bin_deb
  print, bin_mid
  print, bin_fin


  FOR i = 0, n_elements(bassin)-1 DO BEGIN

    basin = bassin[i] & help, basin

    IF basin EQ 'SIO'  THEN itp = where(d1_lon GT 30. AND d1_lon LT 130. AND d1_lat LT 0.)
    IF basin EQ 'SWIO' THEN itp = where(d1_lon GT 30. AND d1_lon LT 80.  AND d1_lat LT 0.)
    IF basin EQ 'SEIO' THEN itp = where(d1_lon GT 80. AND d1_lon LT 130. AND d1_lat LT 0.)
    IF basin EQ 'NIO'  THEN itp = where(d1_lon GT 50. AND d1_lon LT 100. AND d1_lat GT 0.)
    IF basin EQ 'NWIO' THEN itp = where(d1_lon GT 50. AND d1_lon LT 80.  AND d1_lat GT 0.)
    IF basin EQ 'NEIO' THEN itp = where(d1_lon GT 80. AND d1_lon LT 100. AND d1_lat GT 0.)
    
    d1_histo = var[itp] & help, d1_histo
    histo_var1 = fltarr(n_elements(bin_mid))
    FOR i = 0, n_elements(bin_mid)-1 DO histo_var1[i] = float(n_elements(where(d1_histo GE bin_deb[i] AND d1_histo LT bin_fin[i]))) / float(n_elements(d1_histo))
    histo_var2 = histogram(d1_histo, binsize=bin_size, min=bin_deb[0], max=(reverse(bin_fin))[0]) / float(n_elements(d1_histo))

    ymin = 0 & ymax = ceil(max([histo_var1*100.,histo_var2*100.])/10.)*10.
    bar_plot, histo_var1[0:bin_num-1]*100., yrange=[ymin,ymax], background=255, barnames=bin_mid, title=var_name+' - '+expname+' - '+basin, xtitle='bins '+var_unit, ytitle='percentage', /outline
    xyouts, 0.8, 0.85, 'TOTAL: '+strtrim(round(total(histo_var1, /NAN)*100),2)+'%' , charsize = 1.5, /normal
    xyouts, 0.8, 0.80, 'MEAN: '+strtrim(mean(d1_histo, /NAN),2) , charsize = 1.5, /normal
    saveimage, pathfig + 'HISTO1_'+var_name+'_'+expname+'_'+basin+'.gif'

    bar_plot, histo_var2[0:bin_num-1]*100., yrange=[ymin,ymax], background=255, barnames=bin_mid, title=var_name+' - '+expname+' - '+basin, xtitle='bins '+var_unit, ytitle='percentage', /outline
    xyouts, 0.8, 0.85, 'TOTAL: '+strtrim(round(total(histo_var2, /NAN)*100),2)+'%' , charsize = 1.5, /normal
    xyouts, 0.8, 0.80, 'MEAN: '+strtrim(mean(d1_histo, /NAN),2) , charsize = 1.5, /normal
    saveimage, pathfig + 'HISTO2_'+var_name+'_'+expname+'_'+basin+'.gif'
stop
  ENDFOR

END
