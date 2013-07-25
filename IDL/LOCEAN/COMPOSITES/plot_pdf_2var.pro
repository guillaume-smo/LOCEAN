PRO plot_pdf_2var
; plots les scatter refroidissement versus vent / PDI par bassin
  @common

  expname    = 'OBS' & help, expname
  basin      = 'SIO' & help, basin  
  datebeg_tc = '19980101' & dateend_tc = '20100101'
  datebeg_sf = '19980101' & dateend_sf = '20100101'
  period     = '1998-2009'
  freq       = '1D'

  IF expname EQ 'OBS' THEN BEGIN
    path_tc  = '/usr/adonis/varclim/gslod/IDL/TRACKER/EXP_IBTRACS/DATA/' 
    path_var = '/usr/adonis/varclim/gslod/IDL/COLLOCALISATION/EXP_TMI-AMSRE/DATA/'
  ENDIF

  IF expname EQ 'OBS' THEN BEGIN
    restore, path_tc  + 'd1_IBTRACSv03r03_WPI_IO_'+period+'.dat', /VERBOSE
    restore, path_var + 'SST_1D_200km_TMI-AMSRE_'+freq+'_IO_'+datebeg_sf+'-'+dateend_sf+'.dat', /VERBOSE
  ENDIF
  STOP

  var1_name = ['WPI']
  var1_unit = ['(SI)']
  bin1_deb  = 0.
  bin1_size = 0.2
  bin1_num  = 30.
  var1_bin  = bin1_deb+findgen(bin1_num)*bin1_size

;  var1_name = ['WIND']
;  var1_unit = ['(m/s)']
;  bin1_deb  = 17.5
;  bin1_size = 5.
;  bin1_num  = 10.
;  var1_bin  = bin1_deb+findgen(bin1_num)*bin1_size

  var2_name = ['SST']
  var2_unit = ['(degC)']
  bin2_deb  = -4.
  bin2_size = 0.2
  bin2_num  = 26.
  var2_bin  = bin2_deb+findgen(bin2_num)*bin2_size

;  var2_name = ['RMW_WRF']
;  var2_unit = ['(km)']
;  bin2_deb  = 0.
;  bin2_size = 25.
;  bin2_num  = 16.
;  var2_bin  = bin2_deb+findgen(bin2_num)*bin2_size

;  var2_name = ['WPI_ANA']
;  var2_unit = ['(SI)']
;  bin2_deb  = 0.
;  bin2_size = 0.2
;  bin2_num  = 30.
;  var2_bin  = bin1_deb+findgen(bin1_num)*bin1_size

  

  IF basin EQ 'SIO'  THEN itp = where(d1_lon GT 30. AND d1_lon LT 130. AND d1_lat LT 0.)
  IF basin EQ 'SWIO' THEN itp = where(d1_lon GT 30. AND d1_lon LT 80.  AND d1_lat LT 0.)
  IF basin EQ 'SEIO' THEN itp = where(d1_lon GT 80. AND d1_lon LT 130. AND d1_lat LT 0.)
  IF basin EQ 'NIO'  THEN itp = where(d1_lon GT 50. AND d1_lon LT 100. AND d1_lat GT 0.)
  IF basin EQ 'NWIO' THEN itp = where(d1_lon GT 40. AND d1_lon LT  75. AND d1_lat GT 0.)
  IF basin EQ 'NEIO' THEN itp = where(d1_lon GT 75. AND d1_lon LT 100. AND d1_lat GT 0.)

  IF var1_name EQ 'WPI' THEN var1_delta = (1./3.)*d1_nrj_v3[itp]^(1./3.)
  IF var1_name EQ 'WPI_ANA' THEN var1_delta = (1./3.)*d1_nrj_v3_ANA[itp]^(1./3.)
  IF var1_name EQ 'WPI_WRF' THEN var1_delta = (1./3.)*d1_nrj_v3_WRF[itp]^(1./3.)
  IF var1_name EQ 'WPI_WND' THEN var1_delta = (1./3.)*d1_nrj_v3_WND[itp]^(1./3.)
  IF var1_name EQ 'WIND'    THEN var1_delta = d1_max_wnd[itp]
  IF var2_name EQ 'SST'     THEN var2_delta = D1_VAR_ANO_MEAN_BEF[itp]
  IF var2_name EQ 'RMW_WRF' THEN var2_delta = D1_RMW_WRF[itp]
  IF var2_name EQ 'RMW_ANA' THEN var2_delta = D1_RMW_ANA[itp]
  IF var2_name EQ 'WPI_ANA' THEN var2_delta = (1./3.)*d1_nrj_v3_ANA[itp]^(1./3.)
  IF var2_name EQ 'WPI_WRF' THEN var2_delta = (1./3.)*d1_nrj_v3_WRF[itp]^(1./3.)
  IF var2_name EQ 'WPI_WND' THEN var2_delta = (1./3.)*d1_nrj_v3_WND[itp]^(1./3.)
 
  help, var1_delta, var2_delta
  
;  indok = where(d1_rmw_wrf LE 100.)
;  var1_delta = var1_delta[indok]
;  var2_delta = var2_delta[indok]
;  help, var1_delta, var2_delta

  ; construit densite  
  xbndplot=[min(var1_bin), max(var1_bin)] 
  ybndplot = [min(var2_bin), max(var2_bin)]
  computegrid,min(var1_bin),min(var2_bin),abs(var1_bin[1]-var1_bin[0]),abs(var2_bin[1]-var2_bin[0]),n_elements(var1_bin),n_elements(var2_bin)

  densite = fltarr(n_elements(var1_bin), n_elements(var2_bin))
  FOR ibin1 = 0, n_elements(var1_bin)-1 DO BEGIN
    index1 = where(var1_delta GT var1_bin[ibin1]-bin1_size/2. AND var1_delta LE var1_bin[ibin1]+bin1_size/2.) 
    IF index1[0] NE -1 THEN densite[ibin1, *]= histogram(var2_delta(index1), binsize = bin2_size, location = hist_axis, min = min(var2_bin)-bin2_size/2, max = max(var2_bin)-bin2_size/2)
  ENDFOR

  var1_finite = var1_delta[where(finite(var1_delta) EQ 1 AND finite(var2_delta) EQ 1)]
  var2_finite = var2_delta[where(finite(var1_delta) EQ 1 AND finite(var2_delta) EQ 1)]
  fit_a_b = LINFIT(var1_finite, var2_finite, YFIT=yf_all,chisqr=chi2_all) ;,PROB=chi2_sig)
  moy = Hist_2D_moment_bin(var1_finite, var2_finite,glamt[*,0],tmoment='moy',n2keep=4)
  tmp_x = (glamt[*,0])[where(finite(glamt[*,0]) EQ 1 AND finite(moy[0,*]) EQ 1)]
  tmp_y = (moy[0,*])[where(finite(glamt[*,0]) EQ 1 AND finite(moy[0,*]) EQ 1)]
          
  ; fit lineaire
  fit_a = regress( tmp_x, tmp_y, YFIT=yf,chisq=chi2,CORRELATION=rr,ftest=ftest,const=fit_b) ;,PROB=chi2_sig)
;  t_lev_sig=t_test_correl(abs(rr),n_elements(tmp_y))


          tmp=alog10(densite)
          tmp[where(finite(tmp) EQ 0)]=0
          lct,15 
          tmp=smooth(tmp,2,/nan)
;          window,0,xsize=800,ysize=800

 
         plt, tmp, 0, 3, /inv, /nocontour, xrange=[bin1_deb,bin1_deb+(bin1_num-1)*bin1_size] ,yrange=[bin2_deb,bin2_deb+(bin2_num-1)*bin2_size], $ ;, /rempli
               xtitle = var1_name+' '+var1_unit, $
               ytitle = var2_name+' '+var2_unit, $
               title = ' PDF('+var1_name+' vs '+var2_name+'): '+basin,$
               charsize = 1.2,charthick = 2, $
               subtitle = '', /port, $
               ytickformat = '', xtickformat = '';, small = [1, 2, 1]
         lct, 50
         oplot, glamt[*, 0], moy[0, *], thick = 4, color = 80
         lct,64
         oplot, glamt[*,0],fit_b[0]+fit_a[0]*glamt[*,0],thick = 4,color=0
;         oplot, [0,6], [0,6],thick = 4,color=0
         saveimage, pathfig + 'pdf_'+var1_name+'-'+var2_name+'_'+expname+'_'+freq+'_'+basin+'.gif'

END
