PRO compare_composite_2D_DIDT
@common


explist = ['COUPLED_SW2_KF','FORCED_SW2_KF'];'FORCED_SW2_KF','FORCED_SW2_BMJ','COUPLED_SW2_BMJ']
varlist = ['SST'];,'Q2','LH','HFX','OLR','UST','T2','PSFC','GLW','GSW'];'RAINC','RAINNC'
rayon   = '500'
basin   = 'SIO'
freq    = '6H'
datebeg = '19900101'
wind    = 0

; definition bin
bin_deb  = 15.
bin_fin  = 65.
bin_size = 10.
bin_unit = '(m/s)'
bin_num  = (bin_fin - bin_deb) / bin_size & print, bin_num
bin      = bin_deb + findgen(bin_num)*bin_size
bin_mid  = bin_deb + findgen(bin_num)*bin_size + bin_size/2. & print, bin_mid


FOR iexp = 0, n_elements(explist)-1 DO BEGIN

expname = explist[iexp] & help, expname
IF expname EQ 'FORCED_SW2_KF' THEN period  = '1990-1998' ELSE period  = '1990-2009' & help, period
IF expname EQ 'FORCED_SW2_KF' THEN dateend = '19990101' ELSE dateend = '20100101' & help, dateend
pathin  = '/net/adonis/usr/adonis/varclim/gslod/IDL/COLLOCALISATION/EXP_'+expname+'/DATA/' & help, pathin
print, pathin + 'd1_TRACKS_'+ expname +'_IO_'+datebeg+'-'+dateend+'.dat'
restore, pathin + 'd1_TRACKS_'+ expname +'_IO_'+datebeg+'-'+dateend+'.dat', /VERBOSE

IF wind EQ 1 THEN BEGIN
  restore, pathin + 'U10_2D_'+rayon+'km_' + expname +'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat', /VERBOSE
  u10tmp = dxy_var_wsc_rot
  restore, pathin + 'V10_2D_'+rayon+'km_' + expname +'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat', /VERBOSE
  v10tmp = dxy_var_wsc_rot
ENDIF

; calcul index didt
calcul_index_didt24h,d1_pres,d1_max_wnd,ind_int24h,ind_dec24h,d1_wind_smooth_24h,dwinddt,dpresdt,i_int24h,i_dec24h


FOR ivar = 0, n_elements(varlist)-1 DO BEGIN

  var_name = varlist[ivar] & help, var_name
  print, pathin + var_name +'_2D_'+rayon+'km_' + expname +'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat'
  restore, pathin + var_name +'_2D_'+rayon+'km_' + expname +'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat', /VERBOSE
  dxy_var_wsc = dxy_var_wsc_rot
;  dxy_var_nsc = dxy_var_nsc_rot
  help, dxy_var_wsc;, dxy_var_nsc

  d1_var = m_mean(m_mean(dxy_var_wsc,dim=4,/nan),dim=3,/nan) & help, d1_var

  IF var_name EQ 'Q2' OR var_name EQ 'QS0' OR var_name EQ 'Q2-QS0' THEN dxy_var_wsc = dxy_var_wsc * 1000.

  iokint = ind_int24h[where(finite(d1_var[ind_int24h]) EQ 1)] & help, iokint
  iokdec = ind_dec24h[where(finite(d1_var[ind_dec24h]) EQ 1)] & help, iokdec
  ijokint = array_indices([(size(d1_var))[1], (size(d1_var))[2]], iokint, /dim)
  ijokdec = array_indices([(size(d1_var))[1], (size(d1_var))[2]], iokdec, /dim)

  var_int_wsc = fltarr(n_elements(iokint),n_elements(dxy_var_wsc[0,0,*,0]),n_elements(dxy_var_wsc[0,0,0,*]))
;  var_int_nsc = fltarr(n_elements(iokint),n_elements(dxy_var_nsc[0,0,*,0]),n_elements(dxy_var_nsc[0,0,0,*]))
  IF wind EQ 1 THEN u10_int_wsc = fltarr(n_elements(iokint),n_elements(dxy_var_wsc[0,0,*,0]),n_elements(dxy_var_wsc[0,0,0,*]))
  IF wind EQ 1 THEN v10_int_wsc = fltarr(n_elements(iokint),n_elements(dxy_var_wsc[0,0,*,0]),n_elements(dxy_var_wsc[0,0,0,*]))
  FOR i = 0, n_elements(iokint)-1 DO BEGIN
    var_int_wsc[i,*,*] = dxy_var_wsc[ijokint[0,i],ijokint[1,i],*,*]
;    var_int_nsc[i,*,*] = dxy_var_nsc[ijokint[0,i],ijokint[1,i],*,*]
    IF wind EQ 1 THEN u10_int_wsc[i,*,*] = u10tmp[ijokint[0,i],ijokint[1,i],*,*]
    IF wind EQ 1 THEN v10_int_wsc[i,*,*] = v10tmp[ijokint[0,i],ijokint[1,i],*,*]
  ENDFOR

  var_dec_wsc = fltarr(n_elements(iokdec),n_elements(dxy_var_wsc[0,0,*,0]),n_elements(dxy_var_wsc[0,0,0,*]))
;  var_dec_nsc = fltarr(n_elements(iokdec),n_elements(dxy_var_nsc[0,0,*,0]),n_elements(dxy_var_nsc[0,0,0,*]))
  IF wind EQ 1 THEN u10_dec_wsc = fltarr(n_elements(iokdec),n_elements(dxy_var_wsc[0,0,*,0]),n_elements(dxy_var_wsc[0,0,0,*]))
  IF wind EQ 1 THEN v10_dec_wsc = fltarr(n_elements(iokdec),n_elements(dxy_var_wsc[0,0,*,0]),n_elements(dxy_var_wsc[0,0,0,*]))

  FOR i = 0, n_elements(iokdec)-1 DO BEGIN
    var_dec_wsc[i,*,*] = dxy_var_wsc[ijokdec[0,i],ijokdec[1,i],*,*]
;    var_dec_nsc[i,*,*] = dxy_var_nsc[ijokdec[0,i],ijokdec[1,i],*,*]
    IF wind EQ 1 THEN u10_dec_wsc[i,*,*] = u10tmp[ijokdec[0,i],ijokdec[1,i],*,*]
    IF wind EQ 1 THEN v10_dec_wsc[i,*,*] = v10tmp[ijokdec[0,i],ijokdec[1,i],*,*]
  ENDFOR

  nb_binint = intarr(bin_num)
  nb_bindec = intarr(bin_num)
  bin_var_int_wsc = fltarr(bin_num,n_elements(dxy_var_wsc[0,0,*,0]),n_elements(dxy_var_wsc[0,0,0,*]))
;  bin_var_int_nsc = fltarr(bin_num,n_elements(dxy_var_nsc[0,0,*,0]),n_elements(dxy_var_nsc[0,0,0,*]))
  bin_var_dec_wsc = fltarr(bin_num,n_elements(dxy_var_wsc[0,0,*,0]),n_elements(dxy_var_wsc[0,0,0,*]))
;  bin_var_dec_nsc = fltarr(bin_num,n_elements(dxy_var_nsc[0,0,*,0]),n_elements(dxy_var_nsc[0,0,0,*]))
  IF wind EQ 1 THEN bin_u10_int_wsc = fltarr(bin_num,n_elements(dxy_var_wsc[0,0,*,0]),n_elements(dxy_var_wsc[0,0,0,*]))
  IF wind EQ 1 THEN bin_v10_int_wsc = fltarr(bin_num,n_elements(dxy_var_wsc[0,0,*,0]),n_elements(dxy_var_wsc[0,0,0,*]))
  IF wind EQ 1 THEN bin_u10_dec_wsc = fltarr(bin_num,n_elements(dxy_var_wsc[0,0,*,0]),n_elements(dxy_var_wsc[0,0,0,*]))
  IF wind EQ 1 THEN bin_v10_dec_wsc = fltarr(bin_num,n_elements(dxy_var_wsc[0,0,*,0]),n_elements(dxy_var_wsc[0,0,0,*]))

  FOR i = 0, bin_num-2 DO BEGIN
    ibin_int = where(d1_wind_smooth_24h[iokint] GE bin[i] AND d1_wind_smooth_24h[iokint] LT bin[i+1])
    ibin_dec = where(d1_wind_smooth_24h[iokdec] GE bin[i] AND d1_wind_smooth_24h[iokdec] LT bin[i+1])
    IF ibin_int[0] NE -1 THEN nb_binint[i] = n_elements(ibin_int) ELSE nb_binint[i] = 0
    IF ibin_dec[0] NE -1 THEN nb_bindec[i] = n_elements(ibin_dec) ELSE nb_bindec[i] = 0
    bin_var_int_wsc[i,*,*] = m_mean(var_int_wsc[ibin_int,*,*],dim=1,/nan)
;    bin_var_int_nsc[i,*,*] = m_mean(var_int_nsc[ibin_int,*,*],dim=1,/nan)
    bin_var_dec_wsc[i,*,*] = m_mean(var_dec_wsc[ibin_dec,*,*],dim=1,/nan)
;    bin_var_dec_nsc[i,*,*] = m_mean(var_dec_nsc[ibin_dec,*,*],dim=1,/nan)
    IF wind EQ 1 THEN bin_u10_int_wsc[i,*,*] = m_mean(u10_int_wsc[ibin_int,*,*],dim=1,/nan)
    IF wind EQ 1 THEN bin_v10_int_wsc[i,*,*] = m_mean(v10_int_wsc[ibin_int,*,*],dim=1,/nan)
    IF wind EQ 1 THEN bin_u10_dec_wsc[i,*,*] = m_mean(u10_dec_wsc[ibin_dec,*,*],dim=1,/nan)
    IF wind EQ 1 THEN bin_v10_dec_wsc[i,*,*] = m_mean(v10_dec_wsc[ibin_dec,*,*],dim=1,/nan)
  ENDFOR

  print, nb_binint
  print, nb_bindec
  iok = where(nb_binint GE 10)
  IF expname EQ 'COUPLED_SW2_KF' THEN iokcpl = iok
  varmoy_int_wsc = m_mean(bin_var_int_wsc[iokcpl,*,*], dim = 1, /nan) & help, varmoy_int_wsc
;  varmoy_int_nsc = m_mean(bin_var_int_nsc[iok,*,*], dim = 1, /nan) & help, varmoy_int_nsc
  varmoy_dec_wsc = m_mean(bin_var_dec_wsc[iokcpl,*,*], dim = 1, /nan) & help, varmoy_dec_wsc
;  varmoy_dec_nsc = m_mean(bin_var_dec_nsc[iok,*,*], dim = 1, /nan) & help, varmoy_dec_nsc
  IF wind EQ 1 THEN u10moy_int_wsc = m_mean(bin_u10_int_wsc[iokcpl,*,*], dim = 1, /nan)
  IF wind EQ 1 THEN v10moy_int_wsc = m_mean(bin_v10_int_wsc[iokcpl,*,*], dim = 1, /nan)
  IF wind EQ 1 THEN u10moy_dec_wsc = m_mean(bin_u10_dec_wsc[iokcpl,*,*], dim = 1, /nan)
  IF wind EQ 1 THEN v10moy_dec_wsc = m_mean(bin_v10_dec_wsc[iokcpl,*,*], dim = 1, /nan)

IF expname EQ 'COUPLED_SW2_KF' THEN BEGIN
  bin_var_int_wsc_cpl = bin_var_int_wsc
  bin_var_dec_wsc_cpl = bin_var_dec_wsc
  varmoy_int_wsc_cpl = varmoy_int_wsc
  varmoy_dec_wsc_cpl = varmoy_dec_wsc
;  IF wind EQ 1 THEN u10moy_int_wsc_cpl = 
ENDIF



IF expname EQ 'FORCED_SW2_KF' THEN BEGIN

computegrid,-1*(n_elements(dxy_var_wsc[0,0,*,0])-1)*25./2.,-1*(n_elements(dxy_var_wsc[0,0,*,0])-1)*25./2.,25,25,n_elements(dxy_var_wsc[0,0,*,0]),n_elements(dxy_var_wsc[0,0,0,*])


pathfig = '/usr/home/gslod/IDL/COMPOSITES/FIGS_COMP_DIDT/'
spawn, 'echo "creation du repertoire '+ pathfig +'"' & spawn, 'mkdir -p '+ pathfig

;IF  var_name EQ 'Q2-QS0' THEN BEGIN & ymin = -5 & ymax = -2.5 & ENDIF
plt, varmoy_int_wsc-varmoy_int_wsc_cpl, xtickformat='', ytickformat='', xtitle='cross-track distance (km)', ytitle='along-track distance (km)', win=0, title=var_name+' WSC - INT - CPL-FRC', subtitle=' '
saveimage, pathfig+'composite_2D_'+rayon+'km_'+var_name+'_wsc_int_CPL-FRC.gif'

plt, (varmoy_int_wsc-varmoy_int_wsc_cpl) / varmoy_int_wsc_cpl * 100., xtickformat='', ytickformat='', xtitle='cross-track distance (km)', ytitle='along-track distance (km)', win=0, title=var_name+' WSC - INT - CPL-FRC', subtitle=' '
saveimage, pathfig+'composite_2D_'+rayon+'km_'+var_name+'_wsc_int_CPL-FRC_PERCENT.gif'

;IF  var_name EQ 'Q2-QS0' THEN BEGIN & ymin = -4.2 & ymax = -1.4 & ENDIF
plt, varmoy_dec_wsc-varmoy_dec_wsc_cpl, xtickformat='', ytickformat='', xtitle='cross-track distance (km)', ytitle='along-track distance (km)', win=1, title=var_name+' WSC - DEC - CPL-FRC', subtitle=' '
saveimage, pathfig+'composite_2D_'+rayon+'km_'+var_name+'_wsc_dec_CPL-FRC.gif'

plt, (varmoy_dec_wsc-varmoy_dec_wsc_cpl) / varmoy_dec_wsc_cpl * 100., xtickformat='', ytickformat='', xtitle='cross-track distance (km)', ytitle='along-track distance (km)', win=0, title=var_name+' WSC - DEC - CPL-FRC', subtitle=' '
saveimage, pathfig+'composite_2D_'+rayon+'km_'+var_name+'_wsc_dec_CPL-FRC_PERCENT.gif'

;plt, reform(varmoy_int_nsc[*,*]),xtickformat='', ytickformat='', xtitle='cross-track distance (km)', ytitle='along-track distance (km)', win=2, title=' ', subtitle=' '

;plt, reform(varmoy_dec_nsc[*,*]),xtickformat='', ytickformat='', xtitle='cross-track distance (km)', ytitle='along-track distance (km)', win=3, title=' ', subtitle=' '

ENDIF


ENDFOR; var
ENDFOR; exp
stop
END
