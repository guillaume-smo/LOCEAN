PRO plot_composite_2D_DIDT
@common


; parametres
explist = ['COUPLED_SW2_KF','FORCED_SW2_KF'];'FORCED_SW2_KF','FORCED_SW2_BMJ','COUPLED_SW2_BMJ']
varlist = ['HFXLH']
rayon   = '500'
basin   = 'SIO'
freq    = '6H'
datebeg = '19900101'
sc      = 'wsc'
write_ps= 1


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


; calcul index didt
calcul_index_didt24h,d1_pres,d1_max_wnd,ind_int24h,ind_dec24h,d1_wind_smooth_24h,dwinddt,dpresdt,i_int24h,i_dec24h


FOR ivar = 0, n_elements(varlist)-1 DO BEGIN

  var_name = varlist[ivar] & help, var_name
  print, pathin + var_name +'_2D_'+rayon+'km_' + expname +'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat'
  restore, pathin + var_name +'_2D_'+rayon+'km_' + expname +'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat', /VERBOSE
  IF sc EQ 'wsc' THEN dxy_var_wsc = dxy_var_wsc_rot ELSE dxy_var_wsc = dxy_var_nsc_rot
  help, dxy_var_wsc

  d1_var = m_mean(m_mean(dxy_var_wsc,dim=4,/nan),dim=3,/nan) & help, d1_var

  IF var_name EQ 'Q2' OR var_name EQ 'QS0' OR var_name EQ 'Q2-QS0' THEN dxy_var_wsc = dxy_var_wsc * 1000.
  IF var_name EQ 'PSFC' THEN dxy_var_wsc = dxy_var_wsc / 100.
  IF var_name EQ 'SST' AND sc EQ 'wsc' THEN dxy_var_wsc = dxy_var_wsc - 273.15
  IF var_name EQ 'T2' AND sc EQ 'wsc' THEN dxy_var_wsc = dxy_var_wsc - 273.15
 
  iokint = ind_int24h[where(finite(d1_var[ind_int24h]) EQ 1)] & help, iokint
  iokdec = ind_dec24h[where(finite(d1_var[ind_dec24h]) EQ 1)] & help, iokdec
  ijokint = array_indices([(size(d1_var))[1], (size(d1_var))[2]], iokint, /dim)
  ijokdec = array_indices([(size(d1_var))[1], (size(d1_var))[2]], iokdec, /dim)

  var_int_wsc = fltarr(n_elements(iokint),n_elements(dxy_var_wsc[0,0,*,0]),n_elements(dxy_var_wsc[0,0,0,*]))
  FOR i = 0, n_elements(iokint)-1 DO BEGIN
    var_int_wsc[i,*,*] = dxy_var_wsc[ijokint[0,i],ijokint[1,i],*,*]
  ENDFOR
; PATCH PRECIP KF
;  indbad = where(var_int_wsc GE 1000.)
;  IF indbad[0] NE -1 THEN BEGIN
;    ijkbad = array_indices(var_int_wsc,indbad)
;    ibad   = ijkbad[0,UNIQ(ijkbad[0,*], SORT(ijkbad[0,*]))]
;    var_int_wsc[ibad,*,*] = !values.f_nan
;  ENDIF

  var_dec_wsc = fltarr(n_elements(iokdec),n_elements(dxy_var_wsc[0,0,*,0]),n_elements(dxy_var_wsc[0,0,0,*]))
  FOR i = 0, n_elements(iokdec)-1 DO BEGIN
    var_dec_wsc[i,*,*] = dxy_var_wsc[ijokdec[0,i],ijokdec[1,i],*,*]
  ENDFOR
; PATCH PRECIP KF
;  indbad = where(var_dec_wsc GE 1000.)
;  IF indbad[0] NE -1 THEN BEGIN
;    ijkbad = array_indices(var_dec_wsc,indbad)
;    ibad   = ijkbad[0,UNIQ(ijkbad[0,*], SORT(ijkbad[0,*]))]
;    var_dec_wsc[ibad,*,*] = !values.f_nan
;  ENDIF

  nb_binint = intarr(bin_num)
  nb_bindec = intarr(bin_num)
  bin_var_int_wsc = fltarr(bin_num,n_elements(dxy_var_wsc[0,0,*,0]),n_elements(dxy_var_wsc[0,0,0,*]))
  bin_var_dec_wsc = fltarr(bin_num,n_elements(dxy_var_wsc[0,0,*,0]),n_elements(dxy_var_wsc[0,0,0,*]))
  FOR i = 0, bin_num-2 DO BEGIN
    ibin_int = where(d1_wind_smooth_24h[iokint] GE bin[i] AND d1_wind_smooth_24h[iokint] LT bin[i+1])
    ibin_dec = where(d1_wind_smooth_24h[iokdec] GE bin[i] AND d1_wind_smooth_24h[iokdec] LT bin[i+1])
    IF ibin_int[0] NE -1 THEN nb_binint[i] = n_elements(ibin_int) ELSE nb_binint[i] = 0
    IF ibin_dec[0] NE -1 THEN nb_bindec[i] = n_elements(ibin_dec) ELSE nb_bindec[i] = 0
    bin_var_int_wsc[i,*,*] = m_mean(var_int_wsc[ibin_int,*,*],dim=1,/nan)
    bin_var_dec_wsc[i,*,*] = m_mean(var_dec_wsc[ibin_dec,*,*],dim=1,/nan)
  ENDFOR

  print, nb_binint
  print, nb_bindec
  iokint = where(nb_binint GE 10) & print, iokint
  iokdec = where(nb_bindec GE 10) & print, iokdec
  varmoy_int_wsc = m_mean(bin_var_int_wsc[iokint,*,*], dim = 1, /nan) & help, varmoy_int_wsc
  varmoy_dec_wsc = m_mean(bin_var_dec_wsc[iokdec,*,*], dim = 1, /nan) & help, varmoy_dec_wsc

  IF iexp EQ 0 THEN varmoy_int_wsc_cpl = varmoy_int_wsc
  IF iexp EQ 0 THEN varmoy_dec_wsc_cpl = varmoy_dec_wsc


;PLOT
computegrid,-1*(n_elements(dxy_var_wsc[0,0,*,0])-1)*25./2.,-1*(n_elements(dxy_var_wsc[0,0,*,0])-1)*25./2.,25,25,n_elements(dxy_var_wsc[0,0,*,0]),n_elements(dxy_var_wsc[0,0,0,*])

pathfig = '/usr/home/gslod/IDL/COMPOSITES/FIGS_COMP_DIDT/'
spawn, 'echo "creation du repertoire '+ pathfig +'"' & spawn, 'mkdir -p '+ pathfig

ymin = min(varmoy_int_wsc) & ymax = max(varmoy_int_wsc)
IF var_name EQ 'CAPE' THEN BEGIN & ymin = 40 & ymax = 340 & ENDIF
IF var_name EQ 'RAINC' THEN BEGIN & ymin = 0 & ymax = 30 & ENDIF
IF var_name EQ 'RAINNC' THEN BEGIN & ymin = 0 & ymax = 90 & ENDIF
IF var_name EQ 'PSFC' THEN BEGIN & ymin = 965 & ymax = 1005 & ENDIF
IF var_name EQ 'Q2-QS0' THEN BEGIN & ymin = -5 & ymax = -2.5 & ENDIF
IF var_name EQ 'THETAE' THEN BEGIN & ymin = 360 & ymax = 372 & ENDIF
IF var_name EQ 'SST' AND sc EQ 'wsc' THEN BEGIN & ymin = 28.5 & ymax = 29.5 & ENDIF
IF var_name EQ 'HFXLH' THEN BEGIN & ymin = 150 & ymax = 850 & ENDIF
IF var_name EQ 'T2' THEN BEGIN & ymin = 27.8 & ymax = 28.8 & ENDIF
IF var_name EQ 'Q2' THEN BEGIN & ymin = 19.7 & ymax = 23.5 & ENDIF
IF var_name EQ 'LH' THEN BEGIN & ymin = 100 & ymax = 500 & ENDIF


IF write_ps THEN openps, filename='composite_2D_'+rayon+'km_'+var_name+'_wsc_int_'+expname+'.ps'
lct, 39 & plt, varmoy_int_wsc, ymin, ymax, xtickformat='', ytickformat='', xtitle='cross-track distance (km)', ytitle='along-track distance (km)', win=0, title=var_name+' WSC - INT - '+expname, subtitle=' '
irvm = where(varmoy_int_wsc EQ max(varmoy_int_wsc))
print, sqrt(glamt[irvm]^2+gphit[irvm]^2)
lct, 45 & plots, CIRCLE(0,0,75), /data, color=255, thick=3
plots, CIRCLE(0,0,250), /data, color=255, thick=3
plots, CIRCLE(0,0,500), /data, color=255, thick=3
;saveimage, pathfig+'composite_2D_'+rayon+'km_'+var_name+'_wsc_int_'+expname+'.gif'
IF write_ps THEN closeps & STOP


ymin = min(varmoy_dec_wsc) & ymax = max(varmoy_dec_wsc)
IF var_name EQ 'CAPE' THEN BEGIN & ymin = 10 & ymax = 330 & ENDIF
IF var_name EQ 'RAINC' THEN BEGIN & ymin = 0 & ymax = 20 & ENDIF
IF var_name EQ 'RAINNC' THEN BEGIN & ymin = 0 & ymax = 75 & ENDIF
IF var_name EQ 'PSFC' THEN BEGIN & ymin = 965 & ymax = 1005 & ENDIF
IF var_name EQ 'THETAE' THEN BEGIN & ymin = 352 & ymax = 367 & ENDIF
IF var_name EQ 'Q2-QS0' THEN BEGIN & ymin = -4.2 & ymax = -1.4 & ENDIF
IF var_name EQ 'SST' AND sc EQ 'wsc' THEN BEGIN & ymin = 26.5 & ymax = 28.5 & ENDIF
IF var_name EQ 'HFXLH' THEN BEGIN & ymin = 50 & ymax = 800 & ENDIF
IF var_name EQ 'T2' THEN BEGIN & ymin = 26 & ymax = 27.7 & ENDIF
IF var_name EQ 'Q2' THEN BEGIN & ymin = 18 & ymax = 22.4 & ENDIF
IF var_name EQ 'LH' THEN BEGIN & ymin = 100 & ymax = 500 & ENDIF


IF write_ps THEN openps, filename='composite_2D_'+rayon+'km_'+var_name+'_wsc_dec_'+expname+'.ps'
lct, 39 & plt, varmoy_dec_wsc, ymin, ymax, xtickformat='', ytickformat='', xtitle='cross-track distance (km)', ytitle='along-track distance (km)', win=1, title=var_name+' WSC - DEC - '+expname, subtitle=' '
IF expname EQ 'COUPLED_SW2_KF' THEN rvm = 106. ELSE rvm = 90.
lct, 45 & plots, CIRCLE(0,0,rvm), /data, color=255, thick=3
plots, CIRCLE(0,0,250), /data, color=255, thick=3
plots, CIRCLE(0,0,500), /data, color=255, thick=3
;saveimage, pathfig+'composite_2D_'+rayon+'km_'+var_name+'_wsc_dec_'+expname+'.gif'
IF write_ps THEN closeps & STOP



;IF iexp EQ 1 THEN BEGIN
;lct, 39 & plt, varmoy_int_wsc_cpl-varmoy_int_wsc, xtickformat='', ytickformat='', xtitle='cross-track distance (km)', ytitle='along-track distance (km)', win=0, title=var_name+' WSC - INT - DIFF', subtitle=' ', charsize=1.5
;lct, 45 & plots, CIRCLE(0,0,75), /data, color=255, thick=3
;plots, CIRCLE(0,0,250), /data, color=255, thick=3
;plots, CIRCLE(0,0,500), /data, color=255, thick=3
;saveimage, pathfig+'composite_2D_'+rayon+'km_'+var_name+'_wsc_int_DIFF.gif'

;lct, 39 & plt, varmoy_dec_wsc_cpl-varmoy_dec_wsc, xtickformat='', ytickformat='', xtitle='cross-track distance (km)', ytitle='along-track distance (km)', win=0, title=var_name+' WSC - DEC - DIFF', subtitle=' ', charsize=1.5
;lct, 45 & plots, CIRCLE(0,0,75), /data, color=255, thick=3
;plots, CIRCLE(0,0,250), /data, color=255, thick=3
;plots, CIRCLE(0,0,500), /data, color=255, thick=3
;saveimage, pathfig+'composite_2D_'+rayon+'km_'+var_name+'_wsc_dec_DIFF.gif'
;ENDIF


ENDFOR; var
ENDFOR; exp
stop
END
