PRO plot_composite_2D_DIDT_UV
@common


; parametres
explist = ['COUPLED_SW2_KF','FORCED_SW2_KF'];'FORCED_SW2_KF','FORCED_SW2_BMJ','COUPLED_SW2_BMJ']
varlist = ['UV10'];,'Q2','LH','HFX','OLR','UST','T2','PSFC','GLW','GSW'];'RAINC','RAINNC'
rayon   = '500' ; 200 ou 500 en km
basin   = 'SIO'
freq    = '6H'
datebeg = '19900101'
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
restore, pathin + 'd1_TRACKS_'+ expname +'_IO_'+datebeg+'-'+dateend+'.dat', /VERBOSE


; calcul index didt
calcul_index_didt24h,d1_pres,d1_max_wnd,ind_int24h,ind_dec24h,d1_wind_smooth_24h,dwinddt,dpresdt,i_int24h,i_dec24h


FOR ivar = 0, n_elements(varlist)-1 DO BEGIN

  var_name = varlist[ivar] & help, var_name
  restore, pathin + var_name +'_2D_'+rayon+'km_' + expname +'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat', /VERBOSE
  dxy_uv10_wsc = dxy_var_wsc_rot
;  restore, pathin + 'U10_2D_'+rayon+'km_' + expname +'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat', /VERBOSE
;  dxy_u10_wsc = dxy_var_wsc_rot
;  restore, pathin + 'V10_2D_'+rayon+'km_' + expname +'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat', /VERBOSE
;  dxy_v10_wsc = dxy_var_wsc_rot

  d1_var = m_mean(m_mean(dxy_uv10_wsc,dim=4,/nan),dim=3,/nan) & help, d1_var
  iokint = ind_int24h[where(finite(d1_var[ind_int24h]) EQ 1)] & help, iokint
  iokdec = ind_dec24h[where(finite(d1_var[ind_dec24h]) EQ 1)] & help, iokdec
  ijokint = array_indices([(size(d1_var))[1], (size(d1_var))[2]], iokint, /dim)
  ijokdec = array_indices([(size(d1_var))[1], (size(d1_var))[2]], iokdec, /dim)

  uv10_int_wsc = fltarr(n_elements(iokint),n_elements(dxy_uv10_wsc[0,0,*,0]),n_elements(dxy_uv10_wsc[0,0,0,*]))
;  u10_int_wsc = fltarr(n_elements(iokint),n_elements(dxy_u10_wsc[0,0,*,0]),n_elements(dxy_u10_wsc[0,0,0,*]))
;  v10_int_wsc = fltarr(n_elements(iokint),n_elements(dxy_v10_wsc[0,0,*,0]),n_elements(dxy_v10_wsc[0,0,0,*]))
  FOR i = 0, n_elements(iokint)-1 DO BEGIN
    uv10_int_wsc[i,*,*] = dxy_uv10_wsc[ijokint[0,i],ijokint[1,i],*,*]
;    u10_int_wsc[i,*,*] = dxy_u10_wsc[ijokint[0,i],ijokint[1,i],*,*]
;    v10_int_wsc[i,*,*] = dxy_v10_wsc[ijokint[0,i],ijokint[1,i],*,*]
  ENDFOR

  uv10_dec_wsc = fltarr(n_elements(iokdec),n_elements(dxy_uv10_wsc[0,0,*,0]),n_elements(dxy_uv10_wsc[0,0,0,*]))
;  u10_dec_wsc = fltarr(n_elements(iokdec),n_elements(dxy_u10_wsc[0,0,*,0]),n_elements(dxy_u10_wsc[0,0,0,*]))
;  v10_dec_wsc = fltarr(n_elements(iokdec),n_elements(dxy_v10_wsc[0,0,*,0]),n_elements(dxy_v10_wsc[0,0,0,*]))
  FOR i = 0, n_elements(iokdec)-1 DO BEGIN
    uv10_dec_wsc[i,*,*] = dxy_uv10_wsc[ijokdec[0,i],ijokdec[1,i],*,*]
;    u10_dec_wsc[i,*,*] = dxy_u10_wsc[ijokdec[0,i],ijokdec[1,i],*,*]
;    v10_dec_wsc[i,*,*] = dxy_v10_wsc[ijokdec[0,i],ijokdec[1,i],*,*]
  ENDFOR

  nb_binint = intarr(bin_num)
  nb_bindec = intarr(bin_num)
  bin_uv10_int_wsc = fltarr(bin_num,n_elements(dxy_uv10_wsc[0,0,*,0]),n_elements(dxy_uv10_wsc[0,0,0,*]))
  bin_uv10_dec_wsc = fltarr(bin_num,n_elements(dxy_uv10_wsc[0,0,*,0]),n_elements(dxy_uv10_wsc[0,0,0,*]))
;  bin_u10_int_wsc = fltarr(bin_num,n_elements(dxy_u10_wsc[0,0,*,0]),n_elements(dxy_u10_wsc[0,0,0,*]))
;  bin_u10_dec_wsc = fltarr(bin_num,n_elements(dxy_u10_wsc[0,0,*,0]),n_elements(dxy_u10_wsc[0,0,0,*]))
;  bin_v10_int_wsc = fltarr(bin_num,n_elements(dxy_v10_wsc[0,0,*,0]),n_elements(dxy_v10_wsc[0,0,0,*]))
;  bin_v10_dec_wsc = fltarr(bin_num,n_elements(dxy_v10_wsc[0,0,*,0]),n_elements(dxy_v10_wsc[0,0,0,*]))
  FOR i = 0, bin_num-2 DO BEGIN
    ibin_int = where(d1_wind_smooth_24h[iokint] GE bin[i] AND d1_wind_smooth_24h[iokint] LT bin[i+1])
    ibin_dec = where(d1_wind_smooth_24h[iokdec] GE bin[i] AND d1_wind_smooth_24h[iokdec] LT bin[i+1])
    IF ibin_int[0] NE -1 THEN nb_binint[i] = n_elements(ibin_int) ELSE nb_binint[i] = 0
    IF ibin_dec[0] NE -1 THEN nb_bindec[i] = n_elements(ibin_dec) ELSE nb_bindec[i] = 0
    bin_uv10_int_wsc[i,*,*] = m_mean(uv10_int_wsc[ibin_int,*,*],dim=1,/nan)
    bin_uv10_dec_wsc[i,*,*] = m_mean(uv10_dec_wsc[ibin_dec,*,*],dim=1,/nan)
;    bin_u10_int_wsc[i,*,*] = m_mean(u10_int_wsc[ibin_int,*,*],dim=1,/nan)
;    bin_u10_dec_wsc[i,*,*] = m_mean(u10_dec_wsc[ibin_dec,*,*],dim=1,/nan)
;    bin_v10_int_wsc[i,*,*] = m_mean(v10_int_wsc[ibin_int,*,*],dim=1,/nan)
;    bin_v10_dec_wsc[i,*,*] = m_mean(v10_dec_wsc[ibin_dec,*,*],dim=1,/nan)
  ENDFOR

  print, nb_binint
  print, nb_bindec
  iok_int = where(nb_binint GE 10) & print, iok_int
  uv10moy_int_wsc = m_mean(bin_uv10_int_wsc[iok_int,*,*], dim = 1, /nan) & help, uv10moy_int_wsc
;  u10moy_int_wsc = m_mean(bin_u10_int_wsc[iok_int,*,*], dim = 1, /nan) & help, u10moy_int_wsc
;  v10moy_int_wsc = m_mean(bin_v10_int_wsc[iok_int,*,*], dim = 1, /nan) & help, v10moy_int_wsc
  iok_dec = where(nb_bindec  GE 10) & print, iok_dec
  uv10moy_dec_wsc = m_mean(bin_uv10_dec_wsc[iok_dec,*,*], dim = 1, /nan) & help, uv10moy_dec_wsc
;  u10moy_dec_wsc = m_mean(bin_u10_dec_wsc[iok_dec,*,*], dim = 1, /nan) & help, u10moy_dec_wsc
;  v10moy_dec_wsc = m_mean(bin_v10_dec_wsc[iok_dec,*,*], dim = 1, /nan) & help, v10moy_dec_wsc


;PLOT
computegrid,-1*(n_elements(dxy_uv10_wsc[0,0,*,0])-1)*25./2.,-1*(n_elements(dxy_uv10_wsc[0,0,*,0])-1)*25./2.,25,25,n_elements(dxy_uv10_wsc[0,0,*,0]),n_elements(dxy_uv10_wsc[0,0,0,*])

pathfig = '/usr/home/gslod/IDL/COMPOSITES/FIGS_COMP_DIDT/'
spawn, 'echo "creation du repertoire '+ pathfig +'"' & spawn, 'mkdir -p '+ pathfig

IF write_ps THEN openps, filename='composite_2D_'+rayon+'km_'+var_name+'_wsc_int_'+expname+'.ps'
ymin = 10 & ymax = 30
lct, 39 & plt, uv10moy_int_wsc, ymin, ymax, xtickformat='', ytickformat='', xtitle='cross-track distance (km)', ytitle='along-track distance (km)', win=0, title=var_name+' WSC - INT - '+expname, subtitle=' '
lct, 45 & plots, CIRCLE(0,0,75), /data, color=255, thick=3
plots, CIRCLE(0,0,250), /data, color=255, thick=3
plots, CIRCLE(0,0,500), /data, color=255, thick=3
;saveimage, pathfig+'composite_2D_'+rayon+'km_'+var_name+'_wsc_int_'+expname+'.gif'
IF write_ps THEN closeps & STOP

IF write_ps THEN openps, filename='composite_2D_'+rayon+'km_'+var_name+'_wsc_dec_'+expname+'.ps'
lct, 39 & plt, uv10moy_dec_wsc, ymin, ymax, xtickformat='', ytickformat='', xtitle='cross-track distance (km)', ytitle='along-track distance (km)', win=1, title=var_name+' WSC - DEC - '+expname, subtitle=' '
IF expname EQ 'COUPLED_SW2_KF' THEN rvm = 106. ELSE rvm = 90.
lct, 45 & plots, CIRCLE(0,0,rvm), /data, color=255, thick=3
plots, CIRCLE(0,0,250), /data, color=255, thick=3
plots, CIRCLE(0,0,500), /data, color=255, thick=3
;saveimage, pathfig+'composite_2D_'+rayon+'km_'+var_name+'_wsc_dec_'+expname+'.gif'
IF write_ps THEN closeps & STOP


;plt, varmoy_int_nsc, xtickformat='', ytickformat='', xtitle='cross-track distance (km)', ytitle='along-track distance (km)', win=2, title=' ', subtitle=' '
;plt, varmoy_dec_nsc, xtickformat='', ytickformat='', xtitle='cross-track distance (km)', ytitle='along-track distance (km)', win=3, title=' ', subtitle=' '


ENDFOR; var
ENDFOR; exp
stop
END
