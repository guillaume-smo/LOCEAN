PRO plot_composite_2D
@common


; params
explist = ['COUPLED_SW2_KF','COUPLED_SW2_BMJ']
varlist = ['RAIN'];,'Q2','LH','HFX','OLR','UST','T2','PSFC','GLW','GSW'];'RAINC','RAINNC'
basin   = 'SIO'
freq    = '6H'
datebeg = '19900101'
var_typ = 'WSC' ; wsc / nsc
windbin = [17.5,60]
vdepbin = [0,20]
reskm   = 25. ; resolution radiale du profil en km
distmaxkm = 500. ; rayon maximum
disttc    = '500km'
radius  = findgen(distmaxkm/reskm+1)*reskm
diam    = findgen(distmaxkm/reskm*2+1)*reskm - distmaxkm
write_ps= 1


; definition bin
bin_deb  = 15.
bin_fin  = 65.
bin_size = 10.
bin_unit = '(m/s)'
bin_num  = (bin_fin - bin_deb) / bin_size
bin      = bin_deb + findgen(bin_num)*bin_size
bin_mid  = bin_deb + findgen(bin_num)*bin_size + bin_size/2.



FOR iexp = 0, n_elements(explist)-1 DO BEGIN


; lecture data
expname = explist[iexp] & help, expname
pathin = '/net/adonis/usr/adonis/varclim/gslod/IDL/COLLOCALISATION/EXP_'+expname+'/DATA/' & help, pathin
pathtc = '/net/adonis/usr/adonis/varclim/gslod/IDL/TRACKER/EXP_'+expname+'/DATA/' & help, pathtc
IF expname EQ 'FORCED_SW2_KF'   THEN period  = '1990-1998' ELSE period  = '1990-2009' & help, period
IF expname EQ 'FORCED_SW2_KF'   THEN dateend = '19990101' ELSE dateend = '20100101' & help, dateend
IF expname EQ 'COUPLED_SW2_KF'  THEN restore, pathtc+'d1_TRACKS_'+expname+'_IO_'+period+'.dat', /VERBOSE
IF expname EQ 'COUPLED_SW2_BMJ' THEN restore, pathtc+'d1_TRACKS_NEW_'+expname+'_IO_'+period+'.dat', /VERBOSE


; selection data
;ifin = where(finite(d1_pres) eq 1 AND finite(d1_max_wnd) eq 1) & help, ifin
IF basin EQ 'SIO'  THEN idom1 = where(d1_lon GT 30. AND d1_lon LT 130. AND d1_lat LT 0. AND d1_lat GE -30.)
IF basin EQ 'SWIO' THEN idom1 = where(d1_lon GT 30. AND d1_lon LT 80. AND d1_lat LT 0. AND d1_lat GE -30.)
IF basin EQ 'SEIO' THEN idom1 = where(d1_lon GT 80. AND d1_lon LT 130. AND d1_lat LT 0. AND d1_lat GE -30.)
IF basin EQ 'NIO'  THEN idom1 = where(d1_lon GT 50. AND d1_lon LT 100. AND d1_lat GT 0. AND d1_lat LE 25.)
IF basin EQ 'NWIO' THEN idom1 = where(d1_lon GT 50. AND d1_lon LT 80. AND d1_lat GT 0. AND d1_lat LE 25.)
IF basin EQ 'NEIO' THEN idom1 = where(d1_lon GT 80. AND d1_lon LT 100. AND d1_lat GT 0. AND d1_lat LE 25.)
help, idom1
;iok = intersect(ifin,idom1) & help, iok
iwind1 = where(d1_max_wnd GE windbin[0] AND d1_max_wnd LE windbin[1]) & help, iwind1
iok = intersect(idom1,iwind1) & help, iok
;ivdep1 = where(d1_speed GE vdepbin[0] AND d1_speed LE vdepbin[1]) & help, ivdep1
;iok = intersect(iok,ivdep1) & help, iok
irmw1 = where(d1_rmw_wrf LE 200.) & help, irmw1
iok = intersect(iok,irmw1) & help, iok
ijok = array_indices([(size(d1_lon))[1], (size(d1_lon))[2]], iok, /dim)


FOR ivar = 0, n_elements(varlist)-1 DO BEGIN


  ; lecture data
  var_name = varlist[ivar] & help, var_name
  IF var_name EQ 'RAIN' THEN BEGIN 
    IF expname EQ 'COUPLED_SW2_KF'  THEN restore, pathin + var_name +'C_2D_'+ disttc + '_' + expname +'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat', /VERBOSE
    IF expname EQ 'COUPLED_SW2_BMJ' THEN restore, pathin + var_name +'C_NEW_2D_'+ disttc + '_' + expname +'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat', /VERBOSE
    IF var_typ EQ 'WSC' THEN dxy_var1 = dxy_var_wsc_rot ELSE dxy_var1 = dxy_var_nsc_rot
    IF expname EQ 'COUPLED_SW2_KF'  THEN restore, pathin + var_name +'NC_2D_'+ disttc + '_' + expname +'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat', /VERBOSE
    IF expname EQ 'COUPLED_SW2_BMJ' THEN restore, pathin + var_name +'NC_NEW_2D_'+ disttc + '_' + expname +'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat', /VERBOSE
    IF var_typ EQ 'WSC' THEN dxy_var2 = dxy_var_wsc_rot ELSE dxy_var2 = dxy_var_nsc_rot
    ; bug de 1999
    dxy_var = temporary(dxy_var1) + temporary(dxy_var2)
    ibad    = where(dxy_var GT 500.) & help, ibad
    IF ibad[0] NE -1 THEN dxy_var[ibad] = !values.f_nan
    dxy_var = dxy_var * 4. ; conversion mm/6h -> mm/day
  ENDIF
  help, dxy_var


  ; lecture 10m wind
  restore, pathin + 'UV10TR_2D_'+ disttc + '_' + expname +'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat', /VERBOSE
  help, ux_rot, vy_rot, ut_rot, vr_rot


  ; mise en forme data
  dist_tc = float(dist_tc)
  cpt    = n_elements(iok)
  allvar = fltarr(cpt,n_elements(dxy_var[0,0,*,0]),n_elements(dxy_var[0,0,0,*])) + !values.f_nan
  allux  = fltarr(cpt,n_elements(dxy_var[0,0,*,0]),n_elements(dxy_var[0,0,0,*])) + !values.f_nan
  allvy  = fltarr(cpt,n_elements(dxy_var[0,0,*,0]),n_elements(dxy_var[0,0,0,*])) + !values.f_nan
  allut  = fltarr(cpt,n_elements(dxy_var[0,0,*,0]),n_elements(dxy_var[0,0,0,*])) + !values.f_nan
  allvr  = fltarr(cpt,n_elements(dxy_var[0,0,*,0]),n_elements(dxy_var[0,0,0,*])) + !values.f_nan
  allmaxwind = fltarr(cpt) + !values.f_nan
  allradwind = fltarr(cpt) + !values.f_nan
  allmaxrain = fltarr(cpt) + !values.f_nan
  allradrain = fltarr(cpt) + !values.f_nan
  allat      = fltarr(cpt) + !values.f_nan
  FOR i = 0, cpt-1 DO BEGIN
    allvar[i,*,*] = dxy_var[ijok[0,i],ijok[1,i],*,*]
    allux[i,*,*]  = ux_rot[ijok[0,i],ijok[1,i],*,*]
    allvy[i,*,*]  = vy_rot[ijok[0,i],ijok[1,i],*,*]
    allut[i,*,*]  = ut_rot[ijok[0,i],ijok[1,i],*,*]
    allvr[i,*,*]  = vr_rot[ijok[0,i],ijok[1,i],*,*]
    allat[i]      = d1_lat[ijok[0,i],ijok[1,i]]
;    allmaxwind[i] = d1_max_wnd[ijok[0,i],ijok[1,i]]
    allmaxwind[i] = max(sqrt(allut[i,*,*]^2+allvr[i,*,*]^2))
    allmaxrain[i] = max(allvar[i,*,*], /nan)
    IF allmaxwind[i] NE 0. AND finite(allmaxwind[i]) NE 0 THEN $
      allradwind[i] = dist_tc[where(reform(sqrt(allut[i,*,*]^2+allvr[i,*,*]^2)) EQ allmaxwind[i])]
    IF allmaxrain[i] NE 0. AND finite(allmaxrain[i]) NE 0 THEN $
      allradrain[i] = dist_tc[where(reform(allvar[i,*,*]) EQ allmaxrain[i])]
  ENDFOR


  ; profil radial en moyenne azimuthale pour chaque TC
  allut_radmean = fltarr(n_elements(allvar[*,0,0]),(n_elements(dist_tc[*,0])-1)/2+1)
  allvr_radmean = fltarr(n_elements(allvar[*,0,0]),(n_elements(dist_tc[*,0])-1)/2+1)
  FOR i = 0, (n_elements(dist_tc[*,0])-1)/2 DO BEGIN
    irad = where(dist_tc GE i*reskm AND dist_tc LT (i+1)*reskm)
    FOR j = 0, n_elements(allvar[*,0,0])-1 DO BEGIN
      tmp1 = allut[j,*,*]
      tmp2 = allvr[j,*,*]
      allut_radmean[j,i] = mean(tmp1[irad])
      allvr_radmean[j,i] = mean(tmp2[irad])
    ENDFOR
  ENDFOR
  help, allut_radmean, allvr_radmean


  ; max de vent et rayon de vent max apres moyenne azimuthale
  iradwind = fltarr(cpt) + !values.f_nan
  allmaxwindmean = fltarr(cpt) + !values.f_nan
  allradwindmean = fltarr(cpt) + !values.f_nan
  FOR i = 0, cpt-1 DO BEGIN
    allmaxwindmean[i] = max(sqrt(allut_radmean[i,*]^2+allvr_radmean[i,*]^2))
    IF allmaxwindmean[i] NE 0. AND finite(allmaxwindmean[i]) NE 0 THEN $
      iradwind[i] = where(sqrt(allut_radmean[i,*]^2+allvr_radmean[i,*]^2) EQ allmaxwindmean[i])
    IF iradwind[i] NE -1 AND iradwind[i] NE 0 THEN allradwindmean[i] = radius[iradwind[i]]
    IF iradwind[i] EQ 0 THEN allmaxwindmean[i] = !values.f_nan
  ENDFOR
  help, iradwind, allradwindmean, allmaxwindmean


  ; profil analytique willoughby
  ; rmw = 46.4 * exp(-0.0155*max_allprofil + 0.0169*abs(allat))
  rmw  = allradwindmean
  vmax = allmaxwindmean
  x2 = 25.
;  x1 = 287.60 - 1.9420*vmax + 7.7990*alog(rmw) + 1.8190*abs(allat)
  x1 = 317.1 - 2.026*vmax + 1.915*abs(allat)
;  n  = 2.1340 + 0.0077*vmax - 0.4522*alog(rmw) - 0.0038*abs(allat)
  n  = 0.4067 + 0.0144*vmax - 0.0038*abs(allat)
;  a  = abs(0.5913 + 0.0029*vmax - 0.1361*alog(rmw) - 0.0042*abs(allat))
  a  = 0.0696 + 0.0049*vmax - 0.0064*abs(allat)
  allwil = fltarr(cpt,n_elements(dxy_var[0,0,*,0]),n_elements(dxy_var[0,0,0,*])) + !values.f_nan
  FOR i = 0, cpt-1 DO BEGIN
    tmp = reform(allwil[i,*,*])
    IF vmax[i] NE 0. AND vmax[i] NE -1 AND finite(vmax[i]) EQ 1 AND finite(rmw[i]) EQ 1 AND iok[0] NE -1 THEN BEGIN
      tmp = reform(allwil[i,*,*]) + !values.f_nan
      iok = where(dist_tc LT rmw[i], cntok)
      tmp[iok] = vmax[i] * ((dist_tc[iok]/rmw[i])^n[i])
      iok = where(dist_tc GE rmw[i], cntok)
      tmp[iok] = vmax[i]*((1-a[i])*exp(-(dist_tc[iok]-rmw[i])/x1[i]) + a[i]*exp(-(dist_tc[iok]-rmw[i])/x2))
      allwil[i,*,*] = tmp
    ENDIF
  ENDFOR
  help, allwil


  ; profil analytique tuleya
  ; 1 inch = 25.4 mm
  alltul  = fltarr(cpt,n_elements(dxy_var[0,0,*,0]),n_elements(dxy_var[0,0,0,*])) + !values.f_nan
  a1 = -1.10 ; in/day
  a2 = -1.60 ; in/day
  a3 = 64.5  ; km
  a4 = 150   ; km
  b1 = 3.63  ; in/day
  b2 = 4.24  ; in/day
  b3 = -13.0 ; km
  b4 = -16.0 ; km
  ;  u: normalized maximum wind speed in knots
  ; t0: rain rate at r = 0
  ; tm: maximum rain rate at r = rm
  ; rm: radial extent of the inner-core rain rate
  ; re: radial extent of the tropical system rainfall
  u  = 1. + (allmaxwind * 1.9438 - 35.) / 33.
  t0 = a1 + b1 * u
  tm = a2 + b2 * u
  ;rm = a3 + b3 * u
  rm = allradrain ; rayon de precip max
  re = a4 + b4 * u
  ; r <  rm
  ; inchperday = t0+(tm-t0)*(r/rm)
  ; r >= rm
  ; inchperday = tm*exp(-(r-rm)/re)
  FOR i = 0, cpt-1 DO BEGIN
    iok = where(dist_tc LT rm[i], cntok)
    tmp = reform(alltul[i,*,*])
    IF rm[i] NE 0. AND rm[i] NE -1 AND iok[0] NE -1 THEN tmp[iok] = t0[i]+(tm[i]-t0[i])*(dist_tc[iok]/rm[i])
    iok = where(dist_tc GE rm[i], cntok)
    IF rm[i] NE 0. AND rm[i] NE -1 AND iok[0] NE -1 THEN tmp[iok] = tm[i]*exp(-(dist_tc[iok]-rm[i])/re[i])
    IF rm[i] NE 0. AND rm[i] NE -1 THEN alltul[i,*,*] = tmp
  ENDFOR
  alltul = alltul * 25.4 ; conversion inch/day -> mm/day


; moyenne sur lensemble des TCs
;  allvarmoy = m_mean(allvar, dim = 1, /nan) & help, allvarmoy
;  allvarstd = fltarr(n_elements(dxy_var[0,0,*,0]),n_elements(dxy_var[0,0,0,*]))
;  FOR j= 0, n_elements(dxy_var[0,0,0,*])-1 DO $
;  FOR i= 0, n_elements(dxy_var[0,0,*,0])-1 DO allvarstd[i,j]=stddev(allvar[*,i,j], /nan)


; moyenne par bin de vent
  bin_var = fltarr(bin_num,n_elements(dxy_var[0,0,*,0]),n_elements(dxy_var[0,0,0,*]))
  bin_ux  = fltarr(bin_num,n_elements(dxy_var[0,0,*,0]),n_elements(dxy_var[0,0,0,*]))
  bin_vy  = fltarr(bin_num,n_elements(dxy_var[0,0,*,0]),n_elements(dxy_var[0,0,0,*]))
  bin_wil = fltarr(bin_num,n_elements(dxy_var[0,0,*,0]),n_elements(dxy_var[0,0,0,*]))
  bin_tul = fltarr(bin_num,n_elements(dxy_var[0,0,*,0]),n_elements(dxy_var[0,0,0,*])) 
  nb_bin  = intarr(bin_num)
  FOR i = 0, bin_num-2 DO BEGIN
    ibin = where(allmaxwind GE bin[i] AND allmaxwind LT bin[i+1])
    IF ibin[0] NE -1 THEN BEGIN
      nb_bin[i] = n_elements(ibin)
      bin_var[i,*,*] = m_mean(allvar[ibin,*,*], dim=1, /nan)
      bin_ux[i,*,*]  = m_mean(allux[ibin,*,*], dim=1, /nan)
      bin_vy[i,*,*]  = m_mean(allvy[ibin,*,*], dim=1, /nan)
      bin_wil[i,*,*] = m_mean(allwil[ibin,*,*], dim=1, /nan)
      bin_tul[i,*,*] = m_mean(alltul[ibin,*,*], dim=1, /nan)
    ENDIF ELSE BEGIN & nb_bin[i] = 0 & ENDELSE
  ENDFOR
  print, bin_mid
  print, nb_bin, total(nb_bin)


; moyenne ponderees 
;  iok = where(nb_bin GE 10) & print, iok
  iok = [0,1,2] & print, iok
  binvarmoy = m_mean(bin_var[iok,*,*], dim=1, /nan)
  binuxmoy  = m_mean(bin_ux[iok,*,*], dim=1, /nan)
  binvymoy  = m_mean(bin_vy[iok,*,*], dim=1, /nan)
  binwilmoy = m_mean(bin_wil[iok,*,*], dim=1, /nan)
  bintulmoy = m_mean(bin_tul[iok,*,*], dim=1, /nan)


; projection willoughby
x = diam # replicate(1,41)
y = replicate(1,41)#transpose(diam)
theta = atan(y,x)
ywil = -1*binwilmoy/(sin(theta)^2/cos(theta)+cos(theta))
xwil = -1*ywil*sin(theta)/cos(theta)


; plot 2D
  computegrid,-500,-500,25,25,41,41
;  computegrid,-20,-20,1,1,41,41
  key_onearth = 1
  set_plot, 'X'
  device, retain=0, decomposed=0
  lct,39

  IF write_ps THEN openps, filename='COMP2D_RAIN_'+expname+'.ps'
  plt, binvarmoy, min=0, max=230, lct=22, /nocont, $
  xtickformat='', ytickformat='', xtitle='cross-track distance (km)', ytitle='along-track distance (km)', title='' & $
;  ajoutvect, {u:{a:binuxmoy, g:'T'}, v:{a:binvymoy, g:'T'}}, unvectsur = [3,3], normeref=15, cmref=1 & stop
  IF write_ps THEN closeps & STOP

  IF write_ps THEN openps, filename='COMP2D_RAIN_TUL_'+expname+'.ps'
  plt, bintulmoy, min=0, max=230, lct=22, /nocont, $
  xtickformat='', ytickformat='', xtitle='cross-track distance (km)', ytitle='along-track distance (km)', title='' & $
;  ajoutvect, {u:{a:xwil,g:'T'}, v:{a:ywil, g:'T'}}, unvectsur = [3,3], normeref=15, cmref=1 & stop
  IF write_ps THEN closeps & STOP


;  lct, 12
;  pathfig = '/net/cratos/usr/cratos/varclim/gslod/IDL/COLLOCALISATION/FIGS_'+expname+'/'
;  spawn, 'echo "creation du repertoire '+ pathfig +'"' & spawn, 'mkdir -p '+ pathfig

;  plt, varmoy, vmin, vmax, title='COMPOSITE: '+var_name+' - '+var_typ+' - '+expname, subtitle='min: '+min(varstd)+' - max: '+max(varstd), charsize=1.5
;  xyouts, 0.1, 0.85, 'SAMPLE SIZE: '+strtrim(n_elements(iok),2), align = 0, /normal, color=0, charsize=1.5
;  saveimage, pathfig+'composite_2D_'+var_name+'_'+strtrim(windbin[0],2)+'-'+strtrim(windbin[1],2)+'ms_'+var_typ+'_'+expname+'_'+freq+'_'+basin+'_'+period+'.gif'

;  plt, varstd, title='STD DEV: '+var_name+' - '+var_typ+' - '+expname, subtitle='min: '+min(varstd)+' - max: '+max(varstd), charsize=1.5
; xyouts, 0.1, 0.85, 'SAMPLE SIZE: '+strtrim(n_elements(iok),2), align = 0, /normal, color=0, charsize=1.5
;  saveimage, pathfig+'composite_2D_'+var_name+'std_'+strtrim(windbin[0],2)+'-'+strtrim(windbin[1],2)+'ms_'+var_typ+'_'+expname+'_'+freq+'_'+basin+'_'+period+'.gif'

ENDFOR; var
ENDFOR; exp

STOP
END
