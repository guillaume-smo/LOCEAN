PRO compute_mean_rain_radial_profile
@common


; params
explist = ['COUPLED_SW2_KF','COUPLED_SW2_BMJ']
varlist = ['RAIN']
basin   = 'SIO'
freq    = '6H'
datebeg = '19900101'
var_typ = 'WSC'
disttc  = '500km'
windbin = [17.5,60]
vdepbin = [0,20]
reskm   = 25. ; resolution radiale du profil en km
distmaxkm = 500. ; rayon maximum
radius  = findgen(distmaxkm/reskm+1)*reskm
write_ps= 0


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
pathin  = '/net/adonis/usr/adonis/varclim/gslod/IDL/COLLOCALISATION/EXP_'+expname+'/DATA/' & help, pathin
IF expname EQ 'FORCED_SW2_KF' THEN period  = '1990-1998' ELSE period  = '1990-2009' & help, period
IF expname EQ 'FORCED_SW2_KF' THEN dateend = '19990101' ELSE dateend = '20100101' & help, dateend


; lecture data
IF expname EQ 'IBTRACS' THEN BEGIN 
  @read_ibtracs.pro
  d1_lon = loncyc
  d1_lat = latcyc
  d1_max_wnd = uv10cyc
  d1_speed = vdepcyc
ENDIF ELSE BEGIN
  pathtc = '/net/adonis/usr/adonis/varclim/gslod/IDL/TRACKER/EXP_'+expname+'/DATA/' & help, pathtc
  IF expname EQ 'COUPLED_SW2_KF' THEN  restore, pathtc+'d1_TRACKS_'+expname+'_IO_'+period+'.dat', /VERBOSE
  IF expname EQ 'COUPLED_SW2_BMJ' THEN restore, pathtc+'d1_TRACKS_NEW_'+expname+'_IO_'+period+'.dat', /VERBOSE
ENDELSE


; definition des points valides
;ifin = where(finite(d1_pres) eq 1 AND finite(d1_max_wnd) eq 1) & help, ifin
IF basin EQ 'SIO'  THEN idom1 = where(d1_lon GT 30. AND d1_lon LT 130. AND d1_lat LT 0. AND d1_lat GE -30.)
IF basin EQ 'SWIO' THEN idom1 = where(d1_lon GT 30. AND d1_lon LT 80. AND d1_lat LT 0. AND d1_lat GE -30.)
IF basin EQ 'SEIO' THEN idom1 = where(d1_lon GT 80. AND d1_lon LT 130. AND d1_lat LT 0. AND d1_lat GE -30.)
IF basin EQ 'NIO'  THEN idom1 = where(d1_lon GT 50. AND d1_lon LT 100. AND d1_lat GT 0. AND d1_lat LE 25.)
IF basin EQ 'NWIO' THEN idom1 = where(d1_lon GT 50. AND d1_lon LT 80. AND d1_lat GT 0. AND d1_lat LE 25.)
IF basin EQ 'NEIO' THEN idom1 = where(d1_lon GT 80. AND d1_lon LT 100. AND d1_lat GT 0. AND d1_lat LE 25.)
help, idom1
;iok1 = intersect(ifin,idom1) & help, iok1
iwind1 = where(d1_max_wnd GE windbin[0] AND d1_max_wnd LE windbin[1]) & help, iwind1
iok1 = intersect(idom1,iwind1) & help, iok1
;ivdep1 = where(d1_speed GE vdepbin[0] AND d1_speed LE vdepbin[1]) & help, ivdep1
;iok1 = intersect(iok1,ivdep1) & help, iok1
irmw1 = where(d1_rmw_wrf LE 200.) & help, irmw1
iok1 = intersect(iok1,irmw1) & help, iok1
ijok1 = array_indices([(size(d1_lon))[1], (size(d1_lon))[2]], iok1, /dim)


FOR ivar = 0, n_elements(varlist)-1 DO BEGIN

  var_name = varlist[ivar] & help, var_name

  IF expname NE 'IBTRACS' THEN BEGIN


    ; lecture data
    IF expname EQ 'COUPLED_SW2_KF' THEN restore, pathin + var_name +'C_2D_'+ disttc + '_' + expname +'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat', /VERBOSE
    IF expname EQ 'COUPLED_SW2_BMJ' THEN restore, pathin + var_name +'C_NEW_2D_'+ disttc + '_' + expname +'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat', /VERBOSE
    IF var_typ EQ 'WSC' THEN dxy_var1 = dxy_var_wsc_rot ELSE dxy_var1 = dxy_var_nsc_rot
    IF expname EQ 'COUPLED_SW2_KF' THEN restore, pathin + var_name +'NC_2D_'+ disttc + '_' + expname +'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat', /VERBOSE
    IF expname EQ 'COUPLED_SW2_BMJ' THEN restore, pathin + var_name +'NC_NEW_2D_'+ disttc + '_' + expname +'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat', /VERBOSE
    IF var_typ EQ 'WSC' THEN dxy_var2 = dxy_var_wsc_rot ELSE dxy_var2 = dxy_var_nsc_rot
    ; bug de 1999
    ibad = where(dxy_var1 GT 100.)
    IF ibad[0] NE -1 THEN dxy_var1[ibad] = !values.f_nan
    IF ibad[0] NE -1 THEN dxy_var2[ibad] = !values.f_nan 
    help, dxy_var1, dxy_var2


    ; mise en forme data
    allrmw = fltarr(n_elements(iok1))
    allat = fltarr(n_elements(iok1))
    allmaxwind = fltarr(n_elements(iok1))
    tmp1 = fltarr(n_elements(iok1),n_elements(dxy_var1[0,0,*,0]),n_elements(dxy_var1[0,0,0,*]))
    tmp2 = fltarr(n_elements(iok1),n_elements(dxy_var2[0,0,*,0]),n_elements(dxy_var2[0,0,0,*]))
    FOR i = 0, n_elements(iok1)-1 DO BEGIN
      tmp1[i,*,*] = dxy_var1[ijok1[0,i],ijok1[1,i],*,*]
      tmp2[i,*,*] = dxy_var2[ijok1[0,i],ijok1[1,i],*,*]
      allat[i] = d1_lat[ijok1[0,i],ijok1[1,i]]
      allrmw[i] = d1_rmw_wrf[ijok1[0,i],ijok1[1,i]]
      allmaxwind[i] = d1_max_wnd[ijok1[0,i],ijok1[1,i]]
    ENDFOR
    dxy_var1 = temporary(tmp1)
    dxy_var2 = temporary(tmp2)
    help, dxy_var1, dxy_var2, allat, allrmw


    ; profil radial en moyenne azimuthale pour chaque TC
    allprofil1 = fltarr(n_elements(dxy_var1[*,0,0]),(n_elements(dist_tc[*,0])-1)/2+1)
    allprofil2 = fltarr(n_elements(dxy_var2[*,0,0]),(n_elements(dist_tc[*,0])-1)/2+1)
    FOR i = 0, (n_elements(dist_tc[*,0])-1)/2 DO BEGIN
      irad = where(dist_tc GE i*reskm AND dist_tc LT (i+1)*reskm)
      FOR j = 0, n_elements(dxy_var1[*,0,0])-1 DO BEGIN
        tmp1 = dxy_var1[j,*,*]
        tmp2 = dxy_var2[j,*,*]
        allprofil1[j,i] = mean(tmp1[irad])
        allprofil2[j,i] = mean(tmp2[irad])
      ENDFOR
    ENDFOR
    help, allprofil1, allprofil2
    IF var_name EQ 'RAIN' THEN allprofil1 = allprofil1 * 4. ; conversion mm/6h -> mm/day
    IF var_name EQ 'RAIN' THEN allprofil2 = allprofil2 * 4. ; conversion mm/6h -> mm/day
    IF var_name EQ 'RAIN' THEN allprofil  = allprofil1 + allprofil2


    ; max de precip et rayon associe pour chaque profil
    max_allprofil  = fltarr(n_elements(iok1)) + !values.f_nan
    rmax_allprofil = fltarr(n_elements(iok1)) + !values.f_nan
    FOR i = 0, n_elements(iok1)-1 DO BEGIN
      IF max(allprofil[i,*]) NE 0. AND finite(max(allprofil[i,*])) NE 0 THEN BEGIN
        max_allprofil[i] = max(allprofil[i,*])
        irmax = where(allprofil[i,*] EQ max_allprofil[i])
        IF radius[irmax] NE 0. THEN rmax_allprofil[i] = radius[irmax]
      ENDIF ELSE BEGIN
        max_allprofil[i] = !values.f_nan
        rmax_allprofil[i] = !values.f_nan
      ENDELSE
    ENDFOR

 
    ; profil moyen par bin de vent
    bin_var  = fltarr(bin_num, n_elements(allprofil[0,*]))
    bin_var1 = fltarr(bin_num, n_elements(allprofil[0,*]))
    bin_var2 = fltarr(bin_num, n_elements(allprofil[0,*]))
    bin_rmax = fltarr(bin_num)
    nb_bin   = intarr(bin_num)
    FOR i = 0, bin_num-2 DO BEGIN
      ibin = where(allmaxwind GE bin[i] AND allmaxwind LT bin[i+1])
      IF ibin[0] NE -1 THEN BEGIN 
        nb_bin[i]     = n_elements(ibin)
        bin_var[i,*]  = m_mean(allprofil[ibin,*], dim=1, /nan)
        bin_var1[i,*] = m_mean(allprofil1[ibin,*], dim=1, /nan)
        bin_var2[i,*] = m_mean(allprofil2[ibin,*], dim=1, /nan)
        bin_rmax[i]   = m_mean(rmax_allprofil[ibin], dim=1, /nan)
      ENDIF ELSE BEGIN & nb_bin[i] = 0 & ENDELSE
    ENDFOR
    print, bin_mid
    print, nb_bin, total(nb_bin)
;    iok = where(nb_bin GE 10) & print, iok
    iok = [0,1,2]
    profil  = m_mean(bin_var[iok,*], dim=1, /nan) & help, profil
    profil1 = m_mean(bin_var1[iok,*], dim=1, /nan) & help, profil1
    profil2 = m_mean(bin_var2[iok,*], dim=1, /nan) & help, profil2
    rmax    = m_mean(bin_rmax[iok], dim=1, /nan) & help, rmax
  ENDIF


; profil analytique tuleya

; constantes
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
rm1 = a3 + b3 * u
rm2 = rmax_allprofil ; rayon de precip max en moyenne azimuthale
re = a4 + b4 * u

;r <  rm
;inchperday = t0+(tm-t0)*(r/rm)
;r >= rm
;inchperday = tm*exp(-(r-rm)/re)

  allprofilana1 = fltarr(n_elements(iok1),n_elements(radius))
  allprofilana2 = fltarr(n_elements(iok1),n_elements(radius))
  FOR i = 0, n_elements(iok1)-1 DO BEGIN
    allprofilana1[i,where(radius LT rm1[i])] = t0[i]+(tm[i]-t0[i])*(radius[where(radius LT rm1[i])]/rm1[i])
    allprofilana1[i,where(radius GE rm1[i])] = tm[i]*exp(-(radius[where(radius GE rm1[i])]-rm1[i])/re[i])
;    allprofilana1[i,where(radius EQ 0.)]     = 0. 
    IF rm2[i] NE 0. AND finite(rm2[i]) NE 0 THEN BEGIN
      allprofilana2[i,where(radius LT rm2[i])] = t0[i]+(tm[i]-t0[i])*(radius[where(radius LT rm2[i])]/rm2[i])
      allprofilana2[i,where(radius GE rm2[i])] = tm[i]*exp(-(radius[where(radius GE rm2[i])]-rm2[i])/re[i])
;      allprofilana2[i,where(radius EQ 0.)]     = 0.
    ENDIF ELSE BEGIN & allprofilana2[i,*] = !values.f_nan & ENDELSE
  ENDFOR
  allprofilana1 = 25.4 * allprofilana1 ; conversion inch/day -> mm/day
  allprofilana2 = 25.4 * allprofilana2 ; conversion inch/day -> mm/day


  ; profil analytique moyen par bin de vent
  bin_var_ana1  = fltarr(bin_num, n_elements(allprofilana1[0,*]))
  bin_var_ana2  = fltarr(bin_num, n_elements(allprofilana2[0,*]))
  bin_rmax_ana1 = fltarr(bin_num)
  bin_rmax_ana2 = fltarr(bin_num)
  nb_bin_ana    = intarr(bin_num)
  FOR i = 0, bin_num-2 DO BEGIN
    ibin = where(allmaxwind GE bin[i] AND allmaxwind LT bin[i+1])
    IF ibin[0] NE -1 THEN BEGIN 
      nb_bin_ana[i]     = n_elements(ibin)
      bin_var_ana1[i,*] = m_mean(allprofilana1[ibin,*], dim=1, /nan)
      bin_var_ana2[i,*] = m_mean(allprofilana2[ibin,*], dim=1, /nan)
      bin_rmax_ana1[i]  = m_mean(rm1[ibin], dim=1, /nan)
      bin_rmax_ana2[i]  = m_mean(rm2[ibin], dim=1, /nan)
    ENDIF ELSE BEGIN & nb_bin_ana[i] = 0 & ENDELSE
  ENDFOR
  print, bin_mid
  print, nb_bin_ana, total(nb_bin_ana)
;  iok = where(nb_bin_ana GE 10) & print, iok
  iok = [0,1,2]
  profil_ana1 = m_mean(bin_var_ana1[iok,*], dim=1, /nan) & help, profil_ana1
  profil_ana2 = m_mean(bin_var_ana2[iok,*], dim=1, /nan) & help, profil_ana2
  rmax_ana1   = m_mean(bin_rmax_ana1[iok], dim=1, /nan) & help, rmax_ana1
  rmax_ana2   = m_mean(bin_rmax_ana2[iok], dim=1, /nan) & help, rmax_ana2


; sauvegarde profils
  IF expname EQ 'COUPLED_SW2_KF' THEN BEGIN
    profil_kf = profil   ; real profile
    profil1_kf = profil1 ; convective
    profil2_kf = profil2 ; explicit
    rmax_kf    = rmax    ; max rain radius
    profil_ana1_kf = profil_ana1 ; willoughby profile + analytic rain radius
    profil_ana2_kf = profil_ana2 ; willoughby profile + real rain radius
    rmax_ana1_kf   = rmax_ana1
    rmax_ana2_kf   = rmax_ana2
  ENDIF

  IF expname EQ 'COUPLED_SW2_BMJ' THEN BEGIN
    profil_bmj = profil
    profil1_bmj = profil1
    profil2_bmj = profil2
    profil_ana1_bmj = profil_ana1
    profil_ana2_bmj = profil_ana2
    rmax_ana1_bmj   = rmax_ana1
    rmax_ana2_bmj   = rmax_ana2
  ENDIF

ENDFOR; var
ENDFOR; exp


; plots 1D
  set_plot, 'X'
  device, retain=0, decomposed=0
  lct,39

  radius2 = findgen(distmaxkm/(reskm/2.)+1)*reskm/2.

  IF write_ps THEN thc = 6 ELSE thc = 2
  IF write_ps THEN openps, filename='COMP1D_RADIAL_RAIN.ps'

  splot, radius2, smooth(interpol(profil_kf,n_elements(radius2)),3), thick=thc, color=0, yrange=[0,210.], xtitle='RADIUS (km)', ytitle='RAIN (mm/day)' & $

  oplot, radius2, smooth(interpol(profil_kf,n_elements(radius2)),3), thick=thc, color=60 & $
;  oplot, radius2, smooth(profil_ana1_kf,3), thick=thc, line=2, color=195 & $
  oplot, radius2, smooth(interpol(profil_ana2_kf,n_elements(radius2)),3), thick=thc, color=60, line=2 & $
  oplot, radius2, smooth(interpol(profil_bmj,n_elements(radius2)),3), thick=thc, color=250 & $
;  oplot, radius2, smooth(profil_ana1_bmj,3), thick=thc, line=2, color=150 & $
  oplot, radius2, smooth(interpol(profil_ana2_bmj,n_elements(radius2)),3), thick=thc, color=250, line=2 & $
  xyouts, 0.15, 0.15 , exp_list[0], /normal, color=0  , charsize=1.5 & $
  xyouts, 0.15, 0.125, exp_list[1], /normal, color=50 , charsize=1.5 & $
  xyouts, 0.15, 0.1  , exp_list[2], /normal, color=225, charsize=1.5

  IF write_ps THEN closeps & STOP
  IF write_ps THEN openps, filename='COMP1D_RADIAL_RAIN_RATIO.ps'

  splot, radius, smooth(profil1_kf/profil_kf,3)*100., thick=thc, color=0, yrange=[0,100] & $
  oplot, radius, smooth(profil1_kf/profil_kf,3)*100., thick=thc, color=60 & $
  oplot, radius, smooth(profil2_kf/profil_kf,3)*100., thick=thc, line=2, color=60 & $
  oplot, radius, smooth(profil1_bmj/profil_bmj,3)*100., thick=thc, color=250 & $
  oplot, radius, smooth(profil2_bmj/profil_bmj,3)*100., thick=thc, line=2, color=250

  IF write_ps THEN closeps & STOP


  IF iexp EQ 0 THEN window, 0 ELSE wset, 0 
  IF iexp EQ 0 THEN plot, radius, bin_var[0,*], thick=thc
  IF iexp EQ 1 THEN oplot, radius, profil, thick=thc, line=1
  IF iexp EQ 2 THEN oplot, radius, profil, thick=thc, line=2

  IF iexp EQ 0 THEN window, 1 ELSE wset, 1
  IF iexp EQ 0 THEN plot, radius, bin_var[1,*], thick=thc

  IF iexp EQ 0 THEN window, 2 ELSE wset, 2
  IF iexp EQ 0 THEN plot, radius, bin_var[2,*], thick=thc

  IF iexp EQ 0 THEN window, 3 ELSE wset, 3
  IF iexp EQ 0 THEN plot, radius, bin_var[3,*], thick=thc

stop
END
