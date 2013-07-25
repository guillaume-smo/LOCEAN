PRO compute_mean_wind_radial_profile
@common


; params
explist = ['COUPLED_SW2_KF','COUPLED_SW2_BMJ']
varlist = ['UV10TR']
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
IF expname EQ 'FORCED_SW2_KF' THEN dateend = '19990101'  ELSE dateend = '20100101'  & help, dateend


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
    IF expname EQ 'COUPLED_SW2_KF' THEN restore, pathin + var_name +'_2D_'+ disttc + '_' + expname +'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat', /VERBOSE
    IF expname EQ 'COUPLED_SW2_BMJ' THEN restore, pathin + var_name +'_2D_'+ disttc + '_' + expname +'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat', /VERBOSE
    IF var_name EQ 'UV10TR' THEN BEGIN
      dxy_var1 = ut_rot
      dxy_var2 = vr_rot
    ENDIF ELSE BEGIN
      dxy_var1 = dxy_var_wsc_rot
      dxy_var2 = dxy_var_nsc_rot
    ENDELSE
    help, dxy_var1, dxy_var2


    ; mise en forme data
    allrmw = fltarr(n_elements(iok1))
    allat = fltarr(n_elements(iok1))
    tmp1 = fltarr(n_elements(iok1),n_elements(dxy_var1[0,0,*,0]),n_elements(dxy_var1[0,0,0,*]))
    tmp2 = fltarr(n_elements(iok1),n_elements(dxy_var2[0,0,*,0]),n_elements(dxy_var2[0,0,0,*]))
    FOR i = 0, n_elements(iok1)-1 DO BEGIN
      tmp1[i,*,*] = dxy_var1[ijok1[0,i],ijok1[1,i],*,*]
      tmp2[i,*,*] = dxy_var2[ijok1[0,i],ijok1[1,i],*,*]
      allat[i] = d1_lat[ijok1[0,i],ijok1[1,i]]
      allrmw[i] = d1_rmw_wrf[ijok1[0,i],ijok1[1,i]]
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
    allprofil = sqrt(allprofil1^2+allprofil2^2)
    max_allprofil = fltarr(n_elements(iok1))
    FOR i = 0, n_elements(iok1)-1 DO max_allprofil[i] = max(allprofil[i,*])

 
; profil moyen par bin de vent
    bin_var1 = intarr(bin_num, n_elements(allprofil1[0,*]))
    bin_var2 = intarr(bin_num, n_elements(allprofil2[0,*]))  
    nb_bin  = intarr(bin_num)
    FOR i = 0, bin_num-2 DO BEGIN
      ibin = where(max_allprofil GE bin[i] AND max_allprofil LT bin[i+1])
      IF ibin[0] NE -1 THEN BEGIN 
        nb_bin[i] = n_elements(ibin)
        bin_var1[i,*] = m_mean(allprofil1[ibin,*], dim=1, /nan)
        bin_var2[i,*] = m_mean(allprofil2[ibin,*], dim=1, /nan)
      ENDIF ELSE BEGIN & nb_bin[i] = 0 & ENDELSE
    ENDFOR
    print, bin_mid
    print, nb_bin, total(nb_bin)
    iok = where(nb_bin GE 10) & print, iok
    profil1 = m_mean(bin_var1[iok,*], dim=1, /nan) 
    profil2 = m_mean(bin_var2[iok,*], dim=1, /nan)
    help, profil1, profil2
    profil = sqrt(profil1^2+profil2^2)
  ENDIF


; profil analytique willoughby

  IF expname EQ 'IBTRACS' THEN BEGIN
; on utilise le vent max local pour definir le vent max moyen

    rmw = 46.4 * exp(-0.0155*d1_max_wnd + 0.0169*abs(d1_lat))
    xx2 = 25.
    xx1 = 287.6 - 1.942*d1_max_wnd + 7.799*alog(rmw) + 1.819*abs(d1_lat)
    nn  = 2.1340 + 0.0077*d1_max_wnd - 0.4522*alog(rmw) - 0.0038*abs(d1_lat)
    aa  = 0.5913 + 0.0029*d1_max_wnd - 0.1361*alog(rmw) - 0.0042*abs(d1_lat) > 0

    allprofilana = fltarr(n_elements(d1_lon[*,0]),n_elements(d1_lon[0,*]),n_elements(radius))
    FOR i = 0, n_elements(d1_lon[*,0])-1 DO BEGIN
    FOR j = 0, n_elements(d1_lon[0,*])-1 DO BEGIN
      IF finite(d1_max_wnd[i,j]) EQ 1 THEN BEGIN
        allprofilana[i,j,*] = d1_max_wnd[i,j] * ( (1-aa[i,j]) * exp(- (radius-rmw[i,j])/xx1[i,j] ) + aa[i,j] * exp(- (radius-rmw[i,j])/xx2 ) )
        allprofilana[i,j,where(radius LT rmw[i,j])] = d1_max_wnd[i,j] * ( (radius[where(radius LT rmw[i,j])]/rmw[i,j])^nn[i,j] )
      ENDIF ELSE BEGIN & allprofilana[i,j,*] = !values.f_nan & ENDELSE
    ENDFOR
    ENDFOR
    tmp = fltarr(n_elements(iok1),n_elements(allprofilana[0,0,*]))
    FOR i = 0, n_elements(iok1)-1 DO BEGIN
      tmp[i,*] = allprofilana[ijok1[0,i],ijok1[1,i],*]
    ENDFOR
    allprofilana = tmp

    bin_var_ana = intarr(bin_num, n_elements(allprofilana[0,*]))
    nb_bin_ana = intarr(bin_num)
    FOR i = 0, bin_num-2 DO BEGIN
      ibin = where(d1_max_wnd[iok1] GE bin[i] AND d1_max_wnd[iok1] LT bin[i+1])
      IF ibin[0] NE -1 THEN nb_bin_ana[i] = n_elements(ibin) ELSE nb_bin_ana[i] = 0
      bin_var_ana[i,*] = m_mean(allprofilana[ibin,*], dim=1, /nan)
    ENDFOR

  ENDIF ELSE BEGIN

    rmw1 = 46.4 * exp(-0.0155*max_allprofil + 0.0169*abs(allat))
    rmw2 = allrmw
    xx2 = 25.
    xx11 = 287.6 - 1.942*max_allprofil + 7.799*alog(rmw1) + 1.819*abs(allat)
    xx12 = 287.6 - 1.942*max_allprofil + 7.799*alog(rmw2) + 1.819*abs(allat)
    nn1  = 2.1340 + 0.0077*max_allprofil - 0.4522*alog(rmw1) - 0.0038*abs(allat)
    nn2  = 2.1340 + 0.0077*max_allprofil - 0.4522*alog(rmw2) - 0.0038*abs(allat)
    aa1  = 0.5913 + 0.0029*max_allprofil - 0.1361*alog(rmw1) - 0.0042*abs(allat) > 0
    aa2  = 0.5913 + 0.0029*max_allprofil - 0.1361*alog(rmw2) - 0.0042*abs(allat) > 0

    allprofilana1 = fltarr(n_elements(iok1),n_elements(radius))
    allprofilana2 = fltarr(n_elements(iok1),n_elements(radius))
    FOR i = 0, n_elements(iok1)-1 DO BEGIN
      allprofilana1[i,*] = max_allprofil[i] * ( (1-aa1[i]) * exp(- (radius-rmw1[i])/xx11[i] ) + aa1[i] * exp(- (radius-rmw1[i])/xx2 ) )
      allprofilana1[i,where(radius LT rmw1[i])] = max_allprofil[i] * ( (radius[where(radius LT rmw1[i])]/rmw1[i])^nn1[i] )
      allprofilana2[i,*] = max_allprofil[i] * ( (1-aa2[i]) * exp(- (radius-rmw2[i])/xx12[i] ) + aa2[i] * exp(- (radius-rmw2[i])/xx2 ) )
      allprofilana2[i,where(radius LT rmw2[i])] = max_allprofil[i] * ( (radius[where(radius LT rmw2[i])]/rmw2[i])^nn2[i] )
    ENDFOR

    bin_var_ana1 = intarr(bin_num, n_elements(allprofilana1[0,*]))
    bin_var_ana2 = intarr(bin_num, n_elements(allprofilana2[0,*]))
    nb_bin_ana = intarr(bin_num)
    FOR i = 0, bin_num-2 DO BEGIN
      ibin = where(max_allprofil GE bin[i] AND max_allprofil LT bin[i+1])
      IF ibin[0] NE -1 THEN BEGIN 
        nb_bin_ana[i] = n_elements(ibin)
        bin_var_ana1[i,*] = m_mean(allprofilana1[ibin,*], dim=1, /nan)
        bin_var_ana2[i,*] = m_mean(allprofilana2[ibin,*], dim=1, /nan)
      ENDIF ELSE BEGIN & nb_bin_ana[i] = 0 & ENDELSE
    ENDFOR

  ENDELSE

  print, bin_mid
  print, nb_bin_ana, total(nb_bin_ana)

  iok = where(nb_bin_ana GE 10) & print, iok
  profil_ana1 = m_mean(bin_var_ana1[iok,*], dim=1, /nan) & help, profil_ana1
  profil_ana2 = m_mean(bin_var_ana2[iok,*], dim=1, /nan) & help, profil_ana2


; profil analytique holland
  distmaxkm = 500.
  reskm = 25.
  radius = findgen(distmaxkm/reskm+1)*reskm
  rmw1 = 51.6*exp(-0.0223*max_allprofil + 0.0281*abs(allat))
  rmw2 = allrmw
;  b = 1.33
  b1 = 1.0036 + 0.0173*max_allprofil - 0.0313*alog(rmw1) + 0.0087*abs(allat)
  b2 = 1.0036 + 0.0173*max_allprofil - 0.0313*alog(rmw2) + 0.0087*abs(allat)

  allprofilhol1 = fltarr(n_elements(iok1),n_elements(radius))
  allprofilhol2 = fltarr(n_elements(iok1),n_elements(radius))
  FOR i = 0, n_elements(iok1)-1 DO BEGIN
    allprofilhol1[i,*] = max_allprofil[i]*sqrt(( rmw1[i] / (radius) )^b1[i]* exp(1-(rmw1[i]/ (radius) )^b1[i]))
    allprofilhol2[i,*] = max_allprofil[i]*sqrt(( rmw2[i] / (radius) )^b2[i]* exp(1-(rmw2[i]/ (radius) )^b2[i]))
    allprofilhol1[i,0] = 0.
    allprofilhol2[i,0] = 0.
  ENDFOR

; calcul profil holland par bin de vent
  bin_var_hol1 = intarr(bin_num, n_elements(allprofilhol1[0,*]))
  bin_var_hol2 = intarr(bin_num, n_elements(allprofilhol2[0,*]))
  nb_bin_hol = intarr(bin_num)
  FOR i = 0, bin_num-2 DO BEGIN
    ibin = where(max_allprofil GE bin[i] AND max_allprofil LT bin[i+1])
    IF ibin[0] NE -1 THEN BEGIN
      nb_bin_hol[i] = n_elements(ibin)
      bin_var_hol1[i,*] = m_mean(allprofilhol1[ibin,*], dim=1, /nan)
      bin_var_hol2[i,*] = m_mean(allprofilhol2[ibin,*], dim=1, /nan)
    ENDIF ELSE BEGIN & nb_bin_hol[i] = 0 & ENDELSE
  ENDFOR
  print, bin_mid
  print, nb_bin_hol, total(nb_bin_hol)
  iok = where(nb_bin_hol GE 10) & print, iok
  profil_hol1 = m_mean(bin_var_hol1[iok,*], dim=1, /nan) & help, profil_hol1
  profil_hol2 = m_mean(bin_var_hol2[iok,*], dim=1, /nan) & help, profil_hol2


; sauvegarde des profils
  IF expname EQ 'COUPLED_SW2_KF' THEN BEGIN
    profil_kf = profil ; real profile
    profil_ana1_kf = profil_ana1 ; willoughby profile + rmw
    profil_ana2_kf = profil_ana2 ; willoughby profile + real rmw
    profil_hol1_kf = profil_hol1 ; holland profile + rmw
    profil_hol2_kf = profil_hol2 ; holland profile + real rmw
  ENDIF

  IF expname EQ 'COUPLED_SW2_BMJ' THEN BEGIN
    profil_bmj = profil ; real profile
    profil_ana1_bmj = profil_ana1 ; willoughby profile + rmw
    profil_ana2_bmj = profil_ana2 ; willoughby profile + real rmw
    profil_hol1_bmj = profil_hol1 ; holland profile + rmw
    profil_hol2_bmj = profil_hol2 ; holland profile + real rmw
  ENDIF


ENDFOR; var
ENDFOR; exp



; plots 1D
  set_plot, 'X'
  device, retain=0, decomposed=0
  lct,39

  radius2 = findgen(distmaxkm/(reskm/2.)+1)*reskm/2.

  IF write_ps THEN thc = 6 ELSE thc = 2
  IF write_ps THEN openps, filename='COMP1D_RADIAL_WIND.ps'
  splot, radius2, smooth(interpol(profil_kf,n_elements(radius2)),3), thick=thc, color=0 , yrange=[0.,30.], xtitle='RADIUS (km)', ytitle='WIND SPEED (m/s)' & $

  oplot, radius2, smooth(interpol(profil_kf,n_elements(radius2)),3), thick=thc, color=60 & $
;  oplot, radius, profil_hol1_kf, thick=2, line=2, color=150 & $
  oplot, radius2, smooth(interpol(profil_hol2_kf,n_elements(radius2)),3), thick=thc, color=60, line=2 & $
;  oplot, radius2, smooth(interpol(profil_ana1_kf,n_elements(radius2)),3), thick=thc, line=2, color=195 & $
  oplot, radius2, smooth(interpol(profil_ana2_kf,n_elements(radius2)),3), thick=thc, color=60, line=3 & $

;  plot, radius, profil_bmj, thick=2, color=0 & $
  oplot, radius2, smooth(interpol(profil_bmj,n_elements(radius2)),3), thick=thc, color=250 & $
;  oplot, radius, profil_hol1_bmj, thick=2, line=2, color=150 & $
  oplot, radius2, smooth(interpol(profil_hol2_bmj,n_elements(radius2)),3), thick=thc, color=250, line=2 & $
;  oplot, radius2, smooth(interpol(profil_ana1_bmj,n_elements(radius2)),3), thick=thc, line=2, color=150 & $
  oplot, radius2, smooth(interpol(profil_ana2_bmj,n_elements(radius2)),3), thick=thc, color=250,line=3
  IF write_ps THEN closeps & STOP

  IF iexp EQ 0 THEN window, 0 ELSE wset, 0 
  IF iexp EQ 0 THEN plot, radius, bin_var[0,*], thick=2
  IF iexp EQ 1 THEN oplot, radius, profil, thick=2, line=1
  IF iexp EQ 2 THEN oplot, radius, profil, thick=2, line=2

  IF iexp EQ 0 THEN window, 1 ELSE wset, 1
  IF iexp EQ 0 THEN plot, radius, bin_var[1,*], thick=2

  IF iexp EQ 0 THEN window, 2 ELSE wset, 2
  IF iexp EQ 0 THEN plot, radius, bin_var[2,*], thick=2

  IF iexp EQ 0 THEN window, 3 ELSE wset, 3
  IF iexp EQ 0 THEN plot, radius, bin_var[3,*], thick=2

stop
END
