PRO plot_composite_TRMM3B42_2D
; plot composites de precips issus de la collocalisation entre IBTRACS et TRMM-3B42
@common



; params
basin     = 'SIO'
datebeg   = '19980101'
dateend   = '20110101'
period    = '1998-2010'
reskm     = 25.  ; resolution radiale du profil en km
distmaxkm = 500. ; rayon maximum
disttc    = '500km'
radius    = findgen(distmaxkm/reskm+1)*reskm
diam      = findgen(distmaxkm/reskm*2+1)*reskm - distmaxkm
write_ps  = 1


; definition bin
bin_deb  = 15.
bin_fin  = 65.
bin_size = 10.
bin_unit = '(m/s)'
bin_num  = (bin_fin - bin_deb) / bin_size
bin      = bin_deb + findgen(bin_num)*bin_size
bin_mid  = bin_deb + findgen(bin_num)*bin_size + bin_size/2.


; lecture tracks (IBTRACS interpole a 3h)
pathin  = '/net/adonis/usr/adonis/varclim/gslod/IDL/COLLOCALISATION/EXP_TRMM3B42/DATA/' & help, pathin
pathtc  = '/net/adonis/usr/adonis/varclim/gslod/IDL/TRACKER/EXP_IBTRACS/DATA/' & help, pathtc
restore, pathtc+'d1_IBTRACS_IO_'+period+'_3H_NOBADTIME.dat', /VERBOSE


; selection data
IF basin EQ 'SIO'  THEN itc = where(d1_lon_3h GT 30. AND d1_lon_3h LT 130. AND d1_lat_3h LT 0. AND d1_lat_3h GE -30.)
IF basin EQ 'SWIO' THEN itc = where(d1_lon_3h GT 30. AND d1_lon_3h LT 80. AND d1_lat_3h LT 0. AND d1_lat_3h GE -30.)
IF basin EQ 'SEIO' THEN itc = where(d1_lon_3h GT 80. AND d1_lon_3h LT 130. AND d1_lat_3h LT 0. AND d1_lat_3h GE -30.)
IF basin EQ 'NIO'  THEN itc = where(d1_lon_3h GT 50. AND d1_lon_3h LT 100. AND d1_lat_3h GT 0. AND d1_lat_3h LE 25.)
IF basin EQ 'NWIO' THEN itc = where(d1_lon_3h GT 50. AND d1_lon_3h LT 80. AND d1_lat_3h GT 0. AND d1_lat_3h LE 25.)
IF basin EQ 'NEIO' THEN itc = where(d1_lon_3h GT 80. AND d1_lon_3h LT 100. AND d1_lat_3h GT 0. AND d1_lat_3h LE 25.)
help, itc
iok = itc & help, iok
ijok = array_indices([(size(d1_lon_3h))[1], (size(d1_lon_3h))[2]], iok, /dim)


; lecture data (TRMM 3B42)
restore, pathin + 'RAIN_2D_500km_TRMM3B42_3H_IO_19980101-20110101_NOBADTIME.dat', /VERBOSE
dist_tc = restaure('/net/adonis/usr/adonis/varclim/gslod/IDL/COLLOCALISATION/EXP_COUPLED_SW2_KF/DATA/UV10TR_2D_500km_COUPLED_SW2_KF_6H_IO_19900101-20100101.dat', 'dist_tc')
help, dxy_var_wsc_rot, dist_tc
rain = temporary(dxy_var_wsc_rot)


; mise en forme data
dist_tc = float(dist_tc)
cptok   = n_elements(iok)
allrain  = fltarr(cptok,n_elements(rain[0,0,*,0]),n_elements(rain[0,0,0,*])) + !values.f_nan
allmaxwind = fltarr(cptok) + !values.f_nan
allradwind = fltarr(cptok) + !values.f_nan
allmaxrain = fltarr(cptok) + !values.f_nan
allradrain = fltarr(cptok) + !values.f_nan
allat      = fltarr(cptok) + !values.f_nan

FOR i = 0, cptok-1 DO BEGIN
  allrain[i,*,*] = rain[ijok[0,i],ijok[1,i],*,*]
  allat[i]      = d1_lat_3h[ijok[0,i],ijok[1,i]]
  allmaxwind[i] = d1_wind_3h[ijok[0,i],ijok[1,i]]
  allmaxrain[i] = max(allrain[i,*,*], /nan)
  allradwind[i] = d1_rmw_3h[ijok[0,i],ijok[1,i]]
  IF allmaxrain[i] EQ 0.   THEN allmaxrain[i] = !values.f_nan
  IF allmaxrain[i] LT 100. THEN allmaxrain[i] = !values.f_nan
  IF finite(allmaxrain[i]) THEN $
    allradrain[i] = dist_tc[where(reform(allrain[i,*,*]) EQ allmaxrain[i])]
    IF allradrain[i] GE distmaxkm THEN BEGIN & allmaxrain[i] = !values.f_nan & allradrain[i] = !values.f_nan & ENDIF
;  print, i, allmaxwind[i], allradwind[i], allmaxrain[i], allradrain[i]
ENDFOR
print, n_elements(where(finite(allmaxrain)))


; profil radial en moyenne azimuthale pour chaque TC
allrain_radmean = fltarr(n_elements(allrain[*,0,0]),(n_elements(dist_tc[*,0])-1)/2+1)
FOR i = 0, (n_elements(dist_tc[*,0])-1)/2 DO BEGIN
  IF finite(allmaxrain[i]) THEN BEGIN
    irad = where(dist_tc GE i*reskm AND dist_tc LT (i+1)*reskm)
    FOR j = 0, n_elements(allrain[*,0,0])-1 DO BEGIN
      tmp = allrain[j,*,*]
      allrain_radmean[j,i] = mean(tmp[irad], /nan)
    ENDFOR
  ENDIF
ENDFOR
help, allrain_radmean


; max de precip et rayon de precip max apres moyenne azimuthale
iradrain = fltarr(cptok) + !values.f_nan
allmaxrainmean = fltarr(cptok) + !values.f_nan
allradrainmean = fltarr(cptok) + !values.f_nan
FOR i = 0, cptok-1 DO BEGIN
  IF finite(allmaxrain[i]) THEN BEGIN
    allmaxrainmean[i] = max(allrain_radmean[i,*], /nan)
    IF allmaxrainmean[i] EQ 0. OR finite(allmaxrainmean[i]) NE 1 THEN STOP
    iradrain[i] = where(allrain_radmean[i,*] EQ allmaxrainmean[i])
    allradrainmean[i] = radius[iradrain[i]]
  ENDIF
ENDFOR
help, iradrain, allradrainmean, allmaxrainmean


  ; profil analytique tuleya
  ; 1 inch = 25.4 mm
  alltul  = fltarr(cptok,n_elements(rain[0,0,*,0]),n_elements(rain[0,0,0,*])) + !values.f_nan
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
  FOR i = 0, cptok-1 DO BEGIN
    IF finite(allmaxrain[i]) THEN BEGIN
      IF finite(rm[i]) NE 1 THEN STOP
      iok = where(dist_tc LE rm[i], cntok)
      tmp = reform(alltul[i,*,*])
      tmp[iok] = t0[i]+(tm[i]-t0[i])*(dist_tc[iok]/rm[i])
      iok = where(dist_tc GE rm[i], cntok)
      tmp[iok] = tm[i]*exp(-(dist_tc[iok]-rm[i])/re[i])
      alltul[i,*,*] = tmp
    ENDIF
  ENDFOR
  alltul = alltul * 25.4 ; conversion inch/day -> mm/day


; moyenne sur lensemble des TCs
;  allrainmoy = m_mean(allrain, dim = 1, /nan) & help, allrainmoy
;  allrainstd = fltarr(n_elements(rain[0,0,*,0]),n_elements(rain[0,0,0,*]))
;  FOR j= 0, n_elements(rain[0,0,0,*])-1 DO $
;  FOR i= 0, n_elements(rain[0,0,*,0])-1 DO allrainstd[i,j]=stddev(allrain[*,i,j], /nan)


; moyenne par bin de vent
  bin_rain = fltarr(bin_num,n_elements(rain[0,0,*,0]),n_elements(rain[0,0,0,*]))+ !values.f_nan
  bin_radrain = fltarr(bin_num,n_elements(rain[0,0,*,0]),n_elements(rain[0,0,0,*]))+ !values.f_nan
  bin_tul = fltarr(bin_num,n_elements(rain[0,0,*,0]),n_elements(rain[0,0,0,*])) + !values.f_nan
  nb_bin  = intarr(bin_num)
  FOR i = 0, bin_num-2 DO BEGIN
    ibin = where(allmaxwind GE bin[i] AND allmaxwind LT bin[i+1])
    IF ibin[0] NE -1 THEN BEGIN
      nb_bin[i]          = n_elements(ibin)
      bin_rain[i,*,*]    = m_mean(allrain[ibin,*,*], dim=1, /nan)
      bin_radrain[i,*,*] = m_mean(allradrain[ibin,*,*], dim=1, /nan)
      bin_tul[i,*,*]     = m_mean(alltul[ibin,*,*], dim=1, /nan)
    ENDIF ELSE BEGIN & nb_bin[i] = 0 & ENDELSE
  ENDFOR
  print, bin_mid
  print, nb_bin, total(nb_bin)


; moyenne ponderees 
;  iok = where(nb_bin GE 10) & print, iok
  iok = [0,1,2,3] & print, iok
  bin_rainmoy = m_mean(bin_rain[iok,*,*], dim=1, /nan)
  bin_rainradmoy = m_mean(bin_radrain[iok,*,*], dim=1, /nan)
  bin_tulmoy  = m_mean(bin_tul[iok,*,*], dim=1, /nan)
  tmp = bin_rainmoy
  FOR i = 0, n_elements(bin_rainmoy[*,0])-1 DO bin_rainmoy[i,*] = tmp[n_elements(bin_rainmoy[*,0])-1-i,*]


; plots 2D 
  computegrid,-500,-500,25,25,41,41
  set_plot, 'X'
  device, retain=0, decomposed=0
  lct,39

  IF write_ps THEN openps, filename='COMP2D_RAIN_TRMM3B4.ps'
  key_onearth = 1
  plt, bin_rainmoy, min=0, max=230, lct=22, /nocont, $
  xtickformat='', ytickformat='', xtitle='cross-track distance (km)', ytitle='along-track distance (km)', title=''
  IF write_ps THEN closeps & STOP

  IF write_ps THEN openps, filename='COMP2D_RAIN_TULLEYA.ps'
  plt, bin_tulmoy, min=0, max=230, lct=22, /nocont, $
  xtickformat='', ytickformat='', xtitle='cross-track distance (km)', ytitle='along-track distance (km)', title=''
  IF write_ps THEN closeps & STOP


STOP
END
