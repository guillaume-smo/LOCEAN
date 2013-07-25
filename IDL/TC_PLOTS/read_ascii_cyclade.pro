;PRO read_ascii_cyclade

asc_path = '/home/gsamson/WORK/DATA/IVAN_CYCLADE/'

mnhdir_list = FILE_SEARCH(asc_path+'MNH*',/FULLY_QUALIFY_PATH)
arodir_list = FILE_SEARCH(asc_path+'T*'  ,/FULLY_QUALIFY_PATH)

mnhfic_list = FILE_SEARCH(asc_path+'MNH*/PMER_2008021300_previ*h_stepinconnu__ND???',/FULLY_QUALIFY_PATH)
arofic_list = FILE_SEARCH(asc_path+'T*/PMER_2008021300_previ*h_stepinconnu__ND???'  ,/FULLY_QUALIFY_PATH)

nbexp_mnhd = n_elements(mnhfic_list)
nbexp_arod = n_elements(arofic_list)

lct,60
color_factor=70
key_portrait=1



; LECTURE MESO-NH
FOR i = 0, nbexp_mnhd-1 DO BEGIN

  exp_name = (STRSPLIT( mnhfic_list[i] , '/', /EXTRACT))[5]
  fic_name = (STRSPLIT( mnhfic_list[i] , '/', /EXTRACT))[6]
  print, '' & print, 'MESO-NH EXP: ', exp_name

  tmp = READ_ASCII(mnhfic_list[i], DATA_START=4)
  all_data = TRANSPOSE(tmp.FIELD01) & help, all_data
  print, 'NB COLONNES: ',strtrim(n_elements(all_data[0,*]),2)
  
  IF all_data[0,5] EQ 1. OR all_data[0,5] EQ 3. THEN BEGIN
    nbech_mnhd= n_elements(all_data[*,0])+1
    lat_mnhd  = [all_data[0,0],all_data[*,0]]
    lon_mnhd  = [all_data[0,1],all_data[*,1]]
    mslp_mnhd = [all_data[0,2],all_data[*,2]]
    date_mnhd = [all_data[0,3],all_data[*,3]]
    hour_mnhd = [0.,all_data[*,4]]/100.
    ech_mnhd  = [0.,all_data[*,5]]
    juld_mnhd = date2jul(double(date_mnhd))+1
    juld_mnhd = juld_mnhd[0] + ech_mnhd/24.
    date_mnhd = jul2date(juld_mnhd)
    max_w10m_mnhd = fltarr(nbech_mnhd-1)
    FOR j = 0, nbech_mnhd-2 DO max_w10m_mnhd[j] = max([all_data[j,10],all_data[j,11],all_data[j,12],all_data[j,13]])
    max_w10m_mnhd = [max_w10m_mnhd[0], max_w10m_mnhd] & help, max_w10m_mnhd
  ENDIF ELSE BEGIN
    nbech_mnhd= n_elements(all_data[*,0])
    lat_mnhd  = all_data[*,0]
    lon_mnhd  = all_data[*,1]
    mslp_mnhd = all_data[*,2]
    date_mnhd = all_data[*,3]
    hour_mnhd = all_data[*,4]/100.
    ech_mnhd  = all_data[*,5]
    juld_mnhd = date2jul(double(date_mnhd))+1
    juld_mnhd = juld_mnhd[0] + ech_mnhd/24.
    date_mnhd = jul2date(juld_mnhd)
    max_w10m_mnhd = fltarr(nbech_mnhd)
    FOR j = 0, nbech_mnhd-1 DO max_w10m_mnhd[j] = max([all_data[j,10],all_data[j,11],all_data[j,12],all_data[j,13]])
    help, max_w10m_mnhd
  ENDELSE

  help, max_w10m_mnhd, mslp_mnhd
  print, 'MIN/MAX WIND: ',min(max_w10m_mnhd), max(max_w10m_mnhd)
  print, 'MIN/MAX MSLP: ',min(mslp_mnhd), max(mslp_mnhd)

;  IF i EQ 0 THEN splot, max_w10m_mnhd, mslp_mnhd, XTICKFORMAT='(F10.1)', xminor=8, yrange=[895.,1005.], xrange=[20.,90.], title='WIND-PRESSURE RELATIONSHIP', xtitle='MAX 10M-WIND (m/s)', ytitle='MIN MSLP (hPa)', thick=1, psym=1, symsize=1.5, win=0
  oplot, max_w10m_mnhd, mslp_mnhd, color=color_factor*4, thick=1, psym=1, symsize=1.0

  IF i EQ 0 THEN all_wind_mnhd = max_w10m_mnhd ELSE all_wind_mnhd = [all_wind_mnhd,max_w10m_mnhd]
  IF i EQ 0 THEN all_mslp_mnhd = mslp_mnhd     ELSE all_mslp_mnhd = [all_mslp_mnhd,mslp_mnhd]

ENDFOR
;print, 'MIN/MAX WIND MNHD: ',min(all_wind_mnhd), max(all_wind_mnhd)
;print, 'MIN/MAX MSLP MNHD: ',min(all_mslp_mnhd), max(all_mslp_mnhd)



; LECTURE AROME
FOR i = 0, nbexp_arod-1 DO BEGIN

  exp_name = (STRSPLIT( arofic_list[i] , '/', /EXTRACT))[5]
  fic_name = (STRSPLIT( arofic_list[i] , '/', /EXTRACT))[6]
  print, '' & print, 'AROME EXP: ', exp_name

  tmp = READ_ASCII(arofic_list[i], DATA_START=4)
  all_data = TRANSPOSE(tmp.FIELD01) & help, all_data
  print, 'NB COLONNES: ',strtrim(n_elements(all_data[0,*]),2)

  IF all_data[0,5] EQ 1. OR all_data[0,5] EQ 3. THEN BEGIN
    nbech_arod= n_elements(all_data[*,0])+1
    lat_arod  = [all_data[0,0],all_data[*,0]]
    lon_arod  = [all_data[0,1],all_data[*,1]]
    mslp_arod = [all_data[0,2],all_data[*,2]]
    date_arod = [all_data[0,3],all_data[*,3]]
    hour_arod = [0.,all_data[*,4]]/100.
    ech_arod  = [0.,all_data[*,5]]
    juld_arod = date2jul(double(date_arod))+1
    juld_arod = juld_arod[0] + ech_arod/24.
    date_arod = jul2date(juld_arod)
    max_w10m_arod = fltarr(nbech_arod-1)
    FOR j = 0, nbech_arod-2 DO max_w10m_arod[j] = max([all_data[j,10],all_data[j,11],all_data[j,12],all_data[j,13]])
    max_w10m_arod = [max_w10m_arod[0], max_w10m_arod]
  ENDIF ELSE BEGIN
    nbech_arod= n_elements(all_data[*,0])
    lat_arod  = all_data[*,0]
    lon_arod  = all_data[*,1]
    mslp_arod = all_data[*,2]
    date_arod = all_data[*,3]
    hour_arod = all_data[*,4]/100.
    ech_arod  = all_data[*,5]
    juld_arod = date2jul(double(date_arod))+1
    juld_arod = juld_arod[0] + ech_arod/24.
    date_arod = jul2date(juld_arod)
    max_w10m_arod = fltarr(nbech_arod)
    FOR j = 0, nbech_arod-1 DO max_w10m_arod[j] = max([all_data[j,10],all_data[j,11],all_data[j,12],all_data[j,13]])
  ENDELSE

  help, max_w10m_arod, mslp_arod
;  print, 'MIN/MAX WIND: ',min(max_w10m_arod), max(max_w10m_arod)
;  print, 'MIN/MAX MSLP: ',min(mslp_arod), max(mslp_arod)

  oplot, max_w10m_arod, mslp_arod, color=color_factor*5, thick=1, psym=1, symsize=1.0

  IF i EQ 0 THEN all_wind_arod = max_w10m_arod ELSE all_wind_arod = [all_wind_arod,max_w10m_arod]
  IF i EQ 0 THEN all_mslp_arod = mslp_arod     ELSE all_mslp_arod = [all_mslp_arod,mslp_arod]

  IF (where(finite(max_w10m_arod) EQ 0))[0] NE -1 THEN print, where(finite(max_w10m_arod) EQ 0)

ENDFOR
;print, 'MIN/MAX WIND AROD: ',min(all_wind_arod), max(all_wind_arod)
;print, 'MIN/MAX MSLP AROD: ',min(all_mslp_arod), max(all_mslp_arod)


; PLOT MESO-NH
a = (regress(all_wind_mnhd,all_mslp_mnhd,const=b))[0]
x = findgen(100)
y = a*x + b
oplot, x, y, min_value=minplot, max_value=maxplot, color=color_factor*4, thick=2.
xyouts, 0.15, 0.300, 'coef MESO-NH:'+strtrim(a,2), /normal, color=color_factor*4, charsize=1.5, charthick=1

; PLOT AROME
a = (regress(all_wind_arod,all_mslp_arod,const=b))[0]
x = findgen(100)
y = a*x + b
oplot, x, y, min_value=minplot, max_value=maxplot, color=color_factor*5, thick=2.
xyouts, 0.15, 0.275, 'coef ARODA:'+strtrim(a,2), /normal, color=color_factor*5, charsize=1.5, charthick=1


;END
