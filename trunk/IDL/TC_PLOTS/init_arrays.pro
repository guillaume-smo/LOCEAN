  print, '' & print, 'INIT ARRAYS...'

  cmd = execute('tdim = tdim_'+strtrim(i,2))
;  lon_maxvor = fltarr(tdim)
;  lat_maxvor = fltarr(tdim)
  lon_maxwnd = fltarr(tdim) + !Values.F_NAN
  lat_maxwnd = fltarr(tdim) + !Values.F_NAN
  max_w10m   = fltarr(tdim) + !Values.F_NAN
  lon_minwnd = fltarr(tdim) + !Values.F_NAN
  lat_minwnd = fltarr(tdim) + !Values.F_NAN
  min_mslp   = fltarr(tdim) + !Values.F_NAN
  lon_mslp   = fltarr(tdim) + !Values.F_NAN
  lat_mslp   = fltarr(tdim) + !Values.F_NAN

  cmd = execute('lon_maxwnd_'+strtrim(i,2)+' = fltarr(tdim) + !Values.F_NAN')
  cmd = execute('lat_maxwnd_'+strtrim(i,2)+' = fltarr(tdim) + !Values.F_NAN')
  cmd = execute('max_w10m_'+strtrim(i,2)+'   = fltarr(tdim) + !Values.F_NAN')
  cmd = execute('lon_minwnd_'+strtrim(i,2)+' = fltarr(tdim) + !Values.F_NAN')
  cmd = execute('lat_minwnd_'+strtrim(i,2)+' = fltarr(tdim) + !Values.F_NAN')
  cmd = execute('min_mslp_'+strtrim(i,2)+'   = fltarr(tdim) + !Values.F_NAN')
  cmd = execute('lon_mslp_'+strtrim(i,2)+'   = fltarr(tdim) + !Values.F_NAN')
  cmd = execute('lat_mslp_'+strtrim(i,2)+'   = fltarr(tdim) + !Values.F_NAN')
;  ike_ts = fltarr(tdim, nb_exp)
;  ike_tc = fltarr(tdim, nb_exp)

  cmd = execute('nb_pts = ceil(2.*radius / res_'+strtrim(i,2)+') & help, nb_pts')
  IF nb_pts MOD 2 EQ 0 THEN nb_pts = nb_pts-1 & help, nb_pts ; DOIT TOUJOURS ETRE IMPAIR

;  cmd = execute('initncdf, file_'+strtrim(i,2))
  cmd = execute('computegrid, xaxis=lon_'+strtrim(i,2)+', yaxis=lat_'+strtrim(i,2)+', /FULLCGRID')
  jpt = tdim
  cmd = execute('help, ZON10M_SEA_'+strtrim(i,2)+',MER10M_SEA_'+strtrim(i,2))
;  cmd = execute('vor10m = curl(ZON10M_SEA_'+strtrim(i,2)+',MER10M_SEA_'+strtrim(i,2)+', MILLION=1) & help, vor10m')
  cmd = execute('lon_2DTC_'+strtrim(i,2)+' = fltarr(nb_pts, nb_pts, tdim) + !Values.F_NAN & help, lon_2DTC_'+strtrim(i,2))
  cmd = execute('lat_2DTC_'+strtrim(i,2)+' = fltarr(nb_pts, nb_pts, tdim) + !Values.F_NAN & help, lat_2DTC_'+strtrim(i,2))

  FOR k = 0, nb_var-1 DO BEGIN
    var = var_list[k]
    cmd = execute('test = n_elements('+var+'_'+strtrim(i,2)+')')
    IF test GT 0 THEN BEGIN
      cmd = execute(var+'_1DTC_'+strtrim(i,2)+' = fltarr(tdim) + !Values.F_NAN & help, '+var+'_1DTC_'+strtrim(i,2))
      cmd = execute(var+'_1D_'+  strtrim(i,2)+' = fltarr(tdim) + !Values.F_NAN & help, '+var+'_1D_'+strtrim(i,2))    
      cmd = execute(var+'_2DTC_'+strtrim(i,2)+' = fltarr(nb_pts, nb_pts, tdim) + !Values.F_NAN & help, '+var+'_2DTC_'+strtrim(i,2))
      cmd = execute(var+'_RADTC_'+strtrim(i,2)+' = fltarr(long(radius/res_rad), tdim) + !Values.F_NAN & help, '+var+'_RADTC_'+strtrim(i,2))
    ENDIF
  ENDFOR
  cmd = execute('MAX_W10M_RADTC_'+strtrim(i,2)+' = fltarr(tdim) + !Values.F_NAN')
  cmd = execute('RVM_1DTC_'+strtrim(i,2)+' = fltarr(tdim) + !Values.F_NAN')

  print, 'INIT ARRAYS OK' & print, ''
