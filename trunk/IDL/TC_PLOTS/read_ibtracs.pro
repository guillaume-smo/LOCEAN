print, '' & print, 'READING IBTRACS FILES...'


; IBTRACS parameters
ibtracs_wmo = '/home/gsamson/WORK/DATA/IBTRACS/v03r04-WMO/'
date_ini    = 18581117.00d

; read data
name_wmo = string(ncdf_lec('Basin.SI.ibtracs_wmo.v03r04.nc', iodir = ibtracs_wmo, var = 'name'))
time_wmo = ncdf_lec('Basin.SI.ibtracs_wmo.v03r04.nc', iodir = ibtracs_wmo, var = 'time_wmo')
lat_wmo  = ncdf_lec('Basin.SI.ibtracs_wmo.v03r04.nc', iodir = ibtracs_wmo, var = 'lat_wmo')*0.01
lon_wmo  = ncdf_lec('Basin.SI.ibtracs_wmo.v03r04.nc', iodir = ibtracs_wmo, var = 'lon_wmo')*0.01
maxuv10_wmo = ncdf_lec('Basin.SI.ibtracs_wmo.v03r04.nc', iodir = ibtracs_wmo, var = 'wind_wmo')*0.1*0.5144
mslp_wmo = ncdf_lec('Basin.SI.ibtracs_wmo.v03r04.nc', iodir = ibtracs_wmo, var = 'pres_wmo')*0.1
help, name_wmo, time_wmo, lat_wmo, lon_wmo, maxuv10_wmo, mslp_wmo

; missing value -> NaN
ind_wmo  = where(name_wmo EQ tc_name)
IF ind_wmo NE -1 AND force_rsmc EQ 0 THEN BEGIN
  name_wmo = name_wmo[ind_wmo]
  time_wmo = time_wmo[*,ind_wmo] & time_wmo[where(time_wmo GE 9.9e+36)] = !VALUES.F_NAN
  juld_wmo = date2jul(date_ini) + time_wmo
  date_wmo = jul2date(juld_wmo)
  lat_wmo  = lat_wmo[*,ind_wmo]  & lat_wmo[where(lat_wmo LE -327.6)] = !VALUES.F_NAN
  lon_wmo  = lon_wmo[*,ind_wmo]  & lon_wmo[where(lon_wmo LE -327.6)] = !VALUES.F_NAN
  maxuv10_wmo = maxuv10_wmo[*,ind_wmo] & maxuv10_wmo[where(maxuv10_wmo LE -3276.7 OR maxuv10_wmo EQ 0.)] = !VALUES.F_NAN
  mslp_wmo = mslp_wmo[*,ind_wmo] & mslp_wmo[where(mslp_wmo LE -3276.7 OR mslp_wmo EQ 0.)] = !VALUES.F_NAN
  help, name_wmo, time_wmo, lat_wmo, lon_wmo, maxuv10_wmo, mslp_wmo

  ; remove nan at the end of vectors
  iok_wmo = where(finite(date_wmo), cntok) & help, iok_wmo
  time_wmo = time_wmo[0:iok_wmo[cntok-1]]
  juld_wmo = juld_wmo[0:iok_wmo[cntok-1]]
  date_wmo = date_wmo[0:iok_wmo[cntok-1]]
  lat_wmo  = lat_wmo[0:iok_wmo[cntok-1]]
  lon_wmo  = lon_wmo[0:iok_wmo[cntok-1]]
  maxuv10_wmo = maxuv10_wmo[0:iok_wmo[cntok-1]]
  mslp_wmo = mslp_wmo[0:iok_wmo[cntok-1]]
  datedeb_wmo = date_wmo[0] & help, datedeb_wmo
  datefin_wmo = date_wmo[iok_wmo[cntok-1]] & help, datefin_wmo
  help, name_wmo, time_wmo, lat_wmo, lon_wmo, maxuv10_wmo, mslp_wmo

  IF datefin_wmo GE date_end_obs THEN BEGIN
    ; TIME
    ibeg = where(date_wmo EQ date_beg_obs)
    iend = where(date_wmo EQ date_end_obs)
    print, ibeg, iend
    ; VARIABLES
    cmd = execute('date_'+strtrim(i,2)+' = date_wmo[ibeg:iend]')    
    cmd = execute('W10M_SEA_'+strtrim(i,2)+' = maxuv10_wmo[ibeg:iend]')
    cmd = execute('MSLP_SEA_'+strtrim(i,2)+' = mslp_wmo[ibeg:iend]')
    cmd = execute('lon_mslp_'+strtrim(i,2)+' = lon_wmo[ibeg:iend]')
    cmd = execute('lat_mslp_'+strtrim(i,2)+' = lat_wmo[ibeg:iend]')
    cmd = execute('max_w10m_'+strtrim(i,2)+' = maxuv10_wmo[ibeg:iend]')
    cmd = execute('min_mslp_'+strtrim(i,2)+' = mslp_wmo[ibeg:iend]')
    ; DIMENSIONS
    cmd = execute('tdim_'+strtrim(i,2)+' = n_elements(mslp_wmo[ibeg:iend]) & help, tdim_'+strtrim(i,2))
  ENDIF ELSE STOP

ENDIF ELSE BEGIN
   print, '' & print, 'WARNING: TC '+tc_name+' not read in IBTRACS database -> use RSMC data'
;   tmp = READ_ASCII('/home/gsamson/WORK/DATA/CYCPREVI_CYCLADE/'+STRUPCASE(tc_name)+'.txt', DATA_START=4)
;   tmp = TRANSPOSE(tmp.FIELD01) & help, tmp
   tmp = READ_TABLE('/home/gsamson/WORK/DATA/CYCPREVI_CYCLADE/'+STRUPCASE(tc_name)+'.txt', HEAD=4, /TEXT)
   tmp = TRANSPOSE(tmp) & help, tmp

   cmd = execute('date_'+strtrim(i,2)+' = double(tmp[*,3]) + double(tmp[*,4])/2400.00d')
   cmd = execute('ibeg = where(date_'+strtrim(i,2)+' EQ date_beg_obs)')
   cmd = execute('iend = where(date_'+strtrim(i,2)+' EQ date_end_obs)')
   IF ibeg EQ -1 OR iend EQ -1 THEN STOP
   cmd = execute('date_'+strtrim(i,2)+' = date_'+strtrim(i,2)+'[ibeg:iend]')
   cmd = execute('W10M_SEA_'+strtrim(i,2)+' = float(tmp[ibeg:iend,10:13])')
   cmd = execute('MSLP_SEA_'+strtrim(i,2)+' = float(tmp[ibeg:iend,2])')
   cmd = execute('lon_mslp_'+strtrim(i,2)+' = float(tmp[ibeg:iend,1])')
   cmd = execute('lat_mslp_'+strtrim(i,2)+' = float(tmp[ibeg:iend,0])')
   ; missing values set to NaN
   cmd = execute('indbad = where(MSLP_SEA_'+strtrim(i,2)+' EQ 9999.00)')
   IF indbad[0] NE -1 THEN cmd = execute('MSLP_SEA_'+strtrim(i,2)+'[indbad] = !VALUES.F_NAN')
   cmd = execute('min_mslp_'+strtrim(i,2)+' = MSLP_SEA_'+strtrim(i,2))   
   cmd = execute('indbad = where(W10M_SEA_'+strtrim(i,2)+' EQ 9999.00)')
   IF indbad[0] NE -1 THEN cmd = execute('W10M_SEA_'+strtrim(i,2)+'[indbad] = !VALUES.F_NAN')
   cmd = execute('max_w10m_radtc_'+strtrim(i,2)+' = AVG(W10M_SEA_'+strtrim(i,2)+', 1, /nan)')
   cmd = execute('W10M_SEA_'+strtrim(i,2)+' = MAX(W10M_SEA_'+strtrim(i,2)+', dim=2, /nan)')
   cmd = execute('max_w10m_'+strtrim(i,2)+' = W10M_SEA_'+strtrim(i,2))
   cmd = execute('juld_'+strtrim(i,2)+' = date2jul(date_'+strtrim(i,2)+')')
   cmd = execute('tdim_'+strtrim(i,2)+' = n_elements(min_mslp_'+strtrim(i,2)+') & help, tdim_'+strtrim(i,2))
ENDELSE

; RVM CYCPREVI
;tmp = READ_ASCII('/home/gsamson/WORK/DATA/CYCPREVI_CYCLADE/'+STRUPCASE(tc_name)+'.txt', DATA_START=4)
;tmp = TRANSPOSE(tmp.FIELD01)
tmp = float(tmp)
tmp[where(tmp EQ 9999.00)] = !VALUES.F_NAN
RVM_1DTC_0 = avg(tmp[ibeg:iend,18:21],1, /NAN) ; moyenne des 4 cadrants
