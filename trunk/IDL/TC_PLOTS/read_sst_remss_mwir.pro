; SST REMSS IR+MW

CASE tc_name OF
  'IVAN'     : file_ssti = '/home/gsamson/WORK/DATA/SST_REMSS_MWIR_IVAN/SST_MWIR_20080210H12-20080220H12_v03.nc'
  'GIOVANNA' : file_ssti = '/home/gsamson/WORK/DATA/SST_REMSS_MWIR_GIOV/SST_MWIR_20120208H12-20120216H12_v03.nc'
  'FELLENG'  : file_ssti = '/home/gsamson/WORK/DATA/SST_REMSS_MWIR_FELL/SST_MWIR_20130124H12-20130203H12_v03.nc'
  'GAEL'     : file_ssti = '/home/gsamson/WORK/DATA/SST_REMSS_MWIR_GAEL/SST_MWIR_20090201H12-20090210H12_v03.nc'
  'GELANE'   : file_ssti = '/home/gsamson/WORK/DATA/SST_REMSS_MWIR_GELA/SST_MWIR_20100212H12-20090223H12_v03.nc'
  'BINGIZA'  : file_ssti = '/home/gsamson/WORK/DATA/SST_REMSS_MWIR_BING/SST_MWIR_20110205H12-20110216H12_v03.nc'
ENDCASE
help, file_ssti

initncdf, file_ssti & domdef, dom_tc

juld_ssti = ncdf_gettime(file_ssti)
juldini_ssti = juld_ssti[0]
date_ssti = jul2date(juld_ssti)
;date_ssti = double(round(date_ssti))
dateini_ssti = date_ssti[0]
ibeg = max(where(date_ssti LE date_beg_obs))
iend = min(where(date_ssti GE date_end_obs))
date_ssti = date_ssti[ibeg:iend]
juldini_ssti = juld_ssti[ibeg]
jpt  = iend - ibeg +1
SSTI_0 = read_ncdf('analysed_sst', ibeg, iend, timestep=1, filename=file_ssti, /nostruct)
SSTERRI_0 = read_ncdf('analysis_error', ibeg, iend, timestep=1, filename=file_ssti, /nostruct)
help, SSTI_0, SSTERRI_0
; interpolation temporelle DAILY -> 6H (comme IBTRACS)
SSTI_0 = interpolate(SSTI_0, findgen(n_elements(SSTI_0[*,0,0])), findgen(n_elements(SSTI_0[0,*,0])), findgen((jpt-1)*4.+1.)/4., /GRID)
juld_ssti = juldini_ssti[0] + dindgen((jpt-1)*4.+1.)/4.
date_ssti = jul2date(juld_ssti)
ibeg = max(where(date_ssti LE date_beg_obs))
iend = min(where(date_ssti GE date_end_obs))
SSTI_0 = SSTI_0[*,*,ibeg:iend]
date_ssti = date_ssti[ibeg:iend]
tdim_ssti = n_elements(date_ssti) & help, tdim_ssti
lon_ssti  = glamt[firstxt:lastxt,0] & help,lon_ssti
lat_ssti  = reform(gphit[0,firstyt:lastyt]) & help, lat_ssti
xdim_ssti = n_elements(lon_ssti) & help, xdim_ssti
ydim_ssti = n_elements(lat_ssti) & help, ydim_ssti
res_ssti  = max(e1t, /nan)/1000. & help, res_ssti
