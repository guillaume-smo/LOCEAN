; SST REMSS MW
CASE tc_name OF
  'IVAN'     : file_sstm = '/home/gsamson/WORK/DATA/SST_REMSS_MW_IVAN/SST_MW_20080210H12-20080220H12_v03.nc'
  'GIOVANNA' : file_sstm = '/home/gsamson/WORK/DATA/SST_REMSS_MW_GIOV/SST_MW_20120208H12-20120216H12_v03.nc'
  'FELLENG'  : file_sstm = '/home/gsamson/WORK/DATA/SST_REMSS_MW_FELL/SST_MW_20130124H12-20130203H12_v03.nc'
  'GAEL'     : file_sstm = '/home/gsamson/WORK/DATA/SST_REMSS_MW_GAEL/SST_MW_20090201H12-20090210H12_v03.nc'
  'GELANE'   : file_sstm = '/home/gsamson/WORK/DATA/SST_REMSS_MW_GELA/SST_MW_20100212H12-20090223H12_v03.nc'
  'BINGIZA'  : file_sstm = '/home/gsamson/WORK/DATA/SST_REMSS_MW_BING/SST_MW_20110205H12-20110216H12_v03.nc'
ENDCASE
help, file_sstm

initncdf, file_sstm & domdef, dom_tc

juld_sstm = ncdf_gettime(file_sstm)
juldini_sstm = juld_sstm[0]
date_sstm = jul2date(juld_sstm)
dateini_sstm = date_sstm[0]
ibeg = max(where(date_sstm LE date_beg_obs))
iend = min(where(date_sstm GE date_end_obs))
IF ibeg EQ -1 OR iend EQ -1 THEN STOP
date_sstm = date_sstm[ibeg:iend]
juldini_sstm = juld_sstm[ibeg]
jpt  = iend - ibeg +1
SSTM_0 = read_ncdf('analysed_sst', ibeg, iend, timestep=1, filename=file_sstm, /nostruct)
SSTERRM_0 = read_ncdf('analysis_error', ibeg, iend, timestep=1, filename=file_sstm, /nostruct)
help, SSTM_0, SSTERRM_0
; interpolation temporelle DAILY -> 6H
SSTM_0 = interpolate(SSTM_0, findgen(n_elements(SSTM_0[*,0,0])), findgen(n_elements(SSTM_0[0,*,0])), findgen((jpt-1)*4.+1.)/4., /GRID)
juld_sstm = juldini_sstm[0] + dindgen((jpt-1)*4.+1.)/4.
date_sstm = jul2date(juld_sstm)
ibeg = max(where(date_sstm LE date_beg_obs))
iend = min(where(date_sstm GE date_end_obs))
SSTM_0 = SSTM_0[*,*,ibeg:iend]
date_sstm = date_sstm[ibeg:iend]
tdim_sstm = n_elements(date_sstm) & help, tdim_sstm
lon_sstm  = glamt[firstxt:lastxt,0] & help,lon_sstm
lat_sstm  = reform(gphit[0,firstyt:lastyt]) & help, lat_sstm
xdim_sstm = n_elements(lon_sstm) & help, xdim_sstm
ydim_sstm = n_elements(lat_sstm) & help, ydim_sstm
res_sstm  = max(e1t, /nan)/1000. & help, res_sstm
