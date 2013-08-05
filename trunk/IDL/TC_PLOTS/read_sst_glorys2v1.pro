print, '' & print, 'READING SST GLORYS2V1 FILE...'


exp_path = '/home/gsamson/WORK/DATA/SST_GLORYS2V1_'+tc_name+'/'
file_sstg1 = FILE_SEARCH(exp_path + "SST_GLORYS2V1_1dAV_*-*_gridT.nc") & print, file_sstg1
file_sstg1 = file_sstg1[n_elements(file_sstg1)-1]
initncdf, file_sstg1 & domdef, dom_tc
print, 'RES:', max(e1t, /nan)/1000.

juld_sstg1 = ncdf_gettime(file_sstg1)-0.5 ; moyenne journalière centrée sur midi
juldini_sstg1 = juld_sstg1[0]
date_sstg1 = jul2date(juld_sstg1)
dateini_sstg1 = date_sstg1[0]
ibeg = max(where(date_sstg1 LE date_beg_obs))
iend = min(where(date_sstg1 GE date_end_obs))
IF ibeg[0] EQ -1 OR iend[0] EQ -1 THEN STOP
date_sstg1 = date_sstg1[ibeg:iend]
juldini_sstg1 = juld_sstg1[ibeg]
jpt  = iend - ibeg +1
SSTG1_0 = reform(read_ncdf('votemper', ibeg, iend, timestep=1, filename=file_sstg1, /nostruct)+273.15)
help, SSTG1_0
; interpolation temporelle DAILY -> 6H (comme IBTRACS)
SSTG1_0 = interpolate(SSTG1_0, findgen(n_elements(SSTG1_0[*,0,0])), findgen(n_elements(SSTG1_0[0,*,0])), findgen((jpt-1)*4.+1.)/4., /GRID)
juld_sstg1 = juldini_sstg1[0] + dindgen((jpt-1)*4.+1.)/4.
date_sstg1 = jul2date(juld_sstg1)
ibeg = max(where(date_sstg1 LE date_beg_obs))
iend = min(where(date_sstg1 GE date_end_obs))
SSTG1_0 = SSTG1_0[*,*,ibeg:iend]
date_sstg1 = date_sstg1[ibeg:iend]
tdim_sstg1 = n_elements(date_sstg1) & help, tdim_sstg1
lon_sstg1  = glamt[firstxt:lastxt,0] & help,lon_sstg1
lat_sstg1  = reform(gphit[0,firstyt:lastyt]) & help, lat_sstg1
xdim_sstg1 = n_elements(lon_sstg1) & help, xdim_sstg1
ydim_sstg1 = n_elements(lat_sstg1) & help, ydim_sstg1
res_sstg1  = max(e1t, /nan)/1000. & help, res_sstg1
