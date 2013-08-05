print, '' & print, 'READING SST GLORYS2V3 FILE...'


exp_path = '/home/gsamson/WORK/DATA/SST_GLORYS2V3_'+tc_name+'/'
file_sstg3 = FILE_SEARCH(exp_path+"SST_GLORYS2V3_1dAV_*-*_gridT.nc")
print, file_sstg3
file_sstg3 = file_sstg3[n_elements(file_sstg3)-1]
initncdf, file_sstg3 & domdef, dom_tc
print, 'RES:', max(e1t, /nan)/1000.

juld_sstg3 = ncdf_gettime(file_sstg3)-0.5 ; moyenne journalière centrée sur midi
juldini_sstg3 = juld_sstg3[0]
date_sstg3 = jul2date(juld_sstg3)
dateini_sstg3 = date_sstg3[0]
ibeg = max(where(date_sstg3 LE date_beg_obs))
iend = min(where(date_sstg3 GE date_end_obs))
IF ibeg[0] EQ -1 OR iend[0] EQ -1 THEN STOP
date_sstg3 = date_sstg3[ibeg:iend]
juldini_sstg3 = juld_sstg3[ibeg]
jpt  = iend - ibeg +1
SSTG3_0 = reform(read_ncdf('votemper', ibeg, iend, timestep=1, filename=file_sstg3, /nostruct)+273.15)
help, SSTG3_0
; interpolation temporelle DAILY -> 6H (comme IBTRACS)
SSTG3_0 = interpolate(SSTG3_0, findgen(n_elements(SSTG3_0[*,0,0])), findgen(n_elements(SSTG3_0[0,*,0])), findgen((jpt-1)*4.+1.)/4., /GRID)
juld_sstg3 = juldini_sstg3[0] + dindgen((jpt-1)*4.+1.)/4.
date_sstg3 = jul2date(juld_sstg3)
ibeg = max(where(date_sstg3 LE date_beg_obs))
iend = min(where(date_sstg3 GE date_end_obs))
SSTG3_0 = SSTG3_0[*,*,ibeg:iend]
date_sstg3 = date_sstg3[ibeg:iend]
tdim_sstg3 = n_elements(date_sstg3) & help, tdim_sstg3
lon_sstg3  = glamt[firstxt:lastxt,0] & help,lon_sstg3
lat_sstg3  = reform(gphit[0,firstyt:lastyt]) & help, lat_sstg3
xdim_sstg3 = n_elements(lon_sstg3) & help, xdim_sstg3
ydim_sstg3 = n_elements(lat_sstg3) & help, ydim_sstg3
res_sstg3  = max(e1t, /nan)/1000. & help, res_sstg3
