print, '' & print, 'READING SST GLORYS2V3+ALADIN FILE...'

sst_name = sst_list[l]
exp_path = '/home/gsamson/WORK/DATA/SST_'+sst_name+'_'+tc_name+'/'
file_sstg3a = FILE_SEARCH(exp_path + 'SST_'+sst_name+'_1dAV_*-*_gridT.nc') & print, file_sstg3a
file_sstg3a = file_sstg3a[n_elements(file_sstg3a)-1]
initncdf, file_sstg3a & domdef, [dom_tc,0,0], /ZINDEX
print, 'RES:', max(e1t, /nan)/1000.

juld_sstg3a = ncdf_gettime(file_sstg3a)-0.5 ; moyenne journalière centrée sur midi
juldini_sstg3a = juld_sstg3a[0]
date_sstg3a = jul2date(juld_sstg3a)
dateini_sstg3a = date_sstg3a[0]
ibeg = max(where(date_sstg3a LE date_beg_obs))
iend = min(where(date_sstg3a GE date_end_obs))
IF ibeg[0] EQ -1 OR iend[0] EQ -1 THEN STOP
date_sstg3a = date_sstg3a[ibeg:iend]
juldini_sstg3a = juld_sstg3a[ibeg]
jpt  = iend - ibeg +1
print, ibeg, iend
sstg3a_0 = reform(read_ncdf('votemper', ibeg, iend, timestep=1, filename=file_sstg3a, /nostruct)+273.15)
help, sstg3a_0
; interpolation temporelle DAILY -> 6H (comme IBTRACS)
sstg3a_0 = interpolate(sstg3a_0, findgen(n_elements(sstg3a_0[*,0,0])), findgen(n_elements(sstg3a_0[0,*,0])), findgen((jpt-1)*4.+1.)/4., /GRID)
juld_sstg3a = juldini_sstg3a[0] + dindgen((jpt-1)*4.+1.)/4.
date_sstg3a = jul2date(juld_sstg3a)
ibeg = max(where(date_sstg3a LE date_beg_obs))
iend = min(where(date_sstg3a GE date_end_obs))
sstg3a_0 = sstg3a_0[*,*,ibeg:iend]
date_sstg3a = date_sstg3a[ibeg:iend]
tdim_sstg3a = n_elements(date_sstg3a) & help, tdim_sstg3a
lon_sstg3a  = glamt[firstxt:lastxt,0] & help,lon_sstg3a
lat_sstg3a  = reform(gphit[0,firstyt:lastyt]) & help, lat_sstg3a
xdim_sstg3a = n_elements(lon_sstg3a) & help, xdim_sstg3a
ydim_sstg3a = n_elements(lat_sstg3a) & help, ydim_sstg3a
res_sstg3a  = max(e1t, /nan)/1000. & help, res_sstg3a
