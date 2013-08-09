print, '' & print, 'READING SST GLORYS2V1+ALADIN FILE...'

sst_name = sst_list[l]
exp_path = '/home/gsamson/WORK/DATA/SST_'+sst_name+'_'+tc_name+'/'
file_sstg1a = FILE_SEARCH(exp_path + 'SST_'+sst_name+'_1dAV_*-*_gridT.nc') & print, file_sstg1a
file_sstg1a = file_sstg1a[n_elements(file_sstg1a)-1]
initncdf, file_sstg1a & domdef, [dom_tc,0,0], /ZINDEX
print, 'RES:', max(e1t, /nan)/1000.

juld_sstg1a = ncdf_gettime(file_sstg1a)-0.5 ; moyenne journalière centrée sur midi
juldini_sstg1a = juld_sstg1a[0]
date_sstg1a = jul2date(juld_sstg1a)
dateini_sstg1a = date_sstg1a[0]
ibeg = max(where(date_sstg1a LE date_beg_obs))
iend = min(where(date_sstg1a GE date_end_obs))
IF ibeg[0] EQ -1 OR iend[0] EQ -1 THEN STOP
date_sstg1a = date_sstg1a[ibeg:iend]
juldini_sstg1a = juld_sstg1a[ibeg]
jpt  = iend - ibeg +1
print, ibeg, iend
sstg1a_0 = reform(read_ncdf('votemper', ibeg, iend, timestep=1, filename=file_sstg1a, /nostruct)+273.15)
help, sstg1a_0
; interpolation temporelle DAILY -> 6H (comme IBTRACS)
sstg1a_0 = interpolate(sstg1a_0, findgen(n_elements(sstg1a_0[*,0,0])), findgen(n_elements(sstg1a_0[0,*,0])), findgen((jpt-1)*4.+1.)/4., /GRID)
juld_sstg1a = juldini_sstg1a[0] + dindgen((jpt-1)*4.+1.)/4.
date_sstg1a = jul2date(juld_sstg1a)
ibeg = max(where(date_sstg1a LE date_beg_obs))
iend = min(where(date_sstg1a GE date_end_obs))
sstg1a_0 = sstg1a_0[*,*,ibeg:iend]
date_sstg1a = date_sstg1a[ibeg:iend]
tdim_sstg1a = n_elements(date_sstg1a) & help, tdim_sstg1a
lon_sstg1a  = glamt[firstxt:lastxt,0] & help,lon_sstg1a
lat_sstg1a  = reform(gphit[0,firstyt:lastyt]) & help, lat_sstg1a
xdim_sstg1a = n_elements(lon_sstg1a) & help, xdim_sstg1a
ydim_sstg1a = n_elements(lat_sstg1a) & help, ydim_sstg1a
res_sstg1a  = max(e1t, /nan)/1000. & help, res_sstg1a
