print, '' & print, 'READING OHC GLORYS2V1 FILE...'


exp_path = '/home/gsamson/WORK/DATA/OHC26_GLORYS2V1_'+tc_name+'/'
file_ohcg1 = FILE_SEARCH(exp_path + "OHC26_GLORYS2V1_1dAV_*-*_gridT.nc") & print, file_ohcg1
file_ohcg1 = file_ohcg1[n_elements(file_ohcg1)-1]
initncdf, file_ohcg1 & domdef, dom_tc
print, 'RES:', max(e1t, /nan)/1000.
juld_ohcg1 = ncdf_gettime(file_sstg1)-0.5 ; moyenne journalière centrée sur midi
juldini_ohcg1 = juld_ohcg1[0]
date_ohcg1 = jul2date(juld_ohcg1)
dateini_ohcg1 = date_ohcg1[0]
ibeg = max(where(date_ohcg1 LE date_beg_obs))
iend = min(where(date_ohcg1 GE date_end_obs))
IF ibeg[0] EQ -1 OR iend[0] EQ -1 THEN STOP
date_ohcg1 = date_ohcg1[ibeg:iend]
juldini_ohcg1 = juld_ohcg1[ibeg]
jpt  = iend - ibeg +1
ohcg1_0 = reform(read_ncdf('ohc26', ibeg, iend, timestep=1, filename=file_ohcg1, /nostruct))
dept26g1_0 = reform(read_ncdf('dept26', ibeg, iend, timestep=1, filename=file_ohcg1, /nostruct))
help, ohcg1_0, dept26g1_0

; interpolation temporelle DAILY -> 6H (comme IBTRACS)
ohcg1_0 = interpolate(ohcg1_0, findgen(n_elements(ohcg1_0[*,0,0])), findgen(n_elements(ohcg1_0[0,*,0])), findgen((jpt-1)*4.+1.)/4., /GRID)
dept26g1_0 = interpolate(dept26g1_0, findgen(n_elements(dept26g1_0[*,0,0])), findgen(n_elements(dept26g1_0[0,*,0])), findgen((jpt-1)*4.+1.)/4., /GRID)

juld_ohcg1 = juldini_ohcg1[0] + dindgen((jpt-1)*4.+1.)/4.
date_ohcg1 = jul2date(juld_ohcg1)
ibeg = max(where(date_ohcg1 LE date_beg_obs))
iend = min(where(date_ohcg1 GE date_end_obs))
date_ohcg1 = date_ohcg1[ibeg:iend]
tdim_ohcg1 = n_elements(date_ohcg1) & help, tdim_ohcg1
lon_ohcg1  = glamt[firstxt:lastxt,0] & help,lon_ohcg1
lat_ohcg1  = reform(gphit[0,firstyt:lastyt]) & help, lat_ohcg1
xdim_ohcg1 = n_elements(lon_ohcg1) & help, xdim_ohcg1
ydim_ohcg1 = n_elements(lat_ohcg1) & help, ydim_ohcg1
res_ohcg1  = max(e1t, /nan)/1000. & help, res_ohcg1

ohcg1_0 = ohcg1_0[*,*,ibeg:iend]
dept26g1_0 = dept26g1_0[*,*,ibeg:iend]
