print, '' & print, 'READING OHC GLORYS2V3 FILE...'


exp_path = '/home/gsamson/WORK/DATA/OHC26_GLORYS2V3_'+tc_name+'/'
file_ohcg3 = FILE_SEARCH(exp_path+"OHC26_GLORYS2V3_1dAV_*-*_gridT.nc")
print, file_ohcg3
file_ohcg3 = file_ohcg3[n_elements(file_ohcg3)-1]
initncdf, file_ohcg3 & domdef, dom_tc
print, 'RES:', max(e1t, /nan)/1000.
juld_ohcg3 = ncdf_gettime(file_sstg3)-0.5 ; moyenne journalière centrée sur midi
juldini_ohcg3 = juld_ohcg3[0]
date_ohcg3 = jul2date(juld_ohcg3)
dateini_ohcg3 = date_ohcg3[0]
ibeg = max(where(date_ohcg3 LE date_beg_obs))
iend = min(where(date_ohcg3 GE date_end_obs))
IF ibeg[0] EQ -1 OR iend[0] EQ -1 THEN STOP
date_ohcg3 = date_ohcg3[ibeg:iend]
juldini_ohcg3 = juld_ohcg3[ibeg]
jpt  = iend - ibeg +1
ohcg3_0 = reform(read_ncdf('ohc26', ibeg, iend, timestep=1, filename=file_ohcg3, /nostruct))
dept26g3_0 = reform(read_ncdf('dept26', ibeg, iend, timestep=1, filename=file_ohcg3, /nostruct))
help, ohcg3_0, dept26g3_0

; interpolation temporelle DAILY -> 6H (comme IBTRACS)
ohcg3_0 = interpolate(ohcg3_0, findgen(n_elements(ohcg3_0[*,0,0])), findgen(n_elements(ohcg3_0[0,*,0])), findgen((jpt-1)*4.+1.)/4., /GRID)
dept26g3_0 = interpolate(dept26g3_0, findgen(n_elements(dept26g3_0[*,0,0])), findgen(n_elements(dept26g3_0[0,*,0])), findgen((jpt-1)*4.+1.)/4., /GRID)

juld_ohcg3 = juldini_ohcg3[0] + dindgen((jpt-1)*4.+1.)/4.
date_ohcg3 = jul2date(juld_ohcg3)
ibeg = max(where(date_ohcg3 LE date_beg_obs))
iend = min(where(date_ohcg3 GE date_end_obs))
date_ohcg3 = date_ohcg3[ibeg:iend]
tdim_ohcg3 = n_elements(date_ohcg3) & help, tdim_ohcg3
lon_ohcg3  = glamt[firstxt:lastxt,0] & help,lon_ohcg3
lat_ohcg3  = reform(gphit[0,firstyt:lastyt]) & help, lat_ohcg3
xdim_ohcg3 = n_elements(lon_ohcg3) & help, xdim_ohcg3
ydim_ohcg3 = n_elements(lat_ohcg3) & help, ydim_ohcg3
res_ohcg3  = max(e1t, /nan)/1000. & help, res_ohcg3

ohcg3_0 = ohcg3_0[*,*,ibeg:iend]
dept26g3_0 = dept26g3_0[*,*,ibeg:iend]
