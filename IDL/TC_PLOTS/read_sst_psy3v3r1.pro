print, '' & print, 'READING PSY3V3R1 FILE...'


exp_path = '/home/gsamson/WORK/DATA/SST_PSY3V3R1_'+tc_name+'/'
file_sstp = FILE_SEARCH(exp_path+"SST_PSY3V3R1_1dAV_*-*_gridT.nc")
print, file_sstp
file_sstp = file_sstp[n_elements(file_sstp)-1]
initncdf, file_sstp & domdef, dom_tc
print, 'RES:', max(e1t, /nan)/1000.

juld_sstp = ncdf_gettime(file_sstp)-0.5 ; moyenne journalière centrée sur midi
juldini_sstp = juld_sstp[0]
date_sstp = jul2date(juld_sstp)
dateini_sstp = date_sstp[0]
ibeg = max(where(date_sstp LE date_beg_obs))
iend = min(where(date_sstp GE date_end_obs))
IF ibeg[0] EQ -1 OR iend[0] EQ -1 THEN STOP
date_sstp = date_sstp[ibeg:iend]
juldini_sstp = juld_sstp[ibeg]
jpt  = iend - ibeg +1
SSTP_0 = reform(read_ncdf('votemper', ibeg, iend, timestep=1, filename=file_sstp, /nostruct)+273.15)
help, SSTP_0
; interpolation temporelle DAILY -> 6H (comme IBTRACS)
SSTP_0 = interpolate(SSTP_0, findgen(n_elements(SSTP_0[*,0,0])), findgen(n_elements(SSTP_0[0,*,0])), findgen((jpt-1)*4.+1.)/4., /GRID)
juld_sstp = juldini_sstp[0] + dindgen((jpt-1)*4.+1.)/4.
date_sstp = jul2date(juld_sstp)
ibeg = max(where(date_sstp LE date_beg_obs))
iend = min(where(date_sstp GE date_end_obs))
SSTP_0 = SSTP_0[*,*,ibeg:iend]
date_sstp = date_sstp[ibeg:iend]
tdim_sstp = n_elements(date_sstp) & help, tdim_sstp
lon_sstp  = glamt[firstxt:lastxt,0] & help,lon_sstp
lat_sstp  = reform(gphit[0,firstyt:lastyt]) & help, lat_sstp
xdim_sstp = n_elements(lon_sstp) & help, xdim_sstp
ydim_sstp = n_elements(lat_sstp) & help, ydim_sstp
res_sstp  = max(e1t, /nan)/1000. & help, res_sstp
