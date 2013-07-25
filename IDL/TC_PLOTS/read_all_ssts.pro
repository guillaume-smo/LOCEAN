; SST ODYSSEA
file_ssto = '/home/gsamson/WORK/DATA/SST_ODYSSEA_IVAN/SST_20080212-20080218_DAILY.nc'
initncdf, file_ssto
domdef, dom_tc
juld_ssto = ncdf_gettime(file_ssto)
juldini_ssto = juld_ssto[0]
date_ssto = jul2date(juld_ssto)
dateini_ssto = date_ssto[0]
ibeg = where(date_ssto EQ date_beg_obs)
iend = where(date_ssto EQ date_end_obs)
date_ssto = date_ssto[ibeg:iend]
juldini_ssto = juld_ssto[ibeg]
jpt  = iend - ibeg +1
print, jpt, ibeg, iend
SSTO_0 = read_ncdf('analysed_sst', ibeg, iend, timestep=1, filename=file_ssto, /nostruct)
SSTERRO_0 = read_ncdf('analysis_error', ibeg, iend, timestep=1, filename=file_ssto, /nostruct)
help, SSTO_0, SSTERRO_0
; interpolation temporelle DAILY -> 6H (comme IBTRACS)
SSTO_0 = interpolate(SSTO_0, findgen(n_elements(SSTO_0[*,0,0])), findgen(n_elements(SSTO_0[0,*,0])), findgen(n_elements(SSTO_0[0,0,*])*3.5)/4., /GRID)
juld_ssto = juldini_ssto[0] + dindgen(jpt*3.5)/4.
date_ssto = jul2date(juld_ssto)
tdim_ssto = jpt*3.5 & help, tdim_ssto
lon_ssto  = glamt[firstxt:lastxt,0] & help,lon_ssto
lat_ssto  = reform(gphit[0,firstyt:lastyt]) & help, lat_ssto
xdim_ssto = n_elements(lon_ssto) & help, xdim_ssto
ydim_ssto = n_elements(lat_ssto) & help, ydim_ssto
res_ssto  = max(e1t, /nan)/1000. & help, res_ssto


; SST OSTIA
file_ssts = '/home/gsamson/WORK/DATA/SST_OSTIA_IVAN/SST_OSTIA_20080210-20080220.nc'
initncdf, file_ssts & domdef, dom_tc
juld_ssts = ncdf_gettime(file_ssts)
juldini_ssts = juld_ssts[0]
date_ssts = jul2date(juld_ssts)
dateini_ssts = date_ssts[0]
ibeg = max(where(date_ssts LE date_beg_obs))
iend = min(where(date_ssts GE date_end_obs))
date_ssts = date_ssts[ibeg:iend]
juldini_ssts = juld_ssts[ibeg]
jpt  = iend - ibeg +1
SSTS_0 = read_ncdf('analysed_sst', ibeg, iend, timestep=1, filename=file_ssts, /nostruct)
SSTERRS_0 = read_ncdf('analysis_error', ibeg, iend, timestep=1, filename=file_ssts, /nostruct)
help, SSTS_0, SSTERRS_0
; interpolation temporelle DAILY -> 6H (comme IBTRACS)
SSTS_0 = interpolate(SSTS_0, findgen(n_elements(SSTS_0[*,0,0])), findgen(n_elements(SSTS_0[0,*,0])), findgen((jpt-1)*4.+1.)/4., /GRID)
juld_ssts = juldini_ssts[0] + dindgen((jpt-1)*4.+1.)/4.
date_ssts = jul2date(juld_ssts)
ibeg = max(where(date_ssts LE date_beg_obs))
iend = min(where(date_ssts GE date_end_obs))
SSTS_0 = SSTS_0[*,*,ibeg:iend]
date_ssts = date_ssts[ibeg:iend]
tdim_ssts = n_elements(date_ssts) & help, tdim_ssts
lon_ssts  = glamt[firstxt:lastxt,0] & help,lon_ssts
lat_ssts  = reform(gphit[0,firstyt:lastyt]) & help, lat_ssts
xdim_ssts = n_elements(lon_ssts) & help, xdim_ssts
ydim_ssts = n_elements(lat_ssts) & help, ydim_ssts
res_ssts  = max(e1t, /nan)/1000. & help, res_ssts



; SST GLORYS2V1
file_sstg = '/home/gsamson/WORK/DATA/SST_GLORYS2V1_IVAN/SST_GLORYS2V1_1dAV_20080211H12-20080218H12_gridT.nc'
initncdf, file_sstg & domdef, dom_tc
juld_sstg = ncdf_gettime(file_sstg)-0.5 ; moyenne journalière centrée sur midi
juldini_sstg = juld_sstg[0]
date_sstg = jul2date(juld_sstg)
dateini_sstg = date_sstg[0]
ibeg = max(where(date_sstg LE date_beg_obs))
iend = min(where(date_sstg GE date_end_obs))
date_sstg = date_sstg[ibeg:iend]
juldini_sstg = juld_sstg[ibeg]
jpt  = iend - ibeg +1
SSTG_0 = read_ncdf('votemper', ibeg, iend, timestep=1, filename=file_sstg, /nostruct)+273.15
help, SSTG_0
; interpolation temporelle DAILY -> 6H (comme IBTRACS)
SSTG_0 = interpolate(SSTG_0, findgen(n_elements(SSTG_0[*,0,0])), findgen(n_elements(SSTG_0[0,*,0])), findgen((jpt-1)*4.+1.)/4., /GRID)
juld_sstg = juldini_sstg[0] + dindgen((jpt-1)*4.+1.)/4.
date_sstg = jul2date(juld_sstg)
ibeg = max(where(date_sstg LE date_beg_obs))
iend = min(where(date_sstg GE date_end_obs))
SSTG_0 = SSTG_0[*,*,ibeg:iend]
date_sstg = date_sstg[ibeg:iend]
tdim_sstg = n_elements(date_sstg) & help, tdim_sstg
lon_sstg  = glamt[firstxt:lastxt,0] & help,lon_sstg
lat_sstg  = reform(gphit[0,firstyt:lastyt]) & help, lat_sstg
xdim_sstg = n_elements(lon_sstg) & help, xdim_sstg
ydim_sstg = n_elements(lat_sstg) & help, ydim_sstg
res_sstg  = max(e1t, /nan)/1000. & help, res_sstg


; SST ALADIN
file_ssta = '/home/gsamson/WORK/DATA/SST_ALADIN_IVAN/SST_ALADIN_ECH0_200802.nc'
initncdf, file_ssta
domdef, dom_tc
juld_ssta = ncdf_gettime(file_ssta)
juldini_ssta = juld_ssta[0]
date_ssta = jul2date(juld_ssta)
dateini_ssta = date_ssta[0]
ibeg = where(date_ssta EQ date_beg_obs)
iend = where(date_ssta EQ date_end_obs)
date_ssta = date_ssta[ibeg:iend]
juldini_ssta = juld_ssta[ibeg]
jpt  = iend - ibeg +1
SSTA_0 = read_ncdf('SST', ibeg, iend, timestep=1, filename=file_ssta, /nostruct)
MASK   = read_ncdf('IND.TERREMER', ibeg, iend, timestep=1, filename=file_ssta, /nostruct)
SSTA_0[where(mask EQ 1)] = !VALUES.F_NAN
help, SSTA_0
tdim_ssta = jpt & help, tdim_ssta
lon_ssta  = glamt[firstxt:lastxt,0] & help,lon_ssta
lat_ssta  = reform(gphit[0,firstyt:lastyt]) & help, lat_ssta
xdim_ssta = n_elements(lon_ssta) & help, xdim_ssta
ydim_ssta = n_elements(lat_ssta) & help, ydim_ssta
res_ssta  = max(e1t, /nan)/1000. & help, res_ssta


; SST RESTART
file_sstr1 = '/home/gsamson/WORK/DATA/SST_RESTART/SST_20080212.nc'
initncdf, file_sstr1 & domdef, dom_tc
date_sstr1 = 20080212.00d
jpt  = 1
SSTR1_0 = read_ncdf('sst', filename=file_sstr1, /nostruct)
SSTR1_0[where(SSTR1_0 EQ 0.)] = !VALUES.F_NAN
SSTR1_0 = SSTR1_0 + 273.15
help, SSTR1_0
tdim_sstr1 = jpt & help, tdim_sstr1
lon_sstr1  = glamt[firstxt:lastxt,0] & help,lon_sstr1
lat_sstr1  = reform(gphit[0,firstyt:lastyt]) & help, lat_sstr1
xdim_sstr1 = n_elements(lon_sstr1) & help, xdim_sstr1
ydim_sstr1 = n_elements(lat_sstr1) & help, ydim_sstr1
res_sstr1  = max(e1t, /nan)/1000. & help, res_sstr1

file_sstr2 = '/home/gsamson/WORK/DATA/SST_RESTART/SST_20080213.nc'
initncdf, file_sstr2 & domdef, dom_tc
date_sstr2 = 20080213.00d
jpt  = 1
SSTR2_0 = read_ncdf('sst', filename=file_sstr2, /nostruct)
SSTR2_0[where(SSTR2_0 EQ 0.)] = !VALUES.F_NAN
SSTR2_0 = SSTR2_0 + 273.15
help, SSTR2_0
tdim_sstr2 = jpt & help, tdim_sstr2
lon_sstr2  = glamt[firstxt:lastxt,0] & help,lon_sstr2
lat_sstr2  = reform(gphit[0,firstyt:lastyt]) & help, lat_sstr2
xdim_sstr2 = n_elements(lon_sstr2) & help, xdim_sstr2
ydim_sstr2 = n_elements(lat_sstr2) & help, ydim_sstr2
res_sstr2  = max(e1t, /nan)/1000. & help, res_sstr2

file_sstr3 = '/home/gsamson/WORK/DATA/SST_RESTART/SST_20080214.nc'
initncdf, file_sstr3 & domdef, dom_tc
date_sstr3 = 20080214.00d
jpt  = 1
SSTR3_0 = read_ncdf('sst', filename=file_sstr3, /nostruct)
SSTR3_0[where(SSTR3_0 EQ 0.)] = !VALUES.F_NAN
SSTR3_0 = SSTR3_0 + 273.15
help, SSTR3_0
tdim_sstr3 = jpt & help, tdim_sstr3
lon_sstr3  = glamt[firstxt:lastxt,0] & help,lon_sstr3
lat_sstr3  = reform(gphit[0,firstyt:lastyt]) & help, lat_sstr3
xdim_sstr3 = n_elements(lon_sstr3) & help, xdim_sstr3
ydim_sstr3 = n_elements(lat_sstr3) & help, ydim_sstr3
res_sstr3  = max(e1t, /nan)/1000. & help, res_sstr3


; choix ODYSSEA ou REMSS
file_sstobs = file_ssti
tdim_sstobs = tdim_ssti
xdim_sstobs = xdim_ssti
ydim_sstobs = ydim_ssti
res_sstobs  = res_ssti
SST_0 = SSTI_0


; check
;SSTR_AVE   = avg(SSTR_0, 2)
;SSTO_AVE   = avg(SSTO_0, 2)
;SSTO_GRIDR = fromreg('bilinear', SSTO_AVE, lon_ssto, lat_ssto, lon_sstr, lat_sstr)
;plt, SSTO_GRIDR-SSTR_AVE, -2, 2, lct=67, /realcont, title=''
