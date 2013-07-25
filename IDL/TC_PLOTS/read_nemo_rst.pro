; SST NEMO RESTART 12
file_nemo12 = '/home/gsamson/WORK/NEMO/RUNS_JEROME/IVAN12-T00_1hAV_20080212-20080219_gridT_R20080212.nc'
initncdf, file_nemo12 & domdef, dom_tc

juld_nemo12 = ncdf_gettime(file_nemo12)
date_nemo12 = jul2date(juld_nemo12)
ibeg = max([where(date_nemo12 LE date_beg_obs), 0])
iend = min([where(date_nemo12 GE date_end_obs), jpt-1])
date_nemo12 = date_nemo12[ibeg:iend]
SST_NEMO12 = read_ncdf('sotemper', ibeg, iend, timestep=1, filename=file_nemo12, /nostruct)+273.15
iok = (listmatch(date_nemo12,date_0))[*,0]
date_nemo12 = date_nemo12[iok]
tdim_nemo12 = n_elements(iok)
SST_NEMO12 = SST_NEMO12[*,*,iok]
res_nemo12  = max(e1t, /nan)/1000. & help, res_nemo12
xdim_nemo12 = nxt
ydim_nemo12 = nyt


; SST NEMO RESTART 13
file_nemo13 = '/home/gsamson/WORK/NEMO/RUNS_JEROME/IVAN12-T00_1hAV_20080213-20080219_gridT_R20080213.nc'
initncdf, file_nemo13 & domdef, dom_tc

juld_nemo13 = ncdf_gettime(file_nemo13)
date_nemo13 = jul2date(juld_nemo13)
ibeg = max([where(date_nemo13 LE date_beg_obs), 0])
iend = min([where(date_nemo13 GE date_end_obs), jpt-1])
date_nemo13 = date_nemo13[ibeg:iend]
SST_NEMO13 = read_ncdf('sotemper', ibeg, iend, timestep=1, filename=file_nemo13, /nostruct)+273.15
iok = (listmatch(date_nemo13,date_0))[*,0]
date_nemo13 = date_nemo13[iok]
tdim_nemo13 = n_elements(iok)
SST_NEMO13 = SST_NEMO13[*,*,iok]
res_nemo13  = max(e1t, /nan)/1000. & help, res_nemo13
xdim_nemo13 = nxt
ydim_nemo13 = nyt


; SST NEMO RESTART 14
file_nemo14 = '/home/gsamson/WORK/NEMO/RUNS_JEROME/IVAN12-T00_1hAV_20080214-20080219_gridT_R20080214.nc'
initncdf, file_nemo14 & domdef, dom_tc

juld_nemo14 = ncdf_gettime(file_nemo14)
date_nemo14 = jul2date(juld_nemo14)
ibeg = max([where(date_nemo14 LE date_beg_obs), 0])
iend = min([where(date_nemo14 GE date_end_obs), jpt-1])
date_nemo14 = date_nemo14[ibeg:iend]
SST_NEMO14 = read_ncdf('sotemper', ibeg, iend, timestep=1, filename=file_nemo14, /nostruct)+273.15
iok = (listmatch(date_nemo14,date_0))[*,0]
date_nemo14 = date_nemo14[iok]
tdim_nemo14 = n_elements(iok)
SST_NEMO14 = SST_NEMO14[*,*,iok]
res_nemo14  = max(e1t, /nan)/1000. & help, res_nemo14
xdim_nemo14 = nxt
ydim_nemo14 = nyt
