print, ''
print, 'READING IFS-ECMWF FILES...'

; IFS-ECMWF
cmd = execute('file_'+strtrim(i,2)+' = "/homelocal-px/px-126/gsamson/WORK/ECMWF/IVAN_DOMAIN/ECMWF_ORCA025_20080213-20080217.nc" & help, file_'+strtrim(i,2))
cmd = execute('initncdf, file_'+strtrim(i,2))
cmd = execute('res_'+strtrim(i,2)+' = min(e1t, /nan)/1000.')
cmd = execute('lon_'+strtrim(i,2)+' = read_ncdf("lon", filename=file_'+strtrim(i,2)+', /allrecords, /nostruct)')
cmd = execute('lat_'+strtrim(i,2)+' = read_ncdf("lat", filename=file_'+strtrim(i,2)+', /allrecords, /nostruct)')
cmd = execute('xdim_'+strtrim(i,2)+' = n_elements(lon_'+strtrim(i,2)+'[*,0])')
cmd = execute('ydim_'+strtrim(i,2)+' = n_elements(lat_'+strtrim(i,2)+'[0,*])')


; TEMPS
;cmd = execute('time_ifs = ncdf_gettime(file_'+strtrim(i,2)+')')
dateini_ifs = 20080213.00d
juldini_ifs = date2jul(dateini_ifs)
juld_ifs = juldini_ifs + dindgen(jpt)/8. & help, juld_ifs
cmd = execute('date_'+strtrim(i,2)+' = jul2date(juld_ifs, hour=hour, day=day, month=month, year=year)')
cmd = execute('ibeg = where(date_'+strtrim(i,2)+' EQ 20080213.00d)')
cmd = execute('iend = where(date_'+strtrim(i,2)+' EQ 20080217.00d)')
cmd = execute('date_'+strtrim(i,2)+' = date_'+strtrim(i,2)+'[ibeg:iend]')


; LECTURE VARIABLES
cmd = execute('T2M_SEA_'+strtrim(i,2)+' = read_ncdf("sotemair", ibeg, iend, timestep=1, filename=file_'+strtrim(i,2)+', /nostruct) + 273.15')
cmd = execute('HU2M_SEA_'+strtrim(i,2)+' = read_ncdf("sohumrel", ibeg, iend, timestep=1, filename=file_'+strtrim(i,2)+', /nostruct)')
cmd = execute('ZON10M_SEA_'+strtrim(i,2)+' = read_ncdf("sowinu10", ibeg, iend, timestep=1, filename=file_'+strtrim(i,2)+', /nostruct)')
cmd = execute('MER10M_SEA_'+strtrim(i,2)+' = read_ncdf("sowinv10", ibeg, iend, timestep=1, filename=file_'+strtrim(i,2)+', /nostruct)')
cmd = execute('SWD_SEA_'+strtrim(i,2)+' = read_ncdf("sosudosw", ibeg, iend, timestep=1, filename=file_'+strtrim(i,2)+', /nostruct)')
cmd = execute('LWD_SEA_'+strtrim(i,2)+' = read_ncdf("sosudolw", ibeg, iend, timestep=1, filename=file_'+strtrim(i,2)+', /nostruct)')
cmd = execute('W10M_SEA_'+strtrim(i,2)+' = sqrt(ZON10M_SEA_'+strtrim(i,2)+'^2+MER10M_SEA_'+strtrim(i,2)+'^2)')
cmd = execute('tdim_'+strtrim(i,2)+' = n_elements(W10M_SEA_'+strtrim(i,2)+'[0,0,*]) & help, tdim_'+strtrim(i,2))


; MASK IFS (ORCA 0.25)
file_mask = '/dataref/rd/INITIALISATION/ORCA025_LIM/ORCA025_bathy_etopo1_gebco1_smoothed_coast_corrected_sept09.nc'
dom_tc = [glamt[0,0],glamt[jpi-1,0],gphit[0,0],gphit[0,jpj-1]]
initncdf, file_mask
bathy = read_ncdf('Bathymetry', boxzoom=dom_tc, filename=file_mask, /nostruct) & help, bathy
cmd = execute('mask_'+strtrim(i,2)+' = bathy * !VALUES.F_NAN & mask_'+strtrim(i,2)+'[where(bathy NE 0.)] = 1. & help, mask_'+strtrim(i,2))
cmd = execute('mask_'+strtrim(i,2)+' = replicate_array(mask_'+strtrim(i,2)+', tdim_'+strtrim(i,2)+') & help, mask_'+strtrim(i,2))


; MASKAGE
FOR j = 0, nb_var-1 DO BEGIN
  var = var_list[j]
  cmd = execute(var+'_'+strtrim(i,2)+' = '+var+'_'+strtrim(i,2)+'*mask_'+strtrim(i,2))
  cmd = execute('help, '+var+'_'+strtrim(i,2))
ENDFOR
