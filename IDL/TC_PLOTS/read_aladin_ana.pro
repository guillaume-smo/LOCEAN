print, ''
print, 'READING ALADIN-OPER ANALYSE FILES...'

ech='0'
path_aladana = "/homelocal-px/px-126/gsamson/WORK/ALADIN/OPER_ASSIM_ECH"+ech+"/"
file_aladana = "ICMSHALAD+000"+ech+"_200802_GRIDOK.nc"


; ALADIN
cmd = execute('file_'+strtrim(i,2)+' = path_aladana + file_aladana & help, file_'+strtrim(i,2))
cmd = execute('initncdf, file_'+strtrim(i,2))
domdef, dom_tc


; TIME
cmd = execute('juld_ala = ncdf_gettime(file_'+strtrim(i,2)+')')
cmd = execute('date_'+strtrim(i,2)+' = jul2date(juld_ala, hour=hour, day=day, month=month, year=year)')
cmd = execute('ibeg = where(date_'+strtrim(i,2)+' EQ date_beg_obs)')
cmd = execute('iend = where(date_'+strtrim(i,2)+' EQ date_end_obs)')
cmd = execute('date_'+strtrim(i,2)+' = date_'+strtrim(i,2)+'[ibeg:iend]')
help, jpt, ibeg, iend


; VARIABLES
;cmd = execute('T2M_SEA_'+strtrim(i,2)+' = read_ncdf("sotemair", ibeg, iend, timestep=1, filename=file_'+strtrim(i,2)+', /nostruct) + 273.15')
;cmd = execute('HU2M_SEA_'+strtrim(i,2)+' = read_ncdf("sohumrel", ibeg, iend, timestep=1, filename=file_'+strtrim(i,2)+', /nostruct)')
;cmd = execute('ZON10M_SEA_'+strtrim(i,2)+' = read_ncdf("sowinu10", ibeg, iend, timestep=1, filename=file_'+strtrim(i,2)+', /nostruct)')
;cmd = execute('MER10M_SEA_'+strtrim(i,2)+' = read_ncdf("sowinv10", ibeg, iend, timestep=1, filename=file_'+strtrim(i,2)+', /nostruct)')
;cmd = execute('SWD_SEA_'+strtrim(i,2)+' = read_ncdf("sosudosw", ibeg, iend, timestep=1, filename=file_'+strtrim(i,2)+', /nostruct)')
;cmd = execute('LWD_SEA_'+strtrim(i,2)+' = read_ncdf("sosudolw", ibeg, iend, timestep=1, filename=file_'+strtrim(i,2)+', /nostruct)')
;cmd = execute('W10M_SEA_'+strtrim(i,2)+' = sqrt(ZON10M_SEA_'+strtrim(i,2)+'^2+MER10M_SEA_'+strtrim(i,2)+'^2)')
;cmd = execute('MSLP_SEA_'+strtrim(i,2)+' = read_ncdf("somslpre", ibeg, iend, timestep=1, filename=file_'+strtrim(i,2)+', /nostruct) / 100.')
;cmd = execute('SST_'+strtrim(i,2)+' = read_ncdf("sst", ibeg, iend, timestep=1, filename=file_'+strtrim(i,2)+', /nostruct)')

cmd = execute('T2M_SEA_'+strtrim(i,2)+'    = read_ncdf("T2M", ibeg, iend, timestep=1, filename=file_'+strtrim(i,2)+', /nostruct)')
cmd = execute('HU2M_SEA_'+strtrim(i,2)+'   = read_ncdf("RH2M", ibeg, iend, timestep=1, filename=file_'+strtrim(i,2)+', /nostruct)')
cmd = execute('ZON10M_SEA_'+strtrim(i,2)+' = read_ncdf("ZON10M", ibeg, iend, timestep=1, filename=file_'+strtrim(i,2)+', /nostruct)')
cmd = execute('MER10M_SEA_'+strtrim(i,2)+' = read_ncdf("MER10M", ibeg, iend, timestep=1, filename=file_'+strtrim(i,2)+', /nostruct)')
cmd = execute('W10M_SEA_'+strtrim(i,2)+'   = sqrt(ZON10M_SEA_'+strtrim(i,2)+'^2+MER10M_SEA_'+strtrim(i,2)+'^2)')
cmd = execute('MSLP_SEA_'+strtrim(i,2)+'   = reform(read_ncdf("MSLP_SEA", ibeg, iend, timestep=1, filename=file_'+strtrim(i,2)+', /nostruct)) / 100.')
cmd = execute('SST_'+strtrim(i,2)+'        = read_ncdf("SST", ibeg, iend, timestep=1, filename=file_'+strtrim(i,2)+', /nostruct)')

IF ech EQ '6' THEN BEGIN
  cmd = execute('FMT_SEA_'+strtrim(i,2)+' = sqrt((read_ncdf("TENS.TURB.ZO", ibeg, iend, timestep=1, filename=file_'+strtrim(i,2)+', /nostruct)/(6.*3600.))^2+(read_ncdf("SURFTENS.TURB.ME", ibeg, iend, timestep=1, filename=file_'+strtrim(i,2)+', /nostruct)/(6.*3600.))^2)')
  cmd = execute('LE_SEA_'+strtrim(i,2)+'  = -1. * read_ncdf("FLU.LAT.MEVA", ibeg, iend, timestep=1, filename=file_'+strtrim(i,2)+', /nostruct)/(6.*3600.)')
  cmd = execute('H_SEA_'+strtrim(i,2)+'   = -1. * read_ncdf("FLU.CHA.SENS", ibeg, iend, timestep=1, filename=file_'+strtrim(i,2)+', /nostruct)/(6.*3600.)')
  cmd = execute('LWD_SEA_'+strtrim(i,2)+' = read_ncdf("SURFRAYT THER DE", ibeg, iend, timestep=1, filename=file_'+strtrim(i,2)+', /nostruct)/(6.*3600.)')
  cmd = execute('SWD_SEA_'+strtrim(i,2)+' = read_ncdf("SURFRAYT SOLA DE", ibeg, iend, timestep=1, filename=file_'+strtrim(i,2)+', /nostruct)/(6.*3600.)')
ENDIF


cmd = execute('mask = read_ncdf("IND.TERREMER", ibeg, iend, timestep=1, filename=file_'+strtrim(i,2)+', /nostruct) & help, mask')
mask[where(mask EQ 1.)] = !VALUES.F_NAN
mask[where(mask EQ 0.)] = 1.
cmd = execute('mask_'+strtrim(i,2)+' = mask & help, mask_'+strtrim(i,2))

newmask = mask[*,*,0]
FOR l = 1, n_elements(gphit[0,firstyt:lastyt])-2 DO BEGIN
  FOR k = 1, n_elements(glamt[firstxt:lastxt,0])-2 DO BEGIN
    IF finite(mask[k,l]) EQ 0. THEN BEGIN
      newmask[k-1,l-1] = !VALUES.F_NAN
      newmask[k-1,l  ] = !VALUES.F_NAN
      newmask[k-1,l+1] = !VALUES.F_NAN
      newmask[k,l-1] = !VALUES.F_NAN
      newmask[k,l+1] = !VALUES.F_NAN
      newmask[k+1,l-1] = !VALUES.F_NAN
      newmask[k+1,l  ] = !VALUES.F_NAN
      newmask[k+1,l+1] = !VALUES.F_NAN
    ENDIF
  ENDFOR
ENDFOR
mask = newmask
FOR l = 1, n_elements(gphit[0,firstyt:lastyt])-2 DO BEGIN
  FOR k = 1, n_elements(glamt[firstxt:lastxt,0])-2 DO BEGIN
    IF finite(mask[k,l]) EQ 0. THEN BEGIN
      newmask[k-1,l-1] = !VALUES.F_NAN
      newmask[k-1,l  ] = !VALUES.F_NAN
      newmask[k-1,l+1] = !VALUES.F_NAN
      newmask[k,l-1] = !VALUES.F_NAN
      newmask[k,l+1] = !VALUES.F_NAN
      newmask[k+1,l-1] = !VALUES.F_NAN
      newmask[k+1,l  ] = !VALUES.F_NAN
      newmask[k+1,l+1] = !VALUES.F_NAN
    ENDIF
  ENDFOR
ENDFOR
grosmask = temporary(newmask) & help, grosmask
;cmd = execute('grosmask = replicate_array(grosmask, tdim_'+strtrim(i,2)+') & help, grosmask')
grosmask = replicate_array(grosmask, jpt) & help, grosmask



; MASKAGE
;FOR j = 0, nb_var-1 DO BEGIN
;  var = var_list[j]
;  cmd = execute(var+'_'+strtrim(i,2)+' = '+var+'_'+strtrim(i,2)+'*mask_'+strtrim(i,2))
;  cmd = execute('help, '+var+'_'+strtrim(i,2))
;ENDFOR

FOR j = 0, nb_var-1 DO BEGIN
  var = var_list[j]
  IF var EQ 'MSLP_SEA' THEN cmd = execute(var+'_'+strtrim(i,2)+' = '+var+'_'+strtrim(i,2)+'*grosmask') ELSE $
  cmd = execute(var+'_'+strtrim(i,2)+' = '+var+'_'+strtrim(i,2)+'*mask_'+strtrim(i,2))
  cmd = execute('help, '+var+'_'+strtrim(i,2))
ENDFOR


; DEGRADATION SPATIALE
IF degrad_aladin THEN BEGIN
  cmd = execute('lonout = glamt[firstxt:lastxt:3,0]')
  cmd = execute('latout = reform(gphit[0,firstyt:lastyt:3])')
  help, lonout, latout
  FOR j = 0, nb_var-1 DO BEGIN
    var = var_list[j]
    cmd = execute(var+'_'+strtrim(i,2)+'d = fltarr(n_elements(lonout),n_elements(latout),jpt)')
    FOR k = 0, jpt-1 DO BEGIN
      cmd = execute(var+'_'+strtrim(i,2)+'d[*,*,k] = fromreg("bilinear",'+var+'_'+strtrim(i,2)+'[*,*,k],glamt[firstxt:lastxt,firstyt:lastyt],gphit[firstxt:lastxt,firstyt:lastyt],lonout,latout)')
    ENDFOR
    cmd = execute(var+'_'+strtrim(i,2)+' = '+var+'_'+strtrim(i,2)+'d')
    cmd = execute('help, '+var+'_'+strtrim(i,2))
  ENDFOR
  computegrid, xaxis=lonout, yaxis=latout
  help, glamt, gphit
ENDIF


; DIMENSIONS
cmd = execute('tdim_'+strtrim(i,2)+' = jpt & help, tdim_'+strtrim(i,2))
cmd = execute('lon_'+strtrim(i,2)+' = glamt[firstxt:lastxt,0] & help, lon_'+strtrim(i,2))
cmd = execute('lat_'+strtrim(i,2)+' = reform(gphit[0,firstyt:lastyt]) & help, lat_'+strtrim(i,2))
cmd = execute('xdim_'+strtrim(i,2)+' = n_elements(lon_'+strtrim(i,2)+') & help, xdim_'+strtrim(i,2))
cmd = execute('ydim_'+strtrim(i,2)+' = n_elements(lat_'+strtrim(i,2)+') & help, ydim_'+strtrim(i,2))
cmd = execute('res_'+strtrim(i,2)+' = max(e1t, /nan)/1000. & help, res_'+strtrim(i,2))
