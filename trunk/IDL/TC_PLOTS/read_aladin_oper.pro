print, ''
print, 'READING ALADIN-OPER FORECAST FILES...'


; ALADIN-OPER
reseau = (STRSPLIT(alt_list[i], 'R', /EXTRACT))[1]
hstart = '00H'
IF reseau EQ '00' THEN fct_time = hstart+'-54H' ELSE fct_time = hstart+'-42H'
cmd = execute('file_'+strtrim(i,2)+' = "/home/gsamson/WORK/ALADIN/OPER_PROD/ICMSHALAD_'+alt_list[i]+'_'+fct_time+'.nc" & help, file_'+strtrim(i,2))
cmd = execute('initncdf, file_'+strtrim(i,2))
domdef, dom_tc
print, 'RES:', max(e1t, /nan)/1000.


; TIME
cmd = execute('juld_ala = ncdf_gettime(file_'+strtrim(i,2)+')')
cmd = execute('date_'+strtrim(i,2)+' = jul2date(juld_ala) & help, date_'+strtrim(i,2))
cmd = execute('ibeg = max([max(where(date_'+strtrim(i,2)+' LE date_beg_obs)),0])')
cmd = execute('iend = min(where(date_'+strtrim(i,2)+' GE date_end_obs))')
IF iend EQ -1 THEN iend = jpt-1
cmd = execute('date_'+strtrim(i,2)+' = date_'+strtrim(i,2)+'[ibeg:iend]')
jpt = iend - ibeg + 1
print, jpt, ibeg, iend


; VARIABLES
cmd = execute('T2M_SEA_'+strtrim(i,2)+' = read_ncdf("T2M", ibeg, iend, timestep=1, filename=file_'+strtrim(i,2)+', /nostruct)')
cmd = execute('HU2M_SEA_'+strtrim(i,2)+' = read_ncdf("RH2M", ibeg, iend, timestep=1, filename=file_'+strtrim(i,2)+', /nostruct)')
cmd = execute('SST_'+strtrim(i,2)+' = read_ncdf("SST", ibeg, iend, timestep=1, filename=file_'+strtrim(i,2)+', /nostruct)')
cmd = execute('ZON10M_SEA_'+strtrim(i,2)+' = read_ncdf("ZON10M", ibeg, iend, timestep=1, filename=file_'+strtrim(i,2)+', /nostruct)')
cmd = execute('MER10M_SEA_'+strtrim(i,2)+' = read_ncdf("MER10M", ibeg, iend, timestep=1, filename=file_'+strtrim(i,2)+', /nostruct)')
cmd = execute('W10M_SEA_'+strtrim(i,2)+' = sqrt(ZON10M_SEA_'+strtrim(i,2)+'^2+MER10M_SEA_'+strtrim(i,2)+'^2)')
cmd = execute('MSLP_SEA_'+strtrim(i,2)+' = reform(exp(read_ncdf("MSLP", ibeg, iend, timestep=1, filename=file_'+strtrim(i,2)+', /nostruct))/100.)')

IF hstart EQ '03H' THEN BEGIN
cmd = execute('FMT_SEA_'+strtrim(i,2)+' = sqrt((read_ncdf("TENS.TURB.ZO", ibeg, iend, timestep=1, filename=file_'+strtrim(i,2)+', /nostruct))^2+(read_ncdf("SURFTENS.TURB.ME", ibeg, iend, timestep=1, filename=file_'+strtrim(i,2)+', /nostruct))^2)')
cmd = execute('LE_SEA_'+strtrim(i,2)+' = -1. * read_ncdf("FLU.LAT.MEVA", ibeg, iend, timestep=1, filename=file_'+strtrim(i,2)+', /nostruct)')
;cmd = execute('H_SEA_'+strtrim(i,2)+' = -1. * read_ncdf("FLU.CHA.SENS", ibeg, iend, timestep=1, filename=file_'+strtrim(i,2)+', /nostruct)')
cmd = execute('LWD_SEA_'+strtrim(i,2)+' = read_ncdf("SURFRAYT THER DE", ibeg, iend, timestep=1, filename=file_'+strtrim(i,2)+', /nostruct)')
cmd = execute('SWD_SEA_'+strtrim(i,2)+' = read_ncdf("SURFRAYT SOLA DE", ibeg, iend, timestep=1, filename=file_'+strtrim(i,2)+', /nostruct)')
;print, grossemoyenne(LE_SEA_2,'xy')

; DESACCUMULATION
cmd = execute('FMT_SEA_'+strtrim(i,2)+'[*,*,0] = 0.')
cmd = execute('FMT_SEA_'+strtrim(i,2)+' = FMT_SEA_'+strtrim(i,2)+' - shift(FMT_SEA_'+strtrim(i,2)+',0,0,1)')
;cmd = execute('FMT_SEA_'+strtrim(i,2)+'[*,*,0] = 0.')
cmd = execute('LE_SEA_'+strtrim(i,2)+'[*,*,0] = 0.')
cmd = execute('LE_SEA_'+strtrim(i,2)+' = LE_SEA_'+strtrim(i,2)+' - shift(LE_SEA_'+strtrim(i,2)+',0,0,1)')
;cmd = execute('LE_SEA_'+strtrim(i,2)+'[*,*,0] = 0.')
;cmd = execute('H_SEA_'+strtrim(i,2)+'[*,*,0] = 0.')
;cmd = execute('H_SEA_'+strtrim(i,2)+' = H_SEA_'+strtrim(i,2)+' - shift(H_SEA_'+strtrim(i,2)+',0,0,1)')
;cmd = execute('H_SEA_'+strtrim(i,2)+'[*,*,0] = 0.')
cmd = execute('LWD_SEA_'+strtrim(i,2)+'[*,*,0] = 0.')
cmd = execute('LWD_SEA_'+strtrim(i,2)+' = LWD_SEA_'+strtrim(i,2)+' - shift(LWD_SEA_'+strtrim(i,2)+',0,0,1)')
;cmd = execute('LWD_SEA_'+strtrim(i,2)+'[*,*,0] = 0.')
cmd = execute('SWD_SEA_'+strtrim(i,2)+'[*,*,0] = 0.')
cmd = execute('SWD_SEA_'+strtrim(i,2)+' = SWD_SEA_'+strtrim(i,2)+' - shift(SWD_SEA_'+strtrim(i,2)+',0,0,1)')
;cmd = execute('SWD_SEA_'+strtrim(i,2)+'[*,*,0] = 0.')
ind3h = indgen(9)
ind6h = indgen(jpt-9)+9
cmd = execute('FMT_SEA_'+strtrim(i,2)+'[*,*,ind3h] = FMT_SEA_'+strtrim(i,2)+'[*,*,ind3h] / (3.*3600.)')
cmd = execute('FMT_SEA_'+strtrim(i,2)+'[*,*,ind6h] = FMT_SEA_'+strtrim(i,2)+'[*,*,ind6h] / (6.*3600.)')
cmd = execute('LE_SEA_'+strtrim(i,2)+'[*,*,ind3h] = LE_SEA_'+strtrim(i,2)+'[*,*,ind3h] / (3.*3600.)')
cmd = execute('LE_SEA_'+strtrim(i,2)+'[*,*,ind6h] = LE_SEA_'+strtrim(i,2)+'[*,*,ind6h] / (6.*3600.)')
;cmd = execute('H_SEA_'+strtrim(i,2)+'[*,*,ind3h] = H_SEA_'+strtrim(i,2)+'[*,*,ind3h] / (3.*3600.)')
;cmd = execute('H_SEA_'+strtrim(i,2)+'[*,*,ind6h] = H_SEA_'+strtrim(i,2)+'[*,*,ind6h] / (6.*3600.)')
cmd = execute('LWD_SEA_'+strtrim(i,2)+'[*,*,ind3h] = LWD_SEA_'+strtrim(i,2)+'[*,*,ind3h] / (3.*3600.)')
cmd = execute('LWD_SEA_'+strtrim(i,2)+'[*,*,ind6h] = LWD_SEA_'+strtrim(i,2)+'[*,*,ind6h] / (6.*3600.)')
cmd = execute('SWD_SEA_'+strtrim(i,2)+'[*,*,ind3h] = SWD_SEA_'+strtrim(i,2)+'[*,*,ind3h] / (3.*3600.)')
cmd = execute('SWD_SEA_'+strtrim(i,2)+'[*,*,ind6h] = SWD_SEA_'+strtrim(i,2)+'[*,*,ind6h] / (6.*3600.)')

; on remplace la valeur "t0" par la valeur de l'analyse "6h" correspondante
cmd = execute('iok = where(date_1 EQ date_'+strtrim(i,2)+'[0])') & print, iok
cmd = execute('FMT_SEA_'+strtrim(i,2)+'[*,*,0] = FMT_SEA_1[*,*,iok]')
cmd = execute('LE_SEA_'+strtrim(i,2)+'[*,*,0] = LE_SEA_1[*,*,iok]')
;cmd = execute('H_SEA_'+strtrim(i,2)+'[*,*,0] = H_SEA_1[*,*,iok]')
cmd = execute('LWD_SEA_'+strtrim(i,2)+'[*,*,0] = LWD_SEA_1[*,*,iok]')
cmd = execute('SWD_SEA_'+strtrim(i,2)+'[*,*,0] = SWD_SEA_1[*,*,iok]')
ENDIF


; MASK
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
FOR j = 0, nb_var-1 DO BEGIN
  var = var_list[j]
  cmd = execute('test = n_elements('+var+'_'+strtrim(i,2)+')')
  IF test GT 0 THEN BEGIN
    IF var EQ 'MSLP_SEA' THEN cmd = execute(var+'_'+strtrim(i,2)+' = '+var+'_'+strtrim(i,2)+'*grosmask') ELSE $
    cmd = execute(var+'_'+strtrim(i,2)+' = '+var+'_'+strtrim(i,2)+'*mask_'+strtrim(i,2))
    cmd = execute('help, '+var+'_'+strtrim(i,2))
  ENDIF
ENDFOR


; DEGRADATION SPATIALE
IF degrad_aladin THEN BEGIN
  cmd = execute('lonout = glamt[firstxt:lastxt:3,0]')
  cmd = execute('latout = reform(gphit[0,firstyt:lastyt:3])')
  help, lonout, latout
  FOR j = 0, nb_var-1 DO BEGIN
    var = var_list[j]
    cmd = execute('test = n_elements('+var+'_'+strtrim(i,2)+')')
    IF test GT 0 THEN BEGIN
      cmd = execute(var+'_'+strtrim(i,2)+'d = fltarr(n_elements(lonout),n_elements(latout),jpt)')
      FOR k = 0, jpt-1 DO BEGIN
        cmd = execute(var+'_'+strtrim(i,2)+'d[*,*,k] = fromreg("bilinear",'+var+'_'+strtrim(i,2)+'[*,*,k],glamt[firstxt:lastxt,firstyt:lastyt],gphit[firstxt:lastxt,firstyt:lastyt],lonout,latout)')
      ENDFOR
      cmd = execute(var+'_'+strtrim(i,2)+' = '+var+'_'+strtrim(i,2)+'d')
      cmd = execute('help, '+var+'_'+strtrim(i,2))
    ENDIF
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
