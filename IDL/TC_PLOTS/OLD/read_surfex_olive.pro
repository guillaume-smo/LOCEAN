print, ''
print, 'READING SURFEX FILES...'


; SURFEX
fic_name = 'AROMOUT_00H-96H_6h.nc'
exp_path = '/home/gsamson/WORK/AROME/OLIVE/'

cmd = execute('file_'+strtrim(i,2)+' = exp_path+exp_name+"/"+fic_name & print, file_'+strtrim(i,2))
cmd = execute('initncdf, file_'+strtrim(i,2))
domdef, dom_tc
print, 'RES:', max(e1t, /nan)/1000.

; TEMPS
cmd = execute('temps_sfx = ncdf_lec(file_'+strtrim(i,2)+', VAR="time")')
cmd = execute('time_sfx = ncdf_gettime(file_'+strtrim(i,2)+') & help, time_sfx')

IF exp_name EQ '9AZ2+9AZ1' OR exp_name EQ '9AZN+9AZO' OR exp_name EQ '9AZH+9AZG' OR exp_name EQ '9B15+9B16' $
OR exp_name EQ '9B1C+9B1D' OR exp_name EQ '9B1L+9B1M' OR exp_name EQ '9B3L+9B3M' OR exp_name EQ '9B4T+9B4S' $
THEN dateini_sfx = 20080212.00d

IF exp_name EQ '9B0I+9B0J' OR exp_name EQ '9B0B+9B0C' OR exp_name EQ '9B18+9B17' OR exp_name EQ '9B1F+9B1G' OR $
   exp_name EQ '9B1O+9B1N' OR exp_name EQ '9B3N+9B3O' OR exp_name EQ '9B4X+9B4W' THEN dateini_sfx = 20080212.50d

IF exp_name EQ '9AWS+9AWT' OR exp_name EQ '9AXW+9AXX' OR exp_name EQ '9AXK+9AXL' OR exp_name EQ '9AXV+9AXU' $
OR exp_name EQ '9AX1+9AX2' OR exp_name EQ '9B3P+9B3Q' OR exp_name EQ '9B4Y+9B4Z' THEN dateini_sfx = 20080213.00d

IF exp_name EQ '9B09+9B0A' OR exp_name EQ '9B0D+9B0E' OR exp_name EQ '9B19+9B1A' OR exp_name EQ '9B1H+9B1I' OR $
   exp_name EQ '9B1P+9B1Q' OR exp_name EQ '9B3R+9B3S' OR exp_name EQ '9B51+9B50' THEN dateini_sfx = 20080213.50d

IF exp_name EQ '9AZ4+9AZ3' OR exp_name EQ '9AZP+9AZQ' OR exp_name EQ '9AZJ+9AZI' OR exp_name EQ '9B13+9B12' OR $
   exp_name EQ '9B1J+9B1K' OR exp_name EQ '9B1R+9B1S' OR exp_name EQ '9B3T+9B3U' OR exp_name EQ '9B53+9B52' $
THEN dateini_sfx = 20080214.00d 


;IF temps_sfx[0] EQ 66700800 THEN dateini_sfx = 20080212.00d
help, dateini_sfx

juldini_sfx = date2jul(dateini_sfx)
juld_sfx = juldini_sfx + dindgen(jpt)/4. & help, juld_sfx
cmd = execute('date_'+strtrim(i,2)+' = jul2date(juld_sfx, hour=hour, day=day, month=month, year=year)')
ibeg = 0
iend = jpt-1
;cmd = execute('ibeg = where(date_'+strtrim(i,2)+' EQ 20080213.00d)')
;IF ibeg EQ -1 THEN ibeg = 0
cmd = execute('iend = where(date_'+strtrim(i,2)+' EQ date_end_obs)')
IF iend EQ -1 THEN iend = jpt-1
cmd = execute('date_'+strtrim(i,2)+' = date_'+strtrim(i,2)+'[ibeg:iend]')
jpt = iend - ibeg + 1
help, jpt, ibeg, iend


; LECTURE VARIABLES+MASK
FOR j = 0, nb_var-1 DO BEGIN
      var = var_list[j]
      cmd = execute(var+'_'+strtrim(i,2)+' = read_ncdf("'+var+'", ibeg, iend, timestep=1, filename=file_'+strtrim(i,2)+', /nostruct, /allrecords)')
      cmd = execute('help, '+var+'_'+strtrim(i,2))
      IF j EQ 0 THEN BEGIN
        cmd = execute('mask = '+var+'_'+strtrim(i,2)+'[*,*,0] * 0. + 1.')
        cmd = execute('mask[where(finite('+var+'_'+strtrim(i,2)+'[*,*,0]) EQ 0.)] = !Values.F_NAN & help, mask')
        cmd = execute('tdim_'+strtrim(i,2)+' = n_elements('+var+'_'+strtrim(i,2)+'[0,0,*]) & help, tdim_'+strtrim(i,2))

newmask = mask
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
mask = temporary(newmask) & help, mask
cmd = execute('mask_'+strtrim(i,2)+' = replicate_array(mask, tdim_'+strtrim(i,2)+') & help, mask_'+strtrim(i,2))

  ENDIF
  IF var EQ 'MSLP_SEA' THEN cmd = execute(var+'_'+strtrim(i,2)+' = reform('+var+'_'+strtrim(i,2)+')/100.*mask_'+strtrim(i,2))
  IF var EQ 'FMT_SEA'  THEN cmd = execute(var+'_'+strtrim(i,2)+'[where(finite(mask_'+strtrim(i,2)+') EQ 0)] = !Values.F_NAN')
ENDFOR


IF degrad_surfex THEN BEGIN
  cmd = execute('lonout = glamt[0:n_elements(lon_'+strtrim(i,2)+')-1:8,0]')
  cmd = execute('latout = reform(gphit[0,0:n_elements(lat_'+strtrim(i,2)+')-1:8])')
  help, lonout, latout
  FOR j = 0, nb_var-1 DO BEGIN
    var = var_list[j]
    cmd = execute(var+'_'+strtrim(i,2)+'d = fltarr(n_elements(lonout),n_elements(latout),jpt)')
    FOR k = 0, jpt-1 DO BEGIN
      cmd = execute(var+'_'+strtrim(i,2)+'d[*,*,k] = fromreg("bilinear",'+var+'_'+strtrim(i,2)+'[*,*,k],glamt,gphit,lonout,latout)')
    ENDFOR
    cmd = execute(var+'_'+strtrim(i,2)+' = '+var+'_'+strtrim(i,2)+'d')
    cmd = execute('help, '+var+'_'+strtrim(i,2))
  ENDFOR
  computegrid, xaxis=lonout, yaxis=latout
  help, glamt, gphit
ENDIF

cmd = execute('lon_'+strtrim(i,2)+' = glamt[firstxt:lastxt,0] & help, lon_'+strtrim(i,2))
cmd = execute('lat_'+strtrim(i,2)+' = reform(gphit[0,firstyt:lastyt]) & help, lat_'+strtrim(i,2))
cmd = execute('xdim_'+strtrim(i,2)+' = n_elements(lon_'+strtrim(i,2)+')')
cmd = execute('ydim_'+strtrim(i,2)+' = n_elements(lat_'+strtrim(i,2)+')')
cmd = execute('lon_'+strtrim(i,2)+' = replicate_array(lon_'+strtrim(i,2)+', ydim_'+strtrim(i,2)+')')
cmd = execute('lat_'+strtrim(i,2)+' = transpose(replicate_array(lat_'+strtrim(i,2)+', xdim_'+strtrim(i,2)+'))')
;cmd = execute('res_'+strtrim(i,2)+'  = 4.') ; (km)
cmd = execute('res_'+strtrim(i,2)+' = max(e1t, /nan)/1000. & help, res_'+strtrim(i,2))
