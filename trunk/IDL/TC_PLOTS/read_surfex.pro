print, '' & print, 'READING SURFEX FILES...'


IF i EQ 1 THEN idate = -1
idate = (idate + 1) MOD n_elements(date_list)

; SURFEX
fic_name = 'AROMOUT_00H-??H_6h.nc'
exp_path = '/home/gsamson/WORK/AROME/TEST_CPL/'
print, 'SEARCH FILE: '+exp_path+'EXPS_'+STRMID(exp_name, 0, 7)+'/EXP_'+exp_name+'_'+date_list[idate]+'/'+fic_name+'...'
cmd = execute('file_'+strtrim(i,2)+' = FILE_SEARCH(exp_path+"EXPS_"+STRMID(exp_name, 0, 7)+"/EXP_"+exp_name+"_"+date_list[idate]+"/"+fic_name)')
cmd = execute('IF file_'+strtrim(i,2)+' EQ "" THEN STOP ELSE print, "FILE FOUND: "+file_'+strtrim(i,2))
;cmd = execute('file_'+strtrim(i,2)+' = exp_path+"EXPS_"+STRMID(exp_name, 0, 7)+"/EXP_"+exp_name+"_"+date_list[idate]+"/"+fic_name & print, file_'+strtrim(i,2))
cmd = execute('initncdf, file_'+strtrim(i,2))
domdef, dom_tc
print, 'RES:', max(e1t, /nan)/1000.

; TEMPS
cmd = execute( 'temps_sfx = ncdf_lec(file_'+strtrim(i,2)+', VAR="time")' )
cmd = execute( 'time_sfx = ncdf_gettime(file_'+strtrim(i,2)+') & help, time_sfx' )
cmd = execute( 'last_ech = STRMID((STRSPLIT(file_'+strtrim(i,2)+',"/", count=cnt, /EXTRACT))[cnt-1], 12, 2)' )
IF jpt NE (last_ech/6)+1 THEN STOP
;dateini_sfx = 20080200.00d + DOUBLE(STRMID(exp_name, 0, 2))
dateini_sfx =  DOUBLE(STRMID(date_list[idate],0,8)) + DOUBLE(STRMID(date_list[idate],9,2))/24.
print, dateini_sfx, format='(F11.2)'
juldini_sfx = date2jul(dateini_sfx)
cmd = execute('juld_'+strtrim(i,2)+' = juldini_sfx + dindgen(jpt)/4.')
cmd = execute('date_'+strtrim(i,2)+' = jul2date(juld_'+strtrim(i,2)+', hour=hour, day=day, month=month, year=year)')
ibeg = 0
iend = jpt-1
cmd = execute('iend = where(date_'+strtrim(i,2)+' EQ date_end_obs)')
IF iend EQ -1 THEN iend = jpt-1
cmd = execute('date_'+strtrim(i,2)+' = date_'+strtrim(i,2)+'[ibeg:iend]')
cmd = execute('juld_'+strtrim(i,2)+' = juld_'+strtrim(i,2)+'[ibeg:iend]')
jpt = iend - ibeg + 1
help, jpt, ibeg, iend

cmd = execute('lon_'+strtrim(i,2)+' = glamt[firstxt:lastxt,0] & help, lon_'+strtrim(i,2))
cmd = execute('lat_'+strtrim(i,2)+' = reform(gphit[0,firstyt:lastyt]) & help, lat_'+strtrim(i,2))


; LECTURE VARIABLES+MASK
FOR j = 0, nb_var-1 DO BEGIN
      var = var_list[j]
      cmd = execute(var+'_'+strtrim(i,2)+' = read_ncdf("'+var+'", ibeg, iend, timestep=1, filename=file_'+strtrim(i,2)+', /nostruct, /allrecords)')
      cmd = execute('help, '+var+'_'+strtrim(i,2))
      IF j EQ 0 THEN BEGIN
        cmd = execute('mask = '+var+'_'+strtrim(i,2)+'[*,*,0] * 0. + 1.')
        cmd = execute('mask[where(finite('+var+'_'+strtrim(i,2)+'[*,*,0]) EQ 0.)] = !Values.F_NAN & help, mask')
        cmd = execute('tdim_'+strtrim(i,2)+' = n_elements('+var+'_'+strtrim(i,2)+'[0,0,*]) & help, tdim_'+strtrim(i,2))

nb_extendmask = 6
newmask = mask

FOR m = 0, nb_extendmask-1 DO BEGIN

  FOR l = 1, n_elements(gphit[0,firstyt:lastyt])-2 DO BEGIN
    FOR k = 1, n_elements(glamt[firstxt:lastxt,0])-2 DO BEGIN
      IF finite(mask[k,l]) EQ 0. THEN BEGIN
	newmask[k-1,l-1] = !VALUES.F_NAN
	newmask[k-1,l  ] = !VALUES.F_NAN
	newmask[k-1,l+1] = !VALUES.F_NAN
	newmask[k  ,l-1] = !VALUES.F_NAN
	newmask[k  ,l+1] = !VALUES.F_NAN
	newmask[k+1,l-1] = !VALUES.F_NAN
	newmask[k+1,l  ] = !VALUES.F_NAN
	newmask[k+1,l+1] = !VALUES.F_NAN
      ENDIF
    ENDFOR
  ENDFOR
  mask = newmask

ENDFOR

mask = temporary(newmask) & help, mask
cmd = execute('mask_'+strtrim(i,2)+' = replicate_array(mask, tdim_'+strtrim(i,2)+') & help, mask_'+strtrim(i,2))

  ENDIF
  IF var EQ 'MSLP_SEA' THEN cmd = execute(var+'_'+strtrim(i,2)+' = reform('+var+'_'+strtrim(i,2)+')/100.*mask_'+strtrim(i,2))
  IF var EQ 'FM_SEA'   THEN cmd = execute(var+'_'+strtrim(i,2)+' = '+var+'_'+strtrim(i,2)+' * mask_'+strtrim(i,2))
  IF var EQ 'HLE_SEA'  THEN cmd = execute(var+'_'+strtrim(i,2)+' = '+var+'_'+strtrim(i,2)+' * mask_'+strtrim(i,2))


  IF var EQ 'SST' THEN BEGIN
    IF exp_name EQ 'NOPRES_COARE_12' OR exp_name EQ 'NOPRES_ECUME_12' THEN $
    cmd = execute(var+'_'+strtrim(i,2)+'[*,*,0] = fromreg("bilinear", SSTR1_0, lon_sstr1, lat_sstr1, lon_'+strtrim(i,2)+', lat_'+strtrim(i,2)+')')
    IF exp_name EQ 'NOPRES_COARE_13' OR exp_name EQ 'NOPRES_ECUME_13' THEN $
    cmd = execute(var+'_'+strtrim(i,2)+'[*,*,0] = fromreg("bilinear", SSTR2_0, lon_sstr2, lat_sstr2, lon_'+strtrim(i,2)+', lat_'+strtrim(i,2)+')')
    IF exp_name EQ 'NOPRES_COARE_14' OR exp_name EQ 'NOPRES_ECUME_14' THEN $
    cmd = execute(var+'_'+strtrim(i,2)+'[*,*,0] = fromreg("bilinear", SSTR3_0, lon_sstr3, lat_sstr3, lon_'+strtrim(i,2)+', lat_'+strtrim(i,2)+')')
  ENDIF
    
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

cmd = execute('xdim_'+strtrim(i,2)+' = n_elements(lon_'+strtrim(i,2)+')')
cmd = execute('ydim_'+strtrim(i,2)+' = n_elements(lat_'+strtrim(i,2)+')')
cmd = execute('lon_'+strtrim(i,2)+' = replicate_array(lon_'+strtrim(i,2)+', ydim_'+strtrim(i,2)+')')
cmd = execute('lat_'+strtrim(i,2)+' = transpose(replicate_array(lat_'+strtrim(i,2)+', xdim_'+strtrim(i,2)+'))')
cmd = execute('res_'+strtrim(i,2)+' = max(e1t, /nan)/1000. & help, res_'+strtrim(i,2))
