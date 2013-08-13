print, '' & print, 'READING NEMO CPL FILE...'


fic_name = 'IVAN12_gridT_00H-96H_6h.nc'
exp_path = '/home/gsamson/WORK/AROME/TEST_CPL/'
print, 'SEARCH FILE: '+exp_path+'EXPS_'+STRMID(exp_name, 0, 7)+'/EXP_'+exp_name+'_'+date_list[idate]+'/'+fic_name+'...'
cmd = execute('filen_'+strtrim(i,2)+' = FILE_SEARCH(exp_path+"EXPS_"+STRMID(exp_name, 0, 7)+"/EXP_"+exp_name+"_"+date_list[idate]+"/"+fic_name)')
cmd = execute('IF filen_'+strtrim(i,2)+' EQ "" THEN STOP ELSE print, "FILE FOUND: "+filen_'+strtrim(i,2))
cmd = execute('initncdf, filen_'+strtrim(i,2))
domdef, dom_tc
print, 'RES:', max(e1t, /nan)/1000.

cmd = execute('juldn_'+strtrim(i,2)+' = ncdf_gettime(filen_'+strtrim(i,2)+')')
cmd = execute('juldinin_'+strtrim(i,2)+' = juldn_'+strtrim(i,2)+'[0]')
cmd = execute('juldn_'+strtrim(i,2)+' = juldinin_'+strtrim(i,2)+' + dindgen(jpt)*0.25')
ibeg = 0
iend = jpt-1

cmd = execute('ohc_'+strtrim(i,2)+' = reform(read_ncdf("ohc26", ibeg, iend, timestep=1, filename=filen_'+strtrim(i,2)+', /nostruct))')
cmd = execute('dept26_'+strtrim(i,2)+' = reform(read_ncdf("dept26", ibeg, iend, timestep=1, filename=filen_'+strtrim(i,2)+', /nostruct))')
cmd = execute('help, ohc_'+strtrim(i,2)+', dept26_'+strtrim(i,2))

cmd = execute('lonn_'+strtrim(i,2)+'  = glamt[firstxt:lastxt,0] & help,lonn_'+strtrim(i,2))
cmd = execute('latn_'+strtrim(i,2)+'  = reform(gphit[0,firstyt:lastyt]) & help, latn_'+strtrim(i,2))
cmd = execute('xdimn_'+strtrim(i,2)+' = n_elements(lonn_'+strtrim(i,2)+') & help, xdimn_'+strtrim(i,2))
cmd = execute('ydimn_'+strtrim(i,2)+' = n_elements(latn_'+strtrim(i,2)+') & help, ydimn_'+strtrim(i,2))
cmd = execute('resn_'+strtrim(i,2)+'  = max(e1t, /nan)/1000. & help, resn_'+strtrim(i,2))

; interpolation temporelle (recntrage à 6h au lieu de 3h)
cmd = execute( 'ohc_'+strtrim(i,2)+' = INTERPOLATE( ohc_'+strtrim(i,2)+', findgen(xdimn_'+strtrim(i,2)+'), findgen(ydimn_'+strtrim(i,2)+'), [0, findgen((jpt-1))+0.5, jpt-1 ], /GRID)' )
cmd = execute( 'dept26_'+strtrim(i,2)+' = INTERPOLATE( dept26_'+strtrim(i,2)+', findgen(xdimn_'+strtrim(i,2)+'), findgen(ydimn_'+strtrim(i,2)+'), [0, findgen((jpt-1))+0.5, jpt-1 ], /GRID)' )
cmd = execute('help, ohc_'+strtrim(i,2)+', dept26_'+strtrim(i,2))

cmd = execute( 'tdimn_'+strtrim(i,2)+' = n_elements(ohc_'+strtrim(i,2)+'[0,0,*]) & help, tdimn_'+strtrim(i,2) )
cmd = execute( 'juldn_'+strtrim(i,2)+' = FLOOR(juldinin_'+strtrim(i,2)+')+0.50d + dindgen(tdimn_'+strtrim(i,2)+')/4.' )
cmd = execute( 'daten_'+strtrim(i,2)+' = jul2date(juldn_'+strtrim(i,2)+')' )
