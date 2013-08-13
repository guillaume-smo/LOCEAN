print, '' & print, 'EXTRACTION DATA MODEL...'


; variables locales
cmd = execute( 'wnd10m = W10M_SEA_'+strtrim(i,2))
cmd = execute( 'mslp   = MSLP_SEA_'+strtrim(i,2))  
cmd = execute( 'tdim   = tdim_'+strtrim(i,2))
cmd = execute( 'xdim   = xdim_'+strtrim(i,2))
cmd = execute( 'ydim   = ydim_'+strtrim(i,2))
cmd = execute( 'res    = res_'+strtrim(i,2))
cmd = execute( 'file   = file_'+strtrim(i,2) )


;------------------------------------------------------------------------------------------------------------------
FOR j = 0, tdim-1 DO BEGIN

  ; MIN MSLP
  min_mslp[j] = min(mslp[firstxt+1:lastxt-1,firstyt+1:lastyt-1,j], /nan)      
  ind_minmslp = where(mslp[*,*,j] EQ min_mslp[j])
  lon_mslp[j] = glamt[ind_minmslp]
  lat_mslp[j] = gphit[ind_minmslp]
  imin = (array_indices(mslp[*,*,j],ind_minmslp))[0]-10
  cmd  = execute('imax = min([(array_indices(mslp[*,*,j],ind_minmslp))[0]+10,xdim_'+strtrim(i,2)+'-1])')
  jmin = (array_indices(mslp[*,*,j],ind_minmslp))[1]-10
  cmd  = execute('jmax = min([(array_indices(mslp[*,*,j],ind_minmslp))[1]+10,ydim_'+strtrim(i,2)+'-1])')
;      print, j, 'min mslp:', min(mslp[*,*,j], /nan),' @',lon_mslp[j],lat_mslp[j], imin, jmin

  ; MAX WIND
  max_w10m[j] = max(wnd10m[firstxt+1:lastxt-1,firstyt+1:lastyt-1,j], /nan)
  ind_maxwnd = where(wnd10m[*,*,j] EQ max_w10m[j])
  lon_maxwnd[j] = glamt[ind_maxwnd]
  lat_maxwnd[j] = gphit[ind_maxwnd]
  imin = max([(array_indices(wnd10m[*,*,j],ind_maxwnd))[0]-10,0])
  cmd  = execute('imax = min([(array_indices(wnd10m[*,*,j],ind_maxwnd))[0]+10,xdim_'+strtrim(i,2)+'-1])')
  jmin = max([(array_indices(wnd10m[*,*,j],ind_maxwnd))[1]-10,0])
  cmd  = execute('jmax = min([(array_indices(wnd10m[*,*,j],ind_maxwnd))[1]+10,ydim_'+strtrim(i,2)+'-1])')
;      print, 'max wnd:', max(wnd10m[*,*,j], /nan),' @',lon_maxwnd[j],lat_maxwnd[j]

  ; MAX VOR (useless)
;      ind_maxvor = where(vor10m[*,*,j] EQ min(vor10m[imin:imax,jmin:jmax,j], /nan))
;      lon_maxvor[j] = glamt[ind_maxvor]
;      lat_maxvor[j] = gphit[ind_maxvor]
;      print, 'max vor:', min(vor10m[imin:imax,jmin:jmax,j], /nan),' @',lon_maxvor[j],lat_maxvor[j]
;      imin = (array_indices(vor10m[*,*,j],ind_maxvor))[0]-5
;      cmd = execute('imax = min([(array_indices(vor10m[*,*,j],ind_maxvor))[0]+5,xdim_'+strtrim(i,2)+'-1])')
;      jmin = (array_indices(vor10m[*,*,j],ind_maxvor))[1]-5
;      cmd = execute('jmax = min([(array_indices(vor10m[*,*,j],ind_maxvor))[1]+5,ydim_'+strtrim(i,2)+'-1])')
;      print, 'vor imin, imax, jmin, jmax:', imin, imax, jmin, jmax

  ; MIN WIND
  ind_minwnd    = where(wnd10m[imin:imax,jmin:jmax,j] EQ min(wnd10m[imin:imax,jmin:jmax,j], /nan))
  lon_minwnd[j] = (glamt[imin:imax,jmin:jmax])[ind_minwnd]
  lat_minwnd[j] = (gphit[imin:imax,jmin:jmax])[ind_minwnd]
;    print, 'min wnd:', min(wnd10m[imin:imax,jmin:jmax,j], /nan),' @',lon_minwnd[j],lat_minwnd[j]
  
  IF finite(lon_mslp[j]) AND finite(lat_mslp[j]) THEN BEGIN
    @calcul_index_extract
  ENDIF


  ; SAUVEGARDE DES VARIABLES EXTRAITES
  cmd = execute('lon_2DTC_'+strtrim(i,2)+'[ibeg:iend,jbeg:jend,j] = glamt[imin:imax,jmin:jmax]')
  cmd = execute('lat_2DTC_'+strtrim(i,2)+'[ibeg:iend,jbeg:jend,j] = gphit[imin:imax,jmin:jmax]')
  FOR k = 0, nb_var-1 DO BEGIN
    var = var_list[k]
    cmd = execute('test = n_elements('+var+'_'+strtrim(i,2)+')')
    IF test GT 0 THEN BEGIN
      cmd = execute('tmp = '+var+'_'+strtrim(i,2)+'[*,*,j]')
      tmp[ind_bad] = !Values.F_NAN
      cmd = execute(var+'_1D_'+strtrim(i,2)+'[j] = avg('+var+'_'+strtrim(i,2)+'[*,*,j], /nan)')
      cmd = execute(var+'_1DTC_'+strtrim(i,2)+'[j] = avg(tmp[imin:imax,jmin:jmax], /nan)')
      cmd = execute(var+'_2DTC_'+strtrim(i,2)+'[ibeg:iend,jbeg:jend,j] = tmp[imin:imax,jmin:jmax]')	
      ; calcul moyenne azimuthale
      FOR r = 0, long(radius/res_rad)-1 DO BEGIN
	rok = where((dist_tc GE r*res_rad) AND (dist_tc LE (r+1)*res_rad))
	cmd = execute(var+'_RADTC_'+strtrim(i,2)+'[r,j] = mean(tmp[rok], /nan)')
      ENDFOR
    ENDIF
  ENDFOR


  ; DETECTION PROXIMITE COTE (< 25km)
  cmd = execute( 'mslp_sea_2dtc = mslp_sea_2dtc_'+strtrim(i,2)+'[*,*,j]' )
  cmd = execute( '     lon_2dtc =      lon_2dtc_'+strtrim(i,2)+'[*,*,j]' )
  cmd = execute( '     lat_2dtc =      lat_2dtc_'+strtrim(i,2)+'[*,*,j]' )
  indok   = where(finite(mslp_sea_2dtc) EQ 1, cntok)
  indbad  = where(finite(mslp_sea_2dtc) EQ 0, cntbad)
  dist_coast = dblarr(cntbad)
  FOR k = 0, cntbad-1 DO BEGIN
    dist_coast[k] = map_2points( lon_mslp[j], lat_mslp[j], lon_2dtc[indbad[k]], lat_2dtc[indbad[k]], /meters) / 1000.
  ENDFOR
  indcoast = where(dist_coast LT 25., cntcoast)
  IF indcoast[0] NE -1 AND cntcoast GT 10 THEN BEGIN
    print, '' & print, exp_list[i], ' dt=', j
    print, 'coast:', cntcoast, ' bad:', cntbad, ' ok:', cntok
    FOR k = 0, nb_var-1 DO BEGIN
      var = var_list[k]
      cmd = execute('test = n_elements('+var+'_'+strtrim(i,2)+')')
      IF test GT 0 THEN BEGIN
	tmp[ind_bad] = !Values.F_NAN
	cmd = execute( var+'_1DTC_' +strtrim(i,2)+'[j]     = !Values.F_NAN' )
	cmd = execute( var+'_2DTC_' +strtrim(i,2)+'[*,*,j] = !Values.F_NAN' )
	cmd = execute( var+'_RADTC_'+strtrim(i,2)+'[*,j]   = !Values.F_NAN' )
      ENDIF
    ENDFOR
  ENDIF


  ; DETECTION PROXIMITE BORD DU DOMAINE (< 25km)
  indboard = 0
  cmd = execute( 'south = min(lat_'+strtrim(i,2)+', /nan)' )
  cmd = execute( 'north = max(lat_'+strtrim(i,2)+', /nan)' )
  cmd = execute( ' east = min(lon_'+strtrim(i,2)+', /nan)' )
  cmd = execute( ' west = max(lon_'+strtrim(i,2)+', /nan)' )
  dist_south = map_2points( lon_mslp[j], lat_mslp[j], lon_mslp[j], south, /meters) / 1000.
  dist_north = map_2points( lon_mslp[j], lat_mslp[j], lon_mslp[j], north, /meters) / 1000.
  dist_east  = map_2points( lon_mslp[j], lat_mslp[j], east, lat_mslp[j], /meters) / 1000.
  dist_west  = map_2points( lon_mslp[j], lat_mslp[j], west, lat_mslp[j], /meters) / 1000.
  IF dist_south LE 25. OR dist_north LE 25. OR dist_east LE 25. OR dist_west LE 25. THEN BEGIN
    print, 'DOMAIN BOARDER < 25km DETECTED !'
    indboard = 1
    FOR k = 0, nb_var-1 DO BEGIN
      var = var_list[k]
      cmd = execute('test = n_elements('+var+'_'+strtrim(i,2)+')')
      IF test GT 0 THEN BEGIN
	tmp[ind_bad] = !Values.F_NAN
	cmd = execute( var+'_1DTC_' +strtrim(i,2)+'[j]     = !Values.F_NAN' )
	cmd = execute( var+'_2DTC_' +strtrim(i,2)+'[*,*,j] = !Values.F_NAN' )
	cmd = execute( var+'_RADTC_'+strtrim(i,2)+'[*,j]   = !Values.F_NAN' )
      ENDIF
    ENDFOR
  ENDIF
    

  ; SAUVEGARDE PARAMETRES TC
  IF indcoast[0] EQ -1 AND indboard NE 1 THEN BEGIN
    cmd = execute('RVM_1DTC_'+strtrim(i,2)+'[j] = where(w10m_sea_radtc_'+strtrim(i,2)+'[*,j] EQ max(w10m_sea_radtc_'+strtrim(i,2)+'[*,j] , /nan)) * res_rad')
    cmd = execute('MAX_W10M_RADTC_'+strtrim(i,2)+'[j] = MAX(W10M_SEA_RADTC_'+strtrim(i,2)+'[*,j], /NAN)')
    cmd = execute('lon_maxwnd_'+strtrim(i,2)+'[j] = lon_maxwnd[j]')
    cmd = execute('lat_maxwnd_'+strtrim(i,2)+'[j] = lat_maxwnd[j]')
    cmd = execute('max_w10m_'+strtrim(i,2)+'[j]   = max_w10m[j]')
    cmd = execute('lon_minwnd_'+strtrim(i,2)+'[j] = lon_minwnd[j]')
    cmd = execute('lat_minwnd_'+strtrim(i,2)+'[j] = lat_minwnd[j]')
    cmd = execute('min_mslp_'+strtrim(i,2)+'[j]   = min_mslp[j]')
    cmd = execute('lon_mslp_'+strtrim(i,2)+'[j] = lon_mslp[j]')
    cmd = execute('lat_mslp_'+strtrim(i,2)+'[j] = lat_mslp[j]')
  ENDIF

ENDFOR ; loop TIME
;------------------------------------------------------------------------------------------------------------------


;------------------------------------------------------------------------------------------------------------------
; RVM "BRUT"
cmd = execute('rvm_'+strtrim(i,2)+' = reform(map_npoints(lon_mslp,lat_mslp,lon_maxwnd,lat_maxwnd, /two_by_two))/1000. & help, rvm_'+strtrim(i,2))

; FIX SI RVM(MOYENNE AZIMUTHALE) = 0
cmd = execute('izero = where(RVM_1DTC_'+strtrim(i,2)+' EQ 0., nb_izero)')
IF izero[0] NE -1 THEN BEGIN
  cmd = execute( 'test = where(izero+1 EQ tdim_'+strtrim(i,2)+')' )
  IF test[0] EQ -1 THEN BEGIN
    FOR z = 0, nb_izero-1 DO cmd = execute( 'RVM_1DTC_'+strtrim(i,2)+'[izero[z]] = (RVM_1DTC_'+strtrim(i,2)+'[izero[z]-1] + RVM_1DTC_'+strtrim(i,2)+'[izero[z]+1]) / 2.' )
  ENDIF ELSE BEGIN
    FOR z = 0, nb_izero-1 DO cmd = execute( 'RVM_1DTC_'+strtrim(i,2)+'[izero[z]] = RVM_1DTC_'+strtrim(i,2)+'[izero[z]-1]' )
  ENDELSE
  cmd = execute( 'print, RVM_1DTC_'+strtrim(i,2)+'[izero]' )
ENDIF

; VERIF RVM
cmd = execute('izero = where(RVM_1DTC_'+strtrim(i,2)+' EQ 0., nb_izero)') & IF izero[0] NE -1 THEN STOP
cmd = execute('izero = where(RVM_'+strtrim(i,2)+' EQ 0., nb_izero)') & IF izero[0] NE -1 THEN STOP
;------------------------------------------------------------------------------------------------------------------


;------------------------------------------------------------------------------------------------------------------
; VITESSE DEPLACEMENT
cmd = execute( 'vdep_'+strtrim(i,2)+' = FLTARR(tdim)' )
cmd = execute( 'vdep_'+strtrim(i,2)+'[0] = map_2points(lon_mslp[0],lat_mslp[0],lon_mslp[1],lat_mslp[1], /meters)/(6.*60.*60.)' )
FOR j = 1, tdim-2 DO cmd = execute( 'vdep_'+strtrim(i,2)+'[j] = ( map_2points(lon_mslp[j-1],lat_mslp[j-1],lon_mslp[j],lat_mslp[j], /meters)/(6.*60.*60.) + map_2points(lon_mslp[j],lat_mslp[j],lon_mslp[j+1],lat_mslp[j+1], /meters)/(6.*60.*60.) ) / 2.' )
cmd = execute( 'vdep_'+strtrim(i,2)+'[tdim-1] = map_2points(lon_mslp[tdim-2],lat_mslp[tdim-2],lon_mslp[tdim-1],lat_mslp[tdim-1], /meters)/(6.*60.*60.)' )
;------------------------------------------------------------------------------------------------------------------


print, 'EXTRACT DATA MODEL OK' & print, ''
