print, '' & print, 'EXTRACTION DATA MODEL...'


; variables locales
cmd = execute('wnd10m = W10M_SEA_'+strtrim(i,2))
cmd = execute('mslp   = MSLP_SEA_'+strtrim(i,2))  
cmd = execute('tdim   = tdim_'+strtrim(i,2))


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
  
  ; SELECTION ZONE TC ( < "RADIUS" )
  dist_tc = reform(map_npoints(lon_mslp[j],lat_mslp[j],glamt,gphit))/1000.
  cmd = execute('dist_tc = reform(dist_tc,xdim_'+strtrim(i,2)+',ydim_'+strtrim(i,2)+')')
  ind_distmin = where(dist_tc EQ MIN(dist_tc, /NAN))
  IF n_elements(ind_distmin) GT 1 THEN ind_distmin = MAX(ind_distmin, /NAN)
  imid = (array_indices(glamt, ind_distmin))[0]
  jmid = (array_indices(glamt, ind_distmin))[1]
  ind_bad = where(dist_tc GT radius)
  ind_rad = where(dist_tc LE radius)

  IF ind_rad[0] NE -1 THEN BEGIN
    irad = reform((array_indices(glamt,ind_rad))[0,*])
    jrad = reform((array_indices(glamt,ind_rad))[1,*])
    imin = min(irad, /nan)
    imax = max(irad, /nan)
    jmin = min(jrad, /nan)
    jmax = max(jrad, /nan)

    ; ajustement de la zone sélectionnée à la taille du tableau déclaré
    WHILE (imax-imin+1 GT nb_pts+1) DO BEGIN
      imin = imin+1
      imax = imax-1
    ENDWHILE

    WHILE (imax-imin+1 GT nb_pts) DO BEGIN
      print, 't:',j,' imin/imid/imax/idim:', imin, imid, imax, imax-imin+1, ' xdim:', n_elements(glamt[*,0])
      IF imid-imin GT (nb_pts-1)/2 THEN imin = imin+1 ELSE imax = imax -1
      print, 't:',j,' imin/imid/imax/idim:', imin, imid, imax, imax-imin+1, ' xdim:', n_elements(glamt[*,0])   
      print, ((nb_pts-1)/2-(imid-imin)),((nb_pts-1)/2+(imax-imid)),((nb_pts-1)/2-(jmid-jmin)),((nb_pts-1)/2+(jmax-jmid))
    ENDWHILE

    WHILE (jmax-jmin+1 GT nb_pts+1) DO BEGIN
      jmin = jmin+1
      jmax = jmax-1
    ENDWHILE

    WHILE (jmax-jmin+1 GT nb_pts) DO BEGIN
      print, 't:',j,' jmin/jmid/jmax/jdim:', jmin, jmid, jmax, jmax-jmin+1, ' ydim:', n_elements(glamt[0,*])           
      IF jmid-jmin GT (nb_pts-1)/2 THEN jmin = jmin+1 ELSE jmax = jmax -1	
      print, 't:',j,' jmin/jmid/jmax/jdim:', jmin, jmid, jmax, jmax-jmin+1, ' ydim:', n_elements(glamt[0,*])
      print, ((nb_pts-1)/2-(imid-imin)),((nb_pts-1)/2+(imax-imid)),((nb_pts-1)/2-(jmid-jmin)),((nb_pts-1)/2+(jmax-jmid))
    ENDWHILE

    IF ((nb_pts-1)/2+(imax-imid)) GT nb_pts-1 THEN print, 't:',j,' imin/imid/imax/idim/nb_pts:', imin, imid, imax, imax-imin+1, nb_pts
;	IF ((nb_pts-1)/2+(imax-imid)) GT nb_pts-1 THEN STOP
    IF ((nb_pts-1)/2+(jmax-jmid)) GT nb_pts-1 THEN print, 't:',j,' jmin/jmid/jmax/jdim/nb_pts:', jmin, jmid, jmax, jmax-jmin+1, nb_pts
;	IF ((nb_pts-1)/2+(jmax-jmid)) GT nb_pts-1 THEN STOP


    ; SAUVEGARDE DES VARIABLES EXTRAITES
    cmd = execute('lon_2DTC_'+strtrim(i,2)+'[*,*,j] = glamt[imin:imax,jmin:jmax]')
    cmd = execute('lat_2DTC_'+strtrim(i,2)+'[*,*,j] = gphit[imin:imax,jmin:jmax]')
    FOR k = 0, nb_var-1 DO BEGIN
      var = var_list[k]
      cmd = execute('test = n_elements('+var+'_'+strtrim(i,2)+')')
      IF test GT 0 THEN BEGIN
	cmd = execute('tmp = '+var+'_'+strtrim(i,2)+'[*,*,j]')
	tmp[ind_bad] = !Values.F_NAN
	cmd = execute(var+'_1D_'+strtrim(i,2)+'[j] = avg('+var+'_'+strtrim(i,2)+'[*,*,j], /nan)')
	cmd = execute(var+'_1DTC_'+strtrim(i,2)+'[j] = avg(tmp[imin:imax,jmin:jmax], /nan)')
	cmd = execute(var+'_2DTC_'+strtrim(i,2)+'[((nb_pts-1)/2-(imid-imin)):((nb_pts-1)/2+(imax-imid)),((nb_pts-1)/2-(jmid-jmin)):((nb_pts-1)/2+(jmax-jmid)),j] = tmp[imin:imax,jmin:jmax]')	
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


    ; SAUVEGARDE PARAMETRES TC
    IF indcoast[0] EQ -1 THEN BEGIN
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

  ENDIF

ENDFOR ; loop TIME


; RVM "BRUT"
cmd = execute('rvm_'+strtrim(i,2)+' = reform(map_npoints(lon_mslp,lat_mslp,lon_maxwnd,lat_maxwnd, /two_by_two))/1000. & help, rvm_'+strtrim(i,2))

; FIX SI RVM (MOYENNE AZIMUTHALE) = 0
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


print, 'EXTRACT DATA MODEL OK' & print, ''
