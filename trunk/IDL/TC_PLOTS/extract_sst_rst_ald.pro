    ; IBTRACS + SST RESTART*
    print, 'EXTRACT SST RESTART...'
    initncdf, file_sstr1 & domdef, dom_tc
    glamt = glamt[firstxt:lastxt,firstyt:lastyt]
    gphit = gphit[firstxt:lastxt,firstyt:lastyt]
    tdim  = tdim_sstr1
    nb_pts = ceil(2.*radius / res_sstr1) & help, nb_pts
    IF nb_pts MOD 2 EQ 0 THEN nb_pts = nb_pts-1 & help, nb_pts
    cmd = execute('SSTR1_2DTC_'+strtrim(i,2)+' = fltarr(nb_pts, nb_pts, tdim)')
    cmd = execute('SSTR1_1DTC_'+strtrim(i,2)+' = fltarr(tdim)')
    cmd = execute('SSTR1_1D_'+strtrim(i,2)+'   = fltarr(tdim)')
    cmd = execute('SSTR2_2DTC_'+strtrim(i,2)+' = fltarr(nb_pts, nb_pts, tdim)')
    cmd = execute('SSTR2_1DTC_'+strtrim(i,2)+' = fltarr(tdim)')
    cmd = execute('SSTR2_1D_'+strtrim(i,2)+'   = fltarr(tdim)')
    cmd = execute('SSTR3_2DTC_'+strtrim(i,2)+' = fltarr(nb_pts, nb_pts, tdim)')
    cmd = execute('SSTR3_1DTC_'+strtrim(i,2)+' = fltarr(tdim)')
    cmd = execute('SSTR3_1D_'+strtrim(i,2)+'   = fltarr(tdim)')


    j = where( date_0 EQ date_sstr1)

      ; SELECTION ZONE TC
      dist_tc = reform(map_npoints(lon_mslp_0[j],lat_mslp_0[j],glamt,gphit))/1000.
      dist_tc = reform(dist_tc,xdim_sstr1,ydim_sstr1)
      ind_bad = where(dist_tc GT radius)
      ind_rad = where(dist_tc LE radius)
      irad = reform((array_indices(glamt,ind_rad))[0,*])
      jrad = reform((array_indices(gphit,ind_rad))[1,*])
      imin = min(irad, /nan)
      imax = max(irad, /nan); & print, 'idim:', imax-imin+1
      jmin = min(jrad, /nan)
      jmax = max(jrad, /nan); & print, 'jdim:', jmax-jmin+1

      WHILE (imax-imin+1 GT nb_pts) DO BEGIN
	imin = imin+1
	imax = imax-1
      ENDWHILE
;      print, 'idim:', imax-imin+1

      WHILE (jmax-jmin+1 GT nb_pts) DO BEGIN
	jmin = jmin+1
	jmax = jmax-1
      ENDWHILE
;      print, 'jdim:', jmax-jmin+1

      WHILE (jmax-jmin+1 LT nb_pts) DO BEGIN
	jmin = jmin-1
	jmax = jmax+1
      ENDWHILE
;      print, 'jdim:', jmax-jmin+1

      ; SAUVEGARDE VARIABLES EXTRAITES
      tmp = SSTR1_0[*,*,j]
      tmp[ind_bad] = !Values.F_NAN
      cmd = execute('SSTR1_1D_'+strtrim(i,2)+'[j] = avg(SSTR1_0[*,*,j], /nan)')
      cmd = execute('SSTR1_1DTC_'+strtrim(i,2)+'[j] = avg(tmp[imin:imax,jmin:jmax], /nan)')
      cmd = execute('SSTR1_2DTC_'+strtrim(i,2)+'[*,*,j] = tmp[imin:imax,jmin:jmax]')


    j = where( date_0 EQ date_sstr2)

      ; SELECTION ZONE TC
      dist_tc = reform(map_npoints(lon_mslp_0[j],lat_mslp_0[j],glamt,gphit))/1000.
      dist_tc = reform(dist_tc,xdim_sstr1,ydim_sstr1)
      ind_bad = where(dist_tc GT radius)
      ind_rad = where(dist_tc LE radius)
      irad = reform((array_indices(glamt,ind_rad))[0,*])
      jrad = reform((array_indices(gphit,ind_rad))[1,*])
      imin = min(irad, /nan)
      imax = max(irad, /nan); & print, 'idim:', imax-imin+1
      jmin = min(jrad, /nan)
      jmax = max(jrad, /nan); & print, 'jdim:', jmax-jmin+1

      WHILE (imax-imin+1 GT nb_pts) DO BEGIN
	imin = imin+1
	imax = imax-1
      ENDWHILE
;      print, 'idim:', imax-imin+1

      WHILE (jmax-jmin+1 GT nb_pts) DO BEGIN
	jmin = jmin+1
	jmax = jmax-1
      ENDWHILE
;      print, 'jdim:', jmax-jmin+1

      WHILE (jmax-jmin+1 LT nb_pts) DO BEGIN
	jmin = jmin-1
	jmax = jmax+1
      ENDWHILE
;      print, 'jdim:', jmax-jmin+1

      ; SAUVEGARDE VARIABLES EXTRAITES
      tmp = SSTR2_0[*,*,j]
      tmp[ind_bad] = !Values.F_NAN
      cmd = execute('SSTR2_1D_'+strtrim(i,2)+'[j] = avg(SSTR2_0[*,*,j], /nan)')
      cmd = execute('SSTR2_1DTC_'+strtrim(i,2)+'[j] = avg(tmp[imin:imax,jmin:jmax], /nan)')
      cmd = execute('SSTR2_2DTC_'+strtrim(i,2)+'[*,*,j] = tmp[imin:imax,jmin:jmax]')


    j = where( date_0 EQ date_sstr3)

      ; SELECTION ZONE TC
      dist_tc = reform(map_npoints(lon_mslp_0[j],lat_mslp_0[j],glamt,gphit))/1000.
      dist_tc = reform(dist_tc,xdim_sstr1,ydim_sstr1)
      ind_bad = where(dist_tc GT radius)
      ind_rad = where(dist_tc LE radius)
      irad = reform((array_indices(glamt,ind_rad))[0,*])
      jrad = reform((array_indices(gphit,ind_rad))[1,*])
      imin = min(irad, /nan)
      imax = max(irad, /nan)
      jmin = min(jrad, /nan)
      jmax = max(jrad, /nan)

;      print, 'idim:', imax-imin+1
;      print, 'jdim:', jmax-jmin+1

      WHILE (imax-imin+1 GT nb_pts+1) DO BEGIN
	imin = imin+1
	imax = imax-1
      ENDWHILE

      WHILE (imax-imin+1 GT nb_pts) DO BEGIN
	imin = imin+1
	imax = imax
      ENDWHILE

      WHILE (jmax-jmin+1 GT nb_pts+1) DO BEGIN
	jmin = jmin+1
	jmax = jmax-1
      ENDWHILE

      WHILE (jmax-jmin+1 GT nb_pts) DO BEGIN
	jmin = jmin+1
	jmax = jmax
      ENDWHILE

      WHILE (jmax-jmin+1 LT nb_pts-1) DO BEGIN
	jmin = jmin-1
	jmax = jmax+1
      ENDWHILE

      WHILE (jmax-jmin+1 LT nb_pts) DO BEGIN
	jmin = jmin
	jmax = jmax+1
      ENDWHILE

;      print, 'idim:', imax-imin+1
;      print, 'jdim:', jmax-jmin+1

      IF imax-imin+1 NE nb_pts THEN STOP
      IF jmax-jmin+1 NE nb_pts THEN STOP


      ; SAUVEGARDE VARIABLES EXTRAITES

      tmp = SSTR3_0[*,*,j]
      tmp[ind_bad] = !Values.F_NAN
      cmd = execute('SSTR3_1D_'+strtrim(i,2)+'[j] = avg(SSTR3_0[*,*,j], /nan)')
      cmd = execute('SSTR3_1DTC_'+strtrim(i,2)+'[j] = avg(tmp[imin:imax,jmin:jmax], /nan)')
      cmd = execute('SSTR3_2DTC_'+strtrim(i,2)+'[*,*,j] = tmp[imin:imax,jmin:jmax]')
      print, 'EXTRACT SST RESTART OK'
