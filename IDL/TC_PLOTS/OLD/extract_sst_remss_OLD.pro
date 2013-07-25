    ; IBTRACS + SST OBS (ODYSSEA ou REMSS)
    initncdf, file_sstr & domdef, dom_tc
    glamt = glamt[firstxt:lastxt,firstyt:lastyt]
    gphit = gphit[firstxt:lastxt,firstyt:lastyt]
    tdim  = tdim_sstr
    cmd = execute('nb_pts = ceil(2.*radius / res_sstr) & help, nb_pts')
;    IF nb_pts MOD 2 EQ 0 THEN nb_pts = nb_pts-1 & help, nb_pts
    cmd = execute('SSTR_2DTC_'+strtrim(i,2)+' = fltarr(nb_pts, nb_pts, tdim)')
    cmd = execute('SSTR_1DTC_'+strtrim(i,2)+' = fltarr(tdim)')
    cmd = execute('SSTR_1D_'+strtrim(i,2)+' = fltarr(tdim)')

    FOR j = 0, tdim-1 DO BEGIN

      ; SELECTION ZONE TC
      dist_tc = reform(map_npoints(lon_mslp_0[j],lat_mslp_0[j],glamt,gphit))/1000.
      dist_tc = reform(dist_tc,xdim_sstr,ydim_sstr)
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
      tmp = SSTR_0[*,*,j]
      tmp[ind_bad] = !Values.F_NAN
      cmd = execute('SSTR_1D_'+strtrim(i,2)+'[j] = avg(SSTR_0[*,*,j], /nan)')
      cmd = execute('SSTR_1DTC_'+strtrim(i,2)+'[j] = avg(tmp[imin:imax,jmin:jmax], /nan)')
      cmd = execute('SSTR_2DTC_'+strtrim(i,2)+'[*,*,j] = tmp[imin:imax,jmin:jmax]')

    ENDFOR ; loop time      
