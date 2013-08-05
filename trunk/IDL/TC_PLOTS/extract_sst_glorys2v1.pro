    ; IBTRACS + SST OBS (ODYSSEA ou REMSS)
    print, 'EXTRACT SST GLORYS2V1...'
    initncdf, file_sstg1 & domdef, dom_tc
    glamt = glamt[firstxt:lastxt,firstyt:lastyt]
    gphit = gphit[firstxt:lastxt,firstyt:lastyt]
    tdim  = tdim_sstg1
    nb_pts = ceil(2.*radius / res_sstg1) & help, nb_pts
    IF nb_pts MOD 2 EQ 0 THEN nb_pts = nb_pts-1 & help, nb_pts
;    cmd = execute('SSTG1_2DTC_'+strtrim(i,2)+' = fltarr(nb_pts, nb_pts, tdim)')
    cmd = execute('SSTG1_1DTC_'+strtrim(i,2)+' = fltarr(tdim)')
    cmd = execute('SSTG1_1D_'+strtrim(i,2)+' = fltarr(tdim)')

    FOR j = 0, tdim-1 DO BEGIN

      ; SELECTION ZONE TC
      dist_tc = reform(map_npoints(lon_mslp_0[j],lat_mslp_0[j],glamt,gphit))/1000.
      dist_tc = reform(dist_tc,xdim_sstg1,ydim_sstg1)
      ind_distmin = where(dist_tc EQ MIN(dist_tc, /NAN))
      IF n_elements(ind_distmin) GT 1 THEN ind_distmin = MAX(ind_distmin, /NAN)
      imid = (array_indices(SSTG1_0[*,*,j], ind_distmin))[0]
      jmid = (array_indices(SSTG1_0[*,*,j], ind_distmin))[1]
      ind_bad = where(dist_tc GT radius)
      ind_rad = where(dist_tc LE radius)

      IF ind_rad[0] NE -1 THEN BEGIN
	irad = reform((array_indices(glamt,ind_rad))[0,*])
	jrad = reform((array_indices(gphit,ind_rad))[1,*])
	imin = min(irad, /nan)
	imax = max(irad, /nan)
	jmin = min(jrad, /nan)
	jmax = max(jrad, /nan)


	WHILE (imax-imin+1 GT nb_pts+1) DO BEGIN
	  imin = imin+1
	  imax = imax-1
	ENDWHILE

	WHILE (imax-imin+1 GT nb_pts) DO BEGIN
	  print, 't:',j,' imin/imid/imax/idim:', imin, imid, imax, imax-imin+1, ' xdim:', xdim_sstg1      
	  IF imid-imin GT (nb_pts-1)/2 THEN imin = imin+1 ELSE imax = imax -1
	  print, 't:',j,' imin/imid/imax/idim:', imin, imid, imax, imax-imin+1, ' xdim:', xdim_sstg1      
	  print, ((nb_pts-1)/2-(imid-imin)),((nb_pts-1)/2+(imax-imid)),((nb_pts-1)/2-(jmid-jmin)),((nb_pts-1)/2+(jmax-jmid))
	ENDWHILE

	WHILE (jmax-jmin+1 GT nb_pts+1) DO BEGIN
	  jmin = jmin+1
	  jmax = jmax-1
	ENDWHILE

	WHILE (jmax-jmin+1 GT nb_pts) DO BEGIN
	  print, 't:',j,' jmin/jmid/jmax/jdim:', jmin, jmid, jmax, jmax-jmin+1, ' ydim:', ydim_sstg1
	  IF jmid-jmin GT (nb_pts-1)/2 THEN jmin = jmin+1 ELSE jmax = jmax -1	
	  print, 't:',j,' jmin/jmid/jmax/jdim:', jmin, jmid, jmax, jmax-jmin+1, ' ydim:', ydim_sstg1
	  print, ((nb_pts-1)/2-(imid-imin)),((nb_pts-1)/2+(imax-imid)),((nb_pts-1)/2-(jmid-jmin)),((nb_pts-1)/2+(jmax-jmid))
	ENDWHILE

;	IF ((nb_pts-1)/2+(imax-imid)) GT nb_pts-1 THEN STOP
;	IF ((nb_pts-1)/2+(jmax-jmid)) GT nb_pts-1 THEN STOP


	; SAUVEGARDE VARIABLES EXTRAITES
	tmp = SSTG1_0[*,*,j]
	tmp[ind_bad] = !Values.F_NAN
	cmd = execute('SSTG1_1D_'+strtrim(i,2)+'[j] = avg(SSTG1_0[*,*,j], /nan)')
	cmd = execute('SSTG1_1DTC_'+strtrim(i,2)+'[j] = avg(tmp[imin:imax,jmin:jmax], /nan)')
  ;	cmd = execute('SSTG1_2DTC_'+strtrim(i,2)+'[((nb_pts-1)/2-(imid-imin)):((nb_pts-1)/2+(imax-imid)),((nb_pts-1)/2-(jmid-jmin)):((nb_pts-1)/2+(jmax-jmid)),j] = tmp[imin:imax,jmin:jmax]')
      ENDIF


    ENDFOR ; loop time
    print, 'EXTRACT SST GLORYS2V1 OK'
