    ; ANALYSE ALADIN + SST NEMO
    print, 'EXTRACT SST NEMO12...'
    initncdf, file_nemo12 & domdef, dom_tc
    glamt = glamt[firstxt:lastxt,firstyt:lastyt]
    gphit = gphit[firstxt:lastxt,firstyt:lastyt]
    tdim  = tdim_nemo12
    xdim  = xdim_nemo12
    ydim  = ydim_nemo12
    nb_pts = ceil(2.*radius / res_nemo12) & help, nb_pts
    IF nb_pts MOD 2 EQ 0 THEN nb_pts = nb_pts-1 & help, nb_pts
;    SSTN12_2DTC_1 = fltarr(nb_pts, nb_pts, tdim)
    SSTN12_1DTC_1 = fltarr(tdim)
    SSTN12_1D_1 = fltarr(tdim)


    FOR j = 0, tdim-1 DO BEGIN

      ; SELECTION ZONE TC
      jok = where(date_0 EQ date_nemo12[j], cntok)
      IF (jok EQ -1) OR (cntok GT 1) THEN STOP
      dist_tc = reform(map_npoints(lon_mslp_1[jok],lat_mslp_1[jok],glamt,gphit))/1000.
      dist_tc = reform(dist_tc,xdim,ydim)
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
      tmp = SST_NEMO12[*,*,j]
      tmp[ind_bad] = !Values.F_NAN
      SSTN12_1D_1[j] = avg(SST_NEMO12[*,*,j], /nan)
      SSTN12_1DTC_1[j] = avg(tmp[imin:imax,jmin:jmax], /nan)
;      SSTN12_2DTC_1[*,*,j] = tmp[imin:imax,jmin:jmax]

    ENDFOR


    print, 'EXTRACT SST NEMO13...'
    initncdf, file_nemo13 & domdef, dom_tc
    glamt = glamt[firstxt:lastxt,firstyt:lastyt]
    gphit = gphit[firstxt:lastxt,firstyt:lastyt]
    tdim  = tdim_nemo13
    xdim  = xdim_nemo13
    ydim  = ydim_nemo13
    nb_pts = ceil(2.*radius / res_nemo13) & help, nb_pts
    IF nb_pts MOD 2 EQ 0 THEN nb_pts = nb_pts-1 & help, nb_pts
;    SSTN13_2DTC_1 = fltarr(nb_pts, nb_pts, tdim)
    SSTN13_1DTC_1 = fltarr(tdim)
    SSTN13_1D_1 = fltarr(tdim)


    FOR j = 0, tdim-1 DO BEGIN

      ; SELECTION ZONE TC
      jok = where(date_0 EQ date_nemo13[j], cntok)
      IF (jok EQ -1) OR (cntok GT 1) THEN STOP
      dist_tc = reform(map_npoints(lon_mslp_1[jok],lat_mslp_1[jok],glamt,gphit))/1000.
      dist_tc = reform(dist_tc,xdim,ydim)
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
      tmp = SST_NEMO13[*,*,j]
      tmp[ind_bad] = !Values.F_NAN
      SSTN13_1D_1[j] = avg(SST_NEMO13[*,*,j], /nan)
      SSTN13_1DTC_1[j] = avg(tmp[imin:imax,jmin:jmax], /nan)
;      SSTN13_2DTC_1[*,*,j] = tmp[imin:imax,jmin:jmax]

    ENDFOR


    print, 'EXTRACT SST NEMO14...'
    initncdf, file_nemo14 & domdef, dom_tc
    glamt = glamt[firstxt:lastxt,firstyt:lastyt]
    gphit = gphit[firstxt:lastxt,firstyt:lastyt]
    tdim  = tdim_nemo14
    xdim  = xdim_nemo14
    ydim  = ydim_nemo14
    nb_pts = ceil(2.*radius / res_nemo14) & help, nb_pts
    IF nb_pts MOD 2 EQ 0 THEN nb_pts = nb_pts-1 & help, nb_pts
;    SSTN14_2DTC_1 = fltarr(nb_pts, nb_pts, tdim)
    SSTN14_1DTC_1 = fltarr(tdim)
    SSTN14_1D_1 = fltarr(tdim)


    FOR j = 0, tdim-1 DO BEGIN

      ; SELECTION ZONE TC
      jok = where(date_0 EQ date_nemo14[j], cntok)
      IF (jok EQ -1) OR (cntok GT 1) THEN STOP
      dist_tc = reform(map_npoints(lon_mslp_1[jok],lat_mslp_1[jok],glamt,gphit))/1000.
      dist_tc = reform(dist_tc,xdim,ydim)
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
      tmp = SST_NEMO14[*,*,j]
      tmp[ind_bad] = !Values.F_NAN
      SSTN14_1D_1[j] = avg(SST_NEMO14[*,*,j], /nan)
      SSTN14_1DTC_1[j] = avg(tmp[imin:imax,jmin:jmax], /nan)
;      SSTN14_2DTC_1[*,*,j] = tmp[imin:imax,jmin:jmax]

    ENDFOR
