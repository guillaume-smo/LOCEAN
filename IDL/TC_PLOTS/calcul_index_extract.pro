

; SELECTION ZONE TC ( < "RADIUS" )
; print, j, lon_mslp[j], lat_mslp[j]
dist_tc = reform(map_npoints(lon_mslp[j], lat_mslp[j], glamt, gphit)) / 1000.
dist_tc = reform(dist_tc, xdim, ydim)
ind_distmin = where(dist_tc EQ MIN(dist_tc, /NAN))
IF n_elements(ind_distmin) GT 1 THEN ind_distmin = MAX(ind_distmin, /NAN)
imid = (array_indices(glamt, ind_distmin))[0]
jmid = (array_indices(glamt, ind_distmin))[1]
ind_bad = where(dist_tc GT radius)
ind_rad = where(dist_tc LE radius)
irad = reform((array_indices(glamt,ind_rad))[0,*])
jrad = reform((array_indices(glamt,ind_rad))[1,*])
imin = min(irad, /nan)
imax = max(irad, /nan)
jmin = min(jrad, /nan)
jmax = max(jrad, /nan)


; INDICES DE DEPART
WHILE (imax-imin+1 GT nb_pts+1) DO BEGIN
  imin = imin+1
  imax = imax-1
ENDWHILE

WHILE (jmax-jmin+1 GT nb_pts+1) DO BEGIN
  jmin = jmin+1
  jmax = jmax-1
ENDWHILE

WHILE (imax-imin+1 GT nb_pts) DO BEGIN
;      print, 't:',j,' imin/imid/imax/idim:', imin, imid, imax, imax-imin+1, ' xdim:', n_elements(glamt[*,0])
  IF imid-imin GT (nb_pts-1)/2 THEN imin = imin+1 ELSE imax = imax-1
  imid = imin + (imax-imin+1)/2
;      print, 't:',j,' imin/imid/imax/idim:', imin, imid, imax, imax-imin+1, ' xdim:', n_elements(glamt[*,0])   
;      print, ((nb_pts-1)/2-(imid-imin)),((nb_pts-1)/2+(imax-imid)),((nb_pts-1)/2-(jmid-jmin)),((nb_pts-1)/2+(jmax-jmid))
ENDWHILE
WHILE (jmax-jmin+1 GT nb_pts) DO BEGIN
;      print, 't:',j,' jmin/jmid/jmax/jdim:', jmin, jmid, jmax, jmax-jmin+1, ' ydim:', n_elements(glamt[0,*])           
  IF jmid-jmin GT (nb_pts-1)/2 THEN jmin = jmin+1 ELSE jmax = jmax-1
  jmid = jmin + (jmax-jmin+1)/2
;      print, 't:',j,' jmin/jmid/jmax/jdim:', jmin, jmid, jmax, jmax-jmin+1, ' ydim:', n_elements(glamt[0,*])
;      print, ((nb_pts-1)/2-(imid-imin)),((nb_pts-1)/2+(imax-imid)),((nb_pts-1)/2-(jmid-jmin)),((nb_pts-1)/2+(jmax-jmid))
ENDWHILE
;  print, imin, imax, jmin, jmax


; INDICES D'ARRIVEE (2DTC)
ibeg = ((nb_pts-1)/2-(imid-imin))
iend = ((nb_pts-1)/2+(imax-imid))
jbeg = ((nb_pts-1)/2-(jmid-jmin))
jend = ((nb_pts-1)/2+(jmax-jmid))
;    print, 'iend-ibeg+1 =', iend-ibeg+1, ' jend-jbeg+1 =', jend-jbeg+1

WHILE iend GT nb_pts-1 DO BEGIN
;      print, 't:',j,' imin/imid/imax/idim/nb_pts:', imin, imid, imax, imax-imin+1, nb_pts
  IF ibeg GT 0 THEN BEGIN & ibeg = ibeg -1 & iend = iend -1 & ENDIF ELSE STOP
;      help, ibeg, iend, iend-ibeg-1      
ENDWHILE

WHILE jend GT nb_pts-1 DO BEGIN
;      print, 't:',j,' jmin/jmid/jmax/jdim/nb_pts:', jmin, jmid, jmax, jmax-jmin+1, nb_pts
  IF jbeg GT 0 THEN BEGIN & jbeg = jbeg -1 & jend = jend -1 & ENDIF ELSE STOP
;      help, jbeg, jend, jend-jbeg-1
ENDWHILE
