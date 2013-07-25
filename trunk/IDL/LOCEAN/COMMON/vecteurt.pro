;+
;
; @file_comments
;
; @categories
;
; @param angle 
;
; @returns
;
; @restrictions
;
; @examples
;
; @history
;
; @version
; $Id: vecteur.pro 482 2013-02-07 14:30:32Z smasson $
;
;-
FUNCTION cv_cm2normal, angle
;
; Give the length in normal coordinates of a trait oriented of an angle
; by rapport at the x axis and which must do 1 cm on the drawing.
; Angle can be an array.
;
;
;
  compile_opt idl2, strictarrsubs
;
@common
; What is the length in normal coordinates of a trait which will do 1 cm
; on the paper an which is parallel to x?
  mipgsz = min(page_size, max = mapgsz)
   sizexfeuille = mipgsz*key_portrait+mapgsz*(1-key_portrait)
   sizeyfeuille = mapgsz*key_portrait+mipgsz*(1-key_portrait)
   cm_en_normal = 1./sizexfeuille
;
; If the aspect ratio of the window is not equal to 1, the length in
; normalized coordinates of  a trait of 1 cm vary following the polar
; angle of this trait.
;
   aspect = sizexfeuille/sizeyfeuille
   cm_en_normal = cm_en_normal*sqrt( 1 +(aspect^2-1)*sin(angle)^2 )
;
   return, cm_en_normal
END
;
;+
;
; @file_comments
;
; @categories
;
; @param u
;
; @param v
;
; @param w
;
; @restrictions
;
; @examples
;
; @history
;
; @version
; $Id: vecteur.pro 482 2013-02-07 14:30:32Z smasson $
;
;-
PRO normalise, u, v, w
;
; normalize the vector
;
;
  compile_opt idl2, strictarrsubs
;
   IF n_elements(w) NE 0 THEN BEGIN
      norme = sqrt(u^2.+v^2.+w^2.)
      ind = where(norme NE 0)
      u[ind] = u[ind]/norme[ind]
      v[ind] = v[ind]/norme[ind]
      w[ind] = w[ind]/norme[ind]
   ENDIF ELSE BEGIN
      norme = sqrt(u^2.+v^2.)
      ind = where(norme NE 0)
      u[ind] = u[ind]/norme[ind]
      v[ind] = v[ind]/norme[ind]
   ENDELSE
END
;
;+
;
; @file_comments
; Trace vectors (even if they are on a deformed grid) on any projection.
; In this way, all vectors have a comparable norme on the drawing (to be
; clear, a vector which measure 1 cm measure it, no matter the projection
; and is position on the sphere).
;
; @categories
; Graphics
;
; @param composanteu {in}{required}
; It is the u component of the vector to be traced. This 2d array has the
; same dimension that reduitindice2d (see further)
;
; @param composantev {in}{required}
; It is the v component of the vector to be traced. This 2d array has the
; same dimension that reduitindice2d (see further)
;
; @param normevecteur
;
; @param indice2d {in}{required}
; It in an index allowing to to pass from an jpi or jpj array to the zoom
; on which we do the drawing
;
; @param reduitindice2d {in}{required}
; It is an index allowing to pass from an array defined by indice2d to the
; array for which we really have vectors to be traced (to be clear, it is
; for example when we trace only one vector on two).
;
; @keyword CMREF {default=between .5 and 1.5 cm}
; The length in cm that must measure the arrow normed normeref. By default,
; it is adjusted to other drawing and included between .5 and 1.5 cm.
;
; @keyword MISSING
; The value of a missing value. Do not use this keyword. Fixed at 1e5 by
; ajoutvect.pro
;
; @keyword NORMEREF
; The norme of the reference arrow.
;
; @keyword VECTCOLOR {default=0}
; The color of the arrow. Black by default (color 0)
;
; @keyword VECTNORMCOLOR
; a 2 elements vector used to specify the color of the vector according to
; its norm. Vector norm will be scaled to color numbers (1 to 254) by using
; the following rule: VECTNORMCOLOR[0] corresponds to 1 and VECTNORMCOLOR[1] to 254
;
; @keyword VECTTHICK {default=1}
; The thick of the arrow.
;
; @keyword VECTREFPOS
; Vector composed of 2 elements specifying the position on DATA coordinates
; from the beginning of the reference vector. By default at the right bottom
; of the drawing.
;
; @keyword VECTREFFORMAT
; The format to be used to specify the norme of the reference vector.
;
; @keyword VECTREFCHARSIZE
; Character size of the legend of the reference arrow
;
; @keyword VECTREFCHARSIZE
; Character thickness of the legend of the reference arrow
;
; @keyword NOVECTREF
; To delete the display of the reference vector.
;
; @keyword _EXTRA
; Used to pass keywords
;
; @uses
; <pro>common</pro>
;
; @history
;  Creation : 13/02/98 G. Roullet (grlod\@lodyc.jussieu.fr)
;  Modification : 14/01/99 realise la transformation
;  spherique<->cartesien G. Roullet
;                 12/03/99 verification de la routine G. Roullet
;  8/11/1999:
;  G. Roullet et Sebastien Masson (smasson\@lodyc.jussieu.fr)
;  adaptation pour les zoom. reverification...traitement separe de la
;  direction et de la norme des vecteurs. mots cles NORMEREF et CMREF.
;
; @version
; $Id: vecteur.pro 482 2013-02-07 14:30:32Z smasson $
;
;-
PRO vecteur, composanteu, composantev, normevecteur, indice2d, reduitindice2d $
             , CMREF = cmref, MISSING = missing, NORMEREF = normeref $
             , VECTCOLOR = vectcolor, VECTTHICK = vectthick $
             , VECTREFPOS = vectrefpos, VECTNORMCOLOR = vectnormcolor $
             , VECTREFFORMAT = vectrefformat, NOVECTREF = novectref $
             , VECTREFCHARSIZE = vectrefcharsize, VECTREFCHARTHICK = vectrefcharthick $
             , _EXTRA = extra
;
  compile_opt idl2, strictarrsubs
;
@common
  tempsun = systime(1)          ; For key_performance
;
;
  taille = size(composanteu)
  nx = taille[1]
  ny = taille[2]
  if n_elements(reduitindice2d) EQ 0 then reduitindice2d = lindgen(nx, ny)
  zu = composanteu
  zv = composantev
  norme = normevecteur
  taille = size(indice2d)
  nxgd = taille[1]
  nygd = taille[2]
;
  msk = replicate(1, nx, ny)
  if keyword_set(missing) then terre = where(abs(zu) GE missing/10) ELSE terre = -1
  if terre[0] NE -1  then BEGIN
    msk[terre] = 0
    zu[terre] = 0
    zv[terre] = 0
    norme[terre] = 0
  ENDIF
;
; Stage 1:
;
; Given that the directions and the sense that the vector has on the sphere,
; we have to try to determinate this direction and the sense that the vector
; will have on the screen once it will have been projected.
;
; In theory: on the sphere, a vector in a given point has for direction the
; tangent at the circle passing by the center of the Earth and by the vector.
; So, find the direction once the projection is done, it is find the tangent
; to the curve representing the projection of the circle on the 2d plan at the
; point representing the projection of the starting point of the sphere on the
; 2d plan.
;
; In practice we do no know the definition of the curve given by the projection
; of a circle so find its tangente in a point...
;
; What we do:
; In a 3d cartesian reference,
;       a) We find coordinates of the point T (starting of the arrow) situed
;       on the sphere.
;       b) To each point T, we determine local directions defined by the grid
;       on this point and on which coordinates (u,v) of the vector refer to.
;       These local directions are defined by gradients of glam and gphi. Once
;       we have obtain these directions, we consider them like orthogonal and
;       by norming them, we build an orthonormal reference (nu,nv) on which
;       coordinates (u,v) of the vector refer to. In the starting 3d cartesian
;       reference, the vector is defined by:
;       V=u*nu+v*nv
;       (where V, nu and nv are 3d vectors and u and v are scalars).
;       c) To approximate the tangente to the circle by the chord defined by
;       the beginning and the ending of the arrow, we will normalize V, and
;       then divide it by 100.
;       d) This allows us to determine coordinates of extremities of the chord
;       in the 3d cartesian reference. We pass them in spherical coordinates in
;       order to recuperate latitude and longitude position of these points on
;       the sphere.
;       e) We pass coordinates of these points in normalized coordinates, then
;       in polar coordinates in order to find the angle and the direction they
;       determine on the drawing.
;
;
; Stage 1, a)
;
;
; coordinates of the point T (beginning of the arrow) in spherical coordinates.
  glam = glamt[indice2d[reduitindice2d]]
  gphi = gphit[indice2d[reduitindice2d]]
;
; Coordinates of the point T (beginning of the arrow) in the cartesian reference.
; For the sphere, we use a sphere with a radius of 1.
;
  radius = replicate(1, nx*ny)
  coord_sphe = transpose([ [glam[*]], [gphi[*]], [radius[*]] ])
  r = cv_coord(from_sphere = coord_sphe, /to_rect, /degrees)
;
  x0 = reform(r[0, *], nx, ny)
  y0 = reform(r[1, *], nx, ny)
  z0 = reform(r[2, *], nx, ny)
;
; Stage 1, b)
;
; Construction of a vector nu (resp. nv), vectr normed carried by the axis of
; points u[i,j] and u[i-1,j] (resp v[i,j] and v[i,j-1]) which define, for each
; point on the sphere, local directions associated with u and v. These vectors
; define a local orthonormal reference.
; These vectors are built in a cartesian reference (cv_coord). We have choose a
; unity radius of the Earth (unit).
;
; definition of nu
  radius = replicate(1, nxgd*nygd)
  IF finite(glamu[0]*gphiu[0]) NE 0 THEN $
     coord_sphe = transpose([ [(glamu[indice2d])[*]], [(gphiu[indice2d])[*]], [radius[*]] ]) $
  ELSE coord_sphe = transpose([ [(glamf[indice2d])[*]], [(gphit[indice2d])[*]], [radius[*]] ])
  r = cv_coord(from_sphere = coord_sphe, /to_rect, /degrees)
; coordinates of points of the grid u in cartesian.
  ux = reform(r[0, *], nxgd, nygd)
  uy = reform(r[1, *], nxgd, nygd)
  uz = reform(r[2, *], nxgd, nygd)
; calculation of nu
  nux = ux-shift(ux, 1, 0)
  nuy = uy-shift(uy, 1, 0)
  nuz = uz-shift(uz, 1, 0)
; conditions at extremities.
  if NOT keyword_set(key_periodic) OR nxgd NE jpi then begin
    nux[0, *] = nux[1, *]
    nuy[0, *] = nuy[1, *]
    nuz[0, *] = nuz[1, *]
  ENDIF
; reduction of the grid
  nux = nux[reduitindice2d]
  nuy = nuy[reduitindice2d]
  nuz = nuz[reduitindice2d]
; definition of nv
  IF finite(glamv[0]*gphiv[0]) NE 0 THEN $
     coord_sphe = transpose([ [(glamv[indice2d])[*]], [(gphiv[indice2d])[*]], [radius[*]] ]) $
  ELSE coord_sphe = transpose([ [(glamt[indice2d])[*]], [(gphif[indice2d])[*]], [radius[*]] ])
  r = cv_coord(from_sphere = coord_sphe, /to_rect, /degrees)
; coordinates of points of the grid in cartesian.
  vx = reform(r[0, *], nxgd, nygd)
  vy = reform(r[1, *], nxgd, nygd)
  vz = reform(r[2, *], nxgd, nygd)
; calcul of nv
  nvx = vx-shift(vx, 0, 1)
  nvy = vy-shift(vy, 0, 1)
  nvz = vz-shift(vz, 0, 1)
; conditions at extremities
  nvx[*, 0] = nvx[*, 1]
  nvy[*, 0] = nvy[*, 1]
  nvz[*, 0] = nvz[*, 1]
; reduction of the grid
  nvx = nvx[reduitindice2d]
  nvy = nvy[reduitindice2d]
  nvz = nvz[reduitindice2d]
;
; normalization
;
  normalise, nux, nuy, nuz
  normalise, nvx, nvy, nvz
;
; Stage 1, c)
;
; coordinates of the vector V in the cartesian reference
;
  direcx = zu*nux + zv*nvx
  direcy = zu*nuy + zv*nvy
  direcz = zu*nuz + zv*nvz
; normalization of the vector V
  normalise, direcx, direcy, direcz
; on divide by 100
  direcx = direcx/100.
  direcy = direcy/100.
  direcz = direcz/100.
;
; Stage 1, d)
; coordinates of the point of the arrow in the cartesian reference.

  x1 = x0 + direcx
  y1 = y0 + direcy
  z1 = z0 + direcz

; coordinates of the point of the arrow in spherical coordinates.

  coord_rect = transpose([ [x1[*]], [y1[*]], [z1[*]] ])
  r = cv_coord(from_rect = coord_rect, /to_sphere, /degrees)
  glam1 = reform(r[0, *], nx, ny)
  gphi1 = reform(r[1, *], nx, ny)

;
; modification of glams. Everything take place at the level of the line
; of changing of date... BEWARE, do not cut arrow which goes out of the
; window!
; test: If it goes out of the frame, but, thanks to +/- 360° it come in,
; we modify it
;
  ind = where(glam1 LT !x.range[0] AND glam1+360. LE !x.range[1])
  if ind[0] NE -1 then glam1[ind] = glam1[ind]+360.
  ind = where(glam1 GT !x.range[1] AND glam1-360. GE !x.range[0])
  if ind[0] NE -1 then glam1[ind] = glam1[ind]-360.

  ind = where(glam LT !x.range[0] AND glam+360. LE !x.range[1])
  if ind[0] NE -1 then glam[ind] = glam[ind]+360.
  ind = where(glam  GT !x.range[1] AND glam-360. GE !x.range[0])
  if ind[0] NE -1 then glam[ind] = glam[ind]-360.
;
;
; Stage 1, e)
;
  r = convert_coord(glam, gphi, /data, /to_normal)
  x0 = r[0, *]                  ; normal coordinates of the beginning of the array.
  y0 = r[1, *]                  ;

  r = convert_coord(glam1, gphi1, /data, /to_normal)
  x1 = r[0, *]                  ; normal coordinates of the ending of the array (Before scaling).
  y1 = r[1, *]                  ;
;
; tests to avoid that arrows be drawing out of the domain.
;
  out = where(x0 LT !p.position[0] OR x0 GT !p.position[2]  $
              OR y0 LT !p.position[1] OR y0 GT !p.position[3])
  if out[0] NE -1 THEN x0[out] = !values.f_nan
;
; Following projections, there may are points at NaN when we pass in normal coordinates.
; We delete these points.
;
  help, x0, x1, y0, y1
  nan = finite(x0*y0*x1*y1) & help, nan
  number = where(nan EQ 1)  & help, number
  x0 = x0[number] & x1 = x1[number]
  y0 = y0[number] & y1 = y1[number]
  msk = msk[number]
  norme = norme[number]
;
  points = where(msk EQ 1)
  IF points[0] NE -1 THEN BEGIN 

  x0 = x0[points] & x1 = x1[points]
  y0 = y0[points] & y1 = y1[points]
  msk = msk[points]
  norme = norme[points]

; We define the vector direction in the normalize reference.
;
    dirx = x1-x0
    diry = y1-y0
;
;We pass in polar coordinates to recuperate the angle which wasb the goal of all the first stage!!!
;
    dirpol = cv_coord(from_rect = transpose([ [dirx[*]], [diry[*]] ]), /to_polar)
    dirpol = msk*dirpol[0, *]
;
; Stage 2
;
; Now we take care of the norme...
;
; Automatic putting at the scale
;
    if NOT keyword_set(cmref) then BEGIN
      mipgsz = min(page_size, max = mapgsz)
      sizexfeuille = mipgsz*key_portrait+mapgsz*(1-key_portrait)
      sizexfeuille = 10.*sizexfeuille
      cmref = 5 > floor(sizexfeuille/10.) < 15
      cmref = cmref/10.
    ENDIF
    if NOT keyword_set(normeref) then BEGIN
      value = max(norme)
      puissance10 = 10.^floor(alog10(value))
      normeref = puissance10*floor(value/puissance10)
    endif
    cm = 1.*normeref/cmref
;
; We modify the array norme to an element having the value cm be represented
; by a line of lenght 1 cm on the paper. Norme contains the norm of vectors
; we want to draw.
;
; define colors before norme is changed...
    IF NOT KEYWORD_SET(vectcolor) THEN vectcolor = 0
    IF keyword_set(vectnormcolor) THEN BEGIN 
      mp = projsegment([vectnormcolor], [1, 254], /mp)
      colors = byte(round(mp[0] * norme +  mp[1] ))
    ENDIF ELSE colors = byte(vectcolor)
;
    norme = 1/(1.*cm)*norme*cv_cm2normal(dirpol)
;
; Stage 3
; Now that we have the angle and the norm, we recuperate coordinates in
; rectangular and we draw arrows.
;
    r = cv_coord(from_polar = transpose([ [dirpol[*]], [norme[*]] ]), /to_rect)
    composantex = r[0, *]
    composantey = r[1, *]
;
    x1 = x0+composantex
    y1 = y0+composantey
;
; Drawing
;
    points = where(msk EQ 1)
    IF n_elements(colors) GT 1 THEN BEGIN
      FOR i = 0, n_elements(colors)-1 DO arrow, x0[i], y0[i], x1[i], y1[i], /norm, hsize = -.2, COLOR = colors[i], THICK = vectthick
    ENDIF ELSE arrow, x0, y0, x1, y1, /norm, hsize = -.2, COLOR = colors, THICK = vectthick
;
;  Draw an arrow at the right bottom of the drawing as a caption.
;
    if NOT keyword_set(novectref) then BEGIN
      dx = cmref*cv_cm2normal(0) ; Length of the vector of reference in normalized coordinates.
      if keyword_set(vectrefformat) then $
         normelegende = strtrim(string(normeref, format = vectrefformat), 1)+' ' $
      ELSE normelegende = strtrim(normeref, 1)+' '
;
      if keyword_set(vectrefpos) then begin
        r = convert_coord(vectrefpos, /data, /to_normal)
        x0 = r[0]
        y0 = r[1]
      ENDIF ELSE BEGIN
        x0 = !x.window[1]-dx
        r = convert_coord(!d.x_ch_size, !d.y_ch_size, /device, /to_normal)
        dy = 3*r[1]*!p.charsize
        y0 = !y.window[0]-dy
      ENDELSE

      arrow, x0, y0, x0+dx, y0, /norm, hsize = -.2, color = 0
      IF NOT keyword_set(vectrefcharsize) THEN vectrefcharsize = !p.charsize
      IF NOT keyword_set(vectrefcharthick) THEN vectrefcharthick = !p.charthick
      xyouts, x0, y0, normelegende, /norm, align = 1, charsize = vectrefcharsize, charthick = vectrefcharthick, color = 0

    endif
  endif
;
;

  if keyword_set(key_performance) NE 0 THEN print, 'temps vecteur', systime(1)-tempsun
;------------------------------------------------------------
;------------------------------------------------------------
  return
END




