;+
;
; @file_comments
; Trace vertical graphs.
;
; @categories
; Graphics
;
; @param TAB
; The field whose we want to make a vertical cut can be 2 kind of thing:
; 1) An 2d or 3d array.
;    If the field is 2d, indicate, with the keyword BOXZOOM, geographic delineations of the boxzoom.
;    If the field is 3d, we extract the section on we average possibly before to do the plot.
; 2) a structure respecting all criterions specified by <pro>litchamp</pro>.
;    The array contained in the structure must be 2 or 3d (See case 1)
;
; @param GIVENTYPE
;
; @param GIVENMIN {required}
; valeur minimum que l'on veut prendre en compte dans le trace
; des contours. Par defaut on prend le min de tab1 (sur les pts mer)
;
; @param GIVENMAX {required}
; valeur maximum que l'on veut prendre en compte dans le trace
; des contours. Par defaut on prend le max de tab1 (sur les pts mer)
;
; @keyword BOXZOOM
; Vector indicating the geographic zone on which we want to cut the map.
;  If BOXZOOM has :
; 1 element : The extraction is made on [lon1, lon2, lat1, lat2, 0.,boxzoom[0]]
; 2 elements: The extraction is made on [lon1, lon2, lat1, lat2, boxzoom[0],boxzoom[1]]
; 4 elements: The extraction is made on [Boxzoom, 0, max([gdept, gdepw])]
; 5 elements: The extraction is made on [Boxzoom[0:3], 0, Boxzoom[4]]
; 6 elements: The extraction is made on Boxzoom
;
; Where lon1, lon2,lat1,lat2 are global variables defined at the last 
; <pro>domdef</pro> !
;
; @keyword CONTINTERVALLE
; When CONTOUR is activated, it is the value between 2 isolines
; traced by a trait. So it can be different from the one specified by INTERVALLE which,
; in this case, does not control colored isolines in color anymore. If there is noone
; specified min, we choose a contour min which goes well with the specified interval!
; If this keyword is not specified, we trace 20 isolines from the min to the max.
;
; @keyword CONTLABEL {type=integer}
; When CONTOUR is activated, if n
; is different of 0, choose the label type corresponding to n cases for
; the traced by a traitisolines. To specify the type of label of the
; colored contour, use LABEL
;
; @keyword CONTMAX {default=max of the keyword CONTOUR (on ocean points)}
; When CONTOUR is activated, max value we want to consider in the isoline
; traced by a trait's line.
;
; @keyword CONTMIN {default=min of the keyword CONTOUR (on ocean points)}
; When CONTOUR is activated, min value we want to consider in the isoline
; traced by a trait's line.
;
; @keyword CONTNLEVEL {default=20}
; When  CONTOUR is activated, it is the number of contours
; traced by a trait for drawing (active if
; CONTLABEL=0).
;
; @keyword CONTOUR
; If we want to trace contours of a different field than the one
; whose we have the colored drawing (by example E-P in color and QSR in contours).
; It must be a field respecting same characteristics than the argument number one of plt.
;
; @keyword ENDPOINTS
; keyword specifying that we want to make a vertical cut in diagonal. Then coordinated of extremities
; of these ones are defined by the 4 elements of the vector ENDPOINTS: [x1,y1,x2,y2] which are
; coordinates.
;
; @keyword INTERVALLE
; Value of an interval between two isolines. If there is none specified min,
; we choose a min contour which goes well with the specified interval!. If this keyword is not
; specified, we trace 20 isoline from the min to the max. Comment: When CONTOUR is activated,
; INTERVALLE only specify the interval between 2 colored isolines. To specify the interval
; between 2 isolines traced by a trait, use CONTINTERVALLE.
;
; @keyword INV
; Invert the color vector used to color the graph
;               (without use the black, the white and the used palette)
;
; @keyword ZRATIO {default=2./3}
; When the drawing has a zoomed part, it is the size ratio between the zoomed 
; part, hz (zoom height), and the whole drawing, ht (total height).
;
; @keyword LABEL {default=0}{type=integer}
; It choose the label's type corresponding to cases in <pro>label</pro>
; Comment: When CONTOUR is activated, it only specify the label's type for colored isolines.
; For these ones traced by a trait, use CONTLABEL.
;
; @keyword MASKFILL
; set to 0 to avoid to fill the masked values
;
; @keyword MAXIN
; to specify the max value we want to plot with a keyword instead of with the
; input parameter max. If max is defined by both, parameter and keyword, the
; keyword is retained.
;
; @keyword MININ
; to specify the min value we want to plot with a keyword instead of with the
; input parameter min. If min is defined by both, parameter and keyword, the
; keyword is retained.
;
; @keyword NLEVEL {default=20}
; Number of contours to draw. active if
; LABEL=0 or is not specified.
;
; @keyword NOFILL
; To make just isolines with no filling
;
; @keyword NOMASK
; activate to supress the use of the land-sea mask when plotting the field
; 
; @keyword NO_PARTIALSTEP
; activate to supress the use of partial steps (force key_partialstep = 0 within pltz)
;
; @keyword NOTRI
; To force not to use the triangulation. Beware, in this case, the
; drawing only works if the grid is undeformed (It means that each point of a longitude
; give one latitude and each point of a latitude give one longitude) except if we use
; the keyword CELL_FILL=2.
; Comment: if the field contain points !values.f_nan, then we even do a triangulation.
;
; @keyword MASKDTA
; 2d array defining the mask that must be applied to the data instead
; of the default mask which is defined according to the grid (t/u/v/f mask)
;
; @keyword OVERPLOT
; To make a plot over an other one.
; Comment: Contrarily to the use of CONTOUR or VECTEUR, the use of this keyword
; does not the caption and/or the color bar.
;
; @keyword SIN
; Activate this keyword if we want the x axis to be traced in sinus of the
; latitude when we make a drawing f(y)
;
; @keyword STRICTFILL
; Activate this keyword to that the filling of contours be
; precisely done between the min and the max specified. Values inferior/superior at the
; specified min/max are not filled. Note that max values a considered
; as missing values so cells containing one or more corners with
; values above max will have no contours drawn through them.  
;
; @keyword STYLE {default=style=0}
; Contour's style to adopt to draw isolines. See <pro>style</pro> for more informations
;
; @keyword WDEPTH
; To specify that the field is at W depth instead of T
; depth (automatically activated if vargrid eq 'W')
;
; @keyword XZ
; Force to make a cut following xz
;
; @keyword YZ
; Force to make a cut following yz
;
; @keyword ZOOM  {default=200m or max depth if it is inferior at 200m}
; Depth where i can make our maximum zoom.
;
; @uses
; <pro>common</pro>
;
; @history
;  Sebastien Masson (smasson\@lodyc.jussieu.fr)
; 7/1999
; Sebastien Masson 08/02/2000 checkfield and
; notri keyword.
;
; @version
; $Id: pltz.pro 388 2008-12-09 09:36:41Z smasson $
;
; @todo 
; seb definition of paramaters L.215 à 221
;
;-
PRO pltzz, gdep_2D, tab, giventype, givenmin, givenmax $
        , BOXZOOM=boxzoom, CONTOUR=contour $
        , ENDPOINTS=endpoints, INTERVALLE=intervalle, INV=inv $
        , ZRATIO=zratio $
        , CONTINTERVALLE=contintervalle, LABEL=label, CONTLABEL=contlabel $
        , STYLE=style, CONTMAX=contmax, SIN=sin, TYPEIN=typein $
        , CONTMIN=contmin, NLEVEL=nlevel, CONTNLEVEL=contnlevel $
        , NOFILL=nofill, NOMASK = nomask, NO_PARTIALSTEP = no_partialstep, NOTRI=notri $
        , USETRI = usetri, FILLXDIR = fillxdir $
        , ZOOM=zoom, XZ=xz, YZ=yz, MININ=minin, MAXIN=maxin $
        , STRICTFILL=strictfill, OVERPLOT=overplot $
        , WDEPTH=wdepth, REALSECTION=realsection, MASKFILL=maskfill, MASKDTA = maskdta $
        , _EXTRA=ex
;
  compile_opt idl2, strictarrsubs
;
@cm_4mesh
@cm_4data
@cm_4ps
  IF NOT keyword_set(key_forgetold) THEN BEGIN
@updatenew
@updatekwd
  ENDIF
;--------------------------------------------------------------
  tempsun = systime(1)          ; For key_performance
;------------------------------------------------------------
;--------------------------------------------------------------
; 1st part: initialization small calculations...
;--------------------------------------------------------------
; Comment: we do not reinitialize when we call back plt in loop to use contour.
  if n_elements(contour) ne 4 AND NOT keyword_set(overplot) then reinitplt
;
  if n_elements(contour) ne 4 THEN saveboxparam, 'boxparam4pltz.dat'
  key_partialstep = keyword_set(key_partialstep) * (1b - keyword_set(no_partialstep)) 
;--------------------------------------------------------------
;  Reading of the field.
;--------------------------------------------------------------
  if n_elements(giventype) NE 0 then type = giventype
  if n_elements(givenmin) NE 0 then min = givenmin
  if n_elements(givenmax) NE 0 then max = givenmax
  if n_elements(minin) NE 0 then min = minin
  if n_elements(maxin) NE 0 then max = maxin
  if n_elements(realsection) EQ 0 then realsection = 1
  IF n_elements(usetri) EQ 0 THEN BEGIN
    IF n_elements(notri) NE 0 THEN usetri = 2-notri ELSE usetri = 1
  ENDIF
; no need of triangulation
  IF usetri EQ 1 AND keyword_set(realsection) THEN usetri = 0
; did we specify the type ?
  if keyword_set(typein) then BEGIN
    if size(type, /type) NE 7 AND size(type, /type) NE 0 then begin
      if n_elements(min) NE 0 then max = min
      min = type
    endif
    type = typein
  ENDIF
;
  checktypeminmax, 'pltz', TYPE = type, MIN = min, MAX = max $
    , XZ = xz, YZ = yz, ENDPOINTS = endpoints, _extra = ex
;
  if keyword_set(endpoints) then begin
    section, tab, z2d, glam, gphi, ENDPOINTS = endpoints, TYPE = type $
    , BOXZOOM = boxzoom, DIREC = direc, WDEPTH = wdepth, _extra = ex
    if n_elements(z2d) EQ 1 AND z2d[0] EQ -1 AND n_elements(contour) ne 4 then BEGIN
      restoreboxparam, 'boxparam4pltz.dat'
      return
    ENDIF
    nx = n_elements(glam)
    ny = nx
    if strupcase(vargrid) EQ 'W' then begin
      gdep = gdepw[firstzw:lastzw]
      nz = nzw
    ENDIF ELSE BEGIN
      gdep = gdept[firstzt:lastzt]
      nz = nzt
    ENDELSE
    mask = z2d LE valmask/10.
  ENDIF ELSE BEGIN
    z2d = checkfield(tab, 'pltz', TYPE = type, BOXZOOM = boxzoom $
                     , DIREC = direc, WDEPTH = wdepth, _extra = ex)
    if n_elements(z2d) EQ 1 AND z2d[0] EQ -1 AND n_elements(contour) ne 4 then BEGIN
      restoreboxparam, 'boxparam4pltz.dat'
      return
    ENDIF
    IF realsection EQ 1 THEN grille, mask, glam, gphi, gdep, nx, ny, nz, type = type, WDEPTH = wdepth $
    ELSE grille, mask, glam, gphi, gdep, nx, ny, nz, WDEPTH = wdepth
  ENDELSE
  gdep = gdep_2D & help, gdep
  IF size(gdep, /n_dimensions) EQ 2 THEN usetri = 2
;---------------------------------------------------------------
  profmax = !y.range[0]
  profmin = !y.range[1]
  if not keyword_set(zoom) then zoom = 200
  zoom = zoom[0]
  IF zoom LT profmin THEN zoom = profmax
  if zoom GE vert2 then zoom = profmax
; construction of the mask and of the axis
  axis4pltz, type, mask, glam, gphi, gdep, XXAXIS = xxaxis, ZZAXIS = zzaxis, SIN = sin, ZRATIO = zratio, ZOOM = zoom, PROFMAX = profmax, PROFMIN = profmin, _extra = ex
; to draw from bottom to top (avoid using cell_fill)
  z2d = reverse(z2d, 2)
  szmsk = size(mask, /dimensions)
  IF keyword_set(nomask) THEN mask = replicate(1b, szmsk[0], szmsk[1]) 
  IF keyword_set(maskdta) THEN mask = byte(fitintobox(maskdta))
;-----------------------------------------------------------------------------
; Determination of the mi:min and of the ma:max of z2d in the same way
; as max: max and min: min for the drawing.
;-----------------------------------------------------------------------------
  nan = total(finite(z2d, /nan)) < 1
; Do we need to do an autoscale ???
  autoscale = testvar(var = min) EQ testvar(var = max) AND NOT keyword_set(intervalle)
  determineminmax, z2d, mask, mi, ma, MININ = min, MAXIN = max, nan = nan, INTERVALLE = intervalle, _extra = ex
  if n_elements(z2d) EQ 1 AND z2d[0] EQ -1 THEN GOTO, sortie
; We do an autoscale if needed.
  if autoscale then autoscale, min, max, intervalle
;--------------------------------------------------------------
;--------------------------------------------------------------
; 2nd part: drawing
;--------------------------------------------------------------
;--------------------------------------------------------------
  if n_elements(contour) NE 4 AND NOT keyword_set(overplot) THEN $
    placedessin, 'pltz', posfenetre, posbar, contour = contour, endpoints = endpoints $
    , direc = direc, _extra = ex
;------------------------------------------------------------
;--------------------------------------------------------------
; choice of labels
;-----------------------------------------------------------
  if keyword_set(intervalle) AND NOT keyword_set(label) then label = 1
  if keyword_set(label) eq 0 then cas = 0 else cas = label
  label, cas, min, max, ncontour, level_z2d, colnumb, NLEVEL = nlevel $
         , INTERVALLE = intervalle, strictfill = strictfill
;--------------------------------------------------------------
; choice of style
;-----------------------------------------------------------
  if not keyword_set(style) then style = 0
  style, style, level_z2d, linestyle, thick
  if keyword_set(inv) then colnumb = reverse(colnumb)
;-----------------------------------------------------------
;   definition of axes
;----------------------------------------------------------
  if NOT keyword_set(overplot) THEN axe, type, SIN = sin, _EXTRA = ex
  !y.range = [-1, 0]
;--------------------------------------------------------------
; extrapolation of data on lands and specifying of the min/max value
;--------------------------------------------------------------
; define masknan
  if keyword_set(nan) then BEGIN
    masknan = finite(z2d)
    IF NOT keyword_set(nofill) THEN z2d[where(masknan EQ 0)] = max
  ENDIF ELSE masknan = 1
  IF keyword_set(strictfill) THEN BEGIN
    tmp = z2d ge max
    IF total(tmp GE 1) THEN BEGIN 
      tmp = 1b - byte(extrapolate(tmp, tmp, 1))
      key_save = key_periodic & key_periodic = 0
      trifield = triangule(temporary(tmp)*mask, coinmonte = coinmontemask $
                           , coindescend = coindescendmask, keep_cont = 0)
      key_periodic = key_save
    ENDIF
  ENDIF
  IF n_elements(fillxdir) EQ 0  THEN fillxdir = 1
  IF keyword_set(fillxdir) THEN BEGIN
; filling the mask values only in the x direction.
    z2d = remplit(z2d, nite = nx*(1-(n_elements(maskfill) NE 0)) $
                  , mask = mask*masknan, /basique $
                  , /fillxdir, _extra = ex)
; if some lines contains no field (the bottom line for ex) it must be
; also filled with 8 neighbors method
    IF (where(total(mask*masknan, 1) EQ 0))[0] NE -1 THEN $
      z2d = remplit(z2d, nite = 1-(n_elements(maskfill) NE 0) $
                    , mask = z2d LT valmask/10, /basique, _extra = ex)
  ENDIF ELSE BEGIN
; filling the mask values with 8 neighbors
    z2d = remplit(z2d, nite = (1+(vargrid NE 'T')+keyword_set(nan)) $
                  *(1-(n_elements(maskfill) NE 0)), mask = mask*masknan $
                  , /basique, _extra = ex)
  ENDELSE
  if keyword_set(strictfill) EQ 0 AND n_elements(maskfill) EQ 0 $
    then z2d = min > z2d <  max
  if n_elements(maskfill) NE 0 then BEGIN
    z2d = z2d*mask*masknan
    if maskfill NE 0 then z2d = temporary(z2d) + maskfill*(1b-mask*masknan)
  ENDIF
;----------------------------------------------------------
; check the mask and the triangulation according to the grid type and
; nan values. find the coordinates of the mask
;----------------------------------------------------------
;   if (where(mask EQ 0))[0] EQ -1 AND NOT keyword_set(nan) then notri = 1
;   if keyword_set(notri) then trifield = -1 $
;   ELSE trifield = triangule(mask,/basic)
  if n_elements(key_save) EQ 0 AND ((usetri GE 1 AND (vargrid EQ 'T' OR vargrid EQ 'W')) $
                                      OR (usetri EQ 2 AND (vargrid NE 'T' AND vargrid NE 'W'))) THEN $
                                         trifield = triangule(mask, /basic)
;
  IF NOT keyword_set(endpoints)  THEN BEGIN
    if keyword_set(nan) then trinan = triangule(masknan, /basic, coinmonte = coinmontenan, coindescend = coindescendnan)
    maskorg = mask
    decoupeterre, mask, glammsk, gphimsk, gdepmsk, type = type, WDEPTH = wdepth, REALSECTION = realsection, MASKDTA = maskdta
    axis4pltz, type, mask, glammsk, gphimsk, gdepmsk, XXAXIS = xmask, ZZAXIS = zmask, SIN = sin, ZRATIO = zratio, ZOOM = zoom, PROFMAX = profmax, PROFMIN = profmin, _extra = ex
    szmsk = size(mask, /dimensions)
    IF keyword_set(nomask) THEN mask = replicate(1b, szmsk[0], szmsk[1]) 
  ENDIF ELSE BEGIN
    xmask = xxaxis
    zmask = zzaxis
  ENDELSE
;
  if (usetri GE 1 AND (vargrid NE 'T' AND vargrid NE 'W')) THEN BEGIN
    IF keyword_set(realsection) THEN trimsk = triangule(mask, /basic) $
    ELSE trimsk = triangule(mask, /basic, coinmonte = coinmontemask $
                            , coindescend = coindescendmask)
  ENDIF
;------------------------------------------------------------
; dessin en lui meme
;------------------------------------------------------------
  IF n_elements(romszinfos) EQ 1 THEN BEGIN
; add one line at bottom to have nicer plot (colors go until the ocean bottom)
    IF n_elements(romszinfos.h) NE 1 THEN BEGIN
      CASE type OF
        'xz':romsh = moyenne(romszinfos.h, 'y')
        'yz':romsh = moyenne(romszinfos.h, 'x')
      ENDCASE
      IF nzt EQ jpk THEN BEGIN
        z2d = [[z2d], [z2d[*, jpk-1]]]
        zzaxis = [[zzaxis], [romsh]]
      ENDIF
    ENDIF
  ENDIF
  pltbase, z2d, xxaxis, zzaxis, mask, xmask, zmask $
    , level_z2d, colnumb, overplot = overplot $
    , contour = contour, trichamp = trifield, trimsk = trimsk  $
    , c_linestyle = linestyle $
    , c_labels = 1-(indgen(n_elements(level_z2d)) MOD 2) $
    , c_thick = thick, unsur2 = unsur2, NOFILL = nofill $
    , maskorg = maskorg, masknan = masknan, trinan = trinan $
    , coinmontenan = coinmontenan, coindescendnan = coindescendnan $
    , coinmontemask = coinmontemask, coindescendmask = coindescendmask $
    , REALSECTION = realsection, USETRI = usetri, _extra = ex
;------------------------------------------------------------
; recall of pltz in loop when contour is activated
;------------------------------------------------------------
  if n_elements(contour) eq 4 then BEGIN ; It is the second time I pass in pltt
    contour = {mietma:[mi, ma], unit:varunit, inter:intervalle} ; I send back the min, the max and the unity
    return
  endif
  if keyword_set(contour) THEN BEGIN
    pourlegende = [1, 1, 1, 1]
    oldattributs = saveatt()
    oldcolnumb = colnumb
    pltz, contour, contmin, contmax, CONTOUR = pourlegende, ZRATIO = zratio $
      , INTERVALLE = contintervalle, LABEL = contlabel, STYLE = style, /noerase  $
      , NLEVEL = contnlevel, ZOOM = zoom, BOXZOOM = boxzoom, ENDPOINTS = endpoints $
      , STRICTFILL = strictfill, REALSECTION = realsection, MASKFILL = maskfill $
      , USETRI = usetri, _extra = ex
    restoreatt, oldattributs
    colnumb = oldcolnumb
  ENDIF
;------------------------------------------------------------
;------------------------------------------------------------
; 3rd part: drawing of the frame, caption, colorbar...
;------------------------------------------------------------
  if keyword_set(overplot) then BEGIN
    !y.range =  [zoom, profmin] ; We get back on physic coordinates
    plot, [0], [0], /nodata, /noerase, title = '', subtitle = '', xstyle = 5, ystyle = 5
    GOTO, fini
  endif
;------------------------------------------------------------
; Caption + display of them
;------------------------------------------------------------
  legende, mi, ma, type, CONTOUR = pourlegende, INTERVALLE = intervalle, DIREC = direc, endpoints = endpoints, _EXTRA = ex
  if type eq 'yz' then xaxe = 'lataxe' else xaxe = 'lonaxe'
  if keyword_set(sin) OR NOT key_onearth then xaxe = ''
; Frame applied by default
  plot, [xxaxis[0], xxaxis[n_elements(xxaxis)-1]], [-zratio, -zratio], /noerase $
    , xstyle = 1+4*(keyword_set(endpoints) AND ((type EQ 'xz' AND lat1 NE lat2) OR (type EQ 'yz' AND lon1 NE lon2))) $
    , xtickformat = xaxe, _extra = ex
; Add of an axis in the case of we use endpoints
  if keyword_set(endpoints) then addaxe, endpoints, type, posfenetre, _EXTRA = ex
;------------------------------------------------------------
; Y axis in 1 or 2 part
;------------------------------------------------------------
  if n_elements(ex) NE 0 then BEGIN
; To do not put title anymore
    if (where(tag_names(ex) EQ 'TITLE'))[0] NE -1 then ex.TITLE = ' '
; To do not put subtitle anymore
    if (where(tag_names(ex) EQ 'SUBTITLE'))[0] NE -1 then ex.SUBTITLE = ' '
; To have just one ytitle
    if (where(tag_names(ex) EQ 'YTITLE'))[0] NE -1 then BEGIN
      ytitle = ex.YTITLE
      ex.YTITLE = ' '
    endif
  ENDIF
  htotal = posfenetre[3]-posfenetre[1]
  hzoom = 1.*zratio*htotal
  if zoom LT profmax then $
    plot, [0], [0], /nodata, /noerase, ystyle = 1, yrange = [profmax, zoom+0.001] $
    , position = posfenetre+[0, 0, 0, -hzoom], _extra = ex, title = '', subtitle = '', ytitle = ''
;
  !y.range =  [zoom, profmin]   ; We get back in physic coordinates!
  plot, [0], [0], /nodata, /noerase, ystyle = 1, _extra = ex $
    , title = '', subtitle = '', ytitle = '', position = posfenetre+[0, htotal-hzoom, 0, 0]
;------------------------------------------------------------
; to write the ytitle...
;------------------------------------------------------------
  if !d.name EQ 'PS' then $
    xs = (max(page_size, min = mi)*(1-key_portrait) + mi*key_portrait)*!d.x_px_cm $
  ELSE xs = !d.x_size
  if n_elements(ytitle) NE 0 then !y.title = ytitle
  charsize = chkstru(ex, 'ycharsize', /extract)
  if charsize EQ -1 then charsize = !p.charsize
  IF chkstru(ex, 'charsize') THEN ex.charsize = charsize
  if chkstru(ex, 'ytitle', /extract) NE '' then $
    decalage = string(format = '(e10.3)', profmax)
  decalage = float(strmid(decalage, strpos(decalage, 'e')+1))
  posy = posfenetre[1]+1.*htotal/2
  posx = posfenetre[0]-(decalage+3)*!d.x_ch_size*charsize/xs
  xyouts, posx, posy, !y.title, /normal, orientation = 90, color = 0, ALIGNMENT = .5, charsize = charsize, _extra = ex

;------------------------------------------------------------
; colorbar
;------------------------------------------------------------
  colnumb = colnumb[0:ncontour-1-keyword_set(strictfill)]
  barrecouleur, colnumb, min,  max, (ncontour-keyword_set(strictfill))/2 $
                , position = posbar, _extra = ex
;------------------------------------------------------------
;------------------------------------------------------------
; 4th part: possible print
;------------------------------------------------------------
;------------------------------------------------------------
fini:
  terminedessin, _extra = ex
;------------------------------------------------------------
;------------------------------------------------------------
;------------------------------------------------------------
;------------------------------------------------------------
sortie:
  restoreboxparam, 'boxparam4pltz.dat'
;
  if keyword_set(key_performance) NE 0 THEN print, 'temps pltz', systime(1)-tempsun
  return
end
