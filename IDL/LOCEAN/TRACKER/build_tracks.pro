PRO build_tracks
COMPILE_OPT idl2, strictarrsubs

; A FAIRE: GESTION DOUBLON

;-----------------------------------------------------------------------------------------
; BUILD_TRACKS
;
; RECONSTRUIT LES TRAJECTOIRES A PARTIR DES POINTS RETENUS PAR "SELECT_MAXIMA"
;
;
; methodology:
; -----------
; - GUESS PAR ADVECTION SUR 1 PAS DE TEMPS DE LA POSITION DU CYCLONE
; - RECHERCHE D'UN POINT RETENU DANS LA ZONE AUTOURS DE LA POSITION ESTIMEE PAR ADVECTION
;   - SI POINT TROUVE -> NOUVELLE POSITION CYCLONE
;   - SI AUCUN POINT  -> RELAXATION DES CRITERES SAUF VORTICITE ET RECHERCHE MAXIMUM VORTICITE
;     - SI CRITERE VORTICITE REMPLI -> NOUVELLE POSITION CYCLONE
;     - SI AUCUN POINT  -> FIN DE LA TRAJECTOIRE DU CYCLONE
; - CHAQUE POINT NON-RACCORDE A UN CYCLONE DEVIENT UN NOUVEAU CYCLONE
; - IDEM EN RETRO-ADVECTION SUR 1 PAS DE TEMPS A PARTIR DE LA POSITION INITIALE DU CYCLONE
;-----------------------------------------------------------------------------------------



;-----------------------------------------------------------------------------------------
; PARAMETRES
;-----------------------------------------------------------------------------------------

expname  = 'COUPLED_SW2_KF' & help, expname
period   = '1990-2009' & help, period
ucrit    = 17.5   & help, ucrit
vorcrit  = 30.e-5 & help, vorcrit
vorelax  = 30.e-5 & help, vorelax
tempcrit = 1.     & help, tempcrit
temptype = 'treal' & help, temptype
date_ini = 19900101.00d & help, date_ini
date_rst = 19900101.00d & help, date_rst
date_end = 20091231.75d & help, date_end
nbmaxtrack = 20000 & help, nbmaxtrack



;-----------------------------------------------------------------------------------------
; MASQUE (1 sur terre, 0 sur mer pour WRF) + GRILLE "T"
;-----------------------------------------------------------------------------------------

maskfile = 'mask_wrf.nc'
initncdf, maskfile, /fullcgrid
mask = ncdf_lec(maskfile, var = 'LANDMASK') & help, mask
lon = float(ncdf_lec(maskfile,var='longitude'))
lat = float(ncdf_lec(maskfile,var='latitude'))
lon2D = lon # replicate(1., n_elements(lat))
lat2D = replicate(1., n_elements(lon)) # lat

; creation d'un nouveau masque "elargi" pour eviter les effets cotiers et les forçages orographiques
newmask = mask
FOR i = 1, n_elements(lon)-2 DO BEGIN
  FOR j = 1, n_elements(lat)-2 DO BEGIN
    IF mask[i,j] EQ 1 THEN BEGIN
      newmask[i-1,j-1] = 1
      newmask[i-1,j  ] = 1
      newmask[i-1,j+1] = 1
      newmask[i,j-1] = 1
      newmask[i,j+1] = 1
      newmask[i+1,j-1] = 1
      newmask[i+1,j  ] = 1
      newmask[i+1,j+1] = 1
    ENDIF
  ENDFOR
ENDFOR
mask = temporary(newmask)



;-----------------------------------------------------------------------------------------
; PATHS + FILES DEFINITIONS
;-----------------------------------------------------------------------------------------

file_in1 = '/net/adonis/usr/adonis/varclim/gslod/EXP_'+ expname +'/TRACKER/vor800_'+ period +'.nc' & help, file_in1
file_in5 = '/net/adonis/usr/adonis/varclim/gslod/EXP_'+ expname +'/TRACKER/uv10_psfc_'+ period +'.nc' & help, file_in5
path_in  = '/net/adonis/usr/adonis/varclim/gslod/IDL/TRACKER/EXP_'+ expname +'/DATA/'
path_out = path_in



;-----------------------------------------------------------------------------------------
; CREATION AXE TEMPS
;-----------------------------------------------------------------------------------------

fid = ncdf_open(file_in1, /nowrite)
tid = ncdf_dimid(fid, 'time')
ncdf_diminq, fid, tid, dimt, nt & help, nt
ncdf_close, fid

juld_ini = date2jul(date_ini) & help, juld_ini
juld_rst = date2jul(date_rst) & help, juld_rst
juld_end = date2jul(date_end) & help, juld_end
tstart   = long((juld_rst-juld_ini)*4)   & help, tstart
tend     = long((juld_end-juld_ini)*4-1) & help, tend
timejuld = juld_rst + indgen(tend-tstart+1)*0.25 & help, timejuld
timedate = jul2date(timejuld)
year = timedate / 10000.
year_ini = long(year[0])
year_end = long(year[n_elements(year)-1])
;print, timedate, format='(f12.2)'



;-----------------------------------------------------------------------------------------
; DECLARATIONS + INITIALISATIONS
;-----------------------------------------------------------------------------------------

; format: (nombre de tc total, pas de temps)
loncycn  = fltarr(nbmaxtrack,nt) + !values.f_nan
latcycn  = fltarr(nbmaxtrack,nt) + !values.f_nan
mslpcycn = fltarr(nbmaxtrack,nt) + !values.f_nan
uv10cycn = fltarr(nbmaxtrack,nt) + !values.f_nan
datecycn = dblarr(nbmaxtrack,nt) + !values.f_nan
juldcycn = dblarr(nbmaxtrack,nt) + !values.f_nan
rvmcycn  = fltarr(nbmaxtrack,nt) + !values.f_nan
anotcycn = fltarr(nbmaxtrack,nt) + !values.f_nan
vorcycn  = fltarr(nbmaxtrack,nt) + !values.f_nan
indexcyc = intarr(nt,20) + !values.f_nan
nbtotalcyc = 0



;-----------------------------------------------------------------------------------------
; LECTURE FICHIER MAXIMA
;-----------------------------------------------------------------------------------------

print, path_in +'maxima_'+temptype+'_nico_radius_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_'+strtrim(long(date_ini),2)+'-'+strtrim(long(date_end),2)+'.idl'
RESTORE, filename = path_in +'maxima_'+temptype+'_nico_radius_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_'+strtrim(long(date_ini),2)+'-'+strtrim(long(date_end),2)+'.idl', /verbose
help, loncyc, latcyc,mslpcyc,uv10cyc



;-----------------------------------------------------------------------------------------
; BOUCLE TEMPORELLE + LECTURE VARIABLES
;-----------------------------------------------------------------------------------------

rayonsearch = 100.
distcrit    = 400. ; 500km dans Chauvin et al. 2006

FOR t = tstart, tend DO BEGIN ; boucle temporelle
print, ''
print, '----------------------------------------------------------------------------------'
print, '- TIME STEP -', t
print, '----------------------------------------------------------------------------------'

indtmpcyc = where(finite(loncyc[t,*]) EQ 1, cnttmpcyc)
print, 'NOMBRE DE SYSTEMES DETECTES:', cnttmpcyc

vort = read_ncdf('vor800', t, t, TIMESTEP=1, FILENAME=file_in1, /NOSTRUCT) & help, vort
u10t = read_ncdf('U10', t, t, TIMESTEP=1, FILENAME=file_in5, /NOSTRUCT) & help, u10t
v10t = read_ncdf('V10', t, t, TIMESTEP=1, FILENAME=file_in5, /NOSTRUCT) & help, v10t
uv10t = (temporary(u10t)^2+temporary(v10t)^2)^0.5
mslpt = read_ncdf('PSFC', t, t, TIMESTEP=1, FILENAME=file_in5, /NOSTRUCT)/100. & help, mslpt
mslpt[where(mask EQ 1)] = !values.f_nan



;-----------------------------------------------------------------------------------------
; RECONSTRUCTION TRAJECTOIRES
;-----------------------------------------------------------------------------------------

; si t = 0 et systeme present -> c'est forcement un nouveau cyclone
IF t EQ 0 AND cnttmpcyc GT 0 THEN BEGIN
  nbtotalcyc = 0
  FOR i = 0, cnttmpcyc-1 DO BEGIN

    indexcyc[t,i] = nbtotalcyc
    loncycn[indexcyc[t,i],t]  = loncyc[t,i]
    latcycn[indexcyc[t,i],t]  = latcyc[t,i]
    mslpcycn[indexcyc[t,i],t] = mslpcyc[t,i]
    uv10cycn[indexcyc[t,i],t] = uv10cyc[t,i]
    vorcycn[indexcyc[t,i],t]  = vorcyc[t,i]
    anotcycn[indexcyc[t,i],t] = anomtcyc[t,i]
    rvmcycn[indexcyc[t,i],t]  = rvmcyc[t,i]

    loncyc[t,i]   = !values.f_nan
    latcyc[t,i]   = !values.f_nan
    mslpcyc[t,i]  = !values.f_nan
    uv10cyc[t,i]  = !values.f_nan
    vorcyc[t,i]   = !values.f_nan
    anomtcyc[t,i] = !values.f_nan
    rvmcyc[t,i]   = !values.f_nan

    print, 'NOUVEAU CYCLONE N: ', long(indexcyc[t,i])
    print, loncycn[indexcyc[t,i],t], latcycn[indexcyc[t,i],t], mslpcycn[indexcyc[t,i],t], uv10cycn[indexcyc[t,i],t]
    nbtotalcyc = nbtotalcyc + 1

  ENDFOR
ENDIF



; pour t > 0
IF t GT 0 THEN BEGIN
  t1 = t-1
  t2 = t-2

  ; on regarde en premier parmi les cyclones deja existants à t-1 et on les complete
  indo = where(finite(indexcyc[t-1,*]) EQ 1, cnto)
  FOR i = 0, cnto-1 DO BEGIN
  print, '' & print, 'CYCLONE ', i, '/', cnto-1
  print, '' & print, 'CYCLONE N:', long(indexcyc[t1,i])

  ; guess
  IF t EQ 1 THEN BEGIN
    longuess = loncycn[indexcyc[t1,i],t1]
    latguess = latcycn[indexcyc[t1,i],t1]
  ENDIF ELSE BEGIN
    IF indexcyc[t1,i] EQ indexcyc[t2,i] THEN BEGIN; a arranger
      longuess = loncycn[indexcyc[t1,i],t1] + (loncycn[indexcyc[t1,i],t1]-loncycn[indexcyc[t2,i],t2])
      latguess = latcycn[indexcyc[t1,i],t1] + (latcycn[indexcyc[t1,i],t1]-latcycn[indexcyc[t2,i],t2])
    ENDIF ELSE BEGIN
      longuess = loncycn[indexcyc[t1,i],t1]
      latguess = latcycn[indexcyc[t1,i],t1]
    ENDELSE
  ENDELSE
  print, 'GUESS:', longuess, latguess
  IF longuess LT lon[0] OR longuess GT lon[n_elements(lon)-1] THEN print, 'POINT HORS DU DOMAINE'
  IF latguess LT lat[0] OR latguess GT lat[n_elements(lat)-1] THEN print, 'POINT HORS DU DOMAINE'


  ; on teste la distance entre le guess et tous les maxima detecte au temps t
  IF longuess GT lon[0] AND longuess LT lon[n_elements(lon)-1] AND $
     latguess GT lat[0] AND latguess LT lat[n_elements(lat)-1] THEN BEGIN

    indok = where(finite(loncyc[t,*]) EQ 1, cntok)
    IF cntok GT 0 THEN BEGIN ; au moins 1 maximum detecte au temps t
    distcyc = reform(map_npoints(longuess, latguess, loncyc[t,indok], latcyc[t,indok]))/1000.
    indold = where(distcyc LE distcrit, cntold)
    indnew = where(distcyc GT distcrit, cntnew)

    IF cntold EQ 1 THEN BEGIN

      indexcyc[t,i] = indexcyc[t1,i]
      loncycn[indexcyc[t,i],t]  = loncyc[t,indok[indold]]
      latcycn[indexcyc[t,i],t]  = latcyc[t,indok[indold]]
      mslpcycn[indexcyc[t,i],t] = mslpcyc[t,indok[indold]]
      uv10cycn[indexcyc[t,i],t] = uv10cyc[t,indok[indold]]
      vorcycn[indexcyc[t,i],t]  = vorcyc[t,indok[indold]]
      anotcycn[indexcyc[t,i],t] = anomtcyc[t,indok[indold]]
      rvmcycn[indexcyc[t,i],t]  = rvmcyc[t,indok[indold]]

      loncyc[t,indok[indold]] = !values.f_nan
      latcyc[t,indok[indold]] = !values.f_nan
      mslpcyc[t,indok[indold]] = !values.f_nan
      uv10cyc[t,indok[indold]] = !values.f_nan
      vorcyc[t,indok[indold]] = !values.f_nan
      anomtcyc[t,indok[indold]] = !values.f_nan
      rvmcyc[t,indok[indold]] = !values.f_nan

      print, loncycn[indexcyc[t,i],t], latcycn[indexcyc[t,i],t], mslpcycn[indexcyc[t,i],t], uv10cycn[indexcyc[t,i],t]
      print, 'DISTANCE POSITION PRECEDENTE:', distcyc[indold], '/', distcrit

    ENDIF

;   si aucun maxima au temps t ne correspond au guess du cyclone -> relaxation
    IF cntold EQ 0 THEN BEGIN
      distcyc = reform(map_npoints(longuess, latguess, lon2D, lat2D))/1000.
      indcercle = where(distcyc LE distcrit, cntcercle)
;     localisation du maximum de vorticite
      indvorelax = where(abs(vort[indcercle]) GE vorelax, cntvorelax)

      IF cntvorelax GT 0 THEN BEGIN
        maxvorelax = max(abs(vort[indcercle[indvorelax]]), indmaxvorelax, /NAN)
        lonvorelax = lon2D[[indcercle[indvorelax[indmaxvorelax]]]]
        latvorelax = lat2D[[indcercle[indvorelax[indmaxvorelax]]]]
;        print, 'VOR:', lonvorelax[0], latvorelax[0], maxvorelax[0]
	; first guess
        rayon = reform(map_npoints(lonvorelax,latvorelax,lon2D,lat2D)/1000.)
        indcercle = where(rayon LE rayonsearch, cntcercle)
        maxuvrelax = max(uv10t[indcercle], indmaxuvrelax, /NAN)
        lonuvrelax = lon2D[indcercle[indmaxuvrelax]]
        latuvrelax = lat2D[indcercle[indmaxuvrelax]]
;        print, '1GUESS UV10:', lonuvrelax, latuvrelax, maxuvrelax
	; second guess
	rayon = reform(map_npoints(lonuvrelax,latuvrelax,lon2D,lat2D)/1000.)
        indcercle = where(rayon LE rayonsearch, cntcercle)
        maxuvrelax = max(uv10t[indcercle], indmaxuvrelax, /NAN)
        lonuvrelax = lon2D[indcercle[indmaxuvrelax]]
        latuvrelax = lat2D[indcercle[indmaxuvrelax]]
;        print, '2GUESS UV10:', lonuvrelax, latuvrelax, maxuvrelax

        IF mask[where(lon EQ lonvorelax[0]),where(lat EQ latvorelax[0])] EQ 0 THEN BEGIN

	  ; first guess
          minmslprelax = min(mslpt[indcercle], indmslprelax, /NAN)
          lonmslprelax = lon2D[indcercle[indmslprelax]]
          latmslprelax = lat2D[indcercle[indmslprelax]]
 ;         print, '1GUESS MSLP:', lonmslprelax, latmslprelax, minmslprelax
          ; second guess
	  rayon = reform(map_npoints(lonmslprelax,latmslprelax,lon2D,lat2D)/1000.)
          indcercle = where(rayon LE rayonsearch, cntcercle)
          minmslprelax = min(mslpt[indcercle], indmslprelax, /NAN)
          lonmslprelax = lon2D[indcercle[indmslprelax]]
          latmslprelax = lat2D[indcercle[indmslprelax]]
;          print, '2GUESS MSLP:', lonmslprelax, latmslprelax, minmslprelax

          testdist = map_2points(lonmslprelax,latmslprelax,loncycn[indexcyc[t1,i],t1],latcycn[indexcyc[t1,i],t1],/METERS)/1000.
          print, 'DISTANCE POSITION PRECEDENTE:', long(testdist), '/', long(distcrit)
	  
          IF testdist LE distcrit THEN BEGIN
            indexcyc[t,i] = indexcyc[t1,i]
            loncycn[indexcyc[t,i],t]  = lonmslprelax
            latcycn[indexcyc[t,i],t]  = latmslprelax
            mslpcycn[indexcyc[t,i],t] = minmslprelax
            uv10cycn[indexcyc[t,i],t] = maxuvrelax
            print, loncycn[indexcyc[t,i],t], latcycn[indexcyc[t,i],t], mslpcycn[indexcyc[t,i],t], uv10cycn[indexcyc[t,i],t]
	  ENDIF ELSE BEGIN
            print, 'PAS DE SUCCESSEUR -> FIN DU CYCLONE N:', long(indexcyc[t,i])
          ENDELSE

        ENDIF ELSE BEGIN

          testdist = map_2points(lonvorelax,latvorelax,loncycn[indexcyc[t1,i],t1],latcycn[indexcyc[t1,i],t1],/METERS)/1000.
          print, 'DISTANCE POSITION PRECEDENTE:', long(testdist), '/', long(distcrit)

          IF testdist LE distcrit THEN BEGIN
            indexcyc[t,i] = indexcyc[t1,i]
            loncycn[indexcyc[t,i],t]  = lonvorelax
            latcycn[indexcyc[t,i],t]  = latvorelax
            mslpcycn[indexcyc[t,i],t] = !values.f_nan
            uv10cycn[indexcyc[t,i],t] = maxuvrelax
            print, loncycn[indexcyc[t,i],t], latcycn[indexcyc[t,i],t], mslpcycn[indexcyc[t,i],t], uv10cycn[indexcyc[t,i],t]
	  ENDIF ELSE BEGIN
            print, 'PAS DE SUCCESSEUR -> FIN DU CYCLONE N:', long(indexcyc[t,i])
          ENDELSE

        ENDELSE	

      ENDIF ELSE BEGIN
        print, 'PAS DE SUCCESSEUR -> FIN DU CYCLONE N:', long(indexcyc[t,i])
      ENDELSE
    ENDIF


  ENDIF ELSE BEGIN ; si aucun maximum detecte au temps t -> relaxation
    distcyc = reform(map_npoints(longuess, latguess, lon2D, lat2D))/1000.
    indcercle = where(distcyc LE distcrit, cntcercle)

;     localisation du maximum de vorticite
      indvorelax = where(abs(vort[indcercle]) GE vorelax, cntvorelax)
      IF cntvorelax GT 0 THEN BEGIN
        maxvorelax = max(abs(vort[indcercle[indvorelax]]), indmaxvorelax, /NAN)
        lonvorelax = lon2D[[indcercle[indvorelax[indmaxvorelax]]]]
        latvorelax = lat2D[[indcercle[indvorelax[indmaxvorelax]]]]
;        print, 'VOR:', lonvorelax[0], latvorelax[0], maxvorelax[0]
	; first guess
        rayon = reform(map_npoints(lonvorelax,latvorelax,lon2D,lat2D)/1000.)
        indcercle = where(rayon LE rayonsearch, cntcercle)
        maxuvrelax = max(uv10t[indcercle], indmaxuvrelax, /NAN)
        lonuvrelax = lon2D[indcercle[indmaxuvrelax]]
        latuvrelax = lat2D[indcercle[indmaxuvrelax]]
;        print, '1GUESS UV10:', lonuvrelax, latuvrelax, maxuvrelax
	; second guess
	rayon = reform(map_npoints(lonuvrelax,latuvrelax,lon2D,lat2D)/1000.)
        indcercle = where(rayon LE rayonsearch, cntcercle)
        maxuvrelax = max(uv10t[indcercle], indmaxuvrelax, /NAN)
        lonuvrelax = lon2D[indcercle[indmaxuvrelax]]
        latuvrelax = lat2D[indcercle[indmaxuvrelax]]
;        print, '2GUESS UV10:', lonuvrelax, latuvrelax, maxuvrelax
        IF mask[where(lon EQ lonvorelax[0]),where(lat EQ latvorelax[0])] EQ 0 THEN BEGIN
	  ; first guess
          minmslprelax = min(mslpt[indcercle], indmslprelax, /NAN)
          lonmslprelax = lon2D[indcercle[indmslprelax]]
          latmslprelax = lat2D[indcercle[indmslprelax]]
;          print, '1GUESS MSLP:', lonmslprelax, latmslprelax, minmslprelax
          ; second guess
	  rayon = reform(map_npoints(lonmslprelax,latmslprelax,lon2D,lat2D)/1000.)
          indcercle = where(rayon LE rayonsearch, cntcercle)
          minmslprelax = min(mslpt[indcercle], indmslprelax, /NAN)
          lonmslprelax = lon2D[indcercle[indmslprelax]]
          latmslprelax = lat2D[indcercle[indmslprelax]]
;          print, '2GUESS MSLP:', lonmslprelax, latmslprelax, minmslprelax

	  testdist = map_2points(lonmslprelax,latmslprelax,loncycn[indexcyc[t1,i],t1],latcycn[indexcyc[t1,i],t1],/METERS)/1000.
          print, 'DISTANCE POSITION PRECEDENTE:', testdist, '/', distcrit
	 
          IF testdist LE distcrit THEN BEGIN
            indexcyc[t,i] = indexcyc[t1,i]
            loncycn[indexcyc[t,i],t]  = lonmslprelax
            latcycn[indexcyc[t,i],t]  = latmslprelax
            mslpcycn[indexcyc[t,i],t] = minmslprelax
            uv10cycn[indexcyc[t,i],t] = maxuvrelax
	    print, loncycn[indexcyc[t,i],t], latcycn[indexcyc[t,i],t], mslpcycn[indexcyc[t,i],t], uv10cycn[indexcyc[t,i],t]
	  ENDIF ELSE BEGIN
            print, 'PAS DE SUCCESSEUR -> FIN DU CYCLONE N:', long(indexcyc[t,i])
          ENDELSE

        ENDIF ELSE BEGIN

          testdist = map_2points(lonvorelax,latvorelax,loncycn[indexcyc[t1,i],t1],latcycn[indexcyc[t1,i],t1],/METERS)/1000.
          print, 'DISTANCE POSITION PRECEDENTE:', testdist, '/', distcrit
	  
          IF testdist LE distcrit THEN BEGIN
            indexcyc[t,i] = indexcyc[t1,i]
            loncycn[indexcyc[t,i],t]  = lonvorelax
            latcycn[indexcyc[t,i],t]  = latvorelax
            mslpcycn[indexcyc[t,i],t] = !values.f_nan
            uv10cycn[indexcyc[t,i],t] = maxuvrelax
	    print, loncycn[indexcyc[t,i],t], latcycn[indexcyc[t,i],t], mslpcycn[indexcyc[t,i],t], uv10cycn[indexcyc[t,i],t]
	  ENDIF ELSE BEGIN
            print, 'PAS DE SUCCESSEUR -> FIN DU CYCLONE N:', long(indexcyc[t,i])
          ENDELSE

        ENDELSE

      ENDIF ELSE BEGIN
        print, 'PAS DE SUCCESSEUR -> FIN DU CYCLONE N:', long(indexcyc[t,i])
      ENDELSE

  ENDELSE ; si aucun maximum detecte au temps t -> relaxation
  ENDIF; guess a l'interieur du domaine

ENDFOR ; boucle sur les cyclones deja existants
ENDIF ; t > 1


; remise en forme de l'index cyclone
indo = where(finite(indexcyc[t,*]) EQ 1, cnto)
indk = where(finite(indexcyc[t,*]) EQ 0, cntk)
IF cnto GT 0 THEN indexcyc[t,*] = [[indexcyc[t,indo]],[indexcyc[t,indk]]]


; si aucun cyclone existant a t-1, c'est forcement un nouveau
indok = where(finite(loncyc[t,*]) EQ 1, cntok)

IF cntok GT 0 THEN BEGIN
  FOR i = 0, cntok-1 DO BEGIN
    indind = where(finite(indexcyc[t,*]) EQ 1, cntind)
    indexcyc[t,cntind] = nbtotalcyc
    loncycn[indexcyc[t,cntind],t]  = loncyc[t,indok[i]]
    latcycn[indexcyc[t,cntind],t]  = latcyc[t,indok[i]]
    mslpcycn[indexcyc[t,cntind],t] = mslpcyc[t,indok[i]]
    uv10cycn[indexcyc[t,cntind],t] = uv10cyc[t,indok[i]]
    vorcycn[indexcyc[t,cntind],t]  = vorcyc[t,indok[i]]
    anotcycn[indexcyc[t,cntind],t] = anomtcyc[t,indok[i]]
    rvmcycn[indexcyc[t,cntind],t]  = rvmcyc[t,indok[i]]
    nbtotalcyc = nbtotalcyc + 1
    print, '' & print, 'NOUVEAU CYCLONE N: ', long(indexcyc[t,cntind])
    print, loncycn[indexcyc[t,cntind],t], latcycn[indexcyc[t,cntind],t], $
           mslpcycn[indexcyc[t,cntind],t], uv10cycn[indexcyc[t,cntind],t]
  ENDFOR
ENDIF


ENDFOR; boucle temps



;-----------------------------------------------------------------------------------------
; RECONSTRUCTION PRE-TRACK
;-----------------------------------------------------------------------------------------
print, '----------------------------------------------------------------------------------'
print, '- RECONSTRUCTION PRE-TRACK -'
print, '----------------------------------------------------------------------------------'

rayonsearch = 100.
distcrit    = 400. ; 500km dans Chauvin et al. 2006


FOR i = 0, n_elements(loncycn[*,0])-1 DO BEGIN

  cntvorelax = 1
  WHILE cntvorelax GT 0 DO BEGIN

  indok  = where(finite(loncycn[i,*]) EQ 1, cntok)
  IF cntok GT 1 THEN BEGIN
; first guess par retro-advection sur 1 pas de temps
  longuess = loncycn[i,indok[0]] + (loncycn[i,indok[0]]-loncycn[i,indok[1]])
  latguess = latcycn[i,indok[0]] + (latcycn[i,indok[0]]-latcycn[i,indok[1]])
;  print, 'GUESS:', longuess, latguess
  IF longuess EQ loncycn[i,indok[0]] THEN BREAK
  IF latguess EQ latcycn[i,indok[0]] THEN BREAK
  IF longuess LT lon[0] OR longuess GT lon[n_elements(lon)-1] THEN BREAK
  IF latguess LT lat[0] OR latguess GT lat[n_elements(lat)-1] THEN BREAK

  rayon = reform(map_npoints(longuess, latguess, lon2D, lat2D))/1000.
  indcercle = where(rayon LE distcrit, cntcercle)

  t = indok[0]-1

  IF t GE 0 THEN BEGIN

; localisation du maximum de vorticite
  indvorelax = where(abs(vort[indcercle]) GE vorelax, cntvorelax)
  IF cntvorelax GT 0 THEN BEGIN
    maxvorelax = max(abs(vort[indcercle[indvorelax]]), indmaxvorelax, /NAN)
    lonvorelax = lon2D[[indcercle[indvorelax[indmaxvorelax]]]]
    latvorelax = lat2D[[indcercle[indvorelax[indmaxvorelax]]]]
;    print, 'VOR:', lonvorelax, latvorelax, maxvorelax*100000.

;   first guess
    rayon = reform(map_npoints(lonvorelax,latvorelax,lon2D,lat2D)/1000.)
    indcercle = where(rayon LE rayonsearch, cntcercle)
    maxuvrelax = max(uv10t[indcercle], indmaxuvrelax, /NAN)
    lonuvrelax = lon2D[indcercle[indmaxuvrelax]]
    latuvrelax = lat2D[indcercle[indmaxuvrelax]]
;    print, '1GUESS UV10:', lonuvrelax, latuvrelax, maxuvrelax
;   second guess
    rayon = reform(map_npoints(lonuvrelax,latuvrelax,lon2D,lat2D)/1000.)
    indcercle = where(rayon LE rayonsearch, cntcercle)
    maxuvrelax = max(uv10t[indcercle], indmaxuvrelax, /NAN)
    lonuvrelax = lon2D[indcercle[indmaxuvrelax]]
    latuvrelax = lat2D[indcercle[indmaxuvrelax]]
;    print, '2GUESS UV10:', lonuvrelax, latuvrelax, maxuvrelax

    IF mask[where(lon EQ lonvorelax[0]),where(lat EQ latvorelax[0])] EQ 0 THEN BEGIN
;     first guess
      minmslprelax = min(mslpt[indcercle], indmslprelax, /NAN)
      lonmslprelax = lon2D[indcercle[indmslprelax]]
      latmslprelax = lat2D[indcercle[indmslprelax]]
;      print, '1GUESS MSLP:', lonmslprelax, latmslprelax, minmslprelax
;     second guess
      rayon = reform(map_npoints(lonmslprelax,latmslprelax,lon2D,lat2D)/1000.)
      indcercle = where(rayon LE rayonsearch, cntcercle)
      minmslprelax = min(mslpt[indcercle], indmslprelax, /NAN)
      lonmslprelax = lon2D[indcercle[indmslprelax]]
      latmslprelax = lat2D[indcercle[indmslprelax]]
;      print, '2GUESS MSLP:', lonmslprelax, latmslprelax, minmslprelax

      testdist = map_2points(lonmslprelax,latmslprelax,loncycn[i,indok[0]],latcycn[i,indok[0]],/METERS)/1000.
      print, 'DISTANCE POSITION PRECEDENTE:', testdist, '/', distcrit
	 
      IF testdist LE distcrit THEN BEGIN
        loncycn[i,t]  = lonmslprelax
        latcycn[i,t]  = latmslprelax
        mslpcycn[i,t] = minmslprelax
        uv10cycn[i,t] = maxuvrelax
        rvmcycn[i,t] = map_2points(loncycn[i,t], latcycn[i,t], lonuvrelax, latuvrelax, /METERS) / 1000.
        print, loncycn[i,t], latcycn[i,t], mslpcycn[i,t], uv10cycn[i,t]
      ENDIF ELSE BEGIN
        print, 'PAS DE PREDECESSEUR -> FIN DU CYCLONE N:', long(i)
        cntvorelax = 0
      ENDELSE

    ENDIF ELSE BEGIN

      testdist = map_2points(lonmslprelax,latmslprelax,loncycn[i,indok[0]],latcycn[i,indok[0]],/METERS)/1000.
      print, 'DISTANCE POSITION PRECEDENTE:', testdist, '/', distcrit
	 
      IF testdist LE distcrit THEN BEGIN
        loncycn[i,t]  = lonvorelax
        latcycn[i,t]  = latvorelax
        mslpcycn[i,t] = !values.f_nan
        uv10cycn[i,t] = maxuvrelax
	print, loncycn[i,t], latcycn[i,t], mslpcycn[i,t], uv10cycn[i,t]
      ENDIF ELSE BEGIN
        print, 'PAS DE PREDECESSEUR -> FIN DU CYCLONE N:', long(i)
	cntvorelax = 0
      ENDELSE

    ENDELSE
      
  ENDIF

  ENDIF ELSE cntvorelax = 0

  ENDIF ELSE cntvorelax = 0 ; au moins 2 points par cyclone
  ENDWHILE ; cntvorelax > 0
ENDFOR ; boucle cyclone



;-----------------------------------------------------------------------------------------
; MISE EN FORME + NETTOYAGE
;-----------------------------------------------------------------------------------------

; on vire les colonnes vides
;indok = intarr(n_elements(loncycn[*,0])) + 0
;FOR i = 0, n_elements(loncycn[*,0])-1 DO BEGIN
;  indexist = where(finite(anotcycn[i,*]) EQ 1, cntexist)
;  IF cntexist GT 0 THEN indok[i] = 1
;ENDFOR
;test = where(indok EQ 1, cntok)
;IF cntok GT 0 THEN BEGIN
;  loncycn  = loncycn[where(indok EQ 1),*]
;  latcycn  = latcycn[where(indok EQ 1),*]
;  mslpcycn = mslpcycn[where(indok EQ 1),*]
;  uv10cycn = uv10cycn[where(indok EQ 1),*]
;  vorcycn  = vorcycn[where(indok EQ 1),*]
;  anotcycn = anotcycn[where(indok EQ 1),*]
;  rvmcycn  = rvmcycn[where(indok EQ 1),*]
;  datecycn = datecycn[where(indok EQ 1),*]
;  juldcycn = juldcycn[where(indok EQ 1),*]
;ENDIF

; on ne conserve que les trajectoires d'au moins 4 points respectant les criteres
indok = intarr(n_elements(loncycn[*,0])) + 0
FOR i = 0, n_elements(loncycn[*,0])-1 DO BEGIN
  indexist = where(finite(anotcycn[i,*]) EQ 1, cntexist)
  IF cntexist GE 4 THEN indok[i] = 1
ENDFOR
test = where(indok EQ 1, cntok)
IF cntok GT 0 THEN BEGIN
  loncycn  = temporary(loncycn[where(indok EQ 1),*])
  latcycn  = temporary(latcycn[where(indok EQ 1),*])
  mslpcycn = temporary(mslpcycn[where(indok EQ 1),*])
  uv10cycn = temporary(uv10cycn[where(indok EQ 1),*])
  vorcycn  = temporary(vorcycn[where(indok EQ 1),*])
  anotcycn = temporary(anotcycn[where(indok EQ 1),*])
  rvmcycn  = temporary(rvmcycn[where(indok EQ 1),*])
  datecycn = temporary(datecycn[where(indok EQ 1),*])
  juldcycn = temporary(juldcycn[where(indok EQ 1),*])
ENDIF

; creation des dates des positions des cyclones
FOR i = 0, n_elements(loncycn[*,0])-1 DO BEGIN
  indok = where(finite(loncycn[i,*]) EQ 1, cntok)
  IF cntok GT 0 THEN BEGIN
    datecycn[i,indok] = temporary(timedate[indok])
    juldcycn[i,indok] = temporary(timejuld[indok])
  ENDIF
ENDFOR

; nettoyage
FOR i = 0, n_elements(loncycn[*,0])-1 DO BEGIN
  indok = where(finite(loncycn[i,*]) EQ 1, cntok)
  IF cntok GT 0 THEN BEGIN
    loncycn[i,0:cntok-1]  = loncycn[i,indok]  & loncycn[i,cntok:199]  = !values.f_nan
    latcycn[i,0:cntok-1]  = latcycn[i,indok]  & latcycn[i,cntok:199]  = !values.f_nan
    mslpcycn[i,0:cntok-1] = mslpcycn[i,indok] & mslpcycn[i,cntok:199] = !values.f_nan
    uv10cycn[i,0:cntok-1] = uv10cycn[i,indok] & uv10cycn[i,cntok:199] = !values.f_nan
    vorcycn[i,0:cntok-1]  = vorcycn[i,indok]  & vorcycn[i,cntok:199]  = !values.f_nan
    anotcycn[i,0:cntok-1] = anotcycn[i,indok] & anotcycn[i,cntok:199] = !values.f_nan
    rvmcycn[i,0:cntok-1]  = rvmcycn[i,indok]  & rvmcycn[i,cntok:199]  = !values.f_nan
    datecycn[i,0:cntok-1] = datecycn[i,indok] & datecycn[i,cntok:199] = !values.f_nan
    juldcycn[i,0:cntok-1] = juldcycn[i,indok] & juldcycn[i,cntok:199] = !values.f_nan
  ENDIF 
ENDFOR

loncycn = loncycn[*,0:199]
latcycn = latcycn[*,0:199]
mslpcycn = mslpcycn[*,0:199]
uv10cycn = uv10cycn[*,0:199]
vorcycn = vorcycn[*,0:199]
anotcycn = anotcycn[*,0:199]
rvmcycn = rvmcycn[*,0:199]
datecycn = datecycn[*,0:199]
juldcycn = juldcycn[*,0:199]



;-----------------------------------------------------------------------------------------
; SAUVEGARDE FINALE
;-----------------------------------------------------------------------------------------
print, ''
print, '----------------------------------------------------------------------------------'
print, '- SAUVEGARDE FINALE -'
print, '----------------------------------------------------------------------------------'

save, loncycn, latcycn, mslpcycn, uv10cycn, datecycn, juldcycn, vorcycn, anotcycn, rvmcycn, /VERBOSE, $
filename = path_out +'tracker_'+temptype+'_nico_radius_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2) +'_'+ strtrim(year_ini,2) +'-'+ strtrim(year_end,2) +'.idl'


END
