PRO vire_tracks


;-----------------------------------------------------------------------------------------
; PARAMETRES
;-----------------------------------------------------------------------------------------

expname = 'FORCED_SW2_KF' & help, expname

; anciens criteres
ucrit    = 17.5    & help, ucrit
vorcrit  = 30.e-5  & help, vorcrit
vorelax  = 30.e-5  & help, vorelax
tempcrit = 1.      & help, tempcrit
temptype = 'treal' & help, temptype
dthour = 6 ; pas de temps du trackeur en heures
period = '1990-2009'

; nouveaux criteres
critu = 17.5 ; vent
critt_list = [1]  ; temperature
thour = 48   ; duree


;-----------------------------------------------------------------------------------------
; MASQUE (1 sur terre, 0 sur mer pour WRF) + GRILLE "T"
;-----------------------------------------------------------------------------------------

maskfile = '/usr/home/gslod/IDL/MASK_WRF_IND4_GRIDT.nc'
initncdf, maskfile, /fullcgrid
mask = ncdf_lec(maskfile, var = 'LANDMASK') & help, mask


;-----------------------------------------------------------------------------------------
; BOUCLE TEMPORELLE
;-----------------------------------------------------------------------------------------

FOR l = 0, n_elements(critt_list)-1 DO BEGIN
critt = critt_list[l] & help, critt


;-----------------------------------------------------------------------------------------
; BOUCLE CRITERES
;-----------------------------------------------------------------------------------------

path_data = '/net/adonis/usr/adonis/varclim/gslod/IDL/TRACKER/EXP_'+expname+'/DATA/'

file_in  = 'tracker_'+temptype+'_nico_radius_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2) +'_'+ period +'.idl'
file_out = 'tracker_light_'+temptype+'_nico_radius_u'+ strtrim(long(critu),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(critt*10),2) +'_r'+ strtrim(long(vorelax*100000),2) +'_'+ period +'.idl'

print, path_data + file_in
restore, filename = path_data + file_in, /VERBOSE
indok   = where(finite(loncycn[*,0]) EQ 1, cntok) & print, 'INI:',cntok


;-----------------------------------------------------------------------------------------
; recollage traj coupees
distcrit = 400. ; km
timecrit = 1. ; jours

FOR i = 0, n_elements(loncycn[*,0])-1 DO BEGIN
  indoki = where(finite(loncycn[i,*]) EQ 1, cntoki)
  IF cntoki GT 0 THEN BEGIN
    lonlast = loncycn[i, cntoki-1]
    latlast = latcycn[i, cntoki-1]
    jullast = juldcycn[i, cntoki-1]
    FOR j = i+1, n_elements(loncycn[*,0])-1 DO BEGIN
      indokj = where(finite(loncycn[j,*]) EQ 1, cntokj)
      IF cntokj GT 0 THEN BEGIN
        lonfirst = loncycn[j, 0]
        latfirst = latcycn[j, 0]
        julfirst = juldcycn[j, 0]
        IF abs(jullast-julfirst) LE timecrit THEN BEGIN
;          print, i, j & print, jullast, julfirst	  
          distcycn = map_2points(lonlast,latlast,lonfirst,latfirst, /meters) / 1000.
          IF distcycn LE distcrit THEN BEGIN
;            print, i, j & print, jullast, julfirst & print, lonlast,latlast,lonfirst,latfirst
            loncycn[i,cntoki:cntoki+cntokj-1]  = loncycn[j,0:cntokj-1]  & loncycn[j,*]  = !values.f_nan
            latcycn[i,cntoki:cntoki+cntokj-1]  = latcycn[j,0:cntokj-1]  & latcycn[j,*]  = !values.f_nan
            mslpcycn[i,cntoki:cntoki+cntokj-1] = mslpcycn[j,0:cntokj-1] & mslpcycn[j,*] = !values.f_nan
            uv10cycn[i,cntoki:cntoki+cntokj-1] = uv10cycn[j,0:cntokj-1] & uv10cycn[j,*] = !values.f_nan
            rvmcycn[i,cntoki:cntoki+cntokj-1]  = rvmcycn[j,0:cntokj-1]  & rvmcycn[j,*]  = !values.f_nan
            vorcycn[i,cntoki:cntoki+cntokj-1]  = vorcycn[j,0:cntokj-1]  & vorcycn[j,*]  = !values.f_nan	    
            anotcycn[i,cntoki:cntoki+cntokj-1] = anotcycn[j,0:cntokj-1] & anotcycn[j,*] = !values.f_nan
            datecycn[i,cntoki:cntoki+cntokj-1] = datecycn[j,0:cntokj-1] & datecycn[j,*] = !values.f_nan
            juldcycn[i,cntoki:cntoki+cntokj-1] = juldcycn[j,0:cntokj-1] & juldcycn[j,*] = !values.f_nan
          ENDIF
        ENDIF
      ENDIF
    ENDFOR
  ENDIF
ENDFOR

loncycn  = loncycn[indok,*]
latcycn  = latcycn[indok,*]
mslpcycn = mslpcycn[indok,*]
uv10cycn = uv10cycn[indok,*]
rvmcycn  = rvmcycn[indok,*]
anotcycn = anotcycn[indok,*]
vorcycn  = vorcycn[indok,*]
datecycn = datecycn[indok,*]
juldcycn = juldcycn[indok,*]
indok   = where(finite(loncycn[*,0]) EQ 1, cntok) & print, 'RECOL:',cntok


;-----------------------------------------------------------------------------------------
; seuil vent minimum
indok = intarr(n_elements(loncycn[*,0])) + 0
FOR i = 0, n_elements(loncycn[*,0])-1 DO BEGIN
  inducrit = where(uv10cycn[i,*] GE critu, cntucrit)
  IF cntucrit GT thour / dthour THEN indok[i] = 1
ENDFOR

loncycn  = loncycn[where(indok EQ 1),*]
latcycn  = latcycn[where(indok EQ 1),*]
mslpcycn = mslpcycn[where(indok EQ 1),*]
uv10cycn = uv10cycn[where(indok EQ 1),*]
rvmcycn  = rvmcycn[where(indok EQ 1),*]
anotcycn = anotcycn[where(indok EQ 1),*]
vorcycn  = vorcycn[where(indok EQ 1),*]
datecycn = datecycn[where(indok EQ 1),*]
juldcycn = juldcycn[where(indok EQ 1),*]
indok   = where(finite(loncycn[*,0]) EQ 1, cntok) & print, 'VENT:',cntok


;-----------------------------------------------------------------------------------------
; seuil temperature minimum
indok = intarr(n_elements(loncycn[*,0])) + 0
FOR i = 0, n_elements(loncycn[*,0])-1 DO BEGIN
  inducrit = where(anotcycn[i,*] GE critt, cnttempcrit)
  IF cnttempcrit GT thour / dthour THEN indok[i] = 1
ENDFOR

loncycn  = loncycn[where(indok EQ 1),*]
latcycn  = latcycn[where(indok EQ 1),*]
mslpcycn = mslpcycn[where(indok EQ 1),*]
uv10cycn = uv10cycn[where(indok EQ 1),*]
rvmcycn  = rvmcycn[where(indok EQ 1),*]
anotcycn = anotcycn[where(indok EQ 1),*]
vorcycn  = vorcycn[where(indok EQ 1),*]
datecycn = datecycn[where(indok EQ 1),*]
juldcycn = juldcycn[where(indok EQ 1),*]
indok   = where(finite(loncycn[*,0]) EQ 1, cntok) & print, 'TEMP:',cntok


;-----------------------------------------------------------------------------------------
; suppression des points en debut de trajectoire < seuil vent minimum
umin = 17.5 
FOR i = 0, n_elements(loncycn[*,0])-1 DO BEGIN
  indok = where(finite(loncycn[i,*]) EQ 1, cntok)
  WHILE uv10cycn[i,indok[0]] LT umin DO BEGIN
    loncycn[i,indok[0]:indok[cntok-2]]  = loncycn[i,indok[1]:indok[cntok-1]]  & loncycn[i,indok[cntok-1]]  = !values.f_nan
    latcycn[i,indok[0]:indok[cntok-2]]  = latcycn[i,indok[1]:indok[cntok-1]]  & latcycn[i,indok[cntok-1]]  = !values.f_nan
    mslpcycn[i,indok[0]:indok[cntok-2]] = mslpcycn[i,indok[1]:indok[cntok-1]] & mslpcycn[i,indok[cntok-1]] = !values.f_nan
    uv10cycn[i,indok[0]:indok[cntok-2]] = uv10cycn[i,indok[1]:indok[cntok-1]] & uv10cycn[i,indok[cntok-1]] = !values.f_nan
    rvmcycn[i,indok[0]:indok[cntok-2]]  = rvmcycn[i,indok[1]:indok[cntok-1]]  & rvmcycn[i,indok[cntok-1]]  = !values.f_nan
    anotcycn[i,indok[0]:indok[cntok-2]] = anotcycn[i,indok[1]:indok[cntok-1]] & anotcycn[i,indok[cntok-1]] = !values.f_nan
    vorcycn[i,indok[0]:indok[cntok-2]]  = vorcycn[i,indok[1]:indok[cntok-1]]  & vorcycn[i,indok[cntok-1]]  = !values.f_nan
    datecycn[i,indok[0]:indok[cntok-2]] = datecycn[i,indok[1]:indok[cntok-1]] & datecycn[i,indok[cntok-1]] = !values.f_nan
    juldcycn[i,indok[0]:indok[cntok-2]] = juldcycn[i,indok[1]:indok[cntok-1]] & juldcycn[i,indok[cntok-1]] = !values.f_nan
    indok = where(finite(loncycn[i,*]) EQ 1, cntok)  
  ENDWHILE
  WHILE uv10cycn[i,indok[cntok-1]] LT umin DO BEGIN
    loncycn[i,indok[cntok-1]]  = !values.f_nan
    latcycn[i,indok[cntok-1]]  = !values.f_nan
    mslpcycn[i,indok[cntok-1]] = !values.f_nan
    uv10cycn[i,indok[cntok-1]] = !values.f_nan
    rvmcycn[i,indok[cntok-1]]  = !values.f_nan
    anotcycn[i,indok[cntok-1]] = !values.f_nan
    vorcycn[i,indok[cntok-1]]  = !values.f_nan
    datecycn[i,indok[cntok-1]] = !values.f_nan
    juldcycn[i,indok[cntok-1]] = !values.f_nan
    indok = where(finite(loncycn[i,*]) EQ 1, cntok)    
  ENDWHILE
ENDFOR


;-----------------------------------------------------------------------------------------
; duree de vie totale
indok = intarr(n_elements(loncycn[*,0])) + 0
FOR i = 0, n_elements(loncycn[*,0])-1 DO BEGIN
  indpres = where(finite(uv10cycn[i,*]) EQ 1, cntpres)
  IF cntpres GT thour / dthour THEN indok[i] = 1
ENDFOR

loncycn  = loncycn[where(indok EQ 1),*]
latcycn  = latcycn[where(indok EQ 1),*]
mslpcycn = mslpcycn[where(indok EQ 1),*]
uv10cycn = uv10cycn[where(indok EQ 1),*]
rvmcycn  = rvmcycn[where(indok EQ 1),*]
anotcycn = anotcycn[where(indok EQ 1),*]
vorcycn  = vorcycn[where(indok EQ 1),*]
datecycn = datecycn[where(indok EQ 1),*]
juldcycn = juldcycn[where(indok EQ 1),*]
indok   = where(finite(loncycn[*,0]) EQ 1, cntok) & print, 'DUREE:',cntok


;-----------------------------------------------------------------------------------------
; regions
indok = intarr(n_elements(loncycn[*,0])) + 0
FOR i = 0, n_elements(loncycn[*,0])-1 DO BEGIN
  IF latcycn[i,0] GE -25. AND latcycn[i,0] LE  0. AND loncycn[i,0] LE 130. THEN indok[i] = 1 ; SIO
;  IF latcycn[i,0] GT  -5. AND latcycn[i,0] LE  0. AND loncycn[i,0] GT 105. THEN indok[i] = 0 ; INDONESIE
  IF latcycn[i,0] GE   0. AND latcycn[i,0] LE 23. AND loncycn[i,0] GE  60. AND loncycn[i,0] LE 100. THEN indok[i] = 1 ; NIO
  IF latcycn[i,0] GE  15. AND latcycn[i,0] LE 20. AND loncycn[i,0] GE  65. AND loncycn[i,0] LE 75.  THEN indok[i] = 0 ; ARABIAN SEA
ENDFOR

loncycn  = loncycn[where(indok EQ 1),*]
latcycn  = latcycn[where(indok EQ 1),*]
mslpcycn = mslpcycn[where(indok EQ 1),*]
uv10cycn = uv10cycn[where(indok EQ 1),*]
rvmcycn  = rvmcycn[where(indok EQ 1),*]
anotcycn = anotcycn[where(indok EQ 1),*]
vorcycn  = vorcycn[where(indok EQ 1),*]
datecycn = datecycn[where(indok EQ 1),*]
juldcycn = juldcycn[where(indok EQ 1),*]
indok   = where(finite(loncycn[*,0]) EQ 1, cntok) & print, 'REGIONS:',cntok


;-----------------------------------------------------------------------------------------
; calcul de vdep

vdepcycn = loncycn*0.
FOR i = 0, n_elements(loncycn[*,0])-1 DO BEGIN
  indok = where(finite(loncycn[i,*]) EQ 1, cntok)
  lon1 = reform(loncycn[i,indok])
  lon2 = reform(shift(loncycn[i,indok],-1)) & lon2[cntok-1] = lon2[cntok-2]
  lat1 = reform(latcycn[i,indok])
  lat2 = reform(shift(latcycn[i,indok],-1)) & lat2[cntok-1] = lat2[cntok-2]
  vdepcycn[i,indok] = map_npoints(lon1,lat1,lon2,lat2,/TWO_BY_TWO) / (6.*3600.)
ENDFOR

;  IF cntok GE 2 THEN BEGIN
;    FOR j = 1, cntok-2 DO BEGIN
;      vdepcycn[indok[j]] = map_2points(loncycn[i,indok[j]-1],latcycn[i,indok[j]-1],loncycn[i,indok[j]+1],latcycn[i,indok[j]+1], /METERS) / (2. * 6. * 3600.)
;    ENDFOR
;    vdepcycn[indok[0]] = vdepcycn[indok[1]]
;    vdepcycn[indok[cntok-1]] = vdepcycn[indok[cntok-2]]
 ; ENDIF
;ENDFOR


;-----------------------------------------------------------------------------------------
; duree consecutive a l'intensite ucrit

;indok = intarr(n_elements(loncycn[*,0])) + 0
;
;FOR i = 0, n_elements(loncycn[*,0])-1 DO BEGIN
;
;  indu   = where(uv10cycn[i,*] GE ucrit, cntu)
;
;  cpt = 1
;
;  FOR j = 0, cntu-2 DO BEGIN
;    IF indu[j+1] EQ indu[j] + 1 THEN cpt = cpt + 1 ELSE cpt = 1
;  ENDFOR
;
;  IF cpt GE thour / dthour THEN indok[i] = 1
;
;ENDFOR
;
;loncycn  = loncycn[where(indok EQ 1),*]
;latcycn  = latcycn[where(indok EQ 1),*]
;mslpcycn = mslpcycn[where(indok EQ 1),*]
;uv10cycn = uv10cycn[where(indok EQ 1),*]
;rvmcycn  = rvmcycn[where(indok EQ 1),*]
;anotcycn = anotcycn[where(indok EQ 1),*]
;vorcycn  = vorcycn[where(indok EQ 1),*]
;datecycn = datecycn[where(indok EQ 1),*]
;juldcycn = juldcycn[where(indok EQ 1),*]



;-----------------------------------------------------------------------------------------
;sauvegarde des resultats

print, path_data + file_out
save, loncycn, latcycn, mslpcycn, uv10cycn, rvmcycn, anotcycn, vorcycn, datecycn, juldcycn, vdepcycn, filename= path_data + file_out, /VERBOSE

;ENDFOR; mois
;ENDFOR; annee
ENDFOR; critt

END
