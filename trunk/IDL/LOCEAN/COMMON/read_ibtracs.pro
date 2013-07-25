
; repertoire + fichier
version = 'v03r02'
indir   = '/net/adonis/usr/adonis/varclim/gslod/IBTRACS/IBTRACS_'+version+'/'
file    = 'Allstorms.ibtracs_wmo.'+version+'.nc' & help, file

; variables 1D
storm_sn      = string(ncdf_lec(file, iodir = indir, var = 'storm_sn'))
name          = string(ncdf_lec(file, iodir = indir, var = 'name'))
num_obs       = ncdf_lec(file, iodir = indir, var = 'numObs')
season        = ncdf_lec(file, iodir = indir, var = 'season')
genesis_basin = ncdf_lec(file, iodir = indir, var = 'genesis_basin')
help, storm_sn, name, num_obs, season, genesis_basin

; variables 2D
time = ncdf_lec(file, iodir = indir, var = 'time_wmo')
lat  = ncdf_lec(file, iodir = indir, var = 'lat_wmo')*0.01
lon  = ncdf_lec(file, iodir = indir, var = 'lon_wmo')*0.01
windkt = ncdf_lec(file, iodir = indir, var = 'wind_wmo')*0.1
pres = ncdf_lec(file, iodir = indir, var = 'pres_wmo')*0.1
;time = ncdf_lec(file, iodir = indir, var = 'time')
;lat  = ncdf_lec(file, iodir = indir, var = 'lat')*0.01
;lon  = ncdf_lec(file, iodir = indir, var = 'lon')*0.01
;windkt = ncdf_lec(file, iodir = indir, var = 'wind')*0.1
;pres = ncdf_lec(file, iodir = indir, var = 'pres')*0.1
help, time, lon, lat, windkt, pres

; valeurs manquantes
bad       = where(time EQ 9.9692100e+36, cnt) & help, bad
IF cnt EQ 0 THEN stop
time      = time + julday(11,17,1858,00,00,00)
time[bad] = !values.f_nan
lat[bad]  = !values.f_nan
lon[bad]  = !values.f_nan
windkt[bad] = !values.f_nan
windms      = windkt * 0.5144  ; kt -> m/s
;IF windave EQ '1min' THEN BEGIN
;  windms = windms * 1./0.88 ; 10 min -> 1 min
;  windkt = windkt * 1./0.88 ; 10 min -> 1 min
;ENDIF
pres[bad] = !values.f_nan

bad = where(pres EQ -999., cnt) & help, bad
time[bad] = !values.f_nan
lat[bad]  = !values.f_nan
lon[bad]  = !values.f_nan
windkt[bad] = !values.f_nan
pres[bad] = !values.f_nan

bad = where(pres EQ 0., cnt) & help, bad
time[bad] = !values.f_nan
lat[bad]  = !values.f_nan
lon[bad]  = !values.f_nan
windkt[bad] = !values.f_nan
pres[bad] = !values.f_nan


; dates
caldat, time, month, day, year, hour
month[bad] = !values.f_nan
day[bad]   = !values.f_nan
year[bad]  = !values.f_nan
hour[bad]  = !values.f_nan
date = year*10000. + month*double(100.) + day + hour/24.
date[bad]  = !values.f_nan
help, year, month, day, hour, date

; coordonnees + dates cyclogeneses
startlat = reform(lat[0, *])
startlon = reform(lon[0, *])
startdate = reform(year[0, *])*10000 + reform(month[0, *])*double(100) + reform(day[0, *]) + reform(hour[0, *])/24.
help, startlat, startlon, startdate

; selection periode
IF n_elements(datebeg) EQ 0. THEN datebeg = startdate[n_elements(startdate)-1]
IF n_elements(dateend) EQ 0. THEN dateend = startdate[0]
indok = where(startdate GE datebeg AND startdate LE dateend, cntok)
;indok = where(year[0, *] GE firsty AND year[0, *] LE lasty, cntok)
latcycn = transpose(lat[*,indok]) & help, latcycn
loncycn = transpose(lon[*,indok]) & help, loncycn
monthcycn = transpose(month[*,indok]) & help, monthcycn
yearcycn = transpose(year[*,indok]) & help, yearcycn
uv10cycn = transpose(windms[*,indok]) & help, uv10cycn
mslpcycn = transpose(pres[*,indok]) & help, mslpcycn
namecycn = name[indok] & help, namecycn
datecycn = transpose(date[*,indok]) & help, date

; TS only
indts=intarr(n_elements(uv10cycn[*,0]))*0
FOR i = 0, n_elements(uv10cycn[*,0])-1 DO IF (where(uv10cycn[i,*] GE 17.22))[0] NE -1 THEN indts[i] = 1
indok = where(indts EQ 1)
latcycn = latcycn[indok,*] & help, latcycn
loncycn = loncycn[indok,*] & help, loncycn
monthcycn = monthcycn[indok,*] & help, monthcycn
yearcycn = yearcycn[indok,*] & help, yearcycn
uv10cycn = uv10cycn[indok,*] & help, uv10cycn
mslpcycn = mslpcycn[indok,*] & help, mslpcycn
namecycn = name[indok] & help, namecycn
datecycn = datecycn[indok,*] & help, datecycn

; calcul de vdep
;vdepcycn = loncycn*0. & help, vdepcycn 
;FOR i = 0, n_elements(loncycn[*,0])-1 DO BEGIN
;  indok = where(finite(loncycn[i,*]) EQ 1, cntok)
;  lon1 = reform(loncycn[i,indok])
;  lon2 = reform(shift(loncycn[i,indok],-1)) & lon2[cntok-1] = lon2[cntok-2]
;  lat1 = reform(latcycn[i,indok])
;  lat2 = reform(shift(latcycn[i,indok],-1)) & lat2[cntok-1] = lat2[cntok-2]
;  vdepcycn[i,indok] = map_npoints(lon1,lat1,lon2,lat2,/TWO_BY_TWO) / (6.*3600.)
;ENDFOR
