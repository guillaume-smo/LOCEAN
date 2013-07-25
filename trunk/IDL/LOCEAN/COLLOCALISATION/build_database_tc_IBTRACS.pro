PRO  build_database_tc_ibtracs
; build database from ibtracs to store wind speed, pressure, WPi,location....
@common
 

; parametres
  expname = 'IBTRACS'   & help, expname
  version = 'v03r03'    & help, version
  basin   = 'IO'
  minlon  =  30.
  maxlon  = 130.
  minlat  = -30.
  maxlat  =  30.
  datedeb = 19980101.00d
  datefin = 20100101.00d
  period  = '1998-2009'
  indir   = '/net/adonis/usr/adonis/varclim/gslod/IBTRACS/WMO/' & help, indir
  oudir   = '/net/adonis/usr/adonis/varclim/gslod/IDL/TRACKER/EXP_'+ expname +'/DATA/' & help, oudir
  file    = 'Allstorms.ibtracs_wmo.'+version+'.nc' & help, file
  nbtime  = 137 ; max number of time steps per tc
  interpol_3h = 0


; lecture data + missing value
  juld          = ncdf_lec(file, iodir=indir, var='time_wmo') + date2jul(18581117.00d) & help, juld
  lat           = ncdf_lec(file, iodir=indir, var='lat_wmo')  * 0.01 & help, lat
  lon           = ncdf_lec(file, iodir=indir, var='lon_wmo')  * 0.01 & help, lon
  wind_kt       = ncdf_lec(file, iodir=indir, var='wind_wmo') * 0.1 & help, wind_kt
  pres          = ncdf_lec(file, iodir=indir, var='pres_wmo') * 0.1 & help, pres
  ibad_time     = where(juld GE 9.9692100e+36) & help, ibad_time
  ibad_wind     = where(wind_kt LE -3276.70) & help, ibad_wind
  ibad_pres     = where(pres LE 3276.70) & help, ibad_pres
  ibad_lon      = where(lon LE -327.669) & help, ibad_lon
  ibad_lat      = where(lat LE -327.669) & help, ibad_lat
  ibad          = ibad_wind & help, ibad
  juld[ibad]    = !values.f_nan
  wind_kt[ibad] = !values.f_nan
  pres[ibad]    = !values.f_nan
  lon[ibad]     = !values.f_nan
  lat[ibad]     = !values.f_nan
  wind          = wind_kt * 0.5144
  date          = jul2date(juld, hour=hour, day=day, month=month, year=year)


; selection data
  iok1 = where(lon[0,*] GE minlon AND lon[0,*] LE maxlon AND lat[0,*] GE minlat AND lat[0,*] LE 0.)
  iok2 = where(lon[0,*] GE minlon AND lon[0,*] LE 100.   AND lat[0,*] GE 0. AND lat[0,*] LE maxlat)
  iok3 = [iok1,iok2]
  iok4 = where(date[0,*] GE datedeb AND date[0,*] LT datefin)
  iok  = intersect(iok3,iok4)
  cptok = n_elements(iok)
  help, iok1, iok2, iok3, iok4, iok & STOP


; mise en forme data + calcul rmw analytique (willoughby)
  d1_wind = transpose(wind[*,iok]) & help, d1_wind
  d1_juld = transpose(juld[*,iok]) & help, d1_juld
  d1_date = transpose(date[*,iok]) & help, d1_date
  d1_lon  = transpose( lon[*,iok]) & help, d1_lon
  d1_lat  = transpose( lat[*,iok]) & help, d1_lat
  d1_pres = transpose(pres[*,iok]) & help, d1_pres
  d1_rmw_w = 46.4 * exp(-0.0155*d1_wind + 0.0169*abs(d1_lat)) & help, d1_rmw_w
  d1_rmw_h = 51.6 * exp(-0.0223*d1_wind + 0.0281*abs(d1_lat)) & help, d1_rmw_h
  d1_dir  = fltarr(n_elements(d1_pres[*,0]),n_elements(d1_pres[0,*])) + !values.f_nan & help, d1_dir
;  d1_nrj_v2_h = fltarr(n_elements(d1_pres[*,0]),n_elements(d1_pres[0,*])) + !values.f_nan & help, d1_nrj_v2_h
;  d1_nrj_v2_w = fltarr(n_elements(d1_pres[*,0]),n_elements(d1_pres[0,*])) + !values.f_nan & help, d1_nrj_v2_w
;  d1_nrj_v3_h = fltarr(n_elements(d1_pres[*,0]),n_elements(d1_pres[0,*])) + !values.f_nan & help, d1_nrj_v3_h
;  d1_nrj_v3_w = fltarr(n_elements(d1_pres[*,0]),n_elements(d1_pres[0,*])) + !values.f_nan & help, d1_nrj_v3_w


  ; calcul de langle de deplacement du cyclone
  FOR i = 0, n_elements(d1_pres[*,0])-1 DO BEGIN
    jok = where(finite(d1_pres[i,*]), cntok)
    FOR j = 0, cntok-2 DO BEGIN
      lon0 = d1_lon[i,j] & lonp = d1_lon[i,max([j-1,0])] & lonn = d1_lon[i,min([j+1,cntok-1])]
      lat0 = d1_lat[i,j] & latp = d1_lat[i,max([j-1,0])] & latn = d1_lat[i,min([j+1,cntok-1])]
      distx = map_2points(lonp,lat0,lonn,lat0,/meters) / 1000. * sign(lonn-lonp)
      disty = map_2points(lon0,latp,lon0,latn,/meters) / 1000. * sign(latn-latp)
      d1_dir[i,j] = atan(disty,distx)
    ENDFOR
    d1_dir[i,cntok-1] = d1_dir[i,cntok-2]
  ENDFOR


  ; calcul WPI
  FOR i = 0, n_elements(d1_pres[*,0])-1 DO BEGIN
    jok = where(finite(d1_pres[i,*]), cntok)
    FOR j = 0, cntok-2 DO BEGIN
      lon0 = d1_lon[i,j] & lonp = d1_lon[i,max([j-1,0])] & lonn = d1_lon[i,min([j+1,cntok-1])]
      lat0 = d1_lat[i,j] & latp = d1_lat[i,max([j-1,0])] & latn = d1_lat[i,min([j+1,cntok-1])]
      distx = map_2points(lonp,lat0,lonn,lat0,/meters) / 1000. * sign(lonn-lonp)
      disty = map_2points(lon0,latp,lon0,latn,/meters) / 1000. * sign(latn-latp)
      d1_dir[i,j] = atan(disty,distx)
    ENDFOR
    d1_dir[i,cntok-1] = d1_dir[i,cntok-2]
  ENDFOR

  print, '% SAVE: ', oudir+'d1_'+expname+version+'_'+basin+'_'+period+'.dat'
  SAVE, d1_wind, d1_juld, d1_date, d1_lon, d1_lat, d1_pres, d1_rmw_w, d1_rmw_h, d1_dir, $
  FILENAME = oudir+'d1_'+expname+version+'_'+basin+'_'+period+'.dat', /VERBOSE


; interpolation temporelle 6h -> 3h
  IF interpol_3h THEN BEGIN
    ntc  = n_elements(d1_juld[*,0])
    nloc = n_elements(d1_juld[0,*])
    d1_juld_3h = dblarr(ntc,2*nloc) + !values.f_nan
    d1_lon_3h  = fltarr(ntc,2*nloc) + !values.f_nan
    d1_lat_3h  = fltarr(ntc,2*nloc) + !values.f_nan
    d1_rmw_3h  = fltarr(ntc,2*nloc) + !values.f_nan
    d1_pres_3h = fltarr(ntc,2*nloc) + !values.f_nan
    d1_wind_3h = fltarr(ntc,2*nloc) + !values.f_nan
    d1_dir_3h  = fltarr(ntc,2*nloc) + !values.f_nan

    FOR i = 0, ntc-1 DO  BEGIN
     iok  = where(finite(d1_juld[i,*]) EQ 1, cntok)
     ibad = where(fix((d1_juld[i,iok]-floor(d1_juld[i,iok]))*1000.) mod 125 NE 0)
     IF ibad[0] NE -1 THEN CONTINUE
;     IF ibad[0] NE -1 THEN BEGIN & print, i & help, ibad & ENDIF
;     IF ibad[0] NE -1 THEN d1_juld[i,iok[ibad]] = d1_juld[i,iok[ibad]]-0.0416666666666
     xin = reform(d1_juld[i,iok]-d1_juld[i,iok[0]])
     xou = findgen((d1_juld[i,iok[cntok-1]]-d1_juld[i,iok[0]])/0.125+1) * 0.125
     nou = n_elements(xou) & nin = n_elements(xin)
     IF ibad[0] NE -1 THEN print, nin, nou
     d1_juld_3h[i,0:nou-1] = interpol(reform(d1_juld[i,iok]),xin,xou)
     d1_lon_3h[i,0:nou-1]  = interpol(reform(d1_lon[i,iok]),xin,xou)
     d1_lat_3h[i,0:nou-1]  = interpol(reform(d1_lat[i,iok]),xin,xou)
     d1_dir_3h[i,0:nou-1]  = interpol(reform(d1_dir[i,iok]),xin,xou)
     d1_pres_3h[i,0:nou-1] = interpol(reform(d1_pres[i,iok]),xin,xou)
     d1_rmw_3h[i,0:nou-1]  = interpol(reform(d1_rmw[i,iok]),xin,xou)
     d1_wind_3h[i,0:nou-1] = interpol(reform(d1_wind[i,iok]),xin,xou)
     IF ibad[0] NE -1 THEN print, d1_juld[i,iok[0]], d1_juld_3h[i,0], d1_juld[i,iok[cntok-1]], d1_juld_3h[i,nou-1], format='(4(F12.4,1X))'
    ENDFOR

    SAVE, d1_wind_3h, d1_juld_3h, d1_lon_3h, d1_lat_3h, d1_pres_3h, d1_rmw_3h, d1_dir_3h, $
    FILENAME = oudir+'d1_'+expname+'_'+basin+'_'+period+'_3H_NOBADTIME.dat', /VERBOSE

  ENDIF

STOP
END
