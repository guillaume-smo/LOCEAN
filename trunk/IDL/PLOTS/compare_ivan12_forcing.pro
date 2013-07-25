PRO compare_forcing
@all_cm


; IBTRACS parameters
ibtracs_wmo = '/home/gsamson/WORK/IBTRACS/v03r03-WMO/'
;ibtracs_all = '/home/gsamson/WORK/IBTRACS/v03r03-ALL/'
date_ini    = 18581117.00d
tc_name     = 'IVAN'

; read data
name_wmo = string(ncdf_lec('Basin.SI.ibtracs_wmo.v03r03.nc', iodir = ibtracs_wmo, var = 'name'))
time_wmo = ncdf_lec('Basin.SI.ibtracs_wmo.v03r03.nc', iodir = ibtracs_wmo, var = 'time_wmo')
lat_wmo  = ncdf_lec('Basin.SI.ibtracs_wmo.v03r03.nc', iodir = ibtracs_wmo, var = 'lat_wmo')*0.01
lon_wmo  = ncdf_lec('Basin.SI.ibtracs_wmo.v03r03.nc', iodir = ibtracs_wmo, var = 'lon_wmo')*0.01
maxuv10_wmo = ncdf_lec('Basin.SI.ibtracs_wmo.v03r03.nc', iodir = ibtracs_wmo, var = 'wind_wmo')*0.1*0.5144
mslp_wmo = ncdf_lec('Basin.SI.ibtracs_wmo.v03r03.nc', iodir = ibtracs_wmo, var = 'pres_wmo')*0.1
ind_wmo  = where(name_wmo EQ tc_name)
help, name_wmo, time_wmo, lat_wmo, lon_wmo, maxuv10_wmo, mslp_wmo

; missing value -> NaN
name_wmo = name_wmo[ind_wmo]
time_wmo = time_wmo[*,ind_wmo] & time_wmo[where(time_wmo GE 9.9e+36)] = !VALUES.F_NAN
juld_wmo = date2jul(date_ini) + time_wmo
date_wmo = jul2date(juld_wmo)
lat_wmo  = lat_wmo[*,ind_wmo]  & lat_wmo[where(lat_wmo LE -327.6)] = !VALUES.F_NAN
lon_wmo  = lon_wmo[*,ind_wmo]  & lon_wmo[where(lon_wmo LE -327.6)] = !VALUES.F_NAN
maxuv10_wmo = maxuv10_wmo[*,ind_wmo] & maxuv10_wmo[where(maxuv10_wmo LE -3276.7 OR maxuv10_wmo EQ 0.)] = !VALUES.F_NAN
mslp_wmo = mslp_wmo[*,ind_wmo] & mslp_wmo[where(mslp_wmo LE -3276.7 OR mslp_wmo EQ 0.)] = !VALUES.F_NAN
help, name_wmo, time_wmo, lat_wmo, lon_wmo, maxuv10_wmo, mslp_wmo

; remove nan at the end of vectors
iok_wmo = where(finite(date_wmo), cntok) & help, iok_wmo
time_wmo = time_wmo[0:iok_wmo[cntok-1]]
juld_wmo = juld_wmo[0:iok_wmo[cntok-1]]
date_wmo = date_wmo[0:iok_wmo[cntok-1]]
lat_wmo  = lat_wmo[0:iok_wmo[cntok-1]]
lon_wmo  = lon_wmo[0:iok_wmo[cntok-1]]
maxuv10_wmo = maxuv10_wmo[0:iok_wmo[cntok-1]]
mslp_wmo = mslp_wmo[0:iok_wmo[cntok-1]]
datedeb_wmo = date_wmo[0] & help, datedeb_wmo
datefin_wmo = date_wmo[iok_wmo[cntok-1]] & help, datefin_wmo
help, name_wmo, time_wmo, lat_wmo, lon_wmo, maxuv10_wmo, mslp_wmo

; define tc domain from ibtracs data
lon_min = min(lon_wmo, /nan) - 1.
lon_max = max(lon_wmo, /nan) + 1.
lat_min = min(lat_wmo, /nan) - 1
lat_max = max(lat_wmo, /nan) + 1.
help, lon_min, lon_max, lat_min, lat_max
ivandom = [lon_min,lon_max,lat_min,lat_max]

;source   = string(ncdf_lec('Basin.SI.ibtracs_all.v03r03.nc', iodir = ibtracs_all, var = 'source'))
;name_all = string(ncdf_lec('Basin.SI.ibtracs_all.v03r03.nc', iodir = ibtracs_all, var = 'name'))
;time_all = ncdf_lec('Basin.SI.ibtracs_all.v03r03.nc', iodir = ibtracs_all, var = 'source_time')
;lat_all  = ncdf_lec('Basin.SI.ibtracs_all.v03r03.nc', iodir = ibtracs_all, var = 'source_lat')*0.01
;lon_all  = ncdf_lec('Basin.SI.ibtracs_all.v03r03.nc', iodir = ibtracs_all, var = 'source_lon')*0.01
;wind_all = ncdf_lec('Basin.SI.ibtracs_all.v03r03.nc', iodir = ibtracs_all, var = 'source_wind')*0.1*0.5144
;pres_all = ncdf_lec('Basin.SI.ibtracs_all.v03r03.nc', iodir = ibtracs_all, var = 'source_pres')*0.1
;ind_all  = where(name_all EQ 'IVAN')
;ind_source = where(source EQ 'reunion')
;help, name_all, time_all, lat_all, lon_all, wind_all, pres_all

;name_all = name_all[ind_all]
;time_all = time_all[*,ind_all]                    & time_all[where(time_all LE -99999.)] = !VALUES.F_NAN
;juld_all = date2jul(date_ini) + time_all
;date_all = jul2date(juld_all)
;lat_all  = reform(lat_all[ind_source,*,ind_all])  & lat_all[where(lat_all LE -300.)] = !VALUES.F_NAN
;lon_all  = reform(lon_all[ind_source,*,ind_all])  & lon_all[where(lon_all LE -300.)] = !VALUES.F_NAN
;wind_all = reform(wind_all[ind_source,*,ind_all]) & wind_all[where(wind_all LE -999.)] = !VALUES.F_NAN
;pres_all = reform(pres_all[ind_source,*,ind_all]) & pres_all[where(pres_all LE -999.)] = !VALUES.F_NAN
;help, name_all, time_all, lat_all, lon_all, wind_all, pres_all


; Mask ORCA 0.25°
file_mask = '/home/gsamson/WORK/ORCA025_bathy_etopo1_gebco1_smoothed_coast_corrected_sept09.nc'
initncdf, file_mask
domdef, ivandom
bathy = read_ncdf('Bathymetry', filename=file_mask, /nostruct) & help, bathy
mask_ifs = bathy * !VALUES.F_NAN & mask_ifs[where(bathy NE 0.)] = 1. & help, mask_ifs


; IFS-ECMWF
file_ifs = '/data/rd_exchange/gsamson/INPUT_IVAN12/FORCING_ECMWF/ECMWF_PRES_ORCA025_200802.nc'
initncdf, file_ifs
;domdef, ivandom
dateini_ifs = 20080201.00d & help, dateini_ifs
juldini_ifs = date2jul(dateini_ifs)
pres_ifs = read_ncdf('somslpre', filename=file_ifs, /allrecords, /nostruct) & help, pres_ifs
lon_ifs  = read_ncdf('nav_lon', filename=file_ifs, /allrecords, /nostruct) & help, lon_ifs
lat_ifs  = read_ncdf('nav_lat', filename=file_ifs, /allrecords, /nostruct) & help, lat_ifs
file1 = '/data/rd_exchange/gsamson/INPUT_IVAN12/FORCING_ECMWF/ECMWF_BULKU10M_ORCA025_200802.nc'
file2 = '/data/rd_exchange/gsamson/INPUT_IVAN12/FORCING_ECMWF/ECMWF_BULKV10M_ORCA025_200802.nc'
u10   = read_ncdf('sowinu10', filename=file1, /allrecords, /nostruct)
v10   = read_ncdf('sowinv10', filename=file2, /allrecords, /nostruct)
uv10_ifs  = sqrt(u10^2+v10^2) & help, uv10_ifs
nbdt_ifs = n_elements(pres_ifs[0,0,*])
juld_ifs = juldini_ifs + dindgen(nbdt_ifs)/8.
date_ifs = jul2date(juld_ifs, hour=hour, day=day, month=month, year=year)
help, date_ifs[0], date_ifs[nbdt_ifs-1]
stop

; select time interval
ibeg = (where(date_ifs EQ datedeb_wmo))[0] & help, ibeg
iend = (where(date_ifs EQ datefin_wmo))[0] & help, iend
pres_ifs = pres_ifs[*,*,ibeg:iend] & help, pres_ifs
uv10_ifs = uv10_ifs[*,*,ibeg:iend] & help, uv10_ifs
nbdt_ifs = n_elements(pres_ifs[0,0,*])
juld_ifs = juld_ifs[ibeg:iend] & help, juld_ifs
date_ifs = date_ifs[ibeg:iend] & help, date_ifs
help, date_ifs[0], date_ifs[nbdt_ifs-1]

; 3h interpolation wmo -> ifs
juld_wmo_3h = interpol(juld_wmo,juld_wmo,juld_ifs) & help, juld_wmo_3h
date_wmo_3h = jul2date(juld_wmo_3h) & help, date_wmo_3h
lon_wmo_3h = interpol(lon_wmo,juld_wmo,juld_ifs) & help, lon_wmo_3h
lat_wmo_3h = interpol(lat_wmo,juld_wmo,juld_ifs) & help, lat_wmo_3h
maxuv10_wmo_3h = interpol(maxuv10_wmo,juld_wmo,juld_ifs)
mslp_wmo_3h = interpol(mslp_wmo,juld_wmo,juld_ifs)

; tracking mslp+vent max
mslp_ifs = fltarr(nbdt_ifs)
lon_mslp_ifs = fltarr(nbdt_ifs)
lat_mslp_ifs = fltarr(nbdt_ifs)
maxuv10_ifs = fltarr(nbdt_ifs)
lon_maxuv10_ifs = fltarr(nbdt_ifs)
lat_maxuv10_ifs = fltarr(nbdt_ifs)
FOR i = 0, nbdt_ifs-1 DO BEGIN
  dist_tc = reform(map_npoints(lon_wmo_3h[i],lat_wmo_3h[i],glamt[firstxt:lastxt,firstyt:lastyt],gphit[firstxt:lastxt,firstyt:lastyt])) / 1000.
  area  = where(dist_tc LE 500.); & help, area
  ij_ok = array_indices(glamt[firstxt:lastxt,firstyt:lastyt],area)
  i_min = min(ij_ok[0,*],/nan)
  i_max = max(ij_ok[0,*],/nan)
  j_min = min(ij_ok[1,*],/nan)
  j_max = max(ij_ok[1,*],/nan)
;  help, i_min, i_max, j_min, j_max
;  mslp_ifs[i] = min(pres_ifs[*,*,i]*mask_ifs/100., /nan)
;  mslp_ifs[i] = min(pres_ifs[*,*,i]/100., /nan)
  mslp_ifs[i] = min(pres_ifs[i_min:i_max,j_min:j_max,i]/100., /nan)
  lon_mslp_ifs[i] = lon_ifs[where(pres_ifs[*,*,i]/100. EQ mslp_ifs[i])]
  lat_mslp_ifs[i] = lat_ifs[where(pres_ifs[*,*,i]/100. EQ mslp_ifs[i])]
;  maxuv10_ifs[i] = max(uv10_ifs[*,*,i]*mask_ifs, /nan)
;  maxuv10_ifs[i] = max(uv10_ifs[*,*,i], /nan)
  maxuv10_ifs[i] = max(uv10_ifs[i_min:i_max,j_min:j_max,i], /nan)
  lon_maxuv10_ifs = lon_ifs[where(uv10_ifs[i_min:i_max,j_min:j_max,i] EQ maxuv10_ifs[i])]
  lat_maxuv10_ifs = lat_ifs[where(uv10_ifs[i_min:i_max,j_min:j_max,i] EQ maxuv10_ifs[i])]
ENDFOR


; ALADIN
;path_ala = '/home/gsamson/WORK/ALADIN/'
path_ala = '/data/rd_exchange/gsamson/FORCING_ALADIN/'
file_ala = 'BOURBON01+0006_200802.nc'
initncdf, path_ala+file_ala
domdef, ivandom
IF file_ala EQ 'BOURBON01+0000_200802.nc' THEN dateini_ala = 20080201.00d 
IF file_ala EQ 'BOURBON01+0006_200802.nc' THEN dateini_ala = 20080201.25d 
help, dateini_ala
juldini_ala = date2jul(dateini_ala)
pres_ala = read_ncdf('somslpre', filename=path_ala+file_ala, /allrecords, /nostruct) & help, pres_ala
nbdt_ala = n_elements(pres_ala[0,0,*])
juld_ala = juldini_ala + dindgen(nbdt_ala)/4. & help, juld_ala
date_ala = jul2date(juld_ala, hour=hour, day=day, month=month, year=year)
lon_ala  = glamt[firstxt:lastxt,firstyt:lastyt]
lat_ala  = gphit[firstxt:lastxt,firstyt:lastyt]
mask_ala = fromreg('bilinear',mask_ifs,lon_ifs,lat_ifs,lon_ala,lat_ala) & help, mask_ala
u10   = read_ncdf('sowinu10', filename=path_ala+file_ala, /allrecords, /nostruct)
v10   = read_ncdf('sowinv10', filename=path_ala+file_ala, /allrecords, /nostruct)
uv10_ala  = reform(sqrt(u10^2+v10^2)) & help, uv10_ala

; select time interval
ibeg = (where(date_ala EQ datedeb_wmo))[0] & help, ibeg
iend = (where(date_ala EQ datefin_wmo))[0] & help, iend
pres_ala = pres_ala[*,*,ibeg:iend] & help, pres_ala
uv10_ala = uv10_ala[*,*,ibeg:iend] & help, uv10_ala
nbdt_ala = n_elements(pres_ala[0,0,*])
juld_ala = juld_ala[ibeg:iend] & help, juld_ala
date_ala = date_ala[ibeg:iend] & help, date_ala

; tracking mslp+vent max
mslp_ala = fltarr(nbdt_ala)
lon_mslp_ala = fltarr(nbdt_ala)
lat_mslp_ala = fltarr(nbdt_ala)
maxuv10_ala = fltarr(nbdt_ala)
lon_maxuv10_ala = fltarr(nbdt_ala)
lat_maxuv10_ala = fltarr(nbdt_ala)
FOR i = 0, nbdt_ala-1 DO BEGIN
  dist_tc = reform(map_npoints(lon_wmo[i],lat_wmo[i],glamt[firstxt:lastxt,firstyt:lastyt],gphit[firstxt:lastxt,firstyt:lastyt])) / 1000.
  area  = where(dist_tc LE 500.); & help, area
  ij_ok = array_indices(glamt[firstxt:lastxt,firstyt:lastyt],area)
  i_min = min(ij_ok[0,*],/nan)
  i_max = max(ij_ok[0,*],/nan)
  j_min = min(ij_ok[1,*],/nan)
  j_max = max(ij_ok[1,*],/nan)
;  help, i_min, i_max, j_min, j_max
;  mslp_ala[i] = min(pres_ala[*,*,i]*mask_ala/100., /nan)
;  mslp_ala[i] = min(pres_ala[*,*,i]/100., /nan)
  mslp_ala[i] = min(pres_ala[i_min:i_max,j_min:j_max,i]/100., /nan)
;  lon_mslp_ala[i] = lon_ala[where(pres_ala[*,*,i]/100. EQ mslp_ala[i])]
;  lat_mslp_ala[i] = lat_ala[where(pres_ala[*,*,i]/100. EQ mslp_ala[i])]
  tmp1 = lon_ala[where(pres_ala[*,*,i]/100. EQ mslp_ala[i])]
  tmp2 = lat_ala[where(pres_ala[*,*,i]/100. EQ mslp_ala[i])]
  tmp3 = min(map_npoints(tmp1,tmp2,lon_wmo[i],lat_wmo[i]), indok)
  lon_mslp_ala[i] = tmp1[indok]
  lat_mslp_ala[i] = tmp2[indok]
;  maxuv10_ala[i] = max(uv10_ala[*,*,i]*mask_ala, /nan)
;  maxuv10_ala[i] = max(uv10_ala[*,*,i], /nan)
  maxuv10_ala[i] = max(uv10_ala[i_min:i_max,j_min:j_max,i], /nan)
  lon_maxuv10_ala = lon_ala[where(uv10_ala[*,*,i] EQ maxuv10_ala[i])]
  lat_maxuv10_ala = lat_ala[where(uv10_ala[*,*,i] EQ maxuv10_ala[i])]
ENDFOR


; DIAGS
inonan_mslp = where(finite(mslp_wmo) EQ 1, cntnonan)
inan_mslp   = where(finite(mslp_wmo) EQ 0, cntnan)
inan_3h_mslp= where(finite(mslp_wmo_3h) EQ 0, cntnan)

FOR i = 0, cntnonan-1 DO BEGIN
  iok1 = where(date_ifs EQ date_wmo[inonan_mslp[i]])
  IF i EQ 0 AND iok1 NE -1 THEN iok_ifs = iok1
  IF i NE 0 AND iok1 NE -1 THEN iok_ifs = [iok_ifs,iok1]
  iok2 = where(date_ala EQ date_wmo[inonan_mslp[i]])
  IF i EQ 0 AND iok2 NE -1 THEN iok_ala = iok2
  IF i NE 0 AND iok2 NE -1 THEN iok_ala = [iok_ala,iok2]
ENDFOR
help, iok_ifs, iok_ala

cor_wmo_ifs_mslp = round(correlate(mslp_wmo[inonan_mslp],mslp_ifs[iok_ifs])*100.)/100.
cor_wmo_ala_mslp = round(correlate(mslp_wmo[inonan_mslp],mslp_ala[iok_ala])*100.)/100.
std_wmo_ifs_mslp = round(stddev(mslp_ifs[iok_ifs]-mslp_wmo[inonan_mslp])*100.)/100.
std_wmo_ala_mslp = round(stddev(mslp_ala[iok_ala]-mslp_wmo[inonan_mslp])*100.)/100.
;dist_wmo_ifs_v1 = map_npoints(lon_wmo[inonan_mslp],lat_wmo[inonan_mslp],lon_mslp_ifs[iok_ifs],lat_mslp_ifs[iok_ifs], /two_by_two) / 1000.
;dist_wmo_ala_v1 = map_npoints(lon_wmo[inonan_mslp],lat_wmo[inonan_mslp],lon_mslp_ala[iok_ala],lat_mslp_ala[iok_ala], /two_by_two) / 1000.
dist_wmo_ifs_v2 = map_npoints(lon_wmo_3h,lat_wmo_3h,lon_mslp_ifs,lat_mslp_ifs, /two_by_two) / 1000.
dist_wmo_ala_v2 = map_npoints(lon_wmo,lat_wmo,lon_mslp_ala,lat_mslp_ala, /two_by_two) / 1000.
dist_wmo_ifs_noland = dist_wmo_ifs_v2
dist_wmo_ala_noland = dist_wmo_ala_v2
dist_wmo_ifs_noland[inan_3h_mslp] = !VALUES.F_NAN
dist_wmo_ala_noland[inan_mslp] = !VALUES.F_NAN 

inonan_uv10 = where(finite(maxuv10_wmo) EQ 1, cntnonan)
inan_uv10   = where(finite(maxuv10_wmo) EQ 0, cntnan)
FOR i = 0, cntnonan-1 DO BEGIN
  iok1 = where(date_ifs EQ date_wmo[inonan_uv10[i]])
  IF i EQ 0 AND iok1 NE -1 THEN iok_ifs = iok1
  IF i NE 0 AND iok1 NE -1 THEN iok_ifs = [iok_ifs,iok1]
  iok2 = where(date_ala EQ date_wmo[inonan_uv10[i]])
  IF i EQ 0 AND iok2 NE -1 THEN iok_ala = iok2
  IF i NE 0 AND iok2 NE -1 THEN iok_ala = [iok_ala,iok2]
ENDFOR
help, iok_ifs, iok_ala
cor_wmo_ifs_uv10 = correlate(maxuv10_wmo[inonan_uv10],maxuv10_ifs[iok_ifs])
cor_wmo_ala_uv10 = correlate(maxuv10_wmo[inonan_uv10],maxuv10_ala[iok_ala])
std_wmo_ifs_uv10 = round(stddev(maxuv10_ifs[iok_ifs]-maxuv10_wmo[inonan_uv10])*100.)/100.
std_wmo_ala_uv10 = round(stddev(maxuv10_ala[iok_ala]-maxuv10_wmo[inonan_uv10])*100.)/100.


; PLOT PRESSION
;openps, filename='MSLP_IBTRACS+ECMWF+ALADIN.ps'
splot, date_wmo, mslp_wmo, yrange=[920.,1015.], xtickformat='(I8)', win=0 ;, nsum=4
oplot, date_wmo, mslp_wmo, thick=3, color=250
oplot, date_ifs, mslp_ifs, thick=3, color=0
oplot, date_ala, mslp_ala, thick=3, color=50
xyouts, 0.15, 0.325, 'IBTRACS', /normal, charsize=2, charthick=2, color=250
xyouts, 0.15, 0.30, 'ECMWF', /normal, charsize=2, charthick=2, color=0
xyouts, 0.15, 0.275, 'ALADIN', /normal, charsize=2, charthick=2, color=50
xyouts, 0.15, 0.175, 'cor IBTRACS-IFS: '+strtrim(cor_wmo_ifs_mslp,2), /normal, charsize=2, charthick=2, color=0
xyouts, 0.15, 0.15, 'cor IBTRACS-ALADIN: '+strtrim(cor_wmo_ala_mslp,2), /normal, charsize=2, charthick=2, color=50
xyouts, 0.15, 0.125, 'stddev IBTRACS-IFS: '+strtrim(std_wmo_ifs_mslp,2), /normal, charsize=2, charthick=2, color=0
xyouts, 0.15, 0.10, 'stddev IBTRACS-ALADIN: '+strtrim(std_wmo_ala_mslp,2), /normal, charsize=2, charthick=2, color=50
;closeps
STOP


; PLOT VENT
;openps, filename='UV10_IBTRACS+ECMWF+ALADIN.ps'
splot, date_wmo, maxuv10_wmo, yrange=[0.,60.], xtickformat='(I8)', win=1 ;, nsum=4
oplot, date_wmo, maxuv10_wmo, thick=3, color=250
oplot, date_ifs, maxuv10_ifs, thick=3, color=0
oplot, date_ala, maxuv10_ala, thick=3, color=50
xyouts, 0.725, 0.75, 'IBTRACS', /normal, charsize=2, charthick=2, color=250
xyouts, 0.725, 0.725, 'ECMWF'  , /normal, charsize=2, charthick=2, color=0
xyouts, 0.725, 0.70, 'ALADIN'  , /normal, charsize=2, charthick=2, color=50
xyouts, 0.15, 0.175, 'cor IBTRACS-IFS: '+strtrim(cor_wmo_ifs_uv10,2), /normal, charsize=2, charthick=2, color=0
xyouts, 0.15, 0.15, 'cor IBTRACS-ALADIN: '+strtrim(cor_wmo_ala_uv10,2), /normal, charsize=2, charthick=2, color=50
xyouts, 0.15, 0.125, 'stddev IBTRACS-IFS: '+strtrim(std_wmo_ifs_uv10,2), /normal, charsize=2, charthick=2, color=0
xyouts, 0.15, 0.10, 'stddev IBTRACS-ALADIN: '+strtrim(std_wmo_ala_uv10,2), /normal, charsize=2, charthick=2, color=50
;closeps
STOP


; PLOT TRACK
;openps, filename='TRACK_IBTRACS+ECMWF+ALADIN.ps'
initncdf, file_ifs
domdef, ivandom
plt, pres_ifs[*,*,0], /nodata, /realcont, title='', subtitle='', win=2, /no_cb
oplot, lon_wmo, lat_wmo, psym=1, thick=3, color=250
oplot, lon_mslp_ifs, lat_mslp_ifs, psym=1, thick=3, color=0
oplot, lon_mslp_ala, lat_mslp_ala, psym=1, thick=3, color=50
xyouts, 0.75, 0.425, 'IBTRACS', /normal, charsize=2, charthick=2, color=250
xyouts, 0.75, 0.40, 'ECMWF'  , /normal, charsize=2, charthick=2, color=0
xyouts, 0.75, 0.375, 'ALADIN'  , /normal, charsize=2, charthick=2, color=50
;closeps
STOP


; PLOT DISTANCE
;openps, filename='DIST_IBTRACS+ECMWF+ALADIN.ps'
;splot, date_wmo_3h, dist_wmo_ifs_v2, yrange=[0.,500.], xtickformat='(I8)', win=3 
;oplot, date_wmo_3h, dist_wmo_ifs_v2, thick=3, color=0
;oplot, date_wmo, dist_wmo_ala_v2, thick=3, color=50
;mean_wmo_ifs = mean(dist_wmo_ifs_v2,/nan)
;mean_wmo_ala = mean(dist_wmo_ala_v2,/nan)
;xyouts, 0.55, 0.75, 'ECMWF (mean:'+strtrim(mean_wmo_ifs,2)+')'  , /normal, charsize=2, charthick=2, color=0
;xyouts, 0.55, 0.725, 'ALADIN (mean:'+strtrim(mean_wmo_ala,2)+')' , /normal, charsize=2, charthick=2, color=50
;closeps
;openps, filename='DIST_IBTRACS+ECMWF+ALADIN.ps'
;splot, date_wmo[inonan], dist_wmo_ifs_v1, yrange=[0.,500.], xtickformat='(I8)', win=4 
;oplot, date_wmo[inonan], dist_wmo_ifs_v1, thick=3, color=0
;oplot, date_wmo[inonan], dist_wmo_ala_v1, thick=3, color=50
;mean_wmo_ifs = mean(dist_wmo_ifs_v1,/nan)
;mean_wmo_ala = mean(dist_wmo_ala_v1,/nan)
;xyouts, 0.55, 0.75, 'ECMWF (mean:'+strtrim(mean_wmo_ifs,2)+')'  , /normal, charsize=2, charthick=2, color=0
;xyouts, 0.55, 0.725, 'ALADIN (mean:'+strtrim(mean_wmo_ala,2)+')' , /normal, charsize=2, charthick=2, color=50
;closeps

;openps, filename='DIST_IBTRACS+ECMWF+ALADIN.ps'
splot, date_wmo_3h, dist_wmo_ifs_noland, yrange=[0.,500.], xtickformat='(I8)', win=4 
oplot, date_wmo_3h, dist_wmo_ifs_noland, thick=3, color=0
oplot, date_wmo, dist_wmo_ala_noland, thick=3, color=50
tini = where(date_wmo EQ 2000207.00d)
mean_wmo_ala = mean(dist_wmo_ala_noland[tini:n_elements(dist_wmo_ala_noland)-1],/nan)
;tini_3h = where(date_wmo_3h EQ 2000207.00d)
;mean_wmo_ifs = mean(dist_wmo_ifs_noland[tini_3h:n_elements(dist_wmo_ifs_noland)-1],/nan)
mean_wmo_ifs = mean(dist_wmo_ifs_noland,/nan)
xyouts, 0.55, 0.75, 'ECMWF (mean:'+strtrim(mean_wmo_ifs,2)+')'  , /normal, charsize=2, charthick=2, color=0
xyouts, 0.55, 0.725, 'ALADIN (mean:'+strtrim(mean_wmo_ala,2)+')' , /normal, charsize=2, charthick=2, color=50
;closeps
STOP


END
