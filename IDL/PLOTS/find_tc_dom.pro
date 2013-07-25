PRO find_tc_dom
@all_cm

dom_ivan = [45.5000,68.0000,-21.7110,-9.21008]


print, ''
print, 'READING IBTRACS FILES...'


; IBTRACS parameters
dir_ibtracs = '/home/gsamson/WORK/DATA/IBTRACS/v03r04-WMO/'
fic_ibtracs = 'Basin.SI.ibtracs_wmo.v03r04.nc'
;ibtracs_all = '/home/gsamson/WORK/IBTRACS/v03r03-ALL/'
date_ini    = 18581117.00d
;tc_name     = 'IVAN'
dom_ivan    = [45.5000,68.0000,-21.7110,-9.21008]


; read data
name_wmo = string(ncdf_lec(fic_ibtracs, iodir = dir_ibtracs, var = 'name'))
time_wmo = ncdf_lec(fic_ibtracs, iodir = dir_ibtracs, var = 'time_wmo')
lat_wmo  = ncdf_lec(fic_ibtracs, iodir = dir_ibtracs, var = 'lat_wmo')*0.01
lon_wmo  = ncdf_lec(fic_ibtracs, iodir = dir_ibtracs, var = 'lon_wmo')*0.01
maxuv10_wmo = ncdf_lec(fic_ibtracs, iodir = dir_ibtracs, var = 'wind_wmo')*0.1*0.5144
mslp_wmo = ncdf_lec(fic_ibtracs, iodir = dir_ibtracs, var = 'pres_wmo')*0.1
help, name_wmo, time_wmo, lat_wmo, lon_wmo, maxuv10_wmo, mslp_wmo

; domain(TC) = IVAN
lat_wmo[where(lat_wmo LE -327.6)] = !VALUES.F_NAN
lon_wmo[where(lon_wmo LE -327.6)] = !VALUES.F_NAN
ind_lon = where(lon_wmo GE dom_ivan[0] AND lon_wmo LE dom_ivan[1]) & help, ind_lon
ind_lat = where(lat_wmo GE dom_ivan[2] AND lat_wmo LE dom_ivan[3]) & help, ind_lat
ind_wmo = intersect(ind_lon,ind_lat) & help, ind_wmo

; date(TC) > 2008/01/01
time_wmo[where(time_wmo GE 9.9e+36)] = !VALUES.F_NAN
juld_wmo = date2jul(date_ini) + time_wmo
date_wmo = jul2date(juld_wmo)
ind_dat = where(date_wmo GE 20080101.00d) & help, ind_dat
ind_wmo = intersect(ind_wmo,ind_dat) & help, ind_wmo

; wind(TC) > 33 m/s
maxuv10_wmo[where(maxuv10_wmo LE -1685.53 OR maxuv10_wmo EQ 0.)] = !VALUES.F_NAN
mslp_wmo[where(mslp_wmo LE -3276.7 OR mslp_wmo EQ 0.)] = !VALUES.F_NAN
ind_cat = where(maxuv10_wmo GE 33.) & help, ind_cat
ind_wmo = intersect(ind_wmo,ind_cat) & help, ind_wmo


ind_cyc = reform((array_indices(time_wmo,ind_wmo))[1,*])
ind_cyc = (listmatch(name_wmo[ind_cyc], name_wmo))[*,1] & help, ind_cyc



; PLOT TRAJ DATE OBS
initncdf, '/home/gsamson/WORK/AROME/OLIVE/grid_IVAN4km.nc'
domdef, dom_ivan
key_portrait=0

FOR i = 0, n_elements(ind_cyc)-1 DO BEGIN

  print, i, ind_cyc[i], name_wmo[ind_cyc[i]]

  indok = where(finite(maxuv10_wmo[*,ind_cyc[i]]) EQ 1)
  plt, glamt, /nodata, /realcont, subtitle='', /no_cb, win=0, lct=66, $
  title=name_wmo[ind_cyc[i]]+' - '+strtrim(date_wmo[0,ind_cyc[i]],2)+' - '+strtrim(max(maxuv10_wmo[*,ind_cyc[i]],/nan),2)+' m/s - '+strtrim(min(mslp_wmo[*,ind_cyc[i]],/nan),2)+' hPa'
;  colorbar, min=0., max=60., position=[0.1, 0.05, 0.95, 0.1], cb_color=0, /notitle
  oplot, lon_wmo[*,ind_cyc[i]],lat_wmo[*,ind_cyc[i]], psym=1, color=0, thick=1, symsize=1
  oplot, lon_wmo[*,ind_cyc[i]],lat_wmo[*,ind_cyc[i]], linestyle=0, color=0, thick=1
  FOR j = 0, n_elements(indok)-1 DO oplot, [lon_wmo[indok[j],ind_cyc[i]],lon_wmo[indok[j],ind_cyc[i]]],[lat_wmo[indok[j],ind_cyc[i]],lat_wmo[indok[j],ind_cyc[i]]], psym=6, color=maxuv10_wmo[indok[j],ind_cyc[i]]*4.25, thick=3, symsize=2
;  xyouts, lon[indok[0]]+0.1, lat[indok[0]]+0.1, long(date[indok[0]]*100), orientation=45., alignment=0., color=color_factor*i
;  xyouts, lon[indok[n_elements(indok)-1]]+0.1, lat[indok[n_elements(indok)-1]]+0.1, long(date[indok[n_elements(indok)-1]]*100), orientation=45., alignment=0., color=color_factor*i
  saveimage, '/home/gsamson/WORK/IDL/FIGURES/TRAJ_'+name_wmo[ind_cyc[i]]+'.jpg', quality=100, /jpeg
ENDFOR

STOP
END
