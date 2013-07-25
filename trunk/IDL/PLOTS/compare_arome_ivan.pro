PRO compare_arome_ivan
@all_cm

exp_path = '/home/gsamson/WORK/AROME/OLIVE/'
exp_list = ['9AT4+9AT5','9AR1+9ARA','9ATT+9ATS','9AR3+9AR5']
fic_name = 'ICMSH????_00H-96H.nc'
nb_exp = n_elements(exp_list)


;  mask = read_ncdf('IND.TERREMER', filename=exp_path+'grid_IVAN4km.nc', /nostruct) & help, mask
;  mask[where(mask EQ 1.)] = !VALUES.F_NAN & mask[where(mask EQ 0.)] = 1.
  mask = read_ncdf('SST', filename=exp_path+'grid_IVAN4km.nc', /nostruct) & help, mask
  mask = reverse(mask,2) & mask[where(finite(mask) EQ 1.)] = 1.
  mask[where(glamt LT 49. OR gphit LT -19.)] = !VALUES.F_NAN ; lakes fix
stop

; creation d'un nouveau masque "elargi" pour eviter les effets cotiers et les forcages orographiques
newmask = mask
FOR l = 1, n_elements(gphit[0,*])-2 DO BEGIN
  FOR k = 1, n_elements(glamt[*,0])-2 DO BEGIN
    IF finite(mask[k,l]) EQ 0. THEN BEGIN
      newmask[k-1,l-1] = !VALUES.F_NAN
      newmask[k-1,l  ] = !VALUES.F_NAN
      newmask[k-1,l+1] = !VALUES.F_NAN
      newmask[k,l-1] = !VALUES.F_NAN
      newmask[k,l+1] = !VALUES.F_NAN
      newmask[k+1,l-1] = !VALUES.F_NAN
      newmask[k+1,l  ] = !VALUES.F_NAN
      newmask[k+1,l+1] = !VALUES.F_NAN
    ENDIF
  ENDFOR
ENDFOR
mask = temporary(newmask) & help, mask
stop


FOR i = 0, nb_exp-1 DO BEGIN

  exp_name = exp_list[i]
  file_path = exp_path+exp_name+'/'+fic_name
  initncdf, file_path

;  lon = ncdf_lec( exp_path+'grid_IVAN4km.nc', var = 'lon') & help, lon
;  lat = reverse(ncdf_lec( exp_path+'grid_IVAN4km.nc', var = 'lat')) & help, lat
;  xdim = n_elements(lon)
;  ydim = n_elements(lat)
;  lon = replicate_array(lon, ydim)
;  lat = transpose(replicate_array(lat, xdim))
  
  var = 'SST'
  tmp = execute(var+'_'+strtrim(i,2)+' = read_ncdf("'+var+'", filename="'+file_path+'", /nostruct, /allrecords) & help, '+var+'_'+strtrim(i,2))
  tmp = execute('tdim = n_elements(SST_'+strtrim(i,2)+'[0,0,*])')
;  mask = replicate_array(mask, tdim) & help, mask

  var = 'ZON10M'
  tmp = execute(var+'_'+strtrim(i,2)+' = read_ncdf("'+var+'", filename="'+file_path+'", /nostruct, /allrecords) & help, '+var+'_'+strtrim(i,2))

  var = 'MER10M'
  tmp = execute(var+'_'+strtrim(i,2)+' = read_ncdf("'+var+'", filename="'+file_path+'", /nostruct, /allrecords) & help, '+var+'_'+strtrim(i,2))

  var = 'W10M'
  tmp = execute(var+'_'+strtrim(i,2)+' = sqrt(ZON10M_'+strtrim(i,2)+'^2+MER10M_'+strtrim(i,2)+'^2)')

  var = 'MSLP'
  tmp = execute(var+'_'+strtrim(i,2)+' = read_ncdf("'+var+'", filename="'+file_path+'", /nostruct, /allrecords) & help, '+var+'_'+strtrim(i,2))
  tmp = execute(var+'_'+strtrim(i,2)+' = reform('+var+'_'+strtrim(i,2)+')')

  tmp = execute('maxw10m_'+strtrim(i,2)+' = fltarr(tdim)')
  tmp = execute('minmslp_'+strtrim(i,2)+' = fltarr(tdim)')
  tmp = execute('lon_maxw10m_'+strtrim(i,2)+' = fltarr(tdim)')
  tmp = execute('lat_maxw10m_'+strtrim(i,2)+' = fltarr(tdim)')
  tmp = execute('lon_minmslp_'+strtrim(i,2)+' = fltarr(tdim)')
  tmp = execute('lat_minmslp_'+strtrim(i,2)+' = fltarr(tdim)')

  FOR j = 0, tdim-1 DO BEGIN
    tmp = execute('maxw10m_'+strtrim(i,2)+'[j] = MAX( W10M_'+strtrim(i,2)+'[*,*,j] * mask, /nan )')
    tmp = execute('lon_maxw10m_'+strtrim(i,2)+'[j] = glamt[!C]')
    tmp = execute('lat_maxw10m_'+strtrim(i,2)+'[j] = gphit[!C]')
    tmp = execute('minmslp_'+strtrim(i,2)+'[j] = MIN( MSLP_'+strtrim(i,2)+'[*,*,j] * mask, /nan )')
    tmp = execute('lon_minmslp_'+strtrim(i,2)+'[j] = glamt[!C]')
    tmp = execute('lat_minmslp_'+strtrim(i,2)+'[j] = gphit[!C]')
  ENDFOR

  tmp = execute('rmw_'+strtrim(i,2)+' = map_npoints(lon_minmslp_'+strtrim(i,2)+', lat_minmslp_'+strtrim(i,2)+', lon_maxw10m_'+strtrim(i,2)+', lat_maxw10m_'+strtrim(i,2)+', /TWO_BY_TWO) / 1000.')

ENDFOR


STOP

min_plot = MIN([minmslp_0,minmslp_1,minmslp_2])
max_plot = MAX([minmslp_0,minmslp_1,minmslp_2])
splot, minmslp_0, yrange=[min_plot,max_plot]
oplot, minmslp_1, color=50
oplot, minmslp_2, color=100
oplot, minmslp_3, color=150
xyouts, 0.125, 0.175, exp_list[0], /normal, charsize=2, charthick=2, color=0
xyouts, 0.125, 0.150, exp_list[1], /normal, charsize=2, charthick=2, color=50
xyouts, 0.125, 0.125, exp_list[2], /normal, charsize=2, charthick=2, color=100
xyouts, 0.125, 0.100, exp_list[3], /normal, charsize=2, charthick=2, color=150

initncdf, exp_path+'grid_IVAN4km.nc'
plt, glamt, /nodata, /realcont, title='', subtitle='', /no_cb
oplot, lon_minmslp_0, lat_minmslp_0, psym=1
oplot, lon_minmslp_1, lat_minmslp_1, psym=1, color=50
oplot, lon_minmslp_2, lat_minmslp_2, psym=1, color=100
oplot, lon_minmslp_3, lat_minmslp_3, psym=1, color=150
xyouts, 0.125, 0.275, exp_list[0], /normal, charsize=2, charthick=2, color=0
xyouts, 0.125, 0.250, exp_list[1], /normal, charsize=2, charthick=2, color=50
xyouts, 0.125, 0.225, exp_list[2], /normal, charsize=2, charthick=2, color=100
xyouts, 0.125, 0.200, exp_list[3], /normal, charsize=2, charthick=2, color=150


min_plot = MIN([maxw10m_0,maxw10m_1,maxw10m_2])
max_plot = MAX([maxw10m_0,maxw10m_1,maxw10m_2])
splot, maxw10m_0, yrange=[min_plot,max_plot]
oplot, maxw10m_1, color=50
oplot, maxw10m_2, color=100
oplot, maxw10m_3, color=150
xyouts, 0.125, 0.175, exp_list[0], /normal, charsize=2, charthick=2, color=0
xyouts, 0.125, 0.150, exp_list[1], /normal, charsize=2, charthick=2, color=50
xyouts, 0.125, 0.125, exp_list[2], /normal, charsize=2, charthick=2, color=100
xyouts, 0.125, 0.100, exp_list[3], /normal, charsize=2, charthick=2, color=150

initncdf, exp_path+'grid_IVAN4km.nc'
plt, glamt, /nodata, /realcont, title='', subtitle='', /no_cb
oplot, lon_maxw10m_0, lat_maxw10m_0, psym=1
oplot, lon_maxw10m_1, lat_maxw10m_1, psym=1, color=50
oplot, lon_maxw10m_2, lat_maxw10m_2, psym=1, color=100
oplot, lon_maxw10m_3, lat_maxw10m_3, psym=1, color=150
xyouts, 0.125, 0.275, exp_list[0], /normal, charsize=2, charthick=2, color=0
xyouts, 0.125, 0.250, exp_list[1], /normal, charsize=2, charthick=2, color=50
xyouts, 0.125, 0.225, exp_list[2], /normal, charsize=2, charthick=2, color=100
xyouts, 0.125, 0.200, exp_list[3], /normal, charsize=2, charthick=2, color=150


min_plot = MIN([rmw_0,rmw_1,rmw_2])
max_plot = MAX([rmw_0,rmw_1,rmw_2])
splot, rmw_0, yrange=[min_plot,max_plot]
oplot, rmw_1, color=50
oplot, rmw_2, color=100
oplot, rmw_3, color=150
xyouts, 0.125, 0.175, exp_list[0], /normal, charsize=2, charthick=2, color=0
xyouts, 0.125, 0.150, exp_list[1], /normal, charsize=2, charthick=2, color=50
xyouts, 0.125, 0.125, exp_list[2], /normal, charsize=2, charthick=2, color=100
xyouts, 0.125, 0.100, exp_list[3], /normal, charsize=2, charthick=2, color=150


STOP
END
