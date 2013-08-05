PRO plot_tc_list_tracks
@all_cm

  tc_list  = ['IVAN' ,'GAEL', 'FELLENG', 'GIOVANNA', 'GELANE']
  exp_name = 'BEST-TRACK'
  alt_name = 'IBTRACS/RSMC'
  write_ps = 1
  plt_path = '/home/gsamson/WORK/IDL/FIGURES/'
  force_rsmc = 1


; LECTURE DATA
  nb_tc    = n_elements(tc_list)
  FOR i = 0, nb_tc-1 DO BEGIN
    tc_name = tc_list[i]
    @def_dates_obs
    @read_best_track
  ENDFOR


  ; PLOT DATA
  lct,60
  color_factor = 70
  initncdf, '/home/gsamson/WORK/AROME/TEST_CPL/GRID_AROME_IVAN2km.nc'
  key_portrait = 1
  IF write_ps THEN thc = 6 ELSE thc = 2
  IF write_ps THEN openps, filename=plt_path+'ALL_TC_TRACKS_OBS'

  plt, glamt, 0., 70., /nodata, /realcont, title='TC TRACKS & MAXIMUM WIND SPEED (m/s)', subtitle='', charsize=1.5, charthick=2, format='(I2)'

  FOR j = 0, nb_tc-1 DO BEGIN

    cmd = execute('date = date_'+strtrim(j,2))  
    cmd = execute('lon  = lon_mslp_'+strtrim(j,2))
    cmd = execute('lat  = lat_mslp_'+strtrim(j,2))
    cmd = execute('wind = w10m_sea_'+strtrim(j,2))
    cmd = execute('tdim = tdim_'+strtrim(j,2))
    tc_name = tc_list[j]
    indbold = (listmatch(date,round(date)))[*,0]

    oplot, lon, lat, linestyle=0, color=0*color_factor MOD 256, thick=thc/2
;    oplot, lon, lat, psym=1, color=0*color_factor MOD 256, thick=thc/2, symsize=1
    FOR k = 0, tdim -1 DO oplot, [lon[k],lon[k]], [lat[k],lat[k]], symsize=2, psym=cgsymcat(16), color= wind[k] * 256 / 70.
;    oplot, lon[indbold], lat[indbold], psym=1, color=0*color_factor MOD 256, thick=thc, symsize=2
    IF tc_name NE 'IVAN' AND tc_name NE 'GELANE' THEN xyouts, lon[0], lat[0]+0.15, tc_name, orientation=0, alignment=0, color=0, charsize=1, charthick=2
    IF tc_name EQ 'IVAN' THEN xyouts, lon[0], lat[0]-0.5, tc_name, orientation=0, alignment=0, color=0, charsize=1, charthick=2
    IF tc_name EQ 'GELANE' THEN xyouts, lon[0]-1.5, lat[0]-0.5, tc_name, orientation=0, alignment=0, color=0, charsize=1, charthick=2

  ENDFOR
  IF write_ps THEN closeps ELSE saveimage, plt_path+'ALL_TC_TRACKS_OBS', quality=100

END
