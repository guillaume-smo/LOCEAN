print, '' & print, 'PLOT TRACKS...'

lct,60
color_factor = 70
initncdf, '/home/gsamson/WORK/AROME/TEST_CPL/GRID_AROME_IVAN2km.nc'
domdef, dom_plt
key_portrait = 0
;write_ps = 0
IF write_ps THEN thc = 6 ELSE thc = 2



; PLOT ALL TRAJ DATE OBS
IF write_ps THEN openps, filename=plt_path+'ALL_TRACKS_DATEOBS'
plt, glamt, /nodata, /realcont, title='TC TRACKS', subtitle='', /no_cb, charsize=1.5, charthick=2
FOR i = 0, nb_exp-1 DO BEGIN
  cmd = execute('date = date_'+strtrim(i,2))  
  cmd = execute('lon  = lon_mslp_'+strtrim(i,2))
  cmd = execute('lat  = lat_mslp_'+strtrim(i,2))
  indok = (listmatch(date,date_0))[*,0]
  indbold = (listmatch(date[indok],round(date[indok])))[*,0]
  oplot, lon[indok], lat[indok], psym=1, color=i*color_factor MOD 256, thick=thc/2, symsize=1
  oplot, lon[indok[indbold]], lat[indok[indbold]], psym=1, color=i*color_factor MOD 256, thick=thc, symsize=2
  oplot, lon[indok], lat[indok], linestyle=0, color=i*color_factor MOD 256, thick=thc/2
  IF exp_list[i] EQ 'BEST-TRACK' THEN oplot, lon[indok],lat[indok], linestyle=0, color=i*color_factor MOD 256, thick=thc
  IF exp_list[i] EQ 'ALADIN-ANA' THEN oplot, lon[indok],lat[indok], linestyle=0, color=i*color_factor MOD 256, thick=thc
  IF i LE 6 THEN BEGIN
    xyouts, 0.100, 0.125-0.020*i, exp_list[i], /normal, charsize=1, charthick=2, color=color_factor*i MOD 256
    xyouts, 0.350, 0.125-0.020*i, '('+alt_list[i]+')', /normal, charsize=1, charthick=2, color=color_factor*i MOD 256
  ENDIF ELSE BEGIN
    xyouts, 0.550, 0.125-0.020*(i-7), exp_list[i], /normal, charsize=1, charthick=2, color=color_factor*i MOD 256
    xyouts, 0.800, 0.125-0.020*(i-7), '('+alt_list[i]+')', /normal, charsize=1, charthick=2, color=color_factor*i MOD 256
  ENDELSE
;  FOR j = 0, n_elements(date)-1,8 DO xyouts, lon[j]+0.25, lat[j]+0.25, date[j], orientation=45., alignment=0.5, color=color_factor*i MOD 256
  xyouts, lon[indok[0]]+0.1, lat[indok[0]]+0.1, long(date[indok[0]]*100), orientation=45., alignment=0., color=color_factor*i MOD 256
  xyouts, lon[indok[n_elements(indok)-1]]+0.1, lat[indok[n_elements(indok)-1]]+0.1, long(date[indok[n_elements(indok)-1]]*100), orientation=45., alignment=0., color=color_factor*i MOD 256
ENDFOR
IF write_ps THEN closeps ELSE saveimage, plt_path+'ALL_TRACKS_DATEOBS.gif', quality=100


; PLOT TRAJ PAR RESEAU
FOR d = 0, n_elements(date_list)-1 DO BEGIN

  curdate = date_list[d] & print, curdate

  IF write_ps THEN openps, filename=plt_path+curdate+'_TRACK_DATEOBS'
  plt, glamt, /nodata, /realcont, title=curdate+' TC TRACKS', subtitle='', /no_cb, charsize=1.5, charthick=2
  j = 0
  FOR i = 0, nb_exp-1 DO BEGIN
  IF (STRMATCH(alt_list[i],'*'+curdate+'*') EQ 1) OR (i LE 0) THEN BEGIN
    cmd = execute('date = date_'+strtrim(i,2))  
    cmd = execute('lon  = lon_mslp_'+strtrim(i,2))
    cmd = execute('lat  = lat_mslp_'+strtrim(i,2))
    indok = (listmatch(date,date_0))[*,0]
    indbold = (listmatch(date[indok],round(date[indok])))[*,0]
    oplot, lon[indok],lat[indok], psym=1, color=j*color_factor MOD 256, thick=thc/2, symsize=1
    oplot, lon[indok[indbold]],lat[indok[indbold]], psym=1, color=j*color_factor MOD 256, thick=thc, symsize=2
    oplot, lon[indok],lat[indok], linestyle=0, color=j*color_factor MOD 256, thick=thc/2
    IF exp_list[i] EQ 'BEST-TRACK' THEN oplot, lon[indok],lat[indok], linestyle=0, color=j*color_factor MOD 256, thick=thc
    IF exp_list[i] EQ 'ALADIN-ANA' THEN oplot, lon[indok],lat[indok], linestyle=0, color=j*color_factor MOD 256, thick=thc
    xyouts, 0.100, 0.125-0.020*j, exp_list[i], /normal, charsize=1, charthick=2, color=color_factor*j MOD 256
    xyouts, 0.350, 0.125-0.020*j, '('+alt_list[i]+')', /normal, charsize=1, charthick=2, color=color_factor*j MOD 256
    xyouts, lon[indok[0]]+0.1, lat[indok[0]]+0.1, long(date[indok[0]]*100), orientation=45., alignment=0., color=color_factor*j MOD 256
    xyouts, lon[indok[n_elements(indok)-1]]+0.1, lat[indok[n_elements(indok)-1]]+0.1, long(date[indok[n_elements(indok)-1]]*100), orientation=45., alignment=0., color=color_factor*j MOD 256
    j = j + 1
  ENDIF
  ENDFOR
  IF write_ps THEN closeps ELSE saveimage, plt_path+curdate+'_TRACKS_DATEOBS.gif', quality=100
ENDFOR
print, 'PLOT TRACKS OK' & print, ''
