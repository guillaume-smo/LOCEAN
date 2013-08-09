print, '' & print, 'PLOT CPL COOLING...'


lct,60
color_factor=70
key_portrait=0
;write_ps = 0
IF write_ps THEN thc = 6 ELSE thc = 2



  cmd = execute( ' var =  sst_'+strtrim(i,2) )
  cmd = execute( 'xdim = xdim_'+strtrim(i,2) )
  cmd = execute( 'ydim = ydim_'+strtrim(i,2) )
  cmd = execute( 'tdim = tdim_'+strtrim(i,2) )
  cmd = execute( 'file = file_'+strtrim(i,2) )
  cmd = execute( 'date = date_'+strtrim(i,2) )
  cmd = execute( ' lon = lon_mslp_'+strtrim(i,2) )
  cmd = execute( ' lat = lat_mslp_'+strtrim(i,2) )


; calcul cooling max
  tmp = fltarr(xdim, ydim, tdim-1)
  maxcool = fltarr(xdim, ydim)
  FOR j = 1, tdim-1 DO tmp[*,*,j-1] = var[*,*,j] - var[*,*,0]
  FOR j = 0, xdim-1 DO BEGIN
    FOR k = 0, ydim-1 DO BEGIN
      maxcool[j,k] = min(tmp[j,k,*])
    ENDFOR
  ENDFOR
  undefine, tmp


; plot
  initncdf, file & domdef, dom_tc
  IF write_ps THEN openps, filename=plt_path+'MAX_COOLING_'+exp_name+'_'+date_list[(i-1) MOD nb_date] & print, plt_path+'MAX_COOLING_'+exp_name+'_'+date_list[(i-1) MOD nb_date]
  title = exp_name + ' MAX COOLING '+strtrim(long(date[0]*100),2)+' - '+strtrim(long(date[tdim-1]*100),2)
  plt, MAXCOOL, minplot_cool, -1*minplot_cool, lct=64, /nocont, /realcont, subtitle="", title=title, charsize=1.5, charthick=2
  indok = (listmatch(date,date_0))[*,0]
  indbold = (listmatch(date[indok],round(date[indok])))[*,0]
  oplot, lon[indok],lat[indok], psym=1, color=0, thick=thc/2, symsize=1
  oplot, lon[indok[indbold]],lat[indok[indbold]], psym=1, color=0, thick=thc, symsize=2
  oplot, lon[indok],lat[indok], linestyle=0, color=0, thick=thc
  xyouts, lon[indok[0]]+0.1, lat[indok[0]]+0.1, long(date[indok[0]]*100), orientation=45., alignment=0., color=0
  xyouts, lon[indok[n_elements(indok)-1]]+0.1, lat[indok[n_elements(indok)-1]]+0.1, long(date[indok[n_elements(indok)-1]]*100), orientation=45., alignment=0., color=0
  IF write_ps THEN closeps ELSE saveimage, plt_path+'MAX_COOLING_'+exp_name+'_'+date_list[(i-1) MOD nb_date]+'.gif', quality=100


print, 'PLOT CPL COOLING OK' & print, ''
