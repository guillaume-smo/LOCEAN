print, '' & print, 'PLOT MAX COOLING...'


lct,60
color_factor=70
key_portrait=0
;write_ps = 0
IF write_ps THEN thc = 6 ELSE thc = 2


;FOR i = 0, nb_exp-1 DO BEGIN
i = 0

  exp_name = exp_list[i]
  IF i EQ 0 THEN BEGIN
    sst_name = sst_list[l]
    IF sst_name EQ 'REMSS-MWIR' THEN BEGIN & sst = ssti_0 & xdim_0 = xdim_ssti & ydim_0 = ydim_ssti & initncdf, file_ssti & ENDIF
    IF sst_name EQ 'REMSS-MW'   THEN BEGIN & sst = sstm_0 & xdim_0 = xdim_sstm & ydim_0 = ydim_sstm & initncdf, file_sstm & ENDIF
    IF sst_name EQ 'PSY3V3R1'   THEN BEGIN & sst = sstp_0 & xdim_0 = xdim_sstp & ydim_0 = ydim_sstp & initncdf, file_sstp & ENDIF
    IF sst_name EQ 'GLORYS2V1'  THEN BEGIN & sst = sstg1_0 & xdim_0 = xdim_sstg1 & ydim_0 = ydim_sstg1 & initncdf, file_sstg1 & ENDIF        
    IF sst_name EQ 'GLORYS2V3'  THEN BEGIN & sst = sstg3_0 & xdim_0 = xdim_sstg3 & ydim_0 = ydim_sstg3 & initncdf, file_sstg3 & ENDIF
    domdef, dom_tc
  ENDIF ELSE cmd = execute(' sst =  sst_'+strtrim(i,2))


; calcul cooling max
  cmd = execute('date = date_'+strtrim(i,2))
  cmd = execute(' tmp = fltarr(xdim_'+strtrim(i,2)+', ydim_'+strtrim(i,2)+', tdim_'+strtrim(i,2)+'-1)')
  cmd = execute(' maxcool_'+strtrim(i,2)+' =  fltarr(xdim_'+strtrim(i,2)+', ydim_'+strtrim(i,2)+')')
  FOR j = 1, n_elements(date)-1 DO tmp[*,*,j-1] = sst[*,*,j]- sst[*,*,0]
  FOR j = 0, n_elements(tmp[*,0,0])-1 DO BEGIN
    FOR k = 0, n_elements(tmp[0,*,0])-1 DO BEGIN
      cmd = execute(' maxcool_'+strtrim(i,2)+'[j,k] = min(tmp[j,k,*])')
    ENDFOR
  ENDFOR
  undefine, tmp


; plot
  IF write_ps THEN openps, filename=plt_path+'MAX_COOLING_'+exp_name+'_'+sst_name & print, plt_path+'MAX_COOLING_'+exp_name+'_'+sst_name
  cmd = execute(' lon = lon_mslp_'+strtrim(i,2))
  cmd = execute(' lat = lat_mslp_'+strtrim(i,2))
  cmd = execute(' minplot = min(MAXCOOL_'+strtrim(i,2)+', /NAN)')
  title = sst_name + ' - TC MAX COOLING BETWEEN '+strtrim(long(date[0]*100),2)+' AND '+strtrim(long(date[n_elements(date)-1]*100),2)
  cmd = execute(' plt, MAXCOOL_'+strtrim(i,2)+', minplot, -1*minplot, lct=64, /nocont, /realcont, subtitle="", title=title, charsize=1.5, charthick=2')
  indok = (listmatch(date,date_0))[*,0]
  indbold = (listmatch(date[indok],round(date[indok])))[*,0]
  oplot, lon[indok],lat[indok], psym=1, color=0, thick=thc/2, symsize=1
  oplot, lon[indok[indbold]],lat[indok[indbold]], psym=1, color=0, thick=thc, symsize=2
  oplot, lon[indok],lat[indok], linestyle=0, color=0, thick=thc
  xyouts, lon[indok[0]]+0.1, lat[indok[0]]+0.1, long(date[indok[0]]*100), orientation=45., alignment=0., color=0
  xyouts, lon[indok[n_elements(indok)-1]]+0.1, lat[indok[n_elements(indok)-1]]+0.1, long(date[indok[n_elements(indok)-1]]*100), orientation=45., alignment=0., color=0
  IF write_ps THEN closeps ELSE cmd = execute(' saveimage, plt_path+"MAX_COOLING_'+exp_name+'_'+sst_name+'.gif", quality=100' )

;  IF i EQ 0 THEN cmd = execute(' saveimage, plt_path+"MAX_COOLING_'+exp_list[i]+'_'+sst_name+'.gif", quality=100') $
;            ELSE cmd = execute(' saveimage, plt_path+"MAX_COOLING_'+exp_list[i]+'.gif", quality=100')
  
;ENDFOR

print, 'PLOT MAX COOLING OK' & print, ''