print, '' & print, 'PLOT MAX COOLING...'

lct,60
color_factor=70
key_portrait=0
;write_ps = 0
IF write_ps THEN thc = 6 ELSE thc = 2
i = 0


FOR l = 0, n_elements(sst_list)-1 DO BEGIN
  IF sst_list[l] NE '' AND sst_list[l] NE 'AROME' THEN BEGIN

    sst_name = sst_list[l]
    exp_name = exp_list[i]

    IF i EQ 0 THEN BEGIN
      sst_name = sst_list[l]
      IF sst_name EQ 'REMSS-MWIR' THEN BEGIN & sst = ssti_0 & xdim_0 = xdim_ssti & ydim_0 = ydim_ssti & ENDIF
      IF sst_name EQ 'REMSS-MW'   THEN BEGIN & sst = sstm_0 & xdim_0 = xdim_sstm & ydim_0 = ydim_sstm & ENDIF
      IF sst_name EQ 'PSY3V3R1'   THEN BEGIN & sst = sstp_0 & xdim_0 = xdim_sstp & ydim_0 = ydim_sstp & ENDIF
      IF sst_name EQ 'GLORYS2V1'  THEN BEGIN & sst = sstg1_0 & xdim_0 = xdim_sstg1 & ydim_0 = ydim_sstg1 & ENDIF        
      IF sst_name EQ 'GLORYS2V3'  THEN BEGIN & sst = sstg3_0 & xdim_0 = xdim_sstg3 & ydim_0 = ydim_sstg3 & ENDIF
      IF sst_name EQ 'GLO2V1ALAD' THEN BEGIN & sst = sstg1a_0 & xdim_0 = xdim_sstg1a & ydim_0 = ydim_sstg1a & ENDIF
      IF sst_name EQ 'GLO2V3ALAD' THEN BEGIN & sst = sstg3a_0 & xdim_0 = xdim_sstg3a & ydim_0 = ydim_sstg3a & ENDIF
    ENDIF

  ; calcul cooling max
    cmd = execute( 'date = date_'+strtrim(i,2))
    cmd = execute( 'tmp = fltarr(xdim_'+strtrim(i,2)+', ydim_'+strtrim(i,2)+', tdim_'+strtrim(i,2)+'-1)')
    cmd = execute( 'cool'+strtrim(l,2)+'_'+strtrim(i,2)+' =  fltarr(xdim_'+strtrim(i,2)+', ydim_'+strtrim(i,2)+')')
    FOR j = 1, n_elements(date)-1 DO tmp[*,*,j-1] = sst[*,*,j]- sst[*,*,0]
    FOR j = 0, n_elements(tmp[*,0,0])-1 DO BEGIN
      FOR k = 0, n_elements(tmp[0,*,0])-1 DO BEGIN
	cmd = execute( 'cool'+strtrim(l,2)+'_'+strtrim(i,2)+'[j,k] = min(tmp[j,k,*])' )
      ENDFOR
    ENDFOR
    cmd = execute( 'maxcool'+strtrim(l,2)+'_'+strtrim(i,2)+' = min(cool'+strtrim(l,2)+'_'+strtrim(i,2)+', /nan)' )
    undefine, tmp

  ENDIF
ENDFOR


; min
minplot_cool = !VALUES.F_NAN
FOR l = 0, n_elements(sst_list)-1 DO cmd = execute( 'minplot_cool = min( [minplot_cool, maxcool'+strtrim(l,2)+'_'+strtrim(i,2)+' ], /nan)' )
minplot_cool = round( (round(minplot_cool*10)/10.) )
help, minplot_cool

; plot
FOR l = 0, n_elements(sst_list)-1 DO BEGIN
  IF sst_list[l] NE '' AND sst_list[l] NE 'AROME' THEN BEGIN 

    IF i EQ 0 THEN BEGIN
      sst_name = sst_list[l]
      IF sst_name EQ 'REMSS-MWIR' THEN initncdf, file_ssti
      IF sst_name EQ 'REMSS-MW'   THEN initncdf, file_sstm
      IF sst_name EQ 'PSY3V3R1'   THEN initncdf, file_sstp
      IF sst_name EQ 'GLORYS2V1'  THEN initncdf, file_sstg1
      IF sst_name EQ 'GLORYS2V3'  THEN initncdf, file_sstg3
      IF sst_name EQ 'GLO2V1ALAD' THEN initncdf, file_sstg1a
      IF sst_name EQ 'GLO2V3ALAD' THEN initncdf, file_sstg3a
      domdef, dom_tc
    ENDIF

    IF write_ps THEN openps, filename=plt_path+'MAX_COOLING_'+exp_name+'_'+sst_name & print, plt_path+'MAX_COOLING_'+exp_name+'_'+sst_name
    cmd = execute(' lon = lon_mslp_'+strtrim(i,2))
    cmd = execute(' lat = lat_mslp_'+strtrim(i,2))
    title = sst_name + ' - TC MAX COOLING BETWEEN '+strtrim(long(date[0]*100),2)+' AND '+strtrim(long(date[n_elements(date)-1]*100),2)
    cmd = execute(' plt, cool'+strtrim(l,2)+'_'+strtrim(i,2)+', minplot_cool, -1*minplot_cool, lct=64, /nocont, /realcont, subtitle="", title=title, charsize=1.5, charthick=2')
    indok = (listmatch(date,date_0))[*,0]
    indbold = (listmatch(date[indok],round(date[indok])))[*,0]
    oplot, lon[indok],lat[indok], psym=1, color=0, thick=thc/2, symsize=1
    oplot, lon[indok[indbold]],lat[indok[indbold]], psym=1, color=0, thick=thc, symsize=2
    oplot, lon[indok],lat[indok], linestyle=0, color=0, thick=thc
    xyouts, lon[indok[0]]+0.1, lat[indok[0]]+0.1, long(date[indok[0]]*100), orientation=45., alignment=0., color=0
    xyouts, lon[indok[n_elements(indok)-1]]+0.1, lat[indok[n_elements(indok)-1]]+0.1, long(date[indok[n_elements(indok)-1]]*100), orientation=45., alignment=0., color=0
    IF write_ps THEN closeps ELSE cmd = execute(' saveimage, plt_path+"MAX_COOLING_'+exp_name+'_'+sst_name+'.gif", quality=100' )

  ENDIF
ENDFOR


print, 'PLOT MAX COOLING OK' & print, ''
