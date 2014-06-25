
initncdf, path_0+file_0, glam=[20,380] & domdef, box
;box_hov     = [ 80, 100,  0, 25]
box_hov     = [ 72, 87,  0, 25]

; HOVMULLER
ibeg = MIN( WHERE( glamt[firstxt:lastxt,firstyt] GE box_hov[0] )) & help, ibeg
iend = MAX( WHERE( glamt[firstxt:lastxt,firstyt] LE box_hov[1] )) & help, iend
var_xmean_0 = MEAN( var_0[ibeg:iend,*,*], DIMENSION=1, /NAN) & help, var_xmean_0
FOR e = 1, n_elements(exp_list)-1 DO cmd = execute( 'var_xmean_'+strtrim(e,2)+' = MEAN( var_'+strtrim(e,2)+'_gridobs, DIMENSION=1, /NAN)' )

; STATS
FOR e = 1, n_elements(exp_list)-1 DO BEGIN
  cmd = execute( 'indok = WHERE( FINITE('+'var_xmean_0) EQ 1 AND FINITE('+'var_xmean_'+strtrim(e,2)+') EQ 1, nbok)' )
  cmd = execute( 'corrpat_0'+strtrim(e,2)+' = CORRELATE( (REFORM('+'var_xmean_0, n_elements('+'var_xmean_0)))[indok], (REFORM('+'var_xmean_'+strtrim(e,2)+', n_elements('+'var_xmean_'+strtrim(e,2)+')))[indok] )' )
ENDFOR


var_plot = 'var_xmean'
;initncdf, path_0+file_0, glam=[20,380], /fullcgrid & domdef, box_hov, /MEMEINDICES
nb_plot = n_elements(exp_list)
@def_plot_win
@def_plot_minmax
minvar = 0. & maxvar = maxvar/2. & intvar = intvar / 2.
fig_name = var_name+'_hov'+STRTRIM( FIX( box_hov[0]))+'E-'+STRTRIM( FIX( box_hov[1]))+'E_'+data_type+'_'+zone+'_'+mask_title
help, time_0
time = DATE2JUL(time_0)
jpt  = n_elements(time)
IF write_ps THEN openps, filename=plot_dir+'/'+STRUPCASE(data_type)+'/'+zone+'/'+var_name+'/'+fig_name


pltt, var_xmean_0, 'yt', min=minvar, max=maxvar, int=intvar, lct=22, /nocont, small=[win, 1], title=var_name+' '+data_type+' HOVMULLER '+STRING(box_hov[0], FORMAT='(I3)')+'E-'+STRING(box_hov[1], FORMAT='(I3)')+'E - '+obs_name, $
/nocolorbar, COLNUMB=colnumb, NCONTOUR=ncontour, subtitle='', xtitle='', ytitle='', charsize=1, $
ytickname=['JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC']
FOR e = 1, n_elements(exp_list)-1 DO BEGIN
  cmd = execute( 'var_plot = var_xmean_'+strtrim(e,2) )
  pltt, var_plot, 'yt', min=minvar, max=maxvar, int=intvar, lct=22, /nocont, small=[win, e+1], /noerase, /nocolorbar, $
  subtitle='', xtitle='', ytitle='', charsize=1, title=var_name+' '+data_type+' HOVMULLER - '+exp_list[e], $
  ytickname=['JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC']
  ;cmd = execute( 'xyouts, 0, -5, "CORR_PAT=" + string( corrpat_0'+strtrim(e,2)+', format="(F6.3)"), /DATA, charsize=1' )
  cmd = execute( 'xyouts, xy[0], xy[1], "CORR_PAT=" + string( corrpat_0'+strtrim(e,2)+'[0], format="(F6.3)"), /DATA, charsize=1, ALIGNMENT=0.5' )
ENDFOR
@def_plot_cb

IF write_ps THEN closeps ELSE STOP


IF data_type NE 'c1m' AND nbyear_0 GT 1 THEN BEGIN
; HOVMULLER SC1m
FOR e = 0, n_elements(exp_list)-1 DO BEGIN
  cmd = execute( 'var_xmean_sc1m_'+strtrim(e,2)+' = FLTARR(nyt, 12)' )
  FOR m = 0, 12-1 DO BEGIN
    cmd = execute( 'indok = WHERE( listmonth_'+strtrim(e,2)+' EQ m, nbok)' )
    cmd = execute( 'IF nbok NE nbyear_'+strtrim(e,2)+' THEN STOP' )
    cmd = execute( 'var_xmean_sc1m_'+strtrim(e,2)+'[*,m] = MEAN( var_xmean_'+strtrim(e,2)+'[*,indok], DIMENSION=2, /NAN)' )
  ENDFOR
ENDFOR


; STATS
FOR e = 1, n_elements(exp_list)-1 DO BEGIN
  cmd = execute( 'indok = WHERE( FINITE('+'var_xmean_sc1m_0) EQ 1 AND FINITE('+'var_xmean_sc1m_'+strtrim(e,2)+') EQ 1, nbok)' )
  cmd = execute( 'corrpat_0'+strtrim(e,2)+' = CORRELATE( (REFORM('+'var_xmean_sc1m_0, n_elements('+'var_xmean_sc1m_0)))[indok], (REFORM('+'var_xmean_sc1m_'+strtrim(e,2)+', n_elements('+'var_xmean_sc1m_'+strtrim(e,2)+')))[indok] )' )
ENDFOR


var_plot = 'var_xmean_sc1m'
;initncdf, path_0+file_0, glam=[20,380], /fullcgrid & domdef, box_hov, /MEMEINDICES
nb_plot = n_elements(exp_list)
@def_plot_win
@def_plot_minmax
fig_name = var_name+'_hovsc1m_'+STRTRIM( FIX( box_hov[0]))+'E-'+STRTRIM( FIX( box_hov[1]))+'E_'+data_type+'_'+zone+'_'+mask_title
help, time_0
time = DATE2JUL(time_0[0:11])
jpt  = n_elements(time)
IF write_ps THEN openps, filename=plot_dir+'/'+STRUPCASE(data_type)+'/'+zone+'/'+var_name+'/'+fig_name


pltt, var_xmean_sc1m_0, 'yt', min=minvar, max=maxvar, int=intvar, lct=22, /nocont, small=[win, 1], title=var_name+' '+data_type+' HOVMULLER '+STRING(box_hov[0], FORMAT='(I3)')+'E-'+STRING(box_hov[1], FORMAT='(I3)')+'E - '+obs_name, $
/nocolorbar, COLNUMB=colnumb, NCONTOUR=ncontour, subtitle='', xtitle='', ytitle='', charsize=1, $
ytickname=['JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC']
FOR e = 1, n_elements(exp_list)-1 DO BEGIN
  cmd = execute( 'var_plot = var_xmean_sc1m_'+strtrim(e,2) )
  pltt, var_plot, 'yt', min=minvar, max=maxvar, int=intvar, lct=22, /nocont, small=[win, e+1], /noerase, /nocolorbar, $
  subtitle='', xtitle='', ytitle='', charsize=1, title=var_name+' '+data_type+' HOVMULLER - '+exp_list[e], $
  ytickname=['JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC']
  ;cmd = execute( 'xyouts, 0, -5, "CORR_PAT=" + string( corrpat_0'+strtrim(e,2)+', format="(F6.3)"), /DATA, charsize=1' )
  cmd = execute( 'xyouts, xy[0], xy[1], "CORR_PAT=" + string( corrpat_0'+strtrim(e,2)+'[0], format="(F6.3)"), /DATA, charsize=1, ALIGNMENT=0.5' )
ENDFOR
@def_plot_cb

IF write_ps THEN closeps ELSE STOP
ENDIF ; NOT c1m DATA
