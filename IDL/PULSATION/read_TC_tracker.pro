print, '' & print, 'read_TC_tracker...'

tcpath = path
tcfile = 'tracker_'+exp_name+'_u17_v30_t10_r30_1990-2009.idl'
restore, tcpath+tcfile, /VERBOSE
help, loncycn, latcycn, datecycn, juldcycn, uv10cycn, mslpcycn


; selection zone
startlat = reform(latcycn[*, 0])
startlon = reform(loncycn[*, 0])
indok = WHERE( startlon GE box[0] AND startlon LE box[1] AND startlat GE box[2] AND startlat LE box[3])
print, '' & help, indok
latcycn = latcycn[indok,*]
loncycn = loncycn[indok,*]
uv10cycn = uv10cycn[indok,*]
mslpcycn = mslpcycn[indok,*]
datecycn = datecycn[indok,*]
juldcycn = juldcycn[indok,*]
help, loncycn, latcycn, datecycn, juldcycn, uv10cycn, mslpcycn


; TS only (> 17 m/s)
indts = intarr( n_elements( uv10cycn[*,0]))*0
FOR i = 0, n_elements(uv10cycn[*,0])-1 DO IF (where(uv10cycn[i,*] GE 17.22))[0] NE -1 THEN indts[i] = 1
indok = where(indts EQ 1)
print, '' & help, indok
latcycn = latcycn[indok,*]
loncycn = loncycn[indok,*]
uv10cycn = uv10cycn[indok,*]
mslpcycn = mslpcycn[indok,*]
datecycn = datecycn[indok,*]
juldcycn = juldcycn[indok,*]
help, loncycn, latcycn, datecycn, juldcycn, uv10cycn, mslpcycn


; CALCUL NBMONTH
caldat, juldcycn, monthcycn, daycycn, yearcycn, hourcycn
nbmonthcycn = (yearcycn - yearini_obs) * 12. + monthcycn
help, nbmonthcycn


bin_size   = 5.  ; en degres
nbyear_tc  = 20.
cg_density = HIST_2D( loncycn[*,0], latcycn[*,0], bin1=bin_size, min1=box[0], max1=box[1], bin2=bin_size, min2=box[2], max2=box[3]) / float(nbyear_tc)
tc_density = HIST_2D( loncycn     , latcycn     , bin1=bin_size, min1=box[0], max1=box[1], bin2=bin_size, min2=box[2], max2=box[3]) / float(nbyear_tc)
help, bin_size, nbyear_tc, cg_density, tc_density

nbbin_lon = ( box[1] - box[0] ) / bin_size + 1
nbbin_lat = ( box[3] - box[2] ) / bin_size + 1
bins_lon  = box[0] + findgen(nbbin_lon) * bin_size
bins_lat  = box[2] + findgen(nbbin_lat) * bin_size
help, nbbin_lat, nbbin_lon, bins_lat, bins_lon

c_smooth = 1 ; coef smoothing
scg_density = cg_density * 0.
scg_density[*,where(bins_lat LT 0.)] = SMOOTH( cg_density[*,where(bins_lat LT 0.)], c_smooth)
scg_density[*,where(bins_lat GT 0.)] = SMOOTH( cg_density[*,where(bins_lat GT 0.)], c_smooth)
stc_density = tc_density * 0.
stc_density[*,where(bins_lat LT 0.)] = SMOOTH( tc_density[*,where(bins_lat LT 0.)], c_smooth)
stc_density[*,where(bins_lat GT 0.)] = SMOOTH( tc_density[*,where(bins_lat GT 0.)], c_smooth)


;computegrid,box[0], box[2], bin_size, bin_size, (box[1]-box[0]) / bin_size+1, (box[3]-box[2]) / bin_size+1
;plt, scg_density, 0, 1., /realcont, /nocont, lct=51, xtitle='', ytitle='', subtitle='',title='CG DENSITY '+exp_name
;STOP
;plt, scg_density / total(scg_density) * 100., 0, 20., /realcont, /nocont, lct=51, xtitle='', ytitle='', subtitle='',title='NORMALIZED CG DENSITY (%) '+exp_name
;STOP
;plt, stc_density, 0, 30., /realcont, /nocont, lct=51, xtitle='', ytitle='', subtitle='',title='TC DENSITY '+exp_name
;STOP
;plt, stc_density / total(stc_density) * 100., 0, 10., /realcont, /nocont, lct=51, xtitle='', ytitle='', subtitle='',title='NORMALIZED TC DENSITY (%) '+exp_name
;STOP


nbcg_1y    = FLTARR(nbyear_mod)
nbtcday_1y = FLTARR(nbyear_mod)
indok      = LISTMATCH( listyear_mod, listyear_mod)
FOR y = 0, nbyear_mod-1 DO BEGIN
  ind_year = ind_period + indok[y,0]*12
  ind_year = ind_year[ WHERE( ind_year LE nbmonth_mod-1)]
  ind_year = ind_year + 1
  ind_cg = !NULL
  ind_tc = !NULL
  FOR m = 0, n_elements(ind_year)-1 DO BEGIN
    ind_cg = [ ind_cg, WHERE( LONG(nbmonthcycn[*,0]) EQ ind_year[m])]
    ind_tc = [ ind_tc, WHERE( LONG(nbmonthcycn     ) EQ ind_year[m])]
  ENDFOR
  nbcg_1y[y]    = n_elements( ind_cg)
  nbtcday_1y[y] = n_elements( ind_tc)
ENDFOR
help, nbcg_1y, nbtcday_1y

ano_nbcg_1y    = FLTARR(nbyear_obs)
ano_nbtcday_1y = FLTARR(nbyear_obs)
FOR y = 0, nbyear_obs-1 DO BEGIN
  ano_nbcg_1y[y]    = nbcg_1y[y] - MEAN(nbcg_1y)
  ano_nbtcday_1y[y] = nbtcday_1y[y] - MEAN(nbtcday_1y)
ENDFOR
help, ano_nbcg_1y, ano_nbtcday_1y

cor_nbcg_1y    = FLTARR(nbyear_obs)
cor_nbtcday_1y = FLTARR(nbyear_obs)
FOR y = 0, nbyear_obs-1 DO BEGIN
  cor_nbcg_1y    = CORRELATE( nbcg_1y, nbcg_1y_0)
  cor_nbtcday_1y = CORRELATE( nbtcday_1y, nbtcday_1y_0)
ENDFOR
help, cor_nbcg_1y, cor_nbtcday_1y

; SAUVEGARDE
tcvar_list = [ 'cor_nbcg_1y', 'cor_nbtcday_1y', 'ano_nbcg_1y', 'ano_nbtcday_1y', 'nbcg_1y', 'nbtcday_1y', 'stc_density', 'scg_density', 'latcycn', 'loncycn', 'monthcycn', 'yearcycn', 'uv10cycn', 'mslpcycn', 'datecycn', 'nbmonthcycn']
FOR v = 0, n_elements(tcvar_list)-1 DO BEGIN
  cmd = execute( tcvar_list[v] + '_' + STRTRIM(e,2) + ' = ' + tcvar_list[v] )
  cmd = execute( 'help, '+tcvar_list[v] + '_' + STRTRIM(e,2) )
ENDFOR

print, 'read_TC_tracker OK' & print, ''
