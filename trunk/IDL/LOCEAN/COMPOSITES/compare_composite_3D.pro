PRO compare_composite_3D
@common


explist = ['KF']
varlist = ['QRAIN','QCLOUD'];'T','W','QVAPOR','QICE']

basin   = 'SIO'
freq    = '6H'
datebeg = '19900101'
dateend = '20100101'

zlev = [0.00,0.06,0.15,0.26,0.40,0.58,0.80,1.07,1.55,2.06,2.59,3.15,4.18,5.22,6.26,7.30,8.34,9.38,10.41,11.45,12.49,13.53,14.59,15.64,16.71,17.84,19.06]

wind_bin = [0.,60.]


FOR iexp = 0, n_elements(explist)-1 DO BEGIN

exp = explist[iexp]
expname1 = 'COUPLED_SW2_'+ exp & help, expname1
expname2 = 'FORCED_SW2_'+ exp & help, expname2
IF exp EQ 'KF' THEN period2  = '1990-1998' ELSE period2  = '1990-2009' & help, period2
IF exp EQ 'KF' THEN dateend2 = '19990101'  ELSE dateend2 = '20100101' & help, dateend2

; selection des cyclones 1
pathin1  = '/net/adonis/usr/adonis/varclim/gslod/IDL/COLLOCALISATION/EXP_'+expname1+'/DATA/' & help, pathin1
print, pathin1 + 'd1_TRACKS_'+ expname1 +'_IO_'+datebeg+'-'+dateend+'.dat'
restore, pathin1 + 'd1_TRACKS_'+ expname1 +'_IO_'+datebeg+'-'+dateend+'.dat', /VERBOSE
d1_lon1 = d1_lon
d1_lat1 = d1_lat
;ifin = where(finite(d1_pres) eq 1 AND finite(d1_max_wnd) eq 1) & help, ifin
IF basin EQ 'SIO'  THEN idom1 = where(d1_lon GT 30. AND d1_lon LT 130. AND d1_lat LT 0.)
help, idom1
iok1 = idom1
;iok1 = intersect(ifin,idom1) & help, iok1
iwind1 = where(d1_max_wnd GE wind_bin[0] AND d1_max_wnd LE wind_bin[1]) & help, iwind1
iok1 = intersect(idom1,iwind1) & help, iok1
;ivdep1 = where(d1_speed GE vdepbin[0] AND d1_speed LE vdepbin[1]) & help, ivdep1
;iok1 = intersect(iok1,ivdep1) & help, iok1
ijok1 = array_indices([(size(d1_lon1))[1], (size(d1_lon1))[2]], iok1, /dim)

; selection des cyclones 2
pathin2  = '/net/adonis/usr/adonis/varclim/gslod/IDL/COLLOCALISATION/EXP_'+expname2+'/DATA/' & help, pathin2
print, pathin2 + 'd1_TRACKS_'+ expname2 +'_IO_'+datebeg+'-'+dateend2+'.dat'
restore, pathin2 + 'd1_TRACKS_'+ expname2 +'_IO_'+datebeg+'-'+dateend2+'.dat', /VERBOSE
d1_lon2 = d1_lon
d1_lat2 = d1_lat
IF basin EQ 'SIO'  THEN idom2 = where(d1_lon GT 30. AND d1_lon LT 130. AND d1_lat LT 0.)
help, idom2
iok2 = idom2
iwind2 = where(d1_max_wnd GE wind_bin[0] AND d1_max_wnd LE wind_bin[1]) & help, iwind2
iok2 = intersect(idom2,iwind2) & help, iok2
ijok2 = array_indices([(size(d1_lon2))[1], (size(d1_lon2))[2]], iok2, /dim)


FOR ivar = 0, n_elements(varlist)-1 DO BEGIN

  var_name = varlist[ivar] & help, var_name

  print, pathin1 + var_name +'_RZ_1000km_' + expname1 +'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat'
  restore, pathin1 + var_name +'_RZ_1000km_' + expname1 +'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat', /VERBOSE
  help, var_2D
  distr = findgen(n_elements(var_2D[0,0,*,0])) * 50. & help, distr
  var_1 = fltarr(n_elements(iok1), 20, 27)
  FOR itmp = 0, n_elements(iok1)-1 DO BEGIN
    var_1[itmp,*,*] = var_2D[ijok1[0, itmp], ijok1[1, itmp],*,*]
  ENDFOR
  
  print, pathin2 + var_name +'_RZ_1000km_' + expname2 +'_'+freq+'_IO_'+datebeg+'-'+dateend2+'.dat'
  restore, pathin2 + var_name +'_RZ_1000km_' + expname2 +'_'+freq+'_IO_'+datebeg+'-'+dateend2+'.dat', /VERBOSE
  help, var_2D
  distr = findgen(n_elements(var_2D[0,0,*,0])) * 50. & help, distr
  var_2 = fltarr(n_elements(iok2), 20, 27)
  FOR itmp = 0, n_elements(iok2)-1 DO BEGIN
    var_2[itmp,*,*] = var_2D[ijok2[0, itmp], ijok2[1, itmp],*,*]
  ENDFOR

  IF var_name EQ 'T' THEN var_unit='(K)'
  IF var_name EQ 'QRAIN' OR var_name EQ 'QICE' OR var_name EQ 'QCLOUD' OR var_name EQ 'QVAPOR' THEN var_1 = var_1 * 10^3
  IF var_name EQ 'QRAIN' OR var_name EQ 'QICE' OR var_name EQ 'QCLOUD' OR var_name EQ 'QVAPOR' THEN var_2 = var_2 * 10^3

  varmoy_1 = m_mean(var_1, dim = 1, /nan) & help, varmoy_1
  print, where(distr GE 700)
  var_env1  = m_mean(varmoy_1[where(distr GE 800),*], dim=1, /nan) & help, var_env1
  var_ano1  = varmoy_1
  FOR i = 0, n_elements(var_1[0,*,0])-1 DO var_ano1[i,*]  = varmoy_1[i,*] - var_env1 & help, var_ano1
  varstd_1 = fltarr(20,27)
  FOR j= 0, 26 DO FOR i= 0, 19 DO varstd_1[i,j]=stddev(var_1[*,i,j], /nan)

  varmoy_2 = m_mean(var_2, dim = 1, /nan) & help, varmoy_2
  print, where(distr GE 700)
  var_env2  = m_mean(varmoy_2[where(distr GE 800),*], dim=1, /nan) & help, var_env2
  var_ano2  = varmoy_2
  FOR i = 0, n_elements(var_2[0,*,0])-1 DO var_ano2[i,*]  = varmoy_2[i,*] - var_env2 & help, var_ano2
  varstd_2 = fltarr(20,27)
  FOR j= 0, 26 DO FOR i= 0, 19 DO varstd_2[i,j]=stddev(var_2[*,i,j], /nan)


; plot
  nbx = 16 & nby = 16 & nbz = 27
  computegrid,0,0,50,50,nbx,nby
  jpk = nbz
  gdept = -1*zlev
  gdepw = findgen(nbz)
  e3t   = findgen(nbz)
  tmask = fltarr(nbx,nby,nbz) + 1. & help, tmask
  domdef, 0, nbx-1, 5, 5, /xindex, /yindex

  vmin = min([min(varmoy_1),min(varmoy_2)])
  vmax = max([max(varmoy_1),max(varmoy_2)])
  vdmin = min(varmoy_1[0:nbx-1,*]-varmoy_2[0:nbx-1,*])
  vdmax = max(varmoy_1[0:nbx-1,*]-varmoy_2[0:nbx-1,*])
  tmp = max([abs(vdmin),abs(vdmax)])
  vdmin = -1 * tmp & vdmax = tmp
  amin = min([min(var_ano1),min(var_ano2)])
  amax = max([max(var_ano1),max(var_ano2)])
  tmp = max([abs(amin),abs(amax)])
  amin = -1 * tmp & amax = tmp
  admin = min(var_ano1[0:nbx-1,*]-var_ano2[0:nbx-1,*])
  admax = max(var_ano1[0:nbx-1,*]-var_ano2[0:nbx-1,*])
  tmp = max([abs(admin),abs(admax)])
  admin = -1 * tmp & admax = tmp

;  smin = min([min(varstd_1),min(varstd_2)])
;  smax = max([max(varstd_1),max(varstd_2)])
  
  pathfig = '/usr/home/gslod/IDL/COMPOSITES/FIGS_RZ/'
  spawn, 'echo "creation du repertoire '+ pathfig +'"' & spawn, 'mkdir -p '+ pathfig
  lct,13
  pltz, varmoy_1[0:nbx-1,*], vmin, vmax, /rempli, /landscape, box=[-20,0], small=[3,1,1], $
  XTITLE='rayon (km)', YTITLE='altitude (km)'
  pltz, varmoy_2[0:nbx-1,*], vmin, vmax, /rempli, box=[-20,0], small=[3,1,2], /noerase, $
  XTITLE='rayon (km)', YTITLE='altitude (km)'
  lct,42
  pltz, varmoy_1[0:nbx-1,*]-varmoy_2[0:nbx-1,*], vdmin, vdmax, /rempli, box=[-20,0], small=[3,1,3], /noerase, $
  XTITLE='rayon (km)', YTITLE='altitude (km)'
  saveimage, pathfig+'composite_RZ_'+var_name+'_moy_'+exp+'_'+freq+'_'+basin+'.gif'

  lct,42
  pltz, var_ano1[0:nbx-1,*], amin, amax, /rempli, /landscape, box=[-20,0], small=[3,1,1], $
  XTITLE='rayon (km)', YTITLE='altitude (km)'
  pltz, var_ano2[0:nbx-1,*], amin, amax, /rempli, box=[-20,0], small=[3,1,2], /noerase, $
  XTITLE='rayon (km)', YTITLE='altitude (km)'
  pltz, var_ano1[0:nbx-1,*]-var_ano2[0:nbx-1,*], admin, admax, /rempli, box=[-20,0], small=[3,1,3], /noerase, $
  XTITLE='rayon (km)', YTITLE='altitude (km)'
  saveimage, pathfig+'composite_RZ_'+var_name+'_ano_'+exp+'_'+freq+'_'+basin+'.gif'
;  lct,13
;  pltz, varstd_1[0:nbx-1,*], /rempli, box=[-20,0]
;  saveimage, pathfig+'composite_RZ_'+var_name+'_std_'+exp+'_'+freq+'_'+basin+'_'+period+'.gif'

  lct,13
  pltz, varmoy_1[0:nbx-1,*], vmin, vmax, /rempli, /landscape, box=[-5,0], small=[3,1,1], $
  XTITLE='rayon (km)', YTITLE='altitude (km)'
  pltz, varmoy_2[0:nbx-1,*], vmin, vmax, /rempli, box=[-5,0], small=[3,1,2], /noerase, $
  XTITLE='rayon (km)', YTITLE='altitude (km)'
  lct,42
  pltz, varmoy_1[0:nbx-1,*]-varmoy_2[0:nbx-1,*], vdmin, vdmax, /rempli, box=[-5,0], small=[3,1,3], /noerase, $
  XTITLE='rayon (km)', YTITLE='altitude (km)'
  saveimage, pathfig+'composite_RZ5km_'+var_name+'_moy_'+exp+'_'+freq+'_'+basin+'.gif'
  lct,42
  pltz, var_ano1[0:nbx-1,*], amin, amax, /rempli, /landscape, box=[-5,0], small=[3,1,1], $
  XTITLE='rayon (km)', YTITLE='altitude (km)'
  pltz, var_ano2[0:nbx-1,*], amin, amax, /rempli, box=[-5,0], small=[3,1,2], /noerase, $
  XTITLE='rayon (km)', YTITLE='altitude (km)'
  pltz, var_ano1[0:nbx-1,*]-var_ano2[0:nbx-1,*], admin, admax, /rempli, box=[-5,0], small=[3,1,3], /noerase, $
  XTITLE='rayon (km)', YTITLE='altitude (km)'
  saveimage, pathfig+'composite_RZ5km_'+var_name+'_ano_'+exp+'_'+freq+'_'+basin+'.gif'

ENDFOR; var
ENDFOR; exp
stop
END
