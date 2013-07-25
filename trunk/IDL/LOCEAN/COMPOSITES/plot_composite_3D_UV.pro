PRO plot_composite_UV_RZ
@common


explist = ['COUPLED_SW2_BMJ','COUPLED_SW2_KF','FORCED_SW2_KF','FORCED_SW2_BMJ']
basin   = 'SIO'
freq    = '6H'
datebeg = '19900101'

zlev = [0.00,0.06,0.15,0.26,0.40,0.58,0.80,1.07,1.55,2.06,2.59,3.15,4.18,5.22,6.26,7.30,8.34,9.38,10.41,11.45,12.49,13.53,14.59,15.64,16.71,17.84,19.06]

wind_bin = [0.,60.]


FOR iexp = 0, n_elements(explist)-1 DO BEGIN

expname1 = explist[iexp] & help, expname1
IF expname1 EQ 'FORCED_SW2_KF' THEN period  = '1990-1998' ELSE period  = '1990-2009' & help, period
IF expname1 EQ 'FORCED_SW2_KF' THEN dateend = '19990101' ELSE dateend = '20100101' & help, dateend
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



  print, pathin1 + 'UV_RZ_1000km_' + expname1 +'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat'
  restore, pathin1 +'UV_RZ_1000km_' + expname1 +'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat', /VERBOSE
  help, ur_2d, ur_2d
  distr = findgen(n_elements(ut_2D[0,0,*,0])) * 50. & help, distr
  ut = fltarr(n_elements(iok1), 20, 27)
  ur = fltarr(n_elements(iok1), 20, 27)
  FOR itmp = 0, n_elements(iok1)-1 DO BEGIN
    ut[itmp,*,*] = ut_2D[ijok1[0, itmp], ijok1[1, itmp],*,*]
    ur[itmp,*,*] = ur_2D[ijok1[0, itmp], ijok1[1, itmp],*,*]
  ENDFOR
  
  utmoy = m_mean(ut, dim = 1, /nan) & help, utmoy
  urmoy = m_mean(ur, dim = 1, /nan) & help, urmoy
  stop
  env  = m_mean(varmoy_1[where(distr GE 800),*], dim=1, /nan) & help, var_env
  var_ano  = varmoy_1
  FOR i = 0, n_elements(var_1[0,*,0])-1 DO var_ano[i,*]  = varmoy_1[i,*] - var_env & help, var_ano
  varstd_1 = fltarr(20,27)
  FOR j= 0, 26 DO FOR i= 0, 19 DO varstd_1[i,j]=stddev(var_1[*,i,j], /nan)

  nbx = 20 & nby = 20 & nbz = 27
  computegrid,0,0,50,50,nbx,nby
  jpk = nbz
  gdept = -1*zlev
  gdepw = findgen(nbz)
  e3t   = findgen(nbz)
  tmask = fltarr(nbx,nby,nbz) + 1. & help, tmask
  domdef, 0, nbx-1, 5, 5, /xindex, /yindex

  pathfig = '/usr/home/gslod/IDL/COMPOSITES/FIGS_RZ/'
  spawn, 'echo "creation du repertoire '+ pathfig +'"' & spawn, 'mkdir -p '+ pathfig
  lct,13
  pltz, utmoy[0:nbx-1,*], /rempli, box=[-20,0]
  saveimage, pathfig+'composite_RZ_'+var_name+'_moy_'+expname1+'_'+freq+'_'+basin+'_'+period+'.gif'
  lct,42
  pltz, var_ano[0:nbx-1,*], /rempli, box=[-20,0]
  saveimage, pathfig+'composite_RZ_'+var_name+'_ano_'+expname1+'_'+freq+'_'+basin+'_'+period+'.gif'
  lct,13
  pltz, varstd_1[0:nbx-1,*], /rempli, box=[-20,0]
  saveimage, pathfig+'composite_RZ_'+var_name+'_std_'+expname1+'_'+freq+'_'+basin+'_'+period+'.gif'

ENDFOR; exp
stop
END
