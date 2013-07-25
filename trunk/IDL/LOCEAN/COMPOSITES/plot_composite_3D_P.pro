PRO plot_composite_3D_P
@common


explist = ['COUPLED_SW2_BMJ','FORCED_SW2_KF','FORCED_SW2_BMJ']
varlist = ['TPOT','TREAL']

basin   = 'SIO'
freq    = '6H'
datebeg = '19900101'
dr      = 50.            ; resolution radiale en km
zlev = [0.00,0.06,0.15,0.26,0.40,0.58,0.80,1.07,1.55,2.06,2.59,3.15,4.18,5.22,6.26,7.30,8.34,9.38,10.41,11.45,12.49,13.53,14.59,15.64,16.71,17.84,19.06]
wind_bin = [15,20]
rayon_ano = 800

nbx = 20 & nby = 20 & nbz = 27
computegrid,0,0,50,50,nbx,nby,zaxis=findgen(nbz);*100./(nbz-1)


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

print, pathin1 + 'P_RZ_1000km_' + expname1 +'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat'
restore, pathin1 + 'P_RZ_1000km_' + expname1 +'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat', /VERBOSE
prestmp = var_2D / 100.

FOR ivar = 0, n_elements(varlist)-1 DO BEGIN

  var_name = varlist[ivar] & help, var_name
  IF var_name EQ 'TPOT' OR var_name EQ 'TREAL' THEN BEGIN
    print, pathin1 + 'T_RZ_1000km_' + expname1 +'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat'
    restore, pathin1 + 'T_RZ_1000km_' + expname1 +'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat', /VERBOSE
  ENDIF ELSE BEGIN
    print, pathin1 + var_name + '_RZ_1000km_' + expname1 +'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat'
    restore, pathin1 + var_name + '_RZ_1000km_' + expname1 +'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat', /VERBOSE
  ENDELSE
  help, var_2D

  axe_x = findgen(n_elements(var_2D[0,0,*,0])) * dr & help, axe_x
  var_1 = fltarr(n_elements(iok1), 20, 27)
  pres  = fltarr(n_elements(iok1), 20, 27)
  FOR itmp = 0, n_elements(iok1)-1 DO BEGIN
    var_1[itmp,*,*] =  var_2D[ijok1[0, itmp], ijok1[1, itmp],*,*]
    pres[itmp,*,*]  = prestmp[ijok1[0, itmp], ijok1[1, itmp],*,*]
  ENDFOR
  
  IF var_name EQ 'TPOT' OR var_name EQ 'TREAL' THEN  var_unit='(K)'
  IF var_name EQ 'TREAL' THEN var_1 = var_1 / (1000. / pres)^0.2854
  IF var_name EQ 'QRAIN' OR var_name EQ 'QICE' OR var_name EQ 'QCLOUD' OR var_name EQ 'QVAPOR' THEN var_1 = var_1 * 10^3

  varmoy_1 = m_mean(var_1, dim = 1, /nan) & help, varmoy_1
  presmoy  = m_mean(pres, dim = 1, /nan) & help, presmoy
  var_env  = m_mean(varmoy_1[where(axe_x GE 800),*], dim=1, /nan) & help, var_env
  presenv  = m_mean(presmoy[where(axe_x GE 800),*], dim=1, /nan) & help, presenv
  var_ano  = varmoy_1
  presano  = varmoy_1
  FOR i = 0, n_elements(var_1[0,*,0])-1 DO BEGIN
    var_ano[i,*] = varmoy_1[i,*] - var_env
    presano[i,*] = presmoy[i,*] - presenv
  ENDFOR


  varstd_1 = fltarr(20,27)
  FOR j= 0, 26 DO FOR i= 0, 19 DO varstd_1[i,j]=stddev(var_1[*,i,j], /nan)


  domdef, 0, nbx-1, 5, 5, /xindex, /yindex
  IF var_name EQ 'TREAL' THEN BEGIN & vmin = -2.5 & vmax = 2.5 & ENDIF
  IF var_name EQ 'TPOT' THEN BEGIN & vmin = -4   & vmax = 4 & ENDIF
  pathfig = '/usr/home/gslod/IDL/COMPOSITES/FIGS_RZ/'
  spawn, 'echo "creation du repertoire '+ pathfig +'"' & spawn, 'mkdir -p '+ pathfig

  lct,13
  pltzz, presmoy, varmoy_1, /rempli, box=[0,1005],zoom=1005, $
  cell_fill=2, xtickformat='', xtitle='radius (km)', ytitle='pressure (hPa)', subtitle='', title=''
  saveimage, pathfig+'composite_RP_'+var_name+'_moy_'+expname1+'_'+freq+'_'+basin+'_'+period+'.gif'

  lct,42
  pltzz, presmoy, var_ano,  /rempli, box=[0,1005],zoom=1005, min=vmin, max=vmax, $
  cell_fill=2, xtickformat='', xtitle='radius (km)', ytitle='pressure (hPa)', subtitle='', title=''
  saveimage, pathfig+'composite_RP_'+var_name+'_ano'+strtrim(rayon_ano,2)+'_'+expname1+'_'+freq+'_'+basin+'_'+period+'.gif'


ENDFOR; var
ENDFOR; exp

stop
END
