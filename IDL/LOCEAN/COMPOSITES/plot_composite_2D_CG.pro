PRO plot_composite_2D_CG
@common
computegrid,-200,-200,25,25,17,17


explist = ['COUPLED_SW2_KF','FORCED_SW2_KF','FORCED_SW2_BMJ','COUPLED_SW2_BMJ']
varlist = ['SST','UV10','Q2','LH','HFX','OLR','T2','PSFC'];'RAINC','RAINNC'

basin   = 'SIO'
freq    = '6H'
datebeg = '19900101'
var_typ = 'WSC' ; wsc / nsc
azi_dec = 0



FOR iexp = 0, n_elements(explist)-1 DO BEGIN

expname1 = explist[iexp] & help, expname1
IF expname1 EQ 'FORCED_SW2_KF' THEN period  = '1990-1998' ELSE period  = '1990-2009' & help, period
IF expname1 EQ 'FORCED_SW2_KF' THEN dateend = '19990101' ELSE dateend = '20100101' & help, dateend
pathin1  = '/net/adonis/usr/adonis/varclim/gslod/IDL/COLLOCALISATION/EXP_'+expname1+'/DATA/' & help, pathin1
print, pathin1 + 'd1_TRACKS_'+ expname1 +'_IO_'+datebeg+'-'+dateend+'.dat'
restore, pathin1 + 'd1_TRACKS_'+ expname1 +'_IO_'+datebeg+'-'+dateend+'.dat', /VERBOSE
d1_lon1 = d1_lon
d1_lat1 = d1_lat

;icg = intarr(n_elements(d1_lon1[*,0])) & help, icg
;FOR i = 0, n_elements(d1_lon1[*,0])-1 DO icg[i]=(where(finite(d1_pres[i,*]) eq 1 AND finite(d1_max_wnd[i,*]) eq 1))[0]
IF basin EQ 'SIO' THEN iok1 = where(d1_lon1[*,0] GT 30. AND d1_lon1[*,0] LT 130. AND d1_lat1[*,0] LT 0.)
help, iok1
ijok1 = array_indices([(size(d1_lon1))[1], (size(d1_lon1))[2]], iok1, /dim)


FOR ivar = 0, n_elements(varlist)-1 DO BEGIN

  var_name = varlist[ivar] & help, var_name
  print, pathin1 + var_name +'_2D_' + expname1 +'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat'
  restore, pathin1 + var_name +'_2D_' + expname1 +'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat', /VERBOSE
  IF var_typ EQ 'WSC' THEN dxy_var1 = dxy_var_wsc_rot
  IF var_typ EQ 'NSC' THEN dxy_var1 = dxy_var_nsc_rot
  help, dxy_var1

  IF var_name EQ 'RAINC' OR var_name EQ 'RAINNC' THEN BEGIN
  cpt=0
  FOR itmp = 0, n_elements(iok1)-1 DO BEGIN
    IF n_elements(where(dxy_var1[ijok1[0, itmp], ijok1[1, itmp],*,*] GE 1000.)) LE 1 THEN cpt = cpt+1
  ENDFOR
  var_1 = fltarr(cpt, 17,17)
  FOR itmp = 0, cpt-1 DO BEGIN
    IF n_elements(where(dxy_var1[ijok1[0, itmp], ijok1[1, itmp],*,*] GE 1000.)) LE 1 THEN var_1[itmp,*,*] = dxy_var1[ijok1[0, itmp], ijok1[1, itmp],*,*]
  ENDFOR
  ENDIF ELSE BEGIN
  var_1 = fltarr(n_elements(iok1), 17,17) 
  FOR itmp = 0, n_elements(iok1)-1 DO var_1[itmp,*,*] = dxy_var1[ijok1[0, itmp], ijok1[1, itmp],*,*]
  ENDELSE

  IF var_name EQ 'treal700' THEN var_unit='(degC)'
  IF var_name EQ 'Q2'  THEN BEGIN & var_1 = var_1 * 1000. & var_unit = '(g/kg)' & ENDIF
  IF var_name EQ 'UST' THEN BEGIN & var_unit = '(m/s)'  & vmin = 0.2 & vmax = 1.1 & ENDIF
  IF var_name EQ 'SST' THEN BEGIN & var_1 = var_1 - 273.15 & var_unit = '(degC)' & vmin = 28.8  & vmax = 29.7  & ENDIF
  IF var_name EQ 'LH' THEN BEGIN & var_unit = '(W/m^2)' & vmin = 60 & vmax = 480 & ENDIF
  IF var_name EQ 'RAINC' OR var_name EQ 'RAINNC' THEN var_unit = '(mm/h)'

  varmoy_1 = m_mean(var_1, dim = 1, /nan) & help, varmoy_1
  varstd_1 = fltarr(17,17)
  FOR j= 0, 16 DO FOR i= 0, 16 DO varstd_1[i,j]=stddev(var_1[*,i,j], /nan)


  IF azi_dec THEN BEGIN
    fac = 20
    rad = 25. / fac
    npt = (17-1) * fac + 1
    varmoyhd =  bilinear(varmoy_1, findgen(npt)/fac, findgen(npt)/fac)
    disttchd =  bilinear(dist_tc, findgen(npt)/fac, findgen(npt)/fac)
    azimeanhd = fltarr(npt,npt) + !values.f_nan

    FOR i = 0, (npt-1)/2 DO BEGIN
      irad = where(disttchd GE i*rad AND disttchd LT (i+1)*rad); & help, irad
      azimeanhd[irad] = mean(varmoyhd[irad],/nan)    
    ENDFOR

    azianohd = smooth(varmoyhd - azimeanhd,[1,1],/nan)
;    computegrid,-200,-200,rad,rad,npt,npt
    azimean = bilinear(azimeanhd,findgen(17)*fac,findgen(17)*fac)
    aziano  = bilinear(azianohd,findgen(17)*fac,findgen(17)*fac)
  ENDIF

computegrid,-200,-200,25,25,17,17

;  lct, 12
  pathfig = '/usr/home/gslod/IDL/COMPOSITES/'
  spawn, 'echo "creation du repertoire '+ pathfig +'"' & spawn, 'mkdir -p '+ pathfig

  plt, varmoy_1, title='COMPOSITE: '+var_name+' - '+var_typ+' - '+expname1, subtitle='', charsize=1.5
  xyouts, 0.1, 0.85, 'SAMPLE SIZE: '+strtrim(n_elements(iok1),2), align = 0, /normal, color=0, charsize=1.5
  saveimage, pathfig+'composite_2D-CG_'+var_name+'_'+var_typ+'_'+expname1+'_'+freq+'_'+basin+'_'+period+'.gif'

  plt, varstd_1, title='STD DEV: '+var_name+' - '+var_typ+' - '+expname1, subtitle='', charsize=1.5
 xyouts, 0.1, 0.85, 'SAMPLE SIZE: '+strtrim(n_elements(iok1),2), align = 0, /normal, color=0, charsize=1.5
  saveimage, pathfig+'composite_2D-CG_'+var_name+'std_'+var_typ+'_'+expname1+'_'+freq+'_'+basin+'_'+period+'.gif'


ENDFOR; var
ENDFOR; exp

END
