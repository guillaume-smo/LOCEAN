PRO plot_2D_SSTUV10
@all_cm

var_list = ['SST','UV10']
;exp_list = ['ERA-I','COUPLED_SW2_KF','COUPLED_SW2_BMJ','CPL_KFOLD_V33','CPL_BMJ_V33','CPL_BMJCAM_V33']
exp_list = ['ERA-I','COUPLED_SW2_BMJ','CPL_WRF33_NEMO32','CPL_BMJ_V33']
bas_list = ['IO']
maskfile = '/usr/home/gslod/IDL/MASK_WRF_GRID_T.nc'
period   = [19910601d,19910831d]

;--------------------------------------------------------------------------------------

FOR b = 0, n_elements(bas_list)-1 DO BEGIN
FOR e = 0, n_elements(exp_list)-1 DO BEGIN
FOR v = 0, n_elements(var_list)-1 DO BEGIN

basn = bas_list[b] & help, basn
varn = var_list[v] & help, varn
expn = exp_list[e] & help, expn

path = '/net/adonis/usr/adonis/varclim/gslod/EXP_'+expn+'/'
file = varn+'_1990-1991.nc'
IF expn EQ 'CPL_BMJCAM_V33' OR expn EQ 'CPL_KFCAM_V33' THEN file = varn+'_1990.nc'
IF expn EQ 'TROPFLUX' THEN file = varn+'_1989-2010.nc'
help, file
IF expn EQ 'TROPFLUX' THEN date_ini = 19890101d ELSE date_ini = 19900101d
help, date_ini
juld_ini = date2jul(date_ini) & help, juld_ini

IF basn EQ  'IO' THEN box = [30,130,-30,25]
IF basn EQ 'SIO' THEN box = [30,130,-30, 0]
IF basn EQ 'NIO' THEN box = [30,130,  0,25]
IF basn EQ 'BOB' THEN box = [80,100,  0,25]
IF basn EQ 'ARS' THEN box = [50, 80,  0,25]
IF basn EQ 'SWI' THEN box = [40, 80,-25, 0]
IF basn EQ 'SEI' THEN box = [80,120,-25, 0]
IF basn EQ 'SUM' THEN box = [80,100,-10, 5]

IF expn EQ 'TROPFLUX' THEN BEGIN & initncdf, path+file & domdef, box & ENDIF

IF expn EQ 'ERA-I' THEN BEGIN
  initncdf, path+file & domdef, box
  sst = read_ncdf('SST', 0, 0, /timestep, filename='/net/adonis/usr/adonis/varclim/gslod/EXP_ERA-I/SST_1990-1991.nc', /nostruct) & help, sst
  mask = sst & mask[where(finite(sst) EQ 1)] = 1 & mask[where(finite(sst) EQ 0)] = !VALUES.F_NAN & help, mask
ENDIF

IF expn NE 'ERA-I' AND expn NE 'TROPFLUX' THEN BEGIN
  initncdf, maskfile & domdef, box
  landmask = read_ncdf('LANDMASK',filename=maskfile,/nostruct) & help, landmask
  mask = landmask & mask[where(landmask EQ 0)] = 1 & mask[where(landmask EQ 1)] = !VALUES.F_NAN & help, mask
ENDIF

IF varn NE 'UV10' THEN var  = read_ncdf(varn, /allrecords, filename=path+file, /nostruct) 
IF varn EQ 'UV10' THEN BEGIN
  var1 = read_ncdf('U10', /allrecords, filename=path+file, /nostruct)
  var2 = read_ncdf('V10', /allrecords, filename=path+file, /nostruct)
  var  = sqrt(var1^2+var2^2)^0.5
  angle= windangle(var1,var2)+90.
ENDIF
help, var

nbt  = n_elements(var[0,0,*])
IF expn EQ 'TROPFLUX' THEN juld = juld_ini + dindgen(nbt) ELSE juld = juld_ini + dindgen(nbt)/4
help, juld
date = jul2date(juld) & help, date
iok  = where(date GE period[0] AND date LE period[1], cntok) & help, iok
var  = var[*,*,iok] & help, var
var = m_mean(var[*,*,iok],dim=3,/nan) & help, var
IF expn NE 'TROPFLUX' THEN var  = var * mask
IF varn EQ 'SST' THEN var = var - 273.15
IF varn EQ 'UV10' THEN BEGIN 
  var1 = m_mean(var1[*,*,iok],dim=3,/nan) & var1 = var1 * mask & help, var1
  var2 = m_mean(var2[*,*,iok],dim=3,/nan) & var2 = var2 * mask & help, var2
ENDIF

IF varn EQ 'SST' THEN BEGIN
ymin = 21 & ymax = 33
plt, var, ymin, ymax, /nocontour, /realcont, lct=34, title='EXP: '+expn+' PERIOD: '+strtrim(long(period[0]),2)+'-'+strtrim(long(period[1]),2),$
subtitle=varn, xtitle=' ', ytitle=' ', charsize=1.2, cb_charsize=1.2, format='(F4.1)'
ENDIF

IF varn EQ 'UV10' THEN BEGIN
IF expn EQ 'ERA-I' THEN ajoutvect, {u:{a:var1, g:'T'}, v:{a:var2, g:'T'}}, unvectsur = [7,7], vectmin = 0., vectmax = 15.
IF expn NE 'ERA-I' THEN ajoutvect, {u:{a:var1, g:'T'}, v:{a:var2, g:'T'}}, unvectsur = [20,20], vectmin = 0., vectmax = 15.
ENDIF

ENDFOR
saveimage, varn+'_2D_'+strtrim(long(period[0]),2)+'-'+strtrim(long(period[1]),2)+'_'+expn+'.gif'
stop
ENDFOR
ENDFOR

stop
END
