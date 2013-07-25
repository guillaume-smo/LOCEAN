PRO plot_1D
; plot time series 1D
@all_cm

var_list = ['RAIN']
;exp_list = ['ERA-I','COUPLED_SW2_KF','COUPLED_SW2_BMJ','CPL_BMJ_V33','CPL_TDK_V33','CPL_NSAS_V33']
exp_list = ['ERA-I','COUPLED_SW2_KF','COUPLED_SW2_BMJ']
;exp_list = ['NOAA','COUPLED_SW2_KF','COUPLED_SW2_BMJ','CPL_KFOLD_V33','CPL_BMJ_V33','CPL_BMJCAM_V33']
bas_list = ['SIO','NIO']
period   = [19900101d,20091231d]

;--------------------------------------------------------------------------------------

maskfile = '/usr/home/gslod/IDL/MASK_WRF_GRID_T.nc'

FOR b = 0, n_elements(bas_list)-1 DO BEGIN
FOR v = 0, n_elements(var_list)-1 DO BEGIN
FOR e = 0, n_elements(exp_list)-1 DO BEGIN

basn = bas_list[b] & help, basn
varn = var_list[v] & help, varn
expn = exp_list[e] & help, expn

path = '/net/adonis/usr/adonis/varclim/gslod/EXP_'+expn+'/'
file = varn+'_1990-1991.nc'
IF expn EQ 'CPL_BMJCAM_V33' THEN file = varn+'_1990.nc'
IF expn EQ 'TROPFLUX' THEN file = varn+'_1989-2010.nc'
IF expn EQ 'NOAA' THEN file = varn+'_1974-2011.nc'
help, file

IF expn EQ 'TROPFLUX' THEN date_ini = 19890101d ELSE date_ini = 19900101d
IF expn EQ 'NOAA' THEN date_ini = 19740601d ELSE date_ini = 19900101d
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
IF expn EQ 'NOAA' THEN BEGIN & initncdf, path+file & domdef, box & ENDIF

IF expn EQ 'ERA-I' THEN BEGIN
  initncdf, path+file & domdef, box
  sst = read_ncdf('SST', 0, 0, /timestep, filename='/net/adonis/usr/adonis/varclim/gslod/EXP_ERA-I/SST_1990-1991.nc', /nostruct) & help, sst
  mask = sst & mask[where(finite(sst) EQ 1)] = 1 & mask[where(finite(sst) EQ 0)] = !VALUES.F_NAN & help, mask
ENDIF

IF expn NE 'ERA-I' AND expn NE 'TROPFLUX' AND expn NE 'NOAA' THEN BEGIN
  initncdf, maskfile & domdef, box
  landmask = read_ncdf('LANDMASK',filename=maskfile,/nostruct) & help, landmask
  mask = landmask & mask[where(landmask EQ 0)] = 1 & mask[where(landmask EQ 1)] = !VALUES.F_NAN & help, mask
ENDIF

IF varn NE 'UV10' AND varn NE 'HFXLH' AND varn NE 'RAIN' THEN $
var  = read_ncdf(varn, /allrecords, filename=path+file, /nostruct) 
IF varn EQ 'UV10' THEN BEGIN
  var1 = read_ncdf('U10', /allrecords, filename=path+file, /nostruct)
  var2 = read_ncdf('V10', /allrecords, filename=path+file, /nostruct)
  var  = sqrt(var1^2+var2^2)^0.5
  angle= windangle(temporary(var1),temporary(var2))+90.
ENDIF
IF varn EQ 'HFXLH' THEN BEGIN
  var1 = read_ncdf('HFX', /allrecords, filename=path+file, /nostruct)
  var2 = read_ncdf('LH', /allrecords, filename=path+file, /nostruct)
  var  = var1 + var2
  IF expn EQ 'TROPFLUX' THEN var = -1. * var
ENDIF
IF varn EQ 'RAIN' THEN BEGIN
  var1 = read_ncdf('RAINC', /allrecords, filename=path+file, /nostruct)
  var1 = var1 - shift(var1,0,0,1) & var1[*,*,0] = 0.
  var2 = read_ncdf('RAINNC', /allrecords, filename=path+file, /nostruct)
  var2 = var2 - shift(var2,0,0,1) & var2[*,*,0] = 0.
  var  = var1 + var2
  IF expn EQ 'TROPFLUX' THEN var = -1. * var
ENDIF
help, var


nbt  = n_elements(var[0,0,*])
IF expn EQ 'TROPFLUX' OR expn EQ 'NOAA' THEN juld = juld_ini + dindgen(nbt) ELSE juld = juld_ini + dindgen(nbt)/4
help, juld
date = jul2date(juld) & help, date
iok  = where(date GE period[0] AND date LE period[1], cntok) & help, iok
var  = var[*,*,iok] & help, var
IF expn NE 'TROPFLUX' AND expn NE 'NOAA' THEN mask = reform(reform(mask, nxt*nyt)#replicate(1,cntok), nxt, nyt, cntok)
IF expn NE 'TROPFLUX' THEN var  = var * mask
var = m_mean(m_mean(var,dim=2,/nan),dim=1,/nan) & help, var
IF varn EQ 'SST' THEN var = var - 273.15
IF varn EQ 'T2' AND expn NE 'TROPFLUX' THEN var = var - 273.15
IF varn EQ 'Q2' AND expn NE 'TROPFLUX' THEN var = var * 1000.
IF expn EQ 'TROPFLUX' OR expn EQ 'NOAA' THEN varsmooth = ts_smooth(var,11) ELSE varsmooth = ts_smooth(var,41) & help, varsmooth
var = varsmooth

;angle = m_mean(m_mean(angle*mask,dim=2,/nan),dim=1,/nan) & help, var
;angle = ts_smooth(angle,41)

IF expn EQ 'TROPFLUX' OR expn EQ 'NOAA' THEN days = findgen(cntok) ELSE days = findgen(cntok)/4
help, days
IF expn EQ 'TROPFLUX' OR expn EQ 'NOAA' THEN month = findgen(cntok)/30.5 ELSE month = findgen(cntok)/(4*30.5)
help, month

ymax = ceil(max(var[0:n_elements(var)-1]))+10./100*ceil(max(var[0:n_elements(var)-1]))
ymin = floor(min(var[0:n_elements(var)-1]))-10./100*floor(min(var[0:n_elements(var)-1]))
;openps, filename='idl.ps'
IF e EQ 0 THEN plot, month, var, xtitle='MONTHS', ytitle=varn, charsize=1.2, thick=2, $
ystyle=1, yrange=[ymin,ymax], title='BASIN: '+basn ELSE oplot, month, var, color=e*50 
xyouts, 0.15, 0.275-e*0.025, expn, color=e*50, charsize=1.2, /normal

;IF e EQ 0 THEN plot, month, angle, xtitle='MONTHS', ytitle='ANGLE', charsize=1.2, thick=2, $
;ystyle=1, yrange=[0,360], title='BASIN: '+basn ELSE oplot, month, angle, color=e*50
;xyouts, 0.15, 0.275-e*0.025, expn, color=e*50, charsize=1.2, /normal

ENDFOR
;closeps
;fig = varn+'_1D_1990-1991_'+basn+'.pdf'
;spawn, 'ps2pdf /usr/home/gslod/IDL/idl.ps '+fig
saveimage, varn+'_1D_1990-1991_'+basn+'.gif'
;saveimage, 'ANGLE_1D_1990-1991_'+basn+'.gif'

ENDFOR
ENDFOR

stop
END
