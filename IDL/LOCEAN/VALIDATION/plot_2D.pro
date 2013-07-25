PRO plot_2D

var_list = ['UV10']
;exp_list = ['CPL_KFNEW_V33','CPL_BMJ_V33','CPL_TDK_V33','CPL_NSAS_V33']
;exp_list = ['COUPLED_SW2_KF','COUPLED_SW2_BMJ']
exp_list = ['ERA-I','COUPLED_SW2_KF','COUPLED_SW2_BMJ','CPL_KFOLD_V33','CPL_BMJ_V33']
sea_list = ['DJF','MAM','JJA','SON']
maskfile = '/usr/home/gslod/IDL/MASK_WRF_GRID_T.nc'
date_ini = 19900101d & help, date_ini
juld_ini = date2jul(date_ini) & help, juld_ini
period   = [19900601d,19910901d]
box      = [30,130,-30,25]

;FOR s = 0, n_elements(sea_list)-1 DO BEGIN
FOR v = 0, n_elements(var_list)-1 DO BEGIN
FOR e = 0, n_elements(exp_list)-1 DO BEGIN

;sean = sea_list[s] & help, sean
varn = var_list[v] & help, varn
expn = exp_list[e] & help, expn
path = '/net/adonis/usr/adonis/varclim/gslod/EXP_'+expn+'/'
file = varn+'_1990-1991.nc'

IF expn EQ 'ERA-I' THEN initncdf, path+file ELSE initncdf, maskfile
domdef, box
IF expn NE 'ERA-I' THEN BEGIN
  landmask = read_ncdf('LANDMASK',filename=maskfile,/nostruct) & help, landmask
  mask = landmask
  mask[where(landmask EQ 0)] = 1 & mask[where(landmask EQ 1)] = !VALUES.F_NAN & help, mask
ENDIF

var  = read_ncdf(varn,/allrecords,filename=path+file, /nostruct) & help, var
nbt  = n_elements(var[0,0,*]) & help, nbt
juld = juld_ini + dindgen(nbt)/4 & help, juld
date = jul2date(juld) & help, date
iok  = where(date GE period[0] AND date LE period[1]) & help, iok
var  = m_mean(var[*,*,iok],dim=3,/nan) & help, var
IF varn EQ 'SST'   THEN var = var - 273.15
IF expn NE 'ERA-I' THEN var = var * mask

;openps, filename = 'idl.ps'
plt, var, contour=var, contint=1, /realcont, lct=34, title='EXP: '+expn+' PERIOD: '+strtrim(long(period[0]),2)+'-'+strtrim(long(period[1]),2),$
subtitle=varn, xtitle=' ', ytitle=' ', charsize=1.2, cb_charsize=1.2, format='(I2)';, win=e
;closeps
;saveimage, varn+'_2D_AVE_'+strtrim(long(period[0]),2)+'-'+strtrim(long(period[1]),2)+'_'+expn+'.gif'
;fig=varn+'_2D_AVE_'+strtrim(long(period[0]),2)+'-'+strtrim(long(period[1]),2)+'_'+expn+'.pdf'
;spawn, 'ps2pdf /usr/home/gslod/IDL/idl.ps '+fig
stop
ENDFOR
ENDFOR
;ENDFOR

stop
END
