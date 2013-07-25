PRO plot_2D_clim_NEMO
@common

var_list = ['SSS']
exp_list = ['COUPLED_SW2_BMJ']
bas_list = ['IO']
maskfile = '/usr/home/gslod/IDL/GRID_NEMO_IND4.nc'
period   = [19900101d,20100101d]

;--------------------------------------------------------------------------------------

FOR e = 0, n_elements(exp_list)-1 DO BEGIN
FOR b = 0, n_elements(bas_list)-1 DO BEGIN
FOR v = 0, n_elements(var_list)-1 DO BEGIN

basn = bas_list[b] & help, basn
varn = var_list[v] & help, varn
expn = exp_list[e] & help, expn

path = '/net/adonis/usr/adonis/varclim/gslod/EXP_'+expn+'/'
IF expn EQ 'COUPLED_SW2_BMJ' OR expn EQ 'COUPLED_SW2_KF' THEN file = varn+'_DAILY_1990-2009.nc' 
IF expn EQ 'WOA2009' THEN file = 'salinity_monthly_1deg.nc'
help, file
date_ini = 19900101d & help, date_ini
juld_ini = date2jul(date_ini) & help, juld_ini

IF basn EQ  'IO' THEN box = [30,130,-25,25]
IF basn EQ 'SIO' THEN box = [30,130,-30, 0]
IF basn EQ 'NIO' THEN box = [30,130,  0,25]
IF basn EQ 'BOB' THEN box = [80,100,  0,25]
IF basn EQ 'ARS' THEN box = [50, 80,  0,25]
IF basn EQ 'SWI' THEN box = [40, 80,-25, 0]
IF basn EQ 'SEI' THEN box = [80,120,-25, 0]
IF basn EQ 'SUM' THEN box = [80,100,-10, 5]

IF expn EQ 'COUPLED_SW2_BMJ' OR expn EQ 'COUPLED_SW2_KF' THEN initncdf, maskfile 
IF expn EQ 'WOA2009' THEN initncdf, path+file
domdef, box

IF varn EQ 'WFO' THEN var = read_ncdf('wfo', /allrecords, filename=path+file, /nostruct)
IF expn EQ 'COUPLED_SW2_BMJ' OR expn EQ 'COUPLED_SW2_KF' AND varn EQ 'SSS' THEN var = read_ncdf('sos', /allrecords, filename=path+file, /nostruct)
IF  expn EQ 'WOA2009' AND varn EQ 'SSS' THEN var = read_ncdf('s_an', /allrecords, filename=path+file, /nostruct)
help, var

nbt  = n_elements(var[0,0,*])
juld = juld_ini + dindgen(nbt) & help, juld
date = jul2date(juld, MONTH=month, DAY=day, YEAR=year) & help, date
iok  = where(date GE period[0] AND date LE period[1], cntok) & help, iok
var  = var[*,*,iok] & help, var
date = date[iok] & year = year[iok] & month = month[iok] & juld = juld[iok]

var_sc = fltarr(n_elements(var[*,0,0]),n_elements(var[0,*,0]),12)
IF varn EQ 'UV10' THEN var1_sc = fltarr(n_elements(var[*,0,0]),n_elements(var[0,*,0]),12)
IF varn EQ 'UV10' THEN var2_sc = fltarr(n_elements(var[*,0,0]),n_elements(var[0,*,0]),12)
FOR i = 0, 12-1 DO BEGIN
  iave = where(month EQ i+1) & help, iave
  var_sc[*,*,i] = m_mean(var[*,*,iave],dim=3,/nan)
  IF varn EQ 'UV10' THEN var1_sc[*,*,i] = m_mean(var1[*,*,iave],dim=3,/nan)
  IF varn EQ 'UV10' THEN var2_sc[*,*,i] = m_mean(var2[*,*,iave],dim=3,/nan)
ENDFOR
stop

initncdf, '/net/adonis/usr/adonis/varclim/gslod/EXP_WOA2009/salinity_monthly_1deg.nc'
domdef, box
var = read_ncdf('s_an', 0, 11, /timestep, filename='/net/adonis/usr/adonis/varclim/gslod/EXP_WOA2009/salinity_monthly_1deg.nc', /nostruct)
var = reform(var[*,*,0,*])
glamt_obs = glamt[firstxt:lastxt,firstyt:lastyt] & help, glamt_obs
gphit_obs = gphit[firstxt:lastxt,firstyt:lastyt] & help, gphit_obs

initncdf, maskfile
domdef, box
glamt_mod = glamt[firstxt:lastxt,firstyt:lastyt] & help, glamt_mod
gphit_mod = gphit[firstxt:lastxt,firstyt:lastyt] & help, gphit_mod
var_sc_gridobs = fltarr(n_elements(glamt_obs[*,0]),n_elements(glamt_obs[0,*]),12)
FOR i = 0, 11 DO var_sc_gridobs[*,*,i] = fromreg('bilinear',var_sc[*,*,i],glamt_mod[*,0],reform(gphit_mod[0,*]),glamt_obs[*,0],reform(gphit_obs[0,*]))

initncdf, '/net/adonis/usr/adonis/varclim/gslod/EXP_WOA2009/salinity_monthly_1deg.nc'
domdef, box

plt, m_mean(var_sc_gridobs[*,*,[11,0,1]],dim=3,/nan), /nocont, /realcont, min=28, max=36, lct=59, title=expn+' DJF' & stop
plt, m_mean(var[*,*,[11,0,1]],dim=3,/nan), /nocont, /realcont, min=28, max=36, lct=59, title='WOA09 DJF' & stop
plt, m_mean(var[*,*,[11,0,1]],dim=3,/nan)-m_mean(var_sc_gridobs[*,*,[11,0,1]],dim=3,/nan), /nocont, /realcont, min=-4, max=4, lct=64, title='WOA09 - '+expn+' DJF' & stop

plt, m_mean(var_sc_gridobs[*,*,[5,6,7]],dim=3,/nan), /nocont, /realcont, min=28, max=36, lct=59, title=expn+' JJA' & stop
plt, m_mean(var[*,*,[5,6,7]],dim=3,/nan), /nocont, /realcont, min=28, max=36, lct=59, title='WOA JJA' & stop
plt, m_mean(var[*,*,[5,6,7]],dim=3,/nan)-m_mean(var_sc_gridobs[*,*,[5,6,7]],dim=3,/nan), /nocont, /realcont, min=-4, max=4, lct=64, title='WOA09 - '+expn+' JJA' & stop

stop
plt, m_mean(var_sc[*,*,[11,0,1,2]],dim=3,/nan)*1000, /nocont, /realcont, /inv,  min=-1., max=0.
stop
plt, m_mean(var_sc[*,*,[5,6,7,8]],dim=3,/nan)*1000, /nocont, /realcont, /inv, min=-1., max=0.
stop

ENDFOR
ENDFOR
ENDFOR

stop
END
