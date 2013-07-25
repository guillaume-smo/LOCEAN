PRO plot_SST_UV10_RAIN_interannuel
@all_cm

exp_list = ['ERA-I','COUPLED_SW2_KF','COUPLED_SW2_BMJ']
period   = [19900101d,20091231d]
basn     = 'IOD'

;--------------------------------------------------------------------------------------

nbyear = long(period[1]/10000) - long(period[0]/10000) + 1
nbmonth = 12-(long(period[0]/100) - long(period[0]/10000)*100)+1 + (nbyear-2)*12 + (long(period[1]/100) - long(period[1]/10000)*100)
maskfile = '/usr/home/gslod/IDL/MASK_WRF_GRID_T.nc'

FOR e = 0, n_elements(exp_list)-1 DO BEGIN
expn = exp_list[e] & help, expn
IF e EQ 0 THEN ins = fltarr(3,nbyear)
IF e EQ 0 THEN inw = fltarr(3,nbyear)
IF e EQ 0 THEN ine = fltarr(3,nbyear)
IF e EQ 0 THEN ano = fltarr(3,nbyear)

date_ini = 19900101d
juld_ini = date2jul(date_ini)
IF basn EQ 'IOD'  THEN box = [40,110,-20,20]
path = '/net/adonis/usr/adonis/varclim/gslod/EXP_'+expn+'/SST/' & help, path
file = 'SST_1990-2009.nc'

IF expn EQ 'ERA-I' THEN BEGIN
  initncdf, path+file & domdef, box;, /memeindice
  sst = read_ncdf('SST', 0, 0, /timestep, filename=path+file, /nostruct) & help, sst
  mask = sst & mask[where(finite(sst) EQ 1)] = 1 & mask[where(finite(sst) EQ 0)] = !VALUES.F_NAN & help, mask
ENDIF
IF expn NE 'ERA-I' THEN BEGIN
  initncdf, maskfile & domdef, box;, /memeindice
  landmask = read_ncdf('LANDMASK',filename=maskfile,/nostruct) & help, landmask
  mask = landmask & mask[where(landmask EQ 0)] = 1 & mask[where(landmask EQ 1)] = !VALUES.F_NAN & help, mask
ENDIF

var = read_ncdf('SST', /allrecords, filename=path+file, /nostruct) & help, var
path = '/net/adonis/usr/adonis/varclim/gslod/EXP_'+expn+'/U_V_10/' & help, path
file = 'uv10_1990-2009.nc'
IF expn EQ 'ERA-I' THEN BEGIN
  initncdf, path+file & domdef, box;, /memeindice
ENDIF
var1 = read_ncdf('U10', /allrecords, filename=path+file, /nostruct) & help, var1
var2 = read_ncdf('V10', /allrecords, filename=path+file, /nostruct) & help, var2
print, gphit[firstxt,firstyt], gphit[lastxt,lastyt]


IF expn EQ 'ERA-I' THEN initncdf, path+file,XMINMESH=firstxt,YMINMESH=firstyt,XMAXMESH=lastxt,YMAXMESH=lastyt $
ELSE initncdf, maskfile,XMINMESH=firstxt,YMINMESH=firstyt,XMAXMESH=lastxt,YMAXMESH=lastyt
print, gphit[firstxt,firstyt], gphit[lastxt,lastyt]
stop

nbt = n_elements(var[0,0,*])
juld = juld_ini + dindgen(nbt)/4 & help, juld
date = jul2date(juld) & help, date
iok  = where(date GE period[0] AND date LE period[1], cntok) & help, iok
var  = var[*,*,iok] & help, var

var_monthly = fltarr(n_elements(var[*,0,0]),n_elements(var[0,*,0]),nbmonth)
FOR i = 0, nbmonth-1 DO BEGIN
  y = floor(i/12)
  m = i mod 12
  iave = where(date[iok] GE (long(period[0]/10000)+y)*10000+(m+1)*100 AND date[iok] LT (long(period[0]/10000)+y)*10000+(m+2)*100, cntave)
;  print, (long(period[0]/10000)+y)*10000+(m+1)*100, (long(period[0]/10000)+y)*10000+(m+2)*100, cntave
  IF iave[0] NE -1 THEN var_monthly[*,*,i] = m_mean(var[*,*,iave], dim=3, /nan) ELSE STOP
ENDFOR

var_3m = fltarr(n_elements(var[*,0,0]),n_elements(var[0,*,0]),nbyear)
FOR i = 0, nbyear-1 DO BEGIN
  deb = max([i*12+9-1,0]); & help, deb
  fin = min([i*12+11-1,nbyear*12+11-1]); & help, fin
  var_3m[*,*,i] = m_mean(var_monthly[*,*,deb:fin], dim=3, /nan)
ENDFOR

mask = reform(reform(mask, n_elements(var_3m[*,0,0])*n_elements(var_3m[0,*,0]))#replicate(1,n_elements(var_3m[0,0,*])), n_elements(var_3m[*,0,0]), n_elements(var_3m[0,*,0]), n_elements(var_3m[0,0,*]))
var_3m = var_3m * mask

var_clim = m_mean(var_3m[*,*,*], dim=3, /nan)
year_monthly = long(period[0]/10000) + findgen(nbyear*12)/12
year = long(period[0]/10000) + findgen(nbyear)

FOR i = 0, nbyear-1 DO BEGIN
  inw[e,i] = moyenne(var_3m[*,*,i], 'xy', /nan , box=[60, 80,-10,10]) 
  ine[e,i] = moyenne(var_3m[*,*,i], 'xy', /nan , box=[90,110,-10, 0])
ENDFOR

ins[e,*] = inw[e,*] - ine[e,*]
ano[e,*] = ins[e,*] - mean(ins[e,*])
iodp = where(ano[e,*] GT  0.75*stddev(ano[e,*]))
iodm = where(ano[e,*] LT -0.75*stddev(ano[e,*]))
print, iodp
print, 'IOD+ :', year[iodp]
print, iodm
print, 'IOD- :', year[iodm]

;plt, m_mean(var_3m[*,*,iodp], dim=3, /nan)-m_mean(var_3m[*,*,iodm], dim=3, /nan), $
;/nocont, real=2, min=-2.5, max=2.5, lct=64

nbt = n_elements(var1[0,0,*])
IF expn EQ 'ERA-I' THEN juld = time
IF expn NE 'ERA-I' THEN juld = juld_ini + dindgen(nbt)/4
help, juld
date = jul2date(juld) & help, date
iok  = where(date GE period[0] AND date LE period[1], cntok) & help, iok
var1  = var1[*,*,iok] & help, var1
var2  = var2[*,*,iok] & help, var2

IF expn EQ 'ERA-I' THEN BEGIN
  var1_monthly = var1
  var2_monthly = var2
ENDIF ELSE BEGIN
  var1_monthly = fltarr(n_elements(var1[*,0,0]),n_elements(var1[0,*,0]),nbmonth)
  var2_monthly = fltarr(n_elements(var2[*,0,0]),n_elements(var2[0,*,0]),nbmonth)
  FOR i = 0, nbmonth-1 DO BEGIN
    y = floor(i/12)
    m = i mod 12
    iave = where(date[iok] GE (long(period[0]/10000)+y)*10000+(m+1)*100 AND date[iok] LT (long(period[0]/10000)+y)*10000+(m+2)*100, cntave)
    IF iave[0] NE -1 THEN var1_monthly[*,*,i] = m_mean(var1[*,*,iave], dim=3, /nan) ELSE STOP
    IF iave[0] NE -1 THEN var2_monthly[*,*,i] = m_mean(var2[*,*,iave], dim=3, /nan) ELSE STOP
  ENDFOR
ENDELSE

var1_3m = fltarr(n_elements(var1[*,0,0]),n_elements(var1[0,*,0]),nbyear)
var2_3m = fltarr(n_elements(var2[*,0,0]),n_elements(var2[0,*,0]),nbyear)
FOR i = 0, nbyear-1 DO BEGIN
  deb = max([i*12+9-1,0]); & help, deb
  fin = min([i*12+11-1,nbyear*12+11-1]); & help, fin
  var1_3m[*,*,i] = m_mean(var1_monthly[*,*,deb:fin], dim=3, /nan)
  var2_3m[*,*,i] = m_mean(var2_monthly[*,*,deb:fin], dim=3, /nan)
ENDFOR
var1_3m = var1_3m * mask
var2_3m = var2_3m * mask

var1_clim = m_mean(var1_3m[*,*,*], dim=3, /nan)
var2_clim = m_mean(var2_3m[*,*,*], dim=3, /nan)
year_monthly = long(period[0]/10000) + findgen(nbyear*12)/12
year = long(period[0]/10000) + findgen(nbyear)

vartmp1 = m_mean(var1_3m[*,*,iodp], dim=3, /nan)-m_mean(var1_3m[*,*,iodm], dim=3, /nan)
vartmp2 = m_mean(var2_3m[*,*,iodp], dim=3, /nan)-m_mean(var2_3m[*,*,iodm], dim=3, /nan)

stop
plt, m_mean(var_3m[*,*,iodp], dim=3, /nan)-m_mean(var_3m[*,*,iodm], dim=3, /nan), gridtype='T', $
/nocont, real=2, min=-2.5, max=2.5, lct=64
IF expn EQ 'ERA-I' THEN ajoutvect, {u:{a:vartmp1, g:'T'}, v:{a:vartmp2, g:'T'}}, unvectsur = [7,7], normeref=5, cmref=1 
IF expn NE 'ERA-I' THEN ajoutvect, {u:{a:vartmp1, g:'T'}, v:{a:vartmp2, g:'T'}}, unvectsur = [20,20], normeref=5, cmref=1
;saveimage, 'IOD_SST_UV10_ANOM_'+expn+'.gif'
stop
ENDFOR; experience

stop
END
