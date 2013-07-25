PRO convert_nemo_daily_to_monthly
@common

expn   = 'COUPLED_SW2_BMJ'

; init
initncdf, '/usr/home/gslod/IDL/MASK_NEMO_IND4_GRIDT.nc', /fullcgrid

; var + mask
mask = read_ncdf('mask',filename='/usr/home/gslod/IDL/MASK_NEMO_IND4_GRIDT.nc', /nostruct, grid='T')
path = '/net/adonis/usr/adonis/varclim/gslod/EXP_'+expn+'/' & help, path
file = 'TAU_1990-2009.nc'
var1 = read_ncdf('tauuo', /allrecords, filename=path+file, /nostruct, grid='U')
var2 = read_ncdf('tauvo', /allrecords, filename=path+file, /nostruct, grid='V')
help, var1, var2

; temps
date_ini = 19900101d
juld_ini = date2jul(date_ini)
nbt  = n_elements(var1[0,0,*])
juld = juld_ini + dindgen(nbt)
date = jul2date(juld) & help, date
nbyear  = long(date[nbt-1]/10000) - long(date[0]/10000) + 1
nbmonth = nbyear * 12 
help, nbyear, nbmonth

; monthly
var1_monthly = fltarr(n_elements(var1[*,0,0]),n_elements(var1[0,*,0]),nbmonth)
var2_monthly = var1_monthly
var3_monthly = var1_monthly
date_monthly = dblarr(nbmonth)
FOR i = 0, nbmonth-1 DO BEGIN
  datemin = (long(date[0]/10000)+floor((i+0)/12.))*10000+(i mod 12 + 1) *100+1
  datemax = (long(date[0]/10000)+floor((i+1)/12.))*10000+((i+1) mod 12 + 1) *100+1
  iave = where(date GE datemin AND date LT datemax, cntave)
  IF iave[0] NE -1 THEN var1_monthly[*,*,i] = m_mean(var1[*,*,iave], dim=3, /nan)*mask ELSE STOP
  IF iave[0] NE -1 THEN var2_monthly[*,*,i] = m_mean(var2[*,*,iave], dim=3, /nan)*mask ELSE STOP
  var3_monthly[*,*,i] = curl(var1_monthly[*,*,i],var2_monthly[*,*,i])
  date_monthly[i] = double(datemin)
ENDFOR
juld_monthly = date2jul(date_monthly, month=month_monthly)
help, var1_monthly, var2_monthly, var3_monthly

; seasonal cycle
var1_sc = fltarr(n_elements(var1[*,0,0]),n_elements(var1[0,*,0]),12)
var2_sc = var1_sc
var3_sc = var1_sc
FOR i = 0, 12-1 DO BEGIN
  iave = where(month_monthly EQ i+1) & help, iave
  var1_sc[*,*,i] = m_mean(var1_monthly[*,*,iave],dim=3,/nan)
  var2_sc[*,*,i] = m_mean(var2_monthly[*,*,iave],dim=3,/nan)
  var3_sc[*,*,i] = m_mean(var3_monthly[*,*,iave],dim=3,/nan)
ENDFOR
help, var1_sc, var2_sc, var3_sc 

; ecriture netcdf
fid = NCDF_CREATE(path+'TAU_MONTHLY_1990-2009.nc', /CLOBBER)
xid = NCDF_DIMDEF(fid, 'nav_lon', n_elements(glamt[*,0]))
yid = NCDF_DIMDEF(fid, 'nav_lat', n_elements(gphit[0,*]))
tid = NCDF_DIMDEF(fid, 'time', /UNLIMITED)
vid1 = NCDF_VARDEF(fid, 'taux', [xid, yid, tid], /FLOAT)
vid2 = NCDF_VARDEF(fid, 'tauy', [xid, yid, tid], /FLOAT)
vid3 = NCDF_VARDEF(fid, 'rot_tau', [xid, yid, tid], /DOUBLE)
lonid = NCDF_VARDEF(fid, 'nav_lon', [xid, yid], /FLOAT)
latid = NCDF_VARDEF(fid, 'nav_lat', [xid, yid], /FLOAT)
timid = NCDF_VARDEF(fid, 'time', [tid], /FLOAT)
NCDF_CONTROL, fid, /ENDEF
NCDF_VARPUT, fid, vid1, var1_monthly
NCDF_VARPUT, fid, vid2, var2_monthly
NCDF_VARPUT, fid, vid3, var3_monthly
NCDF_VARPUT, fid, lonid, glamt
NCDF_VARPUT, fid, latid, gphit
NCDF_VARPUT, fid, timid, date_monthly
NCDF_CLOSE, fid

fid = NCDF_CREATE(path+'TAU_CLIM_1990-2009.nc', /CLOBBER)
xid = NCDF_DIMDEF(fid, 'nav_lon', n_elements(glamt[*,0]))
yid = NCDF_DIMDEF(fid, 'nav_lat', n_elements(gphit[0,*]))
tid = NCDF_DIMDEF(fid, 'time', /UNLIMITED)
vid1 = NCDF_VARDEF(fid, 'taux', [xid, yid, tid], /FLOAT)
vid2 = NCDF_VARDEF(fid, 'tauy', [xid, yid, tid], /FLOAT)
vid3 = NCDF_VARDEF(fid, 'rot_tau', [xid, yid, tid], /DOUBLE)
lonid = NCDF_VARDEF(fid, 'nav_lon', [xid, yid], /FLOAT)
latid = NCDF_VARDEF(fid, 'nav_lat', [xid, yid], /FLOAT)
NCDF_CONTROL, fid, /ENDEF
NCDF_VARPUT, fid, vid1, var1_sc
NCDF_VARPUT, fid, vid2, var2_sc
NCDF_VARPUT, fid, vid3, var3_sc
NCDF_VARPUT, fid, lonid, glamt
NCDF_VARPUT, fid, latid, gphit
NCDF_CLOSE, fid


SET_PLOT, 'X'
DEVICE, decomposed=0, retain=0 & lct,33

STOP
END
