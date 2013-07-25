PRO plot_sst_tc
; plot IBTRACS TC tracks & intensity over TMI-AMSRE SST
@common


; params
varn     = 'SST'  & help, varn
expn     = 'OBS'  & help, expn
namecyc  = 'IVAN' & help, namecyc
yearcyc  = 2008   & help, yearcyc
box      = [45, 70,-25, -5]
write_ps = 1
maskfile = '/usr/home/gslod/IDL/MASK_WRF_IND4_GRIDT.nc'


;--------------------------------------------------------------------------------------


; IBTRACS
@read_ibtracs
cycind = intersect(where(namecycn EQ namecyc),where(yearcycn EQ yearcyc))
IF n_elements(cycind) NE 1. THEN STOP
latcyc = latcycn[cycind,*] & help, latcyc
loncyc = loncycn[cycind,*] & help, loncyc
uv10cyc = uv10cycn[cycind,*] & help, uv10cyc
mslpcyc = mslpcycn[cycind,*] & help, mslpcyc
datecyc = datecycn[cycind,*] & help, datecyc


; obs definition
IF varn EQ 'SST'  AND expn EQ 'OBS' THEN expn = 'TMI-AMSRE'
IF varn EQ 'SSH'  AND expn EQ 'OBS' THEN expn = 'AVISO'
IF varn EQ 'RAIN' AND expn EQ 'OBS' THEN expn = 'TRMM3B43'
IF varn EQ 'UV10' AND expn EQ 'OBS' THEN expn = 'QSCAT'
help, expn


; path + file
path = '/net/adonis/usr/adonis/varclim/gslod/EXP_'+expn+'/'
IF expn EQ 'TROPFLUX'     THEN file = varn+'_DAILY_1989-2009.nc'
IF expn EQ 'TMI-AMSRE'    THEN file = varn+'_DAILY_1998-2009.nc'
IF expn EQ 'TRMM3B43'     THEN file = varn+'_MONTHLY_1998-2009.nc'
IF expn EQ 'CCMP'         THEN file = varn+'_MONTHLY_1990-2009.nc'
IF expn EQ 'QSCAT'        THEN file = varn+'_MONTHLY_2000-2009.nc'
IF expn EQ 'NOAA'         THEN file = varn+'_DAILY_1974-2011.nc'
IF expn EQ 'AVISO'        THEN file = varn+'_WEEKLY_19921014-20091230.nc'
IF expn EQ 'GPCP'         THEN file = varn+'_DAILY_1996-2009.nc'
help, file


; date_ini
date_ini = 19900101d 
IF expn EQ 'TROPFLUX'  THEN date_ini = 19890101d
IF expn EQ 'TMI-AMSRE' THEN date_ini = 19980101d
IF expn EQ 'TRMM3B43'  THEN date_ini = 19980101d
IF expn EQ 'QSCAT'     THEN date_ini = 20000101d
IF expn EQ 'NOAA'      THEN date_ini = 19740601d
IF expn EQ 'AVISO'     THEN date_ini = 19921014d
IF expn EQ 'GPCP'      THEN date_ini = 19960101d
juld_ini = date2jul(date_ini)
help, date_ini


; date_end
date_end = 20091231.75d
help, date_end


; initncdf + domdef + mask + definition grille
IF expn EQ 'NOAA' OR expn EQ 'TRMM3B43' OR expn EQ 'GPCP' THEN BEGIN
  initncdf, maskfile & domdef, box
  landmask = read_ncdf('LANDMASK',filename=maskfile,/nostruct) & help, landmask
  mask = landmask & mask[where(landmask EQ 0)] = 1. & mask[where(landmask EQ 1)] = !VALUES.F_NAN & help, mask
  glamt_old = glamt[firstxt:lastxt,firstyt:lastyt]
  gphit_old = gphit[firstxt:lastxt,firstyt:lastyt] 
  initncdf, path+file & domdef, box
  glamt_new = glamt[firstxt:lastxt,firstyt:lastyt]
  gphit_new = gphit[firstxt:lastxt,firstyt:lastyt]
  mask = fromreg('bilinear',mask,glamt_old,gphit_old,glamt_new,gphit_new) & help, mask
  initncdf, path+file,XMINMESH=firstxt,YMINMESH=firstyt,XMAXMESH=lastxt,YMAXMESH=lastyt
ENDIF

IF expn EQ 'TROPFLUX' OR expn EQ 'CCMP' OR expn EQ 'QSCAT' OR expn EQ 'TMI-AMSRE' THEN BEGIN
  initncdf, path+file & domdef, box
  initncdf, path+file,XMINMESH=firstxt,YMINMESH=firstyt,XMAXMESH=lastxt,YMAXMESH=lastyt
ENDIF

IF expn EQ 'ERA-I' THEN BEGIN
  initncdf, path+file & domdef, box
  initncdf, path+file,XMINMESH=firstxt,YMINMESH=firstyt,XMAXMESH=lastxt,YMAXMESH=lastyt
  sst = read_ncdf('SST', 0, 0, /timestep, filename='/net/adonis/usr/adonis/varclim/gslod/EXP_ERA-I/SST_1990-1991.nc', /nostruct) & help, sst
  mask[where(finite(sst) EQ 1)] = 1 & mask[where(finite(sst) EQ 0)] = !VALUES.F_NAN & help, mask
ENDIF

IF varn EQ 'SSH' THEN BEGIN
  initncdf, path+file & domdef, box
  initncdf, path+file,XMINMESH=firstxt,YMINMESH=firstyt,XMAXMESH=lastxt,YMAXMESH=lastyt
ENDIF


; lecture data+mask
IF varn EQ 'SSH' THEN BEGIN
  var = read_ncdf('zos', /allrecords, filename=path+file, /nostruct)
  IF expn NE 'AVISO' THEN var = var * 100. ; m -> cm
  IF expn EQ 'AVISO' THEN BEGIN
    mask = var[*,*,0] * 0. + 1. & mask[where(finite(var[*,*,0]) EQ 0.)] = !VALUES.F_NAN
  ENDIF
  IF expn NE 'AVISO' THEN BEGIN
    mask = var[*,*,0] * 0. + 1. & mask[where(var[*,*,0] EQ 0.)] = !VALUES.F_NAN
  ENDIF
ENDIF

IF varn EQ 'UV10' THEN BEGIN
  IF expn EQ 'TROPFLUX' THEN var = read_ncdf(varn, /allrecords, filename=path+file, /nostruct)
  IF expn EQ 'QSCAT' THEN BEGIN
    var = read_ncdf('windspd', /allrecords, filename=path+file, /nostruct)
    dir = read_ncdf('winddir', /allrecords, filename=path+file, /nostruct) * !PI / 180.
    var1 = var * sin(dir)
    var2 = var * cos(dir)
    mask = var[*,*,0] * 0. + 1. & mask[where(finite(var[*,*,0]) EQ 0.)] = !VALUES.F_NAN
  ENDIF
  IF expn EQ 'CCMP'     THEN var = read_ncdf('wspd', /allrecords, filename=path+file, /nostruct)
ENDIF

IF expn EQ 'TRMM3B43' THEN var = read_ncdf(varn, /allrecords, filename=path+file, /nostruct) * 24. ; mm/h -> mm/d
IF expn EQ 'GPCP'     THEN var = read_ncdf(varn, /allrecords, filename=path+file, /nostruct)

IF expn EQ 'TMI-AMSRE' THEN BEGIN
  var  = read_ncdf(varn, /allrecords, filename=path+file, /nostruct)
  mask = fltarr(n_elements(var[*,0,0]),n_elements(var[0,*,0])) + 1.
  mask[where(var[*,*,0] EQ 35.2500)] = !VALUES.F_NAN
ENDIF

IF varn EQ 'OLR' THEN BEGIN
  var = read_ncdf('OLR', /allrecords, filename=path+file, /nostruct)
ENDIF
help, var


; creation axe temps obs
nbt  = n_elements(var[0,0,*])
juld = juld_ini + dindgen(nbt)
IF expn EQ 'GPCP' THEN juld = time
IF expn EQ 'TRMM3B43' THEN juld = time
IF expn EQ 'CCMP' THEN juld = time
IF expn EQ 'NOAA' THEN juld = time
IF expn EQ 'AVISO' THEN juld = juld_ini + dindgen(nbt)*7.
IF expn EQ 'QSCAT' THEN BEGIN
  year = floor(floor(date_ini / 10000) + indgen(nbt)/12)
  month = indgen(nbt) mod 12 + 1
  day = lonarr(nbt) + 1
  date = day + 100 * month + 10000 * year
  juld = date2jul(date)
ENDIF

date    = jul2date(juld, HOUR=hour, DAY=day, MONTH=month, YEAR=year) & help, date
period  = [max([date_ini,datecyc[0]]),min([date_end,datecyc[n_elements(where(finite(datecyc) EQ 1))-1]])]
IF expn EQ 'AVISO' THEN period  = [19930101d,min([date_end,20091231.75d])]
print, 'PERIOD = ', period
iok     = where(date GE period[0] AND date LE period[1], cntok) & help, iok
var     = var[*,*,iok] & help, var
date    = date[iok] & year = year[iok] & month = month[iok] & day = day[iok] & hour = hour[iok] & juld = juld[iok] & time = juld


; PLOTS

set_plot, 'Z'
device, decomposed=0;, retain=0
lct,33
initncdf, path+file & domdef, box

FOR i = 0, n_elements(where(finite(date) EQ 1))-1 DO BEGIN

;  print, date[i], format='(F11.2)'
  IF write_ps THEN openps, filename='SST_'+strtrim(long(date[i]))+'.ps'

  plt, smooth(var[*,*,i],2,/nan), min=23, max=30, int=0.25, /realcont, lct=64, /nocont, $
  title=expn+' SST - '+strtrim(long(date[i]),2), xtitle='', ytitle='', format='(F4.1)', /carte

  lon = smooth(loncyc,2,/nan)
  lat = smooth(latcyc,2,/nan)

  FOR j = 2, n_elements(where(datecyc LE date[i])),2 DO BEGIN
;    print, datecyc[j], format='(F11.2)'
    oplot, [lon[j-2],lon[j]] , [lat[j-2],lat[j]], $
    color=0, thick=3
    oplot, [lon[j-2],lon[j]] , [lat[j-2],lat[j]], psym=7, symsize=2.
  ENDFOR

  IF NOT write_ps AND i LT 10 THEN saveimage, 'SST_IVAN_0'+strtrim(i,2)+'.gif'
  IF NOT write_ps AND i GE 10 THEN saveimage, 'SST_IVAN_'+strtrim(i,2)+'.gif'
  IF write_ps THEN closeps

  ENDFOR

STOP


STOP
END
