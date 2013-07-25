PRO plot_RAIN_interannuel
; plot 2D clim rainfalls + (E)ISMR time series
@common


; params
var_list = ['RAIN']
exp_list = ['GPCP_MONTHLY','COUPLED_SW2_KF','COUPLED_SW2_BMJ']
period   = [19900101d,20091231d]
bas_list = ['AISMR','EISMR']

maskfile = '/usr/home/gslod/IDL/MASK_WRF_IND4_GRIDT.nc'

nbyear   = long(period[1]/10000) - long(period[0]/10000) + 1 & help, nbyear
nbmonth  = 12-(long(period[0]/100) - long(period[0]/10000)*100)+1 + (nbyear-2)*12 + (long(period[1]/100) - long(period[1]/10000)*100) & help, nbmonth
year_monthly = long(period[0]/10000) + findgen(nbyear*12)/12
year = long(period[0]/10000) + findgen(nbyear)

write_ps = 0 ; ecriture figures ps
write_netcdf = 0 ; ecriture fichier netcdf mensuel


; nino34 stuff
nino34_box = [190,240,-5,5]
sst_file = 'SST_GLOBAL_MONTHLY_1990-2009.nc'
sst_path = '/net/adonis/usr/adonis/varclim/gslod/EXP_ERA-I/SST_MONTHLY_GLOBAL_075/'
print, 'init era-i global monthly sst file...'
initncdf, sst_path+sst_file & domdef, nino34_box
sst_nino34 = read_ncdf('sstk', /allrecords, filename=sst_path+sst_file, /nostruct)
juld_nino34 = time
date_nino34 = jul2date(juld_nino34)
nbyear_nino34 = long(floor(date_nino34[jpt-1]/10000.)) - long(floor(date_nino34[0]/10000.)) + 1
year_nino34 = long(floor(date_nino34[0]/10000.)) + indgen(nbyear_nino34)
glamt_nino34 = glamt[firstxt:lastxt,firstyt:lastyt]
gphit_nino34 = gphit[firstxt:lastxt,firstyt:lastyt]
mask_nino34 = sst_nino34[*,*,0] * 0. + 1.; & mask_nino34[where(finite(sst_nino34[*,*,0]) EQ 0)] = !VALUES.F_NAN
help, sst_nino34, juld_nino34, date_nino34, nbyear_nino34, glamt_nino34, gphit_nino34, mask_nino34

sst4m_nino34 = fltarr(nxt,nyt,nbyear_nino34)
FOR i = 0, nbyear_nino34-1 DO BEGIN
  deb = max([i*12+6-1,0])
  fin = min([i*12+9-1,nbyear_nino34*12+9-1])
  sst4m_nino34[*,*,i] = m_mean(sst_nino34[*,*,deb:fin], dim=3, /nan)
ENDFOR
help, sst4m_nino34

sst4m1D_nino34 = m_mean(m_mean(sst4m_nino34, dim=1, /nan), dim=1, /nan) & help, sst4m1D_nino34
ind_nino34 = sst4m1D_nino34 - mean(sst4m1D_nino34,/nan) & help, ind_nino34

;splot, year_nino34, ind_nino34, yrange=[-2.,2.], ytitle="SST anomaly (K)", xtitle="years", title="NINO3.4 JJAS ERA-I SST ANOMALY"
;oplot, year_nino34, ind_nino34*0.
;saveimage, 'nino34_jjas_erai_sst_anomaly.gif'


; loops
FOR v = 0, n_elements(var_list)-1 DO BEGIN
varn = var_list[v] & help, varn
FOR b = 0, n_elements(bas_list)-1 DO BEGIN
basn = bas_list[b] & help, basn
FOR e = 0, n_elements(exp_list)-1 DO BEGIN
expn = exp_list[e] & help, expn
IF e EQ 0 THEN ins3m = fltarr(4,nbyear)
IF e EQ 0 THEN ins4m = fltarr(4,nbyear)


; path + file
path = '/net/adonis/usr/adonis/varclim/gslod/EXP_'+expn+'/' & help, path
file = varn+'_DAILY_1990-2009.nc'
IF expn EQ 'GPCP' THEN file = varn+'_DAILY_1996-2009.nc'
IF expn EQ 'GPCP_MONTHLY' THEN file = varn+'_MONTHLY_1979-2010.nc'
IF expn EQ 'GPCP_MONTHLY' THEN path = '/net/adonis/usr/adonis/varclim/gslod/EXP_GPCP/' & help, path
IF expn EQ 'TRMM3B43' THEN file = varn+'_MONTHLY_1998-2009.nc'
help, file


; date + box
date_ini = 19900101d
IF expn EQ 'GPCP' THEN date_ini = 19961001d
IF expn EQ 'TRMM3B43' THEN date_ini = 19980101d
IF expn EQ 'GPCP_MONTHLY' THEN date_ini = 19790101d
help, date_ini
juld_ini = date2jul(date_ini)
IF basn EQ 'AISMR' THEN box = [70, 95,  5,25]
IF basn EQ 'EISMR' THEN box = [70,110,  5,25]
IF basn EQ  'IO'   THEN box = [30,130,-25,25]


; grid + mask
print, 'init mask'
initncdf, maskfile & domdef, box
landmask = read_ncdf('LANDMASK',filename=maskfile,/nostruct)
mask = landmask & mask[where(landmask EQ 0)] = 1 & mask[where(landmask EQ 1)] = !VALUES.F_NAN & help, mask
IF expn EQ 'GPCP' OR expn EQ 'GPCP_MONTHLY' OR expn EQ 'TRMM3B43' THEN BEGIN
  maskin = mask
  lonin = glamt[firstxt:lastxt,firstyt:lastyt]
  latin = gphit[firstxt:lastxt,firstyt:lastyt]
  print, 'init file'
  initncdf, path+file & domdef, box
  mask = fromreg('bilinear',maskin,lonin,latin,glamt[firstxt:lastxt,firstyt:lastyt],gphit[firstxt:lastxt,firstyt:lastyt]) & help, mask
ENDIF


; read var
IF expn EQ 'GPCP' OR expn EQ 'GPCP_MONTHLY' THEN BEGIN
  var = read_ncdf(varn, /allrecords, filename=path+file, /nostruct) 
ENDIF ELSE IF expn EQ 'TRMM3B43' THEN BEGIN
  var = read_ncdf(varn, /allrecords, filename=path+file, /nostruct) * 24. ; mm/h -> mm/day
ENDIF ELSE BEGIN
  var = read_ncdf('RAIN', /allrecords, filename=path+file, /nostruct); * 4. ; mm/6h -> mm/day
;  var1 = read_ncdf('RAINC', /allrecords, filename=path+file, /nostruct) * 4. ; mm/6h -> mm/day
;  var2 = read_ncdf('RAINNC', /allrecords, filename=path+file, /nostruct) * 4. ; mm/6h -> mm/day
;  var  = var1 + var2
;  var[where(var GT 100.)] = !VALUES.F_NAN
ENDELSE
help, var, time


; def time axis
IF expn NE 'GPCP' AND expn NE 'GPCP_MONTHLY' AND expn NE 'TRMM3B43' THEN BEGIN
  date_ini = 19900101d & help, date_ini
  juld_ini = date2jul(date_ini)
  nbt = n_elements(var[0,0,*])
  juld = juld_ini + dindgen(nbt);/4.
ENDIF
IF expn EQ 'GPCP' OR expn EQ 'GPCP_MONTHLY' OR expn EQ 'TRMM3B43' THEN juld = time
IF expn EQ 'TRMM3B43' THEN year_ini = 1998 ELSE year_ini = 1990
year_end = 2009
date = jul2date(juld)
print, date[0], date[n_elements(date)-1]; & stop

;IF expn EQ 'COUPLED_SW2_KF' AND varn EQ 'RAIN' THEN BEGIN
;  iya = where(date GE 19990101d AND date LE 19991231d, cntya)
;  var[*,*,iya] = var[*,*,iya] - shift(var[*,*,iya],0,0,1) & var[*,*,iya[0]] = 0.
;ENDIF


; selection periode
iok  = where(date GE period[0] AND date LE period[1], cntok)
print, date[iok[0]], date[iok[cntok-1]]; & stop
var  = var[*,*,iok] & help, var


; maskage
IF basn EQ 'AISMR' THEN BEGIN
  mask = reform(reform(mask, nxt*nyt)#replicate(1,cntok), nxt, nyt, cntok) & help, mask
  var[where(finite(mask) EQ 1)] = !VALUES.F_NAN
ENDIF


; monthly average
IF expn NE 'GPCP_MONTHLY' AND expn NE 'TRMM3B43' THEN BEGIN

  var_monthly = fltarr(n_elements(var[*,0,0]),n_elements(var[0,*,0]),nbmonth)
  date_monthly = dblarr(nbmonth)

  FOR i = 0, nbmonth-1 DO BEGIN
;    y = floor(i/12)
;    m = i mod 12
;    iave = where(date[iok] GE (long(period[0]/10000)+y)*10000+(m+1)*100 AND date[iok] LT (long(period[0]/10000)+y)*10000+(m+2)*100, cntave)
;    print, (long(period[0]/10000)+y)*10000+(m+1)*100, (long(period[0]/10000)+y)*10000+(m+2)*100, cntave
;    IF iave[0] NE -1 THEN var_monthly[*,*,i] = m_mean(var[*,*,iave], dim=3, /nan) ELSE STOP
    datemin = (long(date[0]/10000)+floor((i+0)/12.))*10000+(i mod 12 + 1) *100+1
    datemax = (long(date[0]/10000)+floor((i+1)/12.))*10000+((i+1) mod 12 + 1) *100+1
    iave = where(date GE datemin AND date LT datemax, cntave)
    print, datemin, datemax, n_elements(iave)
    IF iave[0] NE -1 THEN var_monthly[*,*,i] = m_mean(var[*,*,iave], dim=3, /nan) ELSE STOP
    date_monthly[i] = double(datemin)
  ENDFOR
ENDIF ELSE BEGIN & var_monthly = var & ENDELSE


; ecriture netcdf
IF write_netcdf THEN BEGIN
IF expn NE 'GPCP_MONTHLY' AND expn NE 'TRMM3B43' THEN BEGIN
  fid = NCDF_CREATE(path+'RAIN_MONTHLY_1990-2009.nc', /CLOBBER)
  xid = NCDF_DIMDEF(fid, 'lon', n_elements(glamt[firstxt:lastxt,0]))
  yid = NCDF_DIMDEF(fid, 'lat', n_elements(gphit[0,firstyt:lastyt]))
  tid = NCDF_DIMDEF(fid, 'time', /UNLIMITED)
  vid = NCDF_VARDEF(fid, varn, [xid, yid, tid], /FLOAT)
  lonid = NCDF_VARDEF(fid, 'lon', [xid, yid], /FLOAT)
  latid = NCDF_VARDEF(fid, 'lat', [xid, yid], /FLOAT)
  timid = NCDF_VARDEF(fid, 'time', [tid], /FLOAT)
  NCDF_CONTROL, fid, /ENDEF
  NCDF_VARPUT, fid, vid, var_monthly
  NCDF_VARPUT, fid, lonid, glamt[firstxt:lastxt,firstyt:lastyt]
  NCDF_VARPUT, fid, latid, gphit[firstxt:lastxt,firstyt:lastyt]
  NCDF_VARPUT, fid, timid, date_monthly
  NCDF_CLOSE, fid
ENDIF
ENDIF


ibeg = where(year EQ year_ini) & print, ibeg
iend = where(year EQ year_end) & print, iend

; JJA average
var_3m = fltarr(n_elements(var[*,0,0]),n_elements(var[0,*,0]), min([nbyear,year_end-year_ini+1]))
FOR i = 0, min([nbyear,year_end-year_ini+1])-1 DO BEGIN
  deb = max([i*12+6-1,0]) & help, deb
  fin = min([i*12+8-1,nbyear*12+8-1]) & help, fin
  var_3m[*,*,i] = m_mean(var_monthly[*,*,deb:fin], dim=3, /nan)
ENDFOR
help, var_3m

var_clim3m = m_mean(var_3m[*,*,*], dim=3, /nan) & help, var_clim3m
ind = m_mean(m_mean(var_3m, dim=1, /nan), dim=1, /nan)
ins3m[e,ibeg:iend] = ind


; JJAS average
var_4m = fltarr(n_elements(var[*,0,0]),n_elements(var[0,*,0]), min([nbyear,year_end-year_ini+1]))
FOR i = 0, min([nbyear,year_end-year_ini+1])-1 DO BEGIN
  deb = max([i*12+6-1,0]) & help, deb
  fin = min([i*12+9-1,nbyear*12+9-1]) & help, fin
  var_4m[*,*,i] = m_mean(var_monthly[*,*,deb:fin], dim=3, /nan)
ENDFOR
help, var_4m

var_clim4m = m_mean(var_4m[*,*,*], dim=3, /nan) & help, var_clim4m
ind = m_mean(m_mean(var_4m, dim=1, /nan), dim=1, /nan)
ins4m[e,ibeg:iend] = ind


;IF e EQ 0 THEN splot, year[ibeg:iend], ins3m[e,*], yrange=[4,10]
;oplot, year[ibeg:iend], ins4m[0,ibeg:iend], linestyle=2
;oplot, year[ibeg:iend], ins3m[1,ibeg:iend], color=50
;oplot, year[ibeg:iend], ins4m[1,ibeg:iend], color=50, linestyle=2


; plot 2D
;plt, var_clim, 0, 50, real=2, /nocont, title=expn+' JJA RAIN CLIMATOLOGY', subtitle='RAIN (mm/day)'
;saveimage, basn+'_'+expn+'_2D_JJA_CLIM.gif'
;stop


; plot 1D

;IF write_ps THEN openps, filename=basn+'_PRECIP_1990-2009.ps'
;IF write_ps THEN thc = 6 ELSE thc = 2

;IF basn EQ 'EISMR' THEN BEGIN
;IF e EQ 0 THEN BEGIN
;  splot, year, ind, xtitle='YEARS', ytitle=basn+' PRECIPITATION (mm/day)', charsize=1.2, thick=thc, $
;  ystyle=1, yrange=[0,16], title=basn+' INDEX', win=0
;  xyouts, 0.15, 0.35-e*0.025, expn, color=e*100, charsize=1.2, /normal
;  oplot, [1990,2009],[mean(ind[*]),mean(ind[*])], color=e*100, thick=thc
;ENDIF ELSE BEGIN
;  wset, 0
;  oplot, year, ind, color=e*100, thick=2, thick=thc
;  xyouts, 0.15, 0.35-e*0.025, expn, color=e*100, charsize=1.2, /normal
;  oplot, [1990,2009],[mean(ind[*]),mean(ind[*])], color=e*100, thick=thc
;ENDELSE
;IF e EQ 0 THEN BEGIN
;  splot, year, ind-mean(ind[*]), xtitle='YEARS', ytitle=basn+' PRECIPITATION ANOMALY (mm/day)', charsize=1.2, thick=thc, $
;  ystyle=1, yrange=[-1.4,1.4], title=basn+' INDEX', win=1
;  xyouts, 0.15, 0.35-e*0.025, expn, color=e*100, charsize=1.2, /normal
;  oplot, [1990,2009], [0,0], thick=thc
;ENDIF ELSE BEGIN
;  wset, 1
;  oplot, year, ind-mean(ind[*]), color=e*100, thick=thc
;  xyouts, 0.15, 0.35-e*0.025, expn, color=e*100, charsize=1.2, /normal
;ENDELSE
;ENDIF

ENDFOR; experience
;IF write_ps THEN closeps & stop


IF write_ps THEN openps, filename=basn+'_INDEX_1990-2009_JJAS.ps'
IF write_ps THEN thc = 6 ELSE thc = 2

ins = ins4m

splot, year, ins[0,*]-mean(ins[0,*],/nan), xtitle='YEARS', ytitle=basn+' INDEX', charsize=1.2, thick=thc, $
ystyle=1, yrange=[-2,2], title=basn+' INDEX', lct=33 & $
oplot, year,  0.75*stddev(ins[0,*]-mean(ins[0,*],/nan))+indgen(20)*0., line=2, color=0, thick=thc & $
oplot, year, -0.75*stddev(ins[0,*]-mean(ins[0,*],/nan))+indgen(20)*0., line=2, color=0, thick=thc & $
oplot, year, ins[1,*]-mean(ins[1,*],/nan), thick=thc, color=50 & $
oplot, year,  0.75*stddev(ins[1,*]-mean(ins[1,*],/nan))+indgen(20)*0., line=2, color=50, thick=thc & $
oplot, year, -0.75*stddev(ins[1,*]-mean(ins[1,*],/nan))+indgen(20)*0., line=2, color=50, thick=thc & $
oplot, year, ins[2,*]-mean(ins[2,*],/nan), thick=thc, color=210 & $
oplot, year,  0.75*stddev(ins[2,*]-mean(ins[2,*],/nan))+indgen(20)*0., line=2, color=210, thick=thc & $
oplot, year, -0.75*stddev(ins[2,*]-mean(ins[2,*],/nan))+indgen(20)*0., line=2, color=210, thick=thc & $
oplot, year, indgen(20)*0., thick=thc

xyouts, 0.35, 0.350-0*0.025, 'COR OBS-KF:'+strtrim(correlate(ins[0,*],ins[1,*]),2), color=0*100, charsize=1.2, /normal
xyouts, 0.35, 0.350-1*0.025, 'COR OBS-BMJ:'+strtrim(correlate(ins[0,*],ins[2,*]),2), color=0*100, charsize=1.2, /normal
xyouts, 0.35, 0.350-2*0.025, 'COR KF-BMJ:'+strtrim(correlate(ins[1,*],ins[2,*]),2), color=0*100, charsize=1.2, /normal
ind1 = where(year EQ year_nino34) & help, ind1
ind2 = where(year_nino34 EQ year) & help, ind2
STOP
xyouts, 0.35, 0.350-3*0.025, 'COR OBS-NINO34:'+strtrim(correlate(ins[0,in1],ind_nino34[ind2]),2), color=0*100, charsize=1.2, /normal

IF write_ps THEN closeps & stop


;xyouts, 0.15, 0.35-e*0.025, expn, color=e*100, charsize=1.2, /normal

;wset, 0
;xyouts, 0.35, 0.350-1*0.025, '(COR:'+strtrim(correlate(ins[0,*],ins[1,*]),2)+')', color=0*100, charsize=1.2, /normal
;xyouts, 0.35, 0.350-2*0.025, '(COR:'+strtrim(correlate(ins[0,*],ins[2,*]),2)+')', color=0*100, charsize=1.2, /normal
;xyouts, 0.55, 0.350-2*0.025, '(COR:'+strtrim(correlate(ins[1,*],ins[2,*]),2)+')', color=3*100, charsize=1.2, /normal
;IF basn EQ 'ISMR' THEN saveimage, 'ISMR_1990-2009.gif'
;IF basn EQ 'EISMR' THEN saveimage, 'EISMR_1990-2009.gif'

;wset, 1
;xyouts, 0.35, 0.325-1*0.025, '(COR:'+strtrim(correlate(ins[0,*],ins[1,*]),2)+')', color=0*100, charsize=1.2, /normal
;xyouts, 0.35, 0.325-2*0.025, '(COR:'+strtrim(correlate(ins[0,*],ins[2,*]),2)+')', color=0*100, charsize=1.2, /normal
;xyouts, 0.55, 0.325-2*0.025, '(COR:'+strtrim(correlate(ins[1,*],ins[2,*]),2)+')', color=3*100, charsize=1.2, /normal
;IF basn EQ 'ISMR' THEN saveimage, 'ISMR_ANO_1990-2009.gif'
;IF basn EQ 'EISMR' THEN saveimage, 'EISMR_ANO_1990-2009.gif'

ENDFOR; indice
ENDFOR; variable

stop
END
