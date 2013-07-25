PRO find_and_plot_1D_IOD
; identifie les IOD a partir des SST + plot serie temporelle IOD index + correlations 
@all_cm

var_list = ['SST']
exp_list = ['ERA-I','COUPLED_SW2_KF','COUPLED_SW2_BMJ']
period   = [19900101d,20091231d]
bas_list = ['IOD']
write_ps = 0 ; ecriture figures ps

;--------------------------------------------------------------------------------------

nbyear   = long(period[1]/10000) - long(period[0]/10000) + 1
nbmonth  = 12-(long(period[0]/100) - long(period[0]/10000)*100)+1 + (nbyear-2)*12 + (long(period[1]/100) - long(period[1]/10000)*100)
maskfile = '/usr/home/gslod/IDL/MASK_WRF_IND4_GRIDT.nc'
CLOSE, 1


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
  deb = max([i*12+9 -1,0])
  fin = min([i*12+11-1,nbyear_nino34*12+11-1])
  sst4m_nino34[*,*,i] = m_mean(sst_nino34[*,*,deb:fin], dim=3, /nan)
ENDFOR
help, sst4m_nino34
sst4m1D_nino34 = m_mean(m_mean(sst4m_nino34, dim=1, /nan), dim=1, /nan) & help, sst4m1D_nino34
ind_nino34 = sst4m1D_nino34 - mean(sst4m1D_nino34,/nan) & help, ind_nino34


; loops
FOR v = 0, n_elements(var_list)-1 DO BEGIN
varn = var_list[v] & help, varn
FOR e = 0, n_elements(exp_list)-1 DO BEGIN
expn = exp_list[e] & help, expn
OPENW, 1, 'IOD_'+expn+'.txt'
IF e EQ 0 THEN ins = fltarr(3,nbyear)
IF e EQ 0 THEN inw = fltarr(3,nbyear)
IF e EQ 0 THEN ine = fltarr(3,nbyear)
IF e EQ 0 THEN ano = fltarr(3,nbyear)
FOR b = 0, n_elements(bas_list)-1 DO BEGIN
basn = bas_list[b] & help, basn

path = '/net/adonis/usr/adonis/varclim/gslod/EXP_'+expn+'/' & help, path
file = varn+'_DAILY_1990-2009.nc'
date_ini = 19900101d
juld_ini = date2jul(date_ini)

IF basn EQ 'IOD'  THEN box = [40,110,-20,20]

IF expn EQ 'ERA-I' THEN BEGIN
  initncdf, path+file & domdef, box
  sst = read_ncdf('SST', 0, 0, /timestep, filename=path+file, /nostruct) & help, sst
  mask = sst & mask[where(finite(sst) EQ 1)] = 1 & mask[where(finite(sst) EQ 0)] = !VALUES.F_NAN & help, mask
ENDIF
IF expn NE 'ERA-I' THEN BEGIN
  initncdf, maskfile & domdef, box
  landmask = read_ncdf('LANDMASK',filename=maskfile,/nostruct) & help, landmask
  mask = landmask & mask[where(landmask EQ 0)] = 1 & mask[where(landmask EQ 1)] = !VALUES.F_NAN & help, mask
ENDIF

var = read_ncdf(varn, /allrecords, filename=path+file, /nostruct) & help, var
print, gphit[firstxt,firstyt], gphit[lastxt,lastyt]

IF expn EQ 'ERA-I' THEN initncdf, path+file,XMINMESH=firstxt,YMINMESH=firstyt-5,XMAXMESH=lastxt,YMAXMESH=lastyt-5 $
ELSE initncdf, maskfile,XMINMESH=firstxt,YMINMESH=firstyt,XMAXMESH=lastxt,YMAXMESH=lastyt
;print, gphit[firstxt,firstyt], gphit[lastxt,lastyt] & stop


;glamt = glamt[firstxt:lastxt,firstyt:lastyt]
;gphit = gphit[firstxt:lastxt,firstyt:lastyt]
;firstxt = 0 & firstyt = 0
;lastxt = n_elements(glamt[*,0])-1
;lastyt = n_elements(gphit[0,*])-1
;jpi = n_elements(glamt[*,0]) & jpj = n_elements(gphit[0,*])

nbt = n_elements(var[0,0,*])
juld = juld_ini + dindgen(nbt) & help, juld
date = jul2date(juld) & help, date
iok  = where(date GE period[0] AND date LE period[1], cntok) & help, iok
var  = var[*,*,iok] & help, var
IF expn NE 'ERA-I' THEN BEGIN
  mask = reform(reform(mask, n_elements(var[*,0,0])*n_elements(var[0,*,0]))#replicate(1,cntok), n_elements(var[*,0,0]), n_elements(var[0,*,0]), cntok)
  var  = var * mask
ENDIF

var_monthly = fltarr(n_elements(var[*,0,0]),n_elements(var[0,*,0]),nbmonth)
FOR i = 0, nbmonth-1 DO BEGIN
  y = floor(i/12)
  m = i mod 12
  iave = where(date[iok] GE (long(period[0]/10000)+y)*10000+(m+1)*100 AND date[iok] LT (long(period[0]/10000)+y)*10000+(m+2)*100, cntave)
;  print, (long(period[0]/10000)+y)*10000+(m+1)*100, (long(period[0]/10000)+y)*10000+(m+2)*100, cntave
  IF iave[0] NE -1 THEN var_monthly[*,*,i] = m_mean(var[*,*,iave], dim=3, /nan) ELSE STOP
ENDFOR


; moyenne sur SON
var_3m = fltarr(n_elements(var[*,0,0]),n_elements(var[0,*,0]),nbyear)
FOR i = 0, nbyear-1 DO BEGIN
  deb = max([i*12+9-1,0]); & help, deb
  fin = min([i*12+11-1,nbyear*12+11-1]); & help, fin
  var_3m[*,*,i] = m_mean(var_monthly[*,*,deb:fin], dim=3, /nan)
ENDFOR

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
printf, 1, 'IOD+ :', year[iodp]
printf, 1, iodp
printf, 1, 'IOD- :', year[iodm]
printf, 1, iodm
CLOSE,1

;plt, m_mean(var_3m[*,*,iodp], dim=3, /nan)-m_mean(var_3m[*,*,iodm], dim=3, /nan), $
;win=1, /nocont, real=2, min=-2.5, max=2.5, lct=64
;stop
;saveimage, basn+'_'+expn+'_2D_COMPOSITE.gif'

ENDFOR; indice

ENDFOR; experience

var = ins
FOR e = 0, n_elements(exp_list)-1 DO var[e,*] = ins[e,*]-mean(ins[e,*])


; TESTS STATISTIQUES (Pearson correlation r + significance t)
r_obskf  = correlate(var[0,*],var[1,*])
r_obsbmj = correlate(var[0,*],var[2,*])
r_kfbmj  = correlate(var[1,*],var[2,*])
t_obskf  = r_obskf  * sqrt(nbyear-2)/sqrt(1-r_obskf^2)
t_obsbmj = r_obsbmj * sqrt(nbyear-2)/sqrt(1-r_obsbmj^2)
t_kfbmj  = r_kfbmj  * sqrt(nbyear-2)/sqrt(1-r_kfbmj^2)

print, 'PEARSON COR+SIG OBS-KF : ', r_obskf, t_obskf
print, 'PEARSON COR+SIG OBS-BMJ: ', r_obsbmj, t_obsbmj
print, 'PEARSON COR+SIG KF-BMJ : ', r_kfbmj, t_kfbmj

STOP


; PLOTS 1D
IF write_ps THEN openps, filename='IOD_INDEX_1990-2009.ps'
IF write_ps THEN thc = 6 ELSE thc = 2

splot, year, var[0,*], xtitle='YEARS', ytitle='IOD INDEX', charsize=1.2, thick=thc, $
ystyle=1,yrange=[-2.5,2.5],title='IOD INDEX',lct=33
;xyouts, 0.15, 0.325-0*0.025, exp_list[0], color=0, charsize=1.2, /normal
oplot, year, 0.75*stddev(var[0,*])+indgen(20)*0., line=2, color=0, thick=thc
oplot, year, -0.75*stddev(var[0,*])+indgen(20)*0., line=2, color=0, thick=thc

oplot, year, var[1,*], color=50, thick=thc
oplot, year, 0.75*stddev(var[1,*])+indgen(20)*0., line=2, color=50, thick=thc
oplot, year,-0.75*stddev(var[1,*])+indgen(20)*0., line=2, color=50, thick=thc

oplot, year, var[2,*], color=210, thick=thc
oplot, year, 0.75*stddev(var[2,*])+indgen(20)*0., line=2, color=210, thick=thc
oplot, year,-0.75*stddev(var[2,*])+indgen(20)*0., line=2, color=210, thick=thc
;xyouts, 0.15, 0.325-1*0.025, exp_list[1], color=50, charsize=1.2, /normal
;print, year[where(var[e,*] GT  0.75*stddev(var[e,*]))]
;print, year[where(var[e,*] LT -0.75*stddev(var[e,*]))]

oplot, year, indgen(20)*0., thick=thc
xyouts, 0.35, 0.325-0*0.025, 'COR OBS-KF: '+strtrim(correlate(var[0,*],var[1,*]),2), color=0*100, charsize=1.2, /normal
xyouts, 0.35, 0.325-1*0.025, 'COR OBS-BMJ: '+strtrim(correlate(var[0,*],var[2,*]),2), color=0*100, charsize=1.2, /normal
xyouts, 0.35, 0.325-2*0.025, 'COR KF-BMJ: '+strtrim(correlate(var[1,*],var[2,*]),2), color=0*100, charsize=1.2, /normal
;saveimage, 'NEWIOD_1990-2009.gif'

ind1 = where(year EQ year_nino34) & help, ind1
ind2 = where(year_nino34 EQ year) & help, ind2
print, correlate(var[0,ind1],ind_nino34[ind2])
print, correlate(var[1,ind1],ind_nino34[ind2])
print, correlate(var[2,ind1],ind_nino34[ind2])

IF write_ps THEN closeps & stop

ENDFOR; variable

STOP
END
