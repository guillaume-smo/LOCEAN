PRO stats_intensity
@common
computegrid,-200,-200,25,25,17,17

explist = ['BMJ','KF']
parlist = ['COUPLED','FORCED']
basin   = 'SIO'
freq    = '6H'
datebeg = '19900101' 
dt = 24
bin_var = 'MAX_COOLING'
int = 'inc'

IF bin_var EQ 'SST' THEN BEGIN
  bin_deb  = 24.
  bin_fin  = 32.
  bin_size = 1.
  bin_unit = 'degC'
ENDIF

IF bin_var EQ 'MAX_COOLING' THEN BEGIN
  bin_deb  = -4.
  bin_fin  = 0.
  bin_size = 0.5
  bin_unit = 'degC'
ENDIF

IF bin_var EQ 'WIND' THEN BEGIN
  bin_deb  = 15.
  bin_fin  = 50.
  bin_size = 5.
  bin_unit = 'm/s'
ENDIF

IF bin_var EQ 'PRES' THEN BEGIN
  bin_deb  = 920.
  bin_fin  = 1020.
  bin_size = 10.
  bin_unit = 'hPa'
ENDIF

bin_num  = (bin_fin - bin_deb) / bin_size + 1 & print, bin_num
bin  = bin_deb+findgen(bin_num)*bin_size
bin_mid = bin_deb + findgen(bin_num)*bin_size+bin_size/2. & print, bin_mid

;-------------------------------------------------------------------------------------------------------------------------

FOR iexp = 0, n_elements(explist)-1 DO BEGIN
exp = explist[iexp] & help, exp
FOR ipar = 0, n_elements(parlist)-1 DO BEGIN
param = parlist[ipar] & help, param
expname = param + '_SW2_'+ exp & help, expname
IF exp EQ 'KF' AND param EQ 'FORCED' THEN period  = '1990-1998' ELSE period  = '1990-2009' & help, period
IF exp EQ 'KF' AND param EQ 'FORCED' THEN dateend = '19990101'  ELSE dateend = '20100101' & help, dateend
pathin  = '/net/adonis/usr/adonis/varclim/gslod/IDL/COLLOCALISATION/EXP_'+expname+'/DATA/' & help, pathin
restore, pathin + 'd1_TRACKS_'+ expname +'_IO_'+datebeg+'-'+dateend+'.dat', /VERBOSE

; calcul duree de vie moyenne
cntok = fltarr(n_elements(d1_pres[*,0]))
FOR i = 0, n_elements(d1_pres[*,0])-1 DO BEGIN
  ik = where(finite(d1_pres[i,*]) EQ 1, tmp)
  cntok[i] = tmp
ENDFOR
totdays = mean(cntok) / 4. & help, totdays


d1_pres = transpose(d1_pres)
d1_max_wnd = transpose(d1_max_wnd)
d1_lon = transpose(d1_lon)
d1_lat = transpose(d1_lat)
ifin = where(finite(d1_pres) EQ 1 AND finite(d1_max_wnd) EQ 1) & help, ifin

IF  bin_var EQ 'SST' THEN BEGIN
  restore, pathin + 'SST_1D_'+ expname +'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat', /VERBOSE
  d1_sst = transpose(d1_var_during)
  ifin = where(finite(d1_pres) EQ 1 AND finite(d1_max_wnd) EQ 1 AND finite(d1_sst) EQ 1) & help, ifin
ENDIF

IF  bin_var EQ 'MAX_COOLING' THEN BEGIN
  restore, pathin + 'SST_1D_'+ expname +'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat', /VERBOSE
  d1_sst = transpose(D1_VAR_ANO_MIN_BEF)
  ifin = where(finite(d1_pres) EQ 1 AND finite(d1_max_wnd) EQ 1 AND finite(d1_sst) EQ 1) & help, ifin
ENDIF

IF basin EQ 'SIO' THEN idom = where(d1_lon GT 30. AND d1_lon LT 130. AND d1_lat LT 0.)
help, idom
iok = intersect(ifin,idom) & help, iok
;icool = where(D1_VAR_ANO_MEAN_BEF GE 1.)
;iok = intersect(iok,icool) & help, iok
print, correlate(d1_pres[iok],d1_max_wnd[iok])

; pression, vent et SST moyen
print, mean(d1_pres[iok]),mean(d1_max_wnd[iok])
print, mean((transpose(d1_var_before))[iok])-273.15,mean((transpose(d1_var_during))[iok])-273.15,mean((transpose(d1_var_after))[iok])-273.15
print, mean((transpose(d1_var_ano_mean_bef))[iok]), mean((transpose(d1_var_ano_min_bef))[iok]), mean((transpose(d1_var_ano_max_bef))[iok])

;GOTO, JUMP1

IF dt EQ 12 THEN BEGIN
d1_pres_smooth = ts_smooth(d1_pres[iok],3)
d1_wind_smooth = ts_smooth(d1_max_wnd[iok],3)
ENDIF

IF dt EQ 24 THEN BEGIN
d1_pres_smooth = ts_smooth(d1_pres[iok],5)
d1_wind_smooth = ts_smooth(d1_max_wnd[iok],5)
ENDIF

IF dt EQ 36 THEN BEGIN
d1_pres_smooth = ts_smooth(d1_pres[iok],7)
d1_wind_smooth = ts_smooth(d1_max_wnd[iok],7)
ENDIF

IF dt EQ 48 THEN BEGIN
d1_pres_smooth = ts_smooth(d1_pres[iok],9)
d1_wind_smooth = ts_smooth(d1_max_wnd[iok],9)
ENDIF

help, d1_pres_smooth
print, correlate(d1_pres_smooth,d1_wind_smooth)

;dpresdt_6h = (d1_pres_smooth - shift(d1_pres_smooth,1)) / 6.
;dwinddt_6h = (d1_wind_smooth - shift(d1_wind_smooth,1)) / 6.
;help, dpresdt_6h
;print, correlate(dpresdt_6h,dwinddt_6h)

;dpresdt_12h = (shift(d1_pres_smooth,-1) - shift(d1_pres_smooth,1)) / 12.
;dwinddt_12h = (shift(d1_wind_smooth,-1) - shift(d1_wind_smooth,1)) / 12.
;help, dpresdt_12h
;print, correlate(dpresdt_12h,dwinddt_12h)

dpresdt_24h_cen = (shift(d1_pres_smooth,-2) - shift(d1_pres_smooth,2)) / 24.
dwinddt_24h_cen = (shift(d1_wind_smooth,-2) - shift(d1_wind_smooth,2)) / 24.
dpresdt_24h_for = (d1_pres_smooth - shift(d1_pres_smooth,5)) / 24.
dwinddt_24h_for = (d1_wind_smooth - shift(d1_wind_smooth,5)) / 24.

dpresdt_24h_for2 = fltarr(n_elements(d1_pres_smooth))
FOR i = 0, n_elements(d1_pres_smooth)-6 DO dpresdt_24h_for2[i] = d1_pres_smooth - d1_pres_smooth[i+5]
;dpresdt_36h = (shift(d1_pres_smooth,-3) - shift(d1_pres_smooth,3)) / 36.
;dwinddt_36h = (shift(d1_wind_smooth,-3) - shift(d1_wind_smooth,3)) / 36.
;help, dpresdt_36h
;print, correlate(dpresdt_36h,dwinddt_36h)

;dpresdt_48h = (shift(d1_pres_smooth,-4) - shift(d1_pres_smooth,4)) / 48.
;dwinddt_48h = (shift(d1_wind_smooth,-4) - shift(d1_wind_smooth,4)) / 48.
;help, dpresdt_48h
;print, correlate(dpresdt_48h,dwinddt_48h)

dpresdt = dpresdt_24h_for
dwinddt = dwinddt_24h_for
help, dpresdt
print, correlate(dpresdt,dwinddt)
stop

ipinc = where(dpresdt LT 0.) & help, ipinc
iwinc = where(dwinddt GT 0.) & help, iwinc
ipwinc = where(dpresdt LT 0. AND dwinddt GT 0., cptpwinc) & help, ipwinc
ipdec = where(dpresdt GT 0.) & help, ipdec
iwdec = where(dwinddt LT 0.) & help, iwdec
ipwdec = where(dpresdt GT 0. AND dwinddt LT 0., cptpwdec) & help, ipwdec

inclife = float(cptpwinc) / float(cptpwinc+cptpwdec) * 100. & print, inclife
declife = float(cptpwdec) / float(cptpwinc+cptpwdec) * 100. & print, declife

meanpinc = mean(dpresdt[ipwinc]) * dt & print, meanpinc
meanwinc = mean(dwinddt[ipwinc]) * dt & print, meanwinc
meanpdec = mean(dpresdt[ipwdec]) * dt & print, meanpdec
meanwdec = mean(dwinddt[ipwdec]) * dt & print, meanwdec

bin1_deb  = 0.
bin1_fin  = 60.
bin1_size = 10.
bin1_unit = '(m/s)'
bin1_num  = (bin1_fin - bin1_deb) / bin1_size + 1 & print, bin1_num
bin1      = bin1_deb+findgen(bin1_num)*bin1_size
bin1_mid  = bin1_deb + findgen(bin1_num)*bin1_size+bin1_size/2. & print, bin1_mid

pdf_wind_dpresdt = fltarr(bin1_num)
pdf_wind_dwinddt = fltarr(bin1_num)
FOR i = 0, bin1_num-2 DO BEGIN
  ibin1 = where(d1_wind_smooth GE bin1[i] AND d1_wind_smooth LT bin1[i+1])
  IF n_elements(ibin1) GT 1 THEN pdf_wind_dpresdt[i] = mean(dpresdt[ibin1])
  IF n_elements(ibin1) GT 1 THEN pdf_wind_dwinddt[i] = mean(dwinddt[ibin1])
ENDFOR
bar_plot, pdf_wind_dpresdt, back=255, /outline, barname=bin1_mid
saveimage, 'pdf_wind_dpresdt'+'_'+expname+'.gif'
bar_plot, pdf_wind_dwinddt, back=255, /outline, barname=bin1_mid
saveimage, 'pdf_wind_dwinddt'+'_'+expname+'.gif'

pdf_dpresdt = histogram(dpresdt, binsize=0.2, min=-1.4, max=1.4, locations=bins_pres)
bar_plot, pdf_dpresdt, back=255, /outline, barname=bins_pres
saveimage, 'pdf_dpresdt'+'_'+expname+'.gif'
pdf_dwinddt = histogram(dwinddt, binsize=0.1, min=-0.7, max=0.7, locations=bins_wind)
bar_plot, pdf_dwinddt, back=255, /outline, barname=bins_wind
saveimage, 'pdf_dwinddt'+'_'+expname+'.gif'

GOTO, JUMP1


IF bin_var EQ 'WIND' THEN var_bin = d1_wind_smooth
IF bin_var EQ 'PRES' THEN var_bin = d1_pres_smooth
IF bin_var EQ 'SST' THEN  var_bin = d1_sst[iok]-273.15
IF bin_var EQ 'MAX_COOLING' THEN var_bin = d1_sst[iok]
help, var_bin

hwind_bin = fltarr(bin_num)
hpres_bin = fltarr(bin_num)
cpt = 0
IF int EQ 'inc' THEN BEGIN
FOR i = 0, bin_num-2 DO BEGIN
  ibin = where(var_bin[ipwinc] GE bin[i] AND var_bin[ipwinc] LT bin[i+1])
  IF n_elements(ibin) GE 50 THEN hwind_bin[i] = mean(d1_wind_smooth[ipwinc[ibin]])
  IF n_elements(ibin) GE 50 THEN hpres_bin[i] = mean(d1_pres_smooth[ipwinc[ibin]])
  print, i, bin_mid[i],  n_elements(ibin)
  cpt = cpt + n_elements(ibin)
ENDFOR
ENDIF
IF int EQ 'dec' THEN BEGIN
FOR i = 0, bin_num-2 DO BEGIN
  ibin = where(var_bin[ipwdec] GE bin[i] AND var_bin[ipwdec] LT bin[i+1])
  IF n_elements(ibin) GE 50 THEN hwind_bin[i] = mean(d1_wind_smooth[ipwdec[ibin]])
  IF n_elements(ibin) GE 50 THEN hpres_bin[i] = mean(d1_pres_smooth[ipwdec[ibin]])
  print, i, bin_mid[i],  n_elements(ibin)
  cpt = cpt + n_elements(ibin)
ENDFOR
ENDIF
print, cpt


pathfig = '/net/cratos/usr/cratos/varclim/gslod/IDL/COLLOCALISATION/FIGS_STATS_'+expname+'/'
spawn, 'echo "creation du repertoire '+ pathfig +'"' & spawn, 'mkdir -p '+ pathfig

IF param EQ 'COUPLED' THEN window,0
IF param EQ 'FORCED' THEN wset,0
inz = where(hwind_bin NE 0.)
IF param EQ 'COUPLED' THEN plot, bin_mid[inz],hwind_bin[inz], $
yrange=[15,45],xrange=[min(bin_mid),max(bin_mid)],xtitle=bin_unit,ytitle='m.s-1'
IF param EQ 'FORCED' THEN plot, bin_mid[inz],hwind_bin[inz],yrange=[15,45],xrange=[min(bin_mid),max(bin_mid)],/noerase,linestyle=2
IF param EQ 'FORCED' THEN saveimage, pathfig+'wind_'+bin_var+'_anomin_'+int+'_'+strtrim(dt,2)+'h_'+exp+'_'+freq+'_'+basin+'_'+period+'.gif'

IF param EQ 'COUPLED' THEN window,1
IF param EQ 'FORCED' THEN wset,1
inz = where(hpres_bin NE 0.)
IF param EQ 'COUPLED' THEN plot, bin_mid[inz],hpres_bin[inz], $
yrange=[960,1010],xrange=[min(bin_mid),max(bin_mid)],xtitle=bin_unit,ytitle='hPa'
IF param EQ 'FORCED' THEN plot, bin_mid[inz],hpres_bin[inz],yrange=[960,1010],xrange=[min(bin_mid),max(bin_mid)],/noerase,linestyle=2
IF param EQ 'FORCED' THEN saveimage, pathfig+'pres_'+bin_var+'_anomin_'+int+'_'+strtrim(dt,2)+'h_'+exp+'_'+freq+'_'+basin+'_'+period+'.gif'


hdtwind_bin = fltarr(bin_num)
hdtpres_bin = fltarr(bin_num)
cpt = 0
FOR i = 0, bin_num-2 DO BEGIN
  ibin = where(var_bin[ipwinc] GE bin[i] AND var_bin[ipwinc] LT bin[i+1])
  IF n_elements(ibin) GE 50 THEN hdtwind_bin[i] = mean(dwinddt[ipwinc[ibin]])
  IF n_elements(ibin) GE 50 THEN hdtpres_bin[i] = mean(dpresdt[ipwinc[ibin]])
  print, i, bin_mid[i],  n_elements(ibin)
  cpt = cpt + n_elements(ibin)
ENDFOR
print, cpt


pathfig = '/net/cratos/usr/cratos/varclim/gslod/IDL/COLLOCALISATION/FIGS_'+expname+'/'
spawn, 'echo "creation du repertoire '+ pathfig +'"' & spawn, 'mkdir -p '+ pathfig

IF param EQ 'COUPLED' THEN window,2
IF param EQ 'FORCED' THEN wset,2
inz = where(hdtwind_bin NE 0.)
IF param EQ 'COUPLED' THEN plot, bin_mid[inz],hdtwind_bin[inz], $
yrange=[0,0.4],xrange=[min(bin_mid),max(bin_mid)],xtitle=bin_unit,ytitle='m.s-1/'+strtrim(dt,2)+'h'
IF param EQ 'FORCED' THEN plot, bin_mid[inz],hdtwind_bin[inz],yrange=[0,0.4],xrange=[min(bin_mid),max(bin_mid)],/noerase,linestyle=2
IF param EQ 'FORCED' THEN saveimage, pathfig+'dtwind_'+bin_var+'before_'+strtrim(dt,2)+'h_'+exp+'_'+freq+'_'+basin+'_'+period+'.gif'

IF param EQ 'COUPLED' THEN window,3
IF param EQ 'FORCED' THEN wset,3
inz = where(hdtpres_bin NE 0.)
IF param EQ 'COUPLED' THEN plot, bin_mid[inz],hdtpres_bin[inz], $
yrange=[-0.6,0],xrange=[min(bin_mid),max(bin_mid)],xtitle=bin_unit,ytitle='hPa/'+strtrim(dt,2)+'h'
IF param EQ 'FORCED' THEN plot, bin_mid[inz],hdtpres_bin[inz],yrange=[-0.6,0],xrange=[min(bin_mid),max(bin_mid)],/noerase,linestyle=2
IF param EQ 'FORCED' THEN saveimage, pathfig+'dtpres_'+bin_var+'before_'+strtrim(dt,2)+'h_'+exp+'_'+freq+'_'+basin+'_'+period+'.gif'

JUMP1:

ENDFOR
ENDFOR
END
