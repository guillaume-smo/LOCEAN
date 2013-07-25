PRO plot_sst_vs_didt_test



; parametres
explist = ['KF']
parlist = ['COUPLED','FORCED']
basin   = 'SIO'
freq    = '6H'
datebeg = '19900101'
var_nam = 'dUV/dt'
var_uni = '(m.s-1/6h)'
var_typ = 'BEF' ; before / during / after
rayon   = '200'



; definition bin
bin_name  = 'SST'
bin_deb  = 24.
bin_fin  = 32.
bin_size = 1
bin_unit = '(degC)'
bin_num  = (bin_fin - bin_deb) / bin_size & print, bin_num
bin      = bin_deb+findgen(bin_num)*bin_size
bin_mid  = bin_deb + findgen(bin_num)*bin_size+bin_size/2. & print, bin_mid



; declarations
nb_exp = n_elements(parlist)
dpresdtint = fltarr(bin_num,10000,nb_exp)
dwinddtint = fltarr(bin_num,10000,nb_exp)
dpresdtdec = fltarr(bin_num,10000,nb_exp)
dwinddtdec = fltarr(bin_num,10000,nb_exp)
dpresdtall = fltarr(bin_num,10000,nb_exp)
dwinddtall = fltarr(bin_num,10000,nb_exp)
nb_binint = intarr(bin_num,nb_exp)
nb_bindec = intarr(bin_num,nb_exp)
nb_binall = intarr(bin_num,nb_exp)
windint_bin = fltarr(bin_num,nb_exp)
winddec_bin = fltarr(bin_num,nb_exp)
windall_bin = fltarr(bin_num,nb_exp)
presint_bin = fltarr(bin_num,nb_exp)
presdec_bin = fltarr(bin_num,nb_exp)
presall_bin = fltarr(bin_num,nb_exp)
dwinddtint_bin = fltarr(bin_num,nb_exp)
dwinddtdec_bin = fltarr(bin_num,nb_exp)
dwinddtall_bin = fltarr(bin_num,nb_exp)
dpresdtint_bin = fltarr(bin_num,nb_exp)
dpresdtdec_bin = fltarr(bin_num,nb_exp)
dpresdtall_bin = fltarr(bin_num,nb_exp)
dpresdt_intper = fltarr(2,bin_num,nb_exp)
dwinddt_intper = fltarr(2,bin_num,nb_exp)
dpresdt_decper = fltarr(2,bin_num,nb_exp)
dwinddt_decper = fltarr(2,bin_num,nb_exp)
dpresdt_allper = fltarr(2,bin_num,nb_exp)
dwinddt_allper = fltarr(2,bin_num,nb_exp)
;ts_dpresdtint = fltarr(bin_num,2,nb_exp)
;ts_dpresdtdec = fltarr(bin_num,2,nb_exp)
ts_dpresdtall = fltarr(bin_num,2,nb_exp)
;ts_dwinddtint = fltarr(bin_num,2,nb_exp)
;ts_dwinddtdec = fltarr(bin_num,2,nb_exp)
ts_dwinddtall = fltarr(bin_num,2,nb_exp)



; lecture best-track
FOR iexp = 0, n_elements(explist)-1 DO BEGIN
exp = explist[iexp] & help, exp
FOR ipar = 0, n_elements(parlist)-1 DO BEGIN
param = parlist[ipar] & help, param
expname = param + '_SW2_'+ exp & help, expname
IF exp EQ 'KF' AND param EQ 'FORCED' THEN period  = '1990-1998' ELSE period  = '1990-2009' & help, period
IF exp EQ 'KF' AND param EQ 'FORCED' THEN dateend = '19990101'  ELSE dateend = '20100101' & help, dateend
pathin  = '/net/adonis/usr/adonis/varclim/gslod/IDL/COLLOCALISATION/EXP_'+expname+'/DATA/' & help, pathin
restore, pathin + 'd1_TRACKS_'+ expname +'_IO_'+datebeg+'-'+dateend+'.dat', /VERBOSE



; lecture sst
restore, pathin+bin_name+'_1D_'+rayon+'km_'+expname+'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat', /VERBOSE
IF var_typ EQ 'DUR' THEN $
restore, pathin+bin_name+'_2D_'+rayon+'km_'+expname+'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat', /VERBOSE
IF var_typ EQ 'BEF' THEN d1_var = d1_var_before
IF var_typ EQ 'DUR' THEN d1_var = m_mean(m_mean(temporary(dxy_var_wsc_rot),dim=4,/nan),dim=3,/nan)
IF var_typ EQ 'AFT'  THEN d1_var = d1_var_after
IF bin_name EQ 'SST' THEN d1_var = d1_var-273.15 & help, d1_var



; calcul index didt
calcul_index_didt24h,d1_pres,d1_max_wnd,ind_int24h,ind_dec24h,d1_wind_smooth_24h,dwinddt,dpresdt,i_int24h,i_dec24h
iokint = ind_int24h[where(finite(d1_var[ind_int24h]) EQ 1)] & help, iokint
iokdec = ind_dec24h[where(finite(d1_var[ind_dec24h]) EQ 1)] & help, iokdec
iokall = [iokint,iokdec] & help, iokall



; calcul pdf
FOR i = 0, bin_num-2 DO BEGIN
  ibin_int = where(d1_var[iokint] GE bin[i] AND d1_var[iokint] LT bin[i+1])
  ibin_dec = where(d1_var[iokdec] GE bin[i] AND d1_var[iokdec] LT bin[i+1])
  ibin_all = where(d1_var[iokall] GE bin[i] AND d1_var[iokall] LT bin[i+1])
  ; sauvegarde intermediaire
  IF ibin_int[0] NE -1 THEN BEGIN
    dpresdtint[i,0:n_elements(ibin_int)-1,ipar] = dpresdt[iokint[ibin_int]]
    dwinddtint[i,0:n_elements(ibin_int)-1,ipar] = dwinddt[iokint[ibin_int]]
  ENDIF
  IF ibin_dec[0] NE -1 THEN BEGIN
    dpresdtdec[i,0:n_elements(ibin_dec)-1,ipar] = dpresdt[iokdec[ibin_dec]]
    dwinddtdec[i,0:n_elements(ibin_dec)-1,ipar] = dwinddt[iokdec[ibin_dec]]
  ENDIF
  IF ibin_all[0] NE -1 THEN BEGIN
    dpresdtall[i,0:n_elements(ibin_all)-1,ipar] = dpresdt[iokall[ibin_all]]
    dwinddtall[i,0:n_elements(ibin_all)-1,ipar] = dwinddt[iokall[ibin_all]]
  ENDIF
  ; moyenne par bin
  IF n_elements(ibin_int) GT 1 THEN windint_bin[i,ipar]    = mean(d1_wind_smooth_24h[iokint[ibin_int]],/nan)
  IF n_elements(ibin_int) GT 1 THEN presint_bin[i,ipar]    = mean(d1_pres[iokint[ibin_int]],/nan)
  IF n_elements(ibin_int) GT 1 THEN dwinddtint_bin[i,ipar] = mean(dwinddt[iokint[ibin_int]],/nan)
  IF n_elements(ibin_int) GT 1 THEN dpresdtint_bin[i,ipar] = mean(dpresdt[iokint[ibin_int]],/nan)
  IF n_elements(ibin_dec) GT 1 THEN winddec_bin[i,ipar]    = mean(d1_wind_smooth_24h[iokdec[ibin_dec]],/nan)
  IF n_elements(ibin_dec) GT 1 THEN presdec_bin[i,ipar]    = mean(d1_pres[iokdec[ibin_dec]],/nan)
  IF n_elements(ibin_dec) GT 1 THEN dwinddtdec_bin[i,ipar] = mean(dwinddt[iokdec[ibin_dec]],/nan)
  IF n_elements(ibin_dec) GT 1 THEN dpresdtdec_bin[i,ipar] = mean(dpresdt[iokdec[ibin_dec]],/nan)
  IF n_elements(ibin_all) GT 1 THEN windall_bin[i,ipar]    = mean(d1_wind_smooth_24h[iokall[ibin_all]],/nan)
  IF n_elements(ibin_all) GT 1 THEN presall_bin[i,ipar]    = mean(d1_pres[iokall[ibin_all]],/nan)
  IF n_elements(ibin_all) GT 1 THEN dwinddtall_bin[i,ipar] = mean(dwinddt[iokall[ibin_all]],/nan)
  IF n_elements(ibin_all) GT 1 THEN dpresdtall_bin[i,ipar] = mean(dpresdt[iokall[ibin_all]],/nan)
  ; percentile par bin
  IF n_elements(ibin_int) GT 1 THEN dpresdt_intper[*,i,ipar] = percentile(dpresdt[iokint[ibin_int]],0.25)
  IF n_elements(ibin_int) GT 1 THEN dwinddt_intper[*,i,ipar] = percentile(dwinddt[iokint[ibin_int]],0.25)
  IF n_elements(ibin_dec) GT 1 THEN dpresdt_decper[*,i,ipar] = percentile(dpresdt[iokdec[ibin_dec]],0.25)
  IF n_elements(ibin_dec) GT 1 THEN dwinddt_decper[*,i,ipar] = percentile(dwinddt[iokdec[ibin_dec]],0.25)
  IF n_elements(ibin_all) GT 1 THEN dpresdt_allper[*,i,ipar] = percentile(dpresdt[iokall[ibin_all]],0.25)
  IF n_elements(ibin_all) GT 1 THEN dwinddt_allper[*,i,ipar] = percentile(dwinddt[iokall[ibin_all]],0.25)
  ; nombre de points par bin
  IF ibin_int[0] NE -1 THEN nb_binint[i,ipar] = n_elements(ibin_int) ELSE nb_binint[i,ipar] = 0
  IF ibin_dec[0] NE -1 THEN nb_bindec[i,ipar] = n_elements(ibin_dec) ELSE nb_bindec[i,ipar] = 0
  IF ibin_all[0] NE -1 THEN nb_binall[i,ipar] = n_elements(ibin_all) ELSE nb_binall[i,ipar] = 0
ENDFOR

print, 'CHECK: ', total(nb_binint[*,ipar]+nb_bindec[*,ipar]), '/', total(nb_binall[*,ipar])

ENDFOR ; par
ENDFOR ; exp



; test significativite
rstest = fltarr(2,bin_num)
tmtest = fltarr(2,bin_num)
rstest_ok = intarr(bin_num)
tmtest_ok = intarr(bin_num)
FOR i = 0, bin_num-2 DO BEGIN
  print, i, nb_binall[i,0], nb_binall[i,1]
  IF nb_binall[i,0] GT 0 AND nb_binall[i,1] GT 0 THEN BEGIN
    iok0 = where(dwinddtall[i,*,0] NE 0) & help, iok0
    iok1 = where(dwinddtall[i,*,1] NE 0) & help, iok1
    varok0 = reform(dwinddtall[i,iok0,0]) & help, varok0
    varok1 = reform(dwinddtall[i,iok1,1]) & help, varok1
    rstest[*,i] =  RS_TEST(varok0,varok1)
    tmtest[*,i] =  TM_TEST(varok0,varok1, /UNEQUAL)
    print, rstest[*,i]
    print, tmtest[*,i]
  ENDIF
ENDFOR
tmtest_ok[where(tmtest[1,*] LT 0.05)] = 1 & print, tmtest_ok
rstest_ok[where(rstest[1,*] LT 0.05)] = 1 & print, rstest_ok
STOP



; PLOTS
reinitplt
SET_PLOT, 'X'
DEVICE, decomposed=0, retain=0
lct, 33

IF write_ps THEN thc = 6 ELSE thc = 2
IF write_ps THEN openps, filename='SST_vs_DUVDT_'+basin+'.ps'

ymin = -2.5 & ymax = 2.5
iok = where(dwinddtall_bin[*,0] NE 0.)

splot, bin_mid[iok], dwinddtall_bin[iok,0], xstyle=1, ystyle=1, $
xrange=[bin[min(iok)],bin[max(iok)+1]],xtitle=bin_name+' '+bin_unit,ytitle=var_nam+' '+var_uni,yrange=[ymin,ymax]
oplot, bin_mid[iok], dwinddtall_bin[iok,0], thick=thc, color=50
oplot, bin_mid[iok], dwinddt_allper[0,iok,0], linestyle=2, color=50, thick=thc
oplot, bin_mid[iok], dwinddt_allper[1,iok,0], linestyle=2, color=50, thick=thc
oplot, bin_mid[iok], dwinddtall_bin[iok,1], thick=thc, color=225
oplot, bin_mid[iok], dwinddt_allper[0,iok,1], linestyle=2, color=225, thick=thc
oplot, bin_mid[iok], dwinddt_allper[1,iok,1], linestyle=2, color=225, thick=thc
xyouts, bin_mid[iok], -2.4, strtrim(long(tmtest_ok),2), align=0.5
xyouts, 0.15, 0.90, 'COUPLED', /normal, color=50, charsize=1.5
xyouts, 0.15, 0.85, 'FORCED', /normal, color=225, charsize=1.5
oplot, [bin_deb,bin_fin],[0,0]

IF write_ps THEN closeps & STOP


END
