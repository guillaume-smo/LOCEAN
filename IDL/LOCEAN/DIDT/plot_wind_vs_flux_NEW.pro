PRO plot_wind_vs_flux_test



; parametres
explist = ['KF']
parlist = ['COUPLED','FORCED']
basin   = 'SIO'
freq    = '6H'
datebeg = '19900101'
var_nam = 'HFXLH'
var_uni = '(W/m2)'
rayon   = '200'
write_ps= 1


; definition bin
bin_deb  = 15.
bin_fin  = 60.
bin_size = 5
bin_name = '10M WIND'
bin_unit = '(m/s)'
bin_num  = (bin_fin - bin_deb) / bin_size & print, bin_num
bin      = bin_deb + findgen(bin_num)*bin_size
bin_mid  = bin_deb + findgen(bin_num)*bin_size+bin_size/2. & print, bin_mid



; declarations
nb_exp = n_elements(parlist)
varint = fltarr(bin_num, 10000, nb_exp)
vardec = fltarr(bin_num, 10000, nb_exp)
varall = fltarr(bin_num, 10000, nb_exp)
pdf_wind_var_int = fltarr(bin_num,nb_exp)
pdf_wind_var_intper = fltarr(2,bin_num,nb_exp)
pdf_wind_var_dec = fltarr(bin_num,nb_exp)
pdf_wind_var_decper = fltarr(2,bin_num,nb_exp)
pdf_wind_var_all = fltarr(bin_num,nb_exp)
pdf_wind_var_allper = fltarr(2,bin_num,nb_exp)
nb_binint = intarr(bin_num,nb_exp)
nb_bindec = intarr(bin_num,nb_exp)
nb_binall = intarr(bin_num,nb_exp)



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



; lecture var 1D+2D
restore, pathin+var_nam+'_2D_'+rayon+'km_'+expname+'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat', /VERBOSE
var_xy = dxy_var_wsc_rot & help, var_xy
d1_var = m_mean(m_mean(temporary(dxy_var_wsc_rot),dim=4,/nan),dim=3,/nan); - temporary(d1_var_before)



; calcul index didt
calcul_index_didt24h,d1_pres,d1_max_wnd,ind_int24h,ind_dec24h,d1_wind_smooth_24h,dwinddt,dpresdt,i_int24h,i_dec24h
iokint = ind_int24h[where(finite(d1_var[ind_int24h]) EQ 1)] & help, iokint
iokdec = ind_dec24h[where(finite(d1_var[ind_dec24h]) EQ 1)] & help, iokdec
iokall = [iokint,iokdec]



; calcul pdf
FOR i = 0, bin_num-2 DO BEGIN
  ibin_int = where(d1_wind_smooth_24h[iokint] GE bin[i] AND d1_wind_smooth_24h[iokint] LT bin[i+1])
  ibin_dec = where(d1_wind_smooth_24h[iokdec] GE bin[i] AND d1_wind_smooth_24h[iokdec] LT bin[i+1])
  ibin_all = where(d1_wind_smooth_24h[iokall] GE bin[i] AND d1_wind_smooth_24h[iokall] LT bin[i+1])
  ; sauvegarde intermediaire
  IF ibin_int[0] NE -1 THEN varint[i,0:n_elements(ibin_int)-1,ipar] = d1_var[iokint[ibin_int]]
  IF ibin_dec[0] NE -1 THEN vardec[i,0:n_elements(ibin_dec)-1,ipar] = d1_var[iokdec[ibin_dec]]
  IF ibin_all[0] NE -1 THEN varall[i,0:n_elements(ibin_all)-1,ipar] = d1_var[iokall[ibin_all]]
  ; moyenne
  IF n_elements(ibin_int) GT 10 THEN pdf_wind_var_int[i,ipar] = mean(d1_var[iokint[ibin_int]],/nan)
  IF n_elements(ibin_dec) GT 10 THEN pdf_wind_var_dec[i,ipar] = mean(d1_var[iokdec[ibin_dec]],/nan)
  IF n_elements(ibin_all) GT 10 THEN pdf_wind_var_all[i,ipar] = mean(d1_var[iokall[ibin_all]],/nan)
  ; percentile
  IF n_elements(ibin_int) GT 10 THEN pdf_wind_var_intper[*,i,ipar] = percentile(d1_var[iokint[ibin_int]],0.25)
  IF n_elements(ibin_dec) GT 10 THEN pdf_wind_var_decper[*,i,ipar] = percentile(d1_var[iokdec[ibin_dec]],0.25)
  IF n_elements(ibin_all) GT 10 THEN pdf_wind_var_allper[*,i,ipar] = percentile(d1_var[iokall[ibin_all]],0.25)
  ; nombre de points par bin
  IF ibin_int[0] NE -1 THEN nb_binint[i,ipar] = n_elements(ibin_int) ELSE nb_binint[i,ipar] = 0
  IF ibin_dec[0] NE -1 THEN nb_bindec[i,ipar] = n_elements(ibin_dec) ELSE nb_bindec[i,ipar] = 0
  IF ibin_all[0] NE -1 THEN nb_binall[i,ipar] = n_elements(ibin_all) ELSE nb_binall[i,ipar] = 0
ENDFOR

print, 'CHECK: ', total(nb_binint[*,ipar]+nb_bindec[*,ipar]), '/', total(nb_binall[*,ipar])

ENDFOR
ENDFOR


; test significativite
rstest = fltarr(2,bin_num)
tmtest = fltarr(2,bin_num)
rstest_ok = intarr(bin_num)
tmtest_ok = intarr(bin_num)
FOR i = 0, bin_num-2 DO BEGIN
  print, i, nb_binint[i,0], nb_binint[i,1]
  IF nb_binint[i,0] GT 1 AND nb_binint[i,1] GT 1 THEN BEGIN
    iok0 = where(varint[i,*,0] NE 0) & help, iok0
    iok1 = where(varint[i,*,1] NE 0) & help, iok1
    varok0 = reform(varint[i,iok0,0]) & help, varok0
    varok1 = reform(varint[i,iok1,1]) & help, varok1
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


; BARPLOTS
;window, 0
;bar_plot, pdf_wind_var_int, back=255, /outline, barname=bin_mid
;saveimage, 'FIGS_PDF/pdf_wind_bin'+strtrim(bin_size,2)+'_'+var_nam+'_int'+'_'+expname+'.gif'
;window, 1
;bar_plot, pdf_wind_var_dec, back=255, /outline, barname=bin_mid
;saveimage, 'FIGS_PDF/pdf_wind_bin'+strtrim(bin_size,2)+'_'+var_nam+'_dec'+'_'+expname+'.gif'


; PLOTS 1D
IF write_ps THEN thc = 6 ELSE thc = 2
IF write_ps THEN openps, filename='WIND_vs_HFXLH_'+basin+'.ps'

ymin = 0 & ymax = 1100
iok = where(pdf_wind_var_int[*,0] NE 0.)

splot, bin_mid[iok],pdf_wind_var_int[iok,0], xstyle=1, ystyle=1, charsize=1.5, $
xrange=[bin[min(iok)],bin[max(iok)+1]],xtitle=bin_name+' '+bin_unit,ytitle=var_nam+' '+var_uni,yrange=[ymin,ymax]
oplot, bin_mid[iok],pdf_wind_var_int[iok,0],linestyle=0,color=50 ,thick=thc
oplot, bin_mid[iok],pdf_wind_var_intper[0,iok,0], linestyle=2, color=50, thick=thc
oplot, bin_mid[iok],pdf_wind_var_intper[1,iok,0], linestyle=2, color=50, thick=thc
oplot, bin_mid[iok],pdf_wind_var_int[iok,1],linestyle=0,color=225, thick=thc
oplot, bin_mid[iok],pdf_wind_var_intper[0,iok,1], linestyle=2, color=225, thick=thc
oplot, bin_mid[iok],pdf_wind_var_intper[1,iok,1], linestyle=2, color=225, thick=thc
xyouts, bin_mid[iok], 50, strtrim(long(tmtest_ok),2), align=0.5
xyouts, 0.15, 0.90, 'COUPLED', /normal, color=50, charsize=1.5
xyouts, 0.15, 0.85, 'FORCED', /normal, color=225, charsize=1.5

IF write_ps THEN closeps & STOP



;diff = pdf_wind_var_int[iok] - tmpint[iok]
;oplot, bin_mid[iok],diff[iok], linestyle=2, color=200, thick=2
;xyouts, bin_mid[iok],ymax-100,strtrim(nb_binint[iok],2)
;xyouts, bin_mid[iok],ymax-150,strtrim(ts_int[iok,1],2)
;saveimage, 'FIGS_PDF/pdf_'+rayon+'km_wind_bin'+strtrim(bin_size,2)+'_'+var_nam+'_int+dec_'+exp+'_v2.gif'



IF param EQ 'COUPLED' THEN BEGIN
window,3
iok = where(pdf_wind_var_dec NE 0.)
plot, bin_mid[iok],pdf_wind_var_dec[iok], $
xrange=[min(bin_mid[iok]),max(bin_mid[iok])],xtitle='10M WIND '+bin_unit,ytitle=var_nam+' (W/m2)',yrange=[ymin,ymax]
oplot, bin_mid[iok],pdf_wind_var_dec[iok],linestyle=0,color=50,thick=2
oplot, bin_mid[iok],pdf_wind_var_decper[0,iok], linestyle=2, color=50, thick=1
oplot, bin_mid[iok],pdf_wind_var_decper[1,iok], linestyle=2, color=50, thick=1
xyouts, bin_mid[iok],ymax-50,strtrim(nb_bindec[iok],2)
tmpdec = pdf_wind_var_dec
ENDIF

IF param EQ 'FORCED' THEN BEGIN
wset,3
iok = where(tmpdec NE 0. AND pdf_wind_var_dec NE 0.)
plot, bin_mid[iok],pdf_wind_var_dec[iok], /noerase, $
xrange=[min(bin_mid[iok]),max(bin_mid[iok])],yrange=[ymin,ymax]
oplot, bin_mid[iok],pdf_wind_var_dec[iok],linestyle=0,color=150, thick=2
oplot, bin_mid[iok],pdf_wind_var_decper[0,iok], linestyle=2, color=150, thick=1
oplot, bin_mid[iok],pdf_wind_var_decper[1,iok], linestyle=2, color=150, thick=1
diff = pdf_wind_var_dec[iok] - tmpdec[iok]
oplot, bin_mid[iok],diff[iok], linestyle=2, color=200, thick=2
xyouts, bin_mid[iok],ymax-100,strtrim(nb_bindec[iok],2)
xyouts, bin_mid[iok],ymax-150,strtrim(ts_dec[iok,1],2)
saveimage, 'FIGS_PDF/pdf_wind_bin'+strtrim(bin_size,2)+'_'+var_nam+'_dec_'+exp+'.gif'
ENDIF


IF param EQ 'COUPLED' THEN BEGIN
window,4
iok = where(pdf_wind_var_all NE 0.)
plot, bin_mid[iok],pdf_wind_var_all[iok], $
xrange=[min(bin_mid[iok]),max(bin_mid[iok])],xtitle='10M WIND '+bin_unit,ytitle=var_nam+' (W/m2)',yrange=[ymin,ymax]
oplot, bin_mid[iok],pdf_wind_var_all[iok],linestyle=0,color=50,thick=2
oplot, bin_mid[iok],pdf_wind_var_allper[0,iok], linestyle=2, color=50, thick=1
oplot, bin_mid[iok],pdf_wind_var_allper[1,iok], linestyle=2, color=50, thick=1
xyouts, bin_mid[iok],ymax-50,strtrim(nb_binall[iok],2)
tmpall = pdf_wind_var_all
ENDIF

IF param EQ 'FORCED' THEN BEGIN
wset,4
iok = where(tmpall NE 0. AND pdf_wind_var_all NE 0.)
plot, bin_mid[iok],pdf_wind_var_all[iok], /noerase, $
xrange=[min(bin_mid[iok]),max(bin_mid[iok])],yrange=[ymin,ymax]
oplot, bin_mid[iok],pdf_wind_var_all[iok],linestyle=0,color=150, thick=2
oplot, bin_mid[iok],pdf_wind_var_allper[0,iok], linestyle=2, color=150, thick=1
oplot, bin_mid[iok],pdf_wind_var_allper[1,iok], linestyle=2, color=150, thick=1
diff = pdf_wind_var_all[iok] - tmpall[iok]
oplot, bin_mid[iok],diff[iok], linestyle=2, color=200, thick=2
xyouts, bin_mid[iok],ymax-100,strtrim(nb_binall[iok],2)
xyouts, bin_mid[iok],ymax-150,strtrim(ts_all[iok,1],2)
saveimage, 'FIGS_PDF/pdf_wind_bin'+strtrim(bin_size,2)+'_'+var_nam+'_all_'+exp+'.gif'
ENDIF


STOP
END
