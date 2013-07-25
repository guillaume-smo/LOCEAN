PRO plot_wind_vs_didt


; parametres
explist = ['KF']
parlist = ['COUPLED','FORCED']
basin   = 'SIO'
freq    = '6H'
datebeg = '19900101'
var_nam = 'dUV/dt'
var_uni = '(m.s-1/6h)'


; definition bin
bin_deb  = 15.
bin_fin  = 60.
bin_size = 5
bin_name = '10M WIND'
bin_unit = '(m/s)'
bin_num  = (bin_fin - bin_deb) / bin_size & print, bin_num
bin      = bin_deb+findgen(bin_num)*bin_size
bin_mid  = bin_deb + findgen(bin_num)*bin_size+bin_size/2. & print, bin_mid


; declarations
dpresdtint_cpl = fltarr(bin_num, 10000)
dwinddtint_cpl = fltarr(bin_num, 10000)
dpresdtint_frc = fltarr(bin_num, 10000)
dwinddtint_frc = fltarr(bin_num, 10000)
dpresdtdec_cpl = fltarr(bin_num, 10000)
dwinddtdec_cpl = fltarr(bin_num, 10000)
dpresdtdec_frc = fltarr(bin_num, 10000)
dwinddtdec_frc = fltarr(bin_num, 10000)
dpresdtall_cpl = fltarr(bin_num, 10000)
dwinddtall_cpl = fltarr(bin_num, 10000)
dpresdtall_frc = fltarr(bin_num, 10000)
dwinddtall_frc = fltarr(bin_num, 10000)


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


; calcul index didt
calcul_index_didt24h,d1_pres,d1_max_wnd,ind_int24h,ind_dec24h,d1_wind_smooth_24h,dwinddt,dpresdt,i_int24h,i_dec24h
iokall = [ind_int24h,ind_dec24h]


; declaration
pdf_wind_dpresdt_int = fltarr(bin_num)
pdf_wind_dpresdt_intper = fltarr(2,bin_num)
pdf_wind_dwinddt_int = fltarr(bin_num)
pdf_wind_dwinddt_intper = fltarr(2,bin_num)
pdf_wind_dpresdt_dec = fltarr(bin_num)
pdf_wind_dpresdt_decper = fltarr(2,bin_num)
pdf_wind_dwinddt_dec = fltarr(bin_num)
pdf_wind_dwinddt_decper = fltarr(2,bin_num)
pdf_wind_dpresdt_all = fltarr(bin_num)
pdf_wind_dpresdt_allper = fltarr(2,bin_num)
pdf_wind_dwinddt_all = fltarr(bin_num)
pdf_wind_dwinddt_allper = fltarr(2,bin_num)
nb_binint = intarr(bin_num)
nb_bindec = intarr(bin_num)
nb_binall = intarr(bin_num)


; calcul pdf
FOR i = 0, bin_num-2 DO BEGIN
  ibin_int = where(d1_wind_smooth_24h[ind_int24h] GE bin[i] AND d1_wind_smooth_24h[ind_int24h] LT bin[i+1])
  ibin_dec = where(d1_wind_smooth_24h[ind_dec24h] GE bin[i] AND d1_wind_smooth_24h[ind_dec24h] LT bin[i+1])
  ibin_all = where(d1_wind_smooth_24h[iokall] GE bin[i] AND d1_wind_smooth_24h[iokall] LT bin[i+1])
  ; sauvegarde intermediaire
  IF ibin_int[0] NE -1 THEN BEGIN
    IF param EQ 'COUPLED' THEN dpresdtint_cpl[i,0:n_elements(ibin_int)-1] = dpresdt[ind_int24h[ibin_int]] ELSE dpresdtint_frc[i,0:n_elements(ibin_int)-1] = dpresdt[ind_int24h[ibin_int]]
    IF param EQ 'COUPLED' THEN dwinddtint_cpl[i,0:n_elements(ibin_int)-1] = dwinddt[ind_int24h[ibin_int]] ELSE dwinddtint_frc[i,0:n_elements(ibin_int)-1] = dwinddt[ind_int24h[ibin_int]]
  ENDIF
  IF ibin_dec[0] NE -1 THEN BEGIN
    IF param EQ 'COUPLED' THEN dpresdtdec_cpl[i,0:n_elements(ibin_dec)-1] = dpresdt[ind_dec24h[ibin_dec]] ELSE dpresdtdec_frc[i,0:n_elements(ibin_dec)-1] = dpresdt[ind_dec24h[ibin_dec]]
    IF param EQ 'COUPLED' THEN dwinddtdec_cpl[i,0:n_elements(ibin_dec)-1] = dwinddt[ind_dec24h[ibin_dec]] ELSE dwinddtdec_frc[i,0:n_elements(ibin_dec)-1] = dwinddt[ind_dec24h[ibin_dec]]
  ENDIF
  IF ibin_all[0] NE -1 THEN BEGIN
    IF param EQ 'COUPLED' THEN dpresdtall_cpl[i,0:n_elements(ibin_all)-1] = dpresdt[iokall[ibin_all]] ELSE dpresdtall_frc[i,0:n_elements(ibin_all)-1] = dpresdt[iokall[ibin_all]]
    IF param EQ 'COUPLED' THEN dwinddtall_cpl[i,0:n_elements(ibin_all)-1] = dwinddt[iokall[ibin_all]] ELSE dwinddtall_frc[i,0:n_elements(ibin_all)-1] = dwinddt[iokall[ibin_all]]
  ENDIF
  ; moyenne
  IF n_elements(ibin_int) GT 10 THEN pdf_wind_dpresdt_int[i] = mean(dpresdt[ind_int24h[ibin_int]],/nan)
  IF n_elements(ibin_int) GT 10 THEN pdf_wind_dwinddt_int[i] = mean(dwinddt[ind_int24h[ibin_int]],/nan)
  IF n_elements(ibin_dec) GT 10 THEN pdf_wind_dpresdt_dec[i] = mean(dpresdt[ind_dec24h[ibin_dec]],/nan)
  IF n_elements(ibin_dec) GT 10 THEN pdf_wind_dwinddt_dec[i] = mean(dwinddt[ind_dec24h[ibin_dec]],/nan)
  IF n_elements(ibin_all) GT 10 THEN pdf_wind_dpresdt_all[i] = mean(dpresdt[iokall[ibin_all]],/nan)
  IF n_elements(ibin_all) GT 10 THEN pdf_wind_dwinddt_all[i] = mean(dwinddt[iokall[ibin_all]],/nan)
  ; percentile
  IF n_elements(ibin_int) GT 10 THEN pdf_wind_dpresdt_intper[*,i] = percentile(dpresdt[ind_int24h[ibin_int]],0.25)
  IF n_elements(ibin_int) GT 10 THEN pdf_wind_dwinddt_intper[*,i] = percentile(dwinddt[ind_int24h[ibin_int]],0.25)
  IF n_elements(ibin_dec) GT 10 THEN pdf_wind_dpresdt_decper[*,i] = percentile(dpresdt[ind_dec24h[ibin_dec]],0.25)
  IF n_elements(ibin_dec) GT 10 THEN pdf_wind_dwinddt_decper[*,i] = percentile(dwinddt[ind_dec24h[ibin_dec]],0.25)
  IF n_elements(ibin_all) GT 10 THEN pdf_wind_dpresdt_allper[*,i] = percentile(dpresdt[iokall[ibin_all]],0.25)
  IF n_elements(ibin_all) GT 10 THEN pdf_wind_dwinddt_allper[*,i] = percentile(dwinddt[iokall[ibin_all]],0.25)
  ; nombre de points par bin
  IF ibin_int[0] NE -1 THEN nb_binint[i] = n_elements(ibin_int) ELSE nb_binint[i] = 0
  IF ibin_dec[0] NE -1 THEN nb_bindec[i] = n_elements(ibin_dec) ELSE nb_bindec[i] = 0
  IF ibin_all[0] NE -1 THEN nb_binall[i] = n_elements(ibin_all) ELSE nb_binall[i] = 0
ENDFOR
print, long(bin_mid)
print, nb_binint
print, nb_bindec


; test significativite
ts_dpresdtint = fltarr(bin_num,2)
ts_dpresdtdec = fltarr(bin_num,2)
ts_dwinddtint = fltarr(bin_num,2)
ts_dwinddtdec = fltarr(bin_num,2)
IF param EQ 'FORCED' THEN BEGIN
  FOR i = 0, bin_num-2 DO BEGIN
    iok = where(dpresdtint_cpl[i,*] NE 0 AND dpresdtint_frc[i,*], cntok)
    IF iok[0] NE -1 THEN ts_dpresdtint[i,*] = RS_TEST(reform(dpresdtint_cpl[i,where(dpresdtint_cpl[i,*] NE 0)]),reform(dpresdtint_frc[i,where(dpresdtint_frc[i,*] NE 0)]))
    iok = where(dpresdtdec_cpl[i,*] NE 0 AND dpresdtdec_frc[i,*], cntok)
    IF iok[0] NE -1 THEN ts_dpresdtdec[i,*] = RS_TEST(reform(dpresdtdec_cpl[i,where(dpresdtdec_cpl[i,*] NE 0)]),reform(dpresdtdec_frc[i,where(dpresdtdec_frc[i,*] NE 0)]))
    iok = where(dwinddtint_cpl[i,*] NE 0 AND dwinddtint_frc[i,*], cntok)
    IF iok[0] NE -1 THEN ts_dwinddtint[i,*] = RS_TEST(reform(dwinddtint_cpl[i,where(dwinddtint_cpl[i,*] NE 0)]),reform(dwinddtint_frc[i,where(dwinddtint_frc[i,*] NE 0)]))
    iok = where(dwinddtdec_cpl[i,*] NE 0 AND dwinddtdec_frc[i,*], cntok)
    IF iok[0] NE -1 THEN ts_dwinddtdec[i,*] = RS_TEST(reform(dwinddtdec_cpl[i,where(dwinddtdec_cpl[i,*] NE 0)]),reform(dwinddtdec_frc[i,where(dwinddtdec_frc[i,*] NE 0)]))
  ENDFOR
ENDIF


; bar plot par bin
;window, 0
;bar_plot, pdf_wind_dpresdt_int, back=255, /outline, barname=bin_mid
;saveimage, 'FIGS_PDF2/pdf_wind_bin'+strtrim(bin_size,2)+'_dpresdt_int'+'_'+expname+'.gif'
;window, 0
;bar_plot, pdf_wind_dwinddt_int, back=255, /outline, barname=bin_mid
;saveimage, 'FIGS_PDF2/pdf_wind_bin'+strtrim(bin_size,2)+'_dwinddt_int'+'_'+expname+'.gif'
;window, 2
;bar_plot, pdf_wind_dpresdt_dec, back=255, /outline, barname=bin_mid
;saveimage, 'FIGS_PDF2/pdf_wind_bin'+strtrim(bin_size,2)+'_dpresdt_dec'+'_'+expname+'.gif'
;window, 1
;bar_plot, pdf_wind_dwinddt_dec, back=255, /outline, barname=bin_mid
;saveimage, 'FIGS_PDF2/pdf_wind_bin'+strtrim(bin_size,2)+'_dwinddt_dec'+'_'+expname+'.gif'


; line plot par bin

IF param EQ 'COUPLED' THEN BEGIN
window,0
ymin = 0.5 & ymax = 2.5
iok = where(pdf_wind_dwinddt_int NE 0.)
plot, bin_mid[iok],pdf_wind_dwinddt_int[iok], xstyle=1, ystyle=1, charsize=1.5, $
xrange=[bin[min(iok)],bin[max(iok)+1]],xtitle=bin_name+' '+bin_unit,ytitle=var_nam+' '+var_uni,yrange=[ymin,ymax]
oplot, bin_mid[iok],pdf_wind_dwinddt_int[iok],linestyle=0,color=50 ,thick=2
xyouts, 0.85, 0.85, 'CPL', /normal, color=50, charsize=1.5
oplot, bin_mid[iok],pdf_wind_dwinddt_intper[0,iok], linestyle=2, color=50, thick=1
oplot, bin_mid[iok],pdf_wind_dwinddt_intper[1,iok], linestyle=2, color=50, thick=1
tmpint = pdf_wind_dwinddt_int


window,2
ymin = -2.5 & ymax = 2.5
iok = where(pdf_wind_dwinddt_int NE 0.)
plot, bin_mid[iok],pdf_wind_dwinddt_int[iok], xstyle=1, ystyle=1, charsize=1.5, $
xrange=[bin[min(iok)],bin[max(iok)+1]],xtitle=bin_name+' '+bin_unit,ytitle=var_nam+' '+var_uni,yrange=[ymin,ymax]
oplot, bin_mid[iok],pdf_wind_dwinddt_int[iok],linestyle=0,color=50 ,thick=2
oplot, bin_mid[iok],pdf_wind_dwinddt_dec[iok],linestyle=0,color=250,thick=2
xyouts, 0.15, 0.60, 'COUPLED - INTEN', /normal, color=50, charsize=1.5
xyouts, 0.15, 0.45, 'COUPLED - DECAY', /normal, color=250, charsize=1.5
;oplot, bin_mid[iok],pdf_wind_var_intper[0,iok], linestyle=2, color=50, thick=1
;oplot, bin_mid[iok],pdf_wind_var_intper[1,iok], linestyle=2, color=50, thick=1
tmpint = pdf_wind_dwinddt_int
ENDIF

IF param EQ 'FORCED' THEN BEGIN
wset,0
ymin = 0.5 & ymax = 2.5
iok = where(tmpint NE 0. )
plot, bin_mid[iok],pdf_wind_dwinddt_int[iok], xstyle=1, ystyle=1, charsize=1.5, /noerase, $
xrange=[bin[min(iok)],bin[max(iok)+1]],yrange=[ymin,ymax]
oplot, bin_mid[iok],pdf_wind_dwinddt_int[iok],linestyle=0,color=250,thick=2
xyouts, 0.85, 0.80, 'FRC', /normal, color=250, charsize=1.5
oplot, bin_mid[iok],pdf_wind_dwinddt_intper[0,iok], linestyle=2, color=250, thick=1
oplot, bin_mid[iok],pdf_wind_dwinddt_intper[1,iok], linestyle=2, color=250, thick=1
;diff = pdf_wind_var_int[iok] - tmpint[iok]
;oplot, bin_mid[iok],diff[iok], linestyle=2, color=200, thick=2
;xyouts, bin_mid[iok],ymax-100,strtrim(nb_binint[iok],2)
;xyouts, bin_mid[iok],ymax-150,strtrim(ts_int[iok,1],2)
;saveimage, 'FIGS_PDF/pdf_wind_bin'+strtrim(bin_size,2)+'_dUVdt_int_'+exp+'.gif'

wset,2
iok = where(tmpint NE 0. )
plot, bin_mid[iok],pdf_wind_dwinddt_int[iok], xstyle=1, ystyle=1, /noerase, $
xrange=[bin[min(iok)],bin[max(iok)+1]],yrange=[ymin,ymax]
oplot, bin_mid[iok],pdf_wind_dwinddt_int[iok],linestyle=0,color=100,thick=2
oplot, bin_mid[iok],pdf_wind_dwinddt_dec[iok],linestyle=0,color=200,thick=2
xyouts, 0.15, 0.55, 'FORCED - INTEN', /normal, color=100, charsize=1.5
xyouts, 0.15, 0.40, 'FORCED - DECAY', /normal, color=200, charsize=1.5
;oplot, bin_mid[iok],pdf_wind_var_intper[0,iok], linestyle=2, color=150, thick=1
;oplot, bin_mid[iok],pdf_wind_var_intper[1,iok], linestyle=2, color=150, thick=1
;diff = pdf_wind_var_int[iok] - tmpint[iok]
;oplot, bin_mid[iok],diff[iok], linestyle=2, color=200, thick=2
;xyouts, bin_mid[iok],ymax-100,strtrim(nb_binint[iok],2)
;xyouts, bin_mid[iok],ymax-150,strtrim(ts_int[iok,1],2)
oplot, [15,50], [0,0]
;saveimage, 'FIGS_PDF/pdf_wind_bin'+strtrim(bin_size,2)+'_dUVdt_int+dec_'+exp+'.gif'
ENDIF

GOTO, JUMP

IF param EQ 'COUPLED' THEN window,1 ELSE wset,1
iok = where(pdf_wind_dwinddt_int NE 0.)
IF param EQ 'COUPLED' THEN plot, bin_mid[iok],pdf_wind_dwinddt_int[iok], $
xrange=[min(bin_mid),max(bin_mid)],xtitle=bin_unit,ytitle='m.s-1/6h',yrange=[0,3],linestyle=0
IF param EQ 'FORCED' THEN plot, bin_mid[iok],pdf_wind_dwinddt_int[iok],xrange=[min(bin_mid),max(bin_mid)],/noerase,linestyle=2,yrange=[0,3]
oplot, bin_mid[iok],pdf_wind_dwinddt_intper[0,iok], linestyle=1
oplot, bin_mid[iok],pdf_wind_dwinddt_intper[1,iok], linestyle=1
IF param EQ 'COUPLED' THEN xyouts, bin_mid[iok],2.9,strtrim(nb_binint[iok],2)
IF param EQ 'FORCED' THEN  xyouts, bin_mid[iok],2.8,strtrim(nb_binint[iok],2)
IF param EQ 'FORCED' THEN  xyouts, bin_mid[iok],2.7,strtrim(ts_dwinddtint[iok,1],2)
IF param EQ 'FORCED' THEN saveimage, 'FIGS_PDF/pdf_wind_bin'+strtrim(bin_size,2)+'_dwinddt_int_'+exp+'.gif'


IF param EQ 'COUPLED' THEN window,2 ELSE wset,2
iok = where(pdf_wind_dpresdt_int NE 0.)
IF param EQ 'COUPLED' THEN plot, bin_mid[iok],pdf_wind_dpresdt_int[iok], $
xrange=[min(bin_mid),max(bin_mid)],xtitle=bin_unit,ytitle='hPa/6h',yrange=[-6,0]
IF param EQ 'FORCED' THEN plot, bin_mid[iok],pdf_wind_dpresdt_int[iok],xrange=[min(bin_mid),max(bin_mid)],/noerase,linestyle=2,yrange=[-6,0]
oplot, bin_mid[iok],pdf_wind_dpresdt_intper[0,iok], linestyle=1
oplot, bin_mid[iok],pdf_wind_dpresdt_intper[1,iok], linestyle=1
IF param EQ 'COUPLED' THEN xyouts, bin_mid[iok],-5.4,strtrim(nb_binint[iok],2)
IF param EQ 'FORCED' THEN xyouts, bin_mid[iok],-5.6,strtrim(nb_binint[iok],2)
IF param EQ 'FORCED' THEN xyouts, bin_mid[iok],-5.8,strtrim(ts_dpresdtint[iok,1],2)
IF param EQ 'FORCED' THEN saveimage, 'FIGS_PDF/pdf_wind_bin'+strtrim(bin_size,2)+'_dpresdt_int_'+exp+'.gif'


IF param EQ 'COUPLED' THEN window,3 ELSE wset,3
iok = where(pdf_wind_dwinddt_dec NE 0.)
IF param EQ 'COUPLED' THEN plot, bin_mid[iok],pdf_wind_dwinddt_dec[iok], $
xrange=[min(bin_mid),max(bin_mid)],xtitle=bin_unit,ytitle='m.s-1/6h',yrange=[-3,0]
IF param EQ 'FORCED' THEN plot, bin_mid[iok],pdf_wind_dwinddt_dec[iok],xrange=[min(bin_mid),max(bin_mid)],/noerase,linestyle=2,yrange=[-3,0]
oplot, bin_mid[iok],pdf_wind_dwinddt_decper[0,iok], linestyle=1
oplot, bin_mid[iok],pdf_wind_dwinddt_decper[1,iok], linestyle=1
IF param EQ 'COUPLED' THEN xyouts, bin_mid[iok],-2.9,strtrim(nb_bindec[iok],2)
IF param EQ 'FORCED' THEN xyouts, bin_mid[iok],-2.8,strtrim(nb_bindec[iok],2)
IF param EQ 'FORCED' THEN xyouts, bin_mid[iok],-2.7,strtrim(ts_dwinddtdec[iok,1],2)
IF param EQ 'FORCED' THEN saveimage, 'FIGS_PDF/pdf_wind_bin'+strtrim(bin_size,2)+'_dwinddt_dec_'+exp+'.gif'


IF param EQ 'COUPLED' THEN window,4 ELSE wset,4
iok = where(pdf_wind_dpresdt_dec NE 0.)
IF param EQ 'COUPLED' THEN plot, bin_mid[iok],pdf_wind_dpresdt_dec[iok], $
xrange=[min(bin_mid),max(bin_mid)],xtitle=bin_unit,ytitle='hPa/6h',yrange=[0,5]
IF param EQ 'FORCED' THEN plot, bin_mid[iok],pdf_wind_dpresdt_dec[iok],xrange=[min(bin_mid),max(bin_mid)],/noerase,linestyle=2,yrange=[0,5]
oplot, bin_mid[iok],pdf_wind_dpresdt_decper[0,iok], linestyle=1
oplot, bin_mid[iok],pdf_wind_dpresdt_decper[1,iok], linestyle=1
IF param EQ 'COUPLED' THEN xyouts, bin_mid[iok],4.8,strtrim(nb_bindec[iok],2)
IF param EQ 'FORCED' THEN xyouts, bin_mid[iok],4.6,strtrim(nb_bindec[iok],2)
IF param EQ 'FORCED' THEN xyouts, bin_mid[iok],4.4,strtrim(ts_dpresdtdec[iok,1],2)
IF param EQ 'FORCED' THEN saveimage, 'FIGS_PDF/pdf_wind_bin'+strtrim(bin_size,2)+'_dpresdt_dec_'+exp+'.gif'

JUMP:
ENDFOR
ENDFOR
stop
END
