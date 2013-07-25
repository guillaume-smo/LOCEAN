PRO plot_pres_vs_didt


; parametres
explist = ['BMJ','KF']
parlist = ['COUPLED','FORCED']
basin   = 'SIO'
freq    = '6H'
datebeg = '19900101'


; definition bin
bin_deb  = 930.
bin_fin  = 1000.
bin_size = 10
bin_unit = '(hPa)'
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


; declaration
pdf_pres_dpresdt_int = fltarr(bin_num)
pdf_pres_dpresdt_intper = fltarr(2,bin_num)
pdf_pres_dwinddt_int = fltarr(bin_num)
pdf_pres_dwinddt_intper = fltarr(2,bin_num)
pdf_pres_dpresdt_dec = fltarr(bin_num)
pdf_pres_dpresdt_decper = fltarr(2,bin_num)
pdf_pres_dwinddt_dec = fltarr(bin_num)
pdf_pres_dwinddt_decper = fltarr(2,bin_num)
nb_binint = intarr(bin_num)
nb_bindec = intarr(bin_num)


; calcul pdf
FOR i = 0, bin_num-2 DO BEGIN
  ibin_int = where(d1_pres[ind_int24h] GE bin[i] AND d1_pres[ind_int24h] LT bin[i+1])
  ibin_dec = where(d1_pres[ind_dec24h] GE bin[i] AND d1_pres[ind_dec24h] LT bin[i+1])
  ; sauvegarde intermediaire
  IF ibin_int[0] NE -1 THEN BEGIN
    IF param EQ 'COUPLED' THEN dpresdtint_cpl[i,0:n_elements(ibin_int)-1] = dpresdt[ind_int24h[ibin_int]] ELSE dpresdtint_frc[i,0:n_elements(ibin_int)-1] = dpresdt[ind_int24h[ibin_int]]
    IF param EQ 'COUPLED' THEN dwinddtint_cpl[i,0:n_elements(ibin_int)-1] = dwinddt[ind_int24h[ibin_int]] ELSE dwinddtint_frc[i,0:n_elements(ibin_int)-1] = dwinddt[ind_int24h[ibin_int]]
  ENDIF
  IF ibin_dec[0] NE -1 THEN BEGIN
    IF param EQ 'COUPLED' THEN dpresdtdec_cpl[i,0:n_elements(ibin_dec)-1] = dpresdt[ind_dec24h[ibin_dec]] ELSE dpresdtdec_frc[i,0:n_elements(ibin_dec)-1] = dpresdt[ind_dec24h[ibin_dec]]
    IF param EQ 'COUPLED' THEN dwinddtdec_cpl[i,0:n_elements(ibin_dec)-1] = dwinddt[ind_dec24h[ibin_dec]] ELSE dwinddtdec_frc[i,0:n_elements(ibin_dec)-1] = dwinddt[ind_dec24h[ibin_dec]]
  ENDIF
  ; moyenne
  IF n_elements(ibin_int) GT 1 THEN pdf_pres_dpresdt_int[i] = mean(dpresdt[ind_int24h[ibin_int]],/nan)
  IF n_elements(ibin_int) GT 1 THEN pdf_pres_dwinddt_int[i] = mean(dwinddt[ind_int24h[ibin_int]],/nan)
  IF n_elements(ibin_dec) GT 1 THEN pdf_pres_dpresdt_dec[i] = mean(dpresdt[ind_dec24h[ibin_dec]],/nan)
  IF n_elements(ibin_dec) GT 1 THEN pdf_pres_dwinddt_dec[i] = mean(dwinddt[ind_dec24h[ibin_dec]],/nan)
  ; percentile
  IF n_elements(ibin_int) GT 1 THEN pdf_pres_dpresdt_intper[*,i] = percentile(dpresdt[ind_int24h[ibin_int]],0.25)
  IF n_elements(ibin_int) GT 1 THEN pdf_pres_dwinddt_intper[*,i] = percentile(dwinddt[ind_int24h[ibin_int]],0.25)
  IF n_elements(ibin_dec) GT 1 THEN pdf_pres_dpresdt_decper[*,i] = percentile(dpresdt[ind_dec24h[ibin_dec]],0.25)
  IF n_elements(ibin_dec) GT 1 THEN pdf_pres_dwinddt_decper[*,i] = percentile(dwinddt[ind_dec24h[ibin_dec]],0.25)
  ; nombre de points par bin
  IF ibin_int[0] NE -1 THEN nb_binint[i] = n_elements(ibin_int) ELSE nb_binint[i] = 0
  IF ibin_dec[0] NE -1 THEN nb_bindec[i] = n_elements(ibin_dec) ELSE nb_bindec[i] = 0
ENDFOR


; test significativite
ts_int = fltarr(bin_num,2)
ts_dec = fltarr(bin_num,2)
IF param EQ 'FORCED' THEN BEGIN
  FOR i = 0, bin_num-2 DO BEGIN
    iok = where(dpresdtint_cpl[i,*] NE 0, cntok)
    IF iok[0] NE -1 THEN ts_int[i,*] = RS_TEST(reform(dpresdtint_cpl[i,where(dpresdtint_cpl[i,*] NE 0)]),reform(dpresdtint_frc[i,where(dpresdtint_frc[i,*] NE 0)]))
    iok = where(dpresdtdec_cpl[i,*] NE 0, cntok)
    IF iok[0] NE -1 THEN ts_dec[i,*] = RS_TEST(reform(dpresdtdec_cpl[i,where(dpresdtdec_cpl[i,*] NE 0)]),reform(dpresdtdec_frc[i,where(dpresdtdec_frc[i,*] NE 0)]))
  ENDFOR
ENDIF


; bar plot par bin
;window, 0
;bar_plot, pdf_pres_dpresdt_int, back=255, /outline, barname=bin_mid
;saveimage, 'FIGS_PDF2/pdf_pres_bin'+strtrim(bin_size,2)+'_dpresdt_int'+'_'+expname+'.gif'
;bar_plot, pdf_pres_dwinddt_int, back=255, /outline, barname=bin_mid
;saveimage, 'FIGS_PDF2/pdf_pres_bin'+strtrim(bin_size,2)+'_dwinddt_int'+'_'+expname+'.gif'
;bar_plot, pdf_pres_dpresdt_dec, back=255, /outline, barname=bin_mid
;saveimage, 'FIGS_PDF2/pdf_pres_bin'+strtrim(bin_size,2)+'_dpresdt_dec'+'_'+expname+'.gif'
;bar_plot, pdf_pres_dwinddt_dec, back=255, /outline, barname=bin_mid
;saveimage, 'FIGS_PDF2/pdf_pres_bin'+strtrim(bin_size,2)+'_dwinddt_dec'+'_'+expname+'.gif'


; line plot par bin
IF param EQ 'COUPLED' THEN window,1 ELSE wset,1
iok = where(pdf_pres_dwinddt_int NE 0.)
IF param EQ 'COUPLED' THEN plot, bin_mid[iok],pdf_pres_dwinddt_int[iok], $
xrange=[min(bin_mid),max(bin_mid)],xtitle=bin_unit,ytitle='m.s-1/6h',yrange=[0,3],linestyle=0
IF param EQ 'FORCED' THEN plot, bin_mid[iok],pdf_pres_dwinddt_int[iok],xrange=[min(bin_mid),max(bin_mid)],/noerase,linestyle=2,yrange=[0,3]
oplot, bin_mid[iok],pdf_pres_dwinddt_intper[0,iok], linestyle=1
oplot, bin_mid[iok],pdf_pres_dwinddt_intper[1,iok], linestyle=1
IF param EQ 'COUPLED' THEN xyouts, bin_mid[iok],2.9,strtrim(nb_binint[iok],2)
IF param EQ 'FORCED' THEN  xyouts, bin_mid[iok],2.8,strtrim(nb_binint[iok],2)
IF param EQ 'FORCED' THEN  xyouts, bin_mid[iok],2.7,strtrim(ts_int[where(ts_int[*,1] NE 0),1],2)
IF param EQ 'FORCED' THEN saveimage, 'FIGS_PDF2/pdf_pres_bin'+strtrim(bin_size,2)+'_dwinddt_int_'+exp+'.gif'


IF param EQ 'COUPLED' THEN window,2 ELSE wset,2
iok = where(pdf_pres_dpresdt_int NE 0.)
IF param EQ 'COUPLED' THEN plot, bin_mid[iok],pdf_pres_dpresdt_int[iok], $
xrange=[min(bin_mid),max(bin_mid)],xtitle=bin_unit,ytitle='hPa/6h',yrange=[-6,0]
IF param EQ 'FORCED' THEN plot, bin_mid[iok],pdf_pres_dpresdt_int[iok],xrange=[min(bin_mid),max(bin_mid)],/noerase,linestyle=2,yrange=[-6,0]
oplot, bin_mid[iok],pdf_pres_dpresdt_intper[0,iok], linestyle=1
oplot, bin_mid[iok],pdf_pres_dpresdt_intper[1,iok], linestyle=1
IF param EQ 'COUPLED' THEN xyouts, bin_mid[iok],-5.4,strtrim(nb_binint[iok],2)
IF param EQ 'FORCED' THEN xyouts, bin_mid[iok],-5.6,strtrim(nb_binint[iok],2)
IF param EQ 'FORCED' THEN xyouts, bin_mid[iok],-5.8,strtrim(ts_int[where(ts_int[*,1] NE 0),1],2)
IF param EQ 'FORCED' THEN saveimage, 'FIGS_PDF2/pdf_pres_bin'+strtrim(bin_size,2)+'_dpresdt_int_'+exp+'.gif'


IF param EQ 'COUPLED' THEN window,3 ELSE wset,3
iok = where(pdf_pres_dwinddt_dec NE 0.)
IF param EQ 'COUPLED' THEN plot, bin_mid[iok],pdf_pres_dwinddt_dec[iok], $
xrange=[min(bin_mid),max(bin_mid)],xtitle=bin_unit,ytitle='m.s-1/6h',yrange=[-3,0]
IF param EQ 'FORCED' THEN plot, bin_mid[iok],pdf_pres_dwinddt_dec[iok],xrange=[min(bin_mid),max(bin_mid)],/noerase,linestyle=2,yrange=[-3,0]
oplot, bin_mid[iok],pdf_pres_dwinddt_decper[0,iok], linestyle=1
oplot, bin_mid[iok],pdf_pres_dwinddt_decper[1,iok], linestyle=1
IF param EQ 'COUPLED' THEN xyouts, bin_mid[iok],-2.9,strtrim(nb_bindec[iok],2)
IF param EQ 'FORCED' THEN xyouts, bin_mid[iok],-2.8,strtrim(nb_bindec[iok],2)
IF param EQ 'FORCED' THEN xyouts, bin_mid[iok],-2.7,strtrim(ts_dec[where(ts_dec[*,1] NE 0),1],2)
IF param EQ 'FORCED' THEN saveimage, 'FIGS_PDF2/pdf_pres_bin'+strtrim(bin_size,2)+'_dwinddt_dec_'+exp+'.gif'


IF param EQ 'COUPLED' THEN window,4 ELSE wset,4
iok = where(pdf_pres_dpresdt_dec NE 0.)
IF param EQ 'COUPLED' THEN plot, bin_mid[iok],pdf_pres_dpresdt_dec[iok], $
xrange=[min(bin_mid),max(bin_mid)],xtitle=bin_unit,ytitle='hPa/6h',yrange=[0,5]
IF param EQ 'FORCED' THEN plot, bin_mid[iok],pdf_pres_dpresdt_dec[iok],xrange=[min(bin_mid),max(bin_mid)],/noerase,linestyle=2,yrange=[0,5]
oplot, bin_mid[iok],pdf_pres_dpresdt_decper[0,iok], linestyle=1
oplot, bin_mid[iok],pdf_pres_dpresdt_decper[1,iok], linestyle=1
IF param EQ 'COUPLED' THEN xyouts, bin_mid[iok],4.8,strtrim(nb_bindec[iok],2)
IF param EQ 'FORCED' THEN xyouts, bin_mid[iok],4.6,strtrim(nb_bindec[iok],2)
IF param EQ 'FORCED' THEN xyouts, bin_mid[iok],4.4,strtrim(ts_dec[where(ts_dec[*,1] NE 0),1],2)
IF param EQ 'FORCED' THEN saveimage, 'FIGS_PDF2/pdf_pres_bin'+strtrim(bin_size,2)+'_dpresdt_dec_'+exp+'.gif'


ENDFOR
ENDFOR

END
