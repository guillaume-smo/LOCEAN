PRO plot_wind_vs_sst


; parametres
explist = ['KF']
parlist = ['COUPLED','FORCED']
basin   = 'SIO'
freq    = '6H'
datebeg = '19900101'
var_nam = 'SST'
var_uni = '(degC)'

; definition bin
bin_deb  = 15.
bin_fin  = 60.
bin_size = 5
bin_name = '10M WIND'
bin_unit = '(m/s)'
bin_num  = (bin_fin - bin_deb) / bin_size & print, bin_num
bin      = bin_deb + findgen(bin_num)*bin_size
bin_mid  = bin_deb + findgen(bin_num)*bin_size + bin_size/2. & print, bin_mid


; declarations
varint_cpl = fltarr(bin_num, 10000)
varint_frc = fltarr(bin_num, 10000)
vardec_cpl = fltarr(bin_num, 10000)
vardec_frc = fltarr(bin_num, 10000)


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
restore, pathin+var_nam+'_1D_200km_'+expname+'_1D_IO_'+datebeg+'-'+dateend+'.dat', /VERBOSE
var_bef = d1_var_before & help, var_bef ; wsc
restore, pathin+var_nam+'_2D_200km_'+expname+'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat', /VERBOSE
d1_var = m_mean(m_mean(temporary(dxy_var_wsc_rot),dim=4,/nan),dim=3,/nan) - 273.15
befor = d1_var_before - 273.15
durin = d1_var_during - 273.15
after = d1_var_after  - 273.15


; calcul index didt
calcul_index_didt24h,d1_pres,d1_max_wnd,ind_int24h,ind_dec24h,d1_wind_smooth_24h,dwinddt,dpresdt,i_int24h,i_dec24h
iokint = ind_int24h[where(finite(d1_var[ind_int24h]) EQ 1)] & help, iokint
iokdec = ind_dec24h[where(finite(d1_var[ind_dec24h]) EQ 1)] & help, iokdec
iokall = [iokint,iokdec] & help, iokall


; declaration
pdf_wind_var_int = fltarr(bin_num)
pdf_wind_var_intper = fltarr(2,bin_num)
pdf_wind_var_dec = fltarr(bin_num)
pdf_wind_var_decper = fltarr(2,bin_num)
pdf_wind_var_all = fltarr(bin_num)
pdf_wind_var_allper = fltarr(2,bin_num)
pdf_wind_befor_int = fltarr(bin_num)
pdf_wind_befor_dec = fltarr(bin_num)
pdf_wind_befor_all = fltarr(bin_num)
pdf_wind_durin_int = fltarr(bin_num)
pdf_wind_durin_dec = fltarr(bin_num)
pdf_wind_durin_all = fltarr(bin_num)
pdf_wind_after_int = fltarr(bin_num)
pdf_wind_after_dec = fltarr(bin_num)
pdf_wind_after_all = fltarr(bin_num)
pdf_wind_after_allper = fltarr(2,bin_num)

nb_binint = intarr(bin_num)
nb_bindec = intarr(bin_num)
nb_binall = intarr(bin_num)


; calcul pdf
FOR i = 0, bin_num-2 DO BEGIN
  ibin_int = where(d1_wind_smooth_24h[iokint] GE bin[i] AND d1_wind_smooth_24h[iokint] LT bin[i+1])
  ibin_dec = where(d1_wind_smooth_24h[iokdec] GE bin[i] AND d1_wind_smooth_24h[iokdec] LT bin[i+1])
  ibin_all = where(d1_wind_smooth_24h[iokall] GE bin[i] AND d1_wind_smooth_24h[iokall] LT bin[i+1])
  ; sauvegarde intermediaire
  IF ibin_int[0] NE -1 THEN BEGIN
    IF param EQ 'COUPLED' THEN varint_cpl[i,0:n_elements(ibin_int)-1] = d1_var[iokint[ibin_int]] ELSE varint_frc[i,0:n_elements(ibin_int)-1] = d1_var[iokint[ibin_int]]
  ENDIF
  IF ibin_dec[0] NE -1 THEN BEGIN
    IF param EQ 'COUPLED' THEN vardec_cpl[i,0:n_elements(ibin_dec)-1] = d1_var[iokdec[ibin_dec]] ELSE vardec_frc[i,0:n_elements(ibin_dec)-1] = d1_var[iokdec[ibin_dec]]
  ENDIF
  ; moyenne
  IF n_elements(ibin_int) GT 10 THEN pdf_wind_var_int[i] = mean(d1_var[iokint[ibin_int]],/nan)
  IF n_elements(ibin_dec) GT 10 THEN pdf_wind_var_dec[i] = mean(d1_var[iokdec[ibin_dec]],/nan)
  IF n_elements(ibin_all) GT 10 THEN pdf_wind_var_all[i] = mean(d1_var[iokall[ibin_all]],/nan)
  IF n_elements(ibin_int) GT 10 THEN pdf_wind_befor_int[i] = mean(befor[iokint[ibin_int]],/nan)
  IF n_elements(ibin_dec) GT 10 THEN pdf_wind_befor_dec[i] = mean(befor[iokdec[ibin_dec]],/nan)
  IF n_elements(ibin_all) GT 10 THEN pdf_wind_befor_all[i] = mean(befor[iokall[ibin_all]],/nan)
  IF n_elements(ibin_int) GT 10 THEN pdf_wind_durin_int[i] = mean(durin[iokint[ibin_int]],/nan)
  IF n_elements(ibin_dec) GT 10 THEN pdf_wind_durin_dec[i] = mean(durin[iokdec[ibin_dec]],/nan)
  IF n_elements(ibin_all) GT 10 THEN pdf_wind_durin_all[i] = mean(durin[iokall[ibin_all]],/nan)
  IF n_elements(ibin_int) GT 10 THEN pdf_wind_after_int[i] = mean(after[iokint[ibin_int]],/nan)
  IF n_elements(ibin_dec) GT 10 THEN pdf_wind_after_dec[i] = mean(after[iokdec[ibin_dec]],/nan)
  IF n_elements(ibin_all) GT 10 THEN pdf_wind_after_all[i] = mean(after[iokall[ibin_all]],/nan)
  ; percentile
  IF n_elements(ibin_int) GT 10 THEN pdf_wind_var_intper[*,i] = percentile(d1_var[iokint[ibin_int]],0.25)
  IF n_elements(ibin_dec) GT 10 THEN pdf_wind_var_decper[*,i] = percentile(d1_var[iokdec[ibin_dec]],0.25)
  IF n_elements(ibin_all) GT 10 THEN pdf_wind_var_allper[*,i] = percentile(d1_var[iokall[ibin_all]],0.25)
  IF n_elements(ibin_all) GT 10 THEN pdf_wind_after_allper[*,i] = percentile(after[iokall[ibin_all]],0.25)
  ; nombre de points par bin
  IF ibin_int[0] NE -1 THEN nb_binint[i] = n_elements(ibin_int) ELSE nb_binint[i] = 0
  IF ibin_dec[0] NE -1 THEN nb_bindec[i] = n_elements(ibin_dec) ELSE nb_bindec[i] = 0
  IF ibin_all[0] NE -1 THEN nb_binall[i] = n_elements(ibin_all) ELSE nb_binall[i] = 0
ENDFOR
print, long(bin_mid)
print, nb_binint
print, nb_bindec


; test significativite
ts_cpl = fltarr(bin_num,2)*!Values.F_NAN
IF param EQ 'COUPLED' THEN BEGIN
  FOR i = 0, bin_num-1 DO BEGIN
    IF pdf_wind_var_int[i] NE 0. AND pdf_wind_var_dec[i] NE 0. THEN BEGIN
      iok = where(varint_cpl[i,*] NE 0 AND vardec_cpl[i,*] NE 0, cntok) & help, cntok
      IF iok[0] NE -1 THEN ts_cpl[i,*] = RS_TEST(reform(varint_cpl[i,where(varint_cpl[i,*] NE 0)]),reform(vardec_cpl[i,where(vardec_cpl[i,*] NE 0)]))
    ENDIF
  ENDFOR
print, long(bin_mid)
print, ts_cpl[*,1]
ENDIF


; bar plot par bin
;window, 0
;bar_plot, pdf_wind_var_int, back=255, /outline, barname=bin_mid
;saveimage, 'FIGS_PDF/pdf_wind_bin'+strtrim(bin_size,2)+'_'+var_nam+'_int'+'_'+expname+'.gif'
;window,1
;bar_plot, pdf_wind_var_dec, back=255, /outline, barname=bin_mid
;saveimage, 'FIGS_PDF/pdf_wind_bin'+strtrim(bin_size,2)+'_'+var_nam+'_dec'+'_'+expname+'.gif'


; line plot par bin
IF param EQ 'COUPLED' THEN BEGIN
  window,0
  ymin = 26 & ymax = 31
  iok = where(pdf_wind_var_int NE 0.)
  plot, bin_mid[iok],pdf_wind_var_int[iok], xstyle=1, ystyle=1, $
  xrange=[bin[min(iok)],bin[max(iok)+1]],xtitle=bin_name+' '+bin_unit,ytitle=var_nam+' '+var_uni,yrange=[ymin,ymax],linestyle=0, thick=2, color=0
  oplot, bin_mid[iok],pdf_wind_var_int[iok],linestyle=0,thick=2, color=50
  oplot, bin_mid[iok],pdf_wind_var_dec[iok],linestyle=0, thick=2, color=200
;  oplot, bin_mid[iok],pdf_wind_var_intper[0,iok], linestyle=2, thick=1, color=50
;  oplot, bin_mid[iok],pdf_wind_var_intper[1,iok], linestyle=2, thick=1, color=50
;  oplot, bin_mid[iok],pdf_wind_var_decper[0,iok], linestyle=2, thick=1, color=150
;  oplot, bin_mid[iok],pdf_wind_var_decper[1,iok], linestyle=2, thick=1, color=150
  xyouts, 0.15, 0.90, var_nam+'_ICR - INT', color=50, /normal, charsize=1.5
  xyouts, 0.15, 0.85, var_nam+'_ICR - DEC', color=200, /normal, charsize=1.5
;  xyouts, bin_mid[iok],0.4,strtrim(nb_binint[iok],2)
;  xyouts, bin_mid[iok],0.3,strtrim(nb_bindec[iok],2)
;  xyouts, bin_mid[iok],-1.9,strtrim(ts_cpl[iok,1],2)
;  saveimage, 'FIGS_PDF/pdf_wind_bin'+strtrim(bin_size,2)+'_COOLING_200km_int+dec_'+exp+'.gif'
tmpint = pdf_wind_var_int
ENDIF

IF param EQ 'FORCED' THEN BEGIN
  wset,0
  iok = where(tmpint NE 0. )
  plot, bin_mid[iok],pdf_wind_var_int[iok], xstyle=1, ystyle=1, /noerase, $
  xrange=[bin[min(iok)],bin[max(iok)+1]],yrange=[ymin,ymax],linestyle=2
  oplot, bin_mid[iok],pdf_wind_var_int[iok],linestyle=2, thick=2, color=50
  oplot, bin_mid[iok],pdf_wind_var_dec[iok],linestyle=2, thick=2, color=200
;  oplot, bin_mid[iok],pdf_wind_var_intper[0,iok], linestyle=2, thick=1, color=50
;  oplot, bin_mid[iok],pdf_wind_var_intper[1,iok], linestyle=2, thick=1, color=50
;  oplot, bin_mid[iok],pdf_wind_var_decper[0,iok], linestyle=2, thick=1, color=150
;  oplot, bin_mid[iok],pdf_wind_var_decper[1,iok], linestyle=2, thick=1, color=150
;  xyouts,  bin_mid[0], -1.2, var_nam+'_ICR - INT', color=50
;  xyouts,  bin_mid[0], -1.4, var_nam+'_ICR - DEC', color=200
;  xyouts, bin_mid[iok],0.4,strtrim(nb_binint[iok],2)
;  xyouts, bin_mid[iok],0.3,strtrim(nb_bindec[iok],2)
;  xyouts, bin_mid[iok],-1.9,strtrim(ts_cpl[iok,1],2)
;  saveimage, 'FIGS_PDF/pdf_wind_bin'+strtrim(bin_size,2)+'_COOLING_200km_int+dec_'+exp+'.gif'
;  saveimage, 'FIGS_PDF/pdf_wind_bin'+strtrim(bin_size,2)+'_SST-ICR_200km_int+dec_'+exp+'.gif'
ENDIF


IF param EQ 'COUPLED' THEN BEGIN
  window,1
  ymin = 26 & ymax = 31
  iok = where(pdf_wind_var_int NE 0.)
  plot, bin_mid[iok],pdf_wind_befor_int[iok], xstyle=1, ystyle=1, $
  xrange=[bin[min(iok)],bin[max(iok)+1]],xtitle=bin_name+' '+bin_unit,ytitle=var_nam+'-BEF '+var_uni,yrange=[ymin,ymax],linestyle=0, thick=2, color=0
  oplot, bin_mid[iok],pdf_wind_befor_int[iok],linestyle=0,thick=2, color=50
  oplot, bin_mid[iok],pdf_wind_befor_dec[iok],linestyle=0, thick=2, color=250
;  oplot, bin_mid[iok],pdf_wind_var_intper[0,iok], linestyle=2, thick=1, color=50
;  oplot, bin_mid[iok],pdf_wind_var_intper[1,iok], linestyle=2, thick=1, color=50
;  oplot, bin_mid[iok],pdf_wind_var_decper[0,iok], linestyle=2, thick=1, color=150
;  oplot, bin_mid[iok],pdf_wind_var_decper[1,iok], linestyle=2, thick=1, color=150
  xyouts, 0.15, 0.90, var_nam+'-BEF - INT', color=50, /normal, charsize=1.5
  xyouts, 0.15, 0.85, var_nam+'-BEF - DEC', color=250, /normal, charsize=1.5
;  xyouts, bin_mid[iok],0.4,strtrim(nb_binint[iok],2)
;  xyouts, bin_mid[iok],0.3,strtrim(nb_bindec[iok],2)
;  xyouts, bin_mid[iok],-1.9,strtrim(ts_cpl[iok,1],2)
;  saveimage, 'FIGS_PDF/pdf_wind_bin'+strtrim(bin_size,2)+'_COOLING_200km_int+dec_'+exp+'.gif'
tmpbef = pdf_wind_befor_int
ENDIF

IF param EQ 'FORCED' THEN BEGIN
  wset,1
  iok = where(tmpbef NE 0. )
  plot, bin_mid[iok],pdf_wind_befor_int[iok], xstyle=1, ystyle=1, /noerase, $
  xrange=[bin[min(iok)],bin[max(iok)+1]],yrange=[ymin,ymax],linestyle=2
  oplot, bin_mid[iok],pdf_wind_befor_int[iok],linestyle=2, thick=2, color=50
  oplot, bin_mid[iok],pdf_wind_befor_dec[iok],linestyle=2, thick=2, color=250
;  oplot, bin_mid[iok],pdf_wind_var_intper[0,iok], linestyle=2, thick=1, color=50
;  oplot, bin_mid[iok],pdf_wind_var_intper[1,iok], linestyle=2, thick=1, color=50
;  oplot, bin_mid[iok],pdf_wind_var_decper[0,iok], linestyle=2, thick=1, color=150
;  oplot, bin_mid[iok],pdf_wind_var_decper[1,iok], linestyle=2, thick=1, color=150
;  xyouts,  bin_mid[0], -1.2, var_nam+'_ICR - INT', color=50
;  xyouts,  bin_mid[0], -1.4, var_nam+'_ICR - DEC', color=200
;  xyouts, bin_mid[iok],0.4,strtrim(nb_binint[iok],2)
;  xyouts, bin_mid[iok],0.3,strtrim(nb_bindec[iok],2)
;  xyouts, bin_mid[iok],-1.9,strtrim(ts_cpl[iok,1],2)
;  saveimage, 'FIGS_PDF/pdf_wind_bin'+strtrim(bin_size,2)+'_COOLING_200km_int+dec_'+exp+'.gif'
  saveimage, 'FIGS_PDF/pdf_wind_bin'+strtrim(bin_size,2)+'_SST-BEF_200km_int+dec_'+exp+'.gif'
ENDIF

ENDFOR
ENDFOR
stop

END
