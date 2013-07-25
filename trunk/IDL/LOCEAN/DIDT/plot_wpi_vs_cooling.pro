PRO plot_wpi_vs_cooling


; parametres
explist = ['KF']
parlist = ['COUPLED','FORCED']
basin   = 'SIO'
freq    = '6H'
datebeg = '19900101'
period  = '1990-2009'
var_nam = 'SST'
rayon   = '200'

; definition bin
;bin_deb  = 15.
;bin_fin  = 60.
;bin_size = 5
;bin_name = '10M WIND'
;bin_unit = '(m/s)'
bin_deb  = 0.
bin_fin  = 8.
bin_size = 0.25
bin_name = 'WPI'
bin_unit = '(SI)'
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
;IF exp EQ 'KF' AND param EQ 'FORCED' THEN period  = '1990-1998' ELSE period  = '1990-2009' & help, period
;IF exp EQ 'KF' AND param EQ 'FORCED' THEN dateend = '19990101'  ELSE dateend = '20100101' & help, dateend
period  = '1990-2009' & help, period
dateend = '20100101'  & help, dateend
pathtc  = '/net/adonis/usr/adonis/varclim/gslod/IDL/TRACKER/EXP_'+expname+'/DATA/' & help, pathtc
restore, pathtc + 'd1_TRACKS_WPI_treal_new_cst_radius_'+ expname +'_IO_'+period+'.dat', /VERBOSE


; calcul wpi
wpi = (1./3.)*d1_nrj_v3_wrf^(1./3.)


; lecture var 1D+2D
pathin = '/net/adonis/usr/adonis/varclim/gslod/IDL/COLLOCALISATION/EXP_'+expname+'/DATA/' & help, pathin
restore, pathin+var_nam+'_1D_200km_'+expname+'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat', /VERBOSE
var_bef = d1_var_before & help, var_bef ; wsc
restore, pathin+var_nam+'_2D_'+rayon+'km_'+expname+'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat', /VERBOSE
var_xy = dxy_var_wsc_rot & help, var_xy
d1_var = m_mean(m_mean(temporary(dxy_var_wsc_rot),dim=4,/nan),dim=3,/nan) - d1_var_before
durin = d1_var_during - d1_var_before
after = d1_var_after  - d1_var_before


; calcul index didt
calcul_index_didt24h,d1_pres,d1_max_wnd,ind_int24h,ind_dec24h,d1_wind_smooth_24h,dwinddt,dpresdt,i_int24h,i_dec24h
iokint = ind_int24h[where(finite(d1_var[ind_int24h]) EQ 1)] & help, iokint
iokdec = ind_dec24h[where(finite(d1_var[ind_dec24h]) EQ 1)] & help, iokdec
iokall = [iokint,iokdec] & help, iokall & STOP


; declaration
pdf_wpi_var_int = fltarr(bin_num)
pdf_wpi_var_intper = fltarr(2,bin_num)
pdf_wpi_var_dec = fltarr(bin_num)
pdf_wpi_var_decper = fltarr(2,bin_num)
pdf_wpi_var_all = fltarr(bin_num)
pdf_wpi_var_allper = fltarr(2,bin_num)
pdf_wpi_durin_int = fltarr(bin_num)
pdf_wpi_durin_dec = fltarr(bin_num)
pdf_wpi_durin_all = fltarr(bin_num)
pdf_wpi_after_int = fltarr(bin_num)
pdf_wpi_after_dec = fltarr(bin_num)
pdf_wpi_after_all = fltarr(bin_num)
pdf_wpi_after_allper = fltarr(2,bin_num)

nb_binint = intarr(bin_num)
nb_bindec = intarr(bin_num)
nb_binall = intarr(bin_num)


; calcul pdf
FOR i = 0, bin_num-2 DO BEGIN
  ibin_int = where(wpi[iokint] GE bin[i] AND wpi[iokint] LT bin[i+1])
  ibin_dec = where(wpi[iokdec] GE bin[i] AND wpi[iokdec] LT bin[i+1])
  ibin_all = where(wpi[iokall] GE bin[i] AND wpi[iokall] LT bin[i+1])
  ; sauvegarde intermediaire
  IF ibin_int[0] NE -1 THEN BEGIN
    IF param EQ 'COUPLED' THEN varint_cpl[i,0:n_elements(ibin_int)-1] = d1_var[iokint[ibin_int]] ELSE varint_frc[i,0:n_elements(ibin_int)-1] = d1_var[iokint[ibin_int]]
  ENDIF
  IF ibin_dec[0] NE -1 THEN BEGIN
    IF param EQ 'COUPLED' THEN vardec_cpl[i,0:n_elements(ibin_dec)-1] = d1_var[iokdec[ibin_dec]] ELSE vardec_frc[i,0:n_elements(ibin_dec)-1] = d1_var[iokdec[ibin_dec]]
  ENDIF
  ; moyenne
  IF n_elements(ibin_int) GT 10 THEN pdf_wpi_var_int[i] = mean(d1_var[iokint[ibin_int]],/nan)
  IF n_elements(ibin_dec) GT 10 THEN pdf_wpi_var_dec[i] = mean(d1_var[iokdec[ibin_dec]],/nan)
  IF n_elements(ibin_all) GT 10 THEN pdf_wpi_var_all[i] = mean(d1_var[iokall[ibin_all]],/nan)
  IF n_elements(ibin_int) GT 10 THEN pdf_wpi_durin_int[i] = mean(durin[iokint[ibin_int]],/nan)
  IF n_elements(ibin_dec) GT 10 THEN pdf_wpi_durin_dec[i] = mean(durin[iokdec[ibin_dec]],/nan)
  IF n_elements(ibin_all) GT 10 THEN pdf_wpi_durin_all[i] = mean(durin[iokall[ibin_all]],/nan)
  IF n_elements(ibin_int) GT 10 THEN pdf_wpi_after_int[i] = mean(after[iokint[ibin_int]],/nan)
  IF n_elements(ibin_dec) GT 10 THEN pdf_wpi_after_dec[i] = mean(after[iokdec[ibin_dec]],/nan)
  IF n_elements(ibin_all) GT 10 THEN pdf_wpi_after_all[i] = mean(after[iokall[ibin_all]],/nan)
  ; percentile
  IF n_elements(ibin_int) GT 10 THEN pdf_wpi_var_intper[*,i] = percentile(d1_var[iokint[ibin_int]],0.25)
  IF n_elements(ibin_dec) GT 10 THEN pdf_wpi_var_decper[*,i] = percentile(d1_var[iokdec[ibin_dec]],0.25)
  IF n_elements(ibin_all) GT 10 THEN pdf_wpi_var_allper[*,i] = percentile(d1_var[iokall[ibin_all]],0.25)
  IF n_elements(ibin_all) GT 10 THEN pdf_wpi_after_allper[*,i] = percentile(after[iokall[ibin_all]],0.25)
  ; nombre de points par bin
  IF ibin_int[0] NE -1 THEN nb_binint[i] = n_elements(ibin_int) ELSE nb_binint[i] = 0
  IF ibin_dec[0] NE -1 THEN nb_bindec[i] = n_elements(ibin_dec) ELSE nb_bindec[i] = 0
  IF ibin_all[0] NE -1 THEN nb_binall[i] = n_elements(ibin_all) ELSE nb_binall[i] = 0
ENDFOR
;print, bin_mid
print, total(nb_binint),' / ', n_elements(iokint)
print, total(nb_bindec),' / ', n_elements(iokdec)
print, total(nb_binall),' / ', n_elements(iokall)
STOP


; test significativite
ts_cpl = fltarr(bin_num,2)*!Values.F_NAN
IF param EQ 'COUPLED' THEN BEGIN
  FOR i = 0, bin_num-1 DO BEGIN
    IF pdf_wpi_var_int[i] NE 0. AND pdf_wpi_var_dec[i] NE 0. THEN BEGIN
      iok = where(varint_cpl[i,*] NE 0 AND vardec_cpl[i,*] NE 0, cntok) & help, cntok
      IF iok[0] NE -1 THEN ts_cpl[i,*] = RS_TEST(reform(varint_cpl[i,where(varint_cpl[i,*] NE 0)]),reform(vardec_cpl[i,where(vardec_cpl[i,*] NE 0)]))
    ENDIF
  ENDFOR
print, long(bin_mid)
print, ts_cpl[*,1]
ENDIF


; bar plot par bin
window, 0
bar_plot, pdf_wpi_var_int, back=255, /outline, barname=bin_mid
;saveimage, 'FIGS_PDF/pdf_wpi_bin'+strtrim(bin_size,2)+'_'+var_nam+'_int'+'_'+expname+'.gif'
window,1
bar_plot, pdf_wpi_var_dec, back=255, /outline, barname=bin_mid
;saveimage, 'FIGS_PDF/pdf_wpi_bin'+strtrim(bin_size,2)+'_'+var_nam+'_dec'+'_'+expname+'.gif'
STOP


; line plot par bin
IF param EQ 'COUPLED' THEN BEGIN

  pdf_wpi_var_int_cpl = pdf_wpi_var_int
  pdf_wpi_var_intper_cpl = pdf_wpi_var_intper

  window,2;, xsize=525, ysize=525
  ymin = -2 & ymax = 0.5
  iok = where(pdf_wpi_var_int NE 0.)
  plot, bin_mid[iok],pdf_wpi_var_int[iok], xstyle=1, ystyle=1, $
  xrange=[bin[min(iok)],bin[max(iok)+1]],xtitle=bin_name+' '+bin_unit,ytitle='COOLING (degC)',yrange=[ymin,ymax],linestyle=0, thick=2, color=0
  oplot, bin_mid[iok],pdf_wpi_var_int[iok],linestyle=0,thick=2, color=50
  oplot, bin_mid[iok],pdf_wpi_var_dec[iok],linestyle=0, thick=2, color=250
  oplot, bin_mid[iok],pdf_wpi_var_intper[0,iok], linestyle=2, thick=1, color=50
  oplot, bin_mid[iok],pdf_wpi_var_intper[1,iok], linestyle=2, thick=1, color=50
  oplot, bin_mid[iok],pdf_wpi_var_decper[0,iok], linestyle=2, thick=1, color=250
  oplot, bin_mid[iok],pdf_wpi_var_decper[1,iok], linestyle=2, thick=1, color=250
  xyouts, 0.15, 0.20, 'INTEN', color=50, /normal, charsize=1.5
  xyouts, 0.15, 0.15, 'DECAY', color=250, /normal, charsize=1.5
;  xyouts, bin_mid[iok],0.4,strtrim(nb_binint[iok],2)
;  xyouts, bin_mid[iok],0.3,strtrim(nb_bindec[iok],2)
;  xyouts, bin_mid[iok],-1.9,strtrim(ts_cpl[iok,1],2)
  ;saveimage, 'FIGS_PDF/pdf_wpi_bin'+strtrim(bin_size,2)+'_COOLING_'+rayon+'km_int+dec_'+exp+'.gif'

  window,3;, xsize=640, ysize=525
  ymin = -3.5 & ymax = 0.5
  iok = where(pdf_wpi_var_all NE 0.)
  plot, bin_mid[iok],pdf_wpi_var_all[iok], xstyle=1, ystyle=1, $
  xrange=[bin[min(iok)],bin[max(iok)+1]],xtitle=bin_name+' '+bin_unit,ytitle='COOLING (degC)',yrange=[ymin,ymax],linestyle=0,thick=2, color=0
  oplot, bin_mid[iok],pdf_wpi_after_all[iok], linestyle=0, thick=2, color=250
  oplot, bin_mid[iok],pdf_wpi_var_allper[0,iok], linestyle=2, thick=1, color=0
  oplot, bin_mid[iok],pdf_wpi_var_allper[1,iok], linestyle=2, thick=1, color=0
  oplot, bin_mid[iok],pdf_wpi_after_allper[0,iok], linestyle=2, thick=1, color=250
  oplot, bin_mid[iok],pdf_wpi_after_allper[1,iok], linestyle=2, thick=1, color=250
  xyouts, 0.15,0.20, 'ALL-DURING', color=0, charsize=1.5, /normal
  xyouts, 0.15,0.15, 'ALL-AFTER', color=250, charsize=1.5, /normal
;  xyouts,  bin_mid[iok], -3.2, pdf_wpi_after_all[iok]/pdf_wpi_var_all[iok]
;  xyouts, bin_mid[iok],0.4,strtrim(nb_binall[iok],2)
;  saveimage, 'FIGS_PDF/pdf_wpi_bin'+strtrim(bin_size,2)+'_COOLING_200km_DURING+AFTER_all_'+exp+'.gif'

ENDIF

IF param EQ 'FORCED' THEN BEGIN

  window,1
  ymin = -1 & ymax = 0.7
  iok = where(pdf_wpi_var_int_cpl NE 0.)
  plot, bin_mid[iok],pdf_wpi_var_int_cpl[iok], xstyle=1, ystyle=1, $
  xrange=[bin[min(iok)],bin[max(iok)+1]],xtitle='10M WIND '+bin_unit,ytitle='COOLING (degC)',yrange=[ymin,ymax],linestyle=0, thick=2, color=0
  oplot, bin_mid[iok],pdf_wpi_var_int_cpl[iok],linestyle=0,thick=2, color=50
  oplot, bin_mid[iok],pdf_wpi_var_intper_cpl[0,iok], linestyle=2, thick=1, color=50
  oplot, bin_mid[iok],pdf_wpi_var_intper_cpl[1,iok], linestyle=2, thick=1, color=50
  xyouts, 0.15, 0.20, 'CPL', color=50, /normal, charsize=1.5
  oplot, bin_mid[iok],pdf_wpi_var_int[iok],linestyle=0,thick=2, color=250
  oplot, bin_mid[iok],pdf_wpi_var_intper[0,iok], linestyle=2, thick=1, color=250
  oplot, bin_mid[iok],pdf_wpi_var_intper[1,iok], linestyle=2, thick=1, color=250
  xyouts, 0.15, 0.15, 'FRC', color=250, /normal, charsize=1.5
  saveimage, 'FIGS_PDF/pdf_wpi_bin'+strtrim(bin_size,2)+'_COOLING_'+rayon+'km_int_CPL+FRC_'+exp+'.gif'

ENDIF

ENDFOR
ENDFOR
stop
END
