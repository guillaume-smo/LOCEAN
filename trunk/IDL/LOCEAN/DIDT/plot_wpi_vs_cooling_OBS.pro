PRO plot_wpi_vs_cooling_OBS


; parametres
basin   = 'SIO'
freq    = '1D'
datebeg = '19980101'
dateend = '20100101'
period  = '1998-2009'
var_nam = 'SST'
rayon   = '200'


; definition bin
;bin_deb  = 15.
;bin_fin  = 60.
;bin_size = 5
;bin_name = '10M WIND'
;bin_unit = '(m/s)'

bin_deb  = 0.
bin_fin  = 6.
bin_size = 0.2
bin_name = 'WPI'
bin_unit = '(SI)'

bin_num  = (bin_fin - bin_deb) / bin_size & print, bin_num
bin      = bin_deb + findgen(bin_num)*bin_size
bin_mid  = bin_deb + findgen(bin_num)*bin_size + bin_size/2. & print, bin_mid


; declarations
varint_obs = fltarr(bin_num, 10000)
vardec_obs = fltarr(bin_num, 10000)


; lecture best-track
pathtc  = '/net/adonis/usr/adonis/varclim/gslod/IDL/TRACKER/EXP_IBTRACS/DATA/' & help, pathin
restore, pathtc + 'd1_IBTRACSv03r03_WPI_IO_'+period+'.dat', /VERBOSE


; lecture var 1D+2D
pathin  = '/net/adonis/usr/adonis/varclim/gslod/IDL/COLLOCALISATION/EXP_TMI-AMSRE/DATA/' & help, pathin
restore, pathin+var_nam+'_1D_'+rayon+'km_TMI-AMSRE_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat', /VERBOSE
var_bef = d1_var_before & help, var_bef ; wsc
restore, pathin+var_nam+'_2D_'+rayon+'km_TMI-AMSRE_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat', /VERBOSE
var_xy = dxy_var_wsc_rot & help, var_xy ; wsc
d1_var = m_mean(m_mean(temporary(dxy_var_wsc_rot),dim=4,/nan),dim=3,/nan) - d1_var_before
durin = d1_var_during - d1_var_before
after = d1_var_after  - d1_var_before


; calcul index didt
;calcul_index_didt24h,d1_pres,d1_max_wnd,ind_int24h,ind_dec24h,d1_wind_smooth_24h,dwinddt,dpresdt,i_int24h,i_dec24h
calcul_index_didt24h,d1_pres,d1_wind,ind_int24h,ind_dec24h,d1_wind_smooth_24h,dwinddt,dpresdt,i_int24h,i_dec24h
iokint = ind_int24h[where(finite(d1_var[ind_int24h]) EQ 1)] & help, iokint
iokdec = ind_dec24h[where(finite(d1_var[ind_dec24h]) EQ 1)] & help, iokdec
iokall = [iokint,iokdec] & help, iokall


; calcul wpi
wpi_h = (1./3.)*d1_nrj_v3_h^(1./3.)
wpi_w = (1./3.)*d1_nrj_v3_w^(1./3.)
wpi   = wpi_w

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

nb_binint_obs = intarr(bin_num)
nb_bindec_obs = intarr(bin_num)
nb_binall_obs = intarr(bin_num)


; calcul pdf
FOR i = 0, bin_num-2 DO BEGIN
  ibin_int = where(wpi[iokint] GE bin[i] AND wpi[iokint] LT bin[i+1])
  ibin_dec = where(wpi[iokdec] GE bin[i] AND wpi[iokdec] LT bin[i+1])
  ibin_all = where(wpi[iokall] GE bin[i] AND wpi[iokall] LT bin[i+1])
  ; sauvegarde intermediaire
  IF ibin_int[0] NE -1 THEN BEGIN
    varint_obs[i,0:n_elements(ibin_int)-1] = d1_var[iokint[ibin_int]]
  ENDIF
  IF ibin_dec[0] NE -1 THEN BEGIN
    vardec_obs[i,0:n_elements(ibin_dec)-1] = d1_var[iokdec[ibin_dec]]
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
  IF ibin_int[0] NE -1 THEN nb_binint_obs[i] = n_elements(ibin_int) ELSE nb_binint_obs[i] = 0
  IF ibin_dec[0] NE -1 THEN nb_bindec_obs[i] = n_elements(ibin_dec) ELSE nb_bindec_obs[i] = 0
  IF ibin_all[0] NE -1 THEN nb_binall_obs[i] = n_elements(ibin_all) ELSE nb_binall_obs[i] = 0
ENDFOR
;print, bin_mid
print, total(nb_binint_obs),' / ', n_elements(iokint)
print, total(nb_bindec_obs),' / ', n_elements(iokdec)
print, total(nb_binall_obs),' / ', n_elements(iokall)
STOP


; test significativite
ts_obs = fltarr(bin_num,2)*!Values.F_NAN
FOR i = 0, bin_num-1 DO BEGIN
  IF pdf_wpi_var_int[i] NE 0. AND pdf_wpi_var_dec[i] NE 0. THEN BEGIN
    iok = where(varint_obs[i,*] NE 0 AND vardec_obs[i,*] NE 0, cntok) & help, cntok
    IF iok[0] NE -1 THEN ts_obs[i,*] = RS_TEST(reform(varint_obs[i,where(varint_obs[i,*] NE 0)]),reform(vardec_obs[i,where(vardec_obs[i,*] NE 0)]))
  ENDIF
ENDFOR
;print, bin_mid
;print, ts_obs[*,1]


; bar plot par bin
window, 0
bar_plot, pdf_wpi_var_int, back=255, /outline, barname=bin_mid
;saveimage, 'FIGS_PDF/pdf_wpi_bin'+strtrim(bin_size,2)+'_'+var_nam+'_int'+'_'+expname+'.gif'
window,1
bar_plot, pdf_wpi_var_dec, back=255, /outline, barname=bin_mid
;saveimage, 'FIGS_PDF/pdf_wpi_bin'+strtrim(bin_size,2)+'_'+var_nam+'_dec'+'_'+expname+'.gif'
STOP


; line plot par bin
; IF param EQ 'COUPLED' THEN BEGIN

  pdf_wpi_var_int_obs = pdf_wpi_var_int
  pdf_wpi_var_intper_obs = pdf_wpi_var_intper

  window,2;, xsize=525, ysize=525
  ymin = -2 & ymax = 0.5
  iok = where(pdf_wpi_var_int NE 0.)
  plot, bin_mid[iok],pdf_wpi_var_int[iok], xstyle=1, ystyle=1, $
  xrange=[bin[min(iok)],bin[max(iok)+1]],xtitle='10M WIND '+bin_unit,ytitle='COOLING (degC)',yrange=[ymin,ymax],linestyle=0, thick=2, color=0
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
;  xyouts, bin_mid[iok],-1.9,strtrim(ts_obs[iok,1],2)
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

;ENDIF




;  window,1
;  ymin = -1 & ymax = 0.7
;  iok = where(pdf_wpi_var_int_obs NE 0.)
;  plot, bin_mid[iok],pdf_wpi_var_int_obs[iok], xstyle=1, ystyle=1, $
;  xrange=[bin[min(iok)],bin[max(iok)+1]],xtitle='10M WIND '+bin_unit,ytitle='COOLING (degC)',yrange=[ymin,ymax],linestyle=0, thick=2, color=0
;  oplot, bin_mid[iok],pdf_wpi_var_int_obs[iok],linestyle=0,thick=2, color=50
;  oplot, bin_mid[iok],pdf_wpi_var_intper_obs[0,iok], linestyle=2, thick=1, color=50
;  oplot, bin_mid[iok],pdf_wpi_var_intper_obs[1,iok], linestyle=2, thick=1, color=50
;  xyouts, 0.15, 0.20, 'CPL', color=50, /normal, charsize=1.5
;  oplot, bin_mid[iok],pdf_wpi_var_int[iok],linestyle=0,thick=2, color=250
;  oplot, bin_mid[iok],pdf_wpi_var_intper[0,iok], linestyle=2, thick=1, color=250
;  oplot, bin_mid[iok],pdf_wpi_var_intper[1,iok], linestyle=2, thick=1, color=250
;  xyouts, 0.15, 0.15, 'FRC', color=250, /normal, charsize=1.5
;  saveimage, 'FIGS_PDF/pdf_wpi_bin'+strtrim(bin_size,2)+'_COOLING_'+rayon+'km_int_obs+FRC_'+exp+'.gif'


STOP
END
