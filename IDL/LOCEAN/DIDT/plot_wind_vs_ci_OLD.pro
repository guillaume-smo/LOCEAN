PRO plot_wind_vs_ci


; parametres
explist = ['BMJ','KF']
parlist = ['COUPLED']
basin   = 'SIO'
freq    = '1D'
datebeg = '19900101'
var_nam = 'ci'
var_typ = 'before'


; definition bin
bin_deb  = 0.
bin_fin  = 60.
bin_size = 10
bin_unit = '(m/s)'
bin_num  = (bin_fin - bin_deb) / bin_size & print, bin_num
bin      = bin_deb+findgen(bin_num)*bin_size
bin_mid  = bin_deb + findgen(bin_num)*bin_size+bin_size/2. & print, bin_mid


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


; lecture var
restore, pathin+var_nam+'_1D_'+expname+'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat', /VERBOSE
IF var_typ EQ 'min'  THEN d1_var = d1_var_ano_min_bef
IF var_typ EQ 'mean' THEN d1_var = d1_var_ano_mean_bef
IF var_typ EQ 'max'  THEN d1_var = d1_var_ano_max_bef
IF var_typ EQ 'before' THEN d1_var = d1_var_before
IF var_typ EQ 'during' THEN d1_var = d1_var_during
IF var_typ EQ 'after'  THEN d1_var = d1_var_after


; calcul index didt
calcul_index_didt24h,d1_pres,d1_max_wnd,ind_int24h,ind_dec24h,d1_wind_smooth_24h,dwinddt,dpresdt,i_int24h,i_dec24h
ind_int24h = ind_int24h[where(finite(d1_var[ind_int24h]) EQ 1)] & help, ind_int24h
ind_dec24h = ind_dec24h[where(finite(d1_var[ind_dec24h]) EQ 1)] & help, ind_dec24h


; declaration
pdf_wind_var_int = fltarr(bin_num)
pdf_wind_var_intper = fltarr(2,bin_num)
pdf_wind_var_dec = fltarr(bin_num)
pdf_wind_var_decper = fltarr(2,bin_num)
nb_binint = intarr(bin_num)
nb_bindec = intarr(bin_num)


; calcul pdf
FOR i = 0, bin_num-2 DO BEGIN
  ibin_int = where(d1_wind_smooth_24h[ind_int24h] GE bin[i] AND d1_wind_smooth_24h[ind_int24h] LT bin[i+1])
  ibin_dec = where(d1_wind_smooth_24h[ind_dec24h] GE bin[i] AND d1_wind_smooth_24h[ind_dec24h] LT bin[i+1])
  ; sauvegarde intermediaire
  IF ibin_int[0] NE -1 THEN BEGIN
    IF param EQ 'COUPLED' THEN varint_cpl[i,0:n_elements(ibin_int)-1] = d1_var[ind_int24h[ibin_int]] ELSE varint_frc[i,0:n_elements(ibin_int)-1] = d1_var[ind_int24h[ibin_int]]
  ENDIF
  IF ibin_dec[0] NE -1 THEN BEGIN
    IF param EQ 'COUPLED' THEN vardec_cpl[i,0:n_elements(ibin_dec)-1] = d1_var[ind_dec24h[ibin_dec]] ELSE vardec_frc[i,0:n_elements(ibin_dec)-1] = d1_var[ind_dec24h[ibin_dec]]
  ENDIF
  ; moyenne
  IF n_elements(ibin_int) GT 1 THEN pdf_wind_var_int[i] = mean(d1_var[ind_int24h[ibin_int]],/nan)
  IF n_elements(ibin_dec) GT 1 THEN pdf_wind_var_dec[i] = mean(d1_var[ind_dec24h[ibin_dec]],/nan)
  ; percentile
  IF n_elements(ibin_int) GT 1 THEN pdf_wind_var_intper[*,i] = percentile(d1_var[ind_int24h[ibin_int]],0.25)
  IF n_elements(ibin_dec) GT 1 THEN pdf_wind_var_decper[*,i] = percentile(d1_var[ind_dec24h[ibin_dec]],0.25)
  ; nombre de points par bin
  IF ibin_int[0] NE -1 THEN nb_binint[i] = n_elements(ibin_int) ELSE nb_binint[i] = 0
  IF ibin_dec[0] NE -1 THEN nb_bindec[i] = n_elements(ibin_dec) ELSE nb_bindec[i] = 0
ENDFOR


; test significativite
ts_cpl = fltarr(bin_num,2)
IF param EQ 'COUPLED' THEN BEGIN
  FOR i = 0, bin_num-2 DO BEGIN
    iok = where(varint_cpl[i,*] NE 0 AND vardec_cpl[i,*] NE 0, cntok) & help, cntok
    IF iok[0] NE -1 THEN ts_cpl[i,*] = RS_TEST(reform(varint_cpl[i,where(varint_cpl[i,*] NE 0)]),reform(vardec_cpl[i,where(vardec_cpl[i,*] NE 0)]))
  ENDFOR
print, ts_cpl[*,1]
ENDIF


; bar plot par bin
window, 0
bar_plot, pdf_wind_var_int, back=255, /outline, barname=bin_mid
saveimage, 'FIGS_PDF/pdf_wind_bin'+strtrim(bin_size,2)+'_'+var_nam+'_int'+'_'+expname+'.gif'
bar_plot, pdf_wind_var_dec, back=255, /outline, barname=bin_mid
saveimage, 'FIGS_PDF/pdf_wind_bin'+strtrim(bin_size,2)+'_'+var_nam+'_dec'+'_'+expname+'.gif'


; line plot par bin
ymin = 20 & ymax = 40
IF param EQ 'COUPLED' THEN BEGIN
  window,1
  iok = where(pdf_wind_var_int NE 0.)
  plot, bin_mid[iok],pdf_wind_var_int[iok], $
  xrange=[min(bin_mid),max(bin_mid)],xtitle=bin_unit,ytitle='(SI)',yrange=[ymin,ymax],linestyle=0
  plot, bin_mid[iok],pdf_wind_var_dec[iok],xrange=[min(bin_mid),max(bin_mid)],/noerase,linestyle=2,yrange=[ymin,ymax]
  oplot, bin_mid[iok],pdf_wind_var_intper[0,iok], linestyle=1
  oplot, bin_mid[iok],pdf_wind_var_intper[1,iok], linestyle=1
  oplot, bin_mid[iok],pdf_wind_var_decper[0,iok], linestyle=1
  oplot, bin_mid[iok],pdf_wind_var_decper[1,iok], linestyle=1
  xyouts, bin_mid[iok],39,strtrim(nb_binint[iok],2)
  xyouts, bin_mid[iok],38,strtrim(nb_bindec[iok],2)
  xyouts, bin_mid[iok],37,strtrim(ts_cpl[iok,1],2)
  saveimage, 'FIGS_PDF/pdf_wind_bin'+strtrim(bin_size,2)+'_'+var_nam+'_'+var_typ+'_int+dec_'+exp+'.gif'
ENDIF

ENDFOR
ENDFOR

END
