PRO plot_pdf_didt


; parametres
explist = ['BMJ','KF']
parlist = ['COUPLED','FORCED']
basin   = 'SIO'
freq    = '6H'
datebeg = '19900101'
dt = '12h'

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
IF dt EQ '24h' THEN calcul_index_didt24h,d1_pres,d1_max_wnd,ind_int,ind_dec,d1_wind_smooth_24h,dwinddt,dpresdt,i_int,i_dec
IF dt EQ '12h' THEN calcul_index_didt12h,d1_pres,d1_max_wnd,ind_int,ind_dec,d1_wind_smooth_24h,dwinddt,dpresdt,i_int,i_dec


; definition bins
binp_min = -20
binp_max = 20
binp_del = 1
binw_min = -10
binw_max = 10
binw_del = 0.5


; calcul pdf
pdf_dpresdtall = histogram(dpresdt, binsize=binp_del, min=binp_min, max=binp_max, locations=bins_pres)
pdf_dwinddtall = histogram(dwinddt, binsize=binw_del, min=binw_min, max=binw_max, locations=bins_wind)
pdf_dpresdt = histogram([dpresdt[ind_int],dpresdt[ind_dec]], binsize=binp_del, min=binp_min, max=binp_max)
pdf_dwinddt = histogram([dwinddt[ind_int],dwinddt[ind_dec]],binsize=binw_del, min=binw_min, max=binw_max)
pdf_norm_dpresdtall = float(pdf_dpresdtall) / max(pdf_dpresdtall)
pdf_norm_dwinddtall = float(pdf_dwinddtall) / max(pdf_dwinddtall)
pdf_norm_dpresdt = float(pdf_dpresdt) / max(pdf_dpresdt)
pdf_norm_dwinddt = float(pdf_dwinddt) / max(pdf_dwinddt)


; bar plot
window, 0
bar_plot, pdf_dpresdtall, back=255, /outline, barname=long(bins_pres)
xyouts,0.1,0.9,strtrim(n_elements(where(finite(dpresdt) EQ 1)),2), /normal
saveimage, 'FIGS_PDF/pdf_dpresdt'+dt+'_'+expname+'.gif'

bar_plot, pdf_dwinddtall, back=255, /outline, barname=long(bins_wind)
xyouts,0.1,0.9,strtrim(n_elements(where(finite(dwinddt) EQ 1)),2), /normal
saveimage, 'FIGS_PDF/pdf_dwinddt'+dt+'_'+expname+'.gif'

bar_plot, pdf_dpresdt, back=255, /outline, barname=long(bins_pres)
xyouts,0.1,0.9,strtrim(n_elements(where(finite([dpresdt[ind_int],dpresdt[ind_dec]]) EQ 1)),2), /normal
saveimage, 'FIGS_PDF/pdf_dpresdt'+dt+'_'+expname+'.gif'

bar_plot, pdf_dwinddt, back=255, /outline, barname=long(bins_wind)
xyouts,0.1,0.9,strtrim(n_elements(where(finite([dwinddt[ind_int],dwinddt[ind_dec]]) EQ 1)),2), /normal
saveimage, 'FIGS_PDF/pdf_dwinddt'+dt+'_'+expname+'.gif'


; line plot
xw = indgen((binw_max-binw_min)/binw_del+1)*binw_del-(binw_max-binw_min)/2
IF param EQ 'COUPLED' THEN window,1
IF param EQ 'FORCED' THEN wset,1
IF param EQ 'COUPLED' THEN plot, xw, pdf_norm_dwinddtall, $
xrange=[-7,7],xtitle='(m.s-1/6h)',ytitle='normalized number of occurence',yrange=[0,1.]
IF param EQ 'COUPLED' THEN xyouts,0.1,0.9,strtrim(n_elements(where(finite(dwinddt) EQ 1)),2), /normal
IF param EQ 'FORCED' THEN plot, xw, pdf_norm_dwinddtall,xrange=[-7,7],/noerase,linestyle=2,yrange=[0,1.]
IF param EQ 'FORCED' THEN xyouts,0.1,0.85,strtrim(n_elements(where(finite(dwinddt) EQ 1)),2), /normal
IF param EQ 'FORCED' THEN saveimage, 'FIGS_PDF/pdf_norm_dwinddtall_'+exp+'.gif'

xp = indgen((binp_max-binp_min)/binp_del+1)*binp_del-(binp_max-binp_min)/2
IF param EQ 'COUPLED' THEN window,2
IF param EQ 'FORCED' THEN wset,2
IF param EQ 'COUPLED' THEN plot, xp, pdf_norm_dpresdtall, $
xrange=[-15,15],xtitle='(hPa/6h)',ytitle='normalized number of occurence',yrange=[0,1.]
IF param EQ 'COUPLED' THEN xyouts,0.1,0.9,strtrim(n_elements(where(finite(dpresdt) EQ 1)),2), /normal
IF param EQ 'FORCED' THEN plot, xp, pdf_norm_dpresdtall,xrange=[-15,15],/noerase,linestyle=2,yrange=[0,1.]
IF param EQ 'FORCED' THEN xyouts,0.1,0.85,strtrim(n_elements(where(finite(dpresdt) EQ 1)),2), /normal
IF param EQ 'FORCED' THEN saveimage, 'FIGS_PDF/pdf_norm_dpresdtall_'+exp+'.gif'

xw = indgen((binw_max-binw_min)/binw_del+1)*binw_del-(binw_max-binw_min)/2
IF param EQ 'COUPLED' THEN window,3
IF param EQ 'FORCED' THEN wset,3
IF param EQ 'COUPLED' THEN plot, xw, pdf_norm_dwinddt, $
xrange=[-7,7],xtitle='(m.s-1/6h)',ytitle='normalized number of occurence',yrange=[0,1.]
IF param EQ 'COUPLED' THEN xyouts,0.1,0.9,strtrim(n_elements(where(finite([dwinddt[ind_int],dwinddt[ind_dec]]) EQ 1)),2), /normal
IF param EQ 'FORCED' THEN plot, xw, pdf_norm_dwinddt,xrange=[-7,7],/noerase,linestyle=2,yrange=[0,1.]
IF param EQ 'FORCED' THEN xyouts,0.1,0.85,strtrim(n_elements(where(finite([dwinddt[ind_int],dwinddt[ind_dec]]) EQ 1)),2), /normal
IF param EQ 'FORCED' THEN saveimage, 'FIGS_PDF/pdf_norm_dwinddt'+dt+'_'+exp+'.gif'

xp = indgen((binp_max-binp_min)/binp_del+1)*binp_del-(binp_max-binp_min)/2
IF param EQ 'COUPLED' THEN window,4
IF param EQ 'FORCED' THEN wset,4
IF param EQ 'COUPLED' THEN plot, xp, pdf_norm_dpresdt, $
xrange=[-15,15],xtitle='(hPa/6h)',ytitle='normalized number of occurence',yrange=[0,1.]
IF param EQ 'COUPLED' THEN xyouts,0.1,0.9,strtrim(n_elements(where(finite([dpresdt[ind_int],dpresdt[ind_dec]]) EQ 1)),2), /normal
IF param EQ 'FORCED' THEN plot, xp, pdf_norm_dpresdt,xrange=[-15,15],/noerase,linestyle=2,yrange=[0,1.]
IF param EQ 'FORCED' THEN xyouts,0.1,0.85,strtrim(n_elements(where(finite([dpresdt[ind_int],dpresdt[ind_dec]]) EQ 1)),2), /normal
IF param EQ 'FORCED' THEN saveimage, 'FIGS_PDF/pdf_norm_dpresdt'+dt+'_'+exp+'.gif'


ENDFOR
ENDFOR

END
