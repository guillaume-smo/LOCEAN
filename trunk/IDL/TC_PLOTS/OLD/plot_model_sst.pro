; PLOT 1D SST TC+DOMAIN
lct,60
color_factor=30
key_portrait=1
write_ps=0
thc=2  ; 5 en ps



; SST "MODELS" TRACKS
IF write_ps THEN openps, filename=plt_path+'COMPARE_SST_1DMODELTC'
maxplot = max([ssto_1Dtc_0-273.15,sstg_1Dtc_0-273.15,sstp_1Dtc_0-273.15,ssta_1Dtc_0-273.15,sstm_1Dtc_0-273.15,ssti_1Dtc_0-273.15], /nan)
minplot = min([ssto_1Dtc_0-273.15,sstg_1Dtc_0-273.15,sstp_1Dtc_0-273.15,ssta_1Dtc_0-273.15,sstm_1Dtc_0-273.15,ssti_1Dtc_0-273.15], /nan)
maxplot = maxplot + 0.05*(maxplot-minplot)
minplot = minplot - 0.05*(maxplot-minplot)

splot, date_0, ssti_1Dtc_0-273.15, XTICKFORMAT='(I8)', XTICKINTERVAL=1, xminor=4, yrange=[minplot,maxplot], title='TC "REAL" TRACK SST', $
xtitle='DATE', thick=1, win=0, xgridstyle=2, xticklen=1.0, ytitle='SST (degC)', yminor=5, charsize=1.5, charthick=2
oplot, date_0, ssti_1Dtc_0-273.15, color=color_factor*0, thick=thc
;oplot, date_1, sst_1Dtc_1-273.15, color=color_factor*1, thick=thc

;oplot, [date_sstr1,date_sstr1], [sstr1_1Dtc_0-273.15,sstr1_1Dtc_0-273.15], color=color_factor*2, thick=thc, psym=1, symsize=4
;oplot, date_nemo12, sstn12_1Dtc_0-273.15, color=color_factor*2, thick=thc
;oplot, date_1, sstn12_1Dtc_1-273.15, color=color_factor*2, thick=thc, linestyle=2

;oplot, [date_sstr2,date_sstr2], [sstr2_1Dtc_0-273.15,sstr2_1Dtc_0-273.15], color=color_factor*3, thick=thc, psym=1, symsize=4
;oplot, date_nemo13, sstn13_1Dtc_0-273.15, color=color_factor*3, thick=thc
;oplot, date_1, sstn13_1Dtc_1-273.15, color=color_factor*3, thick=thc, linestyle=2

;oplot, [date_sstr3,date_sstr3], [sstr3_1Dtc_0-273.15,sstr3_1Dtc_0-273.15], color=color_factor*4, thick=thc, psym=1, symsize=4
;oplot, date_nemo14, sstn14_1Dtc_0-273.15, color=color_factor*4, thick=thc
;oplot, date_1, sstn14_1Dtc_1-273.15, color=color_factor*3, thick=thc, linestyle=2

;oplot, date_14, sst_1Dtc_14-273.15, color=color_factor*5, thick=thc
;oplot, date_15, sst_1Dtc_15-273.15, color=color_factor*6, thick=thc
;oplot, date_16, sst_1Dtc_16-273.15, color=color_factor*7, thick=thc
STOP

xyouts, 0.125, 0.200-0.020*1, 'REMSS MW+IR', /normal, charsize=1.5, charthick=2, color=color_factor*0
xyouts, 0.125, 0.200-0.020*2, 'REMSS MW', /normal, charsize=1.5, charthick=2, color=color_factor*1
xyouts, 0.125, 0.200-0.020*3, 'ODYSSEA'  , /normal, charsize=1.5, charthick=2, color=color_factor*2
xyouts, 0.125, 0.200-0.020*4, 'GLORYS2V1', /normal, charsize=1.5, charthick=2, color=color_factor*3
xyouts, 0.125, 0.200-0.020*5, 'PSY3V3R1', /normal, charsize=1.5, charthick=2, color=color_factor*4
xyouts, 0.125, 0.200-0.020*6, 'ALADIN', /normal, charsize=1.5, charthick=2, color=color_factor*5
xyouts, 0.125, 0.200-0.020*7, 'RESTART-20080212', /normal, charsize=1.5, charthick=2, color=color_factor*6
xyouts, 0.125, 0.200-0.020*8, 'RESTART-20080213', /normal, charsize=1.5, charthick=2, color=color_factor*7
xyouts, 0.125, 0.200-0.020*9, 'RESTART-20080214', /normal, charsize=1.5, charthick=2, color=color_factor*8
xyouts, 0.425, 0.200-0.020*2, '(+'+string(mean(sstm_1Dtc_0-ssti_1Dtc_0), format='(F4.2)')+' C)', /normal, charsize=1.5, charthick=2, color=color_factor*0
xyouts, 0.425, 0.200-0.020*3, '(+'+string(mean(ssto_1Dtc_0-ssti_1Dtc_0), format='(F4.2)')+' C)', /normal, charsize=1.5, charthick=2, color=color_factor*0
xyouts, 0.425, 0.200-0.020*4, '(+'+string(mean(sstg_1Dtc_0-ssti_1Dtc_0), format='(F4.2)')+' C)', /normal, charsize=1.5, charthick=2, color=color_factor*0
xyouts, 0.425, 0.200-0.020*5, '(+'+string(mean(sstp_1Dtc_0-ssti_1Dtc_0), format='(F4.2)')+' C)', /normal, charsize=1.5, charthick=2, color=color_factor*0
xyouts, 0.425, 0.200-0.020*6, '(+'+string(mean(ssta_1Dtc_0-ssti_1Dtc_0), format='(F4.2)')+' C)', /normal, charsize=1.5, charthick=2, color=color_factor*0
xyouts, 0.425, 0.200-0.020*7, '(+'+string(sstr1_1Dtc_0-ssti_1Dtc_0[where(date_0 EQ 20080212.00d)], format='(F4.2)')+' C)', /normal, charsize=1.5, charthick=2, color=color_factor*0
xyouts, 0.425, 0.200-0.020*8, '(+'+string(sstr2_1Dtc_0-ssti_1Dtc_0[where(date_0 EQ 20080213.00d)], format='(F4.2)')+' C)', /normal, charsize=1.5, charthick=2, color=color_factor*0
xyouts, 0.425, 0.200-0.020*9, '(+'+string(sstr3_1Dtc_0-ssti_1Dtc_0[where(date_0 EQ 20080214.00d)], format='(F4.2)')+' C)', /normal, charsize=1.5, charthick=2, color=color_factor*0
IF write_ps THEN closeps ELSE saveimage, plt_path+'COMPARE_SST_1DMODELTC.gif', quality=100



; SST DOMAIN
maxplot = max([ssto_1D_0,sstg_1D_0,sstp_1D_0,ssta_1D_0,sstm_1D_0,ssti_1D_0], /nan)
minplot = min([ssto_1D_0,sstg_1D_0,sstp_1D_0,ssta_1D_0,sstm_1D_0,ssti_1D_0], /nan)
maxplot = maxplot + 0.05*(maxplot-minplot)
minplot = minplot - 0.05*(maxplot-minplot)

IF write_ps THEN openps, filename=plt_path+'COMPARE_SST_1D'
splot, date_0, ssti_1D_0, XTICKFORMAT='(I8)', XTICKINTERVAL=1, xminor=4, yrange=[minplot,maxplot], title='DOMAIN SST', $
xtitle='DATE', thick=thc, win=0, xgridstyle=2, xticklen=1.0, ytitle='SST (K)', yminor=5, charsize=1.5, charthick=2 
oplot, date_0, sstm_1D_0, color=color_factor*1, thick=thc
oplot, date_0, ssto_1D_0, color=color_factor*2, thick=thc
oplot, date_0, sstg_1D_0, color=color_factor*3, thick=thc
oplot, date_0, sstp_1D_0, color=color_factor*4, thick=thc
oplot, date_0, ssta_1D_0, color=color_factor*5, thick=thc
oplot, [date_sstr1,date_sstr1], [sstr1_1D_0,sstr1_1D_0], color=color_factor*6, thick=thc, psym=1, symsize=4
oplot, date_nemo12, sstn12_1D_0, color=color_factor*6, thick=thc
oplot, [date_sstr2,date_sstr2], [sstr2_1D_0,sstr2_1D_0], color=color_factor*7, thick=thc, psym=1, symsize=4
oplot, date_nemo13, sstn13_1D_0, color=color_factor*7, thick=thc
oplot, [date_sstr3,date_sstr3], [sstr3_1D_0,sstr3_1D_0], color=color_factor*8, thick=thc, psym=1, symsize=4
oplot, date_nemo14, sstn14_1D_0, color=color_factor*8, thick=thc
xyouts, 0.125, 0.200-0.020*1, 'REMSS MW+IR', /normal, charsize=1.5, charthick=2, color=color_factor*0
xyouts, 0.125, 0.200-0.020*2, 'REMSS MW', /normal, charsize=1.5, charthick=2, color=color_factor*1
xyouts, 0.125, 0.200-0.020*3, 'ODYSSEA'  , /normal, charsize=1.5, charthick=2, color=color_factor*2
xyouts, 0.125, 0.200-0.020*4, 'GLORYS2V1', /normal, charsize=1.5, charthick=2, color=color_factor*3
xyouts, 0.125, 0.200-0.020*5, 'PSY3V3R1', /normal, charsize=1.5, charthick=2, color=color_factor*4
xyouts, 0.125, 0.200-0.020*6, 'ALADIN', /normal, charsize=1.5, charthick=2, color=color_factor*5
xyouts, 0.125, 0.200-0.020*7, 'RESTART-20080212', /normal, charsize=1.5, charthick=2, color=color_factor*6
xyouts, 0.125, 0.200-0.020*8, 'RESTART-20080213', /normal, charsize=1.5, charthick=2, color=color_factor*7
xyouts, 0.125, 0.200-0.020*9, 'RESTART-20080214', /normal, charsize=1.5, charthick=2, color=color_factor*8
xyouts, 0.425, 0.200-0.020*2, '(+'+string(mean(sstm_1D_0-ssti_1D_0), format='(F4.2)')+' K)', /normal, charsize=1.5, charthick=2, color=color_factor*0
xyouts, 0.425, 0.200-0.020*3, '(+'+string(mean(ssto_1D_0-ssti_1D_0), format='(F4.2)')+' K)', /normal, charsize=1.5, charthick=2, color=color_factor*0
xyouts, 0.425, 0.200-0.020*4, '(+'+string(mean(sstg_1D_0-ssti_1D_0), format='(F4.2)')+' K)', /normal, charsize=1.5, charthick=2, color=color_factor*0
xyouts, 0.425, 0.200-0.020*5, '(+'+string(mean(sstp_1D_0-ssti_1D_0), format='(F4.2)')+' K)', /normal, charsize=1.5, charthick=2, color=color_factor*0
xyouts, 0.425, 0.200-0.020*6, '(+'+string(mean(ssta_1D_0-ssti_1D_0), format='(F4.2)')+' K)', /normal, charsize=1.5, charthick=2, color=color_factor*0
xyouts, 0.425, 0.200-0.020*7, '('+string(sstr1_1D_0-ssti_1D_0[where(date_0 EQ 20080212.00d)], format='(F5.2)')+' K)', /normal, charsize=1.5, charthick=2, color=color_factor*0
xyouts, 0.425, 0.200-0.020*8, '('+string(sstr2_1D_0-ssti_1D_0[where(date_0 EQ 20080213.00d)], format='(F5.2)')+' K)', /normal, charsize=1.5, charthick=2, color=color_factor*0
xyouts, 0.425, 0.200-0.020*9, '(+'+string(sstr3_1D_0-ssti_1D_0[where(date_0 EQ 20080214.00d)], format='(F4.2)')+' K)', /normal, charsize=1.5, charthick=2, color=color_factor*0
IF write_ps THEN closeps ELSE saveimage, plt_path+'COMPARE_SST_1DDOMAIN.gif', quality=100
write_ps=0
STOP
