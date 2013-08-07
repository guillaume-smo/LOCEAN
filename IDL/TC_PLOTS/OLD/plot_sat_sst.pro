; PLOT 1D SST TC+DOMAIN
lct,60
color_factor=30
key_portrait=1
write_ps=0
thc=2  ; 5 en ps


; SST "REAL TRACKS" SAT ONLY
IF write_ps THEN openps, filename=plt_path+'COMPARE_SST_SAT_1DREALTC'
maxplot = max([ssto_1Dtc_0-273.15,sstg_1Dtc_0-273.15,sstp_1Dtc_0-273.15,ssta_1Dtc_0-273.15,sstm_1Dtc_0-273.15,ssti_1Dtc_0-273.15], /nan)
minplot = min([ssto_1Dtc_0-273.15,sstg_1Dtc_0-273.15,sstp_1Dtc_0-273.15,ssta_1Dtc_0-273.15,sstm_1Dtc_0-273.15,ssti_1Dtc_0-273.15], /nan)
maxplot = maxplot + 0.05*(maxplot-minplot)
minplot = minplot - 0.05*(maxplot-minplot)

splot, date_0, ssti_1Dtc_0-273.15, XTICKFORMAT='(I8)', XTICKINTERVAL=1, xminor=4, yrange=[minplot,maxplot], title='TC "REAL" TRACK SST', $
xtitle='DATE', thick=thc, win=0, xgridstyle=2, xticklen=1.0, ytitle='SST (degC)', yminor=5, charsize=1.5, charthick=2
oplot, date_0, sstm_1Dtc_0-273.15, color=color_factor*1, thick=thc
oplot, date_0, ssto_1Dtc_0-273.15, color=color_factor*2, thick=thc
oplot, date_0, ssts_1Dtc_0-273.15, color=color_factor*3, thick=thc
oplot, date_0, ssta_1Dtc_0-273.15, color=color_factor*5, thick=thc
xyouts, 0.125, 0.200-0.020*1, 'REMSS MW+IR', /normal, charsize=1.5, charthick=2, color=color_factor*0
xyouts, 0.125, 0.200-0.020*2, 'REMSS MW'   , /normal, charsize=1.5, charthick=2, color=color_factor*1
xyouts, 0.125, 0.200-0.020*3, 'ODYSSEA'    , /normal, charsize=1.5, charthick=2, color=color_factor*2
xyouts, 0.125, 0.200-0.020*4, 'OSTIA'      , /normal, charsize=1.5, charthick=2, color=color_factor*3
xyouts, 0.125, 0.200-0.020*5, 'ALADIN'     , /normal, charsize=1.5, charthick=2, color=color_factor*5
IF write_ps THEN closeps ELSE saveimage, plt_path+'COMPARE_SST_SAT_1DREALTC.gif', quality=100



; SST DOMAIN SAT ONLY
IF write_ps THEN openps, filename=plt_path+'COMPARE_SST_SAT_1D'
maxplot = max([ssto_1D_0-273.15,ssts_1D_0-273.15,sstp_1D_0-273.15,ssta_1D_0-273.15,sstm_1D_0-273.15,ssti_1D_0-273.15], /nan)
minplot = min([ssto_1D_0-273.15,ssts_1D_0-273.15,sstp_1D_0-273.15,ssta_1D_0-273.15,sstm_1D_0-273.15,ssti_1D_0-273.15], /nan)
maxplot = maxplot + 0.05*(maxplot-minplot)
minplot = minplot - 0.05*(maxplot-minplot)

splot, date_0, ssti_1D_0-273.15, XTICKFORMAT='(I8)', XTICKINTERVAL=1, xminor=4, yrange=[minplot,maxplot], title='OCEANIC DOMAIN SST', $
xtitle='DATE', thick=thc, win=0, xgridstyle=2, xticklen=1.0, ytitle='SST (degC)', yminor=5, charsize=1.5, charthick=2
oplot, date_0, sstm_1D_0-273.15, color=color_factor*1, thick=thc
oplot, date_0, ssto_1D_0-273.15, color=color_factor*2, thick=thc
oplot, date_0, ssts_1D_0-273.15, color=color_factor*3, thick=thc
oplot, date_0, ssta_1D_0-273.15, color=color_factor*5, thick=thc
xyouts, 0.125, 0.200-0.020*1, 'REMSS MW+IR', /normal, charsize=1.5, charthick=2, color=color_factor*0
xyouts, 0.125, 0.200-0.020*2, 'REMSS MW'   , /normal, charsize=1.5, charthick=2, color=color_factor*1
xyouts, 0.125, 0.200-0.020*3, 'ODYSSEA'    , /normal, charsize=1.5, charthick=2, color=color_factor*2
xyouts, 0.125, 0.200-0.020*4, 'OSTIA'      , /normal, charsize=1.5, charthick=2, color=color_factor*3
xyouts, 0.125, 0.200-0.020*5, 'ALADIN'     , /normal, charsize=1.5, charthick=2, color=color_factor*5
IF write_ps THEN closeps ELSE saveimage, plt_path+'COMPARE_SST_SAT_1D.gif', quality=100
STOP
