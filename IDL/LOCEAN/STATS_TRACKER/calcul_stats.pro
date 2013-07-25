PRO calcul_stats

reinitplt, all=all
@all_cm
maskfile = 'mask_wrf.nc'
initncdf,  maskfile

; A FAIRE: VIRER LES POINTS SUR TERRE
; A FAIRE: VIRER LES POINTS EXTRA-TROPICAL (30S)


;-----------------------------------------------------------------------------------------
; PARAMETRES
;-----------------------------------------------------------------------------------------
expname = 'FORCED_SW2_BMJ'
basin   = 'SI'

firsty = 1990
lasty  = 2009
nbyear = float(lasty - firsty + 1)

ucrit    = 17.5 
vorcrit  = 30.e-5
vorelax  = 30.e-5
tempcrit = 1.
temptype = 'TREAL'

jvdep = 0
jwind = 0
jmois = 1
jyear = 0
jdays = 0
jdens = 1

;-----------------------------------------------------------------------------------------
; PATH + FILE
;-----------------------------------------------------------------------------------------
IF expname EQ 'IBTRACS' THEN BEGIN
  @/Users/gslod/WORK/IDL/TRACKER_IDL/read_ibtracs_wmo.pro
  path_fig = 'EXP_'+ expname +'/FIGS_IBTRACS_u'+ strtrim(long(ucrit),2) + '/STATS_u'+ strtrim(long(ucrit),2) +'/'
  spawn, 'echo "creation du repertoire '+ path_fig +'"' & spawn, 'mkdir -p '+ path_fig
ENDIF ELSE BEGIN
  path_data = 'EXP_'+ expname +'/DATA/'
  path_fig  = 'EXP_'+ expname +'/FIGS_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2) +'/STATS2_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2) +'/'
  spawn, 'echo "creation du repertoire '+ path_fig +'"' & spawn, 'mkdir -p '+ path_fig
  file_in = 'tracker_light_'+temptype+'_RVM_NICO_MASK_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2) +'_'+strtrim(firsty,2)+'-'+strtrim(lasty,2)+'.idl'
  restore, filename = path_data + file_in, /VERBOSE
  monthcycn = long(datecycn/100.) - long(datecycn/10000.)*100.
  yearcycn  = long(datecycn/10000.)
  help, loncycn, latcycn, mslpcycn, uv10cycn, datecycn, juldcycn
ENDELSE


IF basin EQ 'SI' THEN BEGIN

IF jvdep THEN BEGIN
indwind = where(uv10cycn GE 17.5, cntwind)
indsio  = where(latcycn[indwind] LE 0. AND latcycn[indwind] GE -30., cntsio)
indswio = where(latcycn[indwind] LE 0. AND latcycn[indwind] GE -30. AND loncycn[indwind] LE 80., cntswio)
indseio = where(latcycn[indwind] LE 0. AND latcycn[indwind] GE -30. AND loncycn[indwind] GE 80., cntseio)

hist_vdep_sio = histogram(vdepcycn[indwind[indsio]],  min=0,max=16,binsize=2, /NAN) / (nbyear*4.)
barname = strtrim(indgen(8)*2+1,2) & !y.range[0] = 0 & !y.range[1] = 50
bar_plot, hist_vdep_sio[0:7]/total(hist_vdep_sio,/nan)*100., barnames=barname, background=255, /outline
saveimage, path_fig + 'vdep_percent_sio_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2)+'.gif'

;hist_wpo_sio = histogram(uv10cycn[indwind[indsio]]/vdepcycn[indwind[indsio]],  min=0,max=50,binsize=5, /NAN) / (nbyear*4.)
;bar_plot, hist_wpo_sio/total(hist_wpo_sio)*100., background=255, /outline

ENDIF

IF jwind THEN BEGIN
;-----------------------------------------------------------------------------------------
; DISTRIBUTION INTENSITE EN JOURS CYCLONIQUES
;-----------------------------------------------------------------------------------------
indwind = where(uv10cycn GE 17.5, cntwind)
indsio  = where(latcycn[indwind] LE 0. AND latcycn[indwind] GE -30., cntsio)
indswio = where(latcycn[indwind] LE 0. AND latcycn[indwind] GE -30. AND loncycn[indwind] LE 80., cntswio)
indseio = where(latcycn[indwind] LE 0. AND latcycn[indwind] GE -30. AND loncycn[indwind] GE 80., cntseio)
hist_uv10sio  = histogram(uv10cycn[indwind[indsio]],  min=17.5,max=67.5,binsize=5, /NAN) 
hist_uv10swio = histogram(uv10cycn[indwind[indswio]], min=17.5,max=67.5,binsize=5, /NAN)
hist_uv10seio = histogram(uv10cycn[indwind[indseio]], min=17.5,max=67.5,binsize=5, /NAN)

; JOURS CYCLONIQUE
barname = strtrim(indgen(10)*5+20,2) & !y.range[0] = 0 & !y.range[1] = 100
bar_plot, hist_uv10sio[0:9]/(4.*nbyear), background=255, barnames=barname, title='SIO WIND DISTRIBUTION - '+expname, xtitle='wind (m/s)', ytitle='cyclonic days per year', /outline
xyouts, 6, 80, 'TOTAL:'+strtrim(total(hist_uv10sio, /NAN)/(4.*nbyear),2) , charsize = 1.5
saveimage, path_fig + 'uv10day_sio_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2)+'.gif'

barname = strtrim(indgen(10)*5+20,2) & !y.range[0] = 0 & !y.range[1] = 100
bar_plot, hist_uv10swio[0:9]/(4.*nbyear), background=255, barnames=barname, title='SEIO WIND DISTRIBUTION - '+expname, xtitle='wind (m/s)', ytitle='cyclonic days per year', /outline
xyouts, 6, 80, 'TOTAL:'+strtrim(total(hist_uv10swio[0:9], /NAN)/(4.*nbyear),2) , charsize = 1.5
saveimage, path_fig + 'uv10day_swio_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2)+'.gif'

barname = strtrim(indgen(10)*5+20,2) & !y.range[0] = 0 & !y.range[1] = 100
bar_plot, hist_uv10seio[0:9]/(4.*nbyear), background=255, barnames=barname, title='SWIO WIND DISTRIBUTION - '+expname, xtitle='wind (m/s)', ytitle='cyclonic days per year', /outline
xyouts, 6, 80, 'TOTAL:'+strtrim(total(hist_uv10seio[0:9], /NAN)/(4.*nbyear),2) , charsize = 1.5
saveimage, path_fig + 'uv10day_seio_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2)+'.gif'

; POURCENTAGE
barname = findgen(9)*5 + nbyear & !y.range[0] = 0 & !y.range[1] = 60
bar_plot, hist_uv10sio[0:8]/float(n_elements(uv10cycn[indwind[indsio]]))*100., background=255, barnames=barname, title='SIO WIND DISTRIBUTION - '+expname, xtitle='wind (m/s)', ytitle='percentage', /outline
xyouts, 6, 50, 'TOTAL:'+strtrim(total(hist_uv10sio, /NAN)/float(n_elements(uv10cycn[indwind[indsio]])),2) , charsize = 1.5
saveimage, path_fig + 'uv10day_percent_sio_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2)+'.gif'

barname = findgen(9)*5 + nbyear & !y.range[0] = 0 & !y.range[1] = 60
bar_plot, hist_uv10swio[0:8]/float(n_elements(uv10cycn[indwind[indswio]]))*100., background=255, barnames=barname, title='SWIO WIND DISTRIBUTION - '+expname, xtitle='wind (m/s)', ytitle='percentage', /outline
xyouts, 6, 50, 'TOTAL:'+strtrim(total(hist_uv10swio, /NAN)/float(n_elements(uv10cycn[indwind[indswio]])),2) , charsize = 1.5
saveimage, path_fig + 'uv10day_percent_swio_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2)+'.gif'

barname = findgen(9)*5 + nbyear & !y.range[0] = 0 & !y.range[1] = 60
bar_plot, hist_uv10seio[0:8]/float(n_elements(uv10cycn[indwind[indseio]]))*100., background=255, barnames=barname, title='SEIO WIND DISTRIBUTION - '+expname, xtitle='wind (m/s)', ytitle='percentage', /outline
xyouts, 6, 50, 'TOTAL:'+strtrim(total(hist_uv10seio, /NAN)/float(n_elements(uv10cycn[indwind[indseio]])),2) , charsize = 1.5
saveimage, path_fig + 'uv10day_percent_seio_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2)+'.gif'

; LOCALISATION DES MAX
indsio = where(latcycn[*,0] LE 0. AND latcycn[*,0] GE -30., cntsio)
lonmax = fltarr(cntsio)
latmax = fltarr(cntsio)
uvmax  = fltarr(cntsio)

FOR i = 0, cntsio-1 DO BEGIN
  uvmax[i]  = max(uv10cycn[indsio[i],*], indmax)
  lonmax[i] = loncycn[indsio[i],indmax]
  latmax[i] = latcycn[indsio[i],indmax]
ENDFOR

pdf_uvmax = hist_2D(lonmax, latmax, bin1=5, min1=30, max1=130, bin2=5, min2=-30, max2=0)

computegrid,30,-30,5,5,21,7
box=[30,130,-30,0]
plt, pdf_uvmax, min=0, max=20, lct=81, /nocont, /realcont, /landscape, box=box, title='WIND MAXIMA PDF ('+strtrim(cntsio,2)+' pts)', subtitle='number of maxima per 5 deg bin'
saveimage, path_fig + 'density_maxuv_sio_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2)+'.gif'

plt, pdf_uvmax/total(pdf_uvmax)*100., min=0, max=5, lct=81, /nocont, /realcont, /landscape, box=box, title='WIND MAXIMA PDF ('+strtrim(cntsio,2)+' pts)', subtitle='number of maxima per 5 deg bin'
saveimage, path_fig + 'density_percent_maxuv_sio_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2)+'.gif'

ENDIF


IF jmois THEN BEGIN
;-----------------------------------------------------------------------------------------
; CG CLIM MENSUEL
;-----------------------------------------------------------------------------------------
indcgsio  = where(latcycn[*,0] LE 0. AND latcycn[*,0] GE -30. AND loncycn[*,0] GE 30. AND loncycn[*,0] LE 140., cntcgsio)  & help, cntcgsio
indcgswio = where(latcycn[*,0] LE 0. AND latcycn[*,0] GE -30. AND loncycn[*,0] GE 30. AND loncycn[*,0] LE 80., cntcgswio)  & help, cntcgswio
indcgseio = where(latcycn[*,0] LE 0. AND latcycn[*,0] GE -30. AND loncycn[*,0] GE 80. AND loncycn[*,0] LE 140., cntcgseio) & help, cntcgseio
hist_cgsio_monthclim  = histogram(monthcycn[indcgsio,0],  min=1,max=12,nbins=12, /NAN) / nbyear
hist_cgswio_monthclim = histogram(monthcycn[indcgswio,0], min=1,max=12,nbins=12, /NAN) / nbyear
hist_cgseio_monthclim = histogram(monthcycn[indcgseio,0], min=1,max=12,nbins=12, /NAN) / nbyear
hist_cgsio_monthly  = hist_2D(yearcycn[indcgsio,0],monthcycn[indcgsio,0],  bin1=1,min1=firsty,max1=lasty,min2=1,max2=12)
hist_cgswio_monthly = hist_2D(yearcycn[indcgswio,0],monthcycn[indcgswio,0],bin1=1,min1=firsty,max1=lasty,min2=1,max2=12)
hist_cgseio_monthly = hist_2D(yearcycn[indcgseio,0],monthcycn[indcgseio,0],bin1=1,min1=firsty,max1=lasty,min2=1,max2=12)

; NB CYCLONES
;std_dev_month = fltarr(12)
;FOR i = 0,11 DO std_dev_month[i] = stddev(hist_cgsio_monthly[*,i], /NAN)
barname = ['JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC'] & !y.range[0] = 0 & !y.range[1] = 6
bar_plot, hist_cgsio_monthclim, background=255, barnames=barname, title='CLIMATOLOGICAL NUMBER OF CYCLOGENESIS (> 17.5 m/s) IN SIO - '+expname, xtitle='month', ytitle='number', /outline, tickv=tickv, colors=[indgen(n_elements(hist_cgsio_monthclim))*0]
;oploterr, tickv, hist_cgsio_monthclim, std_dev_month
;errplot, tickv, hist_cgsio_monthclim - std_dev_month, hist_cgsio_monthclim + std_dev_month
xyouts, 8, 4, 'TOTAL:'+strtrim(total(hist_cgsio_monthclim, /NAN),2) , charsize = 1.5
saveimage, path_fig + 'cg_sio_monthclim_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2)+'.gif'
print, total(hist_cgsio_monthclim[0:2], /NAN)+hist_cgsio_monthclim[11]

barname = ['JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC'] & !y.range[0] = 0 & !y.range[1] = 6
bar_plot, hist_cgswio_monthclim, background=255, barnames=barname, title='CLIMATOLOGICAL NUMBER OF CYCLOGENESIS (> 17.5 m/s) IN SWIO - '+expname, xtitle='month', ytitle='number', /outline, tickv=tickv, colors=[indgen(n_elements(hist_cgswio_monthclim))*0]
xyouts, 8, 4, 'TOTAL:'+strtrim(total(hist_cgswio_monthclim, /NAN),2) , charsize = 1.5
saveimage, path_fig + 'cg_swio_monthclim_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2)+'.gif'
print, total(hist_cgswio_monthclim[0:2], /NAN)+hist_cgswio_monthclim[11]

barname = ['JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC'] & !y.range[0] = 0 & !y.range[1] = 6
bar_plot, hist_cgseio_monthclim, background=255, barnames=barname, title='CLIMATOLOGICAL NUMBER OF CYCLOGENESIS (> 17.5 m/s) IN SEIO - '+expname, xtitle='month', ytitle='number', /outline, tickv=tickv, colors=[indgen(n_elements(hist_cgseio_monthclim))*0]
xyouts, 8, 4, 'TOTAL:'+strtrim(total(hist_cgseio_monthclim, /NAN),2) , charsize = 1.5
saveimage, path_fig + 'cg_seio_monthclim_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2)+'.gif'
print, total(hist_cgseio_monthclim[0:2], /NAN)+hist_cgseio_monthclim[11]

; POURCENTAGES
barname = ['JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC'] & !y.range[0] = 0 & !y.range[1] = 30
bar_plot, hist_cgsio_monthclim/total(hist_cgsio_monthclim, /NAN)*100., background=255, barnames=barname, title='CLIMATOLOGICAL NUMBER OF CYCLOGENESIS (> 17.5 m/s) IN SIO - '+expname, xtitle='month', ytitle='number', /outline, tickv=tickv, colors=[indgen(n_elements(hist_cgsio_monthclim))*0]
xyouts, 8, 25, 'TOTAL:'+strtrim(total(hist_cgsio_monthclim/total(hist_cgsio_monthclim, /NAN), /NAN),2) , charsize = 1.5
saveimage, path_fig + 'cg_percent_sio_monthclim_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2)+'.gif'

barname = ['JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC'] & !y.range[0] = 0 & !y.range[1] = 30
bar_plot, hist_cgswio_monthclim/total(hist_cgswio_monthclim, /NAN)*100., background=255, barnames=barname, title='CLIMATOLOGICAL NUMBER OF CYCLOGENESIS (> 17.5 m/s) IN SWIO - '+expname, xtitle='month', ytitle='number', /outline, tickv=tickv, colors=[indgen(n_elements(hist_cgswio_monthclim))*0]
xyouts, 8, 25, 'TOTAL:'+strtrim(total(hist_cgswio_monthclim/total(hist_cgswio_monthclim, /NAN), /NAN),2) , charsize = 1.5
saveimage, path_fig + 'cg_percent_swio_monthclim_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2)+'.gif'

barname = ['JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC'] & !y.range[0] = 0 & !y.range[1] = 30
bar_plot, hist_cgseio_monthclim/total(hist_cgseio_monthclim, /NAN)*100., background=255, barnames=barname, title='CLIMATOLOGICAL NUMBER OF CYCLOGENESIS (> 17.5 m/s) IN SEIO - '+expname, xtitle='month', ytitle='number', /outline, tickv=tickv, colors=[indgen(n_elements(hist_cgseio_monthclim))*0]
xyouts, 8, 25, 'TOTAL:'+strtrim(total(hist_cgseio_monthclim/total(hist_cgseio_monthclim, /NAN), /NAN),2) , charsize = 1.5
saveimage, path_fig + 'cg_percent_seio_monthclim_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2)+'.gif'
ENDIF


IF jyear THEN BEGIN
;-----------------------------------------------------------------------------------------
; CG INTERANNUEL
;-----------------------------------------------------------------------------------------
indcgsio  = where(latcycn[*,0] LE 0. AND latcycn[*,0] GE -30. AND loncycn[*,0] GE 30. AND loncycn[*,0] LE 140., cntcgsio)  & help, cntcgsio
indcgswio = where(latcycn[*,0] LE 0. AND latcycn[*,0] GE -30. AND loncycn[*,0] GE 30. AND loncycn[*,0] LE 80., cntcgswio)  & help, cntcgswio
indcgseio = where(latcycn[*,0] LE 0. AND latcycn[*,0] GE -30. AND loncycn[*,0] GE 80. AND loncycn[*,0] LE 140., cntcgseio) & help, cntcgseio
hist_cgsio_interannual  = histogram(yearcycn[indcgsio,0],  min=firsty,max=lasty,nbins=nbyear, /NAN)
hist_cgswio_interannual = histogram(yearcycn[indcgswio,0], min=firsty,max=lasty,nbins=nbyear, /NAN)
hist_cgseio_interannual = histogram(yearcycn[indcgseio,0], min=firsty,max=lasty,nbins=nbyear, /NAN)

barname = indgen(20) + firsty & !y.range[0] = 0 & !y.range[1] = 35
bar_plot, hist_cgsio_interannual, background=255, barnames=barname, title='INTERANNUAL NUMBER OF CYCLOGENESIS (> 17.5 m/s) IN SIO - '+expname, xtitle='year', ytitle='number of cyclogenesis', /outline, colors=[indgen(n_elements(hist_cgsio_interannual))*0]
xyouts, 8, 31, 'MEAN:'+strtrim(mean(hist_cgsio_interannual, /NAN),2) , charsize = 1.5
xyouts, 8, 29, 'STD DEV:'+strtrim(stddev(hist_cgsio_interannual, /NAN),2) , charsize = 1.5
saveimage, path_fig + 'cg_sio_interannual_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2)+'.gif'

barname = indgen(20) + firsty & !y.range[0] = 0 & !y.range[1] = 35
bar_plot, hist_cgswio_interannual, background=255, barnames=barname, title='INTERANNUAL NUMBER OF CYCLOGENESIS (> 17.5 m/s) IN SWIO - '+expname, xtitle='year', ytitle='number of cyclogenesis', /outline, colors=[indgen(n_elements(hist_cgswio_interannual))*0]
xyouts, 8, 31, 'MEAN:'+strtrim(mean(hist_cgswio_interannual, /NAN),2) , charsize = 1.5
xyouts, 8, 29, 'STD DEV:'+strtrim(stddev(hist_cgswio_interannual, /NAN),2) , charsize = 1.5
saveimage, path_fig + 'cg_swio_interannual_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2)+'.gif'

barname = indgen(20) + firsty & !y.range[0] = 0 & !y.range[1] = 35
bar_plot, hist_cgseio_interannual, background=255, barnames=barname, title='INTERANNUAL NUMBER OF CYCLOGENESIS (> 17.5 m/s) IN SEIO - '+expname, xtitle='year', ytitle='number of cyclogenesis', /outline, colors=[indgen(n_elements(hist_cgseio_interannual))*0]
xyouts, 8, 31, 'MEAN:'+strtrim(mean(hist_cgseio_interannual, /NAN),2) , charsize = 1.5
xyouts, 8, 29, 'STD DEV:'+strtrim(stddev(hist_cgseio_interannual, /NAN),2) , charsize = 1.5
saveimage, path_fig + 'cg_seio_interannual_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2)+'.gif'

ENDIF


IF jdays THEN BEGIN
;-----------------------------------------------------------------------------------------
; NOMBRE JOURS CYCLINIQUES CLIM MENSUELLE
;-----------------------------------------------------------------------------------------
indwind = where(uv10cycn GE 17.5, cntwind)
indsio  = where(latcycn[indwind] LE 0. AND latcycn[indwind] GE -30., cntsio)
indswio = where(latcycn[indwind] LE 0. AND latcycn[indwind] GE -30. AND loncycn LE 80., cntswio)
indseio = where(latcycn[indwind] LE 0. AND latcycn[indwind] GE -30. AND loncycn GE 80., cntseio)
hist_cycdays_sio_monthclim   = histogram(monthcycn[indwind[indsio]],  min=1,max=12,nbins=12, /NAN) / (nbyear*4.)
hist_cycdays_swio_monthclim = histogram(monthcycn[indwind[indswio]], min=1,max=12,nbins=12, /NAN) / (nbyear*4.)
hist_cycdays_seio_monthclim = histogram(monthcycn[indwind[indseio]], min=1,max=12,nbins=12, /NAN) / (nbyear*4.)
hist_cycdays_sio_monthly  = hist_2D(yearcycn[indwind[indsio]], monthcycn[indwind[indsio]] ,bin1=1,min1=firsty,max1=lasty,bin2=1,min2=1,max2=12) / 4.
hist_cycdays_swio_monthly = hist_2D(yearcycn[indwind[indswio]],monthcycn[indwind[indswio]],bin1=1,min1=firsty,max1=lasty,bin2=1,min2=1,max2=12) / 4.
hist_cycdays_seio_monthly = hist_2D(yearcycn[indwind[indseio]],monthcycn[indwind[indseio]],bin1=1,min1=firsty,max1=lasty,bin2=1,min2=1,max2=12) / 4.

; NOMBRE DE JOURS CYCLONIQUES
barname = ['JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC'] & !y.range[0] = 0 & !y.range[1] = 50
bar_plot, hist_cycdays_sio_monthclim, background=255, barnames=barname, title='CLIMATOLOGICAL NUMBER OF CYCLONIC DAYS (> 17.5 m/s) IN SIO - '+expname, xtitle='month', ytitle='number of cyclonic days', /outline
xyouts, 6, 45, 'TOTAL:'+strtrim(total(hist_cycdays_sio_monthclim, /NAN),2) , charsize = 1.5
saveimage, path_fig + 'cycdays_sio_monthclim_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2)+'.gif'

barname = ['JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC'] & !y.range[0] = 0 & !y.range[1] = 50
bar_plot, hist_cycdays_swio_monthclim, background=255, barnames=barname, title='CLIMATOLOGICAL NUMBER OF CYCLONIC DAYS (> 17.5 m/s) IN SWIO - '+expname, xtitle='month', ytitle='number of cyclonic days', /outline
xyouts, 6, 45, 'TOTAL:'+strtrim(total(hist_cycdays_swio_monthclim, /NAN),2) , charsize = 1.5
saveimage, path_fig + 'cycdays_swio_monthclim_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2)+'.gif'

barname = ['JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC'] & !y.range[0] = 0 & !y.range[1] = 50
bar_plot, hist_cycdays_seio_monthclim, background=255, barnames=barname, title='CLIMATOLOGICAL NUMBER OF CYCLONIC DAYS (> 17.5 m/s) IN SEIO - '+expname, xtitle='month', ytitle='number of cyclonic days', /outline
xyouts, 6, 45, 'TOTAL:'+strtrim(total(hist_cycdays_seio_monthclim, /NAN),2) , charsize = 1.5
saveimage, path_fig + 'cycdays_seio_monthclim_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2)+'.gif'

; POURCENTAGE DE JOURS CYCLONIQUES
barname = ['JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC'] & !y.range[0] = 0 & !y.range[1] = 70
bar_plot, hist_cycdays_sio_monthclim/total(hist_cycdays_sio_monthclim, /NAN)*100., background=255, barnames=barname, title='CLIMATOLOGICAL NUMBER OF CYCLONIC DAYS (> 17.5 m/s) IN SIO - '+expname, xtitle='month', ytitle='percentage of cyclonic days', /outline
xyouts, 6, 45, 'TOTAL:'+strtrim(total(hist_cycdays_sio_monthclim, /NAN),2) , charsize = 1.5
saveimage, path_fig + 'cycdays_percent_sio_monthclim_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2)+'.gif'

barname = ['JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC'] & !y.range[0] = 0 & !y.range[1] = 70
bar_plot, hist_cycdays_swio_monthclim/total(hist_cycdays_sio_monthclim, /NAN)*100., background=255, barnames=barname, title='CLIMATOLOGICAL NUMBER OF CYCLONIC DAYS (> 17.5 m/s) IN SWIO - '+expname, xtitle='month', ytitle='percentage of cyclonic days', /outline
xyouts, 6, 45, 'TOTAL:'+strtrim(total(hist_cycdays_swio_monthclim, /NAN),2) , charsize = 1.5
saveimage, path_fig + 'cycdays_percent_swio_monthclim_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2)+'.gif'

barname = ['JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC'] & !y.range[0] = 0 & !y.range[1] = 70
bar_plot, hist_cycdays_seio_monthclim/total(hist_cycdays_sio_monthclim, /NAN)*100., background=255, barnames=barname, title='CLIMATOLOGICAL NUMBER OF CYCLONIC DAYS (> 17.5 m/s) IN SEIO - '+expname, xtitle='month', ytitle='percentage of cyclonic days', /outline
xyouts, 6, 45, 'TOTAL:'+strtrim(total(hist_cycdays_seio_monthclim, /NAN),2) , charsize = 1.5
saveimage, path_fig + 'cycdays_percent_seio_monthclim_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2)+'.gif'


;-----------------------------------------------------------------------------------------
; NOMBRE JOURS CYCLINIQUES INTERANNUEL
;-----------------------------------------------------------------------------------------
hist_cycdays_sio_interannual  = histogram(yearcycn[indwind[indsio]],  min=firsty,max=lasty,nbins=nbyear, /NAN) / 4.
hist_cycdays_swio_interannual = histogram(yearcycn[indwind[indswio]], min=firsty,max=lasty,nbins=nbyear, /NAN) / 4.
hist_cycdays_seio_interannual = histogram(yearcycn[indwind[indseio]], min=firsty,max=lasty,nbins=nbyear, /NAN) / 4.

barname = indgen(20) + firsty & !y.range[0] = 0 & !y.range[1] = 320
bar_plot, hist_cycdays_sio_interannual, background=255, barnames=barname, title='INTERANNUAL NUMBER OF CYCLONIC DAYS (> 17.5 m/s) IN SIO - '+expname, xtitle='year', ytitle='number', /outline
xyouts, 8, 300, 'MEAN:'+strtrim(mean(hist_cycdays_sio_interannual, /NAN),2) , charsize = 1.5
xyouts, 8, 280, 'STD DEV:'+strtrim(stddev(hist_cycdays_sio_interannual, /NAN),2) , charsize = 1.5
saveimage, path_fig + 'cycdays_sio_interannual_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2)+'.gif'
ENDIF


IF jdens THEN BEGIN
;-----------------------------------------------------------------------------------------
; DENSITY
;-----------------------------------------------------------------------------------------
density_cg      = hist_2D(loncycn[*,0], latcycn[*,0], bin1=5, min1=30, max1=130, bin2=5, min2=-30, max2=30) / (nbyear+1)
density_cg_sio  = hist_2D(loncycn[*,0], latcycn[*,0], bin1=5, min1=30, max1=130, bin2=5, min2=-30, max2=0)  / (nbyear+1)
density_cg_swio = hist_2D(loncycn[*,0], latcycn[*,0], bin1=5, min1=30, max1=80,  bin2=5, min2=-30, max2=0)  / (nbyear+1)
density_cg_seio = hist_2D(loncycn[*,0], latcycn[*,0], bin1=5, min1=80, max1=130, bin2=5, min2=-30, max2=0)  / (nbyear+1)

;computegrid,30,-30,5,5,21,13             
;plt, density_cg, min=0, max=1, lct=81, /nocont, /realcont, /landscape, title='IO CYCLOGENESIS DENSITY - '+expname, subtitle='cyclogenesis per year per 5 deg bin'
;saveimage, path_fig + 'density_cg_io_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2)+'.gif'

title = 'density_cg_sio_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2) & openps, filename=title, /landscape
computegrid,30,-30,5,5,21,7
box=[30,130,-30,0]
plt, density_cg_sio, min=0, max=1, lct=81, box=box, /nocont, /realcont, /landscape, title='SIO CYCLOGENESIS DENSITY - '+expname, subtitle='cyclogenesis per year per 5 deg bin'
;saveimage, path_fig + 'density_cg_sio_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2)+'.gif'
closeps
stop

computegrid,30,-30,5,5,21,7         
plt, density_cg_sio/total(density_cg_sio)*100., min=0, max=6, lct=81, /nocont, /realcont, /landscape, title='SIO CYCLOGENESIS DENSITY - '+expname, subtitle='cyclogenesis per year per 5 deg bin'
saveimage, path_fig + 'density_percent_cg_sio_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2)+'.gif'

;density_tc = hist_2D(loncycn[indwind], latcycn[indwind], bin1=2.5, min1=30, max1=130, bin2=2.5, min2=-30, max2=30) / (4. * nbyear)

;computegrid,30,-30,2.5,2.5,41,25             
;plt, density_tc, min=0, max=2, lct=81, /nocont, /realcont, /landscape, title='CYCLONE DENSITY', subtitle='cyclonic days per year per 2.5 deg bin'
;saveimage, path_fig + 'density_tc_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2)+'.gif'
ENDIF

ENDIF; basin



IF basin EQ 'NI' THEN BEGIN

IF jwind THEN BEGIN
;-----------------------------------------------------------------------------------------
; DISTRIBUTION INTENSITE EN JOURS CYCLONIQUES
;-----------------------------------------------------------------------------------------

indwind = where(uv10cycn GE 17.5, cntwind)

indsio  = where(latcycn[indwind] LE 0. AND latcycn[indwind] GE -30., cntsio)
indswio = where(latcycn[indwind] LE 0. AND latcycn[indwind] GE -30. AND loncycn LE 80., cntswio)
indseio = where(latcycn[indwind] LE 0. AND latcycn[indwind] GE -30. AND loncycn GE 80., cntseio)
;indnio  = where(latcycn[indwind] GE 0., cntnio)
;indnwio = where(latcycn[indwind] GE 0. AND loncycn LE 80., cntnwio)
;indneio = where(latcycn[indwind] GE 0. AND loncycn GE 80., cntneio)

hist_uv10day    = histogram(uv10cycn[indwind], min=17.5,max=67.5,binsize=5, /NAN) / (4. * nbyear)

hist_uv10sioday = histogram(uv10cycn[indwind[indsio]], min=17.5,max=67.5,binsize=5, /NAN) / (4. * nbyear)
hist_uv10swioday = histogram(uv10cycn[indwind[indswio]], min=17.5,max=67.5,binsize=5, /NAN) / (4. * nbyear)
hist_uv10seioday = histogram(uv10cycn[indwind[indseio]], min=17.5,max=67.5,binsize=5, /NAN) / (4. * nbyear)

;hist_uv10nioday = histogram(uv10cycn[indwind[indnio]], min=17.5,max=67.5,binsize=5, /NAN) / (4. * nbyear)
;hist_uv10nwioday = histogram(uv10cycn[indwind[indnwio]], min=17.5,max=67.5,binsize=5, /NAN) / (4. * nbyear)
;hist_uv10neioday = histogram(uv10cycn[indwind[indneio]], min=17.5,max=67.5,binsize=5, /NAN) / (4. * nbyear)

;barname = strtrim(indgen(10)*5+20,2) & !y.range[0] = 0 & !y.range[1] = 150
;bar_plot, hist_uv10day[0:9], background=255, barnames=barname, title='WIND DISTRIBUTION - '+expname, xtitle='wind (m/s)', ytitle='number of cyclonic days per year', /outline
;xyouts, 6, 130, 'TOTAL:'+strtrim(total(hist_uv10day, /NAN),2) , charsize = 1.5
;saveimage, path_fig + 'uv10day_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2)+'.gif'

barname = strtrim(indgen(10)*5+20,2) & !y.range[0] = 0 & !y.range[1] = 100
bar_plot, hist_uv10sioday[0:9], background=255, barnames=barname, title='SIO WIND DISTRIBUTION - '+expname, xtitle='wind (m/s)', ytitle='number of cyclonic days per year', /outline
xyouts, 6, 80, 'TOTAL:'+strtrim(total(hist_uv10sioday, /NAN),2) , charsize = 1.5
saveimage, path_fig + 'uv10day_sio_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2)+'.gif'

barname = strtrim(indgen(10)*5+20,2) & !y.range[0] = 0 & !y.range[1] = 100
bar_plot, hist_uv10swioday[0:9], background=255, barnames=barname, title='SEIO WIND DISTRIBUTION - '+expname, xtitle='wind (m/s)', ytitle='number of cyclonic days per year', /outline
xyouts, 6, 80, 'TOTAL:'+strtrim(total(hist_uv10sioday, /NAN),2) , charsize = 1.5
saveimage, path_fig + 'uv10day_swio_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2)+'.gif'

barname = strtrim(indgen(10)*5+20,2) & !y.range[0] = 0 & !y.range[1] = 100
bar_plot, hist_uv10seioday[0:9], background=255, barnames=barname, title='SWIO WIND DISTRIBUTION - '+expname, xtitle='wind (m/s)', ytitle='number of cyclonic days per year', /outline
xyouts, 6, 80, 'TOTAL:'+strtrim(total(hist_uv10sioday, /NAN),2) , charsize = 1.5
saveimage, path_fig + 'uv10day_seio_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2)+'.gif'

;barname = strtrim(indgen(10)*5+20,2) & !y.range[0] = 0 & !y.range[1] = 50
;bar_plot, hist_uv10nioday[0:9], background=255, barnames=barname, title='NIO WIND DISTRIBUTION - '+expname, xtitle='wind (m/s)', ytitle='number of cyclonic days per year', /outline
;xyouts, 6, 40, 'TOTAL:'+strtrim(total(hist_uv10nioday, /NAN),2) , charsize = 1.5
;saveimage, path_fig + 'uv10day_nio_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2)+'.gif'



;-----------------------------------------------------------------------------------------
; DISTRIBUTION INTENSITE EN POURCENTAGE
;-----------------------------------------------------------------------------------------

indwind = where(uv10cycn GE 17.5, cntwind)

indsio  = where(latcycn[indwind] LE 0. AND latcycn[indwind] GE -30., cntsio)
indswio = where(latcycn[indwind] LE 0. AND latcycn[indwind] GE -30. AND loncycn LE 80., cntswio)
indseio = where(latcycn[indwind] LE 0. AND latcycn[indwind] GE -30. AND loncycn GE 80., cntseio)
;indnio  = where(latcycn[indwind] GE 0., cntnio)
;indnwio = where(latcycn[indwind] GE 0. AND loncycn LE 80., cntnwio)
;indneio = where(latcycn[indwind] GE 0. AND loncycn GE 80., cntneio)


hist_uv10percent    = histogram(uv10cycn[indwind], min=17.5,max=62.5,binsize=5, /NAN) / float(n_elements(uv10cycn[indwind]))

hist_uv10siopercent = histogram(uv10cycn[indwind[indsio]], min=17.5,max=62.5,binsize=5, /NAN) / float(n_elements(uv10cycn[indwind[indsio]]))
hist_uv10swiopercent = histogram(uv10cycn[indwind[indswio]], min=17.5,max=62.5,binsize=5, /NAN) / float(n_elements(uv10cycn[indwind[indswio]]))
hist_uv10seiopercent = histogram(uv10cycn[indwind[indseio]], min=17.5,max=62.5,binsize=5, /NAN) / float(n_elements(uv10cycn[indwind[indseio]]))

;hist_uv10niopercent = histogram(uv10cycn[indwind[indnio]], min=17.5,max=62.5,binsize=5, /NAN) / float(n_elements(uv10cycn[indwind[indnio]]))
;hist_uv10nwiopercent = histogram(uv10cycn[indwind[indnwio]], min=17.5,max=62.5,binsize=5, /NAN) / float(n_elements(uv10cycn[indwind[indnwio]]))
;hist_uv10neiopercent = histogram(uv10cycn[indwind[indneio]], min=17.5,max=62.5,binsize=5, /NAN) / float(n_elements(uv10cycn[indwind[indneio]]))


;barname = findgen(9)*5 + nbyear & !y.range[0] = 0 & !y.range[1] = 0.6
;bar_plot, hist_uv10percent[0:8], background=255, barnames=barname, title='IO WIND DISTRIBUTION - '+expname, xtitle='wind (m/s)', ytitle='percentage', /outline
;xyouts, 6, 0.5, 'TOTAL:'+strtrim(total(hist_uv10percent, /NAN),2) , charsize = 1.5
;saveimage, path_fig + 'uv10day_percent_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2)+'.gif'

barname = findgen(9)*5 + nbyear & !y.range[0] = 0 & !y.range[1] = 0.6
bar_plot, hist_uv10siopercent[0:8], background=255, barnames=barname, title='SIO WIND DISTRIBUTION - '+expname, xtitle='wind (m/s)', ytitle='percentage', /outline
xyouts, 6, 0.5, 'TOTAL:'+strtrim(total(hist_uv10siopercent, /NAN),2) , charsize = 1.5
saveimage, path_fig + 'uv10day_percent_sio_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2)+'.gif'

barname = findgen(9)*5 + nbyear & !y.range[0] = 0 & !y.range[1] = 0.6
bar_plot, hist_uv10swiopercent[0:8], background=255, barnames=barname, title='SWIO WIND DISTRIBUTION - '+expname, xtitle='wind (m/s)', ytitle='percentage', /outline
xyouts, 6, 0.5, 'TOTAL:'+strtrim(total(hist_uv10swiopercent, /NAN),2) , charsize = 1.5
saveimage, path_fig + 'uv10day_percent_swio_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2)+'.gif'

barname = findgen(9)*5 + nbyear & !y.range[0] = 0 & !y.range[1] = 0.6
bar_plot, hist_uv10seiopercent[0:8], background=255, barnames=barname, title='SEIO WIND DISTRIBUTION - '+expname, xtitle='wind (m/s)', ytitle='percentage', /outline
xyouts, 6, 0.5, 'TOTAL:'+strtrim(total(hist_uv10seiopercent, /NAN),2) , charsize = 1.5
saveimage, path_fig + 'uv10day_percent_seio_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2)+'.gif'

;barname = findgen(9)*5 + nbyear & !y.range[0] = 0 & !y.range[1] = 0.6
;bar_plot, hist_uv10niopercent[0:8], background=255, barnames=barname, title='NIO WIND DISTRIBUTION - '+expname, xtitle='wind (m/s)', ytitle='percentage', /outline
;xyouts, 6, 0.5, 'TOTAL:'+strtrim(total(hist_uv10niopercent, /NAN),2) , charsize = 1.5
;saveimage, path_fig + 'uv10day_percent_nio_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2)+'.gif'
;
;barname = findgen(9)*5 + nbyear & !y.range[0] = 0 & !y.range[1] = 0.6
;bar_plot, hist_uv10nwiopercent[0:8], background=255, barnames=barname, title='NWIO WIND DISTRIBUTION - '+expname, xtitle='wind (m/s)', ytitle='percentage', /outline
;xyouts, 6, 0.5, 'TOTAL:'+strtrim(total(hist_uv10nwiopercent, /NAN),2) , charsize = 1.5
;saveimage, path_fig + 'uv10day_percent_nwio_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2)+'.gif'
;
;barname = findgen(9)*5 + nbyear & !y.range[0] = 0 & !y.range[1] = 0.6
;bar_plot, hist_uv10neiopercent[0:8], background=255, barnames=barname, title='NEIO WIND DISTRIBUTION - '+expname, xtitle='wind (m/s)', ytitle='percentage', /outline
;xyouts, 6, 0.5, 'TOTAL:'+strtrim(total(hist_uv10neiopercent, /NAN),2) , charsize = 1.5
;saveimage, path_fig + 'uv10day_percent_neio_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2)+'.gif'

ENDIF


IF jmois THEN BEGIN

;-----------------------------------------------------------------------------------------
; CG CLIM MENSUEL
;-----------------------------------------------------------------------------------------

indcgsio  = where(latcycn[*,0] LE 0. AND latcycn[*,0] GE -30. AND loncycn[*,0] GE 30. AND loncycn[*,0] LE 140., cntcgsio) & help, cntcgsio
indcgswio = where(latcycn[*,0] LE 0. AND latcycn[*,0] GE -30. AND loncycn[*,0] GE 30. AND loncycn[*,0] LE 80., cntcgswio) & help, cntcgswio
indcgseio = where(latcycn[*,0] LE 0. AND latcycn[*,0] GE -30. AND loncycn[*,0] GE 80. AND loncycn[*,0] LE 140., cntcgseio) & help, cntcgseio

;indcgnio  = where(latcycn[*,0] GE 0. AND latcycn[*,0] LE 30. AND loncycn[*,0] GE 30. AND loncycn[*,0] LE 100., cntnio)
;indcgnwio = where(latcycn[*,0] GE 0. AND loncycn[*,0] LE 80. AND loncycn[*,0] GE 30. AND loncycn[*,0] LE 80., cntcgnwio)
;indcgneio = where(latcycn[*,0] GE 0. AND loncycn[*,0] GE 80. AND loncycn[*,0] GE 80. AND loncycn[*,0] LE 100., cntcgneio)


hist_cgsio_monthclim  = histogram(monthcycn[indcgsio,0],  min=1,max=12,nbins=12, /NAN) / nbyear
hist_cgswio_monthclim = histogram(monthcycn[indcgswio,0], min=1,max=12,nbins=12, /NAN) / nbyear
hist_cgseio_monthclim = histogram(monthcycn[indcgseio,0], min=1,max=12,nbins=12, /NAN) / nbyear
;hist_cgnio_monthclim  = histogram(monthcycn[indcgnio,0],  min=1,max=12,nbins=12, /NAN) / nbyear
;hist_cgnwio_monthclim = histogram(monthcycn[indcgnwio,0], min=1,max=12,nbins=12, /NAN) / nbyear
;hist_cgneio_monthclim = histogram(monthcycn[indcgneio,0], min=1,max=12,nbins=12, /NAN) / nbyear

hist_cgsio_monthly  = hist_2D(yearcycn[indcgsio,0],monthcycn[indcgsio,0],bin1=1,min1=firsty,max1=lasty,min2=1,max2=12)
hist_cgswio_monthly = hist_2D(yearcycn[indcgswio,0],monthcycn[indcgswio,0],bin1=1,min1=firsty,max1=lasty,min2=1,max2=12)
hist_cgseio_monthly = hist_2D(yearcycn[indcgseio,0],monthcycn[indcgseio,0],bin1=1,min1=firsty,max1=lasty,min2=1,max2=12)
;hist_cgnio_monthly  = hist_2D(yearcycn[indcgnio,0],monthcycn[indcgnio,0],bin1=1,min1=firsty,max1=lasty,min2=1,max2=12)
;hist_cgnwio_monthly = hist_2D(yearcycn[indcgnwio,0],monthcycn[indcgnwio,0],bin1=1,min1=firsty,max1=lasty,min2=1,max2=12)
;hist_cgneio_monthly = hist_2D(yearcycn[indcgneio,0],monthcycn[indcgneio,0],bin1=1,min1=firsty,max1=lasty,min2=1,max2=12)

; NB CYCLONES
;std_dev_month = fltarr(12)
;FOR i = 0,11 DO std_dev_month[i] = stddev(hist_cgsio_monthly[*,i], /NAN)
barname = ['JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC'] & !y.range[0] = 0 & !y.range[1] = 6
bar_plot, hist_cgsio_monthclim, background=255, barnames=barname, title='CLIMATOLOGICAL NUMBER OF CYCLOGENESIS (> 17.5 m/s) IN SIO - '+expname, xtitle='month', ytitle='number', /outline, tickv=tickv, colors=[indgen(n_elements(hist_cgsio_monthclim))*0]
;oploterr, tickv, hist_cgsio_monthclim, std_dev_month
;errplot, tickv, hist_cgsio_monthclim - std_dev_month, hist_cgsio_monthclim + std_dev_month
xyouts, 6, 3, 'TOTAL:'+strtrim(total(hist_cgsio_monthclim, /NAN),2) , charsize = 1.5
saveimage, path_fig + 'cg_sio_monthclim_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2)+'.gif'
print, total(hist_cgsio_monthclim[0:2], /NAN)+hist_cgsio_monthclim[11]

barname = ['JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC'] & !y.range[0] = 0 & !y.range[1] = 6
bar_plot, hist_cgswio_monthclim, background=255, barnames=barname, title='CLIMATOLOGICAL NUMBER OF CYCLOGENESIS (> 17.5 m/s) IN SWIO - '+expname, xtitle='month', ytitle='number', /outline, tickv=tickv, colors=[indgen(n_elements(hist_cgswio_monthclim))*0]
xyouts, 6, 3, 'TOTAL:'+strtrim(total(hist_cgswio_monthclim, /NAN),2) , charsize = 1.5
saveimage, path_fig + 'cg_swio_monthclim_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2)+'.gif'
print, total(hist_cgswio_monthclim[0:2], /NAN)+hist_cgswio_monthclim[11]

barname = ['JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC'] & !y.range[0] = 0 & !y.range[1] = 6
bar_plot, hist_cgseio_monthclim, background=255, barnames=barname, title='CLIMATOLOGICAL NUMBER OF CYCLOGENESIS (> 17.5 m/s) IN SEIO - '+expname, xtitle='month', ytitle='number', /outline, tickv=tickv, colors=[indgen(n_elements(hist_cgseio_monthclim))*0]
xyouts, 6, 3, 'TOTAL:'+strtrim(total(hist_cgseio_monthclim, /NAN),2) , charsize = 1.5
saveimage, path_fig + 'cg_seio_monthclim_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2)+'.gif'
print, total(hist_cgseio_monthclim[0:2], /NAN)+hist_cgseio_monthclim[11]

;barname = ['JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC'] & !y.range[0] = 0 & !y.range[1] = 3
;bar_plot, hist_cgnio_monthclim, background=255, barnames=barname, title='CLIMATOLOGICAL NUMBER OF CYCLOGENESIS (> 17.5 m/s) IN NIO - '+expname, xtitle='month', ytitle='number', /outline, colors=[indgen(n_elements(hist_cgnio_monthly))*0]
;;errplot, indgen(12), hist_cgnio_monthclim - std_dev_month, hist_cgnio_monthclim + std_dev_month
;xyouts, 6, 2.5, 'TOTAL:'+strtrim(total(hist_cgnio_monthclim, /NAN),2) , charsize = 1.5
;saveimage, path_fig + 'cg_nio_monthclim_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2)+'.gif'


; POURCENTAGES
barname = ['JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC'] & !y.range[0] = 0 & !y.range[1] = 30
bar_plot, hist_cgsio_monthclim/total(hist_cgsio_monthclim, /NAN)*100., background=255, barnames=barname, title='CLIMATOLOGICAL NUMBER OF CYCLOGENESIS (> 17.5 m/s) IN SIO - '+expname, xtitle='month', ytitle='number', /outline, tickv=tickv, colors=[indgen(n_elements(hist_cgsio_monthclim))*0]
;oploterr, tickv, hist_cgsio_monthclim, std_dev_month
;errplot, tickv, hist_cgsio_monthclim - std_dev_month, hist_cgsio_monthclim + std_dev_month
;xyouts, 6, 3, 'TOTAL:'+strtrim(total(hist_cgsio_monthclim, /NAN),2) , charsize = 1.5
saveimage, path_fig + 'cg_percent_sio_monthclim_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2)+'.gif'

barname = ['JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC'] & !y.range[0] = 0 & !y.range[1] = 30
bar_plot, hist_cgswio_monthclim/total(hist_cgswio_monthclim, /NAN)*100., background=255, barnames=barname, title='CLIMATOLOGICAL NUMBER OF CYCLOGENESIS (> 17.5 m/s) IN SWIO - '+expname, xtitle='month', ytitle='number', /outline, tickv=tickv, colors=[indgen(n_elements(hist_cgswio_monthclim))*0]
;xyouts, 6, 3, 'TOTAL:'+strtrim(total(hist_cgswio_monthclim, /NAN),2) , charsize = 1.5
saveimage, path_fig + 'cg_percent_swio_monthclim_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2)+'.gif'

barname = ['JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC'] & !y.range[0] = 0 & !y.range[1] = 30
bar_plot, hist_cgseio_monthclim/total(hist_cgseio_monthclim, /NAN)*100., background=255, barnames=barname, title='CLIMATOLOGICAL NUMBER OF CYCLOGENESIS (> 17.5 m/s) IN SEIO - '+expname, xtitle='month', ytitle='number', /outline, tickv=tickv, colors=[indgen(n_elements(hist_cgseio_monthclim))*0]
;xyouts, 6, 3, 'TOTAL:'+strtrim(total(hist_cgseio_monthclim, /NAN),2) , charsize = 1.5
saveimage, path_fig + 'cg_percent_seio_monthclim_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2)+'.gif'
ENDIF


IF jyear THEN BEGIN
;-----------------------------------------------------------------------------------------
; CG INTERANNUEL
;-----------------------------------------------------------------------------------------

hist_cgsio_interannual  = histogram(yearcycn[indcgsio,0],  min=firsty,max=lasty,nbins=nbyear, /NAN)
hist_cgswio_interannual = histogram(yearcycn[indcgswio,0], min=firsty,max=lasty,nbins=nbyear, /NAN)
hist_cgseio_interannual = histogram(yearcycn[indcgseio,0], min=firsty,max=lasty,nbins=nbyear, /NAN)
hist_cgnio_interannual  = histogram(yearcycn[indcgnio,0],  min=firsty,max=lasty,nbins=nbyear, /NAN)
hist_cgnwio_interannual = histogram(yearcycn[indcgnwio,0], min=firsty,max=lasty,nbins=nbyear, /NAN)
hist_cgneio_interannual = histogram(yearcycn[indcgneio,0], min=firsty,max=lasty,nbins=nbyear, /NAN)

barname = indgen(20) + firsty & !y.range[0] = 0 & !y.range[1] = 35
bar_plot, hist_cgsio_interannual, background=255, barnames=barname, title='INTERANNUAL NUMBER OF CYCLOGENESIS (> 17.5 m/s) IN SIO - '+expname, xtitle='year', ytitle='number', /outline, colors=[indgen(n_elements(hist_cgsio_interannual))*0]
xyouts, 8, 31, 'MEAN:'+strtrim(mean(hist_cgsio_interannual, /NAN),2) , charsize = 1.5
xyouts, 8, 30, 'STD DEV:'+strtrim(stddev(hist_cgsio_interannual, /NAN),2) , charsize = 1.5
saveimage, path_fig + 'cg_sio_interannual_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2)+'.gif'

barname = indgen(20) + firsty & !y.range[0] = 0 & !y.range[1] = 20
bar_plot, hist_cgnio_interannual, background=255, barnames=barname, title='INTERANNUAL NUMBER OF CYCLOGENESIS (> 17.5 m/s) IN NIO - '+expname, xtitle='year', ytitle='number', /outline, colors=[indgen(n_elements(hist_cgnio_interannual))*0]
xyouts, 8, 18, 'MEAN:'+strtrim(mean(hist_cgnio_interannual, /NAN),2) , charsize = 1.5
xyouts, 8, 17, 'STD DEV:'+strtrim(stddev(hist_cgnio_interannual, /NAN),2) , charsize = 1.5
saveimage, path_fig + 'cg_nio_interannual_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2)+'.gif'
ENDIF




IF jdays THEN BEGIN
;-----------------------------------------------------------------------------------------
; NOMBRE JOURS CYCLONIQUES CLIM MENSUELLE
;-----------------------------------------------------------------------------------------
hist_cycndays_sio_monthclim  = histogram(monthcycn[indwind[indsio]],  min=1,max=12,nbins=12, /NAN) / (nbyear*4.)
hist_cycndays_swio_monthclim = histogram(monthcycn[indwind[indswio]], min=1,max=12,nbins=12, /NAN) / (nbyear*4.)
hist_cycndays_seio_monthclim = histogram(monthcycn[indwind[indseio]], min=1,max=12,nbins=12, /NAN) / (nbyear*4.)
hist_cycndays_sio_monthly  = hist_2D(yearcycn[indwind[indsio]], monthcycn[indwind[indsio]] ,bin1=1,min1=firsty,max1=lasty,bin2=1,min2=1,max2=12) / 4.
hist_cycndays_swio_monthly = hist_2D(yearcycn[indwind[indswio]],monthcycn[indwind[indswio]],bin1=1,min1=firsty,max1=lasty,bin2=1,min2=1,max2=12) / 4.
hist_cycndays_seio_monthly = hist_2D(yearcycn[indwind[indseio]],monthcycn[indwind[indseio]],bin1=1,min1=firsty,max1=lasty,bin2=1,min2=1,max2=12) / 4.

barname = ['JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC'] & !y.range[0] = 0 & !y.range[1] = 50
bar_plot, hist_cycndays_sio_monthclim, background=255, barnames=barname, title='CLIMATOLOGICAL NUMBER OF CYCLONIC DAYS (> 17.5 m/s) IN SIO - '+expname, xtitle='month', ytitle='number', /outline, tickv=tickv
xyouts, 6, 45, 'TOTAL:'+strtrim(total(hist_cycndays_sio_monthclim, /NAN),2) , charsize = 1.5
saveimage, path_fig + 'cycndays_sio_monthclim_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2)+'.gif'

barname = ['JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC'] & !y.range[0] = 0 & !y.range[1] = 30
bar_plot, hist_cycndays_nio_monthclim, background=255, barnames=barname, title='CLIMATOLOGICAL NUMBER OF CYCLONIC DAYS (> 17.5 m/s) IN NIO - '+expname, xtitle='month', ytitle='number', /outline, tickv=tickv
xyouts, 6, 25, 'TOTAL:'+strtrim(total(hist_cycndays_nio_monthclim, /NAN),2) , charsize = 1.5
saveimage, path_fig + 'cycndays_nio_monthclim_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2)+'.gif'



;-----------------------------------------------------------------------------------------
; NOMBRE JOURS CYCLINIQUES INTERANNUEL
;-----------------------------------------------------------------------------------------

hist_cycndays_sio_interannual  = histogram(yearcycn[indwind[indsio]],  min=firsty,max=lasty,nbins=nbyear, /NAN) / 4.
hist_cycndays_swio_interannual = histogram(yearcycn[indwind[indswio]], min=firsty,max=lasty,nbins=nbyear, /NAN) / 4.
hist_cycndays_seio_interannual = histogram(yearcycn[indwind[indseio]], min=firsty,max=lasty,nbins=nbyear, /NAN) / 4.
hist_cycndays_nio_interannual  = histogram(yearcycn[indwind[indnio]],  min=firsty,max=lasty,nbins=nbyear, /NAN) / 4.
hist_cycndays_nwio_interannual = histogram(yearcycn[indwind[indnwio]], min=firsty,max=lasty,nbins=nbyear, /NAN) / 4.
hist_cycndays_neio_interannual = histogram(yearcycn[indwind[indneio]], min=firsty,max=lasty,nbins=nbyear, /NAN) / 4.

barname = indgen(20) + firsty & !y.range[0] = 0 & !y.range[1] = 320
bar_plot, hist_cycndays_sio_interannual, background=255, barnames=barname, title='INTERANNUAL NUMBER OF CYCLONIC DAYS (> 17.5 m/s) IN SIO - '+expname, xtitle='year', ytitle='number', /outline
xyouts, 8, 300, 'MEAN:'+strtrim(mean(hist_cycndays_sio_interannual, /NAN),2) , charsize = 1.5
xyouts, 8, 280, 'STD DEV:'+strtrim(stddev(hist_cycndays_sio_interannual, /NAN),2) , charsize = 1.5
saveimage, path_fig + 'cycndays_sio_interannual_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2)+'.gif'

barname = indgen(20) + firsty & !y.range[0] = 0 & !y.range[1] = 150
bar_plot, hist_cycndays_nio_interannual, background=255, barnames=barname, title='INTERANNUAL NUMBER OF CYCLONIC DAYS (> 17.5 m/s) IN NIO - '+expname, xtitle='year', ytitle='number', /outline
xyouts, 8, 140, 'MEAN:'+strtrim(mean(hist_cycndays_nio_interannual, /NAN),2) , charsize = 1.5
xyouts, 8, 130, 'STD DEV:'+strtrim(stddev(hist_cycndays_nio_interannual, /NAN),2) , charsize = 1.5
saveimage, path_fig + 'cycndays_nio_interannual_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2)+'.gif'
ENDIF


IF jdens THEN BEGIN
;-----------------------------------------------------------------------------------------
; DENSITY
;-----------------------------------------------------------------------------------------

density_cg = hist_2D(loncycn[*,0], latcycn[*,0], bin1=5, min1=30, max1=130, bin2=5, min2=-30, max2=30) / (nbyear+1)
density_cg_sio = hist_2D(loncycn[*,0], latcycn[*,0], bin1=5, min1=30, max1=130, bin2=5, min2=-30, max2=0) / (nbyear+1)
density_cg_swio = hist_2D(loncycn[*,0], latcycn[*,0], bin1=5, min1=30, max1=80, bin2=5, min2=-30, max2=0) / (nbyear+1)
density_cg_seio = hist_2D(loncycn[*,0], latcycn[*,0], bin1=5, min1=80, max1=130, bin2=5, min2=-30, max2=0) / (nbyear+1)


computegrid,30,-30,5,5,21,13             
plt, density_cg, min=0, max=1, lct=81, /nocont, /realcont, /landscape, title='CYCLOGENESIS DENSITY', subtitle='cyclogenesis per year per 5 deg bin'
saveimage, path_fig + 'density_cg_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2)+'.gif'

computegrid,30,-30,5,5,21,7
plt, density_cg_sio, min=0, max=1, lct=81, /nocont, /realcont, /landscape, title='CYCLOGENESIS DENSITY', subtitle='cyclogenesis per year per 5 deg bin'
saveimage, path_fig + 'density_cg_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2)+'.gif'

computegrid,30,-30,5,5,21,7         
plt, density_cg_sio/total(density_cg_sio)*100., min=0, max=6, lct=81, /nocont, /realcont, /landscape, title='CYCLOGENESIS DENSITY - '+expname, subtitle='cyclogenesis per year per 5 deg bin'
saveimage, path_fig + 'density_percent_cg_sio_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2)+'.gif'


;density_tc = hist_2D(loncycn[indwind], latcycn[indwind], bin1=2.5, min1=30, max1=130, bin2=2.5, min2=-30, max2=30) / (4. * nbyear)

;computegrid,30,-30,2.5,2.5,41,25             
;plt, density_tc, min=0, max=2, lct=81, /nocont, /realcont, /landscape, title='CYCLONE DENSITY', subtitle='cyclonic days per year per 2.5 deg bin'
;saveimage, path_fig + 'density_tc_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2)+'.gif'
ENDIF

ENDIF; basin


END
