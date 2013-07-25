PRO compare_2stats

@all_cm
reinitplt, all=all
maskfile = 'mask_wrf.nc'
initncdf,  maskfile


basin   = 'SI'
ucrit    = 17.5 
vorcrit  = 30.e-5
vorelax  = 30.e-5
tempcrit = 1.

expname1 = 'IBTRACS'
firsty1  = 1990 & firsty  = 1990
lasty1   = 2009 & lasty   = 2009
nbyear1  = float(lasty1 - firsty1 + 1)

expname2 = 'COUPLED_SW2_KF'
firsty2  = 1990
lasty2   = 2009
nbyear2  = float(lasty2 - firsty2 + 1)

expname3 = 'FORCED_SW2_KF'
firsty3  = 1990
lasty3   = 1998
nbyear3  = float(lasty3 - firsty3 + 1)


jwind = 0
jmois = 1
jyear = 0
jdays = 0
jdens = 0

path_fig = 'FIGS_COMP_'+ expname1 +'-'+ expname2 +'/u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2) +'/'
spawn, 'echo "creation du repertoire '+ path_fig +'"' & spawn, 'mkdir -p '+ path_fig


IF expname1 EQ 'IBTRACS' THEN BEGIN
  @/Users/gslod/WORK/IDL/TRACKER_IDL/read_ibtracs_wmo.pro
  loncyc1  = temporary(loncycn)
  latcyc1  = temporary(latcycn)
  uv10cyc1 = temporary(uv10cycn)
  mslpcyc1 = temporary(mslpcycn)
  monthcyc1 = temporary(monthcycn)
  yearcyc1  = temporary(yearcycn)
ENDIF ELSE BEGIN
  path_data1 = 'EXP_'+ expname1 +'/DATA/'
  file_in1 = 'tracker_light2_RVM_NICO_MASK_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2) +'_'+strtrim(firsty1,2)+'-'+strtrim(lasty1,2)+'.idl'
  restore, filename = path_data1 + file_in1, /VERBOSE
  loncyc1  = temporary(loncycn)
  latcyc1  = temporary(latcycn)
  uv10cyc1 = temporary(uv10cycn)
  rvmcyc1  = temporary(rvmcycn)
  mslpcyc1 = temporary(mslpcycn)
  vorcyc1  = temporary(vorcycn)
  anotcyc1 = temporary(anotcycn)
  juldcyc1 = temporary(juldcycn)
  datecyc1 = temporary(datecycn)
  monthcyc1 = long(datecyc1/100.) - long(datecyc1/10000.)*100.
  yearcyc1  = long(datecyc1/10000.)
ENDELSE

path_data2 = 'EXP_'+ expname2 +'/DATA/'
file_in2 = 'tracker_light2_RVM_NICO_MASK_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2) +'_'+strtrim(firsty2,2)+'-'+strtrim(lasty2,2)+'.idl'
restore, filename = path_data2 + file_in2, /VERBOSE
loncyc2  = temporary(loncycn)
latcyc2  = temporary(latcycn)
uv10cyc2 = temporary(uv10cycn)
rvmcyc2  = temporary(rvmcycn)
mslpcyc2 = temporary(mslpcycn)
vorcyc2  = temporary(vorcycn)
anotcyc2 = temporary(anotcycn)
juldcyc2 = temporary(juldcycn)
datecyc2 = temporary(datecycn)
monthcyc2 = long(datecyc2/100.) - long(datecyc2/10000.)*100.
yearcyc2  = long(datecyc2/10000.)

path_data3 = 'EXP_'+ expname3 +'/DATA/'
file_in3 = 'tracker_light2_RVM_NICO_MASK_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2) +'_'+strtrim(firsty3,2)+'-'+strtrim(lasty3,2)+'.idl'
restore, filename = path_data3 + file_in3, /VERBOSE
loncyc3  = temporary(loncycn)
latcyc3  = temporary(latcycn)
uv10cyc3 = temporary(uv10cycn)
rvmcyc3  = temporary(rvmcycn)
mslpcyc3 = temporary(mslpcycn)
vorcyc3  = temporary(vorcycn)
anotcyc3 = temporary(anotcycn)
juldcyc3 = temporary(juldcycn)
datecyc3 = temporary(datecycn)
monthcyc3 = long(datecyc3/100.) - long(datecyc3/10000.)*100.
yearcyc3  = long(datecyc3/10000.)


IF basin EQ 'SI' THEN BEGIN
IF jwind THEN BEGIN
;-----------------------------------------------------------------------------------------
; DISTRIBUTION INTENSITE EN JOURS CYCLONIQUES
;-----------------------------------------------------------------------------------------
indwind1 = where(uv10cyc1 GE 17.5, cntwind1)
indsio1  = where(latcyc1[indwind1] LE 0. AND latcyc1[indwind1] GE -30., cntsio1)
indswio1 = where(latcyc1[indwind1] LE 0. AND latcyc1[indwind1] GE -30. AND loncyc1[indwind1] LE 80., cntswio1)
indseio1 = where(latcyc1[indwind1] LE 0. AND latcyc1[indwind1] GE -30. AND loncyc1[indwind1] GE 80., cntseio1)
indwind2 = where(uv10cyc2 GE 17.5, cntwind2)
indsio2  = where(latcyc2[indwind2] LE 0. AND latcyc2[indwind2] GE -30., cntsio2)
indswio2 = where(latcyc2[indwind2] LE 0. AND latcyc2[indwind2] GE -30. AND loncyc2[indwind2] LE 80., cntswio2)
indseio2 = where(latcyc2[indwind2] LE 0. AND latcyc2[indwind2] GE -30. AND loncyc2[indwind2] GE 80., cntseio2)
indwind3 = where(uv10cyc3 GE 17.5, cntwind3)
indsio3  = where(latcyc3[indwind3] LE 0. AND latcyc3[indwind3] GE -30., cntsio3)
indswio3 = where(latcyc3[indwind3] LE 0. AND latcyc3[indwind3] GE -30. AND loncyc3[indwind3] LE 80., cntswio3)
indseio3 = where(latcyc3[indwind3] LE 0. AND latcyc3[indwind3] GE -30. AND loncyc3[indwind3] GE 80., cntseio3)

hist_uv10sio1  = histogram(uv10cyc1[indwind1[indsio1]],  min=17.5,max=67.5,binsize=5, /NAN) 
hist_uv10swio1 = histogram(uv10cyc1[indwind1[indswio1]], min=17.5,max=67.5,binsize=5, /NAN)
hist_uv10seio1 = histogram(uv10cyc1[indwind1[indseio1]], min=17.5,max=67.5,binsize=5, /NAN)
hist_uv10sio2  = histogram(uv10cyc2[indwind2[indsio2]],  min=17.5,max=67.5,binsize=5, /NAN) 
hist_uv10swio2 = histogram(uv10cyc2[indwind2[indswio2]], min=17.5,max=67.5,binsize=5, /NAN)
hist_uv10seio2 = histogram(uv10cyc2[indwind2[indseio2]], min=17.5,max=67.5,binsize=5, /NAN)
hist_uv10sio3  = histogram(uv10cyc3[indwind3[indsio3]],  min=17.5,max=67.5,binsize=5, /NAN) 
hist_uv10swio3 = histogram(uv10cyc3[indwind3[indswio3]], min=17.5,max=67.5,binsize=5, /NAN)
hist_uv10seio3 = histogram(uv10cyc3[indwind3[indseio3]], min=17.5,max=67.5,binsize=5, /NAN)

print, expname1
print, hist_uv10sio1/4
print, hist_uv10sio1 / float(n_elements(uv10cyc1[indwind1[indsio1]]))*100.

print, expname2
print, hist_uv10sio2/4
print, hist_uv10sio2 / float(n_elements(uv10cyc2[indwind2[indsio2]]))*100.

print, expname3
print, hist_uv10sio3/4
print, hist_uv10sio3 / float(n_elements(uv10cyc3[indwind3[indsio3]]))*100.
stop

; JOURS CYCLONIQUE
barname = strtrim(indgen(10)*5+20,2) & !y.range[0] = 0 & !y.range[1] = 100
bar_plot, hist_uv10sio1[0:9]/(4.*nbyear1), background=255, BAROFFSET=0, barspace=2, barwidth=0.6, baserange=0.36, barnames=barname, title='SIO WIND DISTRIBUTION: '+expname1+' - '+expname2, xtitle='wind (m/s)', ytitle='cyclonic days per year', /outline, colors=[lonarr(n_elements(hist_uv10sio1))+1*230]
xyouts, 6, 80, 'TOTAL1:'+strtrim(total(hist_uv10sio1, /NAN)/(4.*nbyear1),2) , charsize = 1.5
bar_plot, hist_uv10sio2[0:9]/(4.*nbyear2), background=255, BAROFFSET=4.5, barspace=2, barwidth=0.6, baserange=0.36, /outline,/over, colors=[lonarr(n_elements(hist_uv10sio2))+1*30]
xyouts, 6, 75, 'TOTAL2:'+strtrim(total(hist_uv10sio2, /NAN)/(4.*nbyear2),2) , charsize = 1.5
saveimage, path_fig + 'uv10day_sio.gif'

barname = strtrim(indgen(10)*5+20,2) & !y.range[0] = 0 & !y.range[1] = 100
bar_plot, hist_uv10swio1[0:9]/(4.*nbyear1), background=255, BAROFFSET=0, barspace=2, barwidth=0.6, baserange=0.36, barnames=barname, title='SWIO WIND DISTRIBUTION: '+expname1+' - '+expname2, xtitle='wind (m/s)', ytitle='cyclonic days per year', /outline, colors=[lonarr(n_elements(hist_uv10swio1))+1*230]
xyouts, 6, 80, 'TOTAL1:'+strtrim(total(hist_uv10swio1, /NAN)/(4.*nbyear1),2) , charsize = 1.5
bar_plot, hist_uv10swio2[0:9]/(4.*nbyear2), background=255, BAROFFSET=4.5, barspace=2, barwidth=0.6, baserange=0.36, /outline,/over, colors=[lonarr(n_elements(hist_uv10swio2))+1*30]
xyouts, 6, 75, 'TOTAL2:'+strtrim(total(hist_uv10swio2, /NAN)/(4.*nbyear2),2) , charsize = 1.5
saveimage, path_fig + 'uv10day_swio.gif'

barname = strtrim(indgen(10)*5+20,2) & !y.range[0] = 0 & !y.range[1] = 100
bar_plot, hist_uv10seio1[0:9]/(4.*nbyear1), background=255, BAROFFSET=0, barspace=2, barwidth=0.6, baserange=0.36, barnames=barname, title='SEIO WIND DISTRIBUTION: '+expname1+' - '+expname2, xtitle='wind (m/s)', ytitle='cyclonic days per year', /outline, colors=[lonarr(n_elements(hist_uv10seio1))+1*230]
xyouts, 6, 80, 'TOTAL1:'+strtrim(total(hist_uv10seio1, /NAN)/(4.*nbyear1),2) , charsize = 1.5
bar_plot, hist_uv10seio2[0:9]/(4.*nbyear2), background=255, BAROFFSET=4.5, barspace=2, barwidth=0.6, baserange=0.36, /outline,/over, colors=[lonarr(n_elements(hist_uv10seio2))+1*30]
xyouts, 6, 75, 'TOTAL2:'+strtrim(total(hist_uv10seio2, /NAN)/(4.*nbyear2),2) , charsize = 1.5
saveimage, path_fig + 'uv10day_seio.gif'


; POURCENTAGE
barname = strtrim(indgen(10)*5+20,2) & !y.range[0] = 0 & !y.range[1] = 60
bar_plot, hist_uv10sio1[0:9]/float(n_elements(uv10cyc1[indwind1[indsio1]]))*100., background=255, BAROFFSET=0, barspace=2, barwidth=0.6, baserange=0.36, barnames=barname, title='SIO WIND DISTRIBUTION: '+expname1+' - '+expname2, xtitle='wind (m/s)', ytitle='percentage', /outline, colors=[lonarr(n_elements(hist_uv10sio1))+1*230]
xyouts, 6, 50, 'TOTAL1:'+strtrim(total(hist_uv10sio1, /NAN)/(4.*nbyear1),2) , charsize = 1.5
bar_plot, hist_uv10sio2[0:9]/float(n_elements(uv10cyc2[indwind2[indsio2]]))*100., background=255, BAROFFSET=4.5, barspace=2, barwidth=0.6, baserange=0.36, /outline,/over, colors=[lonarr(n_elements(hist_uv10sio2))+1*30]
xyouts, 6, 45, 'TOTAL2:'+strtrim(total(hist_uv10sio2, /NAN)/(4.*nbyear2),2) , charsize = 1.5
saveimage, path_fig + 'uv10day_percent_sio.gif'

barname = strtrim(indgen(10)*5+20,2) & !y.range[0] = 0 & !y.range[1] = 60
bar_plot, hist_uv10swio1[0:9]/float(n_elements(uv10cyc1[indwind1[indswio1]]))*100., background=255, BAROFFSET=0, barspace=2, barwidth=0.6, baserange=0.36, barnames=barname, title='SWIO WIND DISTRIBUTION: '+expname1+' - '+expname2, xtitle='wind (m/s)', ytitle='percentage', /outline, colors=[lonarr(n_elements(hist_uv10swio1))+1*230]
xyouts, 6, 50, 'TOTAL1:'+strtrim(total(hist_uv10swio1, /NAN)/(4.*nbyear1),2) , charsize = 1.5
bar_plot, hist_uv10swio2[0:9]/float(n_elements(uv10cyc2[indwind2[indswio2]]))*100., background=255, BAROFFSET=4.5, barspace=2, barwidth=0.6, baserange=0.36, /outline,/over, colors=[lonarr(n_elements(hist_uv10swio2))+1*30]
xyouts, 6, 45, 'TOTAL2:'+strtrim(total(hist_uv10swio2, /NAN)/(4.*nbyear2),2) , charsize = 1.5
saveimage, path_fig + 'uv10day_percent_swio.gif'

barname = strtrim(indgen(10)*5+20,2) & !y.range[0] = 0 & !y.range[1] = 60
bar_plot, hist_uv10seio1[0:9]/float(n_elements(uv10cyc1[indwind1[indseio1]]))*100., background=255, BAROFFSET=0, barspace=2, barwidth=0.6, baserange=0.36, barnames=barname, title='SEIO WIND DISTRIBUTION: '+expname1+' - '+expname2, xtitle='wind (m/s)', ytitle='percentage', /outline, colors=[lonarr(n_elements(hist_uv10seio1))+1*230]
xyouts, 6, 50, 'TOTAL1:'+strtrim(total(hist_uv10seio1, /NAN)/(4.*nbyear1),2) , charsize = 1.5
bar_plot, hist_uv10seio2[0:9]/float(n_elements(uv10cyc2[indwind2[indseio2]]))*100., background=255, BAROFFSET=4.5, barspace=2, barwidth=0.6, baserange=0.36, /outline,/over, colors=[lonarr(n_elements(hist_uv10seio2))+1*30]
xyouts, 6, 45, 'TOTAL2:'+strtrim(total(hist_uv10seio2, /NAN)/(4.*nbyear2),2) , charsize = 1.5
saveimage, path_fig + 'uv10day_percent_seio.gif'

ENDIF



IF jmois THEN BEGIN

;-----------------------------------------------------------------------------------------
; CG CLIM MENSUEL
;-----------------------------------------------------------------------------------------

indcgsio1  = where(latcyc1[*,0] LE 0. AND latcyc1[*,0] GE -30. AND loncyc1[*,0] GE 30. AND loncyc1[*,0] LE 140., cntcgsio1) & help, cntcgsio1
indcgswio1 = where(latcyc1[*,0] LE 0. AND latcyc1[*,0] GE -30. AND loncyc1[*,0] GE 30. AND loncyc1[*,0] LE 80., cntcgswio1) & help, cntcgswio1
indcgseio1 = where(latcyc1[*,0] LE 0. AND latcyc1[*,0] GE -30. AND loncyc1[*,0] GE 80. AND loncyc1[*,0] LE 140., cntcgseio1) & help, cntcgseio1
indcgsio2  = where(latcyc2[*,0] LE 0. AND latcyc2[*,0] GE -30. AND loncyc2[*,0] GE 30. AND loncyc2[*,0] LE 140., cntcgsio2) & help, cntcgsio2
indcgswio2 = where(latcyc2[*,0] LE 0. AND latcyc2[*,0] GE -30. AND loncyc2[*,0] GE 30. AND loncyc2[*,0] LE 80., cntcgswio2) & help, cntcgswio2
indcgseio2 = where(latcyc2[*,0] LE 0. AND latcyc2[*,0] GE -30. AND loncyc2[*,0] GE 80. AND loncyc2[*,0] LE 140., cntcgseio2) & help, cntcgseio2
indcgsio3  = where(latcyc3[*,0] LE 0. AND latcyc3[*,0] GE -30. AND loncyc3[*,0] GE 30. AND loncyc3[*,0] LE 140., cntcgsio3) & help, cntcgsio3
indcgswio3 = where(latcyc3[*,0] LE 0. AND latcyc3[*,0] GE -30. AND loncyc3[*,0] GE 30. AND loncyc3[*,0] LE 80., cntcgswio3) & help, cntcgswio3
indcgseio3 = where(latcyc3[*,0] LE 0. AND latcyc3[*,0] GE -30. AND loncyc3[*,0] GE 80. AND loncyc3[*,0] LE 140., cntcgseio3) & help, cntcgseio3

hist_cgsio_monthclim1  = histogram(monthcyc1[indcgsio1,0],  min=1,max=12,nbins=12, /NAN) / nbyear1
hist_cgswio_monthclim1 = histogram(monthcyc1[indcgswio1,0], min=1,max=12,nbins=12, /NAN) / nbyear1
hist_cgseio_monthclim1 = histogram(monthcyc1[indcgseio1,0], min=1,max=12,nbins=12, /NAN) / nbyear1
hist_cgsio_monthclim2  = histogram(monthcyc2[indcgsio2,0],  min=1,max=12,nbins=12, /NAN) / nbyear2
hist_cgswio_monthclim2 = histogram(monthcyc2[indcgswio2,0], min=1,max=12,nbins=12, /NAN) / nbyear2
hist_cgseio_monthclim2 = histogram(monthcyc2[indcgseio2,0], min=1,max=12,nbins=12, /NAN) / nbyear2
hist_cgsio_monthclim3  = histogram(monthcyc3[indcgsio3,0],  min=1,max=12,nbins=12, /NAN) / nbyear3
hist_cgswio_monthclim3 = histogram(monthcyc3[indcgswio3,0], min=1,max=12,nbins=12, /NAN) / nbyear3
hist_cgseio_monthclim3 = histogram(monthcyc3[indcgseio3,0], min=1,max=12,nbins=12, /NAN) / nbyear3

print, expname1
print, hist_cgsio_monthclim1
print, hist_cgsio_monthclim1 / total(hist_cgsio_monthclim1, /NAN)*100.
print, total(hist_cgsio_monthclim1, /NAN)

print, expname2
print, hist_cgsio_monthclim2
print, hist_cgsio_monthclim2 / total(hist_cgsio_monthclim2, /NAN)*100.
print, total(hist_cgsio_monthclim2, /NAN)

print, expname3
print, hist_cgsio_monthclim3
print, hist_cgsio_monthclim3 / total(hist_cgsio_monthclim3, /NAN)*100.
print, total(hist_cgsio_monthclim3, /NAN)

stop

; NOMBRE
barname = ['JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC']
!y.range[0] = 0 & !y.range[1] = 8
bar_plot, hist_cgsio_monthclim1, background=255, barnames=barname, BAROFFSET=0, barspace=2, barwidth=0.6, BASERANGE=0.36, title='SIO CLIMATOLOGICAL NUMBER OF CYCLOGENESIS', xtitle='month', ytitle='number', /outline, colors=[lonarr(n_elements(hist_cgsio_monthclim1))+1*230]
bar_plot, hist_cgsio_monthclim2, background=255, BAROFFSET=4.5, barspace=2, barwidth=0.6, BASERANGE=0.36, /outline, colors=[lonarr(n_elements(hist_cgsio_monthclim1))+1*30], /over
xyouts, 8, 7.5, 'TOTAL1:  '+strtrim(round(total(hist_cgsio_monthclim1, /NAN)),2) , charsize = 2.5
xyouts, 8, 7, 'TOTAL2: '+strtrim(round(total(hist_cgsio_monthclim2, /NAN)),2) , charsize = 2.5
saveimage, path_fig + 'cg_sio_monthclim.gif'

!y.range[0] = 0 & !y.range[1] = 8
bar_plot, hist_cgswio_monthclim1, background=255, barnames=barname, BAROFFSET=0, barspace=2, barwidth=0.6, BASERANGE=0.36, title='SIO CLIMATOLOGICAL NUMBER OF CYCLOGENESIS', xtitle='month', ytitle='number', /outline, colors=[lonarr(n_elements(hist_cgswio_monthclim1))+1*230]
bar_plot, hist_cgswio_monthclim2, background=255, BAROFFSET=4.5, barspace=2, barwidth=0.6, BASERANGE=0.36, /outline, colors=[lonarr(n_elements(hist_cgswio_monthclim1))+1*30], /over
xyouts, 8, 7.5, 'TOTAL1:  '+strtrim(round(total(hist_cgswio_monthclim1, /NAN)),2) , charsize = 2.5
xyouts, 8, 7, 'TOTAL2: '+strtrim(round(total(hist_cgswio_monthclim2, /NAN)),2) , charsize = 2.5
saveimage, path_fig + 'cg_swio_monthclim.gif'

!y.range[0] = 0 & !y.range[1] = 8
bar_plot, hist_cgseio_monthclim1, background=255, barnames=barname, BAROFFSET=0, barspace=2, barwidth=0.6, BASERANGE=0.36, title='SIO CLIMATOLOGICAL NUMBER OF CYCLOGENESIS', xtitle='month', ytitle='number', /outline, colors=[lonarr(n_elements(hist_cgseio_monthclim1))+1*230]
barname = 0
bar_plot, hist_cgseio_monthclim2, background=255, BAROFFSET=4.5, barspace=2, barwidth=0.6, BASERANGE=0.36, /outline, colors=[lonarr(n_elements(hist_cgseio_monthclim1))+1*30], /over
xyouts, 8, 7.5, 'TOTAL1:  '+strtrim(round(total(hist_cgseio_monthclim1, /NAN)),2) , charsize = 2.5
xyouts, 8, 7, 'TOTAL2: '+strtrim(round(total(hist_cgseio_monthclim2, /NAN)),2) , charsize = 2.5
saveimage, path_fig + 'cg_seio_monthclim.gif'


; POURCENTAGE
barname = ['JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC']
!y.range[0] = 0 & !y.range[1] = 30
bar_plot, hist_cgsio_monthclim1/total(hist_cgsio_monthclim1, /NAN)*100., background=255, barnames=barname, BAROFFSET=0, barspace=2, barwidth=0.6, BASERANGE=0.36, title='SIO CLIMATOLOGICAL PERCENTAGE OF CYCLOGENESIS', xtitle='month', ytitle='percentage', /outline, colors=[lonarr(n_elements(hist_cgsio_monthclim1))+1*230]
bar_plot, hist_cgsio_monthclim2/total(hist_cgsio_monthclim2, /NAN)*100., background=255, BAROFFSET=4.5, barspace=2, barwidth=0.6, BASERANGE=0.36, /outline, colors=[lonarr(n_elements(hist_cgsio_monthclim2))+1*30], /over
xyouts, 8, 28, 'TOTAL1:  '+strtrim(round(total(hist_cgsio_monthclim1, /NAN)),2) , charsize = 1.5
xyouts, 8, 25, 'TOTAL2: '+strtrim(round(total(hist_cgsio_monthclim2, /NAN)),2) , charsize = 1.5
saveimage, path_fig + 'cg_percent_sio_monthclim.gif'

!y.range[0] = 0 & !y.range[1] = 30
bar_plot, hist_cgswio_monthclim1/total(hist_cgswio_monthclim1, /NAN)*100., background=255, barnames=barname, BAROFFSET=0, barspace=2, barwidth=0.6, BASERANGE=0.36, title='SWIO CLIMATOLOGICAL PERCENTAGE OF CYCLOGENESIS', xtitle='month', ytitle='percentage', /outline, colors=[lonarr(n_elements(hist_cgswio_monthclim1))+1*230]
bar_plot, hist_cgswio_monthclim2/total(hist_cgswio_monthclim2, /NAN)*100., background=255, BAROFFSET=4.5, barspace=2, barwidth=0.6, BASERANGE=0.36, /outline, colors=[lonarr(n_elements(hist_cgswio_monthclim2))+1*30], /over
xyouts, 8, 28, 'TOTAL1:  '+strtrim(round(total(hist_cgswio_monthclim1, /NAN)),2) , charsize = 1.5
xyouts, 8, 25, 'TOTAL2: '+strtrim(round(total(hist_cgswio_monthclim2, /NAN)),2) , charsize = 1.5
saveimage, path_fig + 'cg_percent_swio_monthclim.gif'

!y.range[0] = 0 & !y.range[1] = 30
bar_plot, hist_cgseio_monthclim1/total(hist_cgseio_monthclim1, /NAN)*100., background=255, barnames=barname, BAROFFSET=0, barspace=2, barwidth=0.6, BASERANGE=0.36, title='SEIO CLIMATOLOGICAL PERCENTAGE OF CYCLOGENESIS', xtitle='month', ytitle='percentage', /outline, colors=[lonarr(n_elements(hist_cgseio_monthclim1))+1*230]
bar_plot, hist_cgseio_monthclim2/total(hist_cgseio_monthclim2, /NAN)*100., background=255, BAROFFSET=4.5, barspace=2, barwidth=0.6, BASERANGE=0.36, /outline, colors=[lonarr(n_elements(hist_cgseio_monthclim2))+1*30], /over
xyouts, 8, 28, 'TOTAL1:  '+strtrim(round(total(hist_cgseio_monthclim1, /NAN)),2) , charsize = 1.5
xyouts, 8, 25, 'TOTAL2: '+strtrim(round(total(hist_cgseio_monthclim2, /NAN)),2) , charsize = 1.5
saveimage, path_fig + 'cg_percent_seio_monthclim.gif'

ENDIF


IF jyear THEN BEGIN
;-----------------------------------------------------------------------------------------
; CG INTERANNUEL
;-----------------------------------------------------------------------------------------
indcgsio1  = where(latcyc1[*,0] LE 0. AND latcyc1[*,0] GE -30. AND loncyc1[*,0] GE 30. AND loncyc1[*,0] LE 140., cntcgsio1)  & help, cntcgsio1
indcgswio1 = where(latcyc1[*,0] LE 0. AND latcyc1[*,0] GE -30. AND loncyc1[*,0] GE 30. AND loncyc1[*,0] LE 80., cntcgswio1)  & help, cntcgswio1
indcgseio1 = where(latcyc1[*,0] LE 0. AND latcyc1[*,0] GE -30. AND loncyc1[*,0] GE 80. AND loncyc1[*,0] LE 140., cntcgseio1) & help, cntcgseio1
indcgsio2  = where(latcyc2[*,0] LE 0. AND latcyc2[*,0] GE -30. AND loncyc2[*,0] GE 30. AND loncyc2[*,0] LE 140., cntcgsio2)  & help, cntcgsio2
indcgswio2 = where(latcyc2[*,0] LE 0. AND latcyc2[*,0] GE -30. AND loncyc2[*,0] GE 30. AND loncyc2[*,0] LE 80., cntcgswio2)  & help, cntcgswio2
indcgseio2 = where(latcyc2[*,0] LE 0. AND latcyc2[*,0] GE -30. AND loncyc2[*,0] GE 80. AND loncyc2[*,0] LE 140., cntcgsei2)  & help, cntcgseio2

hist_cgsio_interannual1  = histogram(yearcyc1[indcgsio1,0],  min=firsty1,max=lasty1,nbins=nbyear1, /NAN)
hist_cgswio_interannual1 = histogram(yearcyc1[indcgswio1,0], min=firsty1,max=lasty1,nbins=nbyear1, /NAN)
hist_cgseio_interannual1 = histogram(yearcyc1[indcgseio1,0], min=firsty1,max=lasty1,nbins=nbyear1, /NAN)
hist_cgsio_interannual2  = histogram(yearcyc2[indcgsio2,0],  min=firsty2,max=lasty2,nbins=nbyear2, /NAN)
hist_cgswio_interannual2 = histogram(yearcyc2[indcgswio2,0], min=firsty2,max=lasty2,nbins=nbyear2, /NAN)
hist_cgseio_interannual2 = histogram(yearcyc2[indcgseio2,0], min=firsty2,max=lasty2,nbins=nbyear2, /NAN)

barname = indgen(n_elements(hist_cgsio_interannual1)) + firsty1 & !y.range[0] = 0 & !y.range[1] = 2
bar_plot, hist_cgsio_interannual1/mean(hist_cgsio_interannual1, /NAN), background=255, barnames=barname, BAROFFSET=0, barspace=2, barwidth=0.6, BASERANGE=0.36, xtitle='year', ytitle='number / mean', /outline, colors=[lonarr(n_elements(hist_cgsio_interannual1))+1*230]
;[hist_cgsio_interannual1/mean(hist_cgsio_interannual1, /NAN),0,0,0,0,0,0,0,0,0,0]
bar_plot, hist_cgsio_interannual2/mean(hist_cgsio_interannual2, /NAN), background=255, BAROFFSET=4.5, barspace=2, barwidth=0.6, BASERANGE=0.36, title='SIO INTERANNUAL NUMBER OF CYCLOGENESIS (> 17.5 m/s): '+expname1+'-'+expname2, /outline, colors=[indgen(n_elements(hist_cgsio_interannual1))+1*30], /over
xyouts, 8, 1.9, 'MEAN1:'+strtrim(mean(hist_cgsio_interannual1, /NAN),2) , charsize = 1.5
xyouts, 8, 1.8, 'STD DEV1:'+strtrim(stddev(hist_cgsio_interannual1, /NAN),2) , charsize = 1.5
xyouts, 8, 1.7, 'MEAN2:'+strtrim(mean(hist_cgsio_interannual2, /NAN),2) , charsize = 1.5
xyouts, 8, 1.6, 'STD DEV2:'+strtrim(stddev(hist_cgsio_interannual2, /NAN),2) , charsize = 1.5
saveimage, path_fig + 'cg_norme_sio_interannual.gif'
ENDIF




IF jdays THEN BEGIN
;-----------------------------------------------------------------------------------------
; NOMBRE JOURS CYCLINIQUES CLIM MENSUELLE
;-----------------------------------------------------------------------------------------
indwind1 = where(uv10cyc1 GE 17.5, cntwind1)
indsio1  = where(latcyc1[indwind1] LE 0. AND latcyc1[indwind1] GE -30., cntsio1)
indswio1 = where(latcyc1[indwind1] LE 0. AND latcyc1[indwind1] GE -30. AND loncyc1[indwind1] LE 80., cntswio1)
indseio1 = where(latcyc1[indwind1] LE 0. AND latcyc1[indwind1] GE -30. AND loncyc1[indwind1] GE 80., cntseio1)
indwind2 = where(uv10cyc2 GE 17.5, cntwind2)
indsio2  = where(latcyc2[indwind2] LE 0. AND latcyc2[indwind2] GE -30., cntsio2)
indswio2 = where(latcyc2[indwind2] LE 0. AND latcyc2[indwind2] GE -30. AND loncyc2[indwind2] LE 80., cntswio2)
indseio2 = where(latcyc2[indwind2] LE 0. AND latcyc2[indwind2] GE -30. AND loncyc2[indwind2] GE 80., cntseio2)

hist_cycdays_sio_monthclim1  = histogram(monthcyc1[indwind1[indsio1]],  min=1,max=12,nbins=12, /NAN) / (nbyear1*4.)
hist_cycdays_swio_monthclim1 = histogram(monthcyc1[indwind1[indswio1]], min=1,max=12,nbins=12, /NAN) / (nbyear1*4.)
hist_cycdays_seio_monthclim1 = histogram(monthcyc1[indwind1[indseio1]], min=1,max=12,nbins=12, /NAN) / (nbyear1*4.)
hist_cycdays_sio_monthclim2  = histogram(monthcyc2[indwind2[indsio2]],  min=1,max=12,nbins=12, /NAN) / (nbyear2*4.)
hist_cycdays_swio_monthclim2 = histogram(monthcyc2[indwind2[indswio2]], min=1,max=12,nbins=12, /NAN) / (nbyear2*4.)
hist_cycdays_seio_monthclim2 = histogram(monthcyc2[indwind2[indseio2]], min=1,max=12,nbins=12, /NAN) / (nbyear2*4.)

hist_cycdays_sio_monthly1  = hist_2D(yearcyc1[indwind1[indsio1]], monthcyc1[indwind1[indsio1]] ,bin1=1,min1=firsty1,max1=lasty1,bin2=1,min2=1,max2=12) / 4.
hist_cycdays_swio_monthly1 = hist_2D(yearcyc1[indwind1[indswio1]],monthcyc1[indwind1[indswio1]],bin1=1,min1=firsty1,max1=lasty1,bin2=1,min2=1,max2=12) / 4.
hist_cycdays_seio_monthly1 = hist_2D(yearcyc1[indwind1[indseio1]],monthcyc1[indwind1[indseio1]],bin1=1,min1=firsty1,max1=lasty1,bin2=1,min2=1,max2=12) / 4.
hist_cycdays_sio_monthly2  = hist_2D(yearcyc2[indwind2[indsio2]], monthcyc2[indwind2[indsio2]] ,bin1=1,min1=firsty2,max1=lasty2,bin2=1,min2=1,max2=12) / 4.
hist_cycdays_swio_monthly2 = hist_2D(yearcyc2[indwind2[indswio2]],monthcyc2[indwind2[indswio2]],bin1=1,min1=firsty2,max1=lasty2,bin2=1,min2=1,max2=12) / 4.
hist_cycdays_seio_monthly2 = hist_2D(yearcyc2[indwind2[indseio2]],monthcyc2[indwind2[indseio2]],bin1=1,min1=firsty2,max1=lasty2,bin2=1,min2=1,max2=12) / 4.


; NOMBRE
barname = ['JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC'] & !y.range[0] = 0 & !y.range[1] = 80
bar_plot, hist_cycdays_sio_monthclim1, title='SIO CLIMATOLOGICAL NUMBER OF CYCLONIC DAYS (> 17.5 m/s): '+expname1+'-'+expname2, xtitle='month', ytitle='number of cyclonic days', background=255, barnames=barname, BAROFFSET=0, barspace=2, barwidth=0.6, BASERANGE=0.36, /outline, colors=[lonarr(n_elements(hist_cycdays_sio_monthclim1))+1*230]
xyouts, 6, 45, 'TOTAL1:'+strtrim(total(hist_cycdays_sio_monthclim1, /NAN),2) , charsize = 1.5
bar_plot, hist_cycdays_sio_monthclim2, background=255, BAROFFSET=4.5, barspace=2, barwidth=0.6, BASERANGE=0.36, /outline, colors=[lonarr(n_elements(hist_cycdays_sio_monthclim2))+1*30], /over
xyouts, 6, 40, 'TOTAL2:'+strtrim(total(hist_cycdays_sio_monthclim2, /NAN),2) , charsize = 1.5
saveimage, path_fig + 'cycdays_sio_monthclim.gif'

!y.range[0] = 0 & !y.range[1] = 50
bar_plot, hist_cycdays_swio_monthclim1, title='SWIO CLIMATOLOGICAL NUMBER OF CYCLONIC DAYS (> 17.5 m/s): '+expname1+'-'+expname2, xtitle='month', ytitle='number of cyclonic days', background=255, barnames=barname, BAROFFSET=0, barspace=2, barwidth=0.6, BASERANGE=0.36, /outline, colors=[lonarr(n_elements(hist_cycdays_swio_monthclim1))+1*230]
xyouts, 6, 45, 'TOTAL1:'+strtrim(total(hist_cycdays_swio_monthclim1, /NAN),2) , charsize = 1.5
bar_plot, hist_cycdays_swio_monthclim2, background=255, BAROFFSET=4.5, barspace=2, barwidth=0.6, BASERANGE=0.36, /outline, colors=[lonarr(n_elements(hist_cycdays_swio_monthclim2))+1*30], /over
xyouts, 6, 40, 'TOTAL2:'+strtrim(total(hist_cycdays_swio_monthclim2, /NAN),2) , charsize = 1.5
saveimage, path_fig + 'cycdays_swio_monthclim.gif'

!y.range[0] = 0 & !y.range[1] = 50
bar_plot, hist_cycdays_seio_monthclim1, title='SEIO CLIMATOLOGICAL NUMBER OF CYCLONIC DAYS (> 17.5 m/s): '+expname1+'-'+expname2, xtitle='month', ytitle='number of cyclonic days', background=255, barnames=barname, BAROFFSET=0, barspace=2, barwidth=0.6, BASERANGE=0.36, /outline, colors=[lonarr(n_elements(hist_cycdays_seio_monthclim1))+1*230]
xyouts, 6, 45, 'TOTAL1:'+strtrim(total(hist_cycdays_seio_monthclim1, /NAN),2) , charsize = 1.5
bar_plot, hist_cycdays_seio_monthclim2, background=255, BAROFFSET=4.5, barspace=2, barwidth=0.6, BASERANGE=0.36, /outline, colors=[lonarr(n_elements(hist_cycdays_seio_monthclim2))+1*30], /over
xyouts, 6, 40, 'TOTAL2:'+strtrim(total(hist_cycdays_seio_monthclim2, /NAN),2) , charsize = 1.5
saveimage, path_fig + 'cycdays_seio_monthclim.gif'


; POURCENTAGE
barname = ['JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC'] & !y.range[0] = 0 & !y.range[1] = 40
bar_plot, hist_cycdays_sio_monthclim1/total(hist_cycdays_sio_monthclim1,/NAN)*100., title='SIO CLIMATOLOGICAL NUMBER OF CYCLONIC DAYS (> 17.5 m/s): '+expname1+'-'+expname2, xtitle='month', ytitle='percentage of cyclonic days', background=255, barnames=barname, BAROFFSET=0, barspace=2, barwidth=0.6, BASERANGE=0.36, /outline, colors=[lonarr(n_elements(hist_cycdays_sio_monthclim1))+1*230]
xyouts, 6, 35, 'TOTAL1:'+strtrim(total(hist_cycdays_sio_monthclim1/total(hist_cycdays_sio_monthclim1,/NAN), /NAN),2) , charsize = 1.5
bar_plot, hist_cycdays_sio_monthclim2/total(hist_cycdays_sio_monthclim2,/NAN)*100., background=255, BAROFFSET=4.5, barspace=2, barwidth=0.6, BASERANGE=0.36, /outline, colors=[lonarr(n_elements(hist_cycdays_sio_monthclim2))+1*30], /over
xyouts, 6, 30, 'TOTAL2:'+strtrim(total(hist_cycdays_sio_monthclim2/total(hist_cycdays_sio_monthclim2,/NAN), /NAN),2) , charsize = 1.5
saveimage, path_fig + 'cycdays_percent_sio_monthclim.gif'

!y.range[0] = 0 & !y.range[1] = 40
bar_plot, hist_cycdays_swio_monthclim1/total(hist_cycdays_swio_monthclim1,/NAN)*100., title='SWIO CLIMATOLOGICAL NUMBER OF CYCLONIC DAYS (> 17.5 m/s): '+expname1+'-'+expname2, xtitle='month', ytitle='percentage of cyclonic days', background=255, barnames=barname, BAROFFSET=0, barspace=2, barwidth=0.6, BASERANGE=0.36, /outline, colors=[lonarr(n_elements(hist_cycdays_swio_monthclim1))+1*230]
xyouts, 6, 35, 'TOTAL1:'+strtrim(total(hist_cycdays_swio_monthclim1/total(hist_cycdays_swio_monthclim1,/NAN), /NAN),2) , charsize = 1.5
bar_plot, hist_cycdays_swio_monthclim2/total(hist_cycdays_swio_monthclim2,/NAN)*100., background=255, BAROFFSET=4.5, barspace=2, barwidth=0.6, BASERANGE=0.36, /outline, colors=[lonarr(n_elements(hist_cycdays_swio_monthclim2))+1*30], /over
xyouts, 6, 30, 'TOTAL2:'+strtrim(total(hist_cycdays_swio_monthclim2/total(hist_cycdays_swio_monthclim2,/NAN), /NAN),2) , charsize = 1.5
saveimage, path_fig + 'cycdays_percent_swio_monthclim.gif'

!y.range[0] = 0 & !y.range[1] = 40
bar_plot, hist_cycdays_seio_monthclim1/total(hist_cycdays_sio_monthclim1,/NAN)*100., title='SEIO CLIMATOLOGICAL NUMBER OF CYCLONIC DAYS (> 17.5 m/s): '+expname1+'-'+expname2, xtitle='month', ytitle='percentage of cyclonic days', background=255, barnames=barname, BAROFFSET=0, barspace=2, barwidth=0.6, BASERANGE=0.36, /outline, colors=[lonarr(n_elements(hist_cycdays_seio_monthclim1))+1*230]
xyouts, 6, 35, 'TOTAL1:'+strtrim(total(hist_cycdays_seio_monthclim1/total(hist_cycdays_seio_monthclim1,/NAN), /NAN),2) , charsize = 1.5
bar_plot, hist_cycdays_seio_monthclim2/total(hist_cycdays_seio_monthclim2,/NAN)*100., background=255, BAROFFSET=4.5, barspace=2, barwidth=0.6, BASERANGE=0.36, /outline, colors=[lonarr(n_elements(hist_cycdays_seio_monthclim2))+1*30], /over
xyouts, 6, 30, 'TOTAL2:'+strtrim(total(hist_cycdays_seio_monthclim2/total(hist_cycdays_seio_monthclim2,/NAN), /NAN),2) , charsize = 1.5
saveimage, path_fig + 'cycdays_percent_seio_monthclim.gif'


;-----------------------------------------------------------------------------------------
; NOMBRE JOURS CYCLINIQUES INTERANNUEL
;-----------------------------------------------------------------------------------------
hist_cycdays_sio_interannual1  = histogram(yearcyc1[indwind1[indsio1]],  min=firsty1,max=lasty1,nbins=nbyear1,/NAN) / 4.
hist_cycdays_swio_interannual1 = histogram(yearcyc1[indwind1[indswio1]], min=firsty1,max=lasty1,nbins=nbyear1,/NAN) / 4.
hist_cycdays_seio_interannual1 = histogram(yearcyc1[indwind1[indseio1]], min=firsty1,max=lasty1,nbins=nbyear1,/NAN) / 4.
hist_cycdays_sio_interannual2  = histogram(yearcyc2[indwind2[indsio2]],  min=firsty2,max=lasty2,nbins=nbyear2,/NAN) / 4.
hist_cycdays_swio_interannual2 = histogram(yearcyc2[indwind2[indswio2]], min=firsty2,max=lasty2,nbins=nbyear2,/NAN) / 4.
hist_cycdays_seio_interannual2 = histogram(yearcyc2[indwind2[indseio2]], min=firsty2,max=lasty2,nbins=nbyear2,/NAN) / 4.

barname = indgen(n_elements(hist_cycdays_sio_interannual1)) + firsty1 & !y.range[0] = 0 & !y.range[1] = 2
;[hist_cycdays_sio_interannual1/mean(hist_cycdays_sio_interannual1, /NAN),0,0,0,0,0,0,0,0,0,0]
bar_plot, hist_cycdays_sio_interannual1/mean(hist_cycdays_sio_interannual1, /NAN),title='SIO INTERANNUAL NUMBER OF CYCLONIC DAYS (> 17.5 m/s): '+expname1+'-'+expname2, xtitle='year', ytitle='number of cyclonic days', background=255, barnames=barname, BAROFFSET=0, barspace=2, barwidth=0.6, BASERANGE=0.36, /outline, colors=[lonarr(20)+1*230]
xyouts, 8, 1.9, 'MEAN1:'+strtrim(mean(hist_cycdays_sio_interannual1, /NAN),2) , charsize = 1.5
xyouts, 8, 1.8, 'STD DEV1:'+strtrim(stddev(hist_cycdays_sio_interannual1, /NAN),2) , charsize = 1.5
bar_plot, hist_cycdays_sio_interannual2/mean(hist_cycdays_sio_interannual2, /NAN), background=255, BAROFFSET=4.5, barspace=2, barwidth=0.6, BASERANGE=0.36, /outline, colors=[lonarr(20)+1*30], /over
xyouts, 8, 1.7, 'MEAN2:'+strtrim(mean(hist_cycdays_sio_interannual2, /NAN),2) , charsize = 1.5
xyouts, 8, 1.6, 'STD DEV2:'+strtrim(stddev(hist_cycdays_sio_interannual2, /NAN),2) , charsize = 1.5
saveimage, path_fig + 'cycdays_norme_sio_interannual.gif'

ENDIF


IF jdens THEN BEGIN
;-----------------------------------------------------------------------------------------
; DENSITY
;-----------------------------------------------------------------------------------------
density_cg1 = hist_2D(loncyc1[*,0], latcyc1[*,0], bin1=5, min1=30, max1=130, bin2=5, min2=-30, max2=30) / nbyear1
density_cg_sio1 = hist_2D(loncyc1[*,0], latcyc1[*,0], bin1=5, min1=30, max1=130, bin2=5, min2=-30, max2=0) / nbyear1
density_cg_swio1 = hist_2D(loncyc1[*,0], latcyc1[*,0], bin1=5, min1=30, max1=80, bin2=5, min2=-30, max2=0) / nbyear1
density_cg_seio1 = hist_2D(loncyc1[*,0], latcyc1[*,0], bin1=5, min1=80, max1=130, bin2=5, min2=-30, max2=0) / nbyear1
density_cg2 = hist_2D(loncyc2[*,0], latcyc2[*,0], bin1=5, min1=30, max1=130, bin2=5, min2=-30, max2=30) / nbyear2
density_cg_sio2 = hist_2D(loncyc2[*,0], latcyc2[*,0], bin1=5, min1=30, max1=130, bin2=5, min2=-30, max2=0) / nbyear2
density_cg_swio2 = hist_2D(loncyc2[*,0], latcyc2[*,0], bin1=5, min1=30, max1=80, bin2=5, min2=-30, max2=0) / nbyear2
density_cg_seio2 = hist_2D(loncyc2[*,0], latcyc2[*,0], bin1=5, min1=80, max1=130, bin2=5, min2=-30, max2=0) / nbyear2


computegrid,30,-30,5,5,21,13 
;openps, filename='density_cg_diff', /landscape
plt, density_cg1-density_cg2, min=-1, max=1, lct=77, /nocont, /realcont, /landscape, title='CYCLOGENESIS DENSITY DIFFERENCE: '+expname1+'-'+expname2, subtitle='cyclogenesis per year per 5deg bin'
saveimage, path_fig + 'density_cg_diff.gif'
;closeps

computegrid,30,-30,5,5,21,13 
plt, density_cg1/total(density_cg1)*100.-density_cg2/total(density_cg2)*100., min=-1, max=1, lct=77, /nocont, /realcont, /landscape, title='CYCLOGENESIS DENSITY DIFFERENCE: '+expname1+'-'+expname2, subtitle='cyclogenesis per year per 5deg bin'
saveimage, path_fig + 'density_percent_cg_diff.gif'

computegrid,30,-30,5,5,21,7
plt, density_cg_sio1-density_cg_sio2, min=-1, max=1, lct=77, /nocont, /realcont, /landscape, title='CYCLOGENESIS DENSITY DIFFERENCE: '+expname1+'-'+expname2, subtitle='cyclogenesis per year per 5 deg bin'
saveimage, path_fig + 'density_cg_sio_diff.gif'

computegrid,30,-30,5,5,21,7
plt, density_cg_sio1/total(density_cg_sio1)*100.-density_cg_sio2/total(density_cg_sio2)*100., min=-3, max=3, lct=77, /nocont, /realcont, /landscape, title='CYCLOGENESIS DENSITY DIFFERENCE: '+expname1+'-'+expname2, subtitle='cyclogenesis per year per 5 deg bin'
saveimage, path_fig + 'density_percent_cg_sio_diff.gif'


indwind1 = where(uv10cyc1 GE 17.5, cntwind1)
indsio1  = where(latcyc1[indwind1] LE 0. AND latcyc1[indwind1] GE -30., cntsio1)
indswio1 = where(latcyc1[indwind1] LE 0. AND latcyc1[indwind1] GE -30. AND loncyc1[indwind1] LE 80., cntswio1)
indseio1 = where(latcyc1[indwind1] LE 0. AND latcyc1[indwind1] GE -30. AND loncyc1[indwind1] GE 80., cntseio1)
indwind2 = where(uv10cyc2 GE 17.5, cntwind2)
indsio2  = where(latcyc2[indwind2] LE 0. AND latcyc2[indwind2] GE -30., cntsio2)
indswio2 = where(latcyc2[indwind2] LE 0. AND latcyc2[indwind2] GE -30. AND loncyc2[indwind2] LE 80., cntswio2)
indseio2 = where(latcyc2[indwind2] LE 0. AND latcyc2[indwind2] GE -30. AND loncyc2[indwind2] GE 80., cntseio2)

density_tc1 = hist_2D(loncyc1[indwind1], latcyc1[indwind1], bin1=2.5, min1=30, max1=130, bin2=2.5, min2=-30, max2=30) / (4. * nbyear1)
density_tc_sio1 = hist_2D(loncyc1[indwind1], latcyc1[indwind1], bin1=2.5, min1=30, max1=130, bin2=2.5, min2=-30, max2=0) / (4. * nbyear1)
density_tc_seio1 = hist_2D(loncyc1[indwind1], latcyc1[indwind1], bin1=2.5, min1=30, max1=80, bin2=2.5, min2=-30, max2=0) / (4. * nbyear1)
density_tc_swio1 = hist_2D(loncyc1[indwind1], latcyc1[indwind1], bin1=2.5, min1=80, max1=130, bin2=2.5, min2=-30, max2=0) / (4. * nbyear1)
density_tc2 = hist_2D(loncyc2[indwind2], latcyc2[indwind2], bin1=2.5, min1=30, max1=130, bin2=2.5, min2=-30, max2=30) / (4. * nbyear2)
density_tc_sio2 = hist_2D(loncyc2[indwind2], latcyc2[indwind2], bin1=2.5, min1=30, max1=130, bin2=2.5, min2=-30, max2=0) / (4. * nbyear2)
density_tc_seio2 = hist_2D(loncyc2[indwind2], latcyc2[indwind2], bin1=2.5, min1=30, max1=80, bin2=2.5, min2=-30, max2=0) / (4. * nbyear2)
density_tc_swio2 = hist_2D(loncyc2[indwind2], latcyc2[indwind2], bin1=2.5, min1=80, max1=130, bin2=2.5, min2=-30, max2=0) / (4. * nbyear2)

computegrid,30,-30,2.5,2.5,41,25             
plt, density_tc1-density_tc2, min=-2, max=2, lct=77, /nocont, /realcont, /landscape, title='CYCLONE DENSITY DIFFERENCE: '+expname1+'-'+expname2, subtitle='cycnlonic days per year per 2.5 deg bin'
saveimage, path_fig + 'density_tc_diff.gif'

computegrid,30,-30,2.5,2.5,41,25             
plt, density_tc1/total(density_tc1)*100.-density_tc2/total(density_tc2)*100., min=-0.5, max=0.5, lct=77, /nocont, /realcont, /landscape, title='CYCLONE DENSITY DIFFERENCE: '+expname1+'-'+expname2, subtitle='cycnlonic days per year per 2.5 deg bin'
saveimage, path_fig + 'density_percent_tc_diff.gif'


ENDIF

ENDIF; basin



END
