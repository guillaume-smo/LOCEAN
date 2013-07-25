PRO plot_fig_morce

reinitplt, all=all
@all_cm
maskfile = 'mask_wrf.nc'
initncdf,  maskfile


basin   = 'SI'
ucrit    = 17.5 
vorcrit  = 30.e-5
vorelax  = 30.e-5
tempcrit = 1.


expname1 = 'FORCED_SW2_BMJ'
firsty1 = 1990
lasty1 = 1999
nbyear1 = float(lasty1 - firsty1 + 1)


expname2 = 'COUPLED_SW2_BMJ'
firsty2 = 1990
lasty2 = 2009
nbyear2 = float(lasty2 - firsty2 + 1)


jwind = 0
jmois = 1
jyear = 0
jdays = 0
jdens = 1


path_fig = 'FIGS_COMP_'+ expname1 +'-'+ expname2 +'/u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2) +'/'
spawn, 'echo "creation du repertoire '+ path_fig +'"' & spawn, 'mkdir -p '+ path_fig


IF expname1 EQ 'IBTRACS' THEN BEGIN
  @/Users/gslod/WORK/IDL/TRACKER_IDL/read_ibtracs_wmo.pro
  loncyc1  = temporary(loncyc)
  latcyc1  = temporary(latcyc)
  uv10cyc1 = temporary(uv10cyc)
  mslpcyc1 = temporary(mslpcyc)
  monthcyc1 = temporary(monthcyc)
  yearcyc1  = temporary(yearcyc)
ENDIF ELSE BEGIN
  path_data1 = 'EXP_'+ expname1 +'/DATA/'
  file_in1 = 'tracker_light_RVM_NICO_MASK_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2) +'_'+strtrim(firsty1,2)+'-'+strtrim(lasty1,2)+'.idl'
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
file_in2 = 'tracker_light_RVM_NICO_MASK_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2) +'_'+strtrim(firsty2,2)+'-'+strtrim(lasty2,2)+'.idl'
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



IF basin EQ 'SI' THEN BEGIN

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

box=[30,130,-30,0]
computegrid,30,-30,5,5,21,13
page_margins=[5, 5, 1, 1] 

openps, filename='fig_a+b', /landscape
plt, density_cg1/total(density_cg1)*100.-density_cg2/total(density_cg2)*100., min=-3, max=3, lct=77, box=box, /nocont, /realcont, /landscape, title='', subtitle='cyclogenesis anomaly CTL-MORCE (%)',small=[1,2,1], /rempli
xyouts, 80, 2, '(a)' , charsize = 1.

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

hist_cgsio_monthclim1  = histogram(monthcyc1[indcgsio1,0],  min=1,max=12,nbins=12, /NAN) / nbyear1
hist_cgswio_monthclim1 = histogram(monthcyc1[indcgswio1,0], min=1,max=12,nbins=12, /NAN) / nbyear1
hist_cgseio_monthclim1 = histogram(monthcyc1[indcgseio1,0], min=1,max=12,nbins=12, /NAN) / nbyear1
hist_cgsio_monthclim2  = histogram(monthcyc2[indcgsio2,0],  min=1,max=12,nbins=12, /NAN) / nbyear2
hist_cgswio_monthclim2 = histogram(monthcyc2[indcgswio2,0], min=1,max=12,nbins=12, /NAN) / nbyear2
hist_cgseio_monthclim2 = histogram(monthcyc2[indcgseio2,0], min=1,max=12,nbins=12, /NAN) / nbyear2

;openps, filename='cg_sio_monthclim', /landscape
barname = ['JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC']
sbar_plot, hist_cgsio_monthclim1, background=255, barnames=barname, BAROFFSET=0, barspace=2, barwidth=0.6, BASERANGE=0.36, title='', xtitle='', ytitle='cyclogenesis', /outline, colors=[lonarr(n_elements(hist_cgsio_monthclim1))+1*230], yrange=[0,6], small=[1,2,2], /noerase, /rempli
sbar_plot, hist_cgsio_monthclim2, background=255, BAROFFSET=4.5, barspace=2, barwidth=0.6, BASERANGE=0.36, /outline, colors=[lonarr(n_elements(hist_cgsio_monthclim1))+1*30], position=!p.position, /over, /noerase, small=[1,2,2]
xyouts, 8, 5.0, 'CTL: '+strtrim(round(total(hist_cgsio_monthclim1, /NAN)),2) , charsize = 1., color=230
xyouts, 8, 4.5, 'MORCE: '+strtrim(round(total(hist_cgsio_monthclim2, /NAN)),2) , charsize = 1., color=30
xyouts, 5.5, 6.3, '(b)' , charsize = 1.

closeps

ENDIF

ENDIF; basin

END
