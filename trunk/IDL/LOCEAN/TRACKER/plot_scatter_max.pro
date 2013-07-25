PRO plot_scatter_max

expname = 'FORCED_SW2_KF'
period = '1990-1998'

ucrit    = 17.5
vorcrit  = 30.e-5
vorelax  = 30.e-5
tempcrit = 0.
newtcrit = 1.

restore, 'EXP_'+expname+'/DATA/maxima_UST_TREAL_RVM_NICO_MASK_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_'+ period +'.idl', /VERBOSE
pathfig = 'EXP_'+expname+'/FIGS_SCATTER/' & spawn, 'echo "creation du repertoire '+ pathfig +'"' & spawn, 'mkdir -p '+ pathfig
  
indtok = where(anomtcyc GE newtcrit)
uv10cyc  = uv10cyc[indtok]
mslpcyc  = mslpcyc[indtok]
vorcyc   = vorcyc[indtok]
anomtcyc = anomtcyc[indtok]
ustcyc   = ustcyc[indtok]

borderbin = findgen(11)*5+17.5
centerbin = findgen(10)*5+20.
ave_binuv10 = fltarr(n_elements(centerbin))+!values.f_nan
nb_binuv10  = intarr(n_elements(centerbin))
ave_binmslp = fltarr(n_elements(centerbin))+!values.f_nan
ave_binvort = fltarr(n_elements(centerbin))+!values.f_nan
ave_binanot = fltarr(n_elements(centerbin))+!values.f_nan
ave_binust  = fltarr(n_elements(centerbin))+!values.f_nan

FOR i = 0, n_elements(centerbin)-1 DO BEGIN
  indok = where(uv10cyc GE borderbin[i] AND uv10cyc LE borderbin[i+1], cntok)
  IF cntok GT 0 THEN BEGIN
    ave_binuv10[i] = mean(uv10cyc[indok], /NAN)
    ave_binmslp[i] = mean(mslpcyc[indok], /NAN)
    ave_binvort[i] = mean(vorcyc[indok], /NAN)
    ave_binanot[i] = mean(anomtcyc[indok], /NAN)
    ave_binust[i]  = mean(ustcyc[indok], /NAN)    
  ENDIF
  nb_binuv10[i] = cntok
ENDFOR
indok = where(finite(ave_binuv10) EQ 1, cntok)
coef_uvmslp = regress(ave_binuv10[indok], ave_binmslp[indok], const = cst_uvmslp)
coef_uvvort = regress(ave_binuv10[indok], ave_binvort[indok], const = cst_uvvort)
coef_uvanot = regress(ave_binuv10[indok], ave_binanot[indok], const = cst_uvanot)
coef_uvust  = regress(ave_binuv10[indok], ave_binust[indok],  const = cst_uvust)


x = [10,60] & y = x * coef_uvust[0] + cst_uvust
plot, UV10CYC, USTCYC, PSYM=3, xrange=[10,60], yrange=[0,4], xtitle='10m wind (m/s)', ytitle='u star (N.m/kg)', title='nb points: '+strtrim(n_elements(where(finite(UV10CYC) EQ 1)),2)
oplot, ave_binuv10[where(ave_binuv10 NE 0.)], ave_binust[where(ave_binuv10 NE 0.)], color=100, thick=2
oplot, x,y, color=200, thick=2
xyouts, 0.2, 0.8, 'COEF: '+strtrim(coef_uvust[0],2), charsize = 1.5, /NORMAL
xyouts, 0.2, 0.7, 'CST : '+strtrim(cst_uvust,2), charsize = 1.5, /NORMAL
saveimage, pathfig+'SCATTER_UV10M_UST_'+expname+'_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(newtcrit*10),2) +'_'+ period +'.gif'

STOP

x = [10,60] & y = x * coef_uvmslp[0] + cst_uvmslp
plot, UV10CYC, MSLPCYC, PSYM=3, xrange=[10,60], yrange=[930,1020], xtitle='10m wind (m/s)', ytitle='sea level pressure (hPa)', title='nb points: '+strtrim(n_elements(where(finite(UV10CYC) EQ 1)),2)
oplot, ave_binuv10[where(ave_binuv10 NE 0.)], ave_binmslp[where(ave_binuv10 NE 0.)], color=100, thick=2
oplot, x,y, color=200, thick=2
xyouts, 15, 940, 'COEF: '+strtrim(coef_uvmslp[0],2), charsize = 1.5
xyouts, 15, 930, 'CST : '+strtrim(cst_uvmslp,2), charsize = 1.5
saveimage, pathfig+'SCATTER_UV10M_MSLP_'+expname+'_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(newtcrit*10),2) +'_'+ period +'.gif'
print, 'UV10 = 17.5 m/s -> MSLP = ', 17.5 * coef_uvmslp[0] + cst_uvmslp
print, 'UV10 = 20.0 m/s -> MSLP = ', 20.0 * coef_uvmslp[0] + cst_uvmslp
print, 'UV10 = 22.5 m/s -> MSLP = ', 22.5 * coef_uvmslp[0] + cst_uvmslp


x = [10,60] & y = x * coef_uvanot[0] + cst_uvanot
plot, UV10CYC, ANOMTCYC, PSYM=3, xrange=[10,60], yrange=[0,6], xtitle='10m wind (m/s)', ytitle='temperature anomaly (degC)', title='nb points: '+strtrim(n_elements(where(finite(UV10CYC) EQ 1)),2)
oplot, ave_binuv10[where(ave_binuv10 NE 0.)], ave_binanot[where(ave_binuv10 NE 0.)], color=100, thick=2
oplot, x,y, color=200, thick=3
xyouts, 40, 5.5, 'COEF: '+strtrim(coef_uvanot[0],2), charsize = 1.5
xyouts, 40, 5.0, 'CST : '+strtrim(cst_uvanot,2), charsize = 1.5
saveimage, pathfig+'SCATTER_UV10M_ANOMT_'+expname+'_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(newtcrit*10),2) +'_'+ period +'.gif'
print, 'UV10 = 17.5 m/s -> ANOMT = ', 17.5 * coef_uvanot[0] + cst_uvanot
print, 'UV10 = 20.0 m/s -> ANOMT = ', 20.0 * coef_uvanot[0] + cst_uvanot
print, 'UV10 = 22.5 m/s -> ANOMT = ', 22.5 * coef_uvanot[0] + cst_uvanot


x = [10,60] & y = x * coef_uvvort[0] + cst_uvvort
plot, UV10CYC, VORCYC, PSYM=3, xrange=[10,60], yrange=[0,0.004], xtitle='10m wind (m/s)', ytitle='relative vorticity (s-1)', title='nb points: '+strtrim(n_elements(where(finite(UV10CYC) EQ 1)),2)
oplot, ave_binuv10[where(ave_binuv10 NE 0.)], ave_binvort[where(ave_binuv10 NE 0.)], color=100, thick=2
oplot, x,y, color=200, thick=2
xyouts, 40, 0.0035, 'COEF: '+strtrim(coef_uvvort[0],2), charsize = 1.5
xyouts, 40, 0.0030, 'CST : '+strtrim(cst_uvvort,2), charsize = 1.5
saveimage, pathfig+'SCATTER_UV10M_VOR800_'+expname+'_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(newtcrit*10),2) +'_'+ period +'.gif'
print, 'UV10 = 17.5 m/s -> VOR800 = ', 17.5 * coef_uvvort[0] + cst_uvvort
print, 'UV10 = 20.0 m/s -> VOR800 = ', 20.0 * coef_uvvort[0] + cst_uvvort
print, 'UV10 = 22.5 m/s -> VOR800 = ', 22.5 * coef_uvvort[0] + cst_uvvort


stop

indok = where(anomtcyc GE 0.25)

hist_anomt = histogram(anomtcyc[indok], min=0,max=5,binsize=0.25, /NAN)
barname = strtrim(indgen(20)*0.25+0,2) & !y.range[0] = 0 & !y.range[1] = 15000
bar_plot, hist_anomt[0:17], background=255, barnames=barname, title='TEMPERATURE ANOMALY DISTRIBUTION - '+expname, xtitle='anomt bins', ytitle='number of points', /outline
xyouts, 6, 10000, 'TOTAL: '+strtrim(total(hist_anomt, /NAN),2) , charsize = 1.5
saveimage, pathfig+'HIST_ANOMT_'+expname+'_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_'+ period +'.gif'

!y.range[0] = 0 & !y.range[1] = 0.5
bar_plot, float(hist_anomt[0:17])/n_elements(anomtcyc[indok]), background=255, barnames=barname, title='TEMPERATURE ANOMALY DISTRIBUTION - '+expname, xtitle='anomt bins', ytitle='percentage of points', /outline
xyouts, 6, 0.04, 'TOTAL: '+strtrim(total(hist_anomt, /NAN)/n_elements(anomtcyc[indok]),2) , charsize = 1.5
saveimage, pathfig+'HISTPERCENT_ANOMT_'+expname+'_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_'+ period +'.gif'


hist_uv10 = histogram(uv10cyc[indok], min=16.25,max=66.25,binsize=2.5, /NAN)
barname = strtrim(indgen(20)*2.5+17.5,2) & !y.range[0] = 0 & !y.range[1] = 25000
bar_plot, hist_uv10[0:14], background=255, barnames=barname, title='WIND DISTRIBUTION - '+expname, xtitle='wind bins (2.5 m/s)', ytitle='number of points', /outline
xyouts, 6, 10000, 'TOTAL: '+strtrim(total(hist_uv10, /NAN),2) , charsize = 1.5
saveimage, pathfig+'HIST_UV10M_'+expname+'_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_'+ period +'.gif'

!y.range[0] = 0 & !y.range[1] = 0.3
bar_plot, float(hist_uv10[0:14])/n_elements(uv10cyc[indok]), background=255, barnames=barname, title='WIND DISTRIBUTION - '+expname, xtitle='wind bins', ytitle='percentage of points', /outline
xyouts, 6, 0.25, 'TOTAL: '+strtrim(total(hist_uv10, /NAN)/n_elements(uv10cyc[indok]),2) , charsize = 1.5
saveimage, pathfig+'HISTPERCENT_UV10M_'+expname+'_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_'+ period +'.gif'


hist_mslp = histogram(mslpcyc[indok], min=917.5,  max=1007.5, binsize=5, /NAN)
barname = strtrim(indgen(18)*5+920,2) & !y.range[0] = 0 & !y.range[1] = 15000
bar_plot, hist_mslp[0:17], background=255, barnames=barname, title='MSLP DISTRIBUTION - '+expname, xtitle='mslp bins (5 hPa)', ytitle='number of points', /outline
xyouts, 6, 10000, 'TOTAL: '+strtrim(total(hist_mslp, /NAN),2) , charsize = 1.5
saveimage, pathfig+'HIST_MSLP_'+expname+'_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_'+ period +'.gif'

!y.range[0] = 0 & !y.range[1] = 0.3
bar_plot, float(hist_mslp[0:17])/n_elements(mslpcyc[indok]), background=255, barnames=barname, title='MSLP DISTRIBUTION - '+expname, xtitle='mslp bins (2.5 m/s)', ytitle='percentage of points', /outline
xyouts, 6, 0.25, 'TOTAL: '+strtrim(total(hist_mslp[0:17], /NAN)/n_elements(mslpcyc[indok]),2) , charsize = 1.5
saveimage, pathfig+'HISTPERCENT_MSLP_'+expname+'_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_'+ period +'.gif'


hist_vor = histogram(vorcyc[indok], min=0.0003,max=0.003,binsize=0.0001, /NAN)
barname = strtrim(indgen(20)*0.0003+0.0001,2) & !y.range[0] = 0 & !y.range[1] = 20000
bar_plot, hist_vor, background=255, barnames=barname, title='VORTICITY DISTRIBUTION - '+expname, xtitle='vorticity bins', ytitle='number of points', /outline
xyouts, 6, 10000, 'TOTAL: '+strtrim(total(hist_vor, /NAN),2) , charsize = 1.5
saveimage, pathfig+'HIST_VOR_'+expname+'_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_'+ period +'.gif'

!y.range[0] = 0 & !y.range[1] = 0.3
bar_plot, float(hist_vor)/n_elements(vorcyc[indok]), background=255, barnames=barname, title='VORTICITY DISTRIBUTION - '+expname, xtitle='vorticity bins', ytitle='percentage of points', /outline
xyouts, 6, 0.25, 'TOTAL: '+strtrim(total(hist_vor, /NAN)/n_elements(vorcyc[indok]),2) , charsize = 1.5
saveimage, pathfig+'HISTPERCENT_VOR_'+expname+'_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_'+ period +'.gif'

stop
END
