PRO plot_scatter_v2


; parametres
explist  = ['IBTRACS','COUPLED_SW2_KF','COUPLED_SW2_BMJ']
basin    = 'SIO'
datebeg  = '19900101'
ucrit    = 17.5
vorcrit  = 30.e-5
vorelax  = 30.e-5
templist = [1]
temptype = 'treal'
write_ps = 1
period  = '1990-2009' & dateend = '20100101' & nbyear = 20


; definition bin
binc      = [17.5,33,43,50,59,70]
binc_nb   = n_elements(binc)-1
binw_size = 5
binw_deb  = 20
binw_fin  = 60
binw_unit = '(m/s)'
binw_nb   = (binw_fin-binw_deb) / binw_size
binp_size = 20
binp_deb  = 910
binp_fin  = 1010
binp_unit = '(hPa)'
binp_nb   = (binp_fin-binp_deb) / binp_size
binw_mean = fltarr(4,binw_nb)
binw_per  = fltarr(4,2,binw_nb)
binp_mean = fltarr(4,binw_nb)
binp_per  = fltarr(4,2,binw_nb)


icpt = 0
; lecture best-track
FOR iexp = 0, n_elements(explist)-1 DO BEGIN

  expname = explist[iexp] & help, expname

  FOR item = 0, n_elements(templist)-1 DO BEGIN
    tempcrit = templist[item]

    IF expname EQ 'IBTRACS' THEN BEGIN

      @read_ibtracs.pro

    ENDIF ELSE BEGIN

;        IF exp EQ 'KF' AND param EQ 'FORCED' THEN period  = '1990-1998' ELSE period  = '1990-2009' & help, period
;        IF exp EQ 'KF' AND param EQ 'FORCED' THEN dateend = '19990101'  ELSE dateend = '20100101' & help, dateend
;        IF exp EQ 'KF' AND param EQ 'FORCED' THEN nbyear = 9 ELSE nbyear = 20
        pathin = '/net/adonis/usr/adonis/varclim/gslod/IDL/TRACKER/EXP_'+expname+'/DATA/' & help, pathin
;        filein = 'tracker_light_'+temptype+'_RVM_NICO_MASK_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2) +'_'+period+'.idl'
;        filein = 'tracker_light_'+temptype+'_new_cst_radius_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2) +'_'+period+'.idl'
;        restore, pathin + filein, /VERBOSE
        restore, pathin + 'd0_TRACKS_'+ expname +'_IO_'+period+'.dat', /VERBOSE
        restore, pathin + 'd1_TRACKS_'+ expname +'_IO_'+period+'.dat', /VERBOSE
        datecycn = d1_time_trueyr
        juldcycn = d1_time_jul
        loncycn  = d1_lon
        latcycn  = d1_lat
        mslpcycn = d1_pres
        uv10cycn = d1_max_wnd
        monthcycn = long(datecycn/100.) - long(datecycn/10000.)*100.
        yearcycn  = long(datecycn/10000.)
        help, loncycn, latcycn, mslpcycn, uv10cycn, datecycn, juldcycn

    ENDELSE
    IF basin EQ 'SIO' THEN BEGIN
      indcg = where(latcycn[*,0] LE 0. AND latcycn[*,0] GE -30. AND loncycn[*,0] GE 30. AND loncycn[*,0] LE 140., cntcg)
      indtc = where(latcycn[*,*] LE 0. AND latcycn[*,*] GE -30. AND loncycn[*,*] GE 30. AND loncycn[*,*] LE 140., cnttc)
    ENDIF
    IF basin EQ 'NIO' THEN BEGIN
      indcg = where(latcycn[*,0] LE 25. AND latcycn[*,0] GE 0. AND loncycn[*,0] GE 30. AND loncycn[*,0] LE 100., cntcg)
      indtc = where(latcycn[*,*] LE 25. AND latcycn[*,*] GE 0. AND loncycn[*,*] GE 30. AND loncycn[*,*] LE 100., cnttc)
    ENDIF
    IF basin EQ 'IO' THEN BEGIN
      indcg = where((latcycn[*,0] LE 0.  AND latcycn[*,0] GE -30. AND loncycn[*,0] GE 30. AND loncycn[*,0] LE 140.) OR $
                    (latcycn[*,0] LE 25. AND latcycn[*,0] GE 0.   AND loncycn[*,0] GE 30. AND loncycn[*,0] LE 100.), cntcg)
      indtc = where((latcycn[*,*] LE 0.  AND latcycn[*,*] GE -30. AND loncycn[*,*] GE 30. AND loncycn[*,*] LE 140.) OR $
                    (latcycn[*,*] LE 25. AND latcycn[*,*] GE 0.   AND loncycn[*,*] GE 30. AND loncycn[*,*] LE 100.), cnttc)
    ENDIF
    help, indcg, indtc

    FOR i = 0, binw_nb-1 DO BEGIN
      indok = where(uv10cycn[indtc] GE binw_deb+i*binw_size AND uv10cycn[indtc] LE binw_deb+(i+1)*binw_size, cntok)
      print, ' bin min:', binw_deb+i*binw_size, ' bin max:', binw_deb+(i+1)*binw_size, ' nb pts:', cntok

      IF cntok GT 0 THEN BEGIN
        binw_mean[icpt,i] = mean(uv10cycn[indtc[indok]], /NAN)
        binp_mean[icpt,i] = mean(mslpcycn[indtc[indok]], /NAN)
        binw_per[icpt,*,i] = percentile(uv10cycn[indtc[indok]],0.25)
        binp_per[icpt,*,i] = percentile(mslpcycn[indtc[indok]],0.25)
      ENDIF
    ENDFOR
     help, icpt
     icpt = icpt + 1 

     print, 'EXP:',expname, ' - BASIN:', basin
     print, ''
     print, 'max wind (m/s) =', max(uv10cycn[indtc],ind_maxwind,/nan)
     print, 'mslp correspondante (hPa) =', mslpcycn[indtc[ind_maxwind]]
     print, 'localisation:', loncycn[indtc[ind_maxwind]],latcycn[indtc[ind_maxwind]]
     print, datecycn[indtc[ind_maxwind]], format='(F11.2)'
     print, ''
     print, 'mslp (hPa) =', min(mslpcycn[indtc],ind_minmslp,/nan)
     print, 'max wind correspondant (m/s) =', uv10cycn[indtc[ind_minmslp]]
     print, 'localisation:',loncycn[indtc[ind_minmslp]],latcycn[indtc[ind_minmslp]]
     print, datecycn[indtc[ind_minmslp]], format='(F11.2)'
     STOP

  ENDFOR; exp
ENDFOR; temp


; PLOTS
IF write_ps THEN openps, filename='WIND-MSLP_OBS+KF+BMJ'
IF write_ps THEN thc = 4 ELSE thc = 2

lct, 33
splot, binw_mean[0,where(binw_mean[0,*] NE 0.)], binp_mean[0,where(binp_mean[0,*] NE 0.)], color=0, thick=thc, $
xrange=[20,60], yrange=[910,1000], xtitle='10m wind (m/s)', ytitle='sea level pressure (hPa)', xstyle=1, ystyle=1
oplot, binw_mean[1,where(binw_mean[1,*] NE 0.)], binp_mean[1,where(binp_mean[1,*] NE 0.)], color=50, thick=thc
oplot, binw_mean[2,where(binw_mean[2,*] NE 0.)], binp_mean[2,where(binp_mean[2,*] NE 0.)], color=225, thick=thc
oplot, binw_per[0,0,where(binw_per[0,0,*] NE 0.)], binp_per[0,0,where(binp_per[0,0,*] NE 0.)], color=0, line=2
oplot, binw_per[0,1,where(binw_per[0,1,*] NE 0.)], binp_per[0,1,where(binp_per[0,1,*] NE 0.)], color=0, line=2
oplot, binw_per[1,0,where(binw_per[1,0,*] NE 0.)], binp_per[1,0,where(binp_per[1,0,*] NE 0.)], color=50, line=2
oplot, binw_per[1,1,where(binw_per[1,1,*] NE 0.)], binp_per[1,1,where(binp_per[1,1,*] NE 0.)], color=50, line=2
oplot, binw_per[2,0,where(binw_per[2,0,*] NE 0.)], binp_per[2,0,where(binp_per[2,0,*] NE 0.)], color=225, line=2
oplot, binw_per[2,1,where(binw_per[2,1,*] NE 0.)], binp_per[2,1,where(binp_per[2,1,*] NE 0.)], color=225, line=2
xyouts, 0.15, 0.200, 'IBTRACS', color=0, charsize=2, /normal
xyouts, 0.15, 0.175, 'CPL-KF', color=50, charsize=2, /normal
xyouts, 0.15, 0.150, 'CPL-BMJ', color=225, charsize=2, /normal
;saveimage, 'WIND_vs_MSPL_'+exp+'.gif'

IF write_ps THEN closeps & STOP

END
