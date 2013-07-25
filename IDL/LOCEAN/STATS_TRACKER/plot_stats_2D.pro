PRO plot_stats_2D


; parameters
explist  = ['IBTRACS','COUPLED_SW2_KF','COUPLED_SW2_BMJ']
basin    = 'IO' ; IO / SIO / NIO
bin_size = 5.   ; en degres

; tracker
ucrit    = 17.5
vorcrit  = 30.e-5
vorelax  = 30.e-5
templist = [1]

; temps
period   = '1990-2009'
datebeg  = '19900101'
dateend  = '20100101'
nbyear   = 20

; plots
plot_cg  = 1 ; plot densite cg
plot_tc  = 1 ; plot densite tc
plot_sp  = 0 ; plot spaghetti
write_ps = 0
c_smooth = 0 ; coef smoothing
plt_path = '/usr/home/gslod/IDL/STATS_TRACKER/FIGS_2D_SMOOTH/'

;--------------------------------------------------------------------------------------


; lecture best-track
FOR iexp = 0, n_elements(explist)-1 DO BEGIN
  FOR item = 0, n_elements(templist)-1 DO BEGIN

      expname = explist[iexp] & help, expname
      tempcrit = templist[item]

      IF expname EQ 'IBTRACS' THEN BEGIN
        @read_ibtracs.pro
      ENDIF ELSE BEGIN

      IF expname EQ 'CPL_TDK_V33'  THEN BEGIN & nbyear=7 & period='1990-1996' & dateend='19960101' & ENDIF
      IF expname EQ 'CPL_NSAS_V33' THEN BEGIN & nbyear=7 & period='1990-1996' & dateend='19960101' & ENDIF

      pathin = '/net/adonis/usr/adonis/varclim/gslod/IDL/TRACKER/EXP_'+expname+'/DATA/' & help, pathin

      IF expname EQ 'COUPLED_SW2_BMJ' THEN $
;      filein = 'tracker_light_TREAL_RVM_NICO_MASK_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2) +'_'+period+'.idl'
       filein = 'tracker_light_tpot_new_cst_radius_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2) +'_'+period+'.idl'

; OLD TESTS
;      filein = 'tracker_light_treal_new_cst_radius_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2) +'_'+period+'.idl'
;      filein = 'tracker_light_tpot_new_cst_radius_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2) +'_'+period+'.idl'
;      filein = 'tracker_light_TPOT_RVM_NICO_MASK_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2) +'_'+period+'.idl'

      IF expname EQ 'COUPLED_SW2_KF' THEN $
;      filein = 'tracker_light_TREAL_RVM_NICO_MASK_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2) +'_'+period+'.idl'
      filein = 'tracker_light_TREAL_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2) +'_'+period+'.idl'

; OLD TESTS 
;      filein = 'tracker_light_TREAL_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2) +'_'+period+'.idl'
;      filein = 'tracker_light_tpot_new_cst_radius_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2) +'_'+period+'.idl'
;      filein = 'tracker_light_TPOT_RVM_NICO_MASK_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2) +'_'+period+'.idl'

      IF expname EQ 'FORCED_SW2_KF' THEN $
      filein = 'tracker_light_treal_nico_radius_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2) +'_'+period+'.idl'

; OLD TESTS
;      filein = 'tracker_light_treal_new_cst_radius_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2) +'_'+period+'.idl'

      IF expname EQ 'CPL_TDK_V33' THEN $
      filein = 'tracker_light_treal_new_cst_radius_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2) +'_'+period+'.idl'

      IF expname EQ 'CPL_NSAS_V33' THEN $
      filein = 'tracker_light_treal_new_cst_radius_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2) +'_'+period+'.idl'
      help, filein

        restore, pathin + filein, /VERBOSE
        monthcycn = long(datecycn/100.) - long(datecycn/10000.)*100.
        yearcycn  = long(datecycn/10000.)
        help, loncycn, latcycn, mslpcycn, uv10cycn, datecycn, juldcycn
      ENDELSE

      ; calcul
      indcgsio = where(latcycn[*,0] LT 0. AND latcycn[*,0] GE -30. AND loncycn[*,0] GE 30. AND loncycn[*,0] LE 130., cntcgsio)
      indcgnio = where(latcycn[*,0] GT 0. AND latcycn[*,0] LE 25.  AND loncycn[*,0] GE 60. AND loncycn[*,0] LE 100., cntcgnio)
      indcg = [indcgsio,indcgnio] & cntcg = n_elements(indcg)

;      IF basin EQ 'IO'  THEN density_cg = hist_2D(loncycn[indcg,0], latcycn[indcg,0], bin1=bin_size,min1=30,max1=130,bin2=bin_size,min2=-30,max2=30) / float(nbyear)
;      IF basin EQ 'IO'  THEN density_tc = hist_2D(loncycn[indcg,*], latcycn[indcg,*], bin1=bin_size,min1=30,max1=130,bin2=bin_size,min2=-30,max2=30) / float(nbyear)
;      IF basin EQ 'SIO' THEN density_cg = hist_2D(loncycn[indcgsio,0], latcycn[indcgsio,0], bin1=bin_size, min1=30, max1=130, bin2=5, min2=-30, max2=0) / float(nbyear)
;      IF basin EQ 'SIO' THEN density_tc = hist_2D(loncycn[indcgsio,*], latcycn[indcgsio,*], bin1=bin_size, min1=30, max1=130, bin2=5, min2=-30, max2=0) / float(nbyear)
;      IF basin EQ 'NIO' THEN density_cg = hist_2D(loncycn[indcgnio,0], latcycn[indcgnio,0], bin1=bin_size, min1=60, max1=100, bin2=5, min2=0, max2=25) / float(nbyear)
;      IF basin EQ 'NIO' THEN density_tc = hist_2D(loncycn[indcgnio,*], latcycn[indcgnio,*], bin1=bin_size, min1=60, max1=100, bin2=5, min2=0, max2=25) / float(nbyear)

      density_cg = hist_2D(loncycn[indcg,0], latcycn[indcg,0], bin1=bin_size,min1=30,max1=130,bin2=bin_size,min2=-40,max2=30) / float(nbyear)
      density_tc = hist_2D(loncycn[indcg,*], latcycn[indcg,*], bin1=bin_size,min1=30,max1=130,bin2=bin_size,min2=-40,max2=30) / float(nbyear)
      help, density_cg, density_tc
      print, 'VERIF: ', total(density_cg[*,*]),'=',cntcg/float(nbyear),' (',cntcgsio/float(nbyear),'+',cntcgnio/float(nbyear),')' & STOP


      ; SAVE RESULTS
      IF iexp EQ 0 THEN alldensity_cg = fltarr(n_elements(explist),n_elements(density_cg[*,0]),n_elements(density_cg[0,*]))
      IF iexp EQ 0 THEN alldensity_tc = fltarr(n_elements(explist),n_elements(density_tc[*,0]),n_elements(density_tc[0,*]))
      alldensity_cg[iexp,*,*] = density_cg
      alldensity_tc[iexp,*,*] = density_tc




      ; PLOT 2D
      computegrid,30,-40,bin_size,bin_size,(130-30)/bin_size+1,(30+40)/bin_size+1
      IF basin EQ 'IO'  THEN domdef, [30,130,-40,30]
      IF basin EQ 'SIO' THEN domdef, [30,130,-40, 0]
      IF basin EQ 'NIO' THEN domdef, [30,130,  0,25]

      SET_PLOT, 'X'
      DEVICE, decomposed=0, retain=0
      lct, 33


      ; CG DENSITY
      IF plot_cg THEN BEGIN
        IF write_ps THEN openps, filename=plt_path+'CG_2D_'+expname+'.ps'
        plt, smooth(density_cg,c_smooth), 0, 1., /realcont, /nocont, lct=51, xtitle='', ytitle='', subtitle='',title='CG DENSITY '+expname
        IF write_ps THEN closeps ELSE stop
        IF write_ps THEN openps, filename=plt_path+'CG_NORM_2D_'+expname+'.ps'
        plt, smooth(density_cg,c_smooth)/total(smooth(density_cg,c_smooth))*100., 0, 3., /realcont, /nocont, lct=51, xtitle='', ytitle='', subtitle='',title='NORMALIZED CG DENSITY (%) '+expname
        IF write_ps THEN closeps ELSE stop
      ENDIF


      ; TC DENSITY
      IF plot_tc THEN BEGIN
        IF write_ps THEN openps, filename=plt_path+'TC_DENSITY_2D_'+expname+'.ps'
        plt, smooth(density_tc,c_smooth), 0, 30., /realcont, /nocont, lct=51, xtitle='', ytitle='', subtitle='',title='TC DENSITY '+expname
        IF write_ps THEN closeps ELSE stop
        IF write_ps THEN openps, filename=plt_path+'TC_NORM_2D_'+expname+'.ps'
        plt, smooth(density_tc,c_smooth)/total(smooth(density_tc,c_smooth))*100., 0, 3., /realcont, /nocont, lct=51, xtitle='', ytitle='', subtitle='',title='NORMALIZED TC DENSITY (%) '+expname
        IF write_ps THEN closeps ELSE stop
      ENDIF


      ; CG+TC DENSITY
      IF plot_cg AND plot_tc THEN BEGIN
        IF write_ps THEN openps, filename=plt_path+'CG+TC_NORM_2D_'+expname+'.ps'
        plt, smooth(density_cg,c_smooth)/total(smooth(density_cg,c_smooth))*100., 0, 3., /realcont, lct=51, $
        contour=smooth(density_tc,c_smooth)/total(smooth(density_tc,c_smooth))*100., contmin=0, contmax=3, contint=0.25, $
        c_thick=[4,1,4,1,4,1,4,1,4,1,4,1,4], c_label=[1,0,1,0,1,0,1,0,1,0,1,0,1], $
        xtitle='', ytitle='', subtitle='',title='NORMALIZED CG+ TC DENSITY (%) '+expname
        IF write_ps THEN closeps ELSE stop
      ENDIF


      ; SPAGHETTO
      IF plot_sp THEN BEGIN
      IF write_ps THEN openps, filename=plt_path+'SPAGHETTO_2D_'+expname+'.ps'
      min_wind = 0.
      max_wind = 50.
    
      plt, density_cg, /nodata, /realcont, min=min_wind, max=max_wind, title='EXAMPLE OF TC TRACKS & INTENSITY '+expname, subtitle='', xtitle='', ytitle=''

      iok = indcgsio & j = 0
      FOR i = 0, n_elements(uv10cycn[iok,0])-1 DO BEGIN
        IF expname NE 'IBTRACS' AND i GT 50 THEN BREAK
        IF expname EQ 'IBTRACS' AND i GT 100 THEN BREAK

        indok = where(finite(uv10cycn[iok[i],*]) EQ 1, cntok)
        IF cntok GE 8 THEN BEGIN
          IF expname NE 'IBTRACS' THEN BEGIN
            lons  = smooth(reform(loncycn[iok[i],indok]),7,/nan); & help, loncycn[iok[i],indok], lons
            lats  = smooth(reform(latcycn[iok[i],indok]),7,/nan); & help, latcycn[iok[i],indok], lats
            uv10s = smooth(reform(uv10cycn[iok[i],indok]),7,/nan)
        ENDIF ELSE BEGIN
          lons = loncycn[iok[i],indok]
          lats = latcycn[iok[i],indok]
          uv10s = uv10cycn[iok[i],indok]
        ENDELSE

;  domdef, [floor(min(loncycn[iok[i],*], /nan)),ceil(max(loncycn[iok[i],*], /nan)),floor(min(latcycn[iok[i],*], /nan)),ceil(max(latcycn[iok[i],*], /nan))]
;  plt, glamt, /nodata, /realcont, min=min_wind, max=max_wind

        FOR j = 0, cntok-2 DO BEGIN
;      oplot, [loncycn[iok[i],indok[j]],loncycn[iok[i],indok[j+1]]] , [latcycn[iok[i],indok[j]],latcycn[iok[i],indok[j+1]]], $
;      color= min([(uv10cycn[iok[i],indok[j]]-min_wind)/(max_wind-min_wind)*255,255]), thick = 2
          oplot, [lons[indok[j]],lons[indok[j+1]]] , [lats[indok[j]],lats[indok[j+1]]], $
          color= min([max([uv10s[indok[j]]-min_wind,0])*255/max_wind,255]), thick=4
        ENDFOR
;    xyouts, loncycn[i, indok[0]], latcycn[i, indok[0]], strtrim(long(datecycn[i,indok[0]]), 2), charsize = 1.5
;    xyouts, loncycn[i, indok[cntok-1]], latcycn[i, indok[cntok-1]], strtrim(long(datecycn[i,indok[cntok-1]]), 2), charsize = 1.5
;    saveimage, path_fig + 'traj_light_RVM_NICO_MASK_'+ strtrim(long(datecycn[i,indok[0]]), 2) +'_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2) +'.gif'
      ENDIF
    ENDFOR

      iok = indcgnio & j = 0
      FOR i = 0, n_elements(uv10cycn[iok,0])-1 DO BEGIN
        IF expname NE 'IBTRACS' AND i GT 30 THEN BREAK
        IF expname EQ 'IBTRACS' AND i GT 60 THEN BREAK

        indok = where(finite(uv10cycn[iok[i],*]) EQ 1, cntok)
        IF cntok GE 8 THEN BEGIN
          IF expname NE 'IBTRACS' THEN BEGIN
            lons  = smooth(reform(loncycn[iok[i],indok]),7,/nan); & help, loncycn[iok[i],indok], lons
            lats  = smooth(reform(latcycn[iok[i],indok]),7,/nan); & help, latcycn[iok[i],indok], lats
            uv10s = smooth(reform(uv10cycn[iok[i],indok]),7,/nan)
        ENDIF ELSE BEGIN
          lons = loncycn[iok[i],indok]
          lats = latcycn[iok[i],indok]
          uv10s = uv10cycn[iok[i],indok]
        ENDELSE

;  domdef, [floor(min(loncycn[iok[i],*], /nan)),ceil(max(loncycn[iok[i],*], /nan)),floor(min(latcycn[iok[i],*], /nan)),ceil(max(latcycn[iok[i],*], /nan))]
;  plt, glamt, /nodata, /realcont, min=min_wind, max=max_wind

        FOR j = 0, cntok-2 DO BEGIN
;      oplot, [loncycn[iok[i],indok[j]],loncycn[iok[i],indok[j+1]]] , [latcycn[iok[i],indok[j]],latcycn[iok[i],indok[j+1]]], $
;      color= min([(uv10cycn[iok[i],indok[j]]-min_wind)/(max_wind-min_wind)*255,255]), thick = 2
          oplot, [lons[indok[j]],lons[indok[j+1]]] , [lats[indok[j]],lats[indok[j+1]]], $
          color= min([max([uv10s[indok[j]]-min_wind,0])*255/max_wind,255]), thick=4
        ENDFOR
;    xyouts, loncycn[i, indok[0]], latcycn[i, indok[0]], strtrim(long(datecycn[i,indok[0]]), 2), charsize = 1.5
;    xyouts, loncycn[i, indok[cntok-1]], latcycn[i, indok[cntok-1]], strtrim(long(datecycn[i,indok[cntok-1]]), 2), charsize = 1.5
;    saveimage, path_fig + 'traj_light_RVM_NICO_MASK_'+ strtrim(long(datecycn[i,indok[0]]), 2) +'_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2) +'.gif'
      ENDIF
    ENDFOR
    IF write_ps THEN closeps ELSE stop

    ENDIF; plot_sp

ENDFOR; exp
ENDFOR; temp
STOP



; PLOTS DIFFERENCES CG/TC DENSITY
alldensity_cg0 = smooth(reform(alldensity_cg[0,*,*]),3)/total(smooth(reform(alldensity_cg[0,*,*]),3))*100.
alldensity_cg1 = smooth(reform(alldensity_cg[1,*,*]),3)/total(smooth(reform(alldensity_cg[1,*,*]),3))*100.
alldensity_cg2 = smooth(reform(alldensity_cg[2,*,*]),3)/total(smooth(reform(alldensity_cg[2,*,*]),3))*100.
plt, alldensity_cg1-alldensity_cg0, -2., 2., /realcont, /nocont, lct=64, xtitle='', ytitle='', subtitle='',title='NORMALIZED CG DENSITY DIFFERENCE (%)' & stop
plt, alldensity_cg2-alldensity_cg0, -2., 2., /realcont, /nocont, lct=64, xtitle='', ytitle='', subtitle='',title='NORMALIZED CG DENSITY DIFFERENCE (%)' & stop
plt, alldensity_cg2-alldensity_cg1, -2., 2., /realcont, /nocont, lct=64, xtitle='', ytitle='', subtitle='',title='NORMALIZED CG DENSITY DIFFERENCE (%)' & stop

alldensity_tc0 = smooth(reform(alldensity_tc[0,*,*]),3)/total(smooth(reform(alldensity_tc[0,*,*]),3))*100.
alldensity_tc1 = smooth(reform(alldensity_tc[1,*,*]),3)/total(smooth(reform(alldensity_tc[1,*,*]),3))*100.
alldensity_tc2 = smooth(reform(alldensity_tc[2,*,*]),3)/total(smooth(reform(alldensity_tc[2,*,*]),3))*100.
plt, alldensity_tc1-alldensity_tc0, -2., 2., /realcont, /nocont, lct=64, xtitle='', ytitle='', subtitle='',title='NORMALIZED TC DENSITY DIFFERENCE (%)' & stop
plt, alldensity_tc2-alldensity_tc0, -2., 2., /realcont, /nocont, lct=64, xtitle='', ytitle='', subtitle='',title='NORMALIZED TC DENSITY DIFFERENCE (%)' & stop
plt, alldensity_tc2-alldensity_tc1, -2., 2., /realcont, /nocont, lct=64, xtitle='', ytitle='', subtitle='',title='NORMALIZED TC DENSITY DIFFERENCE (%)' & stop

;      plt, density_cg, 0, 1, /realcont, /nocont, subtitle='', title='CG DENSITY '+expname, xtitle='', ytitle=''
;      print, iexp, ipar
;      IF iexp+ipar EQ 0 THEN plt, density_cg, 0, 1.5, /realcont, /nocont, subtitle=' ', title=' ', xtitle=' ', ytitle=' ', small=[1,3,1], format='(F3.1)', lct=66, win=0, charsize=1.2, cb_charsize=1.2
;      wset, 0
;      IF iexp+ipar GE 1 THEN plt, density_cg, 0, 1.5, /realcont, /nocont, subtitle=' ', title=' ', xtitle=' ', ytitle=' ', small=[1,3,ipar+iexp+1], /noerase, lct=66, format='(F3.1)', charsize=1.2, cb_charsize=1.2

;      IF iexp EQ 0 AND ipar EQ 0 THEN plt, density_cg/total(density_cg)*100., 0, 5, /realcont, /nocont, subtitle=' ', title=' ', xtitle=' ', ytitle=' ', small=[1,3,1], lct=66, format='(F3.1)', win=1, charsize=1.2, cb_charsize=1.2
;      wset, 1
;      IF iexp EQ 1 THEN plt, density_cg/total(density_cg)*100., 0, 5, /realcont, /nocont, subtitle=' ', title=' ', xtitle=' ', ytitle=' ', small=[1,3,ipar+2], /noerase, lct=66, format='(F3.1)', charsize=1.2, cb_charsize=1.2
;      print, total(density_cg[*,*]) / total(density_cg) * 100.

;      IF iexp EQ 1 THEN plt, density_cg/total(density_cg), min=0, /realcont, /nocont, subtitle=' ', title=' ', xtitle=' ', ytitle=' ', /rempli, small=[1,3,ipar+1], /noerase, lct=66, format='(F3.1)', win=2
;      IF iexp EQ 1 AND ipar EQ 0 THEN BEGIN
;      tmp=density_cg
;      plt, density_cg/total(density_cg), min=0, /realcont, /nocont, subtitle=' ', title=' ', xtitle=' ', ytitle=' ', /rempli, small=[1,3,1], lct=66, format='(F3.1)', /noerase, win=2
;      ENDIF
;      IF iexp EQ 1 AND ipar EQ 1 THEN BEGIN
;      plt, density_cg/total(density_cg), min=0, /realcont, /nocont, subtitle=' ', title=' ', xtitle=' ', ytitle=' ', /rempli, small=[1,3,2], /noerase, lct=66, format='(F3.1)'
;      plt, tmp/total(tmp)-density_cg/total(density_cg), min=-0.5, max=0.5, /realcont, /nocont, subtitle=' ', title=' ', xtitle=' ', ytitle=' ', /rempli, small=[1,3,3], /noerase, lct=66, format='(F4.1)'
;      ENDIF

;      IF iexp EQ 1 THEN plt, density_cg, min=0, /realcont, /nocont, subtitle=' ', title=' ', xtitle=' ', ytitle=' ', /rempli, small=[1,3,ipar+1], /noerase, lct=66, format='(F3.1)'
;      IF iexp EQ 1 AND ipar EQ 0 THEN BEGIN
;      tmp=density_cg
;      plt, density_cg, min=0, /realcont, /nocont, subtitle=' ', title=' ', xtitle=' ', ytitle=' ', /rempli, small=[1,3,1], lct=66, format='(F3.1)', /noerase
;      ENDIF
;      IF iexp EQ 1 AND ipar EQ 1 THEN BEGIN
;      plt, density_cg, min=0, /realcont, /nocont, subtitle=' ', title=' ', xtitle=' ', ytitle=' ', /rempli, small=[1,3,2], /noerase, lct=66, format='(F3.1)'
;      plt, tmp-density_cg, /realcont, /nocont, subtitle=' ', title=' ', xtitle=' ', ytitle=' ', /rempli, small=[1,3,3], /noerase, lct=66, format='(F4.1)'
;      ENDIF


;      IF fig EQ 'all' THEN BEGIN
;      wset, 0
;      IF iexp EQ 0 AND ipar EQ 0 THEN plt, density, min=0, max=20, /realcont, /nocont, subtitle=' ', title=' ', xtitle=' ', ytitle=' ', /rempli, small=[1,3,1], format='(F5.1)', lct=66, /noerase
;      IF iexp EQ 1 THEN plt, density, min=0, max=20, /realcont, /nocont, subtitle=' ', title=' ', xtitle=' ', ytitle=' ', /rempli, small=[1,3,ipar+2], /noerase, lct=66, format='(F5.1)'

;      wset, 1
;      IF iexp EQ 0 AND ipar EQ 0 THEN plt, density/total(density), min=0, /realcont, /nocont, subtitle=' ', title=' ', xtitle=' ', ytitle=' ', /rempli, small=[1,3,1], lct=66, format='(F5.2)', /noerase
;      IF iexp EQ 1 THEN plt, density/total(density), min=0, /realcont, /nocont, subtitle=' ', title=' ', xtitle=' ', ytitle=' ', /rempli, small=[1,3,ipar+2], /noerase, lct=66, format='(F5.2)'

;      wset, 2
;      IF iexp EQ 1 THEN plt, density/total(density), min=0, /realcont, /nocont, subtitle=' ', title=' ', xtitle=' ', ytitle=' ', /rempli, small=[1,3,ipar+1], /noerase, lct=66, format='(F5.2)'
;      IF iexp EQ 1 AND ipar EQ 0 THEN BEGIN
;      tmp1=density
;      plt, density/total(density), min=0, /realcont, /nocont, subtitle=' ', title=' ', xtitle=' ', ytitle=' ', /rempli, small=[1,3,1], lct=66, format='(F5.2)', /noerase
;      ENDIF
;      IF iexp EQ 1 AND ipar EQ 1 THEN BEGIN
;      plt, density/total(density), min=0, /realcont, /nocont, subtitle=' ', title=' ', xtitle=' ', ytitle=' ', /rempli, small=[1,3,2], /noerase, lct=66, format='(F5.2)'
;      plt, tmp1/total(tmp1)-density/total(density), /realcont, /nocont, subtitle=' ', title=' ', xtitle=' ', ytitle=' ', /rempli, small=[1,3,3], /noerase, lct=66, format='(F5.2)'
;      ENDIF

;      wset, 3
;      IF iexp EQ 1 THEN plt, density, min=0, /realcont, /nocont, subtitle=' ', title=' ', xtitle=' ', ytitle=' ', /rempli, small=[1,3,ipar+1], /noerase, lct=66, format='(F5.1)'
;      IF iexp EQ 1 AND ipar EQ 0 THEN BEGIN
;      plt, density, min=0, /realcont, /nocont, subtitle=' ', title=' ', xtitle=' ', ytitle=' ', /rempli, small=[1,3,1], lct=66, format='(F5.1)', /noerase
;      ENDIF
;      IF iexp EQ 1 AND ipar EQ 1 THEN BEGIN
;      plt, density, min=0, /realcont, /nocont, subtitle=' ', title=' ', xtitle=' ', ytitle=' ', /rempli, small=[1,3,2], /noerase, lct=66, format='(F5.1)'
;      plt, tmp1-density, -15, 15, /realcont, /nocont, subtitle=' ', title=' ', xtitle=' ', ytitle=' ', /rempli, small=[1,3,3], /noerase, lct=66, format='(F5.1)'
;      ENDIF
;     ENDIF


;IF fig EQ 'cg' THEN BEGIN
;  wset, 0 & saveimage, 'CG_2D_OBS-'+exp+'.gif'
;  wset, 1 & saveimage, 'CG_NORM_2D_OBS-'+exp+'.gif'
;  wset, 2 & saveimage, 'CG_NORM_'+exp+'.gif'
;  wset, 3 & saveimage, 'CG_'+exp+'.gif'
;ENDIF

;IF fig EQ 'all' THEN BEGIN
;  wset, 0 & saveimage, 'TRACK_2D_OBS-'+exp+'.gif'
;  wset, 1 & saveimage, 'TRACK_NORM_2D_OBS-'+exp+'.gif'
;  wset, 2 & saveimage, 'TRACK_NORM_'+exp+'.gif'
;  wset, 3 & saveimage, 'TRACK_'+exp+'.gif'
;ENDIF

END
