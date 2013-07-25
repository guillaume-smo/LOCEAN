PRO plot_stats_1D_tracker
; plot de stats 1D cyclogenese+intensite deduites de fichiers format "tracker idl"


; PARAMETERS
explist  = ['IBTRACS','COUPLED_SW2_KF','COUPLED_SW2_BMJ']
basin    = 'IO' ; SIO / NIO / IO

; TRACKER
ucrit    = 17.5   ; critere vent
vorcrit  = 30.e-5 ; critere vorticite
vorelax  = 30.e-5 ; critere vorticite relaxee
templist = [1]    ; critere anomalie temperature

; PLOT
write_ps = 1      ; ecriture figures ps
plot_cg  = 0      ; plots cg distribution
plot_wp  = 1      ; plots wind + pressure distribution
plt_path = '/usr/home/gslod/IDL/STATS_TRACKER/FIGS_1D_SIG95/'

; PERIOD
datebeg = '19900101'
period  = '1990-2009'
dateend = '20100101'
nbyear  = 20
yearini = 1990


; BIN DEFINITIONS

  ; US categories bins
  binc      = [17.5,33,43,50,59,70]
  binc_nb   = n_elements(binc)-1

  ; wind bins
  binw_size = 5
  binw_deb  = 20
  binw_fin  = 60
  binw_unit = '(m/s)'
  binw_nb   = (binw_fin-binw_deb) / binw_size

  ; pressure bins
  binp_size = 10
  binp_deb  = 920
  binp_fin  = 990
  binp_unit = '(hPa)'
  binp_nb   = (binp_fin-binp_deb) / binp_size


; DECLARATIONS
nbmaxcat  = fltarr(3,3,binc_nb)
nbmaxwind = fltarr(3,3,binw_nb)
nbminpres = fltarr(3,3,binp_nb)
nbwind = fltarr(3,3,binw_nb)
nbpres = fltarr(3,3,binp_nb)
nbwind_bin_year  = fltarr(binw_nb,nbyear)
nbpres_bin_year  = fltarr(binp_nb,nbyear)
nbwind_swen = fltarr(3,3,binw_nb)
nbpres_swen = fltarr(3,3,binp_nb)
nbcg_month    = fltarr(3,3,12)
nbcg_month_year  = fltarr(3,12,nbyear)
nbcg_quartil  = fltarr(3,2,12)
nbcg_monthe   = fltarr(3,3,12)
nbcg_month_yeare = fltarr(3,12,nbyear)
nbcg_quartile = fltarr(3,2,12)
nbcg_monthw   = fltarr(3,3,12)
nbcg_quartilw = fltarr(3,2,12)
nbcg_month_yearw = fltarr(3,12,nbyear)


; READ TRACKER DATA
FOR iexp = 0, n_elements(explist)-1 DO BEGIN
FOR item = 0, n_elements(templist)-1 DO BEGIN

      tempcrit = templist[item]
      expn = explist[iexp] & help, expn

      IF expn EQ 'IBTRACS' THEN BEGIN
        @read_ibtracs.pro
      ENDIF ELSE BEGIN
        IF expn EQ 'CPL_TDK_V33' OR expn EQ 'CPL_NSAS_V33'  THEN BEGIN & nbyear=7 & period='1990-1996' & dateend='19960101' & ENDIF

        pathin = '/net/adonis/usr/adonis/varclim/gslod/IDL/TRACKER/EXP_'+expn+'/DATA/' & help, pathin

        IF expn EQ 'COUPLED_SW2_BMJ' THEN $
        filein = 'tracker_light_TREAL_RVM_NICO_MASK_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2) +'_'+period+'.idl'
;        filein = 'tracker_light_tpot_new_cst_radius_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2) +'_'+period+'.idl'

        IF expn EQ 'COUPLED_SW2_KF' THEN $
        filein = 'tracker_light_TREAL_RVM_NICO_MASK_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2) +'_'+period+'.idl'
;        filein = 'tracker_light_treal_new_cst_radius_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2) +'_'+period+'.idl'

        IF expn EQ 'FORCED_SW2_KF' THEN $
        filein = 'tracker_light_treal_nico_radius_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2) +'_'+period+'.idl'
;        filein = 'tracker_light_treal_new_cst_radius_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2) +'_'+period+'.idl'

        IF expn EQ 'CPL_TDK_V33' THEN $
        filein = 'tracker_light_treal_new_cst_radius_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2) +'_'+period+'.idl'

        IF expn EQ 'CPL_NSAS_V33' THEN $
        filein = 'tracker_light_treal_new_cst_radius_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2) +'_'+period+'.idl'
        help, filein

        restore, pathin + filein, /VERBOSE
        monthcycn = long(datecycn/100.) - long(datecycn/10000.)*100.
        yearcycn  = long(datecycn/10000.)
        help, loncycn, latcycn, mslpcycn, uv10cycn, datecycn, juldcycn
      ENDELSE

      ; selection par bassin
      IF basin EQ 'SIO' THEN BEGIN
        indcg = where(latcycn[*,0] LT 0. AND latcycn[*,0] GE -30. AND loncycn[*,0] GE 30. AND loncycn[*,0] LE 130., cntcg)
        indcge = where(latcycn[*,0] LT 0. AND latcycn[*,0] GE -30. AND loncycn[*,0] GE 80. AND loncycn[*,0] LE 130., cntcge)
        indcgw = where(latcycn[*,0] LT 0. AND latcycn[*,0] GE -30. AND loncycn[*,0] GE 30. AND loncycn[*,0] LE 80., cntcgw)
        indtc = where(latcycn LT 0. AND latcycn GE -30. AND loncycn GE 30. AND loncycn LE 130., cnttc)
        indtce = where(latcycn LT 0. AND latcycn GE -30. AND loncycn GE 80. AND loncycn LE 130., cnttce)
        indtcw = where(latcycn LT 0. AND latcycn GE -30. AND loncycn GE 30. AND loncycn LE 80., cnttcw)
      ENDIF
      IF basin EQ 'NIO' THEN BEGIN
        indcg = where(latcycn[*,0] GT 0. AND latcycn[*,0] LE 25.  AND loncycn[*,0] GE 60. AND loncycn[*,0] LE 100., cntcg)
        indcge = where(latcycn[*,0] GT 0. AND latcycn[*,0] LE 25.  AND loncycn[*,0] GE 60. AND loncycn[*,0] LE 80., cntcge)
        indcgw = where(latcycn[*,0] GT 0. AND latcycn[*,0] LE 25.  AND loncycn[*,0] GE 80. AND loncycn[*,0] LE 100., cntcgw)
        indtc = where(latcycn GT 0. AND latcycn LE 25.  AND loncycn GE 60. AND loncycn LE 100., cnttc)
        indtce = where(latcycn GT 0. AND latcycn LE 25.  AND loncycn GE 60. AND loncycn LE 80., cnttce)
        indtcw = where(latcycn GT 0. AND latcycn LE 25.  AND loncycn GE 80. AND loncycn LE 100., cnttcw)
      ENDIF
      IF basin EQ 'IO' THEN BEGIN
        indcg = where((latcycn[*,0] GT 0. AND latcycn[*,0] LE 25.  AND loncycn[*,0] GE 60. AND loncycn[*,0] LE 100.) OR $
                      (latcycn[*,0] LT 0. AND latcycn[*,0] GE -30. AND loncycn[*,0] GE 30. AND loncycn[*,0] LE 130.), cntcg)
        indtc = where((latcycn GT 0. AND latcycn LE 25.  AND loncycn GE 60. AND loncycn LE 100.) OR $
                      (latcycn LT 0. AND latcycn GE -30. AND loncycn GE 30. AND loncycn LE 130.), cnttc)
      ENDIF

      ; declaration
      maxwind = fltarr(cntcg)
      minpres = fltarr(cntcg)
      FOR j = 0, cntcg-1 DO BEGIN
        maxwind[j] = max(uv10cycn[indcg[j],*],/nan)
        minpres[j] = min(mslpcycn[indcg[j],*],/nan)
      ENDFOR

      IF plot_wp THEN BEGIN
      ; max wind per TC
      print, 'CALCUL DISTRIB MAX WIND PER TC...'
      obs = maxwind & help, obs
      nbobs = cntcg & help, nbobs
      bin_min = binw_deb & help, bin_min
      bin_max = binw_fin & help, bin_max
      bin_size = binw_size & help, bin_size
      bin_nb = binw_nb & help, bin_nb
      mom_bin= bootstrap_hist( obs, nbobs, bin_nb, bin_min, bin_size)
      FOR i = 0, 2 DO nbmaxwind[iexp,i,*] = mom_bin[i,*] / (total(mom_bin[i,*],2))[0]

      ; max wind per TC (by category)
      print, 'CALCUL DISTRIB MAX WIND PER TC BY CAT BIN...'
      obs = maxwind & help, obs
      nbobs = cntcg & help, nbobs
      bin = binc
      bin_nb = binc_nb & help, bin_nb
      @bootstrap_irr
      FOR i = 0, 2 DO nbmaxcat[iexp,i,*] = mom_bin[i,*] / (total(mom_bin[i,*],2))[0]

      ; min pres per TC
      print, 'CALCUL DISTRIB MIN MSLP PER TC...'
      obs = minpres & help, obs
      nbobs = cntcg & help, nbobs
      bin_min = binp_deb & help, bin_min
      bin_max = binp_fin & help, bin_max
      bin_size = binp_size & help, bin_size
      bin_nb = binp_nb & help, bin_nb
      mom_bin= bootstrap_hist( obs, nbobs, bin_nb, bin_min, bin_size)
      FOR i = 0, 2 DO nbminpres[iexp,i,*] = mom_bin[i,*] / (total(mom_bin[i,*],2))[0]

      ; all wind
      print, 'CALCUL DISTRIB ALL WIND...'
      obs = uv10cycn[indtc] & help, obs
      nbobs = cnttc & help, nbobs
      bin_min = binw_deb & help, bin_min
      bin_max = binw_fin & help, bin_max
      bin_size = binw_size & help, bin_size
      bin_nb = binw_nb & help, bin_nb
      nbwind_swen[iexp,0,*] = hist_1D(obs, bin_nb, bin_min, bin_size)
      nbwind_swen[iexp,0,*] = nbwind_swen[iexp,0,*] / (total(nbwind_swen[iexp,0,*],3))[0]

      ; SWEN-STATS-STYLE
      ; calcul 1 distrib per year
      FOR j = 0, nbyear-1 DO BEGIN
        indok = where(yearcycn[indtc] EQ yearini+j)
        obs = uv10cycn[indtc[indok]] & help, obs
        nbobs = n_elements(obs) & help, nbobs
        bin_min = binw_deb & help, bin_min
        bin_max = binw_fin & help, bin_max
        bin_size = binw_size & help, bin_size
        bin_nb = binw_nb & help, bin_nb
        nbwind_bin_year[*,j] = hist_1D( obs, bin_nb, bin_min, bin_size)
        nbwind_bin_year[*,j] = nbwind_bin_year[*,j] / (total(nbwind_bin_year[*,j],1))[0]
      ENDFOR
      ; calcul bootstrap par bin de vent
      obs = nbwind_bin_year & help, obs
      nbobs = nbyear & help, nbobs
      bin_min = binw_deb & help, bin_min
      bin_max = binw_fin & help, bin_max
      bin_size = binw_size & help, bin_size
      bin_nb = binw_nb & help, bin_nb
      nbwind_swen[iexp,1:2,*] = bootstrap_swen( obs, nbobs, bin_nb, bin_min, bin_size)

      ; all pres
      print, 'CALCUL DISTRIB ALL MSLP...'
      obs = mslpcycn[indtc] & help, obs
      nbobs = cnttc & help, nbobs
      bin_min = binp_deb & help, bin_min
      bin_max = binp_fin & help, bin_max
      bin_size = binp_size & help, bin_size
      bin_nb = binp_nb & help, bin_nb
      nbpres_swen[iexp,0,*] = hist_1D(obs, bin_nb, bin_min, bin_size) 
      nbpres_swen[iexp,0,*] = nbpres_swen[iexp,0,*] / (total(nbpres_swen[iexp,0,*],3))[0]

      ; SWEN-STATS-STYLE
      ; calcul 1 distrib per year
      FOR j = 0, nbyear-1 DO BEGIN
        indok = where(yearcycn[indtc] EQ yearini+j)
        obs = mslpcycn[indtc[indok]] & help, obs
        nbobs = n_elements(obs) & help, nbobs
        bin_min = binp_deb & help, bin_min
        bin_max = binp_fin & help, bin_max
        bin_size = binp_size & help, bin_size
        bin_nb = binp_nb & help, bin_nb
        nbpres_bin_year[*,j] = hist_1D( obs, bin_nb, bin_min, bin_size)
        nbpres_bin_year[*,j] = nbpres_bin_year[*,j] / (total(nbpres_bin_year[*,j],1))[0]   
      ENDFOR
      ; calcul bootstrap par bin de pression
      obs = nbpres_bin_year & help, obs
      nbobs = nbyear & help, nbobs
      bin_min = binp_deb & help, bin_min
      bin_max = binp_fin & help, bin_max
      bin_size = binp_size & help, bin_size
      bin_nb = binp_nb & help, bin_nb
      nbpres_swen[iexp,1:2,*] = bootstrap_swen( obs, nbobs, bin_nb, bin_min, bin_size)

      ENDIF


      ; cg number
      IF plot_cg THEN BEGIN
      print, 'CALCUL DISTRIB CG...'
      obs = monthcycn[indcg,0] & help, obs
      nbobs = cntcg & help, nbobs
      bin_min = 1
      bin_max = 12
      bin_size = 1
      bin_nb  = 12
      mom_bin= bootstrap_hist( obs, nbobs, bin_nb, bin_min, bin_size)
      nbcg_month[iexp,*,*] = mom_bin / float(nbyear)
      print, 'VERIF CG NUMBER TOTAL: ', total(nbcg_month[iexp,0,*]), cntcg/float(nbyear) & STOP
      ; quartile
      FOR j = 0, nbyear-1 DO BEGIN & FOR i = 0, 12-1 DO BEGIN 
        tmp = where(yearcycn[indcg,0] EQ yearini+j AND monthcycn[indcg,0] EQ i+1)
        IF tmp[0] NE -1 THEN nbcg_month_year[iexp,i,j] = n_elements(tmp) ELSE nbcg_month_year[iexp,i,j] = 0
      ENDFOR & ENDFOR
      FOR i = 0, 11 DO nbcg_quartil[iexp,*,i] = percentile(nbcg_month_year[iexp,i,*],0.25)


      IF basin NE 'IO' THEN BEGIN
      ; cg number east
      obs = monthcycn[indcge,0]
      nbobs = cntcge
      bin_min = 1
      bin_max = 12
      bin_size = 1
      bin_nb  = 12
      mom_bin= bootstrap_hist( obs, nbobs, bin_nb, bin_min, bin_size)
      nbcg_monthe[iexp,*,*] = mom_bin / float(nbyear)
      print, 'VERIF CG NUMBER EAST: ', total(nbcg_monthe[iexp,0,*]), cntcge/float(nbyear)
      ; quartile
      FOR j = 0, nbyear-1 DO BEGIN & FOR i = 0, 12-1 DO BEGIN
        tmp = where(yearcycn[indcge,0] EQ yearini+j AND monthcycn[indcge,0] EQ i+1)
        IF tmp[0] NE -1 THEN nbcg_month_yeare[iexp,i,j] = n_elements(tmp) ELSE nbcg_month_yeare[iexp,i,j] = 0 
      ENDFOR & ENDFOR
      FOR i = 0, 11 DO nbcg_quartile[iexp,*,i] = percentile(nbcg_month_yeare[iexp,i,*],0.25)


      ; cg number west
      obs = monthcycn[indcgw,0]
      nbobs = cntcgw
      bin_min = 1
      bin_max = 12
      bin_size = 1
      bin_nb  = 12
      mom_bin= bootstrap_hist( obs, nbobs, bin_nb, bin_min, bin_size)
      nbcg_monthw[iexp,*,*] = mom_bin / float(nbyear)
      print, 'VERIF CG NUMBER WEST: ', total(nbcg_monthw[iexp,0,*]), cntcgw/float(nbyear)
      ; quartile
      FOR j = 0, nbyear-1 DO BEGIN & FOR i = 0, 12-1 DO BEGIN
        tmp = where(yearcycn[indcgw,0] EQ yearini+j AND monthcycn[indcgw,0] EQ i+1)
        IF tmp[0] NE -1 THEN nbcg_month_yearw[iexp,i,j] = n_elements(tmp) ELSE nbcg_month_yearw[iexp,i,j] = 0 
      ENDFOR & ENDFOR
      FOR i = 0, 11 DO nbcg_quartilw[iexp,*,i] = percentile(nbcg_month_yearw[iexp,i,*],0.25)
      ENDIF
      ENDIF

  ENDFOR; exp
ENDFOR; temp



; PLOTS 1D
reinitplt
SET_PLOT, 'X'
DEVICE, decomposed=0, retain=0
lct, 33


IF plot_wp THEN BEGIN

; PLOT ALL-WIND DISTRIBUTION
IF write_ps THEN openps, filename=plt_path+'WIND_DIST_'+expn+'_'+basin+'.ps'
IF basin EQ 'SIO' THEN !y.range=[0,60] ELSE !y.range=[0,70]
print, 'PLOT WIND DISTRIBUTION...'
FOR i = 0, n_elements(nbwind_swen[0,0,*])-1 DO BEGIN
  bar_name = [' ',strtrim(binw_deb+i*binw_size,2)+'-'+strtrim(binw_deb+(i+1)*binw_size,2),' ']
  sbar_plot, nbwind_swen[*,0,i]*100., BACKGROUND=255, BARWIDTH=0.75, BARSPACE=0.15, BASERANGE=0.075, $
  BAROFFSET= (i+1)*(2*n_elements(nbwind_swen[*,0,0])), OVERPLOT=(I GT 0), BARNAMES=bar_name, COLORS=[0,50,225], TICKV=tickv, $
  xtitle='WIND BINS (m/s)', ytitle='WIND DISTRIBUTION (%)', title=basin, YMIN=!y.range[0], YMAX=!y.range[1]
  errplot, tickv, nbwind_swen[*,1,i]*100., nbwind_swen[*,2,i]*100. & $
  xyouts, 0.15, 0.150, explist[0], /normal, color=0, charsize=1.5
  xyouts, 0.15, 0.125, explist[1], /normal, color=50, charsize=1.5
  xyouts, 0.15, 0.100, explist[2], /normal, color=225, charsize=1.5
ENDFOR
;saveimage, 'WIND_IBTRACS_'+exp+'_'+basin+'.gif'
IF write_ps THEN closeps ELSE STOP


; PLOT MAX WIND DISTRIBUTION
;IF write_ps THEN openps, filename='MAXWIND_IBTRACS_'+expn+'_'+basin+'.ps'
;!y.range=[0,40]
;FOR i = 0, n_elements(nbmaxwind[0,0,*])-1 DO BEGIN
;bar_name = [' ',strtrim(binw_deb+i*binw_size,2)+'-'+strtrim(binw_deb+(i+1)*binw_size,2),' ']
;bar_plot, nbmaxwind[*,0,i]*100., BACKGROUND=255, BARWIDTH=0.75, BARSPACE=0.15, BASERANGE=0.075, $
;BAROFFSET= (i+1)*(2*n_elements(nbmaxwind[*,0,0])), OVER=(I GT 0), BARNAMES=bar_name, COLORS=[0,50,225], TICKV=tickv, $
;xtitle='WIND BINS (m/s)', ytitle='MAX(WIND) DISTRIBUTION (%)', title=basin & $
;errplot, tickv, nbmaxwind[*,1,i]*100., nbmaxwind[*,2,i]*100. & $
;xyouts, 0.75, 0.85, explist[0], /normal, color=0, charsize=1.5 & $
;xyouts, 0.75, 0.80, explist[1], /normal, color=50, charsize=1.5 & $
;xyouts, 0.75, 0.75, explist[2], /normal, color=225, charsize=1.5 & $
;ENDFOR
;saveimage, 'MAXWIND_IBTRACS_'+exp+'_'+basin+'.gif'
;IF write_ps THEN closeps & STOP


; PLOT PRESSURE DISTRIBUTION
IF write_ps THEN openps, filename=plt_path+'PRES_DIST_'+expn+'_'+basin+'.ps'
IF basin EQ 'SIO' THEN !y.range=[0,80] ELSE !y.range=[0,100]
print, 'PLOT PRESSURE DISTRIBUTION...'
FOR i = 0, n_elements(nbpres_swen[0,0,*])-1 DO BEGIN
  bar_name = [' ',strtrim(binp_deb+i*binp_size,2)+'-'+strtrim(binp_deb+(i+1)*binp_size,2),' ']
  sbar_plot, nbpres_swen[*,0,i]*100., BACKGROUND=255, BARWIDTH=0.75, BARSPACE=0.15, BASERANGE=0.075, $
  BAROFFSET= (i+1)*(2*n_elements(nbpres_swen[*,0,0])), OVER=(I GT 0), BARNAMES=bar_name, COLORS=[0,50,225], TICKV=tickv, $
  xtitle='MSLP BINS (hPa)', ytitle='MSLP DISTRIBUTION (%)', title=basin, YMIN=!y.range[0], YMAX=!y.range[1]
  errplot, tickv, nbpres_swen[*,1,i]*100., nbpres_swen[*,2,i]*100.
  xyouts, 0.15, 0.150, explist[0], /normal, color=0, charsize=1.5
  xyouts, 0.15, 0.125, explist[1], /normal, color=50, charsize=1.5
  xyouts, 0.15, 0.100, explist[2], /normal, color=225, charsize=1.5
ENDFOR
;saveimage, 'PRES_IBTRACS_'+exp+'_'+basin+'.gif'
IF write_ps THEN closeps ELSE STOP


; PLOT MIN PRESSURE DISTRIBUTION
;IF write_ps THEN openps, filename='MINPRES_IBTRACS_'+expn+'_'+basin+'.ps'
;!y.range=[0,50]
;FOR i = 0, n_elements(nbminpres[0,0,*])-1 DO BEGIN
;bar_name = [' ',strtrim(binp_deb+i*binp_size,2)+'-'+strtrim(binp_deb+(i+1)*binp_size,2),' ']
;bar_plot, nbminpres[*,0,i]*100., BACKGROUND=255, BARWIDTH=0.75, BARSPACE=0.15, BASERANGE=0.075, $
;BAROFFSET= (i+1)*(2*n_elements(nbminpres[*,0,0])), OVER=(I GT 0), BARNAMES=bar_name, COLORS=[0,50,225], TICKV=tickv, $
;xtitle='MSLP BINS (hPa)', ytitle='MSLP DISTRIBUTION (%)', title=basin
;errplot, tickv, nbminpres[*,1,i]*100., nbminpres[*,2,i]*100.
;& xyouts, 0.15, 0.15 , exp_list[0], /normal, color=0  , charsize=2
;& xyouts, 0.15, 0.125, exp_list[1], /normal, color=50 , charsize=2
;& xyouts, 0.15, 0.1  , exp_list[2], /normal, color=225, charsize=2
;ENDFOR
;saveimage, 'MINPRES_IBTRACS_'+exp+'_'+basin+'.gif'
;IF write_ps THEN closeps
;stop

ENDIF ; plot_wp



IF plot_cg THEN BEGIN

; PLOT CG SC
IF write_ps THEN openps, filename=plt_path+'CG_SC_'+expn+'_'+basin+'.ps'
IF basin EQ 'SIO' THEN !y.range=[0,8] ELSE !y.range=[0,3]
FOR i = 0, n_elements(nbcg_month[0,0,*])-1 DO BEGIN
  bar_name = [' ',strtrim(i+1,2),' ']
  sbar_plot, nbcg_month[0:2,0,i], BACKGROUND=255, BARWIDTH=0.75, BARSPACE=0.15, BASERANGE=0.075, $
  BAROFFSET= i*(1.5*n_elements(nbcg_month[*,0,0])), OVER=(i GT 0), BARNAMES=bar_name, COLORS=[0,50,225], TICKV=tickv, $
  xtitle='MONTH', ytitle='CYCLOGENESIS NUMBER', title=basin, ymin=!y.range[0], ymax=!y.range[1]
  ;errplot, tickv, nbcg_quartil[0:2,0,i], nbcg_quartil[0:2,1,i], thick=2
  errplot, tickv, nbcg_month[0:2,1,i], nbcg_month[0:2,2,i]
  xyouts, 0.15, 0.15 , explist[0]+': '+ strtrim(total(nbcg_month[0,0,*],3),2), /normal, color=0  , charsize=1.5
  xyouts, 0.15, 0.125, explist[1]+': '+ strtrim(total(nbcg_month[1,0,*],3),2), /normal, color=50 , charsize=1.5
  xyouts, 0.15, 0.1  , explist[2]+': '+ strtrim(total(nbcg_month[2,0,*],3),2), /normal, color=225, charsize=1.5
ENDFOR
;saveimage, 'SC_CG_'+basin+'_CPL.gif'
IF write_ps THEN closeps ELSE stop


; PLOT NORMALIZED CG SC
IF write_ps THEN openps, filename=plt_path+'CG_SC_NORM_'+expn+'_'+basin+'.ps'
FOR i = 0, n_elements(nbcg_month[0,0,*])-1 DO BEGIN
  bar_name = [' ',strtrim(i+1,2),' ']
  sbar_plot, nbcg_month[0:2,0,i]/total(nbcg_month[0:2,0,*],3)*100., BACKGROUND=255, BARWIDTH=0.75, BARSPACE=0.15, BASERANGE=0.075, $
  BAROFFSET= i*(1.5*n_elements(nbcg_month[*,0,0])), OVER=(I GT 0), BARNAMES=bar_name, COLORS=[0,50,225], TICKV=tickv, $
  xtitle='MONTH', ytitle='CYCLOGENESIS PERCENTAGE', title=basin, ymin=0, ymax=30
  errplot, tickv, nbcg_month[0:2,1,i]/total(nbcg_month[0:2,0,*],3)*100., nbcg_month[0:2,2,i]/total(nbcg_month[0:2,0,*],3)*100.
  xyouts, 0.15, 0.150, explist[0]+': '+ strtrim(total(nbcg_month[0,0,*],3),2), /normal, color=0, charsize=1.5
  xyouts, 0.15, 0.125, explist[1]+': '+ strtrim(total(nbcg_month[1,0,*],3),2), /normal, color=50, charsize=1.5
  xyouts, 0.15, 0.100, explist[2]+': '+ strtrim(total(nbcg_month[2,0,*],3),2), /normal, color=225, charsize=1.5
ENDFOR
;saveimage, 'SC_CG_'+basin+'_CPL.gif'
IF write_ps THEN closeps ELSE stop


IF basin NE 'IO' THEN BEGIN
; PLOT EAST CG SC
IF write_ps THEN openps, filename='CG_SC_'+expn+'_'+basin+'_EAST.ps'
IF basin EQ 'SIO' THEN !y.range=[0,5] ELSE !y.range=[0,1]
FOR i = 0, n_elements(nbcg_monthe[0,0,*])-1 DO BEGIN
  bar_name = [' ',strtrim(i+1,2),' ']
  sbar_plot, nbcg_monthe[0:2,0,i], BACKGROUND=255, BARWIDTH=0.75, BARSPACE=0.15, BASERANGE=0.075, $
  BAROFFSET= i*(1.5*n_elements(nbcg_monthe[*,0,0])), OVER=(I GT 0), BARNAMES=bar_name, COLORS=[0,50,225], TICKV=tickv, $
  xtitle='MONTH', ytitle='CG NUMBER', title='EAST '+basin, ymin=!y.range[0], ymax=!y.range[1]
  errplot, tickv, nbcg_quartile[0:2,0,i], nbcg_quartile[0:2,1,i], thick=2
  xyouts, 0.1, 0.90, explist[0]+': '+ strtrim(total(nbcg_monthe[0,0,*],3),2), /normal, color=0, charsize=1.5
  xyouts, 0.1, 0.85, explist[1]+': '+ strtrim(total(nbcg_monthe[1,0,*],3),2), /normal, color=50, charsize=1.5
  xyouts, 0.1, 0.80, explist[2]+': '+ strtrim(total(nbcg_monthe[2,0,*],3),2), /normal, color=225, charsize=1.5
ENDFOR
;saveimage, 'SC_CG_E'+basin+'_CPL.gif'
IF write_ps THEN closeps ELSE stop


; PLOT NORMALIZED EAST CG SC
IF write_ps THEN openps, filename='CG_SC_NORM_'+expn+'_'+basin+'_EAST.ps'
FOR i = 0, n_elements(nbcg_month[0,0,*])-1 DO BEGIN
  bar_name = [' ',strtrim(i+1,2),' ']
  sbar_plot, nbcg_monthe[0:2,0,i]/total(nbcg_monthe[0:2,0,*],3)*100., BACKGROUND=255, BARWIDTH=0.75, BARSPACE=0.15, BASERANGE=0.075, $
  BAROFFSET= i*(1.5*n_elements(nbcg_monthe[*,0,0])), OVER=(I GT 0), BARNAMES=bar_name, COLORS=[0,50,225], TICKV=tickv, $
  xtitle='MONTH', ytitle='CYCLOGENESIS PERCENTAGE', title='EAST '+basin, ymin=0, ymax=30
  xyouts, 0.15, 0.90, explist[0]+': '+ strtrim(total(nbcg_monthe[0,0,*],3),2), /normal, color=0, charsize=1.5
  xyouts, 0.15, 0.85, explist[1]+': '+ strtrim(total(nbcg_monthe[1,0,*],3),2), /normal, color=50, charsize=1.5
  xyouts, 0.15, 0.80, explist[2]+': '+ strtrim(total(nbcg_monthe[2,0,*],3),2), /normal, color=225, charsize=1.5
ENDFOR
FOR i = 0,2 DO print, 'VERIF:', total(reform(nbcg_monthe[i,0,*])/total(reform(nbcg_monthe[i,0,*]))*100.)
;saveimage, 'SC_CG_'+basin+'_CPL.gif'
IF write_ps THEN closeps ELSE stop


; PLOT WEST CG SC
IF write_ps THEN openps, filename='CG_SC_'+expn+'_'+basin+'_WEST.ps'
IF basin EQ 'SIO' THEN !y.range=[0,5] ELSE !y.range=[0,2]
FOR i = 0, n_elements(nbcg_monthw[0,0,*])-1 DO BEGIN
  bar_name = [' ',strtrim(i+1,2),' ']
  sbar_plot, nbcg_monthw[0:2,0,i], BACKGROUND=255, BARWIDTH=0.75, BARSPACE=0.15, BASERANGE=0.075, $
  BAROFFSET= i*(1.5*n_elements(nbcg_monthw[*,0,0])), OVER=(I GT 0), BARNAMES=bar_name, COLORS=[0,50,225], TICKV=tickv, $
  xtitle='MONTH', ytitle='CG NUMBER', title='WEST '+basin, ymin=!y.range[0], ymax=!y.range[1]
  errplot, tickv, nbcg_quartilw[0:2,0,i], nbcg_quartilw[0:2,1,i], thick=2
  xyouts, 0.1, 0.90, explist[0]+': '+ strtrim(total(nbcg_monthw[0,0,*],3),2), /normal, color=0, charsize=1.5
  xyouts, 0.1, 0.85, explist[1]+': '+ strtrim(total(nbcg_monthw[1,0,*],3),2), /normal, color=50, charsize=1.5
  xyouts, 0.1, 0.80, explist[2]+': '+ strtrim(total(nbcg_monthw[2,0,*],3),2), /normal, color=225, charsize=1.5
ENDFOR
;saveimage, 'SC_CG_W'+basin+'_CPL.gif'
IF write_ps THEN closeps ELSE stop


; PLOT NORMALIZED WEST CG SC
IF write_ps THEN openps, filename='CG_SC_NORM_'+expn+'_'+basin+'_WEST.ps'
IF basin EQ 'SIO' THEN !y.range=[0,30] ELSE !y.range=[0,30]
FOR i = 0, n_elements(nbcg_month[0,0,*])-1 DO BEGIN
  bar_name = [' ',strtrim(i+1,2),' ']
  sbar_plot, nbcg_monthw[0:2,0,i]/total(nbcg_monthw[0:2,0,*],3)*100., BACKGROUND=255, BARWIDTH=0.75, BARSPACE=0.15, BASERANGE=0.075, $
  BAROFFSET= i*(1.5*n_elements(nbcg_monthw[*,0,0])), OVER=(I GT 0), BARNAMES=bar_name, COLORS=[0,50,225], TICKV=tickv, $
  xtitle='MONTH', ytitle='CYCLOGENESIS PERCENTAGE', title='WEST '+basin, ymin=0, ymax=30
  xyouts, 0.15, 0.90, explist[0]+': '+ strtrim(total(nbcg_monthw[0,0,*],3),2), /normal, color=0, charsize=1.5
  xyouts, 0.15, 0.85, explist[1]+': '+ strtrim(total(nbcg_monthw[1,0,*],3),2), /normal, color=50, charsize=1.5
  xyouts, 0.15, 0.80, explist[2]+': '+ strtrim(total(nbcg_monthw[2,0,*],3),2), /normal, color=225, charsize=1.5
ENDFOR
FOR i = 0,2 DO print, 'VERIF:', total(reform(nbcg_monthw[i,0,*])/total(reform(nbcg_monthw[i,0,*]))*100.)
;saveimage, 'SC_CG_'+basin+'_CPL.gif'
IF write_ps THEN closeps ELSE stop
ENDIF

ENDIF ; plot_cg


; OTHER OLD STUFFS

;FOR i = 0, n_elements(nbcat[0,0,*])-1 DO BEGIN
;bar_name = [' ','cat-'+strtrim(i,2),' '] & help, bar_name
;bar_plot, nbcat[*,0,i], BACKGROUND=255, BARWIDTH=0.75, BARSPACE=0.25, BASERANGE=0.12, $
;BAROFFSET= i*(3*n_elements(nbcat[*,0,0])), OVER=(I GT 0), yrange=[0,15], BARNAMES=bar_name
;ENDFOR

;window, 1
;FOR i = 0, n_elements(nbwind[0,0,*])-1 DO BEGIN
;bar_name = [' ',strtrim(binw_deb+i*binw_size,2)+'-'+strtrim(binw_deb+(i+1)*binw_size,2),' ']
;bar_plot, nbwind[*,0,i], BACKGROUND=255, BARWIDTH=0.75, BARSPACE=0.25, BASERANGE=0.12, $
;BAROFFSET= i*(2.4*n_elements(nbwind[*,0,0])), OVER=(I GT 0), yrange=[0,15], BARNAMES=bar_name
;ENDFOR

;window, 2
;FOR i = 0, n_elements(nbpres[0,0,*])-1 DO BEGIN
;bar_name = [' ',strtrim(binp_deb+i*binp_size,2)+'-'+strtrim(binp_deb+(i+1)*binp_size,2),' ']
;bar_plot, nbpres[*,0,i], BACKGROUND=255, BARWIDTH=0.75, BARSPACE=0.25, BASERANGE=0.12, $
;BAROFFSET= i*(2.4*n_elements(nbpres[*,0,0])), OVER=(I GT 0), yrange=[0,20], BARNAMES=bar_name
;ENDFOR


STOP
END
