PRO plot_stats_1D


; parametres
explist  = ['IBTRACS','COUPLED_SW2_KF','FORCED_SW2_KF']
baslist  = ['SIO']
datebeg  = '19900101'
ucrit    = 17.5
vorcrit  = 30.e-5
tempcrit = 1
period  = '1990-2009'
dateend = '20100101'
nbyear  = 20


; definition bin
binc      = [17.5,33,43,50,59,70]
binc_nb   = n_elements(binc)-1

binw_size = 5
binw_deb  = 20
binw_fin  = 60
binw_unit = '(m/s)'
binw_nb   = (binw_fin-binw_deb) / binw_size

binp_size = 10
binp_deb  = 920
binp_fin  = 990
binp_unit = '(hPa)'
binp_nb   = (binp_fin-binp_deb) / binp_size


; declaration
nbcat  = fltarr(3,3,binc_nb)
nbwind = fltarr(3,3,binw_nb)
nbpres = fltarr(3,3,binp_nb)
nbpts  = intarr(3,1)


FOR ibas = 0, n_elements(baslist)-1 DO BEGIN
FOR iexp = 0, n_elements(explist)-1 DO BEGIN

      basin = baslist[ibas] & help, basin
      expname = explist[iexp] & help, expname

      ; lecture best-track
      IF expname EQ 'IBTRACS' THEN BEGIN
        @read_ibtracs.pro
        loncyc = loncycn & latcyc = latcycn & uv10cyc = uv10cycn & mslpcyc = mslpcycn

        IF basin EQ 'SIO' THEN  iok = where(latcyc LE 0. AND latcyc GE -30. AND loncyc GE 40. AND loncyc LE 130. , cntok)
        IF basin EQ 'SWIO' THEN iok = where(latcyc LE 0. AND latcyc GE -30. AND loncyc GE 40. AND loncyc LE 80. , cntok)
        IF basin EQ 'SEIO' THEN iok = where(latcyc LE 0. AND latcyc GE -30. AND loncyc GE 80. AND loncyc LE 130. , cntok)
        IF basin EQ 'NIO'  THEN iok = where(latcyc GT 0. AND latcyc LE 25.  AND loncyc GE 60. AND loncyc LE 100., cntok)
        IF basin EQ 'ARS'  THEN iok = where(latcyc GT 0. AND latcyc LE 25.  AND loncyc GE 60. AND loncycn LE 80., cntok)
        IF basin EQ 'BOB'  THEN iok = where(latcyc GT 0. AND latcyc LE 25.  AND loncyc GE 80. AND loncycn LE 100., cntok)
        help, iok

      ENDIF ELSE BEGIN

        IF expname EQ 'CPL_TDK_V33'   THEN BEGIN & nbyear=7 & period='1990-1996' & dateend='19960101' & ENDIF
        IF expname EQ 'CPL_NSAS_V33'  THEN BEGIN & nbyear=7 & period='1990-1996' & dateend='19960101' & ENDIF

        pathin = '/net/adonis/usr/adonis/varclim/gslod/IDL/TRACKER/EXP_'+expname+'/DATA/' & help, pathin

        IF expname EQ 'COUPLED_SW2_KF' THEN $
        filein = 'maxima_TREAL_RVM_NICO_MASK_u'+strtrim(long(ucrit),2)+'_v'+strtrim(long(vorcrit*100000),2)+'_t0_'+period+'.idl'

        IF expname EQ 'FORCED_SW2_KF' THEN $
        filein = 'maxima_treal_new_cst_radius_u'+strtrim(long(ucrit),2)+'_v'+strtrim(long(vorcrit*100000),2)+'_t10_'+period+'.idl'

        IF expname EQ 'COUPLED_SW2_BMJ' THEN $
        filein = 'maxima_tpot_new_cst_radius_u'+strtrim(long(ucrit),2)+'_v'+strtrim(long(vorcrit*100000),2)+'_t10_'+period+'.idl'

        IF expname EQ 'CPL_TDK_V33' THEN $
        filein = 'maxima_treal_new_cst_radius_u'+strtrim(long(ucrit),2)+'_v'+strtrim(long(vorcrit*100000),2)+'_t10_'+period+'.idl'

        IF expname EQ 'CPL_NSAS_V33' THEN $
        filein = 'maxima_treal_new_cst_radius_u'+strtrim(long(ucrit),2)+'_v'+strtrim(long(vorcrit*100000),2)+'_t10_'+period+'.idl'

        restore, pathin + filein, /VERBOSE
        monthcyc = long(timedate/100.) - long(timedate/10000.)*100.
        yearcyc  = long(timedate/10000.)
        help, loncyc, latcyc, anomtcyc, mslpcyc, uv10cyc, timedate, timejuld

        IF basin EQ 'SIO' THEN iok = where(latcyc LE 0. AND latcyc GE -30. AND loncyc GE 30. AND loncyc LE 130. AND anomtcyc GE tempcrit, cntok)
        IF basin EQ 'SWIO' THEN iok = where(latcyc LE 0. AND latcyc GE -30. AND loncyc GE 40. AND loncyc LE 80. AND anomtcyc GE tempcrit, cntok)
        IF basin EQ 'SEIO' THEN iok = where(latcyc LE 0. AND latcyc GE -30. AND loncyc GE 80. AND loncyc LE 130. AND anomtcyc GE tempcrit, cntok)
        IF basin EQ 'NIO'  THEN iok = where(latcyc GT 0. AND latcyc LE 25.  AND loncyc GE 60. AND loncyc LE 100. AND anomtcyc GE tempcrit, cntok)
        IF basin EQ 'ARS'  THEN iok = where(latcyc GT 0. AND latcyc LE 25.  AND loncyc GE 60. AND loncycn LE 80. AND anomtcyc GE tempcrit, cntok)
        IF basin EQ 'BOB'  THEN iok = where(latcyc GT 0. AND latcyc LE 25.  AND loncyc GE 80. AND loncycn LE 100. AND anomtcyc GE tempcrit, cntok)

      ENDELSE

      nbpts[iexp,0] = cntok
    
      ; bin wind
      var = uv10cyc[iok]
      nbcyc = cntok
      bin_min = binw_deb & help, bin_min
      bin_max = binw_fin & help, bin_max
      bin_size = binw_size & help, bin_size
      bin_nb = binw_nb & help, bin_nb
      @bootstrap.pro
      FOR i = 0, 2 DO nbwind[iexp,i,*] = mom_bin[i,*] / (total(mom_bin[i,*],2))[0]

      ; bin mslp
      var = mslpcyc[iok]
      nbcyc = cntok
      bin_min = binp_deb & help, bin_min
      bin_max = binp_fin & help, bin_max
      bin_size = binp_size & help, bin_size
      bin_nb = binp_nb & help, bin_nb
      @bootstrap.pro
      FOR i = 0, 2 DO nbpres[iexp,i,*] = mom_bin[i,*] / (total(mom_bin[i,*],2))[0]

      ; bin cat
      var = uv10cyc[iok]
      nbcyc = cntok
      bin = binc
      bin_nb = binc_nb & help, bin_nb
      @bootstrap_irr.pro
      help, mom_bin
      FOR i = 0, 2 DO nbcat[iexp,i,*] = mom_bin[i,*] / (total(mom_bin[i,*],2))[0]

  ENDFOR; exp


reinitplt
SET_PLOT, 'X'
DEVICE, decomposed=0, retain=0
lct, 39

!y.range=[0,40]
FOR i = 0, n_elements(nbwind[0,0,*])-1 DO BEGIN
bar_name = [' ',strtrim(binw_deb+i*binw_size,2)+'-'+strtrim(binw_deb+(i+1)*binw_size,2),' ']
bar_plot, nbwind[*,0,i]*100., BACKGROUND=255, BARWIDTH=0.75, BARSPACE=0.15, BASERANGE=0.075, $
BAROFFSET= (i+1)*(2*n_elements(nbwind[*,0,0])), OVER=(I GT 0), BARNAMES=bar_name, COLORS=[0,50,250], TICKV=tickv, $
xtitle='WIND BINS (m/s)', ytitle='MAX WIND DISTRIBUTION (%)', title=basin & $
errplot, tickv, nbwind[*,1,i]*100., nbwind[*,2,i]*100. & $
xyouts, 0.75, 0.85, explist[0], /normal, color=0, charsize=1.5 & $
xyouts, 0.75, 0.80, explist[1], /normal, color=50, charsize=1.5 & $
xyouts, 0.75, 0.75, explist[2], /normal, color=250, charsize=1.5 & $
ENDFOR
;saveimage, 'WIND_IBTRACS_'+exp+'_'+basin+'.gif'
stop

!y.range=[0,50]
FOR i = 0, n_elements(nbpres[0,0,*])-1 DO BEGIN
bar_name = [' ',strtrim(binp_deb+i*binp_size,2)+'-'+strtrim(binp_deb+(i+1)*binp_size,2),' ']
bar_plot, nbpres[*,0,i]*100., BACKGROUND=255, BARWIDTH=0.75, BARSPACE=0.15, BASERANGE=0.075, $
BAROFFSET= (i+1)*(2*n_elements(nbpres[*,0,0])), OVER=(I GT 0), BARNAMES=bar_name, COLORS=[0,50,250], TICKV=tickv, $
xtitle='MSLP BINS (hPa)', ytitle='MSLP DISTRIBUTION (%)', title=basin
errplot, tickv, nbpres[*,1,i]*100., nbpres[*,2,i]*100.
xyouts, 0.15, 0.85, explist[0], /normal, color=0, charsize=1.5
xyouts, 0.15, 0.80, explist[1], /normal, color=50, charsize=1.5
xyouts, 0.15, 0.75, explist[2], /normal, color=250, charsize=1.5
ENDFOR
;saveimage, 'PRES_IBTRACS_'+exp+'_'+basin+'.gif'
stop

!y.range=[0,90]
FOR i = 0, n_elements(nbcat[0,0,*])-1 DO BEGIN
bar_name = [' ',strtrim(fix(binc[i]),2)+'-'+strtrim(fix(binc[i+1]),2),' ']
bar_plot, nbcat[*,0,i]*100., BACKGROUND=255, BARWIDTH=0.75, BARSPACE=0.15, BASERANGE=0.075, $
BAROFFSET= (i+1)*(3*n_elements(nbcat[*,0,0])), OVER=(I GT 0), BARNAMES=bar_name, COLORS=[0,50,250], TICKV=tickv, $
xtitle='CAT BINS (m/s)', ytitle='FREQUENCY (%)',title=basin
errplot, tickv, nbcat[*,1,i]*100., nbcat[*,2,i]*100.
xyouts, 0.75, 0.90, explist[0], /normal, color=0, charsize=1.5
xyouts, 0.75, 0.85, explist[1], /normal, color=50, charsize=1.5
xyouts, 0.75, 0.80, explist[2], /normal, color=250, charsize=1.5
ENDFOR
;saveimage, 'CAT_IBTRACS_'+exp+'_'+basin+'.gif'
stop

ENDFOR

END
