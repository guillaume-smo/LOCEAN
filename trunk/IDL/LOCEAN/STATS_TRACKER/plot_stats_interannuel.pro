PRO plot_stats_interannuel


; parametres
explist  = ['IBTRACS','COUPLED_SW2_KF','COUPLED_SW2_BMJ']
datebeg  = '19900101'
ucrit    = 17.5
vorcrit  = 30.e-5
vorelax  = 30.e-5
templist = [1]
temptype = 'TREAL'
bin_size = 5 ; bin grille lon-lat en degres
write_ps = 1


; definition bin
binc      = [17.5,33,43,50,59,70] ; categories
binc_nb   = n_elements(binc)-1
binw_size = 10
binw_deb  = 10
binw_fin  = 60
binw_unit = '(m/s)'
binw_nb   = (binw_fin-binw_deb) / binw_size
binp_size = 20
binp_deb  = 910
binp_fin  = 1010
binp_unit = '(hPa)'
binp_nb   = (binp_fin-binp_deb) / binp_size


; declaration
nbcat  = fltarr(3,3,binc_nb)
nbwind = fltarr(3,3,binw_nb)
nbpres = fltarr(3,3,binp_nb)
nbcg   = fltarr(3,3,12)
nbcge  = fltarr(3,3,12)
nbcgw  = fltarr(3,3,12)


; lecture best-track
FOR iexp = 0, n_elements(explist)-1 DO BEGIN
  expn = explist[iexp] & help, expn

  FOR item = 0, n_elements(templist)-1 DO BEGIN
    tempcrit = templist[item]

    IF expn EQ 'IBTRACS' THEN BEGIN
      nbyear = 20 & dateend = '20100101'
      @read_ibtracs.pro
    ENDIF ELSE BEGIN

      IF expn EQ 'FORCED_SW2_KF' THEN period  = '1990-1998' ELSE period  = '1990-2009' & help, period
      IF expn EQ 'FORCED_SW2_KF' THEN dateend = '19990101'  ELSE dateend = '20100101' & help, dateend
      IF expn EQ 'FORCED_SW2_KF' THEN nbyear = 9 ELSE nbyear = 20 & help, nbyear

      ; path + file
      pathin = '/net/adonis/usr/adonis/varclim/gslod/IDL/TRACKER/EXP_'+expn+'/DATA/' & help, pathin
      IF expn EQ 'COUPLED_SW2_BMJ' THEN $
      filein = 'tracker_light_tpot_new_cst_radius_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2) +'_'+period+'.idl'
      IF expn EQ 'COUPLED_SW2_KF' THEN $
      filein = 'tracker_light_TREAL_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2) +'_'+period+'.idl'
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
    yearly = floor(datebeg/10000.) + findgen(nbyear)


    ; index cg par sous bassin
    icg_sio  = where(latcycn[*,0] LT 0. AND latcycn[*,0] GE -25. AND loncycn[*,0] GE 30. AND loncycn[*,0] LE 130., cntcg_sio)
    icg_swi  = where(latcycn[*,0] LT 0. AND latcycn[*,0] GE -25. AND loncycn[*,0] GE 30. AND loncycn[*,0] LE  80., cntcg_swi)
    icg_sei  = where(latcycn[*,0] LT 0. AND latcycn[*,0] GE -25. AND loncycn[*,0] GT 80. AND loncycn[*,0] LE 130., cntcg_sei)
    icg_nio  = where(latcycn[*,0] GT 0. AND latcycn[*,0] LE  25. AND loncycn[*,0] GE 60. AND loncycn[*,0] LT 100., cntcg_nio)
    icg_ars  = where(latcycn[*,0] GT 0. AND latcycn[*,0] LE  25. AND loncycn[*,0] GE 60. AND loncycn[*,0] LT  80., cntcg_ars)
    icg_bob  = where(latcycn[*,0] GT 0. AND latcycn[*,0] LE  25. AND loncycn[*,0] GE 80. AND loncycn[*,0] LT 100., cntcg_bob)


    IF iexp EQ 0 THEN BEGIN
      nbcg_sio_year = fltarr(3,nbyear)
      nbcg_swi_year = fltarr(3,nbyear)
      nbcg_sei_year = fltarr(3,nbyear)
      nbcg_nio_year = fltarr(3,nbyear)
      nbcg_ars_year = fltarr(3,nbyear)
      nbcg_bob_year = fltarr(3,nbyear)
      pdf_sio_year  = fltarr((130-30)/bin_size+1,(0+30)/bin_size+1,nbyear)
    ENDIF

    FOR i = 0, nbyear-1 DO BEGIN
      y = yearly[i]
      iok = [where(yearcycn[icg_sio,0] EQ y-1 AND monthcycn[icg_sio,0] GE 11 AND monthcycn[icg_sio,0] LE 12),where(yearcycn[icg_sio,0] EQ y AND monthcycn[icg_sio,0] GE  1 AND monthcycn[icg_sio,0] LE 4)]
      nbcg_sio_year[iexp,i] = n_elements(where(yearcycn[icg_sio,0] EQ y-1 AND monthcycn[icg_sio,0] GE 11 AND monthcycn[icg_sio,0] LE 12)) + $
                              n_elements(where(yearcycn[icg_sio,0] EQ y   AND monthcycn[icg_sio,0] GE  1 AND monthcycn[icg_sio,0] LE  4))
      nbcg_swi_year[iexp,i] = n_elements(where(yearcycn[icg_swi,0] EQ y-1 AND monthcycn[icg_swi,0] GE 11 AND monthcycn[icg_swi,0] LE 12)) + $
                              n_elements(where(yearcycn[icg_swi,0] EQ y   AND monthcycn[icg_swi,0] GE  1 AND monthcycn[icg_swi,0] LE  4))
      nbcg_sei_year[iexp,i] = n_elements(where(yearcycn[icg_sei,0] EQ y-1 AND monthcycn[icg_sei,0] GE 11 AND monthcycn[icg_sei,0] LE 12)) + $
                              n_elements(where(yearcycn[icg_sei,0] EQ y   AND monthcycn[icg_sei,0] GE  1 AND monthcycn[icg_sei,0] LE  4))
      nbcg_nio_year[iexp,i] = n_elements(where(yearcycn[icg_nio,0] EQ y   AND monthcycn[icg_nio,0] GE  4 AND monthcycn[icg_nio,0] LE 12))
      nbcg_ars_year[iexp,i] = n_elements(where(yearcycn[icg_ars,0] EQ y   AND monthcycn[icg_ars,0] GE  4 AND monthcycn[icg_ars,0] LE 12))
      nbcg_bob_year[iexp,i] = n_elements(where(yearcycn[icg_bob,0] EQ y   AND monthcycn[icg_bob,0] GE  4 AND monthcycn[icg_bob,0] LE 12))
      pdf_sio_year[*,*,i]   =  hist_2D(loncycn[icg_sio[iok],0], latcycn[icg_sio[iok],0], bin1=bin_size, min1=30, max1=130, bin2=bin_size, min2=-30, max2=0)
    ENDFOR
    print, nbcg_sio_year[iexp,*] & stop
    print, 'SIO: ', total(nbcg_sio_year[iexp,*])/float(nbyear)
    print, 'SWI: ', total(nbcg_swi_year[iexp,*])/float(nbyear)
    print, 'SEI: ', total(nbcg_sei_year[iexp,*])/float(nbyear)
    print, 'NIO: ', total(nbcg_nio_year[iexp,*])/float(nbyear)
    print, 'ARS: ', total(nbcg_ars_year[iexp,*])/float(nbyear)
    print, 'BOB: ', total(nbcg_bob_year[iexp,*])/float(nbyear)
    pdf_sio_clim = hist_2D(loncycn[icg_sio,0], latcycn[icg_sio,0], bin1=bin_size, min1=30, max1=130, bin2=bin_size, min2=-30, max2=0) / float(nbyear)
    print, total(pdf_sio_clim),total(pdf_sio_year)/float(nbyear)


    ; IOD years
    IF expn NE 'COUPLED_SW2_BMJ' AND expn NE 'COUPLED_SW2_KF' THEN BEGIN
      iodp_year = [1982,1983,1987,1991,1994,1997,2002]
      iodn_year = [1980,1981,1984,1992,1996,1998]
    ENDIF
    IF expn EQ 'COUPLED_SW2_KF' THEN BEGIN
      iodp_year = [1990,1994,1997,1999,2002]
      iodn_year = [1991,1992,1996,1998,2001]
    ENDIF
    IF expn EQ 'COUPLED_SW2_BMJ' THEN BEGIN
      iodp_year = [1997,2002,2006]
      iodn_year = [1996,1998,2003]
    ENDIF
    year_iodp = intersect(yearly,iodp_year) & print, year_iodp
    year_iodn = intersect(yearly,iodn_year) & print, year_iodn
    nb_iodp   = n_elements(year_iodp)
    nb_iodn   = n_elements(year_iodn)


    ; ENSO years
    ensop_year = [1991,1997,2002,2006];2009]
    enson_year = [1998,1999,2007];,2010]
    year_ensop = intersect(yearly,ensop_year) & print, year_ensop
    year_enson = intersect(yearly,enson_year) & print, year_enson
    nb_ensop   = n_elements(year_ensop)
    nb_enson   = n_elements(year_enson)

    nbcg_sio_iodp = fltarr(nb_iodp)
    nbcg_swi_iodp = fltarr(nb_iodp)
    nbcg_sei_iodp = fltarr(nb_iodp)
    nbcg_sio_iodn = fltarr(nb_iodn)
    nbcg_swi_iodn = fltarr(nb_iodn)
    nbcg_sei_iodn = fltarr(nb_iodn)
    nbcg_sio_ensop = fltarr(nb_ensop)
    nbcg_swi_ensop = fltarr(nb_ensop)
    nbcg_sei_ensop = fltarr(nb_ensop)
    nbcg_sio_enson = fltarr(nb_enson)
    nbcg_swi_enson = fltarr(nb_enson)
    nbcg_sei_enson = fltarr(nb_enson)
    pdf_sio_iodp = fltarr((130-30)/bin_size+1,(0+30)/bin_size+1,nb_iodp)
    pdf_sio_iodn = fltarr((130-30)/bin_size+1,(0+30)/bin_size+1,nb_iodn)
    pdf_sio_ensop = fltarr((130-30)/bin_size+1,(0+30)/bin_size+1,nb_ensop)
    pdf_sio_enson = fltarr((130-30)/bin_size+1,(0+30)/bin_size+1,nb_enson)

    FOR i = 0, nb_iodp-1 DO BEGIN
      y = year_iodp[i]
      iok = [where(yearcycn[icg_sio,0] EQ y   AND monthcycn[icg_sio,0] GE 11 AND monthcycn[icg_sio,0] LE 12),where(yearcycn[icg_sio,0] EQ y+1 AND monthcycn[icg_sio,0] GE  1 AND monthcycn[icg_sio,0] LE  4)]
      nbcg_sio_iodp[i] = n_elements(where(yearcycn[icg_sio,0] EQ y   AND monthcycn[icg_sio,0] GE 11 AND monthcycn[icg_sio,0] LE 12)) + $
                              n_elements(where(yearcycn[icg_sio,0] EQ y+1 AND monthcycn[icg_sio,0] GE  1 AND monthcycn[icg_sio,0] LE  4))
      nbcg_swi_iodp[i] = n_elements(where(yearcycn[icg_swi,0] EQ y   AND monthcycn[icg_swi,0] GE 11 AND monthcycn[icg_swi,0] LE 12)) + $
                              n_elements(where(yearcycn[icg_swi,0] EQ y+1 AND monthcycn[icg_swi,0] GE  1 AND monthcycn[icg_swi,0] LE  4))
      nbcg_sei_iodp[i] = n_elements(where(yearcycn[icg_sei,0] EQ y   AND monthcycn[icg_sei,0] GE 11 AND monthcycn[icg_sei,0] LE 12)) + $
                              n_elements(where(yearcycn[icg_sei,0] EQ y+1 AND monthcycn[icg_sei,0] GE  1 AND monthcycn[icg_sei,0] LE  4))
      pdf_sio_iodp[*,*,i]   = hist_2D(loncycn[icg_sio[iok],0], latcycn[icg_sio[iok],0], bin1=bin_size, min1=30, max1=130, bin2=bin_size, min2=-30, max2=0)
    ENDFOR
    FOR i = 0, nb_iodn-1 DO BEGIN
      y = year_iodn[i]
      iok = [where(yearcycn[icg_sio,0] EQ y   AND monthcycn[icg_sio,0] GE 11 AND monthcycn[icg_sio,0] LE 12),where(yearcycn[icg_sio,0] EQ y+1 AND monthcycn[icg_sio,0] GE  1 AND monthcycn[icg_sio,0] LE  4)]
      nbcg_sio_iodn[i] = n_elements(where(yearcycn[icg_sio,0] EQ y   AND monthcycn[icg_sio,0] GE 11 AND monthcycn[icg_sio,0] LE 12)) + $
                              n_elements(where(yearcycn[icg_sio,0] EQ y+1 AND monthcycn[icg_sio,0] GE  1 AND monthcycn[icg_sio,0] LE  4))
      nbcg_swi_iodn[i] = n_elements(where(yearcycn[icg_swi,0] EQ y   AND monthcycn[icg_swi,0] GE 11 AND monthcycn[icg_swi,0] LE 12)) + $
                              n_elements(where(yearcycn[icg_swi,0] EQ y+1 AND monthcycn[icg_swi,0] GE  1 AND monthcycn[icg_swi,0] LE  4))
      nbcg_sei_iodn[i] = n_elements(where(yearcycn[icg_sei,0] EQ y   AND monthcycn[icg_sei,0] GE 11 AND monthcycn[icg_sei,0] LE 12)) + $
                              n_elements(where(yearcycn[icg_sei,0] EQ y+1 AND monthcycn[icg_sei,0] GE  1 AND monthcycn[icg_sei,0] LE  4))
      pdf_sio_iodn[*,*,i]   = hist_2D(loncycn[icg_sio[iok],0], latcycn[icg_sio[iok],0], bin1=bin_size, min1=30, max1=130, bin2=bin_size, min2=-30, max2=0)
    ENDFOR
    print, 'SIO IODP:', total(nbcg_sio_iodp[*])/float(nb_iodp),total(pdf_sio_iodp)/float(nb_iodp)
    print, 'SWI IODP:', total(nbcg_swi_iodp[*])/float(nb_iodp)
    print, 'SEI IODP:', total(nbcg_sei_iodp[*])/float(nb_iodp)
    print, 'SIO IODN:', total(nbcg_sio_iodn[*])/float(nb_iodn),total(pdf_sio_iodn)/float(nb_iodn)
    print, 'SWI IODN:', total(nbcg_swi_iodn[*])/float(nb_iodn)
    print, 'SEI IODN:', total(nbcg_sei_iodn[*])/float(nb_iodn)
    print, 'SIO IODP-IODN:', total(nbcg_sio_iodp[*])/float(nb_iodp)-total(nbcg_sio_iodn[*])/float(nb_iodn)
    print, 'SWI IODP-IODN:', total(nbcg_swi_iodp[*])/float(nb_iodp)-total(nbcg_swi_iodn[*])/float(nb_iodn)
    print, 'SEI IODP-IODN:', total(nbcg_sei_iodp[*])/float(nb_iodp)-total(nbcg_sei_iodn[*])/float(nb_iodn)

    IF write_ps THEN openps, filename='IOD_CG_ANOMALY_'+expn
      computegrid,30,-30,5,5,21,7
      plt, smooth(total(pdf_sio_iodp,3)/float(nb_iodp),3) - smooth(total(pdf_sio_iodn,3)/float(nb_iodn),3), /nocont, /realcont, xtitle='', ytitle='', title=expn+' CG ANOMALY IODP-IODN', min=-0.4, max=0.4, lct=64
    IF write_ps THEN closeps & STOP



    FOR i = 0, nb_ensop-1 DO BEGIN
      y = year_ensop[i]
      iok = [where(yearcycn[icg_sio,0] EQ y AND monthcycn[icg_sio,0] GE 11 AND monthcycn[icg_sio,0] LE 12),where(yearcycn[icg_sio,0] EQ y+1 AND monthcycn[icg_sio,0] GE  1 AND monthcycn[icg_sio,0] LE  4)]
      nbcg_sio_ensop[i] = n_elements(where(yearcycn[icg_sio,0] EQ y AND monthcycn[icg_sio,0] GE 11 AND monthcycn[icg_sio,0] LE 12)) + $
                               n_elements(where(yearcycn[icg_sio,0] EQ y+1 AND monthcycn[icg_sio,0] GE  1 AND monthcycn[icg_sio,0] LE  4))
      nbcg_swi_ensop[i] = n_elements(where(yearcycn[icg_swi,0] EQ y AND monthcycn[icg_swi,0] GE 11 AND monthcycn[icg_swi,0] LE 12)) + $
                               n_elements(where(yearcycn[icg_swi,0] EQ y+1 AND monthcycn[icg_swi,0] GE  1 AND monthcycn[icg_swi,0] LE  4))
      nbcg_sei_ensop[i] = n_elements(where(yearcycn[icg_sei,0] EQ y AND monthcycn[icg_sei,0] GE 11 AND monthcycn[icg_sei,0] LE 12)) + $
                               n_elements(where(yearcycn[icg_sei,0] EQ y+1 AND monthcycn[icg_sei,0] GE  1 AND monthcycn[icg_sei,0] LE  4))
      pdf_sio_ensop[*,*,i]   = hist_2D(loncycn[icg_sio[iok],0], latcycn[icg_sio[iok],0], bin1=bin_size, min1=30, max1=130, bin2=bin_size, min2=-30, max2=0)
    ENDFOR
    FOR i = 0, nb_enson-1 DO BEGIN
      y = year_enson[i]
      iok = [where(yearcycn[icg_sio,0] EQ y AND monthcycn[icg_sio,0] GE 11 AND monthcycn[icg_sio,0] LE 12),where(yearcycn[icg_sio,0] EQ y+1 AND monthcycn[icg_sio,0] GE  1 AND monthcycn[icg_sio,0] LE  4)]
      nbcg_sio_enson[i] = n_elements(where(yearcycn[icg_sio,0] EQ y AND monthcycn[icg_sio,0] GE 11 AND monthcycn[icg_sio,0] LE 12)) + $
                               n_elements(where(yearcycn[icg_sio,0] EQ y+1 AND monthcycn[icg_sio,0] GE  1 AND monthcycn[icg_sio,0] LE  4))
      nbcg_swi_enson[i] = n_elements(where(yearcycn[icg_swi,0] EQ y AND monthcycn[icg_swi,0] GE 11 AND monthcycn[icg_swi,0] LE 12)) + $
                               n_elements(where(yearcycn[icg_swi,0] EQ y+1 AND monthcycn[icg_swi,0] GE  1 AND monthcycn[icg_swi,0] LE  4))
      nbcg_sei_enson[i] = n_elements(where(yearcycn[icg_sei,0] EQ y AND monthcycn[icg_sei,0] GE 11 AND monthcycn[icg_sei,0] LE 12)) + $
                               n_elements(where(yearcycn[icg_sei,0] EQ y+1 AND monthcycn[icg_sei,0] GE  1 AND monthcycn[icg_sei,0] LE  4))
      pdf_sio_enson[*,*,i]   = hist_2D(loncycn[icg_sio[iok],0], latcycn[icg_sio[iok],0], bin1=bin_size, min1=30, max1=130, bin2=bin_size, min2=-30, max2=0)
    ENDFOR
    print, 'SIO ENSOP:', total(nbcg_sio_ensop[*])/float(nb_ensop),total(pdf_sio_ensop)/float(nb_ensop)
    print, 'SWI ENSOP:', total(nbcg_swi_ensop[*])/float(nb_ensop)
    print, 'SEI ENSOP:', total(nbcg_sei_ensop[*])/float(nb_ensop)
    print, 'SIO ENSON:', total(nbcg_sio_enson[*])/float(nb_enson),total(pdf_sio_enson)/float(nb_enson)
    print, 'SWI ENSON:', total(nbcg_swi_enson[*])/float(nb_enson)
    print, 'SEI ENSON:', total(nbcg_sei_enson[*])/float(nb_enson)
    print, 'SIO ENSOP-ENSON:', total(nbcg_sio_ensop[*])/float(nb_ensop)-total(nbcg_sio_enson[*])/float(nb_enson)
    print, 'SWI ENSOP-ENSON:', total(nbcg_swi_ensop[*])/float(nb_ensop)-total(nbcg_swi_enson[*])/float(nb_enson)
    print, 'SEI ENSOP-ENSON:', total(nbcg_sei_ensop[*])/float(nb_ensop)-total(nbcg_sei_enson[*])/float(nb_enson)

    IF write_ps THEN openps, filename='ENSO_CG_ANOMALY_'+expn
      computegrid,30,-30,5,5,21,7
      plt, smooth(total(pdf_sio_ensop,3)/float(nb_ensop),3) - smooth(total(pdf_sio_enson,3)/float(nb_enson),3), /nocont, /realcont, xtitle='', ytitle='', title=expn+' CG ANOMALY ENSOP-ENSON', min=-0.4, max=0.4, lct=64
    IF write_ps THEN closeps & STOP

  ENDFOR; exp
ENDFOR; temp

stop
END
