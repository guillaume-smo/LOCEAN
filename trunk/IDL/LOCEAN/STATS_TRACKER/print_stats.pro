PRO print_stats


; parametres
explist  = ['IBTRACS','KF']
parlist  = ['COUPLED','FORCED']
basin    = 'SIO'
datebeg  = '19900101'
ucrit    = 17.5
vorcrit  = 30.e-5
vorelax  = 30.e-5
templist = [1]
temptype = 'TREAL'
write_output = 0

; definition bin
binc      = [17.5,33,43,50,59,70]
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

; fichiers sortie
CLOSE, 1,2,3,4
OPENW, 1, 'stats_cat_'+temptype+'.txt'
OPENW, 2, 'stats_wind_'+temptype+'.txt'
OPENW, 3, 'stats_pres_'+temptype+'.txt'
OPENW, 4, 'stats_nbcg_sc_'+temptype+'.txt'

icpt = 0
; lecture best-track
FOR iexp = 0, n_elements(explist)-1 DO BEGIN
  exp = explist[iexp] & help, exp

  FOR ipar = 0, n_elements(parlist)-1 DO BEGIN
    param = parlist[ipar] & help, param
    expname = param + '_SW2_'+ exp & help, expname

    FOR item = 0, n_elements(templist)-1 DO BEGIN
      tempcrit = templist[item]
      IF exp EQ 'IBTRACS' THEN BEGIN
        period  = '1990-2009' & dateend = '20100101' & nbyear = 20
        @read_ibtracs_wmo.pro
      ENDIF ELSE BEGIN
        IF exp EQ 'KF' AND param EQ 'FORCED' THEN period  = '1990-1998' ELSE period  = '1990-2009' & help, period
        IF exp EQ 'KF' AND param EQ 'FORCED' THEN dateend = '19990101'  ELSE dateend = '20100101' & help, dateend
        IF exp EQ 'KF' AND param EQ 'FORCED' THEN nbyear = 9 ELSE nbyear = 20
        pathin = '/net/adonis/usr/adonis/varclim/gslod/IDL/TRACKER/EXP_'+expname+'/DATA/' & help, pathin
        filein = 'tracker_light_'+temptype+'_RVM_NICO_MASK_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2) +'_'+period+'.idl'
        restore, pathin + filein, /VERBOSE
        monthcycn = long(datecycn/100.) - long(datecycn/10000.)*100.
        yearcycn  = long(datecycn/10000.)
        help, loncycn, latcycn, mslpcycn, uv10cycn, datecycn, juldcycn
      ENDELSE

      indcgsio  = where(latcycn[*,0] LE 0. AND latcycn[*,0] GE -30. AND loncycn[*,0] GE 30. AND loncycn[*,0] LE 140., cntcgsio)  

      ; declaration
      maxwind = fltarr(cntcgsio)
      minpres = fltarr(cntcgsio)

      FOR j = 0, cntcgsio-1 DO BEGIN
        maxwind[j] = max(uv10cycn[indcgsio[j],*])
        minpres[j] = min(mslpcycn[indcgsio[j],*])
      ENDFOR

      ; histo
;      FOR j = 0, binw_nb-1 DO BEGIN
;        print, binw_deb+j*binw_size, binw_deb+(j+1)*binw_size
;        nbwind[j] = n_elements(where(maxwind GE binw_deb+j*binw_size AND maxwind LT binw_deb+(j+1)*binw_size))
;      ENDFOR
;      nbwind = nbwind / float(nbyear)

      ; test bootstrap
      var = maxwind
      nbcyc = cntcgsio
      bin_min = binw_deb & help, bin_min
      bin_max = binw_fin & help, bin_max
      bin_size = binw_size & help, bin_size
      bin_nb = binw_nb & help, bin_nb
      @bootstrap.pro
      nbwind[icpt,*,*] = mom_bin / float(nbyear)

      ; histo
;      FOR j = 0, binp_nb-1 DO BEGIN
;        print, binp_deb+j*binp_size, binp_deb+(j+1)*binp_size
;        nbpres[j] = n_elements(where(minpres GE binp_deb+j*binp_size AND minpres LT binp_deb+(j+1)*binp_size))
;      ENDFOR
;      nbpres = nbpres / float(nbyear)

      ; test bootstrap
      var = minpres
      nbcyc = cntcgsio
      bin_min = binp_deb & help, bin_min
      bin_max = binp_fin & help, bin_max
      bin_size = binp_size & help, bin_size
      bin_nb = binp_nb & help, bin_nb
      @bootstrap.pro
      nbpres[icpt,*,*] = mom_bin / float(nbyear)


      ; histo
;      FOR j = 0, binc_nb-2 DO BEGIN
;        print, binc[j],binc[j+1]
;        nbcat[j] = n_elements(where(maxwind GE binc[j] AND maxwind LT binc[j+1]))
;      ENDFOR
;      nbcat = nbcat / float(nbyear)

      ; test bootstrap
      var = maxwind
      nbcyc = cntcgsio
      bin = binc
      bin_nb = binc_nb & help, bin_nb
      @bootstrap_irr.pro
      help, mom_bin
      nbcat[icpt,*,*] = mom_bin / float(nbyear)

      help, icpt
      IF iexp EQ 0 AND ipar EQ 0 THEN icpt = icpt ELSE icpt = icpt + 1

      IF write_output EQ 1 THEN BEGIN
      printf, 1, expname,' ',temptype,' ',tempcrit
      printf, 1, reform(mom_bin_cat[2,*]-mom_bin_cat[0,*]), format = '(5(F5.2,1X))'
      printf, 1, reform(mom_bin_cat[0,*]), format = '(5(F5.2,1X))'
      printf, 1, reform(mom_bin_cat[0,*]-mom_bin_cat[1,*]), format = '(5(F5.2,1X))'
      printf, 1, ''
      printf, 1, reform(mom_bin_cat[2,*]-mom_bin_cat[0,*])/max(mom_bin_cat[0,*]), format = '(5(F5.2,1X))'
      printf, 1, reform(mom_bin_cat[0,*]/max(mom_bin_cat[0,*])), format = '(5(F5.2,1X))'
      printf, 1, reform(mom_bin_cat[0,*]-mom_bin_cat[1,*])/max(mom_bin_cat[0,*]), format = '(5(F5.2,1X))'

      printf, 2, expname,' ',temptype,' ',tempcrit
      printf, 2, reform(mom_bin_wind[2,*]-mom_bin_wind[0,*]), format = '(5(F5.2,1X))'
      printf, 2, reform(mom_bin_wind[0,*]), format = '(5(F5.2,1X))'
      printf, 2, reform(mom_bin_wind[0,*]-mom_bin_wind[1,*]), format = '(5(F5.2,1X))'
      printf, 2, ''
      printf, 2, reform(mom_bin_wind[2,*]-mom_bin_wind[0,*])/max(mom_bin_wind[0,*]), format = '(5(F5.2,1X))'
      printf, 2, reform(mom_bin_wind[0,*])/max(mom_bin_wind[0,*]), format = '(5(F5.2,1X))'    
      printf, 2, reform(mom_bin_wind[0,*]-mom_bin_wind[1,*])/max(mom_bin_wind[0,*]), format = '(5(F5.2,1X))'

      printf, 3, expname,' ',temptype,' ',tempcrit
      printf, 3, reform(mom_bin_pres[2,*]-mom_bin_pres[0,*]), format = '(5(F7.2,1X))'
      printf, 3, reform(mom_bin_pres[0,*]), format = '(5(F7.2,1X))'
      printf, 3, reform(mom_bin_pres[0,*]-mom_bin_pres[1,*]), format = '(5(F7.2,1X))'
      printf, 3, ''
      printf, 3, reform(mom_bin_pres[2,*]-mom_bin_pres[0,*])/max(mom_bin_pres[0,*]), format = '(5(F5.2,1X))'
      printf, 3, reform(mom_bin_pres[0,*])/max(mom_bin_pres[0,*]), format = '(5(F5.2,1X))'
      printf, 3, reform(mom_bin_pres[0,*]-mom_bin_pres[1,*])/max(mom_bin_pres[0,*]), format = '(5(F5.2,1X))'
      ENDIF


      ; cycle saisonnier cg
;      nbcg_sc = histogram(monthcycn[indcgsio,0], min=1, max=12, nbins=12, /NAN) / float(nbyear)
;      print, total(nbcg_sc), n_elements(monthcycn[indcgsio,0]) / float(nbyear)

      ; test bootstrap
;      var = monthcycn[indcgsio,0]
;      nbcyc = cntcgsio
;      bin_min = 1
;      bin_max = 12
;      bin_nb  = 12
;      @bootstrap.pro
;      mom_bin = mom_bin / nbyear

;      printf, 4, expname,' ',temptype,' ',tempcrit
;      printf, 4, nbcg_sc, format = '(12F5.2)'
;      printf, 4, reform(mom_bin[2,*]), format = '(12F5.2)'
;      printf, 4, reform(mom_bin[2,*]-mom_bin[0,*]), format = '(12F5.2)'
;      printf, 4, reform(mom_bin[0,*]), format = '(12F5.2)'
;      printf, 4, reform(mom_bin[1,*]), format = '(12F5.2)'
;      printf, 4, reform(mom_bin[0,*]-mom_bin[1,*]), format = '(12F5.2)'
     

    ENDFOR; par
  ENDFOR; exp
ENDFOR; temp

IF write_output EQ 1 THEN BEGIN
CLOSE, 1
CLOSE, 2
CLOSE, 3
CLOSE, 4
ENDIF

window, 0
FOR i = 0, n_elements(nbcat[0,0,*])-1 DO BEGIN
bar_name = [' ','cat-'+strtrim(i,2),' '] & help, bar_name
bar_plot, nbcat[*,0,i], BACKGROUND=255, BARWIDTH=0.75, BARSPACE=0.25, BASERANGE=0.12, $
BAROFFSET= i*(3*n_elements(nbcat[*,0,0])), OVER=(I GT 0), yrange=[0,15], BARNAMES=bar_name
ENDFOR

window, 1
FOR i = 0, n_elements(nbwind[0,0,*])-1 DO BEGIN
bar_name = [' ',strtrim(binw_deb+i*binw_size,2)+'-'+strtrim(binw_deb+(i+1)*binw_size,2),' ']
bar_plot, nbwind[*,0,i], BACKGROUND=255, BARWIDTH=0.75, BARSPACE=0.25, BASERANGE=0.12, $
BAROFFSET= i*(2.4*n_elements(nbwind[*,0,0])), OVER=(I GT 0), yrange=[0,15], BARNAMES=bar_name
ENDFOR

window, 2
FOR i = 0, n_elements(nbpres[0,0,*])-1 DO BEGIN
bar_name = [' ',strtrim(binp_deb+i*binp_size,2)+'-'+strtrim(binp_deb+(i+1)*binp_size,2),' ']
bar_plot, nbpres[*,0,i], BACKGROUND=255, BARWIDTH=0.75, BARSPACE=0.25, BASERANGE=0.12, $
BAROFFSET= i*(2.4*n_elements(nbpres[*,0,0])), OVER=(I GT 0), yrange=[0,20], BARNAMES=bar_name
ENDFOR

stop

END
