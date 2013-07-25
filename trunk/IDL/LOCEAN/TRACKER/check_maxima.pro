PRO check_maxima

tempcrit = 1
temptype = 'tpot'
expn = 'FORCED_SW2_KF'
path = '/net/adonis/usr/adonis/varclim/gslod/IDL/TRACKER/EXP_'+expn+'/DATA/'
file = 'maxima_'+temptype+'_new_cst_radius_u17_v30_t10_19900101-20091231.idl'

restore, filename=path+file, /verbose

month = long(timedate/100.) - long(timedate/10000.)*100.
year  = long(timedate/10000.)

isio = where(loncyc GE 30. AND loncyc LE 130. AND latcyc GE -30. AND latcyc LE  0., cntsio)
inio = where(loncyc GE 30. AND loncyc LE 130. AND latcyc GE   0. AND latcyc LE 25., cntnio)
iall  = [isio,inio]
;itmp = where(anomtcyc GE temp, cntemp)

FOR i = 1990,2009 DO BEGIN
  iy  = where(year EQ i, cnty) & help, iy
  iok = intersect(iy,iall)
  IF iok[0] NE -1 THEN print, i, n_elements(where(finite(loncyc[iok,*]) EQ 1))
ENDFOR

initncdf, '/usr/home/gslod/IDL/MASK_WRF_IND4_GRIDT.nc'
domdef, [30,130,-30,30]
@cm_4mesh
SET_PLOT, 'X'
lct,33
DEVICE, decomposed=0, retain=0
plt, glamt, /nodata, /realcont, title=expn+' - number of detected '+temptype+' maxima: '+strtrim(n_elements(loncyc[iall]),2)
oplot, loncyc[iall], latcyc[iall], psym=3, color=0


stop
END
