PRO recolle_maxima


;-----------------------------------------------------------------------------------------
; PARAMETRES
;-----------------------------------------------------------------------------------------

expname = 'COUPLED_SW2_KF' & help, expname
ucrit    = 17.5   & help, ucrit
vorcrit  = 30.e-5 & help, vorcrit
;tempcrit = 0.5    & help, tempcrit
templist = [0.]


;-----------------------------------------------------------------------------------------
; BOUCLES
;-----------------------------------------------------------------------------------------

FOR toto = 0, n_elements(templist)-1 DO BEGIN
tempcrit = templist[toto]  & help, tempcrit

path_in  = 'EXP_'+ expname +'/DATA_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'/' & help, path_in
file_out = 'maxima_RVM_FAB_MASK_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_1990-2009.idl' & help, file_out

momo = ['0106','0712']
FOR y = 1990, 2009 DO BEGIN
year = strtrim(long(y),2)
FOR m = 0, n_elements(momo)-1 DO BEGIN
month = momo[m]
date = year + month & help, date

file_in = 'maxima_RVM_FAB_MASK_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_'+ strtrim(long(date),2)+'.idl'

restore, filename = path_in + file_in, /VERBOSE

IF y EQ 1990 AND m EQ 0 THEN BEGIN
  loncyco  = temporary(loncyc)
  latcyco  = temporary(latcyc)
  uv10cyco = temporary(uv10cyc)
  rvmcyco  = temporary(rvmcyc)
  mslpcyco = temporary(mslpcyc)
  vorcyco  = temporary(vorcyc)
  anomtcyco = temporary(anomtcyc)
  timejuldo = temporary(timejuld)
  timedateo = temporary(timedate)
ENDIF ELSE BEGIN
  loncyco  = [ loncyco ,  temporary(loncyc)]
  latcyco  = [ latcyco ,  temporary(latcyc)]
  mslpcyco = [mslpcyco , temporary(mslpcyc)]
  uv10cyco = [uv10cyco , temporary(uv10cyc)]
  rvmcyco  = [rvmcyco  ,  temporary(rvmcyc)]
  vorcyco  = [ vorcyco ,  temporary(vorcyc)]
  anomtcyco = [anomtcyco , temporary(anomtcyc)]
  timejuldo = [timejuldo , temporary(timejuld)]
  timedateo = [timedateo , temporary(timedate)]
ENDELSE

ENDFOR; mois
ENDFOR; annee

loncyc  = temporary(loncyco)
latcyc  = temporary(latcyco)
uv10cyc = temporary(uv10cyco)
rvmcyc  = temporary(rvmcyco)
mslpcyc = temporary(mslpcyco)
vorcyc  = temporary(vorcyco)
anomtcyc = temporary(anomtcyco)
timejuld = temporary(timejuldo)
timedate = temporary(timedateo)

save, loncyc, latcyc, mslpcyc, uv10cyc, rvmcyc, vorcyc, anomtcyc, timejuld, timedate, filename = path_in + file_out, /VERBOSE

ENDFOR; critere temp

END
