PRO colle_tracks


;-----------------------------------------------------------------------------------------
; PARAMETRES
;-----------------------------------------------------------------------------------------

expname = 'FORCED_SW2_KF' & help, expname
ucrit    = 17.5   & help, ucrit
vorcrit  = 30.e-5 & help, vorcrit
vorelax  = 30.e-5 & help, vorelax
tempcrit = 0.0    & help, tempcrit


path_in = 'EXP_'+ expname +'/DATA/'

momo = ['0106','0712']
FOR y = 1990, 2009 DO BEGIN
year = strtrim(long(y),2)
FOR m = 0, n_elements(momo)-1 DO BEGIN
month = momo[m]
date = year + month & help, date

IF y NE 1999 THEN BEGIN

file_in = 'tracker_TREAL_RVM_NICO_MASK_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2)+'_'+'1990-1994.idl' & help, file_in
file_out = 'tracker_TREAL_RVM_NICO_MASK_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2)+'_1990-1999.idl' & help, file_out

restore, filename = path_in + file_in, /VERBOSE

IF y EQ 1990 AND m EQ 0 THEN BEGIN
  loncyc  = temporary(loncycn)
  latcyc  = temporary(latcycn)
  uv10cyc = temporary(uv10cycn)
  rvmcyc  = temporary(rvmcycn)
  mslpcyc = temporary(mslpcycn)
  vorcyc  = temporary(vorcycn)
  anotcyc = temporary(anotcycn)
  juldcyc = temporary(juldcycn)
  datecyc = temporary(datecycn)
ENDIF ELSE BEGIN
  loncyc  = [ loncyc ,  temporary(loncycn)]
  latcyc  = [ latcyc ,  temporary(latcycn)]
  mslpcyc = [mslpcyc , temporary(mslpcycn)]
  uv10cyc = [uv10cyc , temporary(uv10cycn)]
  rvmcyc  = [rvmcyc  ,  temporary(rvmcycn)]
  vorcyc  = [ vorcyc ,  temporary(vorcycn)]
  anotcyc = [anotcyc , temporary(anotcycn)]
  juldcyc = [juldcyc , temporary(juldcycn)]
  datecyc = [datecyc , temporary(datecycn)]
ENDELSE
  loncycn  = temporary(loncyc)
  latcycn  = temporary(latcyc)
  uv10cycn = temporary(uv10cyc)
  rvmcycn  = temporary(rvmcyc)
  mslpcycn = temporary(mslpcyc)
  vorcycn  = temporary(vorcyc)
  anotcycn = temporary(anotcyc)
  juldcycn = temporary(juldcyc)
  datecycn = temporary(datecyc)

ENDIF
ENDFOR ; month
ENDFOR ; year

save, loncycn, latcycn, mslpcycn, uv10cycn, rvmcycn, vorcycn, anotcycn, juldcycn, datecycn, filename = path_in + file_out, /VERBOSE

END
