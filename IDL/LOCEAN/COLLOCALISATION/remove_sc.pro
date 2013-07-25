PRO remove_sc
@all_cm

expname = 'COUPLED_SW2_BMJ'
nbyear = 20

filein = 'ci_daily_1990-2009.nc' & help, filein
pathin = '/Volumes/TIME_MACHINE/EXP_'+expname+'/NEMO/CI/' & help, pathin
initncdf, pathin+filein

var_year = fltarr(nbyear, nxt, nyt, 366) + !values.f_nan & help, var_year

FOR i = 0, nbyear-1 DO BEGIN

  year = 1990 + i & help, year
  nbdayt = 0

  FOR j = 1990, year DO BEGIN
  IF j EQ 1992 OR j EQ 1996 OR j EQ 2000 OR j EQ 2004 OR j EQ 2008 THEN nbday = 366 ELSE nbday = 365
  nbdayt = nbdayt + nbday
  ENDFOR
  
  tbeg = nbdayt-nbday & help, tbeg
  tend = nbdayt-1 & help, tend
  var = read_ncdf('ci',tbeg,tend,/timestep,filename=pathin+filein,/nostruct) & help, var
  var_year[i,*,*,0:nbday-1] = temporary(var)
ENDFOR

END
