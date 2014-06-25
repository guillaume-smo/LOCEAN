; FIND VARPLOT MIN & MAX

print, '' & print, 'MIN/MAX PLOT:'


;-------------------------------------------------------------------------------------------------
; FIRST EXP DEFINITION
;-------------------------------------------------------------------------------------------------

IF STRMATCH(var_plot, '*err*') OR $
   STRMATCH(var_plot, 'ratio_rain*') OR $
   STRMATCH(var_plot, 'rain*period*ratio') THEN ebeg = 1 ELSE ebeg = 0
IF STRMATCH(var_plot, 'diff*') THEN ebeg = 2
IF STRMATCH(var_plot, 'cor2D1m*') THEN ebeg = 1
IF STRMATCH(var_plot, 'maxcor*') THEN ebeg = 1


;-------------------------------------------------------------------------------------------------
; EXPS LOOP
;-------------------------------------------------------------------------------------------------

minvar = !NULL & maxvar = !NULL

FOR e = ebeg, n_elements(exp_list) -1 DO BEGIN

  cmd = execute( 'tmp = '+var_plot+'_'+strtrim(e,2) )

  IF STRMATCH(var_plot, '*ts*', /FOLD_CASE) THEN BEGIN
    cmd = execute( 'help, '+var_plot+'_'+strtrim(e,2) )
    cmd = execute( 'tfin = MIN( [ tend, n_elements('+var_plot+'_'+strtrim(e,2)+')-1] )' )
    IF smooth_coef THEN cmd = execute( 'tmp = TS_SMOOTH('+var_plot+'_'+strtrim(e,2)+'[tbeg:tfin], smooth_coef)' ) $
                   ELSE cmd = execute( 'tmp = '+var_plot+'_'+strtrim(e,2)+'[tbeg:tfin]' )
  ENDIF

  IF STRMATCH(var_plot, '*sc1d*', /FOLD_CASE) THEN BEGIN
    cmd = execute( 'help, '+var_plot+'_'+strtrim(e,2) )
    cmd = execute( 'tfin = MIN( [ tend, n_elements('+var_plot+'_'+strtrim(e,2)+')-1] )' )
    cmd = execute( 'tmp = '+var_plot+'_'+strtrim(e,2)+'[tbeg:tfin]' )
  ENDIF

  IF STRMATCH(var_plot, '*zp*', /FOLD_CASE) THEN BEGIN
    cmd = execute( 'zbeg = WHERE( z_'+strtrim(e,2)+' EQ MAX(zrange))' )
    cmd = execute( 'zend = WHERE( z_'+strtrim(e,2)+' EQ MIN(zrange))' )
    IF zbeg EQ -1 OR zend EQ -1 THEN STOP
    IF zbeg LT zend THEN cmd = execute( 'tmp = '+var_plot+'_'+strtrim(e,2)+'[zbeg:zend]' ) ELSE cmd = execute( 'tmp = '+var_plot+'_'+strtrim(e,2)+'[zend:zbeg]' )
  ENDIF

  IF STRMATCH(var_plot, '???_mean*', /FOLD_CASE) OR STRMATCH(var_plot, 'var?_mean*', /FOLD_CASE) THEN BEGIN
    IF e EQ 0 THEN cmd = execute( 'tmp = '+var_plot+'_'+strtrim(e,2) )
    IF e GT 0 THEN cmd = execute( 'tmp = '+var_plot+'_'+strtrim(e,2)+'_gridobs' )
  ENDIF

  IF STRMATCH(var_plot, 'rat*_mean*', /FOLD_CASE)  THEN cmd = execute( 'tmp = '+var_plot+'_'+strtrim(e,2) )
  IF STRMATCH(var_plot, 'var_?mean*', /FOLD_CASE)  THEN cmd = execute( 'tmp = '+var_plot+'_'+strtrim(e,2) )
  IF STRMATCH(var_plot, 'err*', /FOLD_CASE)  THEN cmd = execute( 'tmp = '+var_plot+'_'+strtrim(e,2) )
  IF STRMATCH(var_plot, 'diffgrad', /FOLD_CASE)     THEN cmd = execute( 'tmp = '+var_plot+'_'+strtrim(e,2) )
  IF STRMATCH(var_plot, 'var_??_mean', /FOLD_CASE) THEN cmd = execute( 'tmp = '+var_plot+'_'+strtrim(e,2) )
  IF STRMATCH(var_plot, 'cor2D1m*', /FOLD_CASE) THEN cmd = execute( 'tmp = '+var_plot+'_'+strtrim(e,2) )
  IF STRMATCH(var_plot, 'delta_*', /FOLD_CASE) THEN cmd = execute( 'tmp = '+var_plot+strtrim(e,2) )
  IF STRMATCH(var_plot, 'vardt_*', /FOLD_CASE) THEN cmd = execute( 'tmp = '+var_plot+'_'+strtrim(e,2) )
  IF STRMATCH(var_plot, 'maxcor_*', /FOLD_CASE) THEN cmd = execute( 'tmp = '+var_plot+strtrim(e,2) )

  ;help, minvar, maxvar, tmp
  minvar = MIN([ minvar, MIN( tmp, /NAN)], /NAN)
  maxvar = MAX([ maxvar, MAX( tmp, /NAN)], /NAN)

ENDFOR


;-------------------------------------------------------------------------------------------------
; ADD 5% TO MIN & MAX VALUES
;-------------------------------------------------------------------------------------------------

IF STRMATCH(var_plot, '*ts*') OR STRMATCH(var_plot, '*sc1*') OR $
   STRMATCH(var_plot, '*zonmean*') OR STRMATCH(var_plot, '*_rain') OR $
   STRMATCH(var_plot, '*_xmean') OR STRMATCH(var_plot, 'cor2D1m*') OR $
   STRMATCH(var_plot, 'delta*') THEN BEGIN
  maxvar = maxvar + 0.05*(maxvar-minvar)
  IF minvar NE 0. THEN minvar = minvar - 0.05*(maxvar-minvar)
ENDIF
IF STRMATCH(var_plot, 'var_iso*', /FOLD_CASE) THEN minvar = 0.
IF STRMATCH(var_plot, 'var_syn*', /FOLD_CASE) THEN minvar = 0.


;-------------------------------------------------------------------------------------------------
; setup interval number
;-------------------------------------------------------------------------------------------------

intvar=(maxvar-minvar)/20. 


;-------------------------------------------------------------------------------------------------
; ERROR PLOTS (SYMETRIC)
;-------------------------------------------------------------------------------------------------

IF STRMATCH( var_plot, 'err*') THEN BEGIN
  IF ABS(minvar) GT maxvar THEN maxvar = ABS(minvar) ELSE minvar = -1. * maxvar
  intvar = ( maxvar + ABS(minvar) ) / 20.
  lct, 64
ENDIF

IF STRMATCH(var_plot, 'ano_*') OR STRMATCH(var_plot, 'delta_*') OR STRMATCH(var_plot, 'vardt_*') THEN BEGIN
  minvar = -1.*MAX([ABS(minvar),maxvar])
  maxvar = MAX([ABS(minvar),maxvar])
  lct, 64
ENDIF

;-------------------------------------------------------------------------------------------------
; EXCEPTIONS
;-------------------------------------------------------------------------------------------------

; 2D MEAN PLOTS
IF var_plot EQ 'var_mean' THEN BEGIN
  IF var_name EQ 'STRESS' THEN BEGIN & fmt='(F7.2)' & vref = 0.15 & minvar = 0. & maxvar = 0.18 & intvar = 0.01 & lct,60 & ENDIF
  IF var_name EQ 'SST' THEN BEGIN & minvar = 20 & maxvar = 31 & intvar = 0.5 & lct, 33 & ENDIF
  IF var_name EQ 'THETAO' THEN BEGIN & minvar = 18 & maxvar = 30 & intvar = 0.5 & lct, 33 & ENDIF
  IF var_name EQ 'UV10' THEN BEGIN & minvar = 0. & maxvar = 12. & intvar = 0.5 & vref=12 & ENDIF
  IF var_name EQ 'UV' THEN BEGIN & minvar = 0. & maxvar = 20. & intvar = 1. & vref=12 & ENDIF
  IF var_name EQ 'PSFC' THEN BEGIN & minvar = 1000. & maxvar = 1025. & intvar = 1. & fmt='(F6.1)' & ENDIF
  IF var_name EQ 'MSLP' THEN BEGIN & minvar = 990. & maxvar = 1026. & intvar = 2. & fmt='(F6.1)' & lct,33 & ENDIF
  IF var_name EQ 'GRAD_MSLP' THEN BEGIN & minvar = 0. & maxvar = 15. & intvar = 1. & ENDIF
  IF var_name EQ 'RAIN' THEN BEGIN
    IF grid EQ 'ind025' THEN BEGIN
      IF period EQ 'DJFM' THEN BEGIN minvar = 0 & maxvar = 20 & intvar = 2 & lct,15 & ENDIF
      IF period EQ 'JJAS' THEN BEGIN minvar = 0 & maxvar = 30 & intvar = 3 & lct,15 & ENDIF
    ENDIF ELSE BEGIN minvar = 0 & maxvar = 25 & intvar = 1 & ENDELSE
  ENDIF
  IF STRMATCH( var_name, '*rot*', /FOLD_CASE) THEN BEGIN minvar = -30  & maxvar = 30  & intvar=(maxvar-minvar)/20. & lct,64 & ENDIF
ENDIF
IF var_plot EQ 'ano_mean' THEN lct, 56 ;64

IF var_plot EQ 'var_ft1d' THEN BEGIN
  IF var_name EQ 'RAIN' THEN BEGIN
    minvar = 0 & intvar = 2
  ENDIF
ENDIF

IF var_plot EQ 'stddev_varyear' THEN BEGIN
  IF var_name EQ 'RAIN' THEN BEGIN
    minvar = 0 & maxvar = 5 & intvar = 0.25
  ENDIF
  IF var_name EQ 'D20' THEN BEGIN
    minvar = 0 & maxvar = 20 & intvar = 1
  ENDIF
ENDIF

IF var_plot EQ 'div'  AND (var_name EQ 'UV10' OR var_name EQ 'UV') THEN BEGIN minvar = -8  & maxvar = 8  & intvar=1. & lct,64 & ENDIF
IF var_plot EQ 'div'  AND var_name EQ 'STRESS' THEN BEGIN minvar = -0.5 & maxvar = 0.5 & intvar=(maxvar-minvar)/20. & ENDIF
IF var_plot EQ 'rot'  AND (var_name EQ 'UV10' OR var_name EQ 'UV')  THEN BEGIN minvar = -30  & maxvar = 30  & intvar=(maxvar-minvar)/20. & lct,64 & ENDIF
IF var_plot EQ 'rot'  AND var_name EQ 'STRESS' THEN BEGIN minvar = -0.8 & maxvar = 0.8 & intvar=(maxvar-minvar)/20. & ENDIF
IF var_plot EQ 'grad' THEN BEGIN minvar = 0. & maxvar = 10. & intvar=0.5 & ENDIF
IF var_plot EQ 'errgrad'  THEN BEGIN minvar = -3. & maxvar = 3. & intvar=0.25 & lct,64 & ENDIF
IF var_plot EQ 'errdiv' AND STRMATCH( var_name, 'UV*') THEN BEGIN minvar = -10 & maxvar = 10 & intvar=1. & lct,64 & ENDIF
IF var_plot EQ 'diffgrad' THEN BEGIN minvar = -3. & maxvar = 3. & intvar=0.25 & lct,64 & ENDIF

IF var_name EQ 'WQ' THEN BEGIN
  IF var_plot EQ 'var_*mean' THEN BEGIN
    maxvar =  0.25
    minvar = -0.25
    intvar = 0.025
  ENDIF
ENDIF

IF var_name EQ 'W' THEN BEGIN 
  IF var_plot EQ 'var_zx_mean' THEN BEGIN
    maxvar =  0.02
    minvar = -0.02
    intvar = 0.002
  ENDIF
  IF var_plot EQ 'var_mean' THEN BEGIN
    maxvar =  0.02
    minvar = -0.02
    intvar = 0.002
  ENDIF
ENDIF

IF STRMATCH( var_plot, 'lag_*') THEN BEGIN
  minvar = -9.5
  maxvar =  9.5
  intvar =  1.
ENDIF

IF var_plot EQ 'err_zx_mean' AND var_name EQ 'GHT' THEN BEGIN & minvar = -10. & maxvar = 10. & intvar = 1. & ENDIF
IF STRMATCH( var_plot, 'var_*') AND var_name EQ 'D20' THEN BEGIN & minvar = 50 & maxvar = 150 & intvar = 5 & lct, 33 & ENDIF
IF STRMATCH( var_plot, 'ano_*') AND var_name EQ 'D20' THEN BEGIN & minvar = -40 & maxvar = 40 & intvar = 5 & lct, 64 & ENDIF

IF STRMATCH( var_plot, 'nbcg*') THEN BEGIN
  minvar = 0
  maxvar = 15
  intvar = 1
ENDIF

IF STRMATCH( var_plot, 'nbtcday*') THEN BEGIN
  minvar = 0
  maxvar = 160
  intvar = 20
ENDIF

IF var_name EQ 'SST' AND var_plot EQ 'stddev_anoyearsc1m' THEN BEGIN
  minvar = 0
  maxvar = 0.7
  intvar = 0.05
ENDIF

print, 'MIN:', minvar, ' MAX:', maxvar, ' INT:', intvar & print, ''
