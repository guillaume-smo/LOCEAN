
IF flag_z THEN BEGIN

  print, '' & print, 'CALCUL_4D...'

  ; REMISE EN FORME
  IF model EQ 'wrf' OR (model EQ 'now' AND flag_nemo EQ 0) THEN BEGIN
    var4D = REVERSE(TEMPORARY(var), 3)
    e3t   = REVERSE(e3t,   /OVERWRITE) & help, e3t
    gdept = REVERSE(gdept, /OVERWRITE) & help, gdept
  ENDIF ELSE var4D = TEMPORARY(var) & help, var4D

  ; VERIF
  IF force_zmean EQ 0 AND force_zdiff EQ 0 THEN STOP

  ; CALCUL
  IF force_zmean THEN $
  IF zlayer_def[0] NE zlayer_def[1] THEN var = GROSSEMOYENNE(var4D, 'z', BOXZOOM=zlayer_def, /NAN) ELSE var = GROSSEMOYENNE(var4D, 'z', /NAN)
  IF zlayer_def[0] NE zlayer_def[1] AND force_zdiff THEN var = REFORM( var4D[*,*,WHERE(gdept EQ MIN(zlayer_def)),*] - var4D[*,*,WHERE(gdept EQ MAX(zlayer_def)),*] )
  IF zlayer_def[0] EQ zlayer_def[1] THEN var = REFORM(var4D[*,*,WHERE(gdept EQ zlayer_def[0]),*])
  help, var, var4D

  ; CALCUL COMPOSANTES XY
  IF STRMATCH(var_name, 'UV*') THEN BEGIN
    varx4D = REVERSE(TEMPORARY(varx), 3) & help, varx4D
    vary4D = REVERSE(TEMPORARY(vary), 3) & help, vary4D
    IF force_zmean THEN $
    IF zlayer_def[0] NE zlayer_def[1] THEN varx = GROSSEMOYENNE(varx4D, 'z', BOXZOOM=zlayer_def, /NAN) ELSE varx = GROSSEMOYENNE(varx4D, 'z', /NAN)
    IF zlayer_def[0] NE zlayer_def[1] AND force_zdiff THEN varx = REFORM( varx4D[*,*,WHERE(gdept EQ MIN(zlayer_def)),*] - varx4D[*,*,WHERE(gdept EQ MAX(zlayer_def)),*] )
    IF zlayer_def[0] EQ zlayer_def[1] THEN varx = REFORM(varx4D[*,*,WHERE(gdept EQ zlayer_def[0]),*])
    IF force_zmean THEN $
    IF zlayer_def[0] NE zlayer_def[1] THEN vary = GROSSEMOYENNE(vary4D, 'z', BOXZOOM=zlayer_def, /NAN) ELSE vary = GROSSEMOYENNE(vary4D, 'z', /NAN)
    IF zlayer_def[0] NE zlayer_def[1] AND force_zdiff THEN vary = REFORM( vary4D[*,*,WHERE(gdept EQ MIN(zlayer_def)),*] - vary4D[*,*,WHERE(gdept EQ MAX(zlayer_def)),*] )
    IF zlayer_def[0] EQ zlayer_def[1] THEN vary = REFORM(vary4D[*,*,WHERE(gdept EQ zlayer_def[0]),*])
    help, varx, vary
  ENDIF

  print, 'CALCUL_4D OK' & print, ''

ENDIF
