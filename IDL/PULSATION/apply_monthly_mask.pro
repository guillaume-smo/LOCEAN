help, force_landmask, force_seamask, force_highmask
IF data_type EQ 'c1m' THEN jpt = 12


; CAS 2D
IF SIZE(var, /N_DIMENSIONS) EQ 3 OR force_zmean THEN BEGIN

  IF force_landmask THEN FOR m = 0, jpt-1 DO var[*,*,m] = var[*,*,m] * landmask
  IF force_seamask  THEN FOR m = 0, jpt-1 DO var[*,*,m] = var[*,*,m] * seamask
  IF force_highmask AND flag_nemo EQ 0 THEN FOR m = 0, jpt-1 DO var[*,*,m] = var[*,*,m] * highmask

  IF force_landmask EQ 0 AND var_name EQ 'SST' THEN FOR m = 0, jpt-1 DO var[*,*,m] = var[*,*,m] * landmask

ENDIF
