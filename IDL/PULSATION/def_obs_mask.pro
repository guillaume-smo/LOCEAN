
; DECLARATIONS
highmask = FLTARR(nxt, nyt) + 1.


; ERAI MASK
IF obs_name EQ 'ERAI' OR obs_name EQ 'ERAIL' OR obs_name EQ 'ERAI-NEMO' OR obs_name EQ 'ERAI-WRF' THEN BEGIN
  @def_erai_mask
ENDIF

; USE ERAI MASK IF NO OBS MASK
IF obs_name EQ 'GPCP' OR obs_name EQ 'NOAA' OR obs_name EQ 'APHRO050' OR obs_name EQ 'APHRO025' OR obs_name EQ 'GPCC' OR obs_name EQ 'REYNOLDS' THEN BEGIN
  lon = glamt[firstxt:lastxt,firstyt:lastyt]
  lat = gphit[firstxt:lastxt,firstyt:lastyt]
  @def_erai_mask
  print, path+grid_file
  initncdf, path+grid_file, glam=[20,380] & domdef, box, /MEMEINDICES
  landmask = fromreg('bilinear', landmask, glamt[firstxt:lastxt,firstyt:lastyt], gphit[firstxt:lastxt,firstyt:lastyt], lon, lat) & help, landmask
  seamask = fromreg('bilinear', seamask, glamt[firstxt:lastxt,firstyt:lastyt], gphit[firstxt:lastxt,firstyt:lastyt], lon, lat) & help, seamask
  highmask = fromreg('bilinear', highmask, glamt[firstxt:lastxt,firstyt:lastyt], gphit[firstxt:lastxt,firstyt:lastyt], lon, lat) & help, highmask
  initncdf, path+file, glam=[20,380] & domdef, box
ENDIF

; TRMM MASK
IF data_type NE '1d' AND var_name EQ 'RAIN' AND STRMATCH( obs_name, 'TRMM*') THEN BEGIN
  landmask = FLOAT( read_ncdf('mask', filename=path+file, /ALLRECORDS, /NOSTRUCT) )
  seamask  = landmask
  seamask[where(landmask EQ 1.)] = !VALUES.F_NAN
  seamask[where(landmask EQ 0.)] = 1.
  landmask[where(landmask EQ 0.)] = !VALUES.F_NAN
  flag_mask = 1
  help, landmask, seamask
  force_highmask = 0
ENDIF

IF data_type EQ '1d' AND var_name EQ 'RAIN' AND obs_name EQ 'TRMMV6' THEN BEGIN
  landmask = FLOAT(read_ncdf('mask', filename=path+file, /ALLRECORDS, /NOSTRUCT))
  seamask  = landmask
  seamask[where(landmask EQ 1.)] = !VALUES.F_NAN
  seamask[where(landmask EQ 0.)] = 1.
  landmask[where(landmask EQ 0.)] = !VALUES.F_NAN
  flag_mask = 1
  help, landmask, seamask
  force_highmask = 0
ENDIF

;IF data_type EQ '1d' AND var_name EQ 'RAIN' AND obs_name EQ 'TRMMV7' THEN BEGIN
IF var_name EQ 'RAIN' AND obs_name EQ 'TRMMV7' THEN BEGIN
  initncdf, path+file, glam=[20,380]
  landmask = FLOAT(read_ncdf('mask', filename=path+file, /ALLRECORDS, /NOSTRUCT))
  domdef, box
  IF FIRSTXT-4*20 LT 0 THEN landmask = [landmask[JPI+(FIRSTXT-4*20):JPI-1,FIRSTYT:LASTYT],landmask[0:lastxt-4*20,FIRSTYT:LASTYT]] $
  ELSE landmask = landmask[JPI-FIRSTXT-4*20-1:JPI-1,FIRSTYT:LASTYT]
  ;landmask = landmask[FIRSTXT-4*20-1:LASTXT-4*20-1,FIRSTYT:LASTYT]
  seamask  = landmask
  seamask[where(landmask EQ 1.)] = !VALUES.F_NAN
  seamask[where(landmask EQ 0.)] = 1.
  landmask[where(landmask EQ 0.)] = !VALUES.F_NAN
  flag_mask = 1
  help, landmask, seamask
  force_highmask = 0
ENDIF

; DEFINITION GPCC MASK
IF var_name EQ 'RAIN' AND obs_name EQ 'GPCC' THEN BEGIN
  seamask = var[*,*,0]
  seamask[WHERE(finite(var[*,*,0]) EQ 1)] = 1.
  landmask = var[*,*,0] * !VALUES.F_NAN
ENDIF

; DEFINITION MLD+BLT CDBM MASK
IF var_name EQ 'MLDT' OR var_name EQ 'MLDR' OR var_name EQ 'BLT' THEN BEGIN
  landmask = read_ncdf('mask', filename=path+file, /ALLRECORDS, /NOSTRUCT)
  landmask[where(landmask EQ 0.)] = !VALUES.F_NAN
  flag_mask = 1
  help, landmask
ENDIF

; CERSAT
IF STRMATCH( var_name, '*UV10*') AND obs_name EQ 'CERSAT-QSCAT' THEN BEGIN
  landmask = var[*,*,0] * 0. + 1.
  landmask[ WHERE( FINITE( var[*,*,0]) EQ 0)] = !VALUES.F_NAN
  help, landmask
ENDIF

; D20 SODA
IF var_name EQ 'D20' AND obs_name EQ 'SODA 2.2.4' THEN BEGIN
  landmask = FLTARR( nxt, nyt) * 0. + 1.
  landmask[ WHERE( var[*,*,0] EQ 0)] = !VALUES.F_NAN
  help, landmask
ENDIF
