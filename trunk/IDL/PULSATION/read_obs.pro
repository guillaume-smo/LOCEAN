
print, '' & print, 'READ_OBS...'

flag_obs = 1 & help, flag_obs



;-------------------------------------------------------------------------------------------------
; LECTURE DATA OBS
;-------------------------------------------------------------------------------------------------


; FILES+PERIOD DEFINITION & INIT
  @def_obs_data
  @def_obs_time

  ; TC SPECIAL CASE
  IF plot_TC THEN BEGIN
    @read_TC_ibtracs_WMO
  ENDIF

  initncdf, path+file, glam=[20,380], /fullcgrid & domdef, box, /MEMEINDICE


; SAUVEGARDE (NECESSAIRE POUR INTERPOLATION)
  lon_obs   = glamt[firstxt:lastxt,0] & help, lon_obs
  lat_obs   = REFORM(gphit[0,firstyt:lastyt]) & help, lat_obs
  nblon_obs = nxt
  nblat_obs = nyt
  cmd = execute( 'lon_'+STRTRIM(e,2)+'     = glamt[firstxt:lastxt,0] & help, lon_'+STRTRIM(e,2) )
  cmd = execute( 'lat_'+STRTRIM(e,2)+'     = REFORM(gphit[0,firstyt:lastyt]) & help, lat_'+STRTRIM(e,2) )
  cmd = execute( 'grid_'+STRTRIM(e,2)+'    = "obs"' )
  cmd = execute( 'nbmonth_'+STRTRIM(e,2)+' = nbmonth_obs' )


; READ VAR
IF (data_type EQ '1d' OR data_type EQ '6h') AND obs_name EQ 'ERAI' THEN BEGIN
  tmp = !NULL
  FOR y = 0, nbyear_obs-1 DO BEGIN
    year = listyear_obs[y]
    file = var_name+'_'+data_type+'_'+obs_name+'_'+STRTRIM(year,2)+'.nc' & help, file
    @get_obs_data
    tmp = [ tmp, var] & help, tmp
  ENDFOR
  var = TEMPORARY(tmp)
ENDIF ELSE BEGIN
  @get_obs_data
ENDELSE
help, var


; VERIF
IF flag_z EQ 0 THEN BEGIN
  IF data_type EQ '1d' THEN IF n_elements(var[0,0,*]) NE nbday_obs THEN STOP
  IF data_type EQ '1m' THEN IF n_elements(var[0,0,*]) NE nbmonth_obs THEN STOP
  IF data_type EQ '6h' THEN IF n_elements(var[0,0,*]) NE nbdt_obs THEN STOP
ENDIF


; CONVERSIONS
IF var_name EQ 'SST' THEN BEGIN
  var[where(var EQ 35.25)] = !VALUES.F_NAN
  var[where(var EQ 0.   )] = !VALUES.F_NAN
  IF obs_name EQ 'GLORYS2V1' THEN var = var -273.15
ENDIF
IF exp_name EQ 'ERAI' AND var_name EQ 'Q2'  THEN var = var*1000. ; g/kg
IF exp_name EQ 'ERAI' OR exp_name EQ 'ERAIL' AND (var_name EQ 'T2' OR var_name EQ 'SKT') THEN var = var-273.15 ; K -> degC
IF var_name EQ 'PSFC' THEN var = var / 100.
IF var_name EQ 'RAIN' AND data_type EQ '1d' AND obs_name EQ 'ERAI' THEN var = var * 4. * 1000. ; m/6h -> mm/day
IF (var_name EQ 'HFX' OR var_name EQ 'LH' OR var_name EQ 'GSW' OR var_name EQ 'LWR') AND obs_name EQ 'ERAI' AND data_type EQ '1d' THEN var = var / (6. * 3600.)


; VERTICAL CALCUL
  @calcul_vertical


; MASKS
  @def_obs_mask



;-------------------------------------------------------------------------------------------------
; c1m+1m DATA
;-------------------------------------------------------------------------------------------------

IF data_type EQ 'c1m' OR data_type EQ '1m' THEN BEGIN

  ; 2D MASK
  help, force_landmask, force_seamask, force_highmask
  IF flag_mask AND force_landmask THEN FOR m = 0, nbmonth_obs-1 DO var[*,*,m] = var[*,*,m]*landmask
  IF flag_mask AND force_seamask  THEN FOR m = 0, nbmonth_obs-1 DO var[*,*,m] = var[*,*,m]*seamask
  IF               force_highmask THEN FOR m = 0, nbmonth_obs-1 DO var[*,*,m] = var[*,*,m]*highmask
  ;IF var_name EQ 'SKT' THEN FOR m = 0, nbmonth_obs-1 DO var[*,*,m] = var[*,*,m]*seamask

  ; 4D MASK + VERTICAL PROFILE
  IF flag_z THEN BEGIN
    IF flag_mask AND force_landmask THEN FOR m = 0, nbmonth_obs-1 DO FOR z = 0, nzt-1 DO var4D[*,*,z,m] = var4D[*,*,z,m]*landmask
    IF flag_mask AND force_seamask  THEN FOR m = 0, nbmonth_obs-1 DO FOR z = 0, nzt-1 DO var4D[*,*,z,m] = var4D[*,*,z,m]*seamask
    IF               force_highmask THEN FOR m = 0, nbmonth_obs-1 DO FOR z = 0, nzt-1 DO var4D[*,*,z,m] = var4D[*,*,z,m]*highmask
    jpt = nbmonth_obs
    var_zx = GROSSEMOYENNE( var4D, 'y', /NAN)
    var_zy = GROSSEMOYENNE( var4D, 'x', /NAN)
    var_zp = GROSSEMOYENNE( TEMPORARY(var4D), 'xy', /NAN)
    help, var_zp
    IF var_name EQ 'UV' THEN BEGIN
      varx_zp = GROSSEMOYENNE( TEMPORARY(varx4D), 'xy', /NAN)
      vary_zp = GROSSEMOYENNE( TEMPORARY(vary4D), 'xy', /NAN)
      help, varx_zp, vary_zp
    ENDIF
  ENDIF 

  ; SEASONAL AVERAGE
  IF n_elements(ind_period) GT 1 THEN BEGIN
    ind_mean1m = ind_mean1m[ WHERE( ind_mean1m NE -1)]
    var_mean = MEAN( var[*,*,ind_mean1m], DIMENSION=3, /NAN)
    IF STRMATCH(var_name, 'UV*') OR var_name EQ 'STRESS' OR var_name EQ 'RAIN' THEN BEGIN
      varx_mean = MEAN(varx[*,*,ind_mean1m], DIMENSION=3, /NAN)
      vary_mean = MEAN(vary[*,*,ind_mean1m], DIMENSION=3, /NAN)
      IF force_highmask THEN BEGIN
        varx_mean = varx_mean * highmask
        vary_mean = vary_mean * highmask
      ENDIF
      help, varx_mean, vary_mean
    ENDIF
    IF flag_z THEN BEGIN
      var_zp_mean = MEAN( var_zp[*,ind_mean1m], DIMENSION=2, /NAN)
      var_zx_mean = MEAN( var_zx[*,*,ind_mean1m], DIMENSION=3, /NAN)
      var_zy_mean = MEAN( var_zy[*,*,ind_mean1m], DIMENSION=3, /NAN)
      IF var_name EQ 'UV' THEN BEGIN
        varx_zp_mean = MEAN( varx_zp[*,ind_mean1m], DIMENSION=2, /NAN)
        vary_zp_mean = MEAN( vary_zp[*,ind_mean1m], DIMENSION=2, /NAN)
      ENDIF
    ENDIF
  ENDIF ELSE BEGIN
    var_mean = var[*,*,ind_mean1m]
    IF STRMATCH(var_name, 'UV*') OR var_name EQ 'STRESS' OR var_name EQ 'RAIN' THEN BEGIN
      varx_mean = varx[*,*,ind_mean1m]
      vary_mean = vary[*,*,ind_mean1m]
      help, varx_mean, vary_mean
    ENDIF
    IF flag_z THEN BEGIN
      var_zp_mean = var_zp[*,ind_mean1m]
      var_zx_mean = var_zx[*,*,ind_mean1m]
      var_zy_mean = var_zy[*,*,ind_mean1m]
      IF var_name EQ 'UV' THEN BEGIN
        varx_zp_mean = varx_zp[*,ind_mean1m]
        vary_zp_mean = vary_zp[*,ind_mean1m]
      ENDIF
    ENDIF
  ENDELSE
  ano_mean = var_mean - MEAN(var, DIMENSION=3, /NAN)
  help, var_mean, ano_mean
  IF flag_z THEN help, var_zp_mean, var_zx_mean, var_zy_mean

  ; MONTHLY TIME SERIES
  var_ts = FLTARR( nbyear_obs*12) * !VALUES.F_NAN
  ano_ts = FLTARR( nbyear_obs*12) * !VALUES.F_NAN
  FOR m = 0, nbmonth_obs-1 DO var_ts[m] = MEAN( var[*,*,m], /NAN)
  FOR m = 0, nbmonth_obs-1 DO ano_ts[m] = var_ts[m] - MEAN( var_ts, /NAN)
  help, var_ts, ano_ts

  ; MONTHLY STANDARD DEVIATION + SC1M
  IF nbyear_obs GT 1 THEN BEGIN
    var_sdc1m  = FLTARR(12)
    var_sc1m   = FLTARR(12)
    var2D_sc1m = FLTARR( nxt, nyt, 12)
    FOR m = 0, 12-1 DO BEGIN
      ind_month         = INDGEN( nbyear_obs) * 12 + m
      var_sdc1m[m]      = STDDEV( var_ts[ind_month], /NAN)
      var_sc1m[m]       = MEAN(   var_ts[ind_month], /NAN)
      var2D_sc1m[*,*,m] = MEAN(  var[*,*,ind_month], DIMENSION=3, /NAN)
    ENDFOR
  ENDIF ELSE BEGIN
    var_sc1m   = var_ts
    var2D_sc1m = var
    var_sdc1m  = FLTARR( nxt, nyt, 12) * !VALUES.F_NAN
  ENDELSE
  help, var_sdc1m, var_sc1m, var2D_sc1m

  IF data_type EQ '1m' THEN BEGIN
    @calcul_1m_diags
  ENDIF

ENDIF ; c1m+1m DATA



;-------------------------------------------------------------------------------------------------
; 1d + c1d DATA
;-------------------------------------------------------------------------------------------------

IF data_type EQ '1d' OR data_type EQ 'c1d' THEN BEGIN

  ; APPLY MASK
  help, force_landmask, force_seamask
  IF flag_mask AND force_landmask THEN FOR d = 0, nbday_obs-1 DO var[*,*,d] = var[*,*,d] * landmask
  IF flag_mask AND force_seamask  THEN FOR d = 0, nbday_obs-1 DO var[*,*,d] = var[*,*,d] * seamask
  IF force_highmask THEN FOR d = 0, nbday_obs-1 DO var[*,*,d] = var[*,*,d] * highmask

  ; COMPUTE TIME SERIES
  var_ts = fltarr(nbday_obs)*!VALUES.F_NAN
  ano_ts = fltarr(nbday_obs)*!VALUES.F_NAN
  FOR d = 0, nbday_obs-1 DO BEGIN
    var_ts[d] = MEAN(var[*,*,d], /NAN)
    ano_ts[d] = var_ts[d] - MEAN(var_ts, /NAN)
  ENDFOR
  help, var_ts, ano_ts

  ; MEAN 2D
;  IF (fym_only OR fyo_only) AND n_elements(ind_mean1d) GT 1 THEN BEGIN
  IF n_elements(ind_mean1d) GT 1 THEN BEGIN
    var_mean = MEAN(var[*,*,ind_mean1d], DIMENSION=3, /NAN)
    IF STRMATCH(var_name, 'UV*') OR var_name EQ 'STRESS' OR var_name EQ 'RAIN' THEN BEGIN
      varx_mean = MEAN(varx[*,*,ind_mean1d], DIMENSION=3, /NAN)
      vary_mean = MEAN(vary[*,*,ind_mean1d], DIMENSION=3, /NAN)
    ENDIF
  ENDIF ELSE STOP
  IF flag_mask AND force_landmask THEN BEGIN
    var_mean = var_mean * landmask
    IF STRMATCH(var_name, 'UV*') OR var_name EQ 'STRESS' THEN BEGIN
      varx_mean = varx_mean * landmask
      vary_mean = vary_mean * landmask
    ENDIF
  ENDIF
  IF flag_mask AND force_seamask THEN BEGIN
    var_mean = var_mean * seamask
    IF STRMATCH(var_name, 'UV*') OR var_name EQ 'STRESS' THEN BEGIN
      varx_mean = varx_mean * seamask
      vary_mean = vary_mean * seamask
    ENDIF
  ENDIF
  IF force_highmask THEN BEGIN
    var_mean = var_mean * highmask
    IF STRMATCH(var_name, 'UV*') OR var_name EQ 'STRESS' THEN BEGIN
      varx_mean = varx_mean * highmask
      vary_mean = vary_mean * highmask
    ENDIF
  ENDIF
  help, var_mean

  ; DAILY SEASONAL CYCLE (sc1d)
  IF data_type EQ '1d' THEN BEGIN
    var_sc1d = FLTARR(365)
    FOR d = 0, 365-1 DO BEGIN
      ind_day = FINDGEN(nbyear_obs)*365+d
      var_sc1d[d]  = MEAN(var_ts[ind_day], /NAN)
    ENDFOR
  ENDIF ELSE var_sc1d = var_ts
  help, var_sc1d

ENDIF ; 1d DATA


;-------------------------------------------------------------------------------------------------
; 6h DATA
;-------------------------------------------------------------------------------------------------

IF data_type EQ '6h' THEN BEGIN

  ; APPLY MASK + COMPUTE TIME SERIES
  var_ts = fltarr(nbdt_obs)*!VALUES.F_NAN
  ano_ts = fltarr(nbdt_obs)*!VALUES.F_NAN
  help, force_landmask, force_seamask
  IF flag_mask AND force_landmask THEN FOR d = 0, nbdt_obs-1 DO var[*,*,d] = var[*,*,d] * landmask
  IF flag_mask AND force_seamask  THEN FOR d = 0, nbdt_obs-1 DO var[*,*,d] = var[*,*,d] * seamask
  FOR d = 0, nbdt_obs-1 DO var_ts[d] = MEAN(var[*,*,d], /NAN)
  FOR d = 0, nbdt_obs-1 DO ano_ts[d] = var_ts[d] - MEAN(var_ts, /NAN)
  help, var_ts, ano_ts

  ; MEAN 2D
  IF (fym_only OR fyo_only) AND ind_mean6h[0] NE -1 THEN BEGIN
    var_mean = MEAN(var[*,*,ind_mean6h], DIMENSION=3, /NAN)
    IF STRMATCH(var_name, 'UV*') OR var_name EQ 'STRESS' OR var_name EQ 'RAIN' THEN BEGIN
      varx_mean = MEAN(varx[*,*,ind_mean6h], DIMENSION=3, /NAN)
      vary_mean = MEAN(vary[*,*,ind_mean6h], DIMENSION=3, /NAN)
    ENDIF
    IF flag_mask AND force_landmask THEN BEGIN
      var_mean = var_mean * landmask
      IF STRMATCH(var_name, 'UV*') OR var_name EQ 'STRESS' THEN BEGIN
        varx_mean = varx_mean * landmask
        vary_mean = vary_mean * landmask
      ENDIF
    ENDIF
    IF flag_mask AND force_seamask THEN BEGIN
      var_mean = var_mean * seamask
      IF STRMATCH(var_name, 'UV*') OR var_name EQ 'STRESS' THEN BEGIN
        varx_mean = varx_mean * seamask
        vary_mean = vary_mean * seamask
      ENDIF
    ENDIF
    IF flag_mask AND force_highmask THEN BEGIN
      var_mean = var_mean * highmask
      IF STRMATCH(var_name, 'UV*') OR var_name EQ 'STRESS' THEN BEGIN
        varx_mean = varx_mean * highmask
        vary_mean = vary_mean * highmask
      ENDIF
    ENDIF
    help, var_mean
  ENDIF ELSE plot_2D = 0

ENDIF ; 6h DATA

  
print, 'READ_OBS OK' & print, ''
IF debug THEN STOP
