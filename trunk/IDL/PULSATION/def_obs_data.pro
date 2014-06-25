

; flag_mask: data need to be masked
; flag_z   : 4D data

var_file  = var_name
IF var_name EQ 'THETAO' OR var_name EQ 'SO' THEN model = 'nemo'
IF (WHERE( STRMATCH( exp_list, 'tr075*', /FOLD_CASE) EQ 1))[0] NE -1 THEN grid = 'trop075'
IF (WHERE( STRMATCH( exp_list, 'tr025*', /FOLD_CASE) EQ 1))[0] NE -1 THEN grid = 'trop025'
IF (WHERE( STRMATCH( exp_list, 'tr12*' , /FOLD_CASE) EQ 1))[0] NE -1 THEN grid = 'trop12'

path = '/Users/guillaumesamson/WORK/DATASETS/' & help, path


;-------------------------------------------------------------------------------------------------
; ATMOSPHERIC & SURFACE OBSERVATION DATASETS DEFINITIONS
;-------------------------------------------------------------------------------------------------

CASE var_name OF

  'HEIGHT': BEGIN
    IF data_type EQ 'c1m' THEN BEGIN
      exp_name  = 'ERAI'
      obs_name  = 'ERAI'
      flag_mask = 1
      yearini_obs = 1989 & yearend_obs = 2009
      var_file = 'height'
      file = 'HEIGHT_ERAII.nc'
    ENDIF
  END

  ; GEOPOTENTIAL HEIGHT
  'GHT': BEGIN
    IF data_type EQ 'c1m' OR data_type EQ '1m' THEN BEGIN
      exp_name  = 'ERAI'
      obs_name  = 'ERAI'
      flag_mask = 1
      flag_z    = 1
      yearini_obs = 1989 & yearend_obs = 2009
      var_file = 'geopt'
      file = var_file+'_'+data_type+'_'+STRTRIM(yearini_obs,2)+'-'+STRTRIM(yearend_obs,2)+'.apmei.GLOBAL_075.nc'
    ENDIF ELSE STOP
  END

  'U': BEGIN
    IF data_type EQ 'c1m' OR data_type EQ '1m' THEN BEGIN
      exp_name  = 'ERAI'
      obs_name  = 'ERAI'
      flag_mask = 1
      flag_z    = 1
      yearini_obs = 1989 & yearend_obs = 2009
      var_file = 'u'
      file = 'uv_'+data_type+'_'+STRTRIM(yearini_obs,2)+'-'+STRTRIM(yearend_obs,2)+'.apmei.GLOBAL_075.nc'
    ENDIF ELSE STOP
  END

  'V': BEGIN
    IF data_type EQ 'c1m' OR data_type EQ '1m' THEN BEGIN
      exp_name  = 'ERAI'
      obs_name  = 'ERAI'
      flag_mask = 1
      flag_z    = 1
      yearini_obs = 1989 & yearend_obs = 2009
      var_file = 'v'
      file = 'uv_'+data_type+'_'+STRTRIM(yearini_obs,2)+'-'+STRTRIM(yearend_obs,2)+'.apmei.GLOBAL_075.nc'
    ENDIF ELSE STOP
  END

  'UV': BEGIN
    IF data_type EQ 'c1m' OR data_type EQ '1m' THEN BEGIN
      exp_name  = 'ERAI'
      obs_name  = 'ERAI'
      flag_mask = 1
      flag_z    = 1
      yearini_obs = 1989 & yearend_obs = 2009
      file = 'uv_'+data_type+'_'+STRTRIM(yearini_obs,2)+'-'+STRTRIM(yearend_obs,2)+'.apmei.GLOBAL_075.nc'
    ENDIF ELSE STOP
  END

  'T': BEGIN
    IF data_type EQ 'c1m' OR data_type EQ '1m' THEN BEGIN
      exp_name  = 'ERAI'
      obs_name  = 'ERAI'
      flag_mask = 1
      flag_z    = 1
      yearini_obs = 1989 & yearend_obs = 2009
      var_file = 'ta'
      file = var_file+'_'+data_type+'_'+STRTRIM(yearini_obs,2)+'-'+STRTRIM(yearend_obs,2)+'.apmei.GLOBAL_075.nc'
    ENDIF ELSE STOP
  END

  'Q': BEGIN
    IF data_type EQ 'c1m' OR data_type EQ '1m' THEN BEGIN
      exp_name  = 'ERAI'
      obs_name  = 'ERAI'
      flag_mask = 1
      flag_z    = 1
      yearini_obs = 1989 & yearend_obs = 2009
      var_file = 'q'
      file = var_file+'_'+data_type+'_'+STRTRIM(yearini_obs,2)+'-'+STRTRIM(yearend_obs,2)+'.apmei.GLOBAL_075.nc'
    ENDIF ELSE STOP
  END

  'UVQ': BEGIN
    IF data_type EQ 'c1m' OR data_type EQ '1m' THEN BEGIN
      exp_name  = 'ERAI'
      obs_name  = 'ERAI'
      flag_mask = 1
      flag_z    = 1
      yearini_obs = 1989 & yearend_obs = 2009
      var_file = 'q'
      file = var_file+'_'+data_type+'_'+STRTRIM(yearini_obs,2)+'-'+STRTRIM(yearend_obs,2)+'.apmei.GLOBAL_075.nc'
    ENDIF ELSE STOP
  END

  'W': BEGIN
    IF data_type EQ 'c1m' OR data_type EQ '1m' THEN BEGIN
      exp_name  = 'ERAI'
      obs_name  = 'ERAI'
      flag_mask = 1
      flag_z    = 1
      yearini_obs = 1989 & yearend_obs = 2009
      var_file = 'w'
      file = var_file+'_'+data_type+'_'+STRTRIM(yearini_obs,2)+'-'+STRTRIM(yearend_obs,2)+'.apmei.GLOBAL_075.nc'
    ENDIF ELSE STOP
  END

  'WQ': BEGIN
    IF data_type EQ 'c1m' OR data_type EQ '1m' THEN BEGIN
      exp_name  = 'ERAI'
      obs_name  = 'ERAI'
      flag_mask = 1
      flag_z    = 1
      yearini_obs = 1989 & yearend_obs = 2009
      var_file = 'q'
      file = var_file+'_'+data_type+'_'+STRTRIM(yearini_obs,2)+'-'+STRTRIM(yearend_obs,2)+'.apmei.GLOBAL_075.nc'
    ENDIF ELSE STOP
  END

  'TPOT': BEGIN
    IF data_type EQ 'c1m' OR data_type EQ '1m' THEN BEGIN
      exp_name  = 'ERAI'
      obs_name  = 'ERAI'
      flag_mask = 1
      flag_z    = 1
      yearini_obs = 1989 & yearend_obs = 2009
      var_file = 'ta'
      file = var_file+'_'+data_type+'_'+STRTRIM(yearini_obs,2)+'-'+STRTRIM(yearend_obs,2)+'.apmei.GLOBAL_075.nc'
    ENDIF ELSE STOP
  END

  'RHOD': BEGIN
    IF data_type EQ 'c1m' OR data_type EQ '1m' THEN BEGIN
      exp_name  = 'ERAI'
      obs_name  = 'ERAI'
      flag_mask = 1
      flag_z    = 1
      yearini_obs = 1989 & yearend_obs = 2009
      var_file = 'ta'
      file = var_file+'_'+data_type+'_'+STRTRIM(yearini_obs,2)+'-'+STRTRIM(yearend_obs,2)+'.apmei.GLOBAL_075.nc'
    ENDIF ELSE STOP
  END

  'RHO': BEGIN
    IF data_type EQ 'c1m' OR data_type EQ '1m' THEN BEGIN
      exp_name  = 'ERAI'
      obs_name  = 'ERAI'
      flag_mask = 1
      flag_z    = 1
      yearini_obs = 1989 & yearend_obs = 2009
      var_file = 'ta'
      file = var_file+'_'+data_type+'_'+STRTRIM(yearini_obs,2)+'-'+STRTRIM(yearend_obs,2)+'.apmei.GLOBAL_075.nc'
    ENDIF ELSE STOP
  END

  'TPOTE': BEGIN
    IF data_type EQ 'c1m' OR data_type EQ '1m' THEN BEGIN
      exp_name  = 'ERAI'
      obs_name  = 'ERAI'
      flag_mask = 1
      flag_z    = 1
      yearini_obs = 1989 & yearend_obs = 2009
      var_file = 'ta'
      file = var_file+'_'+data_type+'_'+STRTRIM(yearini_obs,2)+'-'+STRTRIM(yearend_obs,2)+'.apmei.GLOBAL_075.nc'
    ENDIF ELSE STOP
  END

  'RH': BEGIN
    IF data_type EQ '1m' OR data_type EQ 'c1m' THEN BEGIN
      exp_name  = 'ERAI'
      obs_name  = 'ERAI'
      flag_mask = 1
      flag_z    = 1
      yearini_obs = 1989 & yearend_obs = 2009
      file = var_name+'_'+data_type+'_'+exp_name+'_'+STRTRIM(yearini_obs,2)+'-'+STRTRIM(yearend_obs,2)+'.nc'
    ENDIF ELSE STOP
  END

  ; SEA LEVEL PRESSURE
  'MSLP': BEGIN
    IF data_type EQ 'c1m' OR data_type EQ '1m' THEN BEGIN
      exp_name  = 'ERAI'
      obs_name  = 'ERAI'
      flag_mask = 1
      yearini_obs = 1989
      yearend_obs = 2009
      ;IF data_type EQ 'c1m' THEN yearend_obs = 2011
      ;IF data_type EQ '1m' THEN yearend_obs = 2009
      file = 'PSFC_'+data_type+'_'+exp_name+'_'+STRTRIM(yearini_obs,2)+'-'+STRTRIM(yearend_obs,2)+'.nc'
    ENDIF ELSE STOP
  END

  'GRAD_MSLP': BEGIN
    IF data_type EQ 'c1m' OR data_type EQ '1m' THEN BEGIN
      exp_name  = 'ERAI'
      obs_name  = 'ERAI'
      flag_mask = 1
      yearini_obs = 1989
      yearend_obs = 2009
      ;IF data_type EQ 'c1m' THEN yearend_obs = 2011
      ;IF data_type EQ '1m' THEN yearend_obs = 2009
      file = 'PSFC_'+data_type+'_'+exp_name+'_'+STRTRIM(yearini_obs,2)+'-'+STRTRIM(yearend_obs,2)+'.nc'
    ENDIF ELSE STOP
  END


  ; SURFACE PRESSURE
  'PSFC': BEGIN
    obs_name  = 'ERAI'
    flag_mask = 1
    yearini_obs = 1989 & yearend_obs = 2009
    var_file = 'sp'
    IF data_type NE '1d' THEN file = var_name+'_'+data_type+'_'+obs_name+'_'+STRTRIM(yearini_obs,2)+'-'+STRTRIM(yearend_obs,2)+'.nc' $
    ELSE file = var_name+'_'+data_type+'_'+obs_name+'_'+STRTRIM(yearini_obs,2)+'.nc'
    IF data_type EQ '1d' THEN var_file = 'sshf'
  END

  'ALBEDO': BEGIN
    exp_name  = 'ERAI'
    obs_name  = 'ERAI'
    flag_mask = 1
    yearini_obs = 1989 & yearend_obs = 2009
    file = var_name+'_'+data_type+'_'+exp_name+'_'+STRTRIM(yearini_obs,2)+'-'+STRTRIM(yearend_obs,2)+'.nc'
  END


  'RAIN': BEGIN

    ; dataset selection
    IF exp_name EQ 'OBS' THEN BEGIN
      IF data_type EQ 'c1m' THEN exp_name = 'APHRO025' ; TRMM / APHRO025-050
      IF force_seamask EQ 0 AND data_type EQ '1m'  THEN exp_name = 'TRMM'
      IF data_type EQ '1d'  AND force_seamask EQ 0 THEN exp_name = 'TRMM' ; GPCP / TRMM / ERAI
      IF force_seamask AND zone NE 'AIMRR' AND data_type NE '1d' THEN exp_name = 'GPCC'
      IF force_seamask AND data_type EQ '1d' THEN exp_name = 'TRMM'
      IF (zone EQ 'AIMRR' OR zone EQ 'IND') AND force_seamask AND  grid EQ 'trop075' THEN exp_name = 'APHRO050'
      ;IF (zone EQ 'AIMRR' OR zone EQ 'IND') AND force_seamask AND (grid EQ 'trop025' OR grid EQ 'trop12') THEN exp_name = 'APHRO025'
    ENDIF
    exp_name = 'TRMM'

    ; APHRODITE
    IF exp_name EQ 'APHRO025' OR exp_name EQ 'APHRO050' THEN BEGIN
      obs_name = exp_name
      yearini_obs = 1989
      yearend_obs = 2007
      file = var_name+'_'+data_type+'_'+obs_name+'_'+STRTRIM(yearini_obs,2)+'-'+STRTRIM(yearend_obs,2)+'.nc'
      flag_mask = 0
      var_file  = 'precip'
    ENDIF

    ; TRMM 3B42
    IF exp_name EQ 'TRMM' THEN BEGIN
      ;IF data_type EQ '1d' THEN BEGIN
      ;  yearini_obs = 1998 & yearend_obs = 2009
      ;  file = var_name+'_'+data_type+'_TRMM3B42V6_'+STRTRIM(yearini_obs,2)+'-'+STRTRIM(yearend_obs,2)+'.nc'
      ;  var_file = 'precipitation'
      ;ENDIF
      yearini_obs = 1998 & yearend_obs = 2009
      ;IF data_type EQ '1d' THEN yearend_obs = 1998
      file = var_name+'_'+data_type+'_TRMM3B42V7_'+STRTRIM(yearini_obs,2)+'-'+STRTRIM(yearend_obs,2)+'.nc'
      var_file = 'r'
      obs_name  = 'TRMMV7'
      flag_mask = 1
    ENDIF
    IF exp_name EQ 'TRMMV6' THEN BEGIN
      IF data_type EQ '1d' THEN BEGIN
        yearini_obs = 1998 & yearend_obs = 2009
        file = var_name+'_'+data_type+'_TRMM3B42V6_'+STRTRIM(yearini_obs,2)+'-'+STRTRIM(yearend_obs,2)+'.nc'
        var_file = 'precipitation'
        obs_name  = 'TRMMV6'
        flag_mask = 1
      ENDIF
    ENDIF
    IF exp_name EQ 'TRMMV7' THEN BEGIN
      IF data_type EQ '1d' THEN BEGIN
        yearini_obs = 1998 & yearend_obs = 2009
        file = var_name+'_'+data_type+'_TRMM3B42V7_'+STRTRIM(yearini_obs,2)+'-'+STRTRIM(yearend_obs,2)+'.nc'
        var_file = 'r'
        obs_name  = 'TRMMV7'
        flag_mask = 1
      ENDIF
    ENDIF

    ; GPCP
    IF exp_name EQ 'GPCP' THEN BEGIN
      obs_name = 'GPCP'
      flag_mask = 1
      IF data_type EQ '1m' THEN BEGIN
        yearini_obs = 1979 & yearend_obs = 2010
        file = var_name+'_'+data_type+'_'+obs_name+'_'+STRTRIM(yearini_obs,2)+'-'+STRTRIM(yearend_obs,2)+'.nc'
      ENDIF
      IF data_type EQ '1d' THEN BEGIN
        yearini_obs = 1997 & yearend_obs = 2008
        file = var_name+'_'+data_type+'_'+obs_name+'_'+STRTRIM(yearini_obs,2)+'-'+STRTRIM(yearend_obs,2)+'.nc'
        var_file = 'data'
      ENDIF
    ENDIF

    ; GPCC
    IF exp_name EQ 'GPCC' THEN BEGIN
      IF data_type EQ 'c1m' THEN yearini_obs = 1981 ELSE yearini_obs = 1901
      yearend_obs = 2010
      obs_name = 'GPCC'
      var_file = 'precip'
      flag_mask = 0
      file = var_name+'_'+data_type+'_'+obs_name+'_'+STRTRIM(yearini_obs,2)+'-'+STRTRIM(yearend_obs,2)+'.nc'
    ENDIF
    IF data_type EQ '1d' AND exp_name EQ 'ERAI' THEN BEGIN
      obs_name = 'ERAI'
      yearini_obs = 1989 & yearend_obs = 1989
      file = var_name+'_'+data_type+'_'+obs_name+'_'+STRTRIM(yearini_obs,2)+'.nc'
      var_file = 'rain'
      flag_mask = 1
    ENDIF

  END


  'SST': BEGIN

    ; manual dataset selection
    exp_name = 'REYNOLDS'

    ; REYNOLDS
    IF exp_name EQ 'REYNOLDS' THEN BEGIN
      IF data_type EQ '1m' THEN BEGIN
        obs_name = 'REYNOLDS'
        var_file = 'sst'
        yearini_obs = 1982 & yearend_obs = 2013
        file = var_name+'_'+data_type+'_'+obs_name+'_'+strtrim(yearini_obs,2)+'-'+strtrim(yearend_obs,2)+'.nc'
        flag_mask = 1
      ENDIF ELSE STOP
    ENDIF

    ; TROPFLUX
    IF exp_name EQ 'TROPFLUX' THEN BEGIN
      IF data_type EQ 'c1m' THEN BEGIN
        file = var_name+'_'+data_type+'_TROPFLUX_1979-2011.nc'
        yearini_obs = 1979 & yearend_obs = 2011
      ENDIF ELSE BEGIN
        path = '/ccc/store/cont005/ra0542/berthets/Data/TROPFLUX/'+data_type+'/'
        var_file = 'sst'
        yearini_obs = 1979 & IF data_type EQ '1d' THEN yearend_obs = 2012 ELSE yearend_obs = 2011
        file = var_file+'_tropflux_'+data_type+'_'+strtrim(yearini_obs,2)+'_'+strtrim(yearend_obs,2)+'.nc'
      ENDELSE
      obs_name = 'TROPFLUX'
    ENDIF

    ; AVHRR
    IF exp_name EQ 'AVHRR' THEN BEGIN 
      file = 'SST_'+data_type+'_AVHRR_1989-2011.nc'
      yearini_obs = 1989 & yearend_obs = 2011
      obs_name = 'AVHRR'
    ENDIF

    ; REMSS MW
    IF exp_name EQ 'REMSS-MW' THEN BEGIN
      yearini_obs = 2003 & yearend_obs = 2010
      obs_name    = 'REMSS-MW'
      file        = var_name+'_'+data_type+'_'+exp_name+'_'+strtrim(yearini_obs,2)+'-'+strtrim(yearend_obs,2)+'.nc'
      var_file    = 'sst'
      flag_mask = 0
    ENDIF

    ; CERSAT MW (IO ONLY)
    IF exp_name EQ 'CERSAT-MW' THEN BEGIN
      yearini_obs = 1998
      yearend_obs = 2009
      file = var_name+'_'+data_type+'_'+exp_name+'_'+strtrim(yearini_obs,2)+'-'+strtrim(yearend_obs,2)+'_IO.nc'
      obs_name = 'CERSAT-MW'
      flag_mask = 1
    ENDIF

    ; GLORYS 2V1
    IF exp_name EQ 'GLORYS2V1' THEN BEGIN
      yearini_obs = 1993 & yearend_obs = 2009
      obs_name    = 'GLORYS2V1'
      file        = var_name+'_'+data_type+'_'+exp_name+'_'+strtrim(yearini_obs,2)+'-'+strtrim(yearend_obs,2)+'.nc'
      var_file    = 'votemper'
      flag_mask = 0
    ENDIF

  END


  'SKINTEMP': BEGIN
    IF exp_name EQ 'TROPFLUX' THEN BEGIN
      IF data_type EQ 'c1m' THEN BEGIN
        var_file = 'SST'
        file = var_file+'_'+data_type+'_TROPFLUX_1979-2011.nc'
        yearini_obs = 1979 & yearend_obs = 2011
      ENDIF ELSE BEGIN
        path = '/ccc/store/cont005/ra0542/berthets/Data/TROPFLUX/'+data_type+'/'
        var_file = 'sst'
        yearini_obs = 1979 & IF data_type EQ '1d' THEN yearend_obs = 2012 ELSE yearend_obs = 2011
        file = var_file+'_tropflux_'+data_type+'_'+strtrim(yearini_obs,2)+'_'+strtrim(yearend_obs,2)+'.nc'
        yearini_obs = 1979 & yearend_obs = 2012
      ENDELSE
      obs_name = 'TROPFLUX'
    ENDIF
    IF exp_name EQ 'AVHRR' THEN BEGIN
      var_file = 'SST'
      file = var_file+'_'+data_type+'_AVHRR_1989-2011.nc'
      yearini_obs = 1989 & yearend_obs = 2011
      obs_name = 'AVHRR'
    ENDIF
    IF exp_name EQ 'REMSS-MW' THEN BEGIN
      IF data_type EQ 'c1m' THEN yearini_obs = 2003 ELSE yearini_obs = 2002
      var_file = 'SST'
      file = var_file+'_'+data_type+'_REMSS-MW_'+strtrim(yearini_obs,2)+'-2011.nc'
      yearini_obs = 2003 & yearend_obs = 2011
      obs_name = 'REMSS-MW'
    ENDIF
    flag_mask = 0
  END

  'SKINTEMPMEAN': BEGIN
    IF exp_name EQ 'TROPFLUX' THEN BEGIN
      IF data_type EQ 'c1m' THEN BEGIN
        var_file = 'SST'
        file = var_file+'_'+data_type+'_TROPFLUX_1979-2011.nc'
        yearini_obs = 1979 & yearend_obs = 2011
      ENDIF ELSE BEGIN
        path = '/ccc/store/cont005/ra0542/berthets/Data/TROPFLUX/'+data_type+'/'
        var_file = 'sst'
        yearini_obs = 1979 & IF data_type EQ '1d' THEN yearend_obs = 2012 ELSE yearend_obs = 2011
        file = var_file+'_tropflux_'+data_type+'_'+strtrim(yearini_obs,2)+'_'+strtrim(yearend_obs,2)+'.nc'
        yearini_obs = 1979 & yearend_obs = 2012
      ENDELSE
      obs_name = 'TROPFLUX'
    ENDIF
    IF exp_name EQ 'AVHRR' THEN BEGIN
      var_file = 'SST'
      file = var_file+'_'+data_type+'_AVHRR_1989-2011.nc'
      yearini_obs = 1989 & yearend_obs = 2011
      obs_name = 'AVHRR'
    ENDIF
    IF exp_name EQ 'REMSS-MW' OR exp_name EQ 'OBS' THEN BEGIN
      IF data_type EQ 'c1m' THEN yearini_obs = 2003 ELSE yearini_obs = 2002
      var_file = 'SST'
      file = var_file+'_'+data_type+'_REMSS-MW_'+strtrim(yearini_obs,2)+'-2011.nc'
      yearini_obs = 2003 & yearend_obs = 2011
      obs_name = 'REMSS-MW'
    ENDIF
    flag_mask = 0
  END

  'DELTAT': BEGIN
    IF exp_name EQ 'OBS' OR exp_name EQ 'TROPFLUX' THEN BEGIN
      IF data_type EQ 'c1m' OR data_type EQ '1m' THEN BEGIN
        file = var_name+'_'+data_type+'_TROPFLUX_1979-2011.nc'
        yearini_obs = 1979 & yearend_obs = 2011
      ENDIF ELSE STOP
      flag_mask = 0
      obs_name = 'TROPFLUX'
    ENDIF
    IF exp_name EQ 'ERAI' THEN BEGIN
      IF data_type EQ 'c1m' OR data_type EQ '1m' THEN BEGIN
        file = var_name+'_'+data_type+'_ERAI_1989-2009.nc'
        yearini_obs = 1989 & yearend_obs = 2009
      ENDIF ELSE STOP
      flag_mask = 1
      obs_name = 'ERAI'
    ENDIF
  END

  'DELTAQ': BEGIN
    exp_name = 'TROPFLUX'
    IF exp_name EQ 'TROPFLUX' THEN BEGIN
      IF data_type EQ 'c1m' OR data_type EQ '1m' THEN BEGIN
        file = var_name+'_'+data_type+'_TROPFLUX_1979-2011.nc'
        yearini_obs = 1979 & yearend_obs = 2011
      ENDIF ELSE BEGIN
        yearini_obs = 1979 & IF data_type EQ '1d' THEN yearend_obs = 2012 ELSE yearend_obs = 2011
        file = var_file+'_'+data_type+'_'+exp_name+'_'+strtrim(yearini_obs,2)+'-'+strtrim(yearend_obs,2)+'.nc'
      ENDELSE
      flag_mask = 0
      obs_name = 'TROPFLUX'
    ENDIF
    IF exp_name EQ 'ERAI' THEN BEGIN
      IF data_type EQ 'c1m' OR data_type EQ '1m' THEN BEGIN
        file = var_name+'_'+data_type+'_ERAI_1989-2009.nc'
        yearini_obs = 1989 & yearend_obs = 2009
      ENDIF ELSE STOP
      flag_mask = 1
      obs_name = 'ERAI'
    ENDIF
  END

  'UV10Q2': BEGIN
    obs_name = 'ERAI'
    flag_mask = 1
    IF data_type EQ '1m' OR data_type EQ 'c1m' THEN BEGIN
      yearini_obs = 1989 & yearend_obs = 2011
      file = 'UV10_'+data_type+'_'+obs_name+'_'+STRTRIM(yearini_obs,2)+'-'+STRTRIM(yearend_obs,2)+'.nc'
    ENDIF 
  END

  'UV10': BEGIN
    IF exp_name EQ 'OBS' AND force_seamask THEN exp_name = 'ERAI'
    IF exp_name EQ 'OBS' AND force_landmask THEN exp_name = 'CERSAT-QSCAT' ; CERSAT-QSCAT / TROPFLUX
    IF exp_name EQ 'OBS' AND force_landmask EQ 0 AND force_seamask EQ 0 THEN exp_name = 'ERAI'
    ;exp_name = 'ERAI'
    help, exp_name

    IF exp_name EQ 'CERSAT-QSCAT' THEN BEGIN
      file = 'UV10_'+data_type+'_CERSAT-QSCAT_2000-2008.nc'
      yearini_obs = 2000 & yearend_obs = 2008
      flag_mask = 0
      obs_name = 'CERSAT-QSCAT'
    ENDIF

    IF exp_name EQ 'ERAI' THEN BEGIN
      obs_name = 'ERAI'
      flag_mask = 1
      IF data_type EQ '1m' OR data_type EQ 'c1m' THEN BEGIN
        yearini_obs = 1989 & yearend_obs = 2011
        file = var_name+'_'+data_type+'_'+obs_name+'_'+STRTRIM(yearini_obs,2)+'-'+STRTRIM(yearend_obs,2)+'.nc'
      ENDIF
      IF data_type EQ '6h' OR data_type EQ '1d' THEN BEGIN
        yearini_obs = 1989 & yearend_obs = 2009
        file = var_name+'_'+data_type+'_'+obs_name+'_'+STRTRIM(yearini_obs,2)+'.nc'
      ENDIF
    ENDIF

    IF exp_name EQ 'TROPFLUX' THEN BEGIN
      IF data_type NE 'c1m' THEN path = '/ccc/store/cont005/ra0542/berthets/Data/TROPFLUX/'+data_type+'/'
      var_file = 'ws'
      yearini_obs = 1979
      IF data_type EQ '1m' THEN yearend_obs = 2011
      IF data_type EQ '1d' THEN yearend_obs = 2012
      file = var_file+'_tropflux_'+data_type+'_'+strtrim(yearini_obs,2)+'_'+strtrim(yearend_obs,2)+'.nc'
      flag_mask = 0
      obs_name = 'TROPFLUX'
    ENDIF

  END

  'ROT_UV10': BEGIN
    IF exp_name EQ 'OBS' AND force_seamask THEN exp_name = 'ERAI'
    IF exp_name EQ 'OBS' AND force_landmask THEN exp_name = 'CERSAT-QSCAT' ; CERSAT-QSCAT / TROPFLUX
    IF exp_name EQ 'OBS' AND force_landmask EQ 0 AND force_seamask EQ 0 THEN exp_name = 'ERAI'
    ;exp_name = 'ERAI'
    help, exp_name

    IF exp_name EQ 'CERSAT-QSCAT' THEN BEGIN
      file = 'UV10_'+data_type+'_CERSAT-QSCAT_2000-2008.nc'
      yearini_obs = 2000 & yearend_obs = 2008
      flag_mask = 0
      obs_name = 'CERSAT-QSCAT'
    ENDIF

    IF exp_name EQ 'ERAI' THEN BEGIN
      obs_name = 'ERAI'
      flag_mask = 1
      IF data_type EQ '1m' OR data_type EQ 'c1m' THEN BEGIN
        yearini_obs = 1989 & yearend_obs = 2011
        file = var_name+'_'+data_type+'_'+obs_name+'_'+STRTRIM(yearini_obs,2)+'-'+STRTRIM(yearend_obs,2)+'.nc'
      ENDIF
      IF data_type EQ '6h' OR data_type EQ '1d' THEN BEGIN
        yearini_obs = 1989 & yearend_obs = 2009
        file = var_name+'_'+data_type+'_'+obs_name+'_'+STRTRIM(yearini_obs,2)+'.nc'
      ENDIF
    ENDIF

    IF exp_name EQ 'TROPFLUX' THEN BEGIN
      IF data_type NE 'c1m' THEN path = '/ccc/store/cont005/ra0542/berthets/Data/TROPFLUX/'+data_type+'/'
      var_file = 'ws'
      yearini_obs = 1979
      IF data_type EQ '1m' THEN yearend_obs = 2011
      IF data_type EQ '1d' THEN yearend_obs = 2012
      file = var_file+'_tropflux_'+data_type+'_'+strtrim(yearini_obs,2)+'_'+strtrim(yearend_obs,2)+'.nc'
      flag_mask = 0
      obs_name = 'TROPFLUX'
    ENDIF

  END

  'U10': BEGIN
    exp_name = 'ERAI'
    IF exp_name EQ 'CERSAT-QSCAT' THEN BEGIN
      file = 'UV10_'+data_type+'_CERSAT-QSCAT_2000-2008.nc'
      yearini_obs = 2000 & yearend_obs = 2008
      flag_mask = 0
      obs_name = 'CERSAT-QSCAT'
    ENDIF
    IF exp_name EQ 'ERAI' THEN BEGIN
      file = 'UV10_'+data_type+'_ERAI_1989-2011.nc'
      yearini_obs = 1989 & yearend_obs = 2011
      flag_mask = 1
      obs_name = 'ERAI'
    ENDIF
    IF exp_name EQ 'TROPFLUX' THEN STOP
  END

  'STRESS': BEGIN
    file = 'UV10_'+data_type+'_CERSAT-QSCAT_2000-2008.nc'
    yearini_obs = 2000 & yearend_obs = 2008
    obs_name  = 'CERSAT-QSCAT'
    flag_mask = 0
  END

  'T2'  : BEGIN

    IF exp_name EQ 'OBS' AND force_seamask  THEN exp_name = 'ERAI'
    IF exp_name EQ 'OBS' AND force_landmask THEN exp_name = 'TROPFLUX'
    IF exp_name EQ 'OBS' AND force_landmask EQ 0 AND force_seamask EQ 0 THEN exp_name = 'ERAI'

    IF exp_name EQ 'TROPFLUX' THEN BEGIN
      IF data_type EQ 'c1m' THEN BEGIN
        file = 'T2_'+data_type+'_TROPFLUX_1979-2011.nc'
        yearini_obs = 1979 & yearend_obs = 2011
      ENDIF ELSE BEGIN
        path = '/ccc/store/cont005/ra0542/berthets/Data/TROPFLUX/'+data_type+'/'
        var_file = 't2m'
        yearini_obs = 1979 & IF data_type EQ '1d' THEN yearend_obs = 2012 ELSE yearend_obs = 2011
        file = var_file+'_tropflux_'+data_type+'_'+strtrim(yearini_obs,2)+'_'+strtrim(yearend_obs,2)+'.nc'
        yearini_obs = 1979 & yearend_obs = 2012
      ENDELSE
      obs_name = 'TROPFLUX'
      flag_mask = 0
    ENDIF

    IF exp_name EQ 'ERAI' THEN BEGIN
      obs_name = 'ERAI'
      flag_mask = 1
      IF data_type EQ '1m' OR data_type EQ 'c1m'  THEN BEGIN
        yearini_obs = 1989 & yearend_obs = 2011
        file = var_name+'_'+data_type+'_'+obs_name+'_'+STRTRIM(yearini_obs,2)+'-'+STRTRIM(yearend_obs,2)+'.nc'
      ENDIF
      IF data_type EQ '6h' OR data_type EQ '1d'  THEN BEGIN
        var_file = 't2'
        yearini_obs = 1989 & yearend_obs = 2009
        file = var_name+'_'+data_type+'_'+obs_name+'_'+STRTRIM(yearini_obs,2)+'.nc'
      ENDIF
    ENDIF 

  END

  'THETAE': BEGIN
    IF exp_name EQ 'OBS' THEN exp_name = 'ERAI'
    IF exp_name EQ 'ERAI' THEN BEGIN
      obs_name = 'ERAI'
      flag_mask = 1
      yearini_obs = 1989 & yearend_obs = 2009
      IF data_type EQ '1m' OR data_type EQ 'c1m' THEN file = 'T2_'+data_type+'_'+obs_name+'_'+STRTRIM(yearini_obs,2)+'-'+STRTRIM(yearend_obs,2)+'.nc'
      IF data_type EQ '6h' OR data_type EQ '1d'  THEN file = 'T2_'+data_type+'_'+obs_name+'_'+STRTRIM(yearini_obs,2)+'.nc'
    ENDIF
  END


  'Q2'  : BEGIN

    IF exp_name EQ 'OBS' AND force_seamask THEN exp_name = 'ERAI'
    IF exp_name EQ 'OBS' AND force_landmask THEN exp_name = 'TROPFLUX'
    IF exp_name EQ 'OBS' AND force_landmask EQ 0 AND force_seamask EQ 0 THEN exp_name = 'ERAI'

    IF exp_name EQ 'TROPFLUX' THEN BEGIN
      IF data_type EQ 'c1m' THEN BEGIN
        file = 'Q2_'+data_type+'_TROPFLUX_1979-2011.nc'
        yearini_obs = 1979 & yearend_obs = 2011
      ENDIF ELSE BEGIN
        path = '/ccc/store/cont005/ra0542/berthets/Data/TROPFLUX/'+data_type+'/'
        var_file = 'q2m'
        yearini_obs = 1979 & IF data_type EQ '1d' THEN yearend_obs = 2012 ELSE yearend_obs = 2011
        file = var_file+'_tropflux_'+data_type+'_'+strtrim(yearini_obs,2)+'_'+strtrim(yearend_obs,2)+'.nc'
        yearini_obs = 1979 & yearend_obs = 2012
      ENDELSE
      flag_mask = 0
      obs_name = 'TROPFLUX'
    ENDIF

    IF exp_name EQ 'ERAI' THEN BEGIN
      obs_name = 'ERAI'
      flag_mask = 1
      yearini_obs = 1989 & yearend_obs = 2011
      IF data_type EQ '1m' OR data_type EQ 'c1m' THEN file = var_name+'_'+data_type+'_'+obs_name+'_'+STRTRIM(yearini_obs,2)+'-'+STRTRIM(yearend_obs,2)+'.nc'
      IF data_type EQ '6h' OR data_type EQ '1d'  THEN file = var_name+'_'+data_type+'_'+obs_name+'_'+STRTRIM(yearini_obs,2)+'.nc'
    ENDIF

  END

  'SKT' : BEGIN
    IF exp_name EQ 'OBS' AND force_seamask AND data_type EQ '1d' THEN exp_name = 'APHRO' ELSE $
    IF exp_name EQ 'OBS' THEN exp_name = 'ERAI'
    IF exp_name EQ 'ERAI' THEN BEGIN
      yearini_obs = 1989 & yearend_obs = 2009
      flag_mask = 1
      obs_name = 'ERAI'
      IF data_type EQ '6h' OR data_type EQ '1d' THEN file = var_name+'_'+data_type+'_'+obs_name+'_'+STRTRIM(yearini_obs,2)+'.nc' $
      ELSE file = var_name+'_'+data_type+'_'+obs_name+'_'+STRTRIM(yearini_obs,2)+'-'+STRTRIM(yearend_obs,2)+'.nc'
      IF data_type EQ '6h' OR data_type EQ '1d' THEN var_file = 'skt'
    ENDIF
    IF exp_name EQ 'APHRO' THEN BEGIN
      obs_name = 'APHRO'
      yearini_obs = 1989
      yearend_obs = 2007
      file = var_name+'_'+data_type+'_'+obs_name+'_'+STRTRIM(yearini_obs,2)+'-'+STRTRIM(yearend_obs,2)+'.nc'
      flag_mask = 0
      var_file  = 'tave'
    ENDIF
    IF exp_name EQ 'ERAIL' THEN BEGIN
      obs_name = 'ERAIL'
      yearini_obs = 1989
      yearend_obs = 1999
      file = var_name+'_'+data_type+'_'+obs_name+'_'+STRTRIM(yearini_obs,2)+'-'+STRTRIM(yearend_obs,2)+'.nc'
      flag_mask = 1
      var_file  = 'skt'
    ENDIF
  END

  'GSW' : BEGIN
    IF exp_name EQ 'OBS' AND force_seamask  THEN exp_name = 'ERAI'
    IF exp_name EQ 'OBS' AND force_landmask THEN exp_name = 'TROPFLUX' ELSE exp_name = 'ERAI'

    IF exp_name EQ 'TROPFLUX'  THEN BEGIN
      IF data_type NE '1d' THEN BEGIN
        file = 'GSW_'+data_type+'_TROPFLUX_1979-2011.nc'
        yearini_obs = 1979 & yearend_obs = 2011
      ENDIF ELSE BEGIN
        path = '/ccc/store/cont005/ra0542/berthets/Data/TROPFLUX/1d/'
        file = 'swr_tropflux_'+data_type+'_1979_2012.nc'
        yearini_obs = 1979 & yearend_obs = 2012
        var_file = 'swr'
      ENDELSE
      flag_mask = 0
      obs_name = 'TROPFLUX'
    ENDIF

    IF exp_name EQ 'ERAI' THEN BEGIN
      yearini_obs = 1989 & yearend_obs = 2011
      IF data_type EQ '1d' THEN file = 'GSW_'+data_type+'_ERAI_'+STRTRIM(yearini_obs,2)+'.nc' $
      ELSE file = 'GSW_'+data_type+'_ERAI_'+STRTRIM(yearini_obs,2)+'-'+STRTRIM(yearend_obs,2)+'.nc'
      flag_mask = 1
      obs_name = 'ERAI'
      IF data_type EQ '1d' THEN var_file = 'ssr'
    ENDIF

    ; SW net calculated with ERA-I SW down and WRF albedo
    IF exp_name EQ 'ERAI-WRF' THEN BEGIN
      file = 'GSW_'+data_type+'_ERAI-WRF_1989-2009.nc'
      yearini_obs = 1989 & yearend_obs = 2009
      obs_name = 'ERAI-WRF'
    ENDIF
    ; SW net calculated with ERA-I SW down and NEMO albedo
    IF exp_name EQ 'ERAI-NEMO' THEN BEGIN
      file = 'GSW_'+data_type+'_ERAI-NEMO_1989-2009.nc'
      yearini_obs = 1989 & yearend_obs = 2009
      obs_name = 'ERAI-NEMO'
    ENDIF
  END

  'SWDOWN': BEGIN
    file = 'SWDOWN_'+data_type+'_ERAI_1989-2009.nc'
    yearini_obs = 1989 & yearend_obs = 2009
    obs_name = 'ERAI'
  END

  'GLW': BEGIN
    file = 'GLW_'+data_type+'_ERAI_1989-2009.nc'
    yearini_obs = 1989 & yearend_obs = 2009
    flag_mask = 1
    obs_name  = 'ERAI'
  END

  'LWR': BEGIN
    IF force_seamask  THEN exp_name = 'ERAI'
    IF force_landmask THEN exp_name = 'TROPFLUX'

    IF exp_name EQ 'TROPFLUX' THEN BEGIN
      IF data_type NE '1d' THEN BEGIN
        file = 'LWR_'+data_type+'_TROPFLUX_1979-2011.nc'
        yearini_obs = 1979 & yearend_obs = 2011
      ENDIF ELSE BEGIN
        path = '/ccc/store/cont005/ra0542/berthets/Data/TROPFLUX/1d/'
        file = 'lwr_tropflux_'+data_type+'_1979_2012.nc'
        yearini_obs = 1979 & yearend_obs = 2012
        var_file = 'lwr'
      ENDELSE
      flag_mask = 0
      obs_name = 'TROPFLUX'
    ENDIF

    IF exp_name EQ 'ERAI' THEN BEGIN
      yearini_obs = 1989 & yearend_obs = 2009
      obs_name = 'ERAI'
      IF data_type EQ '1d' THEN file = var_name+'_'+data_type+'_'+obs_name+'_'+STRTRIM(yearini_obs,2)+'.nc' $
      ELSE file = var_name+'_'+data_type+'_'+obs_name+'_'+STRTRIM(yearini_obs,2)+'-'+STRTRIM(yearend_obs,2)+'.nc'
      flag_mask = 1
      IF data_type EQ '1d' THEN var_file = 'str'
    ENDIF

    ; LW net calculated with ERA-I SST+LW down and WRF constans
    IF exp_name EQ 'ERAI-WRF' THEN BEGIN
      file = 'LWR_'+data_type+'_ERAI-WRF_1989-2009.nc'
      yearini_obs = 1989 & yearend_obs = 2009
    ENDIF
    ; LW net calculated with ERA-I SST+LW down and NEMO constans
    IF exp_name EQ 'ERAI-NEMO' THEN BEGIN
      file = 'LWR_'+data_type+'_ERAI-NEMO_1989-2009.nc'
      yearini_obs = 1989 & yearend_obs = 2009
    ENDIF
  END

  'RAD': BEGIN
    IF force_seamask THEN exp_name = 'ERAI'
    IF force_landmask THEN exp_name = 'TROPFLUX'
    IF exp_name EQ 'TROPFLUX' THEN BEGIN
      IF data_type NE '1d' THEN BEGIN
        file = 'RAD_'+data_type+'_TROPFLUX_1979-2011.nc'
        yearini_obs = 1979 & yearend_obs = 2011
      ENDIF ELSE BEGIN
        file = 'RAD_'+data_type+'_TROPFLUX_1979-2012.nc'
        yearini_obs = 1979 & yearend_obs = 2012
      ENDELSE
      flag_mask = 0
    ENDIF
    IF exp_name EQ 'ERAI' THEN BEGIN
      file = 'RAD_'+data_type+'_ERAI_1989-2011.nc'
      yearini_obs = 1989 & yearend_obs = 2011
      flag_mask = 1
    ENDIF
    IF exp_name EQ 'ERAI-WRF' THEN BEGIN
      file = 'RAD_'+data_type+'_ERAI-WRF_1989-2009.nc'
      yearini_obs = 1989 & yearend_obs = 2009
      flag_mask = 1
    ENDIF
    IF exp_name EQ 'ERAI-NEMO' THEN BEGIN
      file = 'RAD_'+data_type+'_ERAI-NEMO_1989-2009.nc'
      yearini_obs = 1989 & yearend_obs = 2009
      flag_mask = 1
    ENDIF
  END

  'NET': BEGIN
    IF force_seamask THEN exp_name = 'ERAI'
    IF force_landmask THEN exp_name = 'TROPFLUX'
    IF exp_name EQ 'TROPFLUX' THEN BEGIN
      IF data_type NE '1d' THEN BEGIN
        file = 'NET_'+data_type+'_TROPFLUX_1979-2011.nc'
        yearini_obs = 1979 & yearend_obs = 2011
      ENDIF ELSE BEGIN
        file = 'NET_'+data_type+'_TROPFLUX_1979-2012.nc'
        yearini_obs = 1979 & yearend_obs = 2012
        var_file = 'netflux'
      ENDELSE
      flag_mask = 0
      obs_name = 'TROPFLUX'
    ENDIF
    IF exp_name EQ 'ERAI' THEN BEGIN
      file = 'NET_'+data_type+'_ERAI_1989-2011.nc'
      yearini_obs = 1989 & yearend_obs = 2011
      flag_mask = 1
      obs_name = 'ERAI'
    ENDIF
  END

  'LH': BEGIN
    IF force_seamask THEN exp_name = 'ERAI'
    IF force_landmask THEN exp_name = 'TROPFLUX'
    IF exp_name EQ 'TROPFLUX' THEN BEGIN
      IF data_type NE '1d' THEN BEGIN
        file = var_name+'_'+data_type+'_TROPFLUX_1979-2011.nc'
        yearini_obs = 1979 & yearend_obs = 2011
      ENDIF ELSE BEGIN
        path = '/ccc/store/cont005/ra0542/berthets/Data/TROPFLUX/1d/'
        file = 'lhf_tropflux_'+data_type+'_1979_2012.nc'
        yearini_obs = 1979 & yearend_obs = 2012
        var_file = 'lhf'
      ENDELSE
      flag_mask = 0
      obs_name = 'TROPFLUX'
    ENDIF
    IF exp_name EQ 'ERAI' THEN BEGIN       
      yearini_obs = 1989 & yearend_obs = 2011
      obs_name = 'ERAI'
      flag_mask = 1
      IF data_type NE '1d' THEN file = var_name+'_'+data_type+'_'+obs_name+'_'+STRTRIM(yearini_obs,2)+'-'+STRTRIM(yearend_obs,2)+'.nc' $
      ELSE file = var_name+'_'+data_type+'_'+obs_name+'_'+STRTRIM(yearini_obs,2)+'.nc'
      IF data_type EQ '1d' THEN var_file = 'slhf'
    ENDIF
  END

  'HFX': BEGIN
    IF force_seamask THEN exp_name = 'ERAI'
    IF force_landmask THEN exp_name = 'TROPFLUX'
    IF exp_name EQ 'TROPFLUX' THEN BEGIN
      IF data_type NE '1d' THEN BEGIN
        file = var_name+'_'+data_type+'_TROPFLUX_1979-2011.nc'
        yearini_obs = 1979 & yearend_obs = 2011
      ENDIF ELSE BEGIN
        path = '/ccc/store/cont005/ra0542/berthets/Data/TROPFLUX/1d/'
        file = 'shf_tropflux_'+data_type+'_1979_2012.nc'
        yearini_obs = 1979 & yearend_obs = 2012
        var_file = 'shf'
      ENDELSE
      flag_mask = 0
      obs_name = 'TROPFLUX'
    ENDIF
    IF exp_name EQ 'ERAI' THEN BEGIN
      yearini_obs = 1989 & yearend_obs = 2011
      obs_name = 'ERAI'
      flag_mask = 1
      IF data_type NE '1d' THEN file = var_name+'_'+data_type+'_'+obs_name+'_'+STRTRIM(yearini_obs,2)+'-'+STRTRIM(yearend_obs,2)+'.nc' $
      ELSE file = var_name+'_'+data_type+'_'+obs_name+'_'+STRTRIM(yearini_obs,2)+'.nc'
      IF data_type EQ '1d' THEN var_file = 'sshf'
    ENDIF
  END

  'OLR': BEGIN
    file = 'OLR_'+data_type+'_NOAA_1974-2011.nc'
    yearini_obs = 1974 & yearend_obs = 2011
    flag_mask = 1
    obs_name = 'NOAA'
  END


;-------------------------------------------------------------------------------------------------
; OCEANIC OBSERVATION DATASETS DEFINITIONS
;-------------------------------------------------------------------------------------------------

  'MLDT': BEGIN
    file = 'mld_DT02_c1m_reg2.0.nc'
    var_file = 'mld'
    flag_mask = 1
    obs_name = 'CDBM'
    yearini_obs = 1961 & yearend_obs = 2008
  END

  'MLDR': BEGIN
    file = 'mld_DR003_c1m_reg2.0.nc'
    var_file = 'mld'
    flag_mask = 1
    obs_name = 'CDBM'
    yearini_obs = 1961 & yearend_obs = 2008
  END

  'BLT': BEGIN
    file = 'blt_DTm02_c1m_reg2.0.nc'
    var_file = 'blt'
    flag_mask = 1
    obs_name = 'CDBM'
    yearini_obs = 1961 & yearend_obs = 2008
  END

  'SLA': BEGIN
    file = 'dt_upd_global_merged_msla_h_y1993_2012_c1m.nc'
    var_file = 'Grid_0001'
    flag_mask = 0
    obs_name = 'AVISO'
    yearini_obs = 1993 & yearend_obs = 2012
  END

  'THETAO': BEGIN
    IF data_type EQ 'c1m' THEN BEGIN
      ; CHECK BOX DEFINITION
      IF box[0] LT 30.5 OR box[1] GT 119.5 OR box[2] LT -29.5 OR box[3] GT 39.5 THEN STOP
      file      = 'nioa_climatology_monthly_temp_salt_nio.nc'
      var_file  = 'TEMP'
      flag_mask = 0
      flag_z    = 1
      obs_name  = 'NIOA'
      yearini_obs = 1900 & yearend_obs = 2009
    ENDIF ELSE STOP
  END

  'D20': BEGIN
    IF data_type EQ 'c1m' THEN BEGIN
      IF exp_name EQ 'OBS' OR exp_name EQ 'NIOA' THEN BEGIN
        ; CHECK BOX DEFINITION
        IF box[0] LT 30.5 OR box[1] GT 119.5 OR box[2] LT -29.5 OR box[3] GT 39.5 THEN STOP
        file      = 'nioa_climatology_monthly_temp_salt_nio.nc'
        var_file  = 'TEMP'
        flag_mask = 0
        flag_z    = 0
        obs_name  = 'NIOA'
        yearini_obs = 1900 & yearend_obs = 2009
      ENDIF
      IF STRMATCH(exp_name, 'GLORYS') THEN BEGIN
        file      = 'GLORYS2V1_'+data_type+'_1993-2009.nc'
        var_file  = 'votemper'
        flag_mask = 0
        flag_z    = 0
        obs_name  = 'GLORYS2V1'
        yearini_obs = 1993 & yearend_obs = 2009
      ENDIF
    ENDIF
    IF data_type EQ '1m' THEN BEGIN
      IF exp_name EQ 'GLORYS' OR exp_name EQ 'GLORYS2V1' THEN BEGIN
        file      = 'GLORYS2V1_'+data_type+'_1993-2009.nc'
        var_file  = 'votemper'
        flag_mask = 0
        flag_z    = 0
        obs_name  = 'GLORYS2V1'
        yearini_obs = 1993 & yearend_obs = 2009
      ENDIF
      IF exp_name EQ 'OBS' OR exp_name EQ 'SODA' THEN BEGIN
        path = '/ccc/store/cont005/ra0542/berthets/Data/SODA/'
        var_file = 'd20'
        yearini_obs = 1871 & yearend_obs = 2008
        file = 'SODA_2.2.4_1m_187101_200812_d20.nc'
        obs_name  = 'SODA 2.2.4'
        flag_mask = 1
      ENDIF
    ENDIF
  END


  'SO': BEGIN
    IF data_type EQ 'c1m' THEN BEGIN
      ; CHECK BOX DEFINITION
      IF box[0] LT 30.5 OR box[1] GT 119.5 OR box[2] LT -29.5 OR box[3] GT 39.5 THEN STOP
      file      = 'nioa_climatology_monthly_temp_salt_nio.nc'
      var_file  = 'SALT'
      flag_mask = 0
      flag_z    = 1
      obs_name  = 'NIOA'
      yearini_obs = 1900 & yearend_obs = 2009
    ENDIF ELSE STOP
  END

  ELSE: STOP

ENDCASE
help, exp_name


IF debug THEN STOP
