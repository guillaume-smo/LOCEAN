; INTERPOLATION FROM GRID "n" TO GRID "0"
;("n" and "0" are experiences number)



; OBS CASE
IF STRMATCH( exp_name, 'tr*') EQ 0 AND STRMATCH( exp_name, 'ind*') EQ 0 THEN BEGIN
  save_data    = 0 & load_data    = 0
  save_gridobs = 0 & load_gridobs = 0
ENDIF


; TEST IF INTERPOLATION IS NECESSARY OR NOT
cmd = execute( 'flag_interp = (n_elements(lon_0) NE n_elements(lon_'+STRTRIM(e,2)+')) OR (n_elements(lat_0) NE n_elements(lat_'+STRTRIM(e,2)+'))' )
help, flag_interp


;IF (STRMATCH( exp_name, 'tr*' ) OR STRMATCH( exp_name, 'ind*' )) AND save_gridobs AND load_gridobs EQ 0 AND e GT 0 THEN BEGIN
IF e GT 0 AND flag_interp THEN BEGIN

  print, 'INTERPOLATION...'

  IF data_type EQ 'c1m' OR data_type EQ '1m' THEN cmd = execute( 'nt = nbmonth_'+STRTRIM(e,2) )
  IF data_type EQ '1d' THEN cmd = execute( 'nt = nbday_'+STRTRIM(e,2) )

  ; INPUT/OUTPUT GRID DEFINITION
  gridin_id = e
  cmd = execute( 'lon_in = lon_'+STRTRIM(gridin_id,2) )
  cmd = execute( 'lat_in = lat_'+STRTRIM(gridin_id,2) )
  help, lon_in, lat_in
  gridout_id = 0
  cmd = execute( 'lon_out = lon_'+STRTRIM(gridout_id,2) )
  cmd = execute( 'lat_out = lat_'+STRTRIM(gridout_id,2) )
  help, lon_out, lat_out

  ; MASK
  IF flag_mask THEN BEGIN
    IF model EQ 'wrf' THEN seamask_gridobs  = fromreg("bilinear",seamask,lon_in,lat_in,lon_out,lat_out)
    landmask_gridobs = fromreg("bilinear",landmask,lon_in,lat_in,lon_out,lat_out)
    IF STRMATCH( exp_name, 'tr*' ) AND model EQ 'wrf' THEN highmask_gridobs = fromreg("bilinear",highmask,lon_in,lat_in,lon_out,lat_out)
    help, seamask_gridobs, landmask_gridobs, highmask_gridobs
  ENDIF

  ; SEASONAL AVERAGE
  var_mean_gridobs = fromreg('bilinear', var_mean, lon_in, lat_in, lon_out, lat_out)
  IF data_type EQ 'c1m' OR data_type EQ '1m' THEN ano_mean_gridobs = fromreg('bilinear', ano_mean, lon_in, lat_in, lon_out, lat_out)
  help, var_mean_gridobs, ano_mean_gridobs
  IF STRMATCH(var_name, 'UV*') OR var_name EQ 'STRESS' OR var_name EQ 'RAIN' THEN BEGIN
    varx_mean_gridobs = fromreg('bilinear', varx_mean, lon_in, lat_in, lon_out, lat_out)
    vary_mean_gridobs = fromreg('bilinear', vary_mean, lon_in, lat_in, lon_out, lat_out)
  ENDIF

  ; c1m+1m+1d DATA
  ;IF data_type EQ 'c1m' OR data_type EQ '1m' OR (data_type EQ '1d' AND (fyc_only OR fym_only OR fyo_only)) THEN BEGIN
  IF data_type NE '6h' THEN BEGIN

    var_gridobs = fltarr(n_elements(lon_out), n_elements(lat_out), nt)
    FOR t = 0, nt-1 DO var_gridobs[*,*,t] = fromreg("bilinear",var[*,*,t],lon_in,lat_in,lon_out,lat_out)
    help, var_gridobs

    IF STRMATCH(var_name, 'UV*') OR var_name EQ 'STRESS' OR var_name EQ 'RAIN' THEN BEGIN
      varx_gridobs = fltarr(n_elements(lon_out), n_elements(lat_out), nt)
      vary_gridobs = fltarr(n_elements(lon_out), n_elements(lat_out), nt)
      FOR t = 0, nt-1 DO BEGIN
        varx_gridobs[*,*,t] = fromreg("bilinear",varx[*,*,t],lon_in,lat_in,lon_out,lat_out)
        vary_gridobs[*,*,t] = fromreg("bilinear",vary[*,*,t],lon_in,lat_in,lon_out,lat_out)
      ENDFOR
      help, varx_gridobs, vary_gridobs
    ENDIF

    ; 1m INTERANNUAL SEASONAL
    IF data_type EQ '1m' THEN BEGIN
      FOR l = 0, n_elements( interp_list)-1 DO BEGIN
        cmd = execute( interp_list[l]+'_gridobs = FLTARR(n_elements(lon_out), n_elements(lat_out), nbyear_mod)' )
        FOR y = 0, nbyear_mod -1 DO cmd = execute( interp_list[l]+'_gridobs[*,*,y] = fromreg("bilinear", '+interp_list[l]+'[*,*,y],lon_in,lat_in,lon_out,lat_out)' )
        cmd = execute( 'help, '+interp_list[l]+'_gridobs' )
      ENDFOR
    ENDIF

    
  ENDIF ; c1m+1m+1d DATA

  print, 'INTERPOLATION OK' & print, ''

ENDIF


IF (STRMATCH( exp_name, 'tr*' ) OR STRMATCH( exp_name, 'ind*' )) AND save_gridobs EQ 0 AND load_gridobs EQ 1 THEN BEGIN
  @restore_gridobs
ENDIF


IF debug THEN STOP
