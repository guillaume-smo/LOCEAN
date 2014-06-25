
FOR e = 0, n_elements(exp_list)-1 DO BEGIN

  ; TENDENCY
  IF e NE 0 THEN BEGIN
    cmd = execute( 'vardt_'+strtrim(e,2)+' = var_'+strtrim(e,2)+'_gridobs - SHIFT(var_'+strtrim(e,2)+'_gridobs, 0, 0, 1)' )
  ENDIF ELSE BEGIN
    cmd = execute( 'vardt_'+strtrim(e,2)+' = var_'+strtrim(e,2)+' - SHIFT(var_'+strtrim(e,2)+', 0, 0, 1)' )
  ENDELSE

  ; TENDENCY TIME SERIES
  cmd = execute( 'vardt_1D_'+strtrim(e,2)+' = MEAN( MEAN( vardt_'+strtrim(e,2)+', DIMENSION=1, /NAN), DIMENSION=1, /NAN)' )

  ; MEAN TENDENCY
  IF n_elements(ind_period) GT 1 THEN cmd = execute( 'vardt_mean_'+strtrim(e,2)+' =  MEAN( vardt_'+strtrim(e,2)+'[*,*,ind_mean1m], DIMENSION=3, /NAN)' ) $
  ELSE cmd = execute( 'vardt_mean_'+strtrim(e,2)+' = vardt_'+strtrim(e,2)+'[*,*,ind_mean1m]')

  ; CUMULATIVE TENDENCY
  IF n_elements(ind_period) GT 1 THEN cmd = execute( 'vardt_cum_'+strtrim(e,2)+' = TOTAL( vardt_'+strtrim(e,2)+' [*,*,ind_mean1m], 3)' ) $
  ELSE cmd = execute( 'vardt_cum_'+strtrim(e,2)+' = vardt_'+strtrim(e,2)+'[*,*,ind_mean1m]' )

ENDFOR


; ERRORS
FOR e = 1, n_elements(exp_list)-1 DO BEGIN
  cmd = execute( 'errdt_mean_'+strtrim(e,2)+' = vardt_mean_'+strtrim(e,2)+' - vardt_mean_0' )
  cmd = execute( 'errdt_cum_'+strtrim(e,2)+'  = vardt_cum_'+strtrim(e,2)+' - vardt_cum_0' )
ENDFOR
