

    ; CALCUL GRAD FROM VAR_MEAN
    FOR e = 0, n_elements(exp_list)-1 DO BEGIN
      IF e EQ 0 THEN cmd = execute( 'tmp = var_mean_'+strtrim(e,2) ) ELSE cmd = execute( 'tmp = var_mean_'+strtrim(e,2)+'_gridobs' )
      cmd = execute( 'gradx_'+strtrim(e,2)+' = GRAD( tmp, "x", /MILLION)' )
      cmd = execute( 'grady_'+strtrim(e,2)+' = GRAD( tmp, "y", /MILLION)' )
      cmd = execute( 'grad_'+strtrim(e,2)+'  = NORM( gradx_'+strtrim(e,2)+', grady_'+strtrim(e,2)+')' )
      cmd = execute( 'help, grad_'+strtrim(e,2) )
    ENDFOR

    ; MEAN GRAD ERROR (MODEL-OBS)
    IF n_elements(exp_list) GT 1 THEN FOR e = 1, n_elements(exp_list)-1 DO cmd = execute( 'errgrad_'+STRTRIM(e,2)+' = grad_'+STRTRIM(e,2)+' - grad_0' )

    ; MEAN GRAD DIFFERENCE (MODEL-MODEL)
    IF n_elements(exp_list) GT 2 THEN FOR e = 2, n_elements(exp_list)-1 DO cmd = execute( 'diffgrad_'+STRTRIM(e,2)+' = grad_'+STRTRIM(e,2)+' - grad_'+STRTRIM(e-1,2) )
