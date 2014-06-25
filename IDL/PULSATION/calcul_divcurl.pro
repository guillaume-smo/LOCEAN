

    ; CALCUL DIVERGENCE
    div_0 = div(smooth(varx_mean_0,1,/NAN),smooth(vary_mean_0,1,/NAN), /MILLION)
    IF flag_nemo OR force_landmask THEN div_0 = div_0 * landmask_0
    FOR e = 1, n_elements(exp_list)-1 DO BEGIN
      cmd = execute( 'div_'+strtrim(e,2)+' = div(smooth(varx_mean_'+strtrim(e,2)+'_gridobs,1,/NAN), smooth(vary_mean_'+strtrim(e,2)+'_gridobs,1,/NAN), /MILLION)')
      IF flag_nemo OR force_landmask THEN cmd = execute( 'div_'+strtrim(e,2)+' = div_'+strtrim(e,2)+' * landmask_'+strtrim(e,2)+'_gridobs' )
    ENDFOR

    ; ERREUR DIVERGENCE
    FOR e = 1, n_elements(exp_list)-1 DO cmd = execute( 'errdiv_'+strtrim(e,2)+' = div_'+strtrim(e,2)+' - div_0' )

    ; CALCUL CURL
    rot_0 = curl(smooth(varx_mean_0,1,/NAN),smooth(vary_mean_0,1,/NAN), /MILLION)
    IF flag_nemo OR force_landmask THEN rot_0 = rot_0 * landmask_0
    FOR e = 1, n_elements(exp_list)-1 DO BEGIN
      cmd = execute( 'rot_'+strtrim(e,2)+' = curl(smooth(varx_mean_'+strtrim(e,2)+'_gridobs,1,/NAN), smooth(vary_mean_'+strtrim(e,2)+'_gridobs,1,/NAN), /MILLION)')
      IF flag_nemo OR force_landmask THEN cmd = execute( 'rot_'+strtrim(e,2)+' = rot_'+strtrim(e,2)+' * landmask_'+strtrim(e,2)+'_gridobs' )
    ENDFOR

    ; ERREUR CURL
    FOR e = 1, n_elements(exp_list)-1 DO cmd = execute( 'errrot_'+strtrim(e,2)+' = rot_'+strtrim(e,2)+' - rot_0' )
