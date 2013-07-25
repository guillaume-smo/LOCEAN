FUNCTION hist_2D_moment_bin, varx, vary, vec_bin, tmoment = tmoment, n2keep = n2keep
  ; function calcule les mediane / moyenne par bin sur l'axe x d'une densite
  ; n2keep = min nb of observation in a bin for a mean value to be considered valid

  IF keyword_set(n2keep) EQ 0 THEN n2keep = 1
  
  IF tmoment EQ 'moy' THEN BEGIN
    mom_bin_deltat = fltarr(2,n_elements(vec_bin)) & mom_bin_deltat[*,*] = !values.f_nan
  ENDIF

  IF tmoment EQ 'med' THEN BEGIN
    mom_bin_deltat = fltarr(3,n_elements(vec_bin)) & mom_bin_deltat[*,*] = !values.f_nan
  ENDIF

  delx=vec_bin[1]-vec_bin[0]
  FOR j=0,n_elements(vec_bin)-1 DO BEGIN
    lbnd=vec_bin[j]-delx/2.
    rbnd=vec_bin[j]+delx/2.
    igs=where(varx GE lbnd AND varx LT rbnd)
    
    IF igs[0] NE -1 THEN BEGIN
      IF n_elements(igs) GE n2keep THEN BEGIN
        IF tmoment EQ 'moy' THEN BEGIN
          mom_bin_deltat[0,j] = m_mean(vary[igs],/nan)
          mom_bin_deltat[1,j] = stddev(vary[igs],/nan)
        ENDIF
        IF tmoment EQ 'med' THEN BEGIN
          mom_bin_deltat[0,j] = median(vary[igs])
          mom_bin_deltat[1:2,j] = percentile(vary[igs],0.25)
        ENDIF
      ENDIF
    ENDIF
  ENDFOR ; fin calcul des moy / med par bin de force de vent
  
  return,mom_bin_deltat
  
END
