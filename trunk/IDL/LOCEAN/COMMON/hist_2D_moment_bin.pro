FUNCTION Hist_2D_moment_bin, varx, vary, vec_bin, tmoment=tmoment, n2keep=n2keep, irr=irr, nb_per_bin=nb_per_bin, bt_lev=bt_lev, quantile=quantile
  ; function calcule les mediane / moyenne par bin sur l'axe x d'une densité
  ; n2keep = min nb of observation in a bin for a mean value to be considered valid
  ; irr if irregular axis, do not give central values for the bin but bnds values with nb_bin+1 elements


  IF keyword_set(n2keep)   EQ 0 THEN n2keep   = 1
  IF keyword_set(bt_lev)   EQ 0 THEN bt_lev   = 0.05
  IF keyword_set(quantile) EQ 0 THEN quantile = 0.25

  IF tmoment EQ 'moy' OR tmoment EQ 'root' THEN BEGIN
    IF keyword_set(irr) EQ 1 THEN BEGIN
      mom_bin_deltat = fltarr(2,n_elements(vec_bin)-1) & mom_bin_deltat[*,*] = !values.f_nan
    ENDIF ELSE BEGIN
      mom_bin_deltat = fltarr(2,n_elements(vec_bin))   & mom_bin_deltat[*,*] = !values.f_nan
    ENDELSE
  ENDIF

  IF tmoment EQ 'med' OR tmoment EQ 'bootstrap' OR tmoment EQ 'rms_bootstrap' THEN BEGIN
    IF keyword_set(irr) EQ 1 THEN BEGIN
      mom_bin_deltat = fltarr(3,n_elements(vec_bin)-1) & mom_bin_deltat[*,*] = !values.f_nan
    ENDIF ELSE BEGIN
      mom_bin_deltat = fltarr(3,n_elements(vec_bin))   & mom_bin_deltat[*,*] = !values.f_nan
    ENDELSE
  ENDIF
  
  IF keyword_set(irr) EQ 1 THEN BEGIN
    nblast = 2
  ENDIF ELSE BEGIN
    delx   = vec_bin[1]-vec_bin[0]
    nblast = 1
  ENDELSE
  
  
  FOR j=0,n_elements(vec_bin)-nblast DO BEGIN
  
    IF keyword_set(irr) EQ 1 THEN BEGIN
      lbnd=vec_bin[j]
      rbnd=vec_bin[j+1]
    ENDIF ELSE BEGIN
      lbnd=vec_bin[j]-delx/2.
      rbnd=vec_bin[j]+delx/2.
    ENDELSE
    
    igs=where(varx GE lbnd AND varx LT rbnd)
    
    IF igs[0] NE -1 THEN BEGIN
      IF n_elements(igs) GE n2keep THEN BEGIN
        IF tmoment EQ 'moy' THEN BEGIN
          mom_bin_deltat[0,j]=m_mean(vary[igs],/nan)
          mom_bin_deltat[1,j]=stddev(vary[igs],/nan)
        ENDIF
        IF tmoment EQ 'med' THEN BEGIN
          mom_bin_deltat[0,j]=median(vary[igs])
          
          mom_bin_deltat[1:2,j]=percentile(vary[igs],quantile)
        ENDIF
        
        IF tmoment EQ 'root' THEN BEGIN  ; if squared errors as entry
          mom_bin_deltat[0,j]=sqrt(m_mean(vary[igs],/nan))
          mom_bin_deltat[1,j]=sqrt(stddev(vary[igs],/nan))
        ENDIF
        
        IF tmoment EQ 'bootstrap' THEN BEGIN
          mom_bin_deltat[0,j]=m_mean(vary[igs],/nan)
          ; print,'avant boot strap'
          nbt=1000
          moy_bsts=fltarr(nbt)
          FOR ibt=0,nbt-1 DO BEGIN
            ibtcg=resample(n_elements(igs), n_elements(igs))
            ings=igs[ibtcg]
            moy_bsts[ibt]=m_mean(vary[ings],/nan)
          ENDFOR
          ;   record the lower-upper confidence interval
          low_up_bnd_95=percentile(moy_bsts,bt_lev)
          mom_bin_deltat[1,j] = low_up_bnd_95[0]
          mom_bin_deltat[2,j] = low_up_bnd_95[1]
        ENDIF ;/boot strap
        
        IF tmoment EQ 'rms_bootstrap' THEN BEGIN
          mom_bin_deltat[0,j]=sqrt(m_mean(vary[igs]^2.,/nan))
          ; print,'avant boot strap'
          nbt=1000
          moy_bsts=fltarr(nbt)
          FOR ibt=0,nbt-1 DO BEGIN
            ibtcg=resample(n_elements(igs), n_elements(igs))
            ings=igs[ibtcg]
            moy_bsts[ibt]=sqrt(m_mean(vary[ings]^2.,/nan))
          ENDFOR
          ;   record the lower-upper confidence interval
          low_up_bnd_95=percentile(moy_bsts,bt_lev)
          mom_bin_deltat[1,j] = low_up_bnd_95[0]
          mom_bin_deltat[2,j] = low_up_bnd_95[1]
        ENDIF ;/RMS boot strap
        
      ENDIF
    ENDIF
  ENDFOR ; fin calcul des moy / med par bin de force de vent
  
  a=1
  return,mom_bin_deltat
  
END
