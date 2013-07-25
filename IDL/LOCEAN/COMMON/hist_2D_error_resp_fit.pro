FUNCTION Hist_2D_error_resp_fit , varx,vary,var_ref,vec_bin,tmoment=tmoment,n2keep=n2keep,allpoints=allpoints
  ; function calcule les mediane / moyenne par bin sur l'axe x d'une densité
  ; n2keep = min nb of observation in a bin for a mean value to be considered valid
  IF keyword_set(n2keep) EQ 0 THEN n2keep = 1
  
  IF keyword_set(allpoints) EQ 0 THEN BEGIN ; we only keep the mean value of the error
  
    mom_bin=fltarr(n_elements(vec_bin)) & mom_bin[*]=!values.f_nan
    delx=vec_bin[1]-vec_bin[0]
    FOR j=0,n_elements(vec_bin)-1 DO BEGIN
      lbnd=vec_bin[j]-delx/2.
      rbnd=vec_bin[j]+delx/2.
      igs=where(varx GE lbnd AND varx LT rbnd)
      
      IF igs[0] NE -1 THEN BEGIN
        IF n_elements(igs) GE n2keep THEN BEGIN
        
          IF tmoment EQ 'mae' THEN BEGIN
            mom_bin[j]=m_mean(abs(vary[igs]-var_ref[j]),/nan)
          ENDIF
          IF tmoment EQ 'rms' THEN BEGIN
            mom_bin[j]=sqrt(m_mean((vary[igs]-var_ref[j])^2.,/nan))
          ENDIF
        ENDIF
      ENDIF
    ENDFOR ; fin calcul des moy / med par bin de force de vent
    
  ENDIF ELSE BEGIN ; we keep all the values
  
    mom_bin=fltarr(size(varx,/dim)) & mom_bin[*]=!values.f_nan
    delx=vec_bin[1]-vec_bin[0]
    FOR j=0,n_elements(vec_bin)-1 DO BEGIN
      lbnd=vec_bin[j]-delx/2.
      rbnd=vec_bin[j]+delx/2.
      igs=where(varx GE lbnd AND varx LT rbnd)
      igs_2D=array_indices([(size(varx))[1],(size(varx))[2]],igs,/dim)
      
      IF igs[0] NE -1 THEN BEGIN
        IF n_elements(igs) GE n2keep THEN BEGIN
          FOR ii=0L,n_elements(igs)-1L DO BEGIN
          
            IF tmoment EQ 'mae' THEN BEGIN
              mom_bin[igs[ii]]=abs(vary[igs[ii]]-var_ref[j])
            ENDIF
            IF tmoment EQ 'rms' THEN BEGIN
              mom_bin[igs[ii]]=(vary[igs[ii]]-var_ref[j])^2.;faire ensuite la racine 
            ENDIF
            
          ENDFOR
        ENDIF
      ENDIF
    ENDFOR ; fin calcul des moy / med par bin de force de vent
    
    
  ENDELSE
  a=1
  return,mom_bin
  
END