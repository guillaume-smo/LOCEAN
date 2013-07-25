FUNCTION calc_delta_epot_pour_1_delta_sst,delta_SST,profil_T,profil_rho,profil_dept,e3t,h_CM=h_CM,interp_2m=interp_2m,prof_t10_m2=prof_t10_m2,$
    delta_Eneg=delta_Eneg,delta_Epos=delta_Epos;,$
  ; pour un profil vertical de T, rho d'océan
  ; calcule le delta Epot associe a un delta SST donne en entree
    
  ;; variables d'entrée
  ;profil_T,    température
  ;profil_S,    salinité
  ;profil_dept  profondeur associée
  ;;
  ;; variables de sortie
  ;delta_Epot              delta energie en fonction du delta SST demande
  ;h_CM,                   profondeur de couche de mélange pour qvoir ce refroidissement
  ;prof_t0_m2,             profondeur de  l'isotherme "T surface - 2deg C"
  ;delta_Eneg              partie NEGATIVE seule du delta energie en fonction du delta SST demande
  ;delta_Epos              partie POSITIVE seule du delta energie en fonction du delta SST demande
  ;delta_Eneg+delta_Epos le delta_Epot avec référence prise à 1000m pour le calcul
    
  ;; option
  ;interp_2m  set to one if interpolation to a 2m grid is required
    
  Cp_= 4.*10.^(3.)  ; J / kg / K
  gg = 9.81         ; m / s^2
  
  T2stop = profil_T[0]-delta_SST
  
  profil_lim2couch=(profil_dept-e3t/2.+shift(profil_dept+e3t/2.,1))/2.
  profil_lim2couch[0]=0
  ;profil_lim2couch=[profil_lim2couch,profil_dept[n_elements(profil_dept)-1]+e3t[n_elements(profil_dept)-1]/2.]
  
  prof_max_mel=max(profil_lim2couch)
  new_deptup=prof_max_mel-profil_dept
  new_dept_lim2couch=prof_max_mel-profil_lim2couch
  ;new_deptup=prof_max_mel-profil_dept
  
  IF keyword_set(interp_2m) THEN BEGIN
    old_dept=profil_dept
    new_dept=findgen( 499. )*2.+3
    igd=(reverse(where(finite(profil_t) EQ 1)))[0]
    igd=(reverse(where(new_dept LE old_dept[igd])))[0]
    ; interpole T,S et rho sur la prof plus précise
    IF n_elements(profil_T) NE n_elements(old_dept) THEN stop ; c'est quoi ce bordel
    profil_T_new=interpol(profil_T,old_dept,new_dept)
    ; profil_S=interpol(profil_S,profil_dept,new_dept)
    profil_rho=interpol(profil_rho,old_dept,new_dept)
    
    IF igd LT n_elements(new_dept)-1 THEN BEGIN
      profil_T_new[igd+1:*]=!values.f_nan
      profil_rho[igd+1:*]=!values.f_nan
    ENDIF
    
    ;  IF finite(total(profil_t)) EQ 0 THEN stop
    
    profil_dept=new_dept
    profil_lim2couch=findgen( 500. )*2.+2
    
    prof_max_mel=max(profil_lim2couch)
    new_deptup=prof_max_mel-profil_dept
    new_dept_lim2couch=prof_max_mel-profil_lim2couch
    
    e3t_new=fltarr(n_elements(profil_dept))+2
  ENDIF ELSE BEGIN
    e3t_new = e3t
  ENDELSE
  
  ;  i_dept1=(where(new_dept GT profil_CM))[0] ; prof où doit commencer le calcul d'approf de CM, après la CM initiale
  ;  dept=new_dept[ i_dept1 -1] ; dernière profondeur appartenant à la CMi
  
  E_therm = CP_*profil_rho[0]*profil_T_new[0]*e3t_new[0] ; J/m2
  T_CM    = profil_T_new[0]
  rho_CM  = profil_rho[0]
  dept=profil_dept[0]
  Epot=1./2.*profil_rho[0]*gg* ( (new_dept_lim2couch[0])^(2.)-(new_dept_lim2couch[1])^(2.) ) ; J/m2
  ip=0
  
  IF T_CM LE T2stop THEN BEGIN
    delta_Epot = !values.f_nan
    GOTO,noEp
  ENDIF
  
  WHILE T_CM GE T2stop AND ip LE n_elements(profil_dept)-2 AND finite(T_CM) EQ 1 DO BEGIN
    ip=ip+1
    ; E_therm au nouveau step
    E_therm_step=CP_*profil_rho[ip]*profil_T_new[ip]*e3t_new[ip]
    E_therm2=E_therm+E_therm_step
    
    rho_CM2=(rho_CM*total(e3t_new[0:ip-1])+profil_rho[ip]*e3t_new[ip] )/(total(e3t_new[0:ip]))
    
    ; temp de la CM après homogénéisation
    T_CM2=1./(rho_CM2*CP_*total(e3t_new[0:ip])) * ( E_therm2 )
    
    ;  Epot_step=profil_rho[ip]*gg*( (-profil_lim2couch[ip]^(2.)+profil_lim2couch[ip+1]^(2.))/2. )
    ;  Epot2=Epot+Epot_step
    
    ; fin de step de boucle, init les variables au rang +1
    E_therm=E_therm2
    rho_CM=rho_CM2
    T_CM=T_CM2
  ;  Epot=Epot2
  ; teste si le critere Tsurf - deltaSST est atteint
  ; IF THEN GOTO,fin_approf
  ENDWHILE
  ; fin_approf:
  ; last_idx=ip+1 < n_elements(new_dept_lim2couch)-1
  ; Epot_mix=1./2.*rho_CM2*gg*( (0)^(2.)-profil_lim2couch[last_idx]^(2.) )
  ;delt_Epot=Epot_mix-Epot2
  last_idx=ip < n_elements(profil_dept)-1
  ;;; IF keyword_set(interp_2m) THEN BEGIN
  ;  delt_Epot = total( (rho_CM2*gg*new_deptup)[0:last_idx]*2. ) - total( (profil_rho*gg*new_deptup)[0:last_idx]*2. )
  
  delta_Epot = total( ((rho_CM2-profil_rho)*gg*(-profil_dept))[0:last_idx]*2. ) ; *2 est pour e3t (cas interp 2m)
  
  IF finite(delta_Epot) THEN BEGIN ; si profondeur assez grande pour atteindre le refroidissement souhaité
    ipos = where(((rho_CM2-profil_rho)*gg*(1000-profil_dept))[0:last_idx]*2. GT 0)
    delta_Epos = total( (((rho_CM2-profil_rho)*gg*(1000-profil_dept))[0:last_idx]*2.)[ipos] )
    ineg = where(((rho_CM2-profil_rho)*gg*(1000-profil_dept))[0:last_idx]*2. LE 0)
    delta_Eneg = total( (((rho_CM2-profil_rho)*gg*(1000-profil_dept))[0:last_idx]*2.)[ineg] )
  ENDIF ELSE BEGIN                 ; sinon pas de chocolat
  delta_Epos = !values.f_nan
  delta_Eneg = !values.f_nan
  ENDELSE
  
  ;;; ENDIF
  
  iz10=(reverse(where(profil_dept LE 10)))[0]
  prof_t10_m2=interpol(profil_dept,profil_T_new, profil_T_new[iz10]-delta_SST)
  ; [ (reverse(where(profil_T GE profil_T[iz10]-delta_SST)))[0] ] ancienne methode à la con; interpole, c'est plus propre
  
  h_CM=profil_dept[last_idx]
  IF finite(T_CM) EQ 0 OR ip EQ 0 THEN BEGIN
    prof_t10_m2 = !values.f_nan
    h_CM = !values.f_nan
  ENDIF
  
  noEP:
  return,delta_Epot
  
;  splot,profil_T,-gdept,yrange=[-600,0],xrange=[min(profil_T)-1,max(profil_T)+1]
;  oplot,profil_Ti,-new_dept,color=100,psym=3
;  oplot,[min(profil_T)-1,max(profil_T)+1],-[profil_CM,profil_CM],linestyle=2
;  splot,profil_S,-gdept,yrange=[-600,0],xrange=[min(profil_S)-1,max(profil_S)+1]
;  oplot,profil_Si,-new_dept,color=100,psym=3
;  oplot,[min(profil_S)-1,max(profil_S)+1],-[profil_CM,profil_CM],linestyle=2
;  splot,profil_rho,-gdept,yrange=[-600,0],xrange=[min(profil_rho)-1,max(profil_rho)+1]
;  oplot,profil_rhoi,-new_dept,color=100,psym=3
;  splot,profil_Si,profil_Ti
  
END
