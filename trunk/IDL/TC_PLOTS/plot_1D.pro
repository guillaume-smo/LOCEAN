; PLOT 1D TC+DOMAIN
lct,60
color_factor=70
key_portrait=1


; calcul des erreurs "domain + TC" par rapport aux obs ou analyses aladins
FOR i = 2, nb_exp-1 DO BEGIN
  cmd = execute('date = date_'+strtrim(i,2))  
  indok = (listmatch(date,date_1))[*,0]
  indok_obs = (listmatch(date,date_1))[*,1]
  cmd = execute('date_err_'+strtrim(i,2)+' = date[indok]')
  FOR k = 0, nb_var-1 DO BEGIN
    cmd = execute('var = '+var_list[k]+'_1DTC_'+strtrim(i,2))
    cmd = execute('err_1DTC_'+var_list[k]+'_'+strtrim(i,2)+' = var[indok]-'+var_list[k]+'_1DTC_1[indok_obs] & help, err_1DTC_'+var_list[k]+'_'+strtrim(i,2))
    cmd = execute('var = '+var_list[k]+'_1D_'+strtrim(i,2))
    cmd = execute('err_1D_'+var_list[k]+'_'+strtrim(i,2)+' = var[indok]-'+var_list[k]+'_1D_1[indok_obs] & help, err_1D_'+var_list[k]+'_'+strtrim(i,2))
  ENDFOR
;  cmd = execute('err_1D_RVM_'+strtrim(i,2)+' = rvm_'+strtrim(i,2)+'[indok] - rvm_1[indok_obs] & help, err_1D_RVM_'+strtrim(i,2))
  cmd = execute('err_1D_RVM_'+strtrim(i,2)+' = RVM_1DTC_'+strtrim(i,2)+'[indok] - RVM_1DTC_0[indok_obs] & help, err_1D_RVM_'+strtrim(i,2))
ENDFOR

; traitement à part de la SST (OBS+ODYSSEA+REMSS+PSY3+GLORYS)
FOR i = 1, nb_exp-1 DO BEGIN
  cmd = execute('date = date_'+strtrim(i,2))  
  indok = (listmatch(date,date_0))[*,0]
  indok_obs = (listmatch(date,date_0))[*,1]
  cmd = execute('date_err_'+strtrim(i,2)+' = date[indok]')
  k = (where(var_list EQ 'SST'))[0]
  ; SST OBS
  cmd = execute('var = '+var_list[k]+'_1DTC_'+strtrim(i,2))
  cmd = execute('err_1DTC_'+var_list[k]+'_'+strtrim(i,2)+' = var[indok]-'+var_list[k]+'_1DTC_0[indok_obs] & help, err_1DTC_'+var_list[k]+'_'+strtrim(i,2))
  cmd = execute('var = '+var_list[k]+'_1D_'+strtrim(i,2))
  cmd = execute('err_1D_'+var_list[k]+'_'+strtrim(i,2)+' = var[indok]-'+var_list[k]+'_1D_0[indok_obs] & help, err_1D_'+var_list[k]+'_'+strtrim(i,2))
  ; SST ODYSSEA
  cmd = execute('var = '+var_list[k]+'_1DTC_'+strtrim(i,2))
  cmd = execute('err_1DTC_'+var_list[k]+'O_'+strtrim(i,2)+' = var[indok]-'+var_list[k]+'O_1DTC_0[indok_obs] & help, err_1DTC_'+var_list[k]+'O_'+strtrim(i,2))
  cmd = execute('var = '+var_list[k]+'_1D_'+strtrim(i,2))
  cmd = execute('err_1D_'+var_list[k]+'O_'+strtrim(i,2)+' = var[indok]-'+var_list[k]+'O_1D_0[indok_obs] & help, err_1D_'+var_list[k]+'O_'+strtrim(i,2))
  ; SST REMSS
  cmd = execute('var = '+var_list[k]+'_1DTC_'+strtrim(i,2))
  cmd = execute('err_1DTC_'+var_list[k]+'R_'+strtrim(i,2)+' = var[indok]-'+var_list[k]+'R_1DTC_0[indok_obs] & help, err_1DTC_'+var_list[k]+'R_'+strtrim(i,2))
  cmd = execute('var = '+var_list[k]+'_1D_'+strtrim(i,2))
  cmd = execute('err_1D_'+var_list[k]+'R_'+strtrim(i,2)+' = var[indok]-'+var_list[k]+'R_1D_0[indok_obs] & help, err_1D_'+var_list[k]+'R_'+strtrim(i,2))
  ; SST PSY3
  cmd = execute('var = '+var_list[k]+'_1DTC_'+strtrim(i,2))
  cmd = execute('err_1DTC_'+var_list[k]+'P_'+strtrim(i,2)+' = var[indok]-'+var_list[k]+'P_1DTC_0[indok_obs] & help, err_1DTC_'+var_list[k]+'P_'+strtrim(i,2))
  cmd = execute('var = '+var_list[k]+'_1D_'+strtrim(i,2))
  cmd = execute('err_1D_'+var_list[k]+'P_'+strtrim(i,2)+' = var[indok]-'+var_list[k]+'P_1D_0[indok_obs] & help, err_1D_'+var_list[k]+'P_'+strtrim(i,2))
  ; SST GLORYS
  cmd = execute('var = '+var_list[k]+'_1DTC_'+strtrim(i,2))
  cmd = execute('err_1DTC_'+var_list[k]+'G_'+strtrim(i,2)+' = var[indok]-'+var_list[k]+'G_1DTC_0[indok_obs] & help, err_1DTC_'+var_list[k]+'G_'+strtrim(i,2))
  cmd = execute('var = '+var_list[k]+'_1D_'+strtrim(i,2))
  cmd = execute('err_1D_'+var_list[k]+'G_'+strtrim(i,2)+' = var[indok]-'+var_list[k]+'G_1D_0[indok_obs] & help, err_1D_'+var_list[k]+'G_'+strtrim(i,2))
ENDFOR


; calcul valeur moyenne par date
ind_alad = where(exp_list EQ 'ALADIN-OPER', nb_alad)
ind_alan = where(exp_list EQ 'ALADIN-ANA' , nb_alan)
ind_arom = where(STRMID(exp_list,0,1) EQ '9', nb_arom)
ind_aro1 = where(STRMATCH(alt_list,'*'+param1+'*') EQ 1, nb_aro1)
ind_aro2 = where(STRMATCH(alt_list,'*'+param2+'*') EQ 1, nb_aro2)

;FOR k = 0, nb_var-1 DO BEGIN
k = 0

  cmd = execute('ave_1DTC_'+var_list[k]+'_alad = fltarr(tdim_0)')
  cmd = execute('ave_1DTC_'+var_list[k]+'_arom = fltarr(tdim_0)')
  cmd = execute('ave_1DTC_'+var_list[k]+'_aro1 = fltarr(tdim_0)')
  cmd = execute('ave_1DTC_'+var_list[k]+'_aro2 = fltarr(tdim_0)')
;  cmd = execute('help, ave_1DTC_'+var_list[k]+'_alad, ave_1DTC_'+var_list[k]+'_arom')

  FOR i = 0, tdim_0-1 DO BEGIN
    date = date_0[i]
    l = 0
    FOR j =  0, nb_alad-1 DO BEGIN
      cmd = execute('iok = where(date_'+strtrim(ind_alad[j],2)+' EQ date, cntok)')
      IF (cntok EQ 1) AND (l EQ 0) THEN cmd = execute('tmp = '+var_list[k]+'_1DTC_'+strtrim(ind_alad[j],2)+'[iok]')
      IF (cntok EQ 1) AND (l GT 0) THEN cmd = execute('tmp = [ tmp, '+var_list[k]+'_1DTC_'+strtrim(ind_alad[j],2)+'[iok] ]')
      IF cntok EQ 1 THEN l = l + 1      
    ENDFOR
    cmd = execute('ave_1DTC_'+var_list[k]+'_alad[i] = mean(tmp, /nan)')     
    l = 0    
    FOR j =  0, nb_arom-1 DO BEGIN
      cmd = execute('iok = where(date_'+strtrim(ind_arom[j],2)+' EQ date, cntok)')
      IF (cntok EQ 1) AND (l EQ 0) THEN cmd = execute('tmp = '+var_list[k]+'_1DTC_'+strtrim(ind_arom[j],2)+'[iok]')
      IF (cntok EQ 1) AND (l GT 0) THEN cmd = execute('tmp = [ tmp, '+var_list[k]+'_1DTC_'+strtrim(ind_arom[j],2)+'[iok] ]')
      IF cntok EQ 1 THEN l = l + 1      
    ENDFOR
    cmd = execute('ave_1DTC_'+var_list[k]+'_arom[i] = mean(tmp, /nan)')         
    l = 0    
    FOR j =  0, nb_aro1-1 DO BEGIN
      cmd = execute('iok = where(date_'+strtrim(ind_aro1[j],2)+' EQ date, cntok)')
      IF (cntok EQ 1) AND (l EQ 0) THEN cmd = execute('tmp = '+var_list[k]+'_1DTC_'+strtrim(ind_aro1[j],2)+'[iok]')
      IF (cntok EQ 1) AND (l GT 0) THEN cmd = execute('tmp = [ tmp, '+var_list[k]+'_1DTC_'+strtrim(ind_aro1[j],2)+'[iok] ]')
      IF cntok EQ 1 THEN l = l + 1      
    ENDFOR
    cmd = execute('ave_1DTC_'+var_list[k]+'_aro1[i] = mean(tmp, /nan)')         
    l = 0    
    FOR j =  0, nb_aro2-1 DO BEGIN
      cmd = execute('iok = where(date_'+strtrim(ind_aro2[j],2)+' EQ date, cntok)')
      IF (cntok EQ 1) AND (l EQ 0) THEN cmd = execute('tmp = '+var_list[k]+'_1DTC_'+strtrim(ind_aro2[j],2)+'[iok]')
      IF (cntok EQ 1) AND (l GT 0) THEN cmd = execute('tmp = [ tmp, '+var_list[k]+'_1DTC_'+strtrim(ind_aro2[j],2)+'[iok] ]')
      IF cntok EQ 1 THEN l = l + 1      
    ENDFOR
    cmd = execute('ave_1DTC_'+var_list[k]+'_aro2[i] = mean(tmp, /nan)')

  ENDFOR

;ENDFOR
STOP





; calcul erreur moyenne par modele
ind_alad = where(exp_list EQ 'ALADIN-OPER', nb_alad)
ind_arom = where(STRMID(exp_list,0,1) EQ '9', nb_arom)
ind_aro1 = where(STRMATCH(alt_list,'*'+param1+'*') EQ 1, nb_aro1)
ind_aro2 = where(STRMATCH(alt_list,'*'+param2+'*') EQ 1, nb_aro2)
maxnbt_alad = 0
FOR j = 0, nb_alad-1 DO cmd = execute('maxnbt_alad = max([maxnbt_alad,n_elements(err_1D_MSLP_SEA_'+strtrim(ind_alad[j],2)+')],/nan)')
maxnbt_arom = 0
FOR j = 0, nb_arom-1 DO cmd = execute('maxnbt_arom = max([maxnbt_arom,n_elements(err_1D_MSLP_SEA_'+strtrim(ind_arom[j],2)+')],/nan)')
help, nb_alad, nb_arom, nb_aro1, nb_aro2
help, maxnbt_alad, maxnbt_arom

; declaration arrays "mean error"
FOR k = 0, nb_var-1 DO BEGIN
  cmd = execute('err_1DTC_'+var_list[k]+'_alad = fltarr(maxnbt_alad)')
  cmd = execute('err_1D_'+var_list[k]+'_alad = fltarr(maxnbt_alad)')
  cmd = execute('err_1DTC_'+var_list[k]+'_arom = fltarr(maxnbt_arom)')
  cmd = execute('err_1D_'+var_list[k]+'_arom = fltarr(maxnbt_arom)')
  cmd = execute('err_1DTC_'+var_list[k]+'_aro1 = fltarr(maxnbt_arom)')
  cmd = execute('err_1D_'+var_list[k]+'_aro1 = fltarr(maxnbt_arom)')
  cmd = execute('err_1DTC_'+var_list[k]+'_aro2 = fltarr(maxnbt_arom)')
  cmd = execute('err_1D_'+var_list[k]+'_aro2 = fltarr(maxnbt_arom)')
ENDFOR
err_1D_RVM_alad = fltarr(maxnbt_alad)
err_1D_RVM_arom = fltarr(maxnbt_arom)
err_1D_RVM_aro1 = fltarr(maxnbt_arom)
err_1D_RVM_aro2 = fltarr(maxnbt_arom)
nbpts_ech_alad  = fltarr(maxnbt_alad)
nbpts_ech_arom  = fltarr(maxnbt_arom)
nbpts_ech_aro1  = fltarr(maxnbt_arom)
nbpts_ech_aro2  = fltarr(maxnbt_arom)


; calculs
FOR k = 0, nb_var-1 DO BEGIN

  FOR i = 0, maxnbt_alad-1 DO BEGIN
    FOR j = 0, nb_alad-1 DO BEGIN
      cmd = execute(' tmp = n_elements(err_1D_'+var_list[k]+'_'+strtrim(ind_alad[j],2)+')')
      IF i LE tmp-1 THEN BEGIN
        IF j EQ 0 THEN cmd = execute('tmp1 = err_1DTC_'+var_list[k]+'_'+strtrim(ind_alad[j],2)+'[i]') $
        ELSE cmd = execute('tmp1 = [tmp1,err_1DTC_'+var_list[k]+'_'+strtrim(ind_alad[j],2)+'[i]]')
        IF j EQ 0 THEN cmd = execute('tmp2 = err_1D_'+var_list[k]+'_'+strtrim(ind_alad[j],2)+'[i]') $
        ELSE cmd = execute('tmp2 = [tmp2,err_1D_'+var_list[k]+'_'+strtrim(ind_alad[j],2)+'[i]]')
      ENDIF
    ENDFOR
    cmd = execute('err_1DTC_'+var_list[k]+'_alad[i]  = mean(tmp1,/nan)')
    cmd = execute('err_1D_'+var_list[k]+'_alad[i]  = mean(tmp2,/nan)')
  ENDFOR

  FOR i = 0, maxnbt_arom-1 DO BEGIN
    FOR j = 0, nb_arom-1 DO BEGIN
      cmd = execute(' tmp = n_elements(err_1D_'+var_list[k]+'_'+strtrim(ind_arom[j],2)+')')
      IF i LE tmp-1 THEN BEGIN
        IF j EQ 0 THEN cmd = execute('tmp1 = err_1DTC_'+var_list[k]+'_'+strtrim(ind_arom[j],2)+'[i]') $
        ELSE cmd = execute('tmp1 = [tmp1,err_1DTC_'+var_list[k]+'_'+strtrim(ind_arom[j],2)+'[i]]')
        IF j EQ 0 THEN cmd = execute('tmp2 = err_1D_'+var_list[k]+'_'+strtrim(ind_arom[j],2)+'[i]') $
        ELSE cmd = execute('tmp2 = [tmp2,err_1D_'+var_list[k]+'_'+strtrim(ind_arom[j],2)+'[i]]')
      ENDIF
    ENDFOR
    cmd = execute('err_1DTC_'+var_list[k]+'_arom[i]  = mean(tmp1,/nan)')
    cmd = execute('err_1D_'+var_list[k]+'_arom[i]  = mean(tmp2,/nan)')
  ENDFOR

  FOR i = 0, maxnbt_arom-1 DO BEGIN
    FOR j = 0, nb_aro1-1 DO BEGIN
      cmd = execute(' tmp = n_elements(err_1D_'+var_list[k]+'_'+strtrim(ind_aro1[j],2)+')')
      IF i LE tmp-1 THEN BEGIN
        IF j EQ 0 THEN cmd = execute('tmp1 = err_1DTC_'+var_list[k]+'_'+strtrim(ind_aro1[j],2)+'[i]') $
        ELSE cmd = execute('tmp1 = [tmp1,err_1DTC_'+var_list[k]+'_'+strtrim(ind_aro1[j],2)+'[i]]')
        IF j EQ 0 THEN cmd = execute('tmp2 = err_1D_'+var_list[k]+'_'+strtrim(ind_aro1[j],2)+'[i]') $
        ELSE cmd = execute('tmp2 = [tmp2,err_1D_'+var_list[k]+'_'+strtrim(ind_aro1[j],2)+'[i]]')
      ENDIF
    ENDFOR
    cmd = execute('err_1DTC_'+var_list[k]+'_aro1[i]  = mean(tmp1,/nan)')
    cmd = execute('err_1D_'+var_list[k]+'_aro1[i]  = mean(tmp2,/nan)')
  ENDFOR

  FOR i = 0, maxnbt_arom-1 DO BEGIN
    FOR j = 0, nb_aro2-1 DO BEGIN
      cmd = execute(' tmp = n_elements(err_1D_'+var_list[k]+'_'+strtrim(ind_aro2[j],2)+')')
      IF i LE tmp-1 THEN BEGIN
        IF j EQ 0 THEN cmd = execute('tmp1 = err_1DTC_'+var_list[k]+'_'+strtrim(ind_aro2[j],2)+'[i]') $
        ELSE cmd = execute('tmp1 = [tmp1,err_1DTC_'+var_list[k]+'_'+strtrim(ind_aro2[j],2)+'[i]]')
        IF j EQ 0 THEN cmd = execute('tmp2 = err_1D_'+var_list[k]+'_'+strtrim(ind_aro2[j],2)+'[i]') $
        ELSE cmd = execute('tmp2 = [tmp2,err_1D_'+var_list[k]+'_'+strtrim(ind_aro2[j],2)+'[i]]')
      ENDIF
    ENDFOR
    cmd = execute('err_1DTC_'+var_list[k]+'_aro2[i]  = mean(tmp1,/nan)')
    cmd = execute('err_1D_'+var_list[k]+'_aro2[i]  = mean(tmp2,/nan)')
  ENDFOR

ENDFOR

; traitement a part du RVM
FOR i = 0, maxnbt_alad-1 DO BEGIN
  FOR j = 0, nb_alad-1 DO BEGIN
    cmd = execute(' tmp = n_elements(err_1D_RVM_'+strtrim(ind_alad[j],2)+')')
    IF i LE tmp-1 THEN BEGIN
      IF j EQ 0 THEN cmd = execute('tmp1 = err_1D_RVM_'+strtrim(ind_alad[j],2)+'[i]') $
      ELSE cmd = execute('tmp1 = [tmp1,err_1D_RVM_'+strtrim(ind_alad[j],2)+'[i]]')
    ENDIF
  ENDFOR
  cmd = execute('err_1D_RVM_alad[i]  = mean(tmp1,/nan)')
  nbpts_ech_alad[i] = n_elements(tmp1)
ENDFOR

FOR i = 0, maxnbt_arom-1 DO BEGIN
  FOR j = 0, nb_arom-1 DO BEGIN
    cmd = execute(' tmp = n_elements(err_1D_RVM_'+strtrim(ind_arom[j],2)+')')
    IF i LE tmp-1 THEN BEGIN
      IF j EQ 0 THEN cmd = execute('tmp1 = err_1D_RVM_'+strtrim(ind_arom[j],2)+'[i]') $
      ELSE cmd = execute('tmp1 = [tmp1,err_1D_RVM_'+strtrim(ind_arom[j],2)+'[i]]')
    ENDIF
  ENDFOR
  cmd = execute('err_1D_RVM_arom[i]  = mean(tmp1,/nan)')
  nbpts_ech_arom[i] = n_elements(tmp1)
ENDFOR

FOR i = 0, maxnbt_arom-1 DO BEGIN
  FOR j = 0, nb_aro1-1 DO BEGIN
    cmd = execute(' tmp = n_elements(err_1D_RVM_'+strtrim(ind_aro1[j],2)+')')
    IF i LE tmp-1 THEN BEGIN
      IF j EQ 0 THEN cmd = execute('tmp1 = err_1D_RVM_'+strtrim(ind_aro1[j],2)+'[i]') $
      ELSE cmd = execute('tmp1 = [tmp1,err_1D_RVM_'+strtrim(ind_aro1[j],2)+'[i]]')
    ENDIF
  ENDFOR
  cmd = execute('err_1D_RVM_aro1[i]  = mean(tmp1,/nan)')
  nbpts_ech_aro1[i] = n_elements(tmp1)  
ENDFOR

FOR i = 0, maxnbt_arom-1 DO BEGIN
  FOR j = 0, nb_aro2-1 DO BEGIN
    cmd = execute(' tmp = n_elements(err_1D_RVM_'+strtrim(ind_aro2[j],2)+')')
    IF i LE tmp-1 THEN BEGIN
      IF j EQ 0 THEN cmd = execute('tmp1 = err_1D_RVM_'+strtrim(ind_aro2[j],2)+'[i]') $
      ELSE cmd = execute('tmp1 = [tmp1,err_1D_RVM_'+strtrim(ind_aro2[j],2)+'[i]]')
    ENDIF
  ENDFOR
  cmd = execute('err_1D_RVM_aro2[i]  = mean(tmp1,/nan)')
  nbpts_ech_aro2[i] = n_elements(tmp1)    
ENDFOR
print, 'CALCUL ERREURS MOYENNES PAR MODELE 1D_TC+1D_DOMAIN OK' & print, ''
STOP


; PLOT ERREURS MOYENNES 1D-TC+1D-DOMAIN EN FONCTION ECHEANCE ONLY
lead_time_alad = indgen(maxnbt_alad)*6 & help, lead_time_alad
lead_time_arom = indgen(maxnbt_arom)*6 & help, lead_time_arom

FOR k = 0, nb_var-1 DO BEGIN
  IF write_ps THEN openps, filename=plt_path+'MEAN_ERR_'+var_list[k]+'_1D'
  cmd = execute('maxplot = max([err_1D_'+var_list[k]+'_alad,err_1D_'+var_list[k]+'_arom,err_1D_'+var_list[k]+'_aro1,err_1D_'+var_list[k]+'_aro2],/nan)')
  cmd = execute('minplot = min([err_1D_'+var_list[k]+'_alad,err_1D_'+var_list[k]+'_arom,err_1D_'+var_list[k]+'_aro1,err_1D_'+var_list[k]+'_aro2],/nan)')
  maxplot = maxplot + 0.05*(maxplot-minplot)
  minplot = minplot - 0.05*(maxplot-minplot)

  cmd = execute('splot, lead_time_alad, err_1D_'+var_list[k]+'_alad, XTICKINTERVAL=12, xminor=2, yrange=[minplot,maxplot], xrange=[0,96], title="DOMAIN MEAN "+var_list[k]+" ERROR ("+unt_list[k]+")", xtitle="LEAD TIME (hours)", ytitle=var_list[k]+" ("+unt_list[k]+")", thick=1, xgridstyle=2, xticklen=1.0, win=0')
  cmd = execute('oplot, lead_time_alad, err_1D_'+var_list[k]+'_alad, color=color_factor*1, thick=2')
  cmd = execute('oplot, lead_time_arom, err_1D_'+var_list[k]+'_arom, color=color_factor*2, thick=2')
  cmd = execute('oplot, lead_time_arom, err_1D_'+var_list[k]+'_aro1, color=color_factor*3, thick=2')
  cmd = execute('oplot, lead_time_arom, err_1D_'+var_list[k]+'_aro2, color=color_factor*4, thick=2')
  xyouts, 0.125, 0.200-0.020*1, 'ALADIN-OPER', /normal, charsize=1.5, charthick=2, color=color_factor*1
  xyouts, 0.125, 0.200-0.020*2, 'AROME-ALL'  , /normal, charsize=1.5, charthick=2, color=color_factor*2
  xyouts, 0.125, 0.200-0.020*3, 'AROME-'+param1, /normal, charsize=1.5, charthick=2, color=color_factor*3
  xyouts, 0.125, 0.200-0.020*4, 'AROME-'+param2, /normal, charsize=1.5, charthick=2, color=color_factor*4
  oplot, indgen(96), indgen(96)*0., thick=1, linestyle=2
  IF write_ps THEN closeps ELSE saveimage, plt_path+''+'MEAN_ERR_'+var_list[k]+'_1D.gif', quality=100

  IF write_ps THEN openps, filename=plt_path+'MEAN_ERR_'+var_list[k]+'_1DTC'
  cmd = execute('maxplot = max([err_1DTC_'+var_list[k]+'_alad,err_1DTC_'+var_list[k]+'_arom,err_1DTC_'+var_list[k]+'_aro1,err_1DTC_'+var_list[k]+'_aro2],/nan)')
  cmd = execute('minplot = min([err_1DTC_'+var_list[k]+'_alad,err_1DTC_'+var_list[k]+'_arom,err_1DTC_'+var_list[k]+'_aro1,err_1DTC_'+var_list[k]+'_aro2],/nan)')
  maxplot = maxplot + 0.05*(maxplot-minplot)
  minplot = minplot - 0.05*(maxplot-minplot)

  cmd = execute('splot, lead_time_alad, err_1DTC_'+var_list[k]+'_alad, XTICKINTERVAL=12, xminor=2, yrange=[minplot,maxplot], xrange=[0,96], title="TC MEAN "+var_list[k]+" ERROR ("+unt_list[k]+")", xtitle="LEAD TIME (hours)", ytitle=var_list[k]+" ("+unt_list[k]+")", xgridstyle=2, xticklen=1.0, thick=1, win=0')
  cmd = execute('oplot, lead_time_alad, err_1DTC_'+var_list[k]+'_alad, color=color_factor*1, thick=2')
  cmd = execute('oplot, lead_time_arom, err_1DTC_'+var_list[k]+'_arom, color=color_factor*2, thick=2')
  cmd = execute('oplot, lead_time_arom, err_1DTC_'+var_list[k]+'_aro1, color=color_factor*3, thick=2')
  cmd = execute('oplot, lead_time_arom, err_1DTC_'+var_list[k]+'_aro2, color=color_factor*4, thick=2')
  xyouts, 0.125, 0.200-0.020*1, 'ALADIN-OPER', /normal, charsize=1.5, charthick=2, color=color_factor*1
  xyouts, 0.125, 0.200-0.020*2, 'AROME-ALL'  , /normal, charsize=1.5, charthick=2, color=color_factor*2
  xyouts, 0.125, 0.200-0.020*3, 'AROME-'+param1, /normal, charsize=1.5, charthick=2, color=color_factor*3
  xyouts, 0.125, 0.200-0.020*4, 'AROME-'+param2, /normal, charsize=1.5, charthick=2, color=color_factor*4
  oplot, indgen(96), indgen(96)*0., thick=1, linestyle=2
  IF write_ps THEN closeps ELSE saveimage, plt_path+''+'MEAN_ERR_'+var_list[k]+'_1DTC.gif', quality=100

ENDFOR

; PLOT MEAN ERROR RVM
IF write_ps THEN openps, filename=plt_path+'MEAN_ERR_RVM_1D'
maxplot = max([err_1D_RVM_alad,err_1D_RVM_arom,err_1D_RVM_aro1,err_1D_RVM_aro2],/nan)
minplot = min([err_1D_RVM_alad,err_1D_RVM_arom,err_1D_RVM_aro1,err_1D_RVM_aro2],/nan)
maxplot = maxplot + 0.05*(maxplot-minplot)
minplot = minplot - 0.05*(maxplot-minplot)

splot, lead_time_alad, err_1D_RVM_alad, XTICKINTERVAL=12, xminor=2, yrange=[minplot,maxplot], xrange=[0,96], title="MEAN RVM ERROR (km)", xtitle="LEAD TIME (hours)", ytitle='RVM ERROR (km)', thick=1, xgridstyle=2, xticklen=1.0, win=0
oplot, lead_time_alad, err_1D_RVM_alad, color=color_factor*1, thick=2
oplot, lead_time_arom, err_1D_RVM_arom, color=color_factor*2, thick=2
oplot, lead_time_arom, err_1D_RVM_aro1, color=color_factor*3, thick=2
oplot, lead_time_arom, err_1D_RVM_aro2, color=color_factor*4, thick=2
xyouts, 0.125, 0.200-0.020*1, 'ALADIN-OPER', /normal, charsize=1.5, charthick=2, color=color_factor*1
xyouts, 0.125, 0.200-0.020*2, 'AROME-ALL'  , /normal, charsize=1.5, charthick=2, color=color_factor*2
xyouts, 0.125, 0.200-0.020*3, 'AROME-'+param1, /normal, charsize=1.5, charthick=2, color=color_factor*3
xyouts, 0.125, 0.200-0.020*4, 'AROME-'+param2, /normal, charsize=1.5, charthick=2, color=color_factor*4
oplot, indgen(96), indgen(96)*0., thick=1, linestyle=2
IF write_ps THEN closeps ELSE saveimage, plt_path+''+'MEAN_ERR_RVM_1D.gif', quality=100


; PLOT VAR 1D-TC+1D-DOMAIN EN FONCTION DATE ONLY (ERREUR A RAJOUTER)
FOR k = 0, nb_var-1 DO BEGIN

  IF var_list[k] EQ 'SST' THEN istart = 0 ELSE istart = 1

  ; calcul min/max 1D TC
  FOR i = istart, nb_exp-1 DO BEGIN
      cmd = execute('var = '+var_list[k]+'_1DTC_'+strtrim(i,2))
      cmd = execute('date = date_'+strtrim(i,2))
      IF i EQ istart THEN maxplot=max(var,/nan) ELSE maxplot=max([maxplot,var],/nan)
      IF i EQ istart THEN minplot=min(var,/nan) ELSE minplot=min([minplot,var],/nan)
      IF i EQ istart THEN maxdate=max(date,/nan) ELSE maxdate=max([maxdate,date],/nan)
      IF i EQ istart THEN mindate=min(date,/nan) ELSE mindate=min([mindate,date],/nan)
  ENDFOR
  maxplot = maxplot + 0.05*(maxplot-minplot)
  minplot = minplot - 0.05*(maxplot-minplot)
  help, minplot, maxplot

  ; plot 1D TC
  IF write_ps THEN openps, filename=plt_path+var_list[k]+'_1DTC'
  FOR i = istart, nb_exp-1 DO BEGIN
    IF var_list[k] EQ 'SST' THEN icolor = i ELSE icolor = (i-1)  
    cmd = execute('var = '+var_list[k]+'_1DTC_'+strtrim(i,2))
    cmd = execute('date = date_'+strtrim(i,2))
    IF i EQ istart THEN splot, date, var, XTICKFORMAT='(I8)', XTICKINTERVAL=1, xminor=4, yrange=[minplot,maxplot], xrange=[mindate,maxdate], title='TC '+var_list[k]+' ('+unt_list[k]+')', xtitle='date', thick=1, win=0, xgridstyle=2, xticklen=1.0, ytitle=var_list[k]+' ('+unt_list[k]+')'
    oplot, date, var, color=color_factor*icolor, thick=1
    IF exp_list[i] EQ 'ALADIN-ANA' THEN oplot, date, var, color=color_factor*icolor, thick=2.5
    IF exp_list[i] EQ 'IBTRACS'    THEN oplot, date, var, color=color_factor*icolor, thick=2.5
    oplot, [0,date[0]], [0,var[0]], psym=1, thick=2, symsize=2, color=color_factor*icolor
    xyouts, 0.125, 0.200-0.020*i, exp_list[i], /normal, charsize=1.5, charthick=2, color=color_factor*icolor
    xyouts, 0.300, 0.200-0.020*i, '('+alt_list[i]+')', /normal, charsize=1.5, charthick=2, color=color_factor*icolor
  ENDFOR
  IF write_ps THEN closeps ELSE saveimage, plt_path+var_list[k]+'_1DTC.gif', quality=100


  ; calcul min/max 1D DOMAIN
  FOR i = istart, nb_exp-1 DO BEGIN
      cmd = execute('var = '+var_list[k]+'_1D_'+strtrim(i,2))
      cmd = execute('date = date_'+strtrim(i,2))
      IF i EQ istart THEN maxplot=max(var,/nan) ELSE maxplot=max([maxplot,var],/nan)
      IF i EQ istart THEN minplot=min(var,/nan) ELSE minplot=min([minplot,var],/nan)
      IF i EQ istart THEN maxdate=max(date,/nan) ELSE maxdate=max([maxdate,date],/nan)
      IF i EQ istart THEN mindate=min(date,/nan) ELSE mindate=min([mindate,date],/nan)
  ENDFOR
  maxplot = maxplot + 0.05*(maxplot-minplot)
  minplot = minplot - 0.05*(maxplot-minplot)
  help, minplot, maxplot

  ; plot 1D DOMAIN
  IF write_ps THEN openps, filename=plt_path+var_list[k]+'_1D'
  FOR i = istart, nb_exp-1 DO BEGIN
    IF var_list[k] EQ 'SST' THEN icolor = i ELSE icolor = (i-1)    
    cmd = execute('var = '+var_list[k]+'_1D_'+strtrim(i,2))
    cmd = execute('date = date_'+strtrim(i,2))
    IF i EQ istart THEN splot, date, var, XTICKFORMAT='(I8)', XTICKINTERVAL=1, xminor=4, yrange=[minplot,maxplot], xrange=[mindate,maxdate], title='DOMAIN '+var_list[k]+' ('+unt_list[k]+')', xtitle='date', thick=1, win=0, xgridstyle=2, xticklen=1.0, ytitle=var_list[k]+' ('+unt_list[k]+')'
    oplot, date, var, color=color_factor*icolor, thick=1
    IF exp_list[i] EQ 'ALADIN-ANA' THEN oplot, date, var, color=color_factor*icolor, thick=2.5
    IF exp_list[i] EQ 'IBTRACS'    THEN oplot, date, var, color=color_factor*icolor, thick=2.5
    oplot, [0,date[0]], [0,var[0]], psym=1, thick=2, symsize=2, color=color_factor*icolor
    xyouts, 0.125, 0.200-0.020*i, exp_list[i], /normal, charsize=1.5, charthick=2, color=color_factor*icolor
    xyouts, 0.300, 0.200-0.020*i, '('+alt_list[i]+')', /normal, charsize=1.5, charthick=2, color=color_factor*icolor
  ENDFOR
  IF write_ps THEN closeps ELSE saveimage, plt_path+var_list[k]+'_1D.gif', quality=100
ENDFOR



; PLOT 1D VENT MAX
FOR i = 0, nb_exp-1 DO BEGIN
  cmd = execute('var = max_w10m_'+strtrim(i,2)+'[*]')
  cmd = execute('date = date_'+strtrim(i,2))
  IF i EQ 0 THEN maxdate=max(date,/nan) ELSE maxdate=max([maxdate,date],/nan)
  IF i EQ 0 THEN mindate=min(date,/nan) ELSE mindate=min([mindate,date],/nan)
  IF i EQ 0 THEN maxplot=max(var,/nan) ELSE maxplot=max([maxplot,var],/nan)
  IF i EQ 0 THEN minplot=min(var,/nan) ELSE minplot=min([minplot,var],/nan)
ENDFOR
maxplot = maxplot + 0.05*(maxplot-minplot)
minplot = minplot - 0.05*(maxplot-minplot)
help, minplot, maxplot

IF write_ps THEN openps, filename=plt_path+'MAXW10M_1DTC'
FOR i = 0, nb_exp-1 DO BEGIN
  cmd = execute('date = date_'+strtrim(i,2))
  cmd = execute('var = max_w10m_'+strtrim(i,2)+'[*]')
  IF i EQ 0 THEN splot, date, var, XTICKFORMAT='(I8)', XTICKINTERVAL=1, xminor=4, yrange=[minplot,maxplot], xrange=[mindate,maxdate], title='TC max 10m wind (m/s)', xtitle='date', thick=1, win=0, xgridstyle=2, xticklen=1.0, ytitle='MAX 10M WIND (m/s)'
  IF exp_list[i] EQ 'IBTRACS' OR exp_list[i] EQ 'ALADIN-ANA' THEN thc=2.5 ELSE thc=1
  oplot, date, var, color=color_factor*i, thick=thc
  oplot, [0,date[0]], [0,var[0]], psym=1, thick=2, symsize=2, color=color_factor*i
  xyouts, 0.125, 0.200-0.020*i, exp_list[i], /normal, charsize=1.5, charthick=2, color=color_factor*i
  xyouts, 0.300, 0.200-0.020*i, '('+alt_list[i]+')', /normal, charsize=1.5, charthick=2, color=color_factor*i
ENDFOR
IF write_ps THEN closeps ELSE saveimage, plt_path+'MAXW10M_1DTC.gif', quality=100



; PLOT 1D MSLP MIN
FOR i = 0, nb_exp-1 DO BEGIN
  cmd = execute('var = min_mslp_'+strtrim(i,2)+'[*]')
  cmd = execute('date = date_'+strtrim(i,2))
  IF i EQ 0 THEN maxdate=max(date,/nan) ELSE maxdate=max([maxdate,date],/nan)
  IF i EQ 0 THEN mindate=min(date,/nan) ELSE mindate=min([mindate,date],/nan)
  IF i EQ 0 THEN maxplot=max(var,/nan) ELSE maxplot=max([maxplot,var],/nan)
  IF i EQ 0 THEN minplot=min(var,/nan) ELSE minplot=min([minplot,var],/nan)
ENDFOR
maxplot = maxplot + 0.05*(maxplot-minplot)
minplot = minplot - 0.05*(maxplot-minplot)
help, minplot, maxplot

IF write_ps THEN openps, filename=plt_path+'MINMSLP_1DTC'
FOR i = 0, nb_exp-1 DO BEGIN
  cmd = execute('date = date_'+strtrim(i,2))
  cmd = execute('var = min_mslp_'+strtrim(i,2)+'[*]')
  IF i EQ 0 THEN splot, date, var, XTICKFORMAT='(I8)', xminor=4, XTICKINTERVAL=1, yrange=[minplot,maxplot], xrange=[mindate,maxdate], title='TC MIN MSLP (hPa)', xtitle='date', thick=1, win=0, xgridstyle=2, xticklen=1.0, ytitle='MIN MSLP (hPa)'
  IF exp_list[i] EQ 'IBTRACS' OR exp_list[i] EQ 'ALADIN-ANA' THEN thc=2.5 ELSE thc=1
  oplot, date, var, color=color_factor*i, thick=thc
  oplot, [0,date[0]], [0,var[0]], psym=1, thick=2, symsize=2, color=color_factor*i
  xyouts, 0.125, 0.200-0.020*i, exp_list[i], /normal, charsize=1.5, charthick=2, color=color_factor*i
  xyouts, 0.300, 0.200-0.020*i, '('+alt_list[i]+')', /normal, charsize=1.5, charthick=2, color=color_factor*i
ENDFOR
IF write_ps THEN closeps ELSE saveimage, plt_path+'MINMSLP_1DTC.gif', quality=100



; PLOT 1D RVM
FOR i = 0, nb_exp-1 DO BEGIN
;  cmd = execute('var = rvm_'+strtrim(i,2)+'[*]')
  cmd = execute('var = RVM_1DTC_'+strtrim(i,2)+'[*]')
  cmd = execute('date = date_'+strtrim(i,2))
  IF i EQ 1 THEN maxdate=max(date,/nan) ELSE maxdate=max([maxdate,date],/nan)
  IF i EQ 1 THEN mindate=min(date,/nan) ELSE mindate=min([mindate,date],/nan)
  IF i EQ 1 THEN maxplot=max(var,/nan) ELSE maxplot=max([maxplot,var],/nan)
  IF i EQ 1 THEN minplot=min(var,/nan) ELSE minplot=min([minplot,var],/nan)
ENDFOR
maxplot = maxplot + 0.05*(maxplot-minplot)
minplot = minplot - 0.05*(maxplot-minplot)
help, minplot, maxplot

IF write_ps THEN openps, filename=plt_path+'RVM_1DTC'
FOR i = 0, nb_exp-1 DO BEGIN

  IF STRMATCH(exp_list[i],'*ALADIN-OPER*') EQ 1 THEN icol=2
  IF STRMATCH(exp_list[i],'9*') EQ 1 THEN icol=3
  IF (STRMATCH(exp_list[i],'9*') NE 1) AND (STRMATCH(exp_list[i],'*ALADIN-OPER*') NE 1) THEN icol=i
  cmd = execute('date = date_'+strtrim(i,2))
  cmd = execute('var  = RVM_1DTC_'+strtrim(i,2)+'[*]')
  IF i EQ 0 THEN splot, date, var, XTICKFORMAT='(I8)', xminor=4, XTICKINTERVAL=1, yrange=[minplot,maxplot], xrange=[mindate,maxdate], title='RADIUS OF MAXIMUM WIND (km)', xtitle='date', thick=1, win=0, xgridstyle=2, xticklen=1.0, ytitle='RADIUS (km)'
  IF exp_list[i] EQ 'IBTRACS' OR exp_list[i] EQ 'ALADIN-ANA' THEN thc=2.5 ELSE thc=1
  oplot, date, var, color=color_factor*icol, thick=thc
  oplot, [0,date[0]], [0,var[0]], psym=1, thick=2, symsize=2, color=color_factor*icol
  xyouts, 0.125, 0.200-0.020*i, exp_list[i], /normal, charsize=1.5, charthick=2, color=color_factor*icol
  xyouts, 0.300, 0.200-0.020*i, '('+alt_list[i]+')', /normal, charsize=1.5, charthick=2, color=color_factor*icol
ENDFOR
IF write_ps THEN closeps ELSE saveimage, plt_path+'RVM_1DTC.gif', quality=100
