; CALCUL DE LA MOYENNE DENSEMBLE DES RUNS ALADINS + AROME + CRITERES
print, '' & print, 'CALCUL MOYENNE ENSEMBLE PAR DATE...'


; liste des variables et des experiences
cal_list = [ 'max_w10m', 'MAX_W10M_RADTC', 'min_mslp', 'RVM_1DTC', 'lon_mslp', 'lat_mslp', 'SST_1DTC' ]
FOR i = 0, n_elements(sup_list)-1 DO cal_list = [ cal_list, sup_list[i]+'_1DTC' ]
nb_cal   = n_elements(cal_list)
ind_alad = where(exp_list EQ 'ALADIN-OPER', nb_alad) & help, nb_alad
;ind_alan = where(exp_list EQ 'ALADIN-ANA' , nb_alan) & help, nb_alan
ind_arom = where((STRMATCH(exp_list,'*'+'CPL'+'*') EQ 1) OR (STRMATCH(exp_list,'*'+'AROME'+'*') EQ 1), nb_arom) & help, nb_arom
FOR i = 0, n_elements(par_list)-1 DO cmd = execute('ind_aro'+strtrim(i,2)+' = where(STRMATCH(exp_list,"*"+par_list[i]+"*") EQ 1, nb_aro'+strtrim(i,2)+')')
FOR i = 0, n_elements(par_list)-1 DO cmd = execute('help, nb_aro'+strtrim(i,2))



FOR k = 0, nb_cal-1 DO BEGIN

  calvar = cal_list[k] & help, calvar
  cmd = execute('ave_'+calvar+'_alad = fltarr(tdim_0) + !VALUES.F_NAN')
  cmd = execute('std_'+calvar+'_alad = fltarr(tdim_0) + !VALUES.F_NAN')  
  cmd = execute('ave_'+calvar+'_arom = fltarr(tdim_0) + !VALUES.F_NAN')
  cmd = execute('std_'+calvar+'_arom = fltarr(tdim_0) + !VALUES.F_NAN')  
  FOR i = 0, n_elements(par_list)-1 DO cmd = execute('ave_'+calvar+'_aro'+strtrim(i,2)+' = fltarr(tdim_0) + !VALUES.F_NAN')
  FOR i = 0, n_elements(par_list)-1 DO cmd = execute('std_'+calvar+'_aro'+strtrim(i,2)+' = fltarr(tdim_0) + !VALUES.F_NAN')  

  FOR i = 0, tdim_0-1 DO BEGIN

    date = date_0[i]; & print, i, date, f='(I2,1x,F11.2)'

    ; ALADIN
    IF ind_alad[0] NE -1 THEN BEGIN
      l = 0
      FOR j = 0, nb_alad-1 DO BEGIN
	cmd = execute('test = n_elements('+calvar+'_'+strtrim(ind_alad[j],2)+')')
	IF test GT 0 THEN BEGIN
	  cmd = execute('iok = where(date_'+strtrim(ind_alad[j],2)+' EQ date, cntok)')
	  IF (cntok EQ 1) AND (l EQ 0) THEN cmd = execute('tmp = '+calvar+'_'+strtrim(ind_alad[j],2)+'[iok]')
	  IF (cntok EQ 1) AND (l GT 0) THEN cmd = execute('tmp = [ tmp, '+calvar+'_'+strtrim(ind_alad[j],2)+'[iok] ]')
	  IF  cntok EQ 1 THEN l = l + 1
	ENDIF
      ENDFOR
      IF l GT 0 THEN cmd = execute('ave_'+calvar+'_alad[i] = mean(tmp, /nan)')
      IF l GT 0 THEN cmd = execute('std_'+calvar+'_alad[i] = stddev(tmp, /nan)')
    ENDIF
    undefine, tmp
    
    ; AROME
    IF ind_arom[0] NE -1 THEN BEGIN
      l = 0
      FOR j = 0, nb_arom-1 DO BEGIN
	cmd = execute('iok = where(date_'+strtrim(ind_arom[j],2)+' EQ date, cntok)')
	IF (cntok EQ 1) AND (l EQ 0) THEN cmd = execute('tmp = '+calvar+'_'+strtrim(ind_arom[j],2)+'[iok]')
	IF (cntok EQ 1) AND (l GT 0) THEN cmd = execute('tmp = [ tmp, '+calvar+'_'+strtrim(ind_arom[j],2)+'[iok] ]')
	IF  cntok EQ 1 THEN l = l + 1
	IF l GT 0 THEN cmd = execute('ave_'+calvar+'_arom[i] = mean(tmp, /nan)')
        IF l GT 0 THEN cmd = execute('std_'+calvar+'_arom[i] = stddev(tmp, /nan)')
      ENDFOR
      undefine, tmp

      ; CRITERES
      FOR m = 0, n_elements(par_list)-1 DO BEGIN
	l = 0
	cmd = execute(' nb_aro =  nb_aro'+strtrim(m,2))
	cmd = execute('ind_aro = ind_aro'+strtrim(m,2))
	FOR j = 0, nb_aro-1 DO BEGIN
	  cmd = execute('iok = where(date_'+strtrim(ind_aro[j],2)+' EQ date, cntok)')
	  IF (cntok EQ 1) AND (l EQ 0) THEN cmd = execute('tmp = '+calvar+'_'+strtrim(ind_aro[j],2)+'[iok]')
	  IF (cntok EQ 1) AND (l GT 0) THEN cmd = execute('tmp = [ tmp, '+calvar+'_'+strtrim(ind_aro[j],2)+'[iok] ]')
	  IF  cntok EQ 1 THEN l = l + 1
	ENDFOR
	IF l GT 0 THEN cmd = execute('ave_'+calvar+'_aro'+strtrim(m,2)+'[i] = mean(tmp, /nan)')
        IF l GT 0 THEN cmd = execute('std_'+calvar+'_aro'+strtrim(m,2)+'[i] = stddev(tmp, /nan)')	
      ENDFOR
      undefine, tmp

    ENDIF

  ENDFOR
ENDFOR
print, 'CALCUL MOYENNE ENSEMBLE OK' & print, ''
