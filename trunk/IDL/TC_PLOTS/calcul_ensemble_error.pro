; calcul des erreurs de distance, vent et pression par experience

err_list = [ 'errdist', 'errwind', 'errwrad', 'errmslp', 'err_rmw', 'err_sst' ]
nb_err   = n_elements(err_list)


; determination du nombre d'echeance max de toutes les simus
maxnbt_alad = !VALUES.F_NAN
maxnbt_arom = !VALUES.F_NAN
IF ind_alad[0] NE -1 THEN FOR i = 0, n_elements(nb_alad)-1 DO $
cmd = execute('maxnbt_alad = fix(max([maxnbt_alad,tdim_'+strtrim(ind_alad[i],2)+'], /nan))') $
ELSE maxnbt_alad = 96 / 6 + 1
IF ind_arom[0] NE -1 THEN FOR i = 0, n_elements(nb_arom)-1 DO $
cmd = execute('maxnbt_arom = fix(max([maxnbt_arom,tdim_'+strtrim(ind_arom[i],2)+'], /nan))') $
ELSE maxnbt_arom = 96 / 6 + 1
help, maxnbt_alad, maxnbt_arom


; calcul des erreurs par simus
FOR i = 1, nb_exp-1 DO BEGIN

  FOR j = 0, nb_err-1 DO cmd = execute( err_list[j]+'_'+strtrim(i,2)+' = FLTARR(maxnbt_arom) + !VALUES.F_NAN' )

  ; recherche des correspondances entre dates best-track et dates simus
  cmd = execute('date = date_'+strtrim(i,2))  
  indok     = (listmatch(date,date_0))[*,0]
  indok_obs = (listmatch(date,date_0))[*,1]
  cmd = execute('date_err_'+strtrim(i,2)+' = date[indok]')

  ; calcul des erreurs pour chaques simus
  cmd = execute('errdist_'+strtrim(i,2)+'[indok] = reform(map_npoints(lon_mslp_'+strtrim(i,2)+'[indok],lat_mslp_'+strtrim(i,2)+'[indok],'+''+$
                                                  'lon_mslp_0[indok_obs],lat_mslp_0[indok_obs], /two_by_two))/1000. & help, errdist_'+strtrim(i,2))
  cmd = execute('errwind_'+strtrim(i,2)+'[indok] = max_w10m_'+strtrim(i,2)+'[indok]-max_w10m_0[indok_obs] & help, errwind_'+strtrim(i,2))
  cmd = execute('errwrad_'+strtrim(i,2)+'[indok] = MAX_W10M_RADTC_'+strtrim(i,2)+'[indok]-max_w10m_0[indok_obs] & help, errwrad_'+strtrim(i,2))
  cmd = execute('errmslp_'+strtrim(i,2)+'[indok] = min_mslp_'+strtrim(i,2)+'[indok]-min_mslp_0[indok_obs] & help, errmslp_'+strtrim(i,2))
  cmd = execute('err_rmw_'+strtrim(i,2)+'[indok] = rvm_1dtc_'+strtrim(i,2)+'[indok]-rvm_1dtc_0[indok_obs] & help, err_rmw_'+strtrim(i,2))
  cmd = execute('err_sst_'+strtrim(i,2)+'[indok] = sst_1dtc_'+strtrim(i,2)+'[indok]-sstm_1dtc_0[indok_obs] & help, err_sst_'+strtrim(i,2))

ENDFOR



; declaration erreur moyenne par modele et par critere
FOR i = 0, nb_err-1 DO BEGIN
;  IF ind_alad[0] NE -1 THEN
  cmd = execute( err_list[i]+'_alad = FLTARR(maxnbt_alad) + !VALUES.F_NAN' )
;  IF ind_arom[0] NE -1 THEN
  cmd = execute( err_list[i]+'_arom = FLTARR(maxnbt_arom) + !VALUES.F_NAN' )
  FOR j = 0, n_elements(par_list)-1 DO cmd = execute( err_list[i]+'_aro'+strtrim(j,2)+' = FLTARR(maxnbt_arom) + !VALUES.F_NAN ')
ENDFOR



; calcul erreurs ALADIN
IF ind_alad[0] NE -1 THEN BEGIN
  FOR i = 0, maxnbt_alad-1 DO BEGIN
    FOR j = 0, nb_alad-1 DO BEGIN
      cmd = execute(' tmp = n_elements(errdist_'+strtrim(ind_alad[j],2)+')')
      IF i LE tmp-1 THEN BEGIN
	IF j EQ 0 THEN cmd = execute('tmp1 = errdist_'+strtrim(ind_alad[j],2)+'[i]') ELSE cmd = execute('tmp1 = [tmp1,errdist_'+strtrim(ind_alad[j],2)+'[i]]')
	IF j EQ 0 THEN cmd = execute('tmp2 = errwind_'+strtrim(ind_alad[j],2)+'[i]') ELSE cmd = execute('tmp2 = [tmp2,errwind_'+strtrim(ind_alad[j],2)+'[i]]')
	IF j EQ 0 THEN cmd = execute('tmp4 = errwrad_'+strtrim(ind_alad[j],2)+'[i]') ELSE cmd = execute('tmp4 = [tmp2,errwrad_'+strtrim(ind_alad[j],2)+'[i]]')
	IF j EQ 0 THEN cmd = execute('tmp3 = errmslp_'+strtrim(ind_alad[j],2)+'[i]') ELSE cmd = execute('tmp3 = [tmp3,errmslp_'+strtrim(ind_alad[j],2)+'[i]]')
	IF j EQ 0 THEN cmd = execute('tmp5 = err_rmw_'+strtrim(ind_alad[j],2)+'[i]') ELSE cmd = execute('tmp5 = [tmp5,err_rmw_'+strtrim(ind_alad[j],2)+'[i]]')
	IF j EQ 0 THEN cmd = execute('tmp6 = err_sst_'+strtrim(ind_alad[j],2)+'[i]') ELSE cmd = execute('tmp6 = [tmp6,err_sst_'+strtrim(ind_alad[j],2)+'[i]]')
      ENDIF
    ENDFOR
    errdist_alad[i]  = mean(tmp1,/nan); & undefine, tmp1
    errwind_alad[i]  = mean(tmp2,/nan); & undefine, tmp2
    errwrad_alad[i]  = mean(tmp4,/nan); & undefine, tmp4
    errmslp_alad[i]  = mean(tmp3,/nan); & undefine, tmp3
    err_rmw_alad[i]  = mean(tmp5,/nan); & undefine, tmp5
    err_sst_alad[i]  = mean(tmp6,/nan); & undefine, tmp6
  ENDFOR
ENDIF


; AROME
IF ind_arom[0] NE -1 THEN BEGIN

  ; declaration
  nb_maxexp = !VALUES.F_NAN  
  FOR i = 0, nb_par-1 DO cmd = execute('nb_maxexp = max([nb_maxexp, nb_aro'+strtrim(i,2)+'], /nan)')
  FOR i = 0, nb_err-1 DO cmd = execute('all_'+err_list[i]+' = FLTARR( nb_par, maxnbt_arom, nb_maxexp) + !VALUES.F_NAN' )

  ; calcul erreur moyenne par critere
  FOR m = 0, nb_par-1 DO BEGIN
    cmd = execute(' nb_aro =  nb_aro'+strtrim(m,2))
    cmd = execute('ind_aro = ind_aro'+strtrim(m,2))
    FOR k = 0, nb_err-1 DO BEGIN
      var = err_list[k]
      tmp = fltarr(nb_aro, maxnbt_arom) + !VALUES.F_NAN
      FOR j = 0, nb_aro-1 DO BEGIN
        cmd = execute( 'tmp[j,*] = '+var+'_'+strtrim(ind_aro[j],2) )
        cmd = execute( 'all_'+var+'[m,*,'+strtrim(j,2)+'] = '+var+'_'+strtrim(ind_aro[j],2) )
      ENDFOR
      cmd = execute( var+'_aro'+strtrim(m,2)+' = mean(tmp, DIM=1, /NAN)')
      cmd = execute( 'help, '+var+'_aro'+strtrim(m,2) )
    ENDFOR
  ENDFOR

  ; calcul erreur moyenne pour toutes les simus
  FOR k = 0, nb_err-1 DO BEGIN
    var = err_list[k]
    tmp = fltarr(nb_arom, maxnbt_arom) + !VALUES.F_NAN
    FOR j = 0, nb_arom-1 DO cmd = execute( 'tmp[j,*] = '+var+'_'+strtrim(ind_arom[j],2) )
    cmd = execute( var+'_arom = mean(tmp, DIM=1, /NAN)')
    cmd = execute( 'help, '+var+'_arom' )
  ENDFOR

ENDIF


; STATISTICAL SIGNIFICANCE
IF nb_par EQ 2 THEN BEGIN

  ; declarations
  FOR i = 0, nb_err-1 DO cmd = execute( 'tmtest_'+err_list[i]+'_arom = FLTARR(maxnbt_arom) + !VALUES.F_NAN' )

  ; calcul test
  FOR i = 0, maxnbt_arom-1 DO BEGIN
    FOR j = 0, nb_err-1 DO $
    cmd = execute( 'tmtest_'+err_list[j]+'_arom[i] = (TM_TEST(REFORM(all_'+err_list[j]+'[0,i,where(finite(all_'+err_list[j]+'[0,i,*]))]), '+''+$
                                                             'REFORM(all_'+err_list[j]+'[1,i,where(finite(all_'+err_list[j]+'[1,i,*]))])))[1]' )
  ENDFOR

  ; 1 = ok
  FOR i = 0, nb_err-1 DO BEGIN
    cmd = execute( 'tmtest_'+err_list[j]+'_arom[where(tmtest_'+err_list[j]+'_arom LE 0.10, complement=bad)] = 1 & tmtest_'+err_list[j]+'_arom[bad] = 0' )
    cmd = execute( 'tmtest_'+err_list[j]+'_arom = fix(tmtest_'+err_list[j]+'_arom)' )
  ENDFOR

ENDIF
