print, '' & print, 'SAVING ERROR DATA...'


FOR i = 1, nb_exp-1 DO BEGIN

  exp_name = exp_list[i]
  IF i EQ 1 THEN idate = -1
  idate     = (idate + 1) MOD n_elements(date_list)
  save_path = '/home/gsamson/WORK/AROME/TEST_CPL/EXPS_'+STRMID(exp_name, 0, 7)+'/EXP_'+exp_name+'_'+date_list[idate]+'/'
  expid     = strtrim(i,2)
  save_file = 'ERRORS_TC.idl'
  cmd = execute( 'SAVE, date_err_'+expid+', errdist_'+expid+', errwind_'+expid+', errwrad_'+expid+', errmslp_'+expid+', err_rmw_'+expid+', err_sst_'+expid+', FILENAME="'+save_path+save_file+'", /VERBOSE')

ENDFOR


err_list = [ 'errdist', 'errwind', 'errwrad', 'errmslp', 'err_rmw', 'err_sst' ]
nb_var   = n_elements(err_list)
save_path = '/home/gsamson/WORK/AROME/TEST_CPL/EXPS_'+STRMID(exp_name, 0, 7)+'/'

FOR i = 0, nb_par-1 DO BEGIN

  expid     = strtrim(i,2)
  save_file = 'MEAN_ERROR_'+par_list[i]+'.idl'
  save_list = STRARR(nb_var)

  FOR j = 0, nb_var-1 DO BEGIN
    cmd = execute(err_list[j]+'_'+par_list[i]+' = '+err_list[j]+'_aro'+expid)
    save_list[j] = err_list[j]+'_'+par_list[i]
  ENDFOR
  
  cmd = execute( 'SAVE, '+STRJOIN(save_list,', ', /SINGLE)+', FILENAME="'+save_path+save_file+'", /VERBOSE')

ENDFOR


print, 'SAVE OK' & print, ''
