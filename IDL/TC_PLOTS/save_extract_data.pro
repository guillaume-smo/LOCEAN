print, '' & print, 'SAVING EXTRACT DATA...'


IF i EQ 1 THEN idate = -1
idate     = (idate + 1) MOD n_elements(date_list)
save_path = '/home/gsamson/WORK/AROME/TEST_CPL/EXPS_'+STRMID(exp_name, 0, 7)+'/EXP_'+exp_name+'_'+date_list[idate]+'/'
expid     = strtrim(i,2)

save_file = 'DATA_TC_R'+STRTRIM(ROUND(radius),2)+'km.idl'
cmd = execute( 'SAVE, file_'+expid+', date_'+expid+', juld_'+expid+', xdim_'+expid+', ydim_'+expid+', lon_'+expid+', lat_'+expid+', tdim_'+expid+', lon_maxwnd_'+expid+', lat_maxwnd_'+expid+', max_w10m_'+expid+', lon_minwnd_'+expid+', lat_minwnd_'+expid+', min_mslp_'+expid+', lon_mslp_'+expid+', lat_mslp_'+expid+', RVM_1DTC_'+expid+', MAX_W10M_RADTC_'+expid+', FILENAME="'+save_path+save_file+'", /VERBOSE')

save_file = 'VAR_1DTC_R'+STRTRIM(ROUND(radius),2)+'km.idl'
save_list = STRARR(nb_var)
FOR k = 0, nb_var-1 DO save_list[k] = var_list[k]+'_1DTC_'+expid
cmd = execute( 'SAVE, '+STRJOIN(save_list,', ', /SINGLE)+', FILENAME="'+save_path+save_file+'", /VERBOSE')

save_file = 'VAR_2DTC_R'+STRTRIM(ROUND(radius),2)+'km.idl'
save_list = STRARR(nb_var+2)
cmd = execute( 'save_list[nb_var+2-1] = "lon_2dtc_'+strtrim(i,2)+'"' )
cmd = execute( 'save_list[nb_var+2-2] = "lat_2dtc_'+strtrim(i,2)+'"' )
FOR k = 0, nb_var-1 DO save_list[k] = var_list[k]+'_2DTC_'+expid
cmd = execute( 'SAVE, '+STRJOIN(save_list,', ', /SINGLE)+', FILENAME="'+save_path+save_file+'", /VERBOSE')

save_file = 'VAR_1D.idl'
save_list = STRARR(nb_var)
FOR k = 0, nb_var-1 DO save_list[k] = var_list[k]+'_1D_'+expid
cmd = execute( 'SAVE, '+STRJOIN(save_list,', ', /SINGLE)+', FILENAME="'+save_path+save_file+'", /VERBOSE')


print, 'SAVE EXTRACT OK' & print, ''
