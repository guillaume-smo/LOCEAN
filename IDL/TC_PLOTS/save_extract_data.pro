print, '' & print, 'SAVING EXTRACT DATA...'


IF i EQ 1 THEN idate = -1
idate     = (idate + 1) MOD n_elements(date_list)
save_path = '/home/gsamson/WORK/AROME/TEST_CPL/EXPS_'+STRMID(exp_name, 0, 7)+'/EXP_'+exp_name+'_'+date_list[idate]+'/'
expid     = strtrim(i,2)


save_list = [ 'file', 'date', 'juld', 'xdim', 'ydim', 'lon', 'lat', 'tdim', 'lon_maxwnd', 'lat_maxwnd', 'max_w10m', 'min_mslp', 'lon_mslp', 'lat_mslp', 'RVM_1DTC', 'MAX_W10M_RADTC' ]
FOR k = 0, n_elements(save_list)-1 DO cmd = execute( save_list[k]+'='+save_list[k]+'_'+expid )
save_file = 'DATA_TC_R'+STRTRIM(ROUND(radius),2)+'km.idl'
cmd = execute( 'SAVE, '+STRJOIN(save_list,', ', /SINGLE)+', FILENAME="'+save_path+save_file+'", /VERBOSE')


save_file = 'VAR_1DTC_R'+STRTRIM(ROUND(radius),2)+'km.idl'
save_list = STRARR(nb_var)
FOR k = 0, nb_var-1 DO save_list[k] = var_list[k]+'_1DTC' ;_'+expid
FOR k = 0, nb_var-1 DO cmd = execute( save_list[k]+'='+save_list[k]+'_'+expid )
cmd = execute( 'SAVE, '+STRJOIN(save_list,', ', /SINGLE)+', FILENAME="'+save_path+save_file+'", /VERBOSE')


save_file = 'VAR_2DTC_R'+STRTRIM(ROUND(radius),2)+'km.idl'
save_list = STRARR(nb_var+2)
cmd = execute( 'save_list[nb_var+2-1] = "lon_2dtc"') ;_'+strtrim(i,2)+'"' )
cmd = execute( 'save_list[nb_var+2-2] = "lat_2dtc"') ;_'+strtrim(i,2)+'"' )
FOR k = 0, nb_var-1 DO save_list[k] = var_list[k]+'_2DTC' ;_'+expid
FOR k = 0, nb_var+2-1 DO cmd = execute( save_list[k]+'='+save_list[k]+'_'+expid )
cmd = execute( 'SAVE, '+STRJOIN(save_list,', ', /SINGLE)+', FILENAME="'+save_path+save_file+'", /VERBOSE')


save_file = 'VAR_1D.idl'
save_list = STRARR(nb_var)
FOR k = 0, nb_var-1 DO save_list[k] = var_list[k]+'_1D' ;_'+expid
FOR k = 0, nb_var-1 DO cmd = execute( save_list[k]+'='+save_list[k]+'_'+expid )
cmd = execute( 'SAVE, '+STRJOIN(save_list,', ', /SINGLE)+', FILENAME="'+save_path+save_file+'", /VERBOSE')


print, 'SAVE EXTRACT OK' & print, ''
