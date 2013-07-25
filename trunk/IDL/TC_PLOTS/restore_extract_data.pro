print, '' & print, 'RESTORING DATA...'


IF i EQ 1 THEN idate = -1
idate     = (idate + 1) MOD n_elements(date_list)
save_path = '/home/gsamson/WORK/AROME/TEST_CPL/EXPS_'+STRMID(exp_name, 0, 7)+'/EXP_'+exp_name+'_'+date_list[idate]+'/'
expid     = strtrim(i,2)

save_file = 'DATA_TC_R'+STRTRIM(ROUND(radius),2)+'km.idl'
RESTORE, FILENAME=save_path+save_file, /VERBOSE

save_file = 'VAR_1DTC_R'+STRTRIM(ROUND(radius),2)+'km.idl'
RESTORE, FILENAME=save_path+save_file, /VERBOSE

save_file = 'VAR_2DTC_R'+STRTRIM(ROUND(radius),2)+'km.idl'
RESTORE, FILENAME=save_path+save_file, /VERBOSE

save_file = 'VAR_1D.idl'
RESTORE, FILENAME=save_path+save_file, /VERBOSE


print, 'RESTORE OK'
