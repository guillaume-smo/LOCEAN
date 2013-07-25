PRO build_database_tc_var3D
@common
  
; Begin define variables to collocalise and chosen period
datebeg_tc  = 19900101
datebeg_sf  = 19900101
datebeg_ok  = 19900101
freq = '6H'
model = 'WRF'
explist = ['COUPLED_SW2_BMJ']
varlist = ['T']
basin   = ['IO']
mean_radius = 500 & help, mean_radius
; End define variables to collocalise and chosen period


FOR iexp = 0, n_elements(explist)-1 DO BEGIN
  expname = explist[iexp]
  IF expname EQ 'FORCED_SW2_KF' THEN dateend_tc = 19990101 ELSE dateend_tc = 20100101 & dateend_sf = dateend_tc & dateend_ok = dateend_tc
  tcdir = '/net/adonis/usr/adonis/varclim/gslod/IDL/COLLOCALISATION/EXP_'+ expname +'/DATA/'
  path_file_tc_d0 = tcdir+'d0_TRACKS_'+expname+'_'+basin+'_'+strtrim(datebeg_tc,2)+'-'+strtrim(dateend_tc,2)+'.dat'
  path_file_tc_d1 = tcdir+'d1_TRACKS_'+expname+'_'+basin+'_'+strtrim(datebeg_tc,2)+'-'+strtrim(dateend_tc,2)+'.dat'

  FOR ivar = 0, n_elements(varlist)-1 DO BEGIN
    var_nm = varlist[ivar]
    indir  = '/net/adonis/usr/adonis/varclim/gslod/EXP_'+ expname +'/WRFOUT/' & help, indir
    fileout = tcdir+var_nm+'_RZ_'+strtrim(mean_radius,2)+'km_'+expname+'_'+freq+'_'+basin+'_'+strtrim(datebeg_ok,2)+'-'+strtrim(dateend_ok,2)+'.dat'   

    colloc_data3D_to_tc, tcdir, var_nm, basin,datebeg_sf,datebeg_ok,dateend_ok, expname, freq, path_file_tc_d0, path_file_tc_d1, fileout, model, mean_radius, indir
  
  ENDFOR
ENDFOR

END
