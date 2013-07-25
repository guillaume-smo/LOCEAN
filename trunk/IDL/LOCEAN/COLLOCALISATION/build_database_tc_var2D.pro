PRO build_database_tc_var2D
  @common
  
; Begin define variables to collocalise and chosen period
  datebeg_tc = 19900101 & datebeg_sf = datebeg_tc & datebeg_ok = datebeg_tc
  dateend_tc = 20100101 & dateend_sf = dateend_tc & dateend_ok = dateend_tc
  freq    = '6H'
  model   = 'WRF' ; WRF / NEMO / OBS
  explist = ['FORCED_SW2_KF']
  varlist = ['SST']
  ficlist = ['SST']
  basin   = ['IO']
  mean_radius = 200.           ; rayon utilise pour calculer les moyennes en km
; End define variables to collocalise and chosen period


  FOR iexp = 0, n_elements(explist)-1 DO BEGIN
  expname = explist[iexp]
;  IF expname EQ 'FORCED_SW2_KF' THEN dateend_tc = 19990101 ELSE dateend_tc = 20100101 & dateend_sf = dateend_tc & dateend_ok = dateend_tc
;  IF expname EQ 'FORCED_SW2_KF' THEN period = '1990-1998' ELSE period = '1990-2009'
  indir = '/net/adonis/usr/adonis/varclim/gslod/IDL/TRACKER/EXP_'+ expname +'/DATA/'
  tcdir = '/net/adonis/usr/adonis/varclim/gslod/IDL/COLLOCALISATION/EXP_'+ expname +'/DATA/'
  path_file_tc_d0 = indir+'d0_TRACKS_'+expname+'_'+basin+'_'+period+'.dat'
  path_file_tc_d1 = indir+'d1_TRACKS_'+expname+'_'+basin+'_'+period+'.dat'

  FOR ivar = 0, n_elements(varlist)-1 DO BEGIN
  var_nm = varlist[ivar]
  fic_nm = ficlist[ivar]
  indir  = '/net/adonis/usr/adonis/varclim/gslod/EXP_'+ expname +'/'+ var_nm +'/' & help, indir
  path_file_wsc_wcy = indir+fic_nm+'_'+period+'.nc' & help, path_file_wsc_wcy
  path_file_nsc_wcy = indir+fic_nm+'_nosc_'+period+'.nc' & help, path_file_nsc_wcy
  fileout = tcdir+var_nm+'_NEW_2D_'+strtrim(mean_radius,2)+'km_'+expname+'_'+freq+'_'+basin+'_'+strtrim(datebeg_ok,2)+'-'+strtrim(dateend_ok,2)+'.dat'   

  colloc_data2D_to_tc, path_file_wsc_wcy, path_file_nsc_wcy, path_file_nsc_ncy, var_nm, basin,datebeg_sf,datebeg_ok,dateend_ok, expname, freq, path_file_tc_d0, path_file_tc_d1, fileout, model, mean_radius
  
  ENDFOR
  ENDFOR

END
