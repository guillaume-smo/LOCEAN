PRO build_database_tc_var1D
  ; for any 2D field, performs the collocalisation with tracks data
  ; and write output files of:
  ; d1 (1dimension= along track) after-before TC passage,
  ; dt (time series) relative to TC passage at each track point
  
  @common
  
; Begin define variables to collocalise and chosen period
  datebeg_tc = 19900101   & dateend_tc = 20100101
  datebeg_sf = datebeg_tc & dateend_sf = dateend_tc
  datebeg_ok = datebeg_tc & dateend_ok = dateend_tc
  freq = '1D'
  model = 'NEMO'
  explist = ['COUPLED_SW2_KF']
  varlist = ['ci']
  ficlist = ['ci']
  basin   = ['IO']
; End define variables to collocalise and chosen period


  FOR iexp = 0, n_elements(explist)-1 DO BEGIN
  expname = explist[iexp] & help, expname
  tcdir = '/net/adonis/usr/adonis/varclim/gslod/IDL/COLLOCALISATION/EXP_'+expname+'/DATA/'
  path_file_tc_d0 = tcdir+'d0_TRACKS_'+expname+'_'+basin+'_'+strtrim(datebeg_tc,2)+'-'+strtrim(dateend_tc,2)+'.dat'
  path_file_tc_d1 = tcdir+'d1_TRACKS_'+expname+'_'+basin+'_'+strtrim(datebeg_tc,2)+'-'+strtrim(dateend_tc,2)+'.dat'

  FOR ivar = 0, n_elements(varlist)-1 DO BEGIN
  var_nm = varlist[ivar]
  fic_nm = ficlist[ivar]
  indir  = '/net/adonis/usr/adonis/varclim/gslod/EXP_'+ expname +'/'+var_nm+'/' & help, indir
  path_file_wsc_wcy = indir+fic_nm+'_1990-2009.nc' & help, path_file_wsc_wcy
  path_file_nsc_wcy = indir+fic_nm+'_nosc_1990-2009.nc' & help, path_file_nsc_wcy
  fileout = tcdir+var_nm+'_1D_200km_'+expname+'_'+freq+'_'+basin+'_'+strtrim(datebeg_ok,2)+'-'+strtrim(dateend_ok,2)+'.dat'   

  day_before = [-10, -3]   
  day_after  = [  1,  4]	; low/high bounds for the before-after calculation
  dt_bnd     = [-10, 40]        ; time axis bounds for dt (timeseries) data
     
  colloc_data1D_to_tc, tcdir, path_file_wsc_wcy, path_file_nsc_wcy, path_file_nsc_ncy, var_nm, basin,datebeg_sf,datebeg_ok,dateend_ok, day_before, day_after, dt_bnd, expname, freq, path_file_tc_d0, path_file_tc_d1, fileout, model
  
  ENDFOR
  ENDFOR

END
