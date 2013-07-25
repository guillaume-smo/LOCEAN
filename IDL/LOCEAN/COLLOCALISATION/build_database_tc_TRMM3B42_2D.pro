PRO build_database_tc_TRMM3B42_2D
@common
  
; Begin define variables to collocalise and chosen period
  datebeg_ok = 19980101
  dateend_ok = 20110101
  period     = '1998-2010'
  freq       = '3H'
  basin      = 'IO'
  mean_radius= 500            ; rayon utilise pour calculer les moyennes en km
; End define variables to collocalise and chosen period


  indir = '/net/adonis/usr/adonis/varclim/gslod/IDL/TRACKER/EXP_IBTRACS/DATA/'
  tcdir = '/net/adonis/usr/adonis/varclim/gslod/IDL/COLLOCALISATION/EXP_TRMM3B42/DATA/'
  path_file_tc_d1 = indir+'d1_IBTRACS_'+basin+'_'+period+'_3H_NOBADTIME.dat'

  var_nm = 'precipitation'
  indir  = '/net/adonis/usr/adonis/varclim/gslod/EXP_TRMM3B42/' & help, indir
  path_file_wsc_wcy = indir+'RAIN_TRMM3B42_3H_19980101-20110101.nc' & help, path_file_wsc_wcy
  fileout = tcdir+'RAIN_2D_'+strtrim(mean_radius,2)+'km_TRMM3B42_'+freq+'_'+basin+'_'+strtrim(datebeg_ok,2)+'-'+strtrim(dateend_ok,2)+'_NOBADTIME.dat'   
  colloc_TRMM3B42_2D_to_tc, path_file_wsc_wcy, var_nm, basin, datebeg_ok, dateend_ok,freq, path_file_tc_d1, fileout, mean_radius
  

END
