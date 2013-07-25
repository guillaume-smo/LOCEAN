PRO build_database_tc_TMIAMSRE_1D
@common
  
  datebeg_ok = 19980101
  dateend_ok = 20100101
  datebeg_sf = datebeg_ok
  period     = '1998-2009'
  freq       = '1D'
  basin      = 'IO'
  mean_radius= 200            ; rayon utilise pour calculer les moyennes en km
  model      = 'OBS'

  indir = '/net/adonis/usr/adonis/varclim/gslod/IDL/TRACKER/EXP_IBTRACS/DATA/'
  path_file_tc_d1 = indir+'d1_IBTRACSv03r03_'+basin+'_1998-2009.dat' & help, path_file_tc_d1

  var_nm = 'SST'
  indir  = '/net/adonis/usr/adonis/varclim/gslod/EXP_TMI-AMSRE/' & help, indir
  path_file_wsc_wcy = indir+'SST_DAILY_1998-2009.nc' & help, path_file_wsc_wcy
  path_file_nsc_wcy = indir+'SST_NOSC_DAILY_1998-2009.nc' & help, path_file_nsc_wcy

  tcdir = '/net/adonis/usr/adonis/varclim/gslod/IDL/COLLOCALISATION/EXP_TMI-AMSRE/DATA/'
  fileout = tcdir+'SST_1D_'+strtrim(mean_radius,2)+'km_TMI-AMSRE_'+freq+'_'+basin+'_'+strtrim(datebeg_ok,2)+'-'+strtrim(dateend_ok,2)+'.dat'


  day_before = [-10, -3]
  day_after  = [  1,  4]        ; low/high bounds for the before-after calculation
  dt_bnd     = [-10, 40]        ; time axis bounds for dt (timeseries) data


colloc_TMIAMSRE_1D_to_tc, path_file_wsc_wcy, path_file_nsc_wcy, var_nm, datebeg_sf, datebeg_ok, dateend_ok, $
                          day_before, day_after, dt_bnd, freq, path_file_tc_d1, fileout


END
