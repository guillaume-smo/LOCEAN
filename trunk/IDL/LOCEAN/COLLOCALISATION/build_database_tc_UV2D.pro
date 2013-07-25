PRO build_database_tc_UV2D
@common
  
; Begin define variables to collocalise and chosen period
  datebeg_tc  = 19900101
  datebeg_sf  = 19900101
  datebeg_ok  = 19900101
  dateend_tc = 20100101 & dateend_sf = dateend_tc & dateend_ok = dateend_tc
  period = '1990-2009'
  freq = '6H'
  model = 'WRF'
  explist = ['FORCED_SW2_KF']
  basin   = ['IO']
  mean_radius = 500            ; rayon utilise pour calculer les moyennes en km
; End define variables to collocalise and chosen period


  FOR iexp = 0, n_elements(explist)-1 DO BEGIN
  expname = explist[iexp]

  indir = '/net/adonis/usr/adonis/varclim/gslod/IDL/TRACKER/EXP_'+ expname +'/DATA/'
  tcdir = '/net/adonis/usr/adonis/varclim/gslod/IDL/COLLOCALISATION/EXP_'+ expname +'/DATA/'

  path_file_tc_d0 = indir+'d0_TRACKS_'+expname+'_'+basin+'_'+period+'.dat'
  path_file_tc_d1 = indir+'d1_TRACKS_'+expname+'_'+basin+'_'+period+'.dat'

  indir  = '/net/adonis/usr/adonis/varclim/gslod/EXP_'+ expname +'/U_V_10/' & help, indir
  path_file_wsc_wcy = indir+'uv10_'+period+'.nc' & help, path_file_wsc_wcy
  path_file_nsc_wcy = indir+'uv10_nosc_'+period+'.nc' & help, path_file_nsc_wcy
  fileout = tcdir+'UV10TR_2D_'+strtrim(mean_radius,2)+'km_'+expname+'_'+freq+'_'+basin+'_'+strtrim(datebeg_ok,2)+'-'+strtrim(dateend_ok,2)+'.dat'   

  colloc_UV2D_to_tc, tcdir, path_file_wsc_wcy, basin,datebeg_sf,datebeg_ok,dateend_ok, expname, freq, path_file_tc_d0, path_file_tc_d1, fileout, model, mean_radius
  
  ENDFOR

END
