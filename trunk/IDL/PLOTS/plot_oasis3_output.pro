PRO plot_oasis3_output
COMPILE_OPT IDL2, STRICTARRSUBS
@all_cm


xdim = 640
ydim = 375
tdim = 6
path = '/home/gsamson/WORK/AROME/TEST_CPL/EXP5_20080213-20080213/'

atm_var_list = ['SISUTESU','COZOTAUV','COZOTAUX','COMETAUU','COMETAUY','CONSFTOT','COSHFTOT','COWATFLU']
oce_var_list = ['O_SSTSST','O_OTaux1','O_OTaux2','O_OTauy1','O_OTauy2','O_QnsOce','O_QsrOce','OOEvaMPr']


FOR i = 0, n_elements(atm_var_list)-1 DO BEGIN

  name = atm_var_list[i]
  file = name + '_out.2008-02-13T00:00:00.nc'

  var = ncdf_lec(path+file, var=name)
  var = reform(var,xdim,ydim,tdim,/overwrite)

  IF i EQ 0 THEN BEGIN
    tmp = ncdf_lec(path+file, var='lon')
    lon_atm = reform(tmp,xdim,ydim,/overwrite) & help, lon_atm
    tmp = ncdf_lec(path+file, var='lat')
    lat_atm = reform(tmp,xdim,ydim,/overwrite) & help, lat_atm
;    mask_atm = var[*,*,0]*0.+1. & mask[where(var[*,*,0] EQ 0.)] = !VALUES.F_NAN & help, mask
    mask_atm = var*0.+1. & mask_atm[where(var EQ 0.)] = !VALUES.F_NAN & help, mask_atm
  ENDIF

  tmp = execute(name+' = var & help, '+name)  
  tmp = execute('m'+name+' = var*mask_atm & help, m'+name)

ENDFOR


FOR i = 0, n_elements(oce_var_list)-1 DO BEGIN

  name = oce_var_list[i]
  file = name + '_out.2008-02-13T00:00:00.nc'
  var = ncdf_lec(path+file, var=name)

  IF i EQ 0 THEN BEGIN
    lon_oce = ncdf_lec(path+file, var='lon') & help, lon_oce
    lat_oce = ncdf_lec(path+file, var='lat') & help, lat_oce
    mask_oce = var*0.+1. & mask_oce[where(var EQ 273.150d)] = !VALUES.F_NAN & help, mask_oce
  ENDIF

  tmp = execute(name+' = var & help, '+name)
  tmp = execute('m'+name+' = var*mask_oce & help, m'+name)

ENDFOR

computegrid, xaxis=lon_atm, yaxis=lat_atm
computegrid, xaxis=lon_oce, yaxis=lat_oce


STOP
END
