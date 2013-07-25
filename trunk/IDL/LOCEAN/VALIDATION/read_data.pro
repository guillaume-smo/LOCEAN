; lecture data+mask


IF varn EQ 'SSH' THEN BEGIN
  var = read_ncdf('zos', /allrecords, filename=path+file, /nostruct)
  IF expn NE 'AVISO' THEN var = var * 100. ; m -> cm
  IF expn EQ 'AVISO' THEN BEGIN
    mask = var[*,*,0] * 0. + 1. & mask[where(finite(var[*,*,0]) EQ 0.)] = !VALUES.F_NAN
  ENDIF
  IF expn NE 'AVISO' THEN BEGIN
    mask = var[*,*,0] * 0. + 1. & mask[where(var[*,*,0] EQ 0.)] = !VALUES.F_NAN
  ENDIF
ENDIF

IF varn EQ 'UV10' THEN BEGIN
  IF expn EQ 'COUPLED_SW2_BMJ' OR expn EQ 'COUPLED_SW2_KF' OR expn EQ 'FORCED_SW2_KF' OR expn EQ 'CPL_TDK_V33' OR expn EQ 'CPL_NSAS_V33' THEN BEGIN
    var1 = read_ncdf('U10', /allrecords, filename=path+file, /nostruct)
    var2 = read_ncdf('V10', /allrecords, filename=path+file, /nostruct)
    var  = sqrt(var1^2+var2^2)
  ENDIF
  IF expn EQ 'TROPFLUX' THEN var = read_ncdf(varn, /allrecords, filename=path+file, /nostruct)
  IF expn EQ 'QSCAT' THEN BEGIN
    var = read_ncdf('windspd', /allrecords, filename=path+file, /nostruct)
    dir = read_ncdf('winddir', /allrecords, filename=path+file, /nostruct) * !PI / 180.
    var1 = var * sin(dir)
    var2 = var * cos(dir)
    mask = var[*,*,0] * 0. + 1. & mask[where(finite(var[*,*,0]) EQ 0.)] = !VALUES.F_NAN
  ENDIF
  IF expn EQ 'CCMP'     THEN var = read_ncdf('wspd', /allrecords, filename=path+file, /nostruct)
ENDIF

IF varn EQ 'UVSURF' THEN BEGIN
  IF expn NE 'SURCOUF' THEN BEGIN
    var1 = reform(read_ncdf('uo', /allrecords, filename=path+file, /nostruct))
    var2 = reform(read_ncdf('vo', /allrecords, filename=path+file, /nostruct))
    var  = sqrt(var1^2+var2^2)
    mask = var[*,*,0] * 0. + 1. & mask[where(var[*,*,0] EQ 0.)] = !VALUES.F_NAN
  ENDIF
  IF expn EQ 'SURCOUF' THEN BEGIN
    var1 = read_ncdf('Grid_0001', /allrecords, filename=path+file, /nostruct)
    var2 = read_ncdf('Grid_0002', /allrecords, filename=path+file, /nostruct)
    var  = sqrt(var1^2+var2^2)
    mask = var[*,*,0] * 0. + 1. & mask[where(finite(var[*,*,0]) EQ 0.)] = !VALUES.F_NAN
  ENDIF
ENDIF

IF expn EQ 'TRMM3B43' THEN var = read_ncdf(varn, /allrecords, filename=path+file, /nostruct) * 24. ; mm/h -> mm/d
IF expn EQ 'GPCP'     THEN var = read_ncdf(varn, /allrecords, filename=path+file, /nostruct)

IF expn EQ 'TMI-AMSRE' THEN BEGIN
  var  = read_ncdf(varn, /allrecords, filename=path+file, /nostruct)
  mask = fltarr(n_elements(var[*,0,0]),n_elements(var[0,*,0])) + 1.
  mask[where(var[*,*,0] EQ 35.2500)] = !VALUES.F_NAN
ENDIF

IF varn EQ 'RAIN' THEN BEGIN
  IF freq EQ '6H' AND expn EQ 'COUPLED_SW2_KF' THEN BEGIN
;    var1 = read_ncdf('RAINC', /allrecords, filename=path+file, /nostruct) * 4.
;    var2 = read_ncdf('RAINNC', /allrecords, filename=path+file, /nostruct) * 4. ; mm/6H -> mm/d
;    var2 = 0.
;    var  = var1 + var2 >0
    var = read_ncdf('RAIN', /allrecords, filename=path+file, /nostruct)
  ENDIF
  IF freq EQ '6H' AND expn EQ 'COUPLED_SW2_BMJ' THEN BEGIN
    var1 = read_ncdf('RAINC', /allrecords, filename=path+file, /nostruct) * 4.
    var2 = read_ncdf('RAINNC', /allrecords, filename=path+file, /nostruct) * 4. ; mm/6H -> mm/d
;    var2 = 0.
    var  = var1 + var2 >0
    IF interp THEN var_smooth = smooth(var,[5,5,1],/nan) >0 ; smooth sur la grille GPCP avant interpolation
    IF interp THEN var = var_smooth
  ENDIF
  IF freq EQ '6H' AND (expn EQ 'CPL_TDK_V33' OR expn EQ 'CPL_NSAS_V33') THEN BEGIN
    var1 = read_ncdf('RAINC', /allrecords, filename=path+file, /nostruct) * 4.
;    var2 = read_ncdf('RAINNC', /allrecords, filename=path+file, /nostruct) * 4. ; mm/6H -> mm/d
    var2 = 0.
    var  = var1 + var2 >0
    var  = var - shift(var,[0,0,1]) >0
    IF interp THEN var_smooth = smooth(var,[5,5,1],/nan) >0 ; smooth sur la grille GPCP avant interpolation
    IF interp THEN var  = var_smooth
  ENDIF
  IF freq EQ 'DAILY' AND (expn EQ 'COUPLED_SW2_KF' OR expn EQ 'FORCED_SW2_KF' OR expn EQ 'COUPLED_SW2_BMJ' OR expn EQ 'CPL_TDK_V33' OR expn EQ 'CPL_NSAS_V33') THEN $
    var = read_ncdf('RAIN', /allrecords, filename=path+file, /nostruct)
ENDIF

IF varn EQ 'SST' THEN BEGIN
  IF expn EQ 'COUPLED_SW2_BMJ' OR expn EQ 'COUPLED_SW2_KF' OR expn EQ 'FORCED_SW2_KF' THEN var = read_ncdf('SST', /allrecords, filename=path+file, /nostruct) - 273.15
  IF expn EQ 'CPL_TDK_V33' OR expn EQ 'CPL_NSAS_V33' THEN var = read_ncdf('SST', /allrecords, filename=path+file, /nostruct) - 273.15
ENDIF

IF varn EQ 'OLR' THEN BEGIN
  var = read_ncdf('OLR', /allrecords, filename=path+file, /nostruct)
  IF write_ncdf AND interp AND expn NE 'NOAA' THEN var_smooth = smooth(var,[11,11,1],/nan) >0 ; smooth sur la grille NOAA avant interpolation
  IF write_ncdf AND interp AND expn NE 'NOAA' THEN var = var_smooth
ENDIF

IF varn EQ 'RH600' THEN BEGIN
  IF expn EQ 'ERA-I' THEN var = reform(read_ncdf('r', /allrecords, filename=path+file, /nostruct)) $
                     ELSE var = reform(read_ncdf('rh600', /allrecords, filename=path+file, /nostruct)) * 100. ; (%)
ENDIF

IF varn EQ 'VOR850' THEN BEGIN
  IF expn EQ 'ERA-I' THEN var = reform(read_ncdf('vo', /allrecords, filename=path+file, /nostruct)) * 1000000. $
                     ELSE var = reform(read_ncdf('vor850', /allrecords, filename=path+file, /nostruct)) * 1000000.
ENDIF

IF varn EQ 'SHEAR' THEN BEGIN
  IF expn EQ 'ERA-I' THEN BEGIN
    var1 = read_ncdf('u', /allrecords, filename=path+file, /nostruct)
    var2 = read_ncdf('v', /allrecords, filename=path+file, /nostruct)
    var  = reform(sqrt((var1[*,*,0,*]-var1[*,*,1,*])^2+(var2[*,*,0,*]-var2[*,*,1,*])^2))
  ENDIF ELSE BEGIN
    var1 = read_ncdf('u200', /allrecords, filename=path+file, /nostruct)
    var2 = read_ncdf('u850', /allrecords, filename=path+file, /nostruct)
    var3 = read_ncdf('v200', /allrecords, filename=path+file, /nostruct)
    var4 = read_ncdf('v850', /allrecords, filename=path+file, /nostruct)
    var  = sqrt((var1 - var2)^2+(var3 - var4)^2)
  ENDELSE
ENDIF

IF varn EQ 'MPI' THEN BEGIN
  var = reform(read_ncdf('vpot', /allrecords, filename=path+file, /nostruct))
ENDIF

IF varn EQ 'GPI' THEN BEGIN
  IF expn EQ 'ERA-I' THEN rh600 = reform(read_ncdf('r', /allrecords, filename=path+'RH600_MONTHLY_1990-2009_v2.nc', /nostruct)) $
                     ELSE rh600 = reform(read_ncdf('rh600', /allrecords, filename=path+'RH600_MONTHLY_1990-2009.nc', /nostruct)) * 100. ; (%)
  help, rh600
  IF expn EQ 'ERA-I' THEN vor850 = reform(read_ncdf('vo', /allrecords, filename=path+'VOR850_MONTHLY_1990-2009.nc', /nostruct)) $
                     ELSE vor850 = reform(read_ncdf('vor850', /allrecords, filename=path+'VOR850_MONTHLY_1990-2009.nc', /nostruct))
  help, vor850
  IF expn EQ 'ERA-I' THEN BEGIN
    initncdf, path+'SHEAR_MONTHLY_1990-2009.nc' & domdef, box
    var1 = read_ncdf('u', /allrecords, filename=path+'SHEAR_MONTHLY_1990-2009.nc', /nostruct)
    var2 = read_ncdf('v', /allrecords, filename=path+'SHEAR_MONTHLY_1990-2009.nc', /nostruct)
    shear = reform(sqrt((var1[*,*,0,*]-var1[*,*,1,*])^2+(var2[*,*,0,*]-var2[*,*,1,*])^2))
  ENDIF ELSE BEGIN
    var1 = read_ncdf('u200', /allrecords, filename=path+'SHEAR_MONTHLY_1990-2009.nc', /nostruct)
    var2 = read_ncdf('u850', /allrecords, filename=path+'SHEAR_MONTHLY_1990-2009.nc', /nostruct)
    var3 = read_ncdf('v200', /allrecords, filename=path+'SHEAR_MONTHLY_1990-2009.nc', /nostruct)
    var4 = read_ncdf('v850', /allrecords, filename=path+'SHEAR_MONTHLY_1990-2009.nc', /nostruct)
    shear = sqrt((var1 - var2)^2+(var3 - var4)^2)
  ENDELSE
  help, shear
  IF expn EQ 'ERA-I' THEN initncdf, path+'MPI_MONTHLY_1990-2009_v2.nc' & domdef, box
  IF expn EQ 'ERA-I' THEN mpi = reform(read_ncdf('vpot', /allrecords, filename=path+'MPI_MONTHLY_1990-2009_v2.nc', /nostruct)) $
                     ELSE mpi = reform(read_ncdf('vpot', /allrecords, filename=path+'MPI_MONTHLY_1990-2009.nc', /nostruct))
  vorf = 2*(2*!pi/86400.)*sin(!pi/180.*gphit[firstxt:lastxt,firstyt:lastyt]) & help, vorf
  cntok = n_elements(mpi[0,0,*])
  vorf = reform(reform(vorf, n_elements(mpi[*,0,0])*n_elements(mpi[0,*,0]))#replicate(1,cntok), n_elements(mpi[*,0,0]), n_elements(mpi[0,*,0]), cntok)
  var =  abs(1.e5*(vor850+vorf))^(3/2.)*(rh600/50.)^3*(mpi/70.)^3*(1+0.1*shear)^(-2)
ENDIF

help, var ; check: plt, reform(var[*,*,0,0])*mask
