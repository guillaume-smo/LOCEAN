PRO plot_compare_conv
@all_cm

exp_list = ['CPL_KF_V33','CPL_BMJ_V33','CPL_TDK_V33','CPL_NSAS_V33']
var_list = ['RAINC'];,'UV10','SST'];,'RAINC','OLR']
bassin   = 'IO'

IF bassin EQ  'IO' THEN box = [30,130,-30,25]
IF bassin EQ 'SIO' THEN box = [30,130,-30, 0]
IF bassin EQ 'NIO' THEN box = [30,130,  0,25]

FOR i = 0, n_elements(var_list)-1 DO BEGIN
FOR j = 0, n_elements(exp_list)-1 DO BEGIN

  exp_name = exp_list[j]
  var_name = var_list[i]

  IF var_name EQ 'UV10' THEN BEGIN

    path_mod = '/net/adonis/usr/adonis/varclim/gslod/EXP_'+exp_name+'/'
    file_mod = 'wrfout_mean_19900103.nc'
    maskfile = '/usr/home/gslod/IDL/MASK_WRF_GRID_T.nc'
    mask = ncdf_lec(maskfile, var = 'LANDMASK')
    initncdf, maskfile
    domdef, box

    var_mod = read_ncdf('U10',0,0, /timestep, filename=path_mod+file_mod, /nostruct) & help, var_mod
    u10_mod = var_mod & u10_mod[where(mask[firstxt:lastxt,firstyt:lastyt] EQ 1)] = !values.f_nan
    var_mod = read_ncdf('V10',0,0, /timestep, filename=path_mod+file_mod, /nostruct) & help, var_mod
    v10_mod = var_mod & v10_mod[where(mask[firstxt:lastxt,firstyt:lastyt] EQ 1)] = !values.f_nan
    uv10_mod  = sqrt(u10_mod^2+v10_mod^2)
    angle_mod = windangle(u10_mod,v10_mod)

    IF j EQ 0 THEN plt, uv10_mod, 0, 15, /realcont, lct=34, title=exp_name, subtitle=' ', xtitle='', ytitle='', cb_charsize=1.2, format='(I3)', small=[2,2,1+j],win=0, /landscape
    IF j NE 0 THEN wset, 0
    IF j NE 0 THEN plt, uv10_mod, 0, 15, /realcont, lct=34, title=exp_name, subtitle=' ', xtitle='', ytitle='', cb_charsize=1.2, format='(I3)', small=[2,2,1+j], /noerase
    IF j EQ n_elements(exp_list)-1 THEN saveimage, 'FIGURES/'+var_name+'_2D_A19900103_'+exp_name+'.gif'

    IF j EQ 0 THEN plt, angle_mod+90, /realcont, lct=34, title=exp_name, subtitle=' ', xtitle='', ytitle='', cb_charsize=1.2, format='(I3)', small=[2,2,1+j],win=1, /landscape
    IF j NE 0 THEN wset, 1   
    IF j NE 0 THEN plt, angle_mod+90, /realcont, lct=34, title=exp_name, subtitle=' ', xtitle='', ytitle='', cb_charsize=1.2, format='(I3)', small=[2,2,1+j], /noerase
    IF j EQ n_elements(exp_list)-1 THEN saveimage, 'FIGURES/'+'ANGLE_mod_2D_A19900103_'+exp_name+'.gif'

  ENDIF


  IF var_name EQ 'SST' THEN BEGIN

    path_mod = '/net/adonis/usr/adonis/varclim/gslod/EXP_'+exp_name+'/'
    file_mod = 'wrfout_mean_19900103.nc'
    maskfile = '/usr/home/gslod/IDL/MASK_WRF_GRID_T.nc'
    mask = ncdf_lec(maskfile, var = 'LANDMASK')
    initncdf, maskfile
    domdef, box

    var_mod = read_ncdf(var_name,0,0, /timestep, filename=path_mod+file_mod, /nostruct) & help, var_mod
    var_mod = var_mod - 273.15
    IF j EQ 0 THEN plt, var_mod, 20, 31, /realcont, lct=34, title=exp_name, subtitle=' ', xtitle='', ytitle='', cb_charsize=1.2, format='(I3)', small=[2,2,1+j],win=0, /landscape
    IF j NE 0 THEN wset, 0
    IF j NE 0 THEN plt, var_mod, 20, 31, /realcont, lct=34, title=exp_name, subtitle=' ', xtitle='', ytitle='', cb_charsize=1.2, format='(I3)', small=[2,2,1+j], /noerase
;    IF j EQ n_elements(exp_list)-1 THEN saveimage, 'FIGURES/'+var_name+'_2D_A19900103_'+exp_name+'.gif'

  ENDIF


  IF var_name EQ 'RAINC' THEN BEGIN

    path_mod = '/net/adonis/usr/adonis/varclim/gslod/EXP_'+exp_name+'/'
    file_mod = 'wrfout_mean_19900103.nc'
    maskfile = '/usr/home/gslod/IDL/MASK_WRF_GRID_T.nc'
    mask = ncdf_lec(maskfile, var = 'LANDMASK')
    initncdf, maskfile
    domdef, box

    var_mod = read_ncdf(var_name,0,0, /timestep, filename=path_mod+file_mod, /nostruct) & help, var_mod
    var_mod = var_mod / 90.
    IF j EQ 0 THEN plt, var_mod, 0, 25, /realcont, lct=34, title=exp_name, subtitle=' ', xtitle='', ytitle='', cb_charsize=1.2, format='(I3)', small=[2,2,1+j],win=0, /landscape, /nocont
    IF j NE 0 THEN wset, 0
    IF j NE 0 THEN plt, var_mod, 0, 25, /realcont, lct=34, title=exp_name, subtitle=' ', xtitle='', ytitle='', cb_charsize=1.2, format='(I3)', small=[2,2,1+j], /noerase, /nocont
    IF j EQ n_elements(exp_list)-1 THEN saveimage, 'FIGURES/'+var_name+'_2D_A19900103_'+exp_name+'.gif'

  ENDIF


  IF var_name EQ 'OLR' THEN BEGIN

    path_mod = '/net/adonis/usr/adonis/varclim/gslod/EXP_'+exp_name+'/'
    file_mod = 'wrfout_mean_19900103.nc'
    maskfile = '/usr/home/gslod/IDL/MASK_WRF_GRID_T.nc'
    mask = ncdf_lec(maskfile, var = 'LANDMASK')
    initncdf, maskfile
    domdef, box

    var_mod = read_ncdf(var_name,0,0, /timestep, filename=path_mod+file_mod, /nostruct) & help, var_mod
    IF j EQ 0 THEN plt, var_mod, 180, 300, /realcont, lct=34, title=exp_name, subtitle=' ', xtitle='', ytitle='', cb_charsize=1.2, format='(I3)', small=[2,2,1+j],win=0, /landscape, /nocont
    IF j NE 0 THEN wset, 0
    IF j NE 0 THEN plt, var_mod, 180, 300, /realcont, lct=34, title=exp_name, subtitle=' ', xtitle='', ytitle='', cb_charsize=1.2, format='(I3)', small=[2,2,1+j], /noerase, /nocont
    IF j EQ n_elements(exp_list)-1 THEN saveimage, 'FIGURES/'+var_name+'_2D_A19900103_'+exp_name+'.gif'

  ENDIF


ENDFOR
ENDFOR

stop

END
