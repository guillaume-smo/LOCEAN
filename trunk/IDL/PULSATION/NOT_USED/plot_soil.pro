PRO plot_soil
@all_cm

data_type = '1d'
nbday = 365 ; 235
nbmonth = 12

grid_path = '/ccc/cont005/home/ra1047/samsong/CONF_GRIDS/'
grid_file = 'grid_wrf_trop075.nc'
;box = [ 66, 94, 6, 36]
;box = [ 66, 94, 6, 30]
box = [ 66, 94, 6, 27]

initncdf, grid_path+grid_file, glam=[20,380], ZAXISNAME='soil_depths'
domdef, [box, 0, 3], /ZINDEX
help, gdept & print, gdept
help, e3t & print, e3t
e3t = ncdf_lec(grid_path+grid_file, var='DZS')
help, gdept & print, gdept
help, e3t & print, e3t

lon_wrf  = glamt[firstxt:lastxt,firstyt:lastyt]
lat_wrf  = gphit[firstxt:lastxt,firstyt:lastyt]
seamask  = read_ncdf('LANDMASK', filename=grid_path+grid_file, /ALLRECORDS, /NOSTRUCT)
landmask = seamask
landmask[where(seamask EQ 1.)] = !VALUES.F_NAN
landmask[where(seamask EQ 0.)] = 1.
seamask[ where(seamask EQ 0.)] = !VALUES.F_NAN
help, landmask, seamask
path = '/ccc/store/cont005/ra1047/samsong/TMP/'


IF data_type EQ '1m' THEN BEGIN

  file = 'wrfout_1m_tmn0.nc'

  tsk_tmn0 = read_ncdf('TSK', filename=path+file, /ALLRECORDS, /NOSTRUCT) & help, tsk_tmn0
  FOR t = 0, n_elements(tsk_tmn0[0,0,*])-1 DO tsk_tmn0[*,*,t] = tsk_tmn0[*,*,t] * seamask
  tsk_tmn0_ts = grossemoyenne(tsk_tmn0, 'xy', /NAN)
  tsk_tmn0_2d = MEAN(tsk_tmn0[*,*,5:7], DIMENSION=3, /NAN)

  tmn_tmn0 = read_ncdf('TMN', filename=path+file, /ALLRECORDS, /NOSTRUCT) & help, tmn_tmn0
  FOR t = 0, n_elements(tmn_tmn0[0,0,*])-1 DO tmn_tmn0[*,*,t] = tmn_tmn0[*,*,t] * seamask
  tmn_tmn0_ts = grossemoyenne(tmn_tmn0, 'xy', /NAN)
  tmn_tmn0_2d = MEAN(tmn_tmn0[*,*,5:7], DIMENSION=3, /NAN)

  mean_tsoil_tmn0 = read_ncdf('TSLB', filename=path+file, /ALLRECORDS, /NOSTRUCT, DIREC='z') & help, mean_tsoil_tmn0
  FOR t = 0, n_elements(mean_tsoil_tmn0[0,0,*])-1 DO mean_tsoil_tmn0[*,*,t] = mean_tsoil_tmn0[*,*,t] * seamask
  mean_tsoil_tmn0_ts = grossemoyenne(mean_tsoil_tmn0, 'xy', /NAN)
  mean_tsoil_tmn0_2d = MEAN(mean_tsoil_tmn0[*,*,5:7], DIMENSION=3, /NAN)

  mean_swv_tmn0 = read_ncdf('SH2O', filename=path+file, /ALLRECORDS, /NOSTRUCT, DIREC='z') & help, mean_swv_tmn0
  FOR t = 0, n_elements(mean_swv_tmn0[0,0,*])-1 DO mean_swv_tmn0[*,*,t] = mean_swv_tmn0[*,*,t] * seamask
  mean_swv_tmn0_ts = grossemoyenne(mean_swv_tmn0, 'xy', /NAN)
  mean_swv_tmn0_2d = MEAN(mean_swv_tmn0[*,*,5:7], DIMENSION=3, /NAN)

  file = 'wrfout_1m_lag150.nc'

  tsk_lag150 = read_ncdf('TSK', filename=path+file, /ALLRECORDS, /NOSTRUCT) & help, tsk_lag150
  FOR t = 0, n_elements(tsk_lag150[0,0,*])-1 DO tsk_lag150[*,*,t] = tsk_lag150[*,*,t] * seamask
  tsk_lag150_ts = grossemoyenne(tsk_lag150, 'xy', /NAN)
  tsk_lag150_2d = MEAN(tsk_lag150[*,*,5:7], DIMENSION=3, /NAN)

  tmn_lag150 = read_ncdf('TMN', filename=path+file, /ALLRECORDS, /NOSTRUCT) & help, tmn_lag150
  FOR t = 0, n_elements(tmn_lag150[0,0,*])-1 DO tmn_lag150[*,*,t] = tmn_lag150[*,*,t] * seamask
  tmn_lag150_ts = grossemoyenne(tmn_lag150, 'xy', /NAN)
  tmn_lag150_2d = MEAN(tmn_lag150[*,*,5:7], DIMENSION=3, /NAN)

  mean_tsoil_lag150 = read_ncdf('TSLB', filename=path+file, /ALLRECORDS, /NOSTRUCT, DIREC='z') & help, mean_tsoil_lag150
  FOR t = 0, n_elements(mean_tsoil_lag150[0,0,*])-1 DO mean_tsoil_lag150[*,*,t] = mean_tsoil_lag150[*,*,t] * seamask
  mean_tsoil_lag150_ts = grossemoyenne(mean_tsoil_lag150, 'xy', /NAN)
  mean_tsoil_lag150_2d = MEAN(mean_tsoil_lag150[*,*,5:7], DIMENSION=3, /NAN)

  mean_swv_lag150 = read_ncdf('SH2O', filename=path+file, /ALLRECORDS, /NOSTRUCT, DIREC='z') & help, mean_swv_lag150
  FOR t = 0, n_elements(mean_swv_lag150[0,0,*])-1 DO mean_swv_lag150[*,*,t] = mean_swv_lag150[*,*,t] * seamask
  mean_swv_lag150_ts = grossemoyenne(mean_swv_lag150, 'xy', /NAN)
  mean_swv_lag150_2d = MEAN(mean_swv_lag150[*,*,5:7], DIMENSION=3, /NAN)


  path = '/ccc/store/cont005/ra0542/massons/trop075_wrf/outputs/tr075_ra12L60_sol094/'
  file = 'tr075_ra12L60_sol094_1m_198901_200912_PLEV.nc'

  tsk_lag1 = read_ncdf('SKINTEMP', 0, 11, filename=path+file, /ALLRECORDS, /NOSTRUCT, /TIMESTEP) & help, tsk_lag1
  FOR t = 0, n_elements(tsk_lag1[0,0,*])-1 DO tsk_lag1[*,*,t] = tsk_lag1[*,*,t] * seamask
  tsk_lag1_ts = grossemoyenne(tsk_lag1, 'xy', /NAN)
  tsk_lag1_2d = MEAN(tsk_lag1[*,*,5:7], DIMENSION=3, /NAN)

ENDIF


IF data_type EQ '1d' THEN BEGIN


  file  = 'wrfout_1d_eratw.nc'

  rain_eratw = read_ncdf('PREC_ACC_C' , 0, nbday-1, filename=path+file, /TIMESTEP, /NOSTRUCT) * 4. + $
               read_ncdf('PREC_ACC_NC', 0, nbday-1, filename=path+file, /TIMESTEP, /NOSTRUCT) * 4. & help, rain_eratw
  FOR t = 0, n_elements(rain_eratw[0,0,*])-1 DO rain_eratw[*,*,t] = rain_eratw[*,*,t] * seamask
  rain_eratw_ts = grossemoyenne(rain_eratw, 'xy', /NAN)
  rain_eratw_2d = MEAN(rain_eratw, DIMENSION=3, /NAN)

  lh_eratw = read_ncdf('LH', 0, nbday-1, filename=path+file, /TIMESTEP, /NOSTRUCT) & help, lh_eratw
  FOR t = 0, n_elements(lh_eratw[0,0,*])-1 DO lh_eratw[*,*,t] = lh_eratw[*,*,t] * seamask
  lh_eratw_ts = grossemoyenne(lh_eratw, 'xy', /NAN)
  lh_eratw_2d = MEAN(lh_eratw, DIMENSION=3, /NAN)

  hfx_eratw = read_ncdf('HFX', 0, nbday-1, filename=path+file, /TIMESTEP, /NOSTRUCT) & help, hfx_eratw
  FOR t = 0, n_elements(hfx_eratw[0,0,*])-1 DO hfx_eratw[*,*,t] = hfx_eratw[*,*,t] * seamask
  hfx_eratw_ts = grossemoyenne(hfx_eratw, 'xy', /NAN)
  hfx_eratw_2d = MEAN(hfx_eratw, DIMENSION=3, /NAN)

  tsk_eratw = read_ncdf('TSK', 0, nbday-1, filename=path+file, /TIMESTEP, /NOSTRUCT) & help, tsk_eratw
  FOR t = 0, n_elements(tsk_eratw[0,0,*])-1 DO tsk_eratw[*,*,t] = tsk_eratw[*,*,t] * seamask
  tsk_eratw_ts = grossemoyenne(tsk_eratw, 'xy', /NAN)
  tsk_eratw_2d = MEAN(tsk_eratw, DIMENSION=3, /NAN)

  tmn_eratw = read_ncdf('TMN', 0, nbday-1, filename=path+file, /TIMESTEP, /NOSTRUCT) & help, tmn_eratw
  FOR t = 0, n_elements(tmn_eratw[0,0,*])-1 DO tmn_eratw[*,*,t] = tmn_eratw[*,*,t] * seamask
  tmn_eratw_ts = grossemoyenne(tmn_eratw, 'xy', /NAN)
  tmn_eratw_2d = MEAN(tmn_eratw, DIMENSION=3, /NAN)

  mean_tsoil_eratw = read_ncdf('TSLB', 0, nbday-1, filename=path+file, /TIMESTEP, /NOSTRUCT, DIREC='z') & help, mean_tsoil_eratw
  FOR t = 0, n_elements(mean_tsoil_eratw[0,0,*])-1 DO mean_tsoil_eratw[*,*,t] = mean_tsoil_eratw[*,*,t] * seamask
  mean_tsoil_eratw_ts = grossemoyenne(mean_tsoil_eratw, 'xy', /NAN)
  mean_tsoil_eratw_2d = MEAN(mean_tsoil_eratw, DIMENSION=3, /NAN)

  mean_swv_eratw = read_ncdf('SMOIS', 0, nbday-1, filename=path+file, /TIMESTEP, /NOSTRUCT, DIREC='z') & help, mean_swv_eratw
  FOR t = 0, n_elements(mean_swv_eratw[0,0,*])-1 DO mean_swv_eratw[*,*,t] = mean_swv_eratw[*,*,t] * seamask
  mean_swv_eratw_ts = grossemoyenne(mean_swv_eratw, 'xy', /NAN)
  mean_swv_eratw_2d = MEAN(mean_swv_eratw, DIMENSION=3, /NAN)

  tsoil_eratw = read_ncdf('TSLB', 0, nbday-1, filename=path+file, /TIMESTEP, /NOSTRUCT) & help, tsoil_eratw
  FOR t = 0, n_elements(tsoil_eratw[0,0,0,*])-1 DO FOR z = 0, n_elements(tsoil_eratw[0,0,*,0])-1 DO tsoil_eratw[*,*,z,t] = tsoil_eratw[*,*,z,t] * seamask
  tsoil_eratw_ts = grossemoyenne(tsoil_eratw, 'xy', /NAN)
  tsoil_eratw_2d = MEAN(tsoil_eratw, DIMENSION=3, /NAN)

  tsoil3_eratw = read_ncdf('STL3', 0, nbday-1, filename=path+file, /TIMESTEP, /NOSTRUCT) & help, tsoil3_eratw
  FOR t = 0, n_elements(tsoil3_eratw[0,0,*])-1 DO tsoil3_eratw[*,*,t] = tsoil3_eratw[*,*,t] * seamask
  tsoil3_eratw_ts = grossemoyenne(tsoil3_eratw, 'xy', /NAN)
  tsoil3_eratw_2d = MEAN(tsoil3_eratw, DIMENSION=3, /NAN)

  swv3_eratw = read_ncdf('SWVL3', 0, nbday-1, filename=path+file, /TIMESTEP, /NOSTRUCT) & help, swv3_eratw
  FOR t = 0, n_elements(swv3_eratw[0,0,*])-1 DO swv3_eratw[*,*,t] = swv3_eratw[*,*,t] * seamask
  swv3_eratw_ts = grossemoyenne(swv3_eratw, 'xy', /NAN)
  swv3_eratw_2d = MEAN(swv3_eratw, DIMENSION=3, /NAN)

  tsoil4_eratw = read_ncdf('STL4', 0, nbday-1, filename=path+file, /TIMESTEP, /NOSTRUCT) & help, tsoil4_eratw
  FOR t = 0, n_elements(tsoil4_eratw[0,0,*])-1 DO tsoil4_eratw[*,*,t] = tsoil4_eratw[*,*,t] * seamask
  tsoil4_eratw_ts = grossemoyenne(tsoil4_eratw, 'xy', /NAN)
  tsoil4_eratw_2d = MEAN(tsoil4_eratw, DIMENSION=3, /NAN)

  swv4_eratw = read_ncdf('SWVL4', 0, nbday-1, filename=path+file, /TIMESTEP, /NOSTRUCT) & help, swv4_eratw
  FOR t = 0, n_elements(swv4_eratw[0,0,*])-1 DO swv4_eratw[*,*,t] = swv4_eratw[*,*,t] * seamask
  swv4_eratw_ts = grossemoyenne(swv4_eratw, 'xy', /NAN)
  swv4_eratw_2d = MEAN(swv4_eratw, DIMENSION=3, /NAN)

  swv_eratw = read_ncdf('SMOIS', 0, nbday-1, filename=path+file, /TIMESTEP, /NOSTRUCT) & help, swv_eratw
  FOR t = 0, n_elements(swv_eratw[0,0,0,*])-1 DO FOR z = 0, n_elements(swv_eratw[0,0,*,0])-1 DO swv_eratw[*,*,z,t] = swv_eratw[*,*,z,t] * seamask
  swv_eratw_ts = grossemoyenne(swv_eratw, 'xy', /NAN)
  swv_eratw_2d = MEAN(swv_eratw, DIMENSION=3, /NAN)


  file  = 'wrfout_1d_tmn0.nc'

  rain_tmn0 = read_ncdf('PREC_ACC_C' , 0, nbday-1, filename=path+file, /TIMESTEP, /NOSTRUCT) * 4. + $
              read_ncdf('PREC_ACC_NC', 0, nbday-1, filename=path+file, /TIMESTEP, /NOSTRUCT) * 4. & help, rain_tmn0
  FOR t = 0, n_elements(rain_tmn0[0,0,*])-1 DO rain_tmn0[*,*,t] = rain_tmn0[*,*,t] * seamask
  rain_tmn0_ts = grossemoyenne(rain_tmn0, 'xy', /NAN)
  rain_tmn0_2d = MEAN(rain_tmn0, DIMENSION=3, /NAN)

  lh_tmn0 = read_ncdf('LH', 0, nbday-1, filename=path+file, /TIMESTEP, /NOSTRUCT) & help, lh_tmn0
  FOR t = 0, n_elements(lh_tmn0[0,0,*])-1 DO lh_tmn0[*,*,t] = lh_tmn0[*,*,t] * seamask
  lh_tmn0_ts = grossemoyenne(lh_tmn0, 'xy', /NAN)
  lh_tmn0_2d = MEAN(lh_tmn0, DIMENSION=3, /NAN)

  hfx_tmn0 = read_ncdf('HFX', 0, nbday-1, filename=path+file, /TIMESTEP, /NOSTRUCT) & help, hfx_tmn0
  FOR t = 0, n_elements(hfx_tmn0[0,0,*])-1 DO hfx_tmn0[*,*,t] = hfx_tmn0[*,*,t] * seamask
  hfx_tmn0_ts = grossemoyenne(hfx_tmn0, 'xy', /NAN)
  hfx_tmn0_2d = MEAN(hfx_tmn0, DIMENSION=3, /NAN)

  tsk_tmn0 = read_ncdf('TSK', 0, nbday-1, filename=path+file, /TIMESTEP, /NOSTRUCT) & help, tsk_tmn0
  FOR t = 0, n_elements(tsk_tmn0[0,0,*])-1 DO tsk_tmn0[*,*,t] = tsk_tmn0[*,*,t] * seamask
  tsk_tmn0_ts = grossemoyenne(tsk_tmn0, 'xy', /NAN)
  tsk_tmn0_2d = MEAN(tsk_tmn0, DIMENSION=3, /NAN)

  tmn_tmn0 = read_ncdf('TMN', 0, nbday-1, filename=path+file, /TIMESTEP, /NOSTRUCT) & help, tmn_tmn0
  FOR t = 0, n_elements(tmn_tmn0[0,0,*])-1 DO tmn_tmn0[*,*,t] = tmn_tmn0[*,*,t] * seamask
  tmn_tmn0_ts = grossemoyenne(tmn_tmn0, 'xy', /NAN)
  tmn_tmn0_2d = MEAN(tmn_tmn0, DIMENSION=3, /NAN)

  mean_tsoil_tmn0 = read_ncdf('TSLB', 0, nbday-1, filename=path+file, /TIMESTEP, /NOSTRUCT, DIREC='z') & help, mean_tsoil_tmn0
  FOR t = 0, n_elements(mean_tsoil_tmn0[0,0,*])-1 DO mean_tsoil_tmn0[*,*,t] = mean_tsoil_tmn0[*,*,t] * seamask
  mean_tsoil_tmn0_ts = grossemoyenne(mean_tsoil_tmn0, 'xy', /NAN)
  mean_tsoil_tmn0_2d = MEAN(mean_tsoil_tmn0, DIMENSION=3, /NAN)

  mean_swv_tmn0 = read_ncdf('SMOIS', 0, nbday-1, filename=path+file, /TIMESTEP, /NOSTRUCT, DIREC='z') & help, mean_swv_tmn0
  FOR t = 0, n_elements(mean_swv_tmn0[0,0,*])-1 DO mean_swv_tmn0[*,*,t] = mean_swv_tmn0[*,*,t] * seamask
  mean_swv_tmn0_ts = grossemoyenne(mean_swv_tmn0, 'xy', /NAN)
  mean_swv_tmn0_2d = MEAN(mean_swv_tmn0, DIMENSION=3, /NAN)

  tsoil_tmn0 = read_ncdf('TSLB', 0, nbday-1, filename=path+file, /TIMESTEP, /NOSTRUCT) & help, tsoil_tmn0
  FOR t = 0, n_elements(tsoil_tmn0[0,0,0,*])-1 DO FOR z = 0, n_elements(tsoil_tmn0[0,0,*,0])-1 DO tsoil_tmn0[*,*,z,t] = tsoil_tmn0[*,*,z,t] * seamask
  tsoil_tmn0_ts = grossemoyenne(tsoil_tmn0, 'xy', /NAN)
  tsoil_tmn0_2d = MEAN(tsoil_tmn0, DIMENSION=3, /NAN)

  swv_tmn0 = read_ncdf('SMOIS', 0, nbday-1, filename=path+file, /TIMESTEP, /NOSTRUCT) & help, swv_tmn0
  FOR t = 0, n_elements(swv_tmn0[0,0,0,*])-1 DO FOR z = 0, n_elements(swv_tmn0[0,0,*,0])-1 DO swv_tmn0[*,*,z,t] = swv_tmn0[*,*,z,t] * seamask
  swv_tmn0_ts = grossemoyenne(swv_tmn0, 'xy', /NAN)
  swv_tmn0_2d = MEAN(swv_tmn0, DIMENSION=3, /NAN)


  path = '/ccc/store/cont005/ra0542/massons/trop075_wrf/outputs/tr075_ra12L60_sol094/'
  file = 'tr075_ra12L60_sol094_1d_19890101_19891231_PLEV.nc'

  rain_lag1 = read_ncdf('PREC_ACC_C' , 0, nbday-1, filename=path+file, /TIMESTEP, /NOSTRUCT) * 4. + $
              read_ncdf('PREC_ACC_NC', 0, nbday-1, filename=path+file, /TIMESTEP, /NOSTRUCT) * 4. & help, rain_lag1
  FOR t = 0, n_elements(rain_lag1[0,0,*])-1 DO rain_lag1[*,*,t] = rain_lag1[*,*,t] * seamask
  rain_lag1_ts = grossemoyenne(rain_lag1, 'xy', /NAN)
  rain_lag1_2d = MEAN(rain_lag1, DIMENSION=3, /NAN)

  tsk_lag1 = read_ncdf('SKINTEMP', 0, nbday-1, filename=path+file, /TIMESTEP, /NOSTRUCT) & help, tsk_lag1
  FOR t = 0, n_elements(tsk_lag1[0,0,*])-1 DO tsk_lag1[*,*,t] = tsk_lag1[*,*,t] * seamask
  tsk_lag1_ts = grossemoyenne(tsk_lag1, 'xy', /NAN)
  tsk_lag1_2d = MEAN(tsk_lag1, DIMENSION=3, /NAN)

  lh_lag1 = read_ncdf('LH', 0, nbday-1, filename=path+file, /TIMESTEP, /NOSTRUCT) & help, lh_lag1
  FOR t = 0, n_elements(lh_lag1[0,0,*])-1 DO lh_lag1[*,*,t] = lh_lag1[*,*,t] * seamask
  lh_lag1_ts = grossemoyenne(lh_lag1, 'xy', /NAN)
  lh_lag1_2d = MEAN(lh_lag1, DIMENSION=3, /NAN)

  hfx_lag1 = read_ncdf('HFX', 0, nbday-1, filename=path+file, /TIMESTEP, /NOSTRUCT) & help, hfx_lag1
  FOR t = 0, n_elements(hfx_lag1[0,0,*])-1 DO hfx_lag1[*,*,t] = hfx_lag1[*,*,t] * seamask
  hfx_lag1_ts = grossemoyenne(hfx_lag1, 'xy', /NAN)
  hfx_lag1_2d = MEAN(hfx_lag1, DIMENSION=3, /NAN)

ENDIF



initncdf, '/ccc/store/cont005/ra1047/samsong/OBS_DATA/MASK_ERAI.nc', ZAXISNAME='soil_depths'
domdef, [box, 0, 3], /ZINDEX
help, gdept & print, gdept
help, e3t & print, e3t
e3t = ncdf_lec('/ccc/store/cont005/ra1047/samsong/OBS_DATA/MASK_ERAI.nc', var='DZS')
help, gdept & print, gdept
help, e3t & print, e3t

lon_erai = glamt[firstxt:lastxt,firstyt:lastyt]
lat_erai = gphit[firstxt:lastxt,firstyt:lastyt]
seamask = read_ncdf('MASK', filename='/ccc/store/cont005/ra1047/samsong/OBS_DATA/MASK_ERAI.nc', /ALLRECORDS, /NOSTRUCT)
landmask = seamask
landmask[where(FIX(seamask) EQ 1)] = !VALUES.F_NAN
landmask[where(FIX(seamask) EQ 0)] = 1.
seamask[where(FIX(seamask) EQ 0)] = !VALUES.F_NAN
help, landmask, seamask

erai_path = '/ccc/store/cont005/ra1047/samsong/OBS_DATA/'

IF data_type EQ '1m' THEN BEGIN

  erai_file = 'SKT_1m_ERAI_1989-2009_INVLAT.nc'
  tsk_erai = read_ncdf('SKT', 0, 11, filename=erai_path+erai_file, /ALLRECORDS, /NOSTRUCT, /TIMESTEP) & help, tsk_erai
  FOR t = 0, n_elements(tsk_erai[0,0,*])-1 DO tsk_erai[*,*,t] = tsk_erai[*,*,t] * seamask
  tsk_erai_ts = GROSSEMOYENNE(tsk_erai, 'xy', /NAN)
  tsk_erai_2d = MEAN(tsk_erai[*,*,5:7], DIMENSION=3, /NAN) 

  erai_file = 'SLT4_1m_ERAI_1989-2009.nc'
  tmn_erai = read_ncdf('stl4', filename=erai_path+erai_file, /ALLRECORDS, /NOSTRUCT) & help, tmn_erai
  FOR t = 0, n_elements(tmn_erai[0,0,*])-1 DO tmn_erai[*,*,t] = tmn_erai[*,*,t] * seamask
  tmn_erai_ts = GROSSEMOYENNE(tmn_erai, 'xy', /NAN)
  tmn_erai_2d = MEAN(tmn_erai[*,*,5:7], DIMENSION=3, /NAN)

  erai_file = 'SWVL_1m_ERAI_1989-2009.nc'
  mean_swv_erai = read_ncdf('SWVL', 0, 11, filename=erai_path+erai_file, /ALLRECORDS, /NOSTRUCT, /TIMESTEP, DIREC='z') & help, mean_swv_erai
  FOR t = 0, n_elements(mean_swv_erai[0,0,*])-1 DO mean_swv_erai[*,*,t] = mean_swv_erai[*,*,t] * seamask
  mean_swv_erai_ts = GROSSEMOYENNE(mean_swv_erai, 'xy', /NAN)
  mean_swv_erai_2d = MEAN(mean_swv_erai[*,*,5:7], DIMENSION=3, /NAN)

  erai_file = 'STL_1m_ERAI_1989-2009.nc'
  mean_tsoil_erai = read_ncdf('STL', 0, 11, filename=erai_path+erai_file, /ALLRECORDS, /NOSTRUCT, /TIMESTEP, DIREC='z') & help, mean_tsoil_erai
  FOR t = 0, n_elements(mean_tsoil_erai[0,0,*])-1 DO mean_tsoil_erai[*,*,t] = mean_tsoil_erai[*,*,t] * seamask
  mean_tsoil_erai_ts = GROSSEMOYENNE(mean_tsoil_erai, 'xy', /NAN)
  mean_tsoil_erai_2d = MEAN(mean_tsoil_erai[*,*,5:7], DIMENSION=3, /NAN)

ENDIF


IF data_type EQ '1d' THEN BEGIN

  erai_file = 'RAIN_1d_ERAI_1989.nc'
  rain_erai = read_ncdf( 'cp', 0, nbday-1, filename=erai_path+erai_file, /ALLRECORDS, /NOSTRUCT, /TIMESTEP)*4.*1000. + $
              read_ncdf('lsp', 0, nbday-1, filename=erai_path+erai_file, /ALLRECORDS, /NOSTRUCT, /TIMESTEP)*4.*1000.   & help, rain_erai
  FOR t = 0, n_elements(rain_erai[0,0,*])-1 DO rain_erai[*,*,t] = rain_erai[*,*,t] * seamask
  rain_erai_ts = GROSSEMOYENNE(rain_erai, 'xy', /NAN)
  rain_erai_2d = MEAN(rain_erai, DIMENSION=3, /NAN)

  erai_file = 'SKT_1d_ERAI_1989.nc'
  tsk_erai = read_ncdf('skt', 0, nbday-1, filename=erai_path+erai_file, /ALLRECORDS, /NOSTRUCT, /TIMESTEP) & help, tsk_erai
  FOR t = 0, n_elements(tsk_erai[0,0,*])-1 DO tsk_erai[*,*,t] = tsk_erai[*,*,t] * seamask
  tsk_erai_ts = GROSSEMOYENNE(tsk_erai, 'xy', /NAN)
  tsk_erai_2d = MEAN(tsk_erai, DIMENSION=3, /NAN)

  erai_file = 'LH_1d_ERAI_1989.nc'
  lh_erai = read_ncdf('slhf', 0, nbday-1, filename=erai_path+erai_file, /ALLRECORDS, /NOSTRUCT, /TIMESTEP) & help, lh_erai
  FOR t = 0, n_elements(lh_erai[0,0,*])-1 DO lh_erai[*,*,t] = lh_erai[*,*,t] * seamask
  lh_erai_ts = GROSSEMOYENNE(lh_erai, 'xy', /NAN)
  lh_erai_2d = MEAN(lh_erai, DIMENSION=3, /NAN)

  erai_file = 'HFX_1d_ERAI_1989.nc'
  hfx_erai = read_ncdf('sshf', 0, nbday-1, filename=erai_path+erai_file, /ALLRECORDS, /NOSTRUCT, /TIMESTEP) & help, hfx_erai
  FOR t = 0, n_elements(hfx_erai[0,0,*])-1 DO hfx_erai[*,*,t] = hfx_erai[*,*,t] * seamask
  hfx_erai_ts = GROSSEMOYENNE(hfx_erai, 'xy', /NAN)
  hfx_erai_2d = MEAN(hfx_erai, DIMENSION=3, /NAN)

  erai_file = 'SWVL_1d_ERAI_1989.nc'
  mean_swv_erai = read_ncdf('swvl', 0, nbday-1, filename=erai_path+erai_file, /ALLRECORDS, /NOSTRUCT, /TIMESTEP, DIREC='z') & help, mean_swv_erai
  FOR t = 0, n_elements(mean_swv_erai[0,0,*])-1 DO mean_swv_erai[*,*,t] = mean_swv_erai[*,*,t] * seamask
  mean_swv_erai_ts = GROSSEMOYENNE(mean_swv_erai, 'xy', /NAN)
  mean_swv_erai_2d = MEAN(mean_swv_erai, DIMENSION=3, /NAN)

  erai_file = 'STL_1d_ERAI_1989.nc'
  mean_tsoil_erai = read_ncdf('stl', 0, nbday-1, filename=erai_path+erai_file, /ALLRECORDS, /NOSTRUCT, /TIMESTEP, DIREC='z') & help, mean_tsoil_erai
  FOR t = 0, n_elements(mean_tsoil_erai[0,0,*])-1 DO mean_tsoil_erai[*,*,t] = mean_tsoil_erai[*,*,t] * seamask
  mean_tsoil_erai_ts = GROSSEMOYENNE(mean_tsoil_erai, 'xy', /NAN)
  mean_tsoil_erai_2d = MEAN(mean_tsoil_erai, DIMENSION=3, /NAN)

  erai_file = 'STL_1d_ERAI_1989.nc'
  tsoil_erai = read_ncdf('stl', 0, nbday-1, filename=erai_path+erai_file, /TIMESTEP, /NOSTRUCT) & help, tsoil_erai
  FOR t = 0, n_elements(tsoil_erai[0,0,0,*])-1 DO FOR z = 0, n_elements(tsoil_erai[0,0,*,0])-1 DO tsoil_erai[*,*,z,t] = tsoil_erai[*,*,z,t] * seamask
  tsoil_erai_ts = grossemoyenne(tsoil_erai, 'xy', /NAN)
  tsoil_erai_2d = MEAN(tsoil_erai, DIMENSION=3, /NAN)

  erai_file = 'SWVL_1d_ERAI_1989.nc'
  swv_erai = read_ncdf('swvl', 0, nbday-1, filename=erai_path+erai_file, /TIMESTEP, /NOSTRUCT) & help, swv_erai
  FOR t = 0, n_elements(swv_erai[0,0,0,*])-1 DO FOR z = 0, n_elements(swv_erai[0,0,*,0])-1 DO swv_erai[*,*,z,t] = swv_erai[*,*,z,t] * seamask
  swv_erai_ts = grossemoyenne(swv_erai, 'xy', /NAN) & help, swv_erai_ts
  swv_erai_2d = MEAN(swv_erai, DIMENSION=3, /NAN)

ENDIF


tsk_tmn0_2d_gridobs = fromreg('bilinear', tsk_tmn0_2d, lon_wrf, lat_wrf, lon_erai, lat_erai)
tsk_lag1_2d_gridobs = fromreg('bilinear', tsk_lag1_2d, lon_wrf, lat_wrf, lon_erai, lat_erai)
rain_tmn0_2d_gridobs = fromreg('bilinear', rain_tmn0_2d, lon_wrf, lat_wrf, lon_erai, lat_erai)
rain_lag1_2d_gridobs = fromreg('bilinear', rain_lag1_2d, lon_wrf, lat_wrf, lon_erai, lat_erai)
tmn_tmn0_2d_gridobs = fromreg('bilinear', tmn_tmn0_2d, lon_wrf, lat_wrf, lon_erai, lat_erai)
mean_swv_tmn0_2d_gridobs = fromreg('bilinear', mean_swv_tmn0_2d, lon_wrf, lat_wrf, lon_erai, lat_erai)
mean_tsoil_tmn0_2d_gridobs = fromreg('bilinear', mean_tsoil_tmn0_2d, lon_wrf, lat_wrf, lon_erai, lat_erai)
lh_tmn0_2d_gridobs = fromreg('bilinear', lh_tmn0_2d, lon_wrf, lat_wrf, lon_erai, lat_erai)
lh_lag1_2d_gridobs = fromreg('bilinear', lh_lag1_2d, lon_wrf, lat_wrf, lon_erai, lat_erai)
hfx_tmn0_2d_gridobs = fromreg('bilinear', hfx_tmn0_2d, lon_wrf, lat_wrf, lon_erai, lat_erai)
hfx_lag1_2d_gridobs = fromreg('bilinear', hfx_lag1_2d, lon_wrf, lat_wrf, lon_erai, lat_erai)
tsk_eratw_2d_gridobs = fromreg('bilinear', tsk_eratw_2d, lon_wrf, lat_wrf, lon_erai, lat_erai)
rain_eratw_2d_gridobs = fromreg('bilinear', rain_eratw_2d, lon_wrf, lat_wrf, lon_erai, lat_erai)
mean_swv_eratw_2d_gridobs = fromreg('bilinear', mean_swv_eratw_2d, lon_wrf, lat_wrf, lon_erai, lat_erai)
mean_tsoil_eratw_2d_gridobs = fromreg('bilinear', mean_tsoil_eratw_2d, lon_wrf, lat_wrf, lon_erai, lat_erai)
lh_eratw_2d_gridobs = fromreg('bilinear', lh_eratw_2d, lon_wrf, lat_wrf, lon_erai, lat_erai)
hfx_eratw_2d_gridobs = fromreg('bilinear', hfx_eratw_2d, lon_wrf, lat_wrf, lon_erai, lat_erai)
STOP



IF data_type EQ '1d' THEN BEGIN

  zl=3
  splot, indgen(nbday)+1, swv_erai_ts[zl,*], yrange=[0.20,0.35], title='1989 daily soil layer '+string(zl+1, format='(I1)')+' water volume evolution over India', xtitle='days', ytitle='water volume (m^3/m^3)', thick=2, charsize=1.5, XTicklen=1, YTicklen=1, XGridStyle=1, YGridStyle=1, lct=39 & $
  oplot, indgen(nbday)+1, swv_eratw_ts[zl,*], color=100, thick=2 & $
  oplot, indgen(nbday)+1, swv4_eratw_ts, color=100, thick=2, line=2 & $
  oplot, indgen(nbday)+1, swv_tmn0_ts[zl,*], color=150, thick=2 & $
  xyouts, 0.125, 0.175, 'ERA-Interim', /NORMAL, charsize=2 & $
  xyouts, 0.125, 0.125, 'ERATW', /NORMAL, charsize=2, color=100 & $
  xyouts, 0.125, 0.100, 'TMN CONSTANT', /NORMAL, charsize=2, color=150
  STOP

  zl=2
  splot, indgen(nbday)+1, swv_erai_ts[zl,*], yrange=[0.15,0.35], title='1989 daily soil layer '+string(zl+1, format='(I1)')+' water volume evolution over India', xtitle='days', ytitle='water volume (m^3/m^3)', thick=2, charsize=1.5, XTicklen=1, YTicklen=1, XGridStyle=1, YGridStyle=1, lct=39 & $
  oplot, indgen(nbday)+1, swv_eratw_ts[zl,*], color=100, thick=2 & $
  oplot, indgen(nbday)+1, swv3_eratw_ts, color=100, thick=2, line=2 & $
  oplot, indgen(nbday)+1, swv_tmn0_ts[zl,*], color=150, thick=2 & $
  xyouts, 0.125, 0.175, 'ERA-Interim', /NORMAL, charsize=2 & $
  xyouts, 0.125, 0.125, 'ERATW', /NORMAL, charsize=2, color=100 & $
  xyouts, 0.125, 0.100, 'TMN CONSTANT', /NORMAL, charsize=2, color=150
  STOP

  zl=1
  splot, indgen(nbday)+1, swv_erai_ts[zl,*], yrange=[0.15,0.35], title='1989 daily soil layer '+string(zl+1, format='(I1)')+' water volume evolution over India', xtitle='days', ytitle='water volume (m^3/m^3)', thick=2, charsize=1.5, XTicklen=1, YTicklen=1, XGridStyle=1, YGridStyle=1, lct=39 & $
  oplot, indgen(nbday)+1, swv_eratw_ts[zl,*], color=100, thick=2 & $
  oplot, indgen(nbday)+1, swv_tmn0_ts[zl,*], color=150, thick=2 & $
  xyouts, 0.125, 0.175, 'ERA-Interim', /NORMAL, charsize=2 & $
  xyouts, 0.125, 0.125, 'ERATW', /NORMAL, charsize=2, color=100 & $
  xyouts, 0.125, 0.100, 'TMN CONSTANT', /NORMAL, charsize=2, color=150
  STOP

  zl=0
  splot, indgen(nbday)+1, swv_erai_ts[zl,*], yrange=[0.10,0.35], title='1989 daily soil layer '+string(zl+1, format='(I1)')+' water volume evolution over India', xtitle='days', ytitle='water volume (m^3/m^3)', thick=2, charsize=1.5, XTicklen=1, YTicklen=1, XGridStyle=1, YGridStyle=1, lct=39 & $
  oplot, indgen(nbday)+1, swv_eratw_ts[zl,*], color=100, thick=2 & $
  oplot, indgen(nbday)+1, swv_tmn0_ts[zl,*], color=150, thick=2 & $
  xyouts, 0.125, 0.175, 'ERA-Interim', /NORMAL, charsize=2 & $
  xyouts, 0.125, 0.125, 'ERATW', /NORMAL, charsize=2, color=100 & $
  xyouts, 0.125, 0.100, 'TMN CONSTANT', /NORMAL, charsize=2, color=150
  STOP


  zl=3
  splot, indgen(nbday)+1, tsoil_erai_ts[zl,*]-273.15, yrange=[20,35], title='1989 daily soil layer '+string(zl+1, format='(I1)')+' temperature evolution over India', xtitle='days', ytitle='temperature (degC)', thick=2, charsize=1.5, XTicklen=1, YTicklen=1, XGridStyle=1, YGridStyle=1, lct=39 & $
  oplot, indgen(nbday)+1, tsoil_eratw_ts[zl,*]-273.15, color=100, thick=2 & $
  oplot, indgen(nbday)+1, tsoil4_eratw_ts-273.15, color=100, thick=2, line=2 & $
  oplot, indgen(nbday)+1, tsoil_tmn0_ts[zl,*]-273.15, color=150, thick=2 & $
  xyouts, 0.125, 0.175, 'ERA-Interim', /NORMAL, charsize=2 & $
  xyouts, 0.125, 0.125, 'ERATW', /NORMAL, charsize=2, color=100 & $
  xyouts, 0.125, 0.100, 'TMN CONSTANT', /NORMAL, charsize=2, color=150
  STOP

  zl=2
  splot, indgen(nbday)+1, tsoil_erai_ts[zl,*]-273.15, yrange=[15,35], title='1989 daily soil layer '+string(zl+1, format='(I1)')+' temperature evolution over India', xtitle='days', ytitle='temperature (degC)', thick=2, charsize=1.5, XTicklen=1, YTicklen=1, XGridStyle=1, YGridStyle=1, lct=39 & $
  oplot, indgen(nbday)+1, tsoil_eratw_ts[zl,*]-273.15, color=100, thick=2 & $
  oplot, indgen(nbday)+1, tsoil3_eratw_ts-273.15, color=100, thick=2, line=2 & $
  oplot, indgen(nbday)+1, tsoil_tmn0_ts[zl,*]-273.15, color=150, thick=2 & $
  xyouts, 0.125, 0.175, 'ERA-Interim', /NORMAL, charsize=2 & $
  xyouts, 0.125, 0.125, 'ERATW', /NORMAL, charsize=2, color=100 & $
  xyouts, 0.125, 0.100, 'TMN CONSTANT', /NORMAL, charsize=2, color=150
  STOP

  zl=1
  splot, indgen(nbday)+1, tsoil_erai_ts[zl,*]-273.15, yrange=[15,40], title='1989 daily soil layer '+string(zl+1, format='(I1)')+' temperature evolution over India', xtitle='days', ytitle='temperature (degC)', thick=2, charsize=1.5, XTicklen=1, YTicklen=1, XGridStyle=1, YGridStyle=1, lct=39 & $
  oplot, indgen(nbday)+1, tsoil_eratw_ts[zl,*]-273.15, color=100, thick=2 & $
  oplot, indgen(nbday)+1, tsoil_tmn0_ts[zl,*]-273.15, color=150, thick=2 & $
  xyouts, 0.125, 0.175, 'ERA-Interim', /NORMAL, charsize=2 & $
  xyouts, 0.125, 0.125, 'ERATW', /NORMAL, charsize=2, color=100 & $
  xyouts, 0.125, 0.100, 'TMN CONSTANT', /NORMAL, charsize=2, color=150
  STOP

  zl=0
  splot, indgen(nbday)+1, tsoil_erai_ts[zl,*]-273.15, yrange=[15,40], title='1989 daily soil layer '+string(zl+1, format='(I1)')+' temperature evolution over India', xtitle='days', ytitle='temperature (degC)', thick=2, charsize=1.5, XTicklen=1, YTicklen=1, XGridStyle=1, YGridStyle=1, lct=39 & $
  oplot, indgen(nbday)+1, tsoil_eratw_ts[zl,*]-273.15, color=100, thick=2 & $
  oplot, indgen(nbday)+1, tsoil_tmn0_ts[zl,*]-273.15, color=150, thick=2 & $
  xyouts, 0.125, 0.175, 'ERA-Interim', /NORMAL, charsize=2 & $
  xyouts, 0.125, 0.125, 'ERATW', /NORMAL, charsize=2, color=100 & $
  xyouts, 0.125, 0.100, 'TMN CONSTANT', /NORMAL, charsize=2, color=150
  STOP


  splot, indgen(nbday)+1, mean_tsoil_erai_ts, yrange=[0.15,0.35], title='1989 daily mean soil water volume evolution over India', xtitle='days', ytitle='water volume (m^3/m^3)', thick=2, charsize=1.5, XTicklen=1, YTicklen=1, XGridStyle=1, YGridStyle=1, lct=39 & $
  oplot, indgen(nbday)+1, mean_tsoil_eratw_ts, color=100, thick=2 & $
  oplot, indgen(nbday)+1, mean_tsoil_tmn0_ts, color=150, thick=2 & $
  xyouts, 0.125, 0.175, 'ERA-Interim', /NORMAL, charsize=2 & $
  xyouts, 0.125, 0.125, 'ERATW', /NORMAL, charsize=2, color=100 & $
  xyouts, 0.125, 0.100, 'TMN CONSTANT', /NORMAL, charsize=2, color=150
  STOP



  splot, indgen(nbday)+1, tsk_erai_ts-273.15, yrange=[15,40], title='1989 daily skin temperature evolution over India', xtitle='days', ytitle='tskin (degC)', thick=2, charsize=1.5, XTicklen=1, YTicklen=1, XGridStyle=1, YGridStyle=1, lct=39 & $
  oplot, indgen(nbday)+1, tsk_lag1_ts-273.15, color=50, thick=2 & $
  oplot, indgen(nbday)+1, tsk_eratw_ts-273.15, color=100, thick=2 & $
  oplot, indgen(nbday)+1, tsk_tmn0_ts-273.15, color=150, thick=2 & $
  xyouts, 0.125, 0.175, 'ERA-Interim  (mean='+STRING(MEAN(tsk_erai_ts-273.15), format='(F5.2)')+')', /NORMAL, charsize=2 & $
  xyouts, 0.125, 0.150, 'LAGDAY = 1   (mean='+STRING(MEAN(tsk_lag1_ts-273.15), format='(F5.2)')+')', /NORMAL, charsize=2, color=50 & $
  xyouts, 0.125, 0.125, 'ERATW        (mean='+STRING(MEAN(tsk_eratw_ts-273.15), format='(F5.2)')+')', /NORMAL, charsize=2, color=100 & $
  xyouts, 0.125, 0.100, 'TMN CONSTANT (mean='+STRING(MEAN(tsk_tmn0_ts-273.15), format='(F5.2)')+')', /NORMAL, charsize=2, color=150
  STOP

  splot, indgen(nbday)+1, hfx_erai_ts/lh_erai_ts, yrange=[0,5], title='1989 daily Bowen ratio over India', xtitle='days', ytitle='hfx / lh', thick=2, charsize=1.5, XTicklen=1, YTicklen=1, XGridStyle=1, YGridStyle=1, lct=39 & $
  oplot, indgen(nbday)+1, hfx_lag1_ts/lh_lag1_ts, color=50, thick=2 & $
  oplot, indgen(nbday)+1, hfx_eratw_ts/lh_eratw_ts, color=100, thick=2 & $
  oplot, indgen(nbday)+1, hfx_tmn0_ts/lh_tmn0_ts, color=150, thick=2 & $
  xyouts, 0.125, 0.175, 'ERA-Interim  (mean='+STRING(MEAN(hfx_erai_ts/lh_erai_ts), format='(F5.2)')+')', /NORMAL, charsize=2 & $
  xyouts, 0.125, 0.150, 'LAGDAY = 1   (mean='+STRING(MEAN(hfx_lag1_ts/hfx_lag1_ts), format='(F5.2)')+')', /NORMAL, charsize=2, color=50 & $
  xyouts, 0.125, 0.125, 'ERATW        (mean='+STRING(MEAN(hfx_eratw_ts), format='(F5.2)')+')', /NORMAL, charsize=2, color=100 & $
  xyouts, 0.125, 0.100, 'TMN CONSTANT (mean='+STRING(MEAN(hfx_tmn0_ts/hfx_tmn0_ts), format='(F5.2)')+')', /NORMAL, charsize=2, color=150
  STOP

  splot, indgen(nbday)+1, lh_erai_ts/21600.*(-1.), yrange=[0,120], title='1989 daily surface latent heat flux over India', xtitle='days', ytitle='lh (W/m2)', thick=2, charsize=1.5, XTicklen=1, YTicklen=1, XGridStyle=1, YGridStyle=1, lct=39 & $
  oplot, indgen(nbday)+1, lh_lag1_ts, color=50, thick=2 & $
  oplot, indgen(nbday)+1, lh_eratw_ts, color=100, thick=2 & $
  oplot, indgen(nbday)+1, lh_tmn0_ts, color=150, thick=2 & $
  xyouts, 0.125, 0.175, 'ERA-Interim  (mean='+STRING(MEAN(lh_erai_ts/21600.*(-1.)), format='(F5.2)')+')', /NORMAL, charsize=2 & $
  xyouts, 0.125, 0.150, 'LAGDAY = 1   (mean='+STRING(MEAN(lh_lag1_ts), format='(F5.2)')+')', /NORMAL, charsize=2, color=50 & $
  xyouts, 0.125, 0.125, 'ERATW        (mean='+STRING(MEAN(lh_eratw_ts), format='(F5.2)')+')', /NORMAL, charsize=2, color=100 & $
  xyouts, 0.125, 0.100, 'TMN CONSTANT (mean='+STRING(MEAN(lh_tmn0_ts), format='(F5.2)')+')', /NORMAL, charsize=2, color=150
  STOP

  splot, indgen(nbday)+1, hfx_erai_ts/21600.*(-1.), yrange=[0,120], title='1989 daily surface sensible heat flux over India', xtitle='days', ytitle='hfx (W/m2)', thick=2, charsize=1.5, XTicklen=1, YTicklen=1, XGridStyle=1, YGridStyle=1, lct=39 & $
  oplot, indgen(nbday)+1, hfx_lag1_ts, color=50, thick=2 & $
  oplot, indgen(nbday)+1, hfx_eratw_ts, color=100, thick=2 & $
  oplot, indgen(nbday)+1, hfx_tmn0_ts, color=150, thick=2 & $
  xyouts, 0.125, 0.175, 'ERA-Interim  (mean='+STRING(MEAN(hfx_erai_ts/21600.*(-1.)), format='(F5.2)')+')', /NORMAL, charsize=2 & $
  xyouts, 0.125, 0.150, 'LAGDAY = 1   (mean='+STRING(MEAN(hfx_lag1_ts), format='(F5.2)')+')', /NORMAL, charsize=2, color=50 & $
  xyouts, 0.125, 0.125, 'ERATW        (mean='+STRING(MEAN(hfx_eratw_ts), format='(F5.2)')+')', /NORMAL, charsize=2, color=100 & $
  xyouts, 0.125, 0.100, 'TMN CONSTANT (mean='+STRING(MEAN(hfx_tmn0_ts), format='(F5.2)')+')', /NORMAL, charsize=2, color=150
  STOP

  splot, indgen(nbday)+1, rain_erai_ts, yrange=[0,15], title='1989 daily precipitation evolution over India', xtitle='days', ytitle='rain (mm)', thick=2, charsize=1.5, XTicklen=1, YTicklen=1, XGridStyle=1, YGridStyle=1, lct=39 & $
  oplot, indgen(nbday)+1, rain_lag1_ts, color=50, thick=2 & $
  oplot, indgen(nbday)+1, rain_eratw_ts, color=100, thick=2 & $
  oplot, indgen(nbday)+1, rain_tmn0_ts, color=150, thick=2 & $
  xyouts, 0.125, 0.175, 'ERA-Interim  (mean='+STRING(MEAN(rain_erai_ts), format='(F5.2)')+')', /NORMAL, charsize=2 & $
  xyouts, 0.125, 0.150, 'LAGDAY = 1   (mean='+STRING(MEAN(rain_lag1_ts), format='(F5.2)')+')', /NORMAL, charsize=2, color=50 & $
  xyouts, 0.125, 0.125, 'ERATW        (mean='+STRING(MEAN(rain_eratw_ts), format='(F5.2)')+')', /NORMAL, charsize=2, color=100 & $
  xyouts, 0.125, 0.100, 'TMN CONSTANT (mean='+STRING(MEAN(rain_tmn0_ts), format='(F5.2)')+')', /NORMAL, charsize=2, color=150
  STOP

ENDIF


END
