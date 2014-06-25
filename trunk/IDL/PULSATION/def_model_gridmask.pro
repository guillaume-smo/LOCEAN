
print, 'DEF_MODEL_GRIDMASK...'


; GRID DEFINITION
IF STRMATCH(exp_name, 'tr12*'  ) THEN grid = 'trop12'
IF STRMATCH(exp_name, 'tr025*' ) THEN grid = 'trop025'
IF STRMATCH(exp_name, 'tr075*' ) THEN grid = 'trop075'
IF STRMATCH(exp_name, 'ind025*') THEN grid = 'ind025'
help, grid


; MODEL DEFINITION
IF STRMATCH(exp_name, '*_ra*'  ) THEN model = 'wrf'
IF STRMATCH(exp_name, '*_bmj*' ) THEN model = 'wrf'
IF STRMATCH(exp_name, '*_cpl*' ) THEN model = 'now'
IF STRMATCH(exp_name, '*now_*' ) THEN model = 'now'
IF STRMATCH(exp_name, '*_erai*') THEN model = 'nemo'
IF STRMATCH(exp_name, '*_quik*') THEN model = 'nemo'
help, model


; FILES & GRIDS PATHS
IF grid EQ 'ind025' THEN path = '/ccc/store/cont005/ra1047/samsong/'+grid+'_'+model+'/outputs/'+exp_name+'/' $
ELSE path = '/ccc/store/cont005/ra0542/massons/'+grid+'_'+model+'/outputs/'+exp_name+'/'
grid_path = '/ccc/cont005/home/ra1047/samsong/CONF_GRIDS/'
geog_path = '/ccc/work/cont005/ra0542/massons/now/input/wrf_'+grid+'l60/'
help, path, grid_path, geog_path


; FLAGS SETUP
IF model EQ 'wrf'    THEN flag_nemo = 0
IF model EQ 'nemo'   THEN flag_nemo = 1
IF model EQ 'now' THEN BEGIN  
  IF var_name EQ 'THETAO' OR $
     var_name EQ 'SO'     THEN BEGIN
       flag_z    = 1
       flag_mask = 0
  ENDIF
  IF var_name EQ 'SST'    OR $
     var_name EQ 'STRESS' OR $
     var_name EQ 'THETAO' OR $
     var_name EQ 'D20'    OR $
     var_name EQ 'SO'     THEN flag_nemo = 1 $
                          ELSE flag_nemo = 0
  IF grid  EQ 'ind025' THEN BEGIN
    force_nomean_wrf = 1
    IF var_name EQ 'SST' THEN flag_nemo = 0
  ENDIF
ENDIF


; WRF SETUP
IF model EQ 'wrf' OR model EQ 'now' AND flag_nemo EQ 0 THEN BEGIN

  ; INIT WRF
  grid_file = 'grid_'+model+'_'+grid+'.nc' & help, grid_file
  initncdf, grid_path+grid_file, glam=[20,380], /fullcgrid, ZAXISNAME='pressure' & domdef, box, /MEMEINDICE

  ; MASKS WRF
  highmask = read_ncdf('HGT_M', filename=grid_path+grid_file, /ALLRECORDS, /NOSTRUCT)
  orog = highmask
  highmask[WHERE(highmask GE highmask_height)] = !VALUES.F_NAN
  highmask[WHERE(highmask LT highmask_height)] = 1
  seamask  = read_ncdf('LANDMASK', filename=grid_path+grid_file, /ALLRECORDS, /NOSTRUCT)
  landmask = seamask
  landmask[where(seamask EQ 1.)] = !VALUES.F_NAN
  landmask[where(seamask EQ 0.)] = 1.
  seamask[ where(seamask EQ 0.)] = !VALUES.F_NAN
  help, landmask, seamask, highmask

ENDIF 


; NEMO SETUP
IF model EQ 'nemo' OR flag_nemo THEN BEGIN

  ; INIT NEMO
;  grid_path = '/ccc/work/cont005/ra0542/massons/now/input/'+model+'_'+grid+'/'
;  grid_file = 'mesh_mask_'+STRUPCASE(grid)+'.nc'
  grid_file = 'grid_nemo_'+grid+'.nc' & help, grid_file
  IF grid EQ 'ind025' THEN initncdf, grid_path+grid_file, glam=[20,380], /FULLCGRID, ZAXISNAME='deptht' $
                      ELSE initncdf, grid_path+grid_file, glam=[20,380], /FULLCGRID
   domdef, box, /MEMEINDICES

  ; MASKS NEMO
  IF grid EQ 'ind025' THEN landmask = FLOAT((read_ncdf( 'mask', filename=grid_path+grid_file, /ALLRECORDS, /NOSTRUCT, GRID='T'))) $
                      ELSE landmask = FLOAT((read_ncdf('tmask', filename=grid_path+grid_file, /ALLRECORDS, /NOSTRUCT, GRID='T'))[*,*,0])
  seamask  = landmask
  seamask[where(landmask EQ 0.)] = 1.
  seamask[where(landmask EQ 1.)] = !VALUES.F_NAN & help, seamask
  landmask[where(landmask EQ 0.)] = !VALUES.F_NAN & help, landmask
  force_landmask = 1 & force_seamask = 0

ENDIF


; SAUVEGARDE NECESSAIRES POUR INTERPOLATION
lon_mod = glamt[firstxt:lastxt,0] & help, lon_mod
lat_mod = REFORM(gphit[0,firstyt:lastyt]) & help, lat_mod
cmd = execute( 'lon_'+STRTRIM(e,2)+' = glamt[firstxt:lastxt,0] & help, lon_'+STRTRIM(e,2) )
cmd = execute( 'lat_'+STRTRIM(e,2)+' = REFORM(gphit[0,firstyt:lastyt]) & help, lat_'+STRTRIM(e,2) )
cmd = execute( 'grid_'+STRTRIM(e,2)+' = grid' )


print, 'DEF_MODEL_GRIDMASK OK' & print, ''
