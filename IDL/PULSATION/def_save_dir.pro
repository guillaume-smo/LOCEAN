
print, 'DEF_SAVE_DIR...'


store_dir = '/ccc/store/cont005/ra1047/samsong/'
IF WHERE( STRMATCH( exp_list[0], 'tr*')  EQ 0) EQ -1 OR $
   WHERE( STRMATCH( exp_list[0], 'ind*') EQ 0) EQ -1 THEN obs_name = exp_name

IF STRMATCH( exp_name, 'tr*') OR STRMATCH( exp_name, 'ind*') THEN BEGIN

  save_dir = store_dir + grid +'_'+ model + '/outputs/' + exp_name + '/' & help, save_dir
  FILE_MKDIR, save_dir

  IF force_landmask THEN mask_title = 'OCEAN'
  IF force_seamask  AND force_highmask      THEN mask_title = 'LANDLOW'
  IF force_seamask  AND force_highmask EQ 0 THEN mask_title = 'LANDALL'
  IF force_landmask EQ 0 AND force_seamask EQ 0 AND force_highmask EQ 0 THEN mask_title = 'OCEAN+LANDALL'
  IF force_landmask EQ 0 AND force_seamask EQ 0 AND force_highmask      THEN mask_title = 'OCEAN+LANDLOW'

  save_file = var_name +'_'+ data_type +'_'+ zone +'_'+ mask_title +'_'+ period +'_'+ STRTRIM(FIX(yearini_mod),2) +'-'+ STRTRIM(FIX(yearend_mod),2) +'.idl'
  help, save_file

  gridobs_file = var_name +'_'+ data_type +'_'+ zone +'_'+ mask_title +'_'+ period +'_'+ STRTRIM(FIX(yearini_mod),2) +'-'+ STRTRIM(FIX(yearend_mod),2) +'_grid'+obs_name+'.idl'
  help, gridobs_file

  IF FILE_TEST(save_dir+save_file) EQ 0 THEN BEGIN
    load_data = 0
    save_data = 1
  ENDIF ELSE BEGIN
    load_data = 1
    save_data = 0
  ENDELSE

  IF FILE_TEST(save_dir+gridobs_file) EQ 0 THEN BEGIN
    load_gridobs = 0
    save_gridobs = 1
  ENDIF ELSE BEGIN
    load_gridobs = 1
    save_gridobs = 0
  ENDELSE

ENDIF


IF force_norestore THEN BEGIN
  load_data = 0
  save_data = 1
  load_gridobs = 0
  save_gridobs = 1
ENDIF

help, save_data, load_data, save_gridobs, load_gridobs


print, 'DEF_SAVE_DIR OK' & print, ''
