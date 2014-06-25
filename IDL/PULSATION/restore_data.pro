
;IF save_data EQ 0 AND load_data AND load_gridobs EQ 0 AND $
IF save_data EQ 0 AND load_data AND $
  (STRMATCH( exp_name, 'tr*') OR STRMATCH( exp_name, 'ind*')) THEN BEGIN

  print, 'RESTORING DATA IDL FILE: '+save_dir+save_file
  RESTORE, save_dir+save_file, /VERBOSE
  print, 'OK' & print, ''

ENDIF
