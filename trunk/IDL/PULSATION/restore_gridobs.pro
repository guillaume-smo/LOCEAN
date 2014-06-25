IF save_gridobs EQ 0 AND load_gridobs AND (STRMATCH( exp_name, 'tr*') OR STRMATCH( exp_name, 'ind*')) THEN BEGIN

  print, 'RESTORING GRIDOBS IDL FILE: '+save_dir+gridobs_file
  RESTORE, save_dir+gridobs_file, /VERBOSE
  print, 'OK' & print, ''

ENDIF
