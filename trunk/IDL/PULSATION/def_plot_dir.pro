
; PLOTS EXCEPTIONS
IF data_type EQ 'c1m' THEN plot_ts = 0
IF data_type EQ '1d' OR data_type EQ '6h' THEN plot_zp = 0
IF data_type NE '1d' THEN plot_sp = 0
IF data_type NE '1m' THEN plot_index = 0

IF write_ps OR write_gif THEN BEGIN
  plot_dir = 'FIGURES/' + plot_dir
  ;FILE_DELETE, plot_dir, /RECURSIVE, /ALLOW_NONEXISTENT
  FOR z = 0, n_elements(zone_list)-1 DO FOR v = 0, n_elements(var_list)-1 DO FILE_MKDIR, plot_dir+'/'+STRUPCASE(data_type)+'/'+zone_list[z]+'/'+var_list[v]
ENDIF
