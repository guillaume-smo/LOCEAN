      dom_plt = dom_tc & plt_path = ''
      IF n_elements(par_list) EQ 2 THEN plt_path = '/home/gsamson/WORK/IDL/FIGURES/'+date_list[0]+'_'+par_list[0]+'_vs_'+par_list[1]+'/'
      IF n_elements(par_list) EQ 1 AND par_list[0] NE '' THEN plt_path = '/home/gsamson/WORK/IDL/FIGURES/'+par_list[0]+'/'
      IF n_elements(par_list) EQ 1 AND par_list[0] EQ '' THEN plt_path = '/home/gsamson/WORK/IDL/FIGURES/'+tc_name+'/'
      IF n_elements(date_list) EQ 1 THEN plt_path = '/home/gsamson/WORK/IDL/FIGURES/'+date_list[0]+'_'+tc_name+'/'
      IF n_elements(par_list) GT 2 AND n_elements(date_list) GT 1 THEN plt_path = '/home/gsamson/WORK/IDL/FIGURES/'+tc_name+'_'+date_list[0]+'-'+date_list[n_elements(date_list)-1]+'/'	
      IF n_elements(date_list) GT 1 AND  n_elements(par_list) EQ 2 THEN plt_path = '/home/gsamson/WORK/IDL/FIGURES/'+par_list[0]+'_vs_'+par_list[1]+'/'
      IF date_list[0] EQ '' AND par_list[0] EQ '' THEN plt_path = '/home/gsamson/WORK/IDL/FIGURES/'+tc_name+'/'
      IF plt_path EQ '' THEN STOP ELSE FILE_MKDIR, plt_path, /NOEXPAND_PATH
