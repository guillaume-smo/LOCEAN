
  FOR e = 0, n_elements(exp_list)-1 DO BEGIN
    cmd = execute( 'filter_sc, var_ts_'+strtrim(e,2)+', date2jul(time_'+strtrim(e,2)+'), var_sc1d_'+strtrim(e,2) )
    cmd = execute( 'var_tsnosc_'+strtrim(e,2)+' = var_ts_'+strtrim(e,2)+' - var_sc1d_'+strtrim(e,2) )
    cmd = execute( 'calcul_fft, var_tsnosc_'+strtrim(e,2)+'[ind_mean1d_'+strtrim(e,2)+'], data_type, var_psp_'+strtrim(e,2)+', period_'+strtrim(e,2) )
    cmd = execute( 'var_psp_'+strtrim(e,2)+'[ WHERE( var_psp_'+strtrim(e,2)+' LT 1.e-10) ] = !VALUES.F_NAN' )
  ENDFOR

  ;var_plot = 'var_tsnosc'
  ;@plot_ts

  ;var_plot = 'var_sc1d'
  ;@plot_ts



  SPLOT, period_0, var_psp_0, /XLOG, /YLOG, $
     XRANGE=[10,90], XSTYLE=1
  STOP
