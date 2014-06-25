    ; PLOT STDDEV ERROR SC1m
    IF n_elements(exp_list) GT 2 THEN BEGIN

      minvar = MIN(std_errvar_sc1m, /NAN) & maxvar = MAX(std_errvar_sc1m, /NAN)
      maxvar = maxvar + 0.05*(maxvar-minvar)
      minvar = minvar - 0.05*(maxvar-minvar)
      lct, 60 & fmt='(F6.1)'

      fig_name = var_name+'_STDDEV_ERROR_sc1m_'+zone
      IF write_ps THEN openps, filename=plot_dir+'/'+STRUPCASE(data_type)+'/'+zone+'/'+var_name+'/'+fig_name
      IF write_ps THEN thc = 6 ELSE thc = 2
      IF write_ps THEN chs = 1 ELSE chs = 1.5
      color_list = [0, 50, 250, 150, 200, 100, 25]

      splot, indgen(12)+1, std_errvar_sc1m, xtitle='MONTHS', title=mask_title+' '+var_name+' ERROR STDDEV SC - '+zone, charsize=1.5, ystyle=1, yrange=[minvar,maxvar], ytitle=var_name, lct=39, charthick=1.5, XTicklen=1, YTicklen=1, XGridStyle=1, YGridStyle=1, /nodata
      oplot, indgen(12)+1, std_errvar_sc1m, thick=thc, color=color_list[0]
      oplot, indgen(12)+1, mean(std_errvar_sc1m,/nan)+indgen(12)*0., thick=thc, color=color_list[0], line=2

      IF write_ps THEN closeps ELSE STOP

    ENDIF
