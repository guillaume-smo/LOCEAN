      IF write_ps THEN openps, filename=var_name+'_index_'+zone+'_'+period+'_'+STRTRIM(yearini_mod,2)+'-'+STRTRIM(yearend_mod,2)+'_'+exp_list[1]+'_'+exp_list[2]+'.ps'
      IF write_ps THEN thc = 6 ELSE thc = 2
      CASE var_name OF
        'SST': BEGIN
          IF zone EQ 'UPW' THEN BEGIN & minvar=26.5 & maxvar=30 & ENDIF
        END
        'RAIN': BEGIN
          minvar=0 & maxvar=20
        END
      ENDCASE
      color_list = [0, 50, 250, 150, 200, 100]

      splot, yearlist_mod, indice_0, xtitle='YEARS', ytitle=var_name+' INDEX', charsize=1.5, ystyle=1, yrange=[minvar,maxvar], title=var_name+' INDEX - ZONE: '+zone+' - PERIOD: '+period, lct=39, charthick=1.5
      FOR e = 0, n_elements(exp_list)-1 DO BEGIN
        cmd = execute( 'oplot, yearlist_mod, indice_'+strtrim(e,2)+', thick=thc, color=color_list[e]' )
        cmd = execute( 'oplot, yearlist_mod, MEAN(indice_'+strtrim(e,2)+', /NAN)+FLTARR(nbyear_mod), thick=thc, line=2, color=color_list[e]' )
        xyouts, 0.125, 0.175-e*0.025, exp_list[e], /NORMAL, charsize=chs, color=color_list[e], charthick=1
        cmd = execute( 'xyouts, 0.45, 0.175-e*0.025, "MEAN="+strtrim(MEAN(indice_'+strtrim(e,2)+', /NAN),2), /NORMAL, charsize=chs, color=color_list[e], charthick=1' )
        IF e GT 0 THEN cmd = execute( 'xyouts, 0.70, 0.175-e*0.025, "CORR_OBS-MOD="+strtrim(coorind_0'+strtrim(e,2)+',2), /NORMAL, charsize=chs, color=color_list[e], charthick=1' )
      ENDFOR

    STOP

      splot, yearlist_mod, (indice_0-mean(indice_0,/nan))/stddev(indice_0, /NAN), xtitle='YEARS', ytitle=var_name+'NORMALIZED INDEX', charsize=1.2, thick=thc, ystyle=1, yrange=[-3,3], title=var_name+' NORMALIZED INDEX - ZONE: '+zone+' - PERIOD: '+period, lct=33
      oplot, yearlist_mod, (indice_1-mean(indice_1,/nan))/stddev(indice_1, /NAN), thick=thc, color=50
      oplot, yearlist_mod, (indice_2-mean(indice_2,/nan))/stddev(indice_2, /NAN), thick=thc, color=210
      oplot, yearlist_mod, yearlist_mod*0., thick=thc

      xyouts, 1989, -4, exp_list[0], /DATA, charsize=1.5
      xyouts, 1989, -4.3, exp_list[1], /DATA, charsize=1.5, color=50
      xyouts, 1989, -4.6, exp_list[2], /DATA, charsize=1.5, color=210
      indok = WHERE( FINITE(indice_0) EQ 1 AND FINITE(indice_1) EQ 1, nbok)
      xyouts, 1990, 2.6, 'MEAN='+strtrim(mean(indice_0,/nan),2), /DATA, charsize=1
      xyouts, 1996, 2.6, 'MEAN='+strtrim(mean(indice_1[indok],/nan),2), /DATA, charsize=1, color=50
      xyouts, 2001, 2.6, 'MEAN='+strtrim(mean(indice_2[indok],/nan),2), /DATA, charsize=1, color=210
      xyouts, 1990, 2.3, 'STDDEV='+strtrim(stddev(indice_0,/nan),2), /DATA, charsize=1
      xyouts, 1996, 2.3, 'STDDEV='+strtrim(stddev(indice_1[indok],/nan),2), /DATA, charsize=1, color=50
      xyouts, 2001, 2.3, 'STDDEV='+strtrim(stddev(indice_2[indok],/nan),2), /DATA, charsize=1, color=210
      xyouts, 1990, 2.0, 'CORR_OBS=', /DATA, charsize=1
      xyouts, 1996, 2.0, strtrim(coorind_01,2), /DATA, charsize=1, color=50
      xyouts, 2001, 2.0, strtrim(coorind_02,2), /DATA, charsize=1, color=210
      xyouts, 1990, 1.7, 'CORR_MOD='+strtrim(coorind_12,2), /DATA, charsize=1

      IF write_ps THEN closeps ; & STOP
      IF debug THEN STOP
