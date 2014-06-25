; PLOT 1D TIME SERIES (6h / 1d / 1m)


IF STRPOS( var_plot, '_') EQ STRLEN( var_plot)-1 THEN var_plot = STRMID( var_plot, 0, STRLEN( var_plot)-1)
print, 'plot_1D_t: '+var_plot



;-------------------------------------------------------------------------------------------------
; PLOT SETUP
;-------------------------------------------------------------------------------------------------

    key_portrait = 1
    fig_name = var_name+'_'+var_plot+'_'+data_type+'_'+zone+'_'+STRTRIM(yearini_mod,2)+'-'+STRTRIM(yearend_mod,2)
    IF write_ps THEN openps, filename=plot_dir+'/'+STRUPCASE(data_type)+'/'+zone+'/'+var_name+'/'+fig_name
    IF write_ps THEN thc = 6 ELSE thc = 2
    IF write_ps THEN chs = 1 ELSE chs = 1.5
    color_list = [0, 50, 250, 150, 200, 100, 25, 75, 175, 225, 125, 60, 260, 160, 210]
    IF force_landmask THEN mask_title = 'OCEAN'
    IF force_seamask  THEN mask_title = 'LAND'
    IF force_landmask EQ 0 AND force_seamask EQ 0 THEN mask_title = 'OCEAN+LAND'
    IF STRMATCH(var_plot, '*ts1y*')EQ 1 OR (data_type EQ '1m' OR data_type EQ 'c1m' OR data_type EQ '6h') THEN smooth_coef = 0
    IF STRMATCH( var_plot, '*tcday*') OR STRMATCH( var_plot, '*cg*') THEN flag_tc = 1 ELSE flag_tc = 0
 

;-------------------------------------------------------------------------------------------------
; PERIOD SELECTION + TIME AXIS DEFINITION
;-------------------------------------------------------------------------------------------------

    IF STRMATCH(var_plot, '*_ts*') THEN BEGIN

      IF data_type EQ '1d' THEN BEGIN
        tbeg = where(time_0 EQ strtrim(yearini_mod,2)+'0101.00d') & print, tbeg
        tend = where(time_0 EQ strtrim(yearend_0,2)+'1231.00d') & print, tend
        tmp  = LABEL_DATE(DATE_FORMAT='%M')
        xticks_nb = 12-1
      ENDIF
      IF data_type EQ '6h' THEN BEGIN
        tbeg = where(time_0 EQ strtrim(yearini_mod,2)+'0101.00d') & print, tbeg
        tend = where(time_0 EQ strtrim(yearend_0,2)+'0131.75d') & print, tend
        tmp  = LABEL_DATE(DATE_FORMAT='%D')
        xticks_nb = 31-1
      ENDIF
      IF data_type EQ '1m' THEN BEGIN
        tbeg = where(time_0 EQ double(yearini_mod*10000.0)+0115.0) & print, tbeg
        tend = where(time_0 EQ double(yearend_0*10000.0)+1215.0) & print, tend
        IF tend-tbeg+1 GT 12 THEN tmp = LABEL_DATE(DATE_FORMAT='%Y') ELSE tmp = LABEL_DATE(DATE_FORMAT='%M')
        IF nbyear LE 10 THEN xticks_nb = nbyear_0 
        IF nbyear GT 10 AND nbyear LE 30 THEN xticks_nb = nbyear_0 / 2.  
        IF nbyear GT 30 THEN xticks_nb = nbyear_0 / 4. ;tend-tbeg+1
      ENDIF
      IF tbeg EQ -1 OR tend EQ -1 THEN STOP
      taxis = date2jul(time_0[tbeg:tend]) + 0.50d ; need "+0.5" for LABEL_DATE function to work properly
      taxis_name = 'time'

    ENDIF

    ; 1y TS CASE
    IF STRMATCH(var_plot, '*ts1y*') THEN BEGIN
      tbeg = where(listyear_0 EQ yearini_mod) & print, tbeg
      tend = where(listyear_0 EQ yearend_0) & print, tend
      IF tbeg EQ -1 OR tend EQ -1 THEN STOP
      tmp   = LABEL_DATE(DATE_FORMAT='%Y')
      taxis = date2jul(DOUBLE(listyear_0[tbeg:tend]+1.)*10000.+101.)
      taxis_name = 'YEARS'
      IF nbyear LE 10 THEN xticks_nb = nbyear_0
      IF nbyear GT 10 AND nbyear LE 30 THEN xticks_nb = nbyear_0 / 2.
      IF nbyear GT 30 THEN xticks_nb = nbyear_0 / 4. ;tend-tbeg+1
    ENDIF
    tbeg = tbeg[0]
    tend = tend[0]

    IF data_type EQ 'c1m' THEN BEGIN
      tmp   = LABEL_DATE(DATE_FORMAT='%N')
      taxis = date2jul(time_0)
      taxis_name = 'MONTHS'
      xticks_nb  = 12-1
    ENDIF


;-------------------------------------------------------------------------------------------------
; PLOT
;-------------------------------------------------------------------------------------------------

    @def_plot_minmax

    IF smooth_coef THEN cmd = execute( 'var_y = TS_SMOOTH('+var_plot+'_'+strtrim(ebeg,2)+'[tbeg:tend], smooth_coef)' ) $
    ELSE cmd = execute( 'var_y = '+var_plot+'_'+strtrim(ebeg,2)+'[tbeg:tend]' )
    help, taxis, var_y
  
    IF STRMATCH( var_plot, '*tcday*') THEN titley = 'TC.DAY/YEAR'
    IF STRMATCH( var_plot, '*cg*')    THEN titley = 'CG/YEAR'
    IF flag_tc EQ 0 THEN titley = var_name
    IF flag_tc EQ 0 THEN titlev = var_name ELSE titlev = 'CYCLONE'

    splot, taxis, var_y, XTICKFORMAT = 'LABEL_DATE',  xtitle=taxis_name, ytitle=titley, charsize=1.5, ystyle=1, yrange=[minvar,maxvar], title=mask_title+' '+titlev+' '+var_plot+' '+data_type+' TIME SERIES - '+zone, lct=39, charthick=1.5, /nodata, XTicklen=1, YTicklen=1, XGridStyle=1, YGridStyle=1, Position=[0.1,0.3,0.9,0.7], XTICKS=xticks_nb

    ecol = 0
    FOR e = ebeg, n_elements(exp_list)-1 DO BEGIN

      IF smooth_coef THEN cmd = execute( 'var_y = TS_SMOOTH('+var_plot+'_'+strtrim(e,2)+', smooth_coef)' ) ELSE cmd = execute( 'var_y = '+var_plot+'_'+strtrim(e,2) )
      IF STRMATCH(var_plot, '*_ts'  ) THEN cmd = execute( 'taxis = date2jul( time_'+strtrim(e,2)+')+0.50d' )
      IF STRMATCH(var_plot, '*_ts1y') THEN cmd = execute( 'taxis = date2jul( DOUBLE( listyear_'+strtrim(e,2)+'+1.)*10000+101)' )

      oplot, taxis, var_y, thick=thc, color=color_list[ecol]

      IF flag_tc EQ 0 THEN BEGIN
        IF exp_list[e] EQ 'OBS' THEN xyouts, 0.05, 0.190-ecol*0.030, obs_name, /NORMAL, charsize=chs, color=color_list[ecol], charthick=1 $
        ELSE xyouts, 0.05, 0.190-ecol*0.030, exp_list[e], /NORMAL, charsize=chs, color=color_list[ecol], charthick=1
      ENDIF ELSE BEGIN
        IF exp_list[e] EQ 'OBS' THEN xyouts, 0.05, 0.190-ecol*0.030, 'IBTRACS', /NORMAL, charsize=chs, color=color_list[ecol], charthick=1 $
        ELSE xyouts, 0.05, 0.190-ecol*0.030, exp_list[e], /NORMAL, charsize=chs, color=color_list[ecol], charthick=1
      ENDELSE

      ; MEAN
      xyouts, 0.35, 0.190-ecol*0.030, 'MEAN='+string(MEAN(var_y, /NAN), format='(F7.2)'), /NORMAL, charsize=chs, color=color_list[ecol], charthick=1
      IF STRMATCH(var_plot, 'var_*') OR STRMATCH(var_plot, 'nb*') THEN oplot, taxis, MEAN(var_y, /NAN)+fltarr(n_elements(var_y)), thick=thc, color=color_list[ecol], line=2 $
      ELSE oplot, taxis, 0.*taxis, thick=thc

      ; STD_DEV
      xyouts, 0.50, 0.190-ecol*0.030, 'STD_DEV='+string(STDDEV(var_y, /NAN), format="(F5.2)"), /NORMAL, charsize=chs, color=color_list[ecol], charthick=1
      IF STRMATCH(var_plot, 'ano*') THEN oplot, taxis, STDDEV(var_y, /NAN)+fltarr(n_elements(var_y)), thick=thc, color=color_list[ecol], line=2
      IF STRMATCH(var_plot, 'ano*') THEN oplot, taxis, -1. * STDDEV(var_y, /NAN)+fltarr(n_elements(var_y)), thick=thc, color=color_list[ecol], line=2

      ; CORRELATIONS
      IF STRMATCH(var_plot, '*_ts', /FOLD_CASE)   THEN var_cor = 'corrts'
      IF STRMATCH(var_plot, '*_ts1y', /FOLD_CASE) THEN var_cor = 'corrind'
      IF STRMATCH(var_plot, '*_ts', /FOLD_CASE) OR STRMATCH(var_plot, '*_ts1y', /FOLD_CASE) THEN $
      IF e GT 0 THEN cmd = execute( 'xyouts, 0.70, 0.190-e*0.030, "CORR_OBS-MOD="+string('+var_cor+'_0'+strtrim(e,2)+'[0], format="(F5.2)"), /NORMAL, charsize=chs, color=color_list[ecol], charthick=1' )
      IF plot_TC AND STRMATCH(var_plot, 'cg')THEN var_cor = 'cor_nbcg_1y'
      IF plot_TC AND STRMATCH(var_plot, 'cg')THEN var_cor = 'cor_nbcg_1y'


      ;xyouts, 0.50, 0.180-e*0.030, "LIN_TREND="+string(TREND(var_y, /NAN)*100., format="(F6.2)"), /NORMAL, charsize=chs, color=color_list[ecol], charthick=1
      ;IF e GT 0 THEN BEGIN
      ;  cmd = execute( 'bias = (MEAN(var_y, /NAN)-MEAN('+var_plot+'_0, /NAN)) / MEAN('+var_plot+'_0, /NAN)*100.' )
      ;  xyouts, 0.35, 0.180-e*0.030, 'BIAS='+string(bias, format='(F5.1)')+' %', /NORMAL, charsize=chs, color=color_list[ecol], charthick=1
      ;  cmd = execute( 'rmse = SQRT( TOTAL( (var_y - '+var_plot+'_0)^2, /NAN) / n_elements(var_y)  )' )
      ;  xyouts, 0.70, 0.180-e*0.030, 'RMSE='+string(rmse, format='(F5.1)'), /NORMAL, charsize=chs, color=color_list[ecol], charthick=1
      ;ENDIF

      ecol = ecol + 1

    ENDFOR

    IF write_ps THEN closeps ELSE STOP
