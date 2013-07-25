PRO colloc_TMIAMSRE_1D_to_tc, path_file_wsc_wcy, path_file_nsc_wcy, var_nm, datebeg_sf, datebeg_ok, dateend_ok, $
                              day_before, day_after,dt_bnd, freq, path_file_tc_d1, fileout
 
  @common

; path_file_wsc_wcy      file for calculation of the "current day" value and the "before cyclone" value
; path_file_nsc_wcy      seasonnal cycle removed file for calculation of the timeseries and delta after - before
; var_nm                 variable name to collocalise (name of variable in ncdf file)
; datebeg (dateend)      YYYYMMDD dates begin (end) to perform colloc (not before 19780101, not after 20071231)
; day_before (day_after) bnds of the "before cyclone" ("after cyclone") [-day0,-day1] in days
; dt_bnd                 bnds [-day0,+day1] for the timeseries calculation
  
  
; Begin define constants
  miss_val    = 0.		; valeur manquante
  mean_radius = 200. 		; rayon utilise pour calculer les moyennes en km
; End define constants  
 

; Begin retreive mask + cyclone parameters
  initncdf, path_file_wsc_wcy
;  var = read_ncdf(var_nm, 0,0, /timestep, filename = path_file_nsc_wcy, /nostruct)
;  mask = var & mask[where(finite(var) EQ 1)] = 1 & mask[where(finite(var) EQ 0)] = 0
  mask = read_ncdf('mask', 0,0, /timestep, filename = path_file_wsc_wcy, /nostruct)
  
  restore, path_file_tc_d1, /VERBOSE
  igt_tc_oc = indgen(n_elements(d1_lon[*,0])) & help, igt_tc_oc
  sz_tab_tc = n_elements(d1_lon[0, *]) & help, sz_tab_tc
; End retreive mask + cyclone parameters
  

; Begin Initialize timeseries variables
  dt_ext_g = dt_bnd[0] & help, dt_ext_g
  dt_ext_d = dt_bnd[1] & help, dt_ext_d
  dt_nday  = dt_ext_d-dt_ext_g+1 & help, dt_nday	 ; minus dt_ext to plus dt_ext day from TC passage
  IF freq EQ '1D' THEN dt_axis = dt_ext_g + indgen(dt_nday)
  IF freq EQ '6H' THEN dt_axis = dt_ext_g + indgen((dt_nday-1)*4+1)*0.25 & help, dt_axis
  dt_var_wsc_wcy = fltarr(n_elements(igt_tc_oc), sz_tab_tc, n_elements(dt_axis)) & dt_var_wsc_wcy[*, *, *] = !values.f_nan         ; tms var dans rayon 200km
  dt_var_nsc_wcy = fltarr(n_elements(igt_tc_oc), sz_tab_tc, n_elements(dt_axis)) & dt_var_nsc_wcy[*, *, *] = !values.f_nan         ; tms var dans rayon 200km
  dt_var_ano_bef = fltarr(n_elements(igt_tc_oc), sz_tab_tc, n_elements(dt_axis)) & dt_var_ano_bef[*, *, *] = !values.f_nan         ; tms var run avec - 1 semaine avant
; End Initialize timeseries variables


; Begin Initialize 1 dimension variables
  d1_var_during = fltarr(n_elements(igt_tc_oc), sz_tab_tc) & d1_var_during[*, *] = !values.f_nan         ; var at the current day under TC position
  d1_var_before = fltarr(n_elements(igt_tc_oc), sz_tab_tc) & d1_var_before[*, *] = !values.f_nan         ; var 1 week before TC passage
  d1_var_after  = fltarr(n_elements(igt_tc_oc), sz_tab_tc) & d1_var_before[*, *] = !values.f_nan         ; var 1 week after TC passage
  d1_var_ano_mean_bef = fltarr(n_elements(igt_tc_oc), sz_tab_tc) & d1_var_ano_mean_bef[*, *] = !values.f_nan
  d1_var_ano_min_bef  = fltarr(n_elements(igt_tc_oc), sz_tab_tc) & d1_var_ano_min_bef[*, *]  = !values.f_nan
  d1_var_ano_max_bef  = fltarr(n_elements(igt_tc_oc), sz_tab_tc) & d1_var_ano_max_bef[*, *]  = !values.f_nan
  d1_var_ano_dur_bef  = fltarr(n_elements(igt_tc_oc), sz_tab_tc) & d1_var_ano_dur_bef[*, *]  = !values.f_nan
; End Initialize 1 dimension variables


; Begin Read full time axis for surface data
  fid = ncdf_open(path_file_wsc_wcy, /nowrite)
  tid = ncdf_dimid(fid,'time')
  juld_ini  = date2jul(datebeg_sf) & help, juld_ini
  ncdf_diminq, fid, tid, tname, tsize & help, tsize
  ncdf_close, fid
  IF freq EQ '6H' AND tsize EQ 13148 THEN time_full = double(juld_ini + indgen(tsize)*0.25)
  IF freq EQ '6H' AND tsize EQ 29220 THEN time_full = juld_ini + indgen(tsize)*0.25
  IF freq EQ '1D' AND tsize EQ 7305  THEN time_full = juld_ini + indgen(tsize)*1.
  IF freq EQ '1D' AND tsize EQ 4383  THEN time_full = juld_ini + indgen(tsize)*1.
  help, time_full
; End Read full time axis


; Begin loop on each TC
  FOR  jtc = 0, n_elements(igt_tc_oc)-1 DO  BEGIN
    idTC = igt_tc_oc[ jtc ]
    IF d1_date[idTC,0] GT datebeg_ok AND d1_date[idTC,0] LT dateend_ok THEN BEGIN
      i_lstc = (reverse(where(finite(d1_juld[idTC, *]) EQ  1)))[0] & help, i_lstc


;  Begin Define time period and region on which data are extracted
      datedeb_jul = d1_juld[idTC,0] + dt_ext_g & print, jul2date(datedeb_jul), format='(f12.3)'
      datefin_jul = d1_juld[idTC,i_lstc] + dt_ext_d  & print, jul2date(datefin_jul), format='(f12.3)'
      caldat, datedeb_jul, m, d, y, h & datedeb = jul2date(datedeb_jul)
      caldat, datefin_jul, m, d, y, h & datefin = jul2date(datefin_jul)
      lon_domin =  floor(min(d1_lon[idTC, *])-4) & help, lon_domin
      lon_domax = floor(max(d1_lon[idTC, *]+5)) & help, lon_domax
      lat_domin = (floor(min(d1_lat[idTC, *])-4) )  >   (-30) & help, lon_domin
      lat_domax = (floor(max(d1_lat[idTC, *])+5) )  < 25 & help, lat_domax
;      IF abs(lat_domin) GE 40 AND abs(lat_domax) GE 30 THEN GOTO, notcdata
      domdef, lon_domin, lon_domax, lat_domin, lat_domax, /memeindice
;  End Define time period and region on which data are extracted


;  Begin Read data with sc with cyclones and store in var_wsc_wcy
      t1 = (where(time_full GE datedeb_jul AND time_full LE datefin_jul))[0] & help, t1
      t2 = (reverse(where(time_full GE datedeb_jul AND time_full LE datefin_jul)))[0] & help, t2
      var_wsc_wcy = read_ncdf(var_nm, t1, t2, /timestep, filename = path_file_wsc_wcy, /nostruct)
      FOR jjt = 0, n_elements(time_full[t1:t2])-1 DO var_wsc_wcy[*, *, jjt] = var_wsc_wcy[*, *, jjt]*mask[firstxt:lastxt, firstyt:lastyt]
      help, var_wsc_wcy
      ith = where(var_wsc_wcy EQ miss_val)
      IF ith[0] NE -1 THEN var_wsc_wcy[ith] = !values.f_nan
;  End Read data with sc and store in var_wsc_wcy
      
        
;  Begin Read data without sc with cyclones and store in var_nsc_wsc
      t1 = (where(time_full GE datedeb_jul AND time_full LE datefin_jul))[0]
      t2 = (reverse(where(time_full GE datedeb_jul AND time_full LE datefin_jul)))[0]
      var_nsc_wcy = read_ncdf(var_nm, t1, t2, /timestep, filename = path_file_nsc_wcy, /nostruct)
      time = double(time_full[t1:t2])
      FOR jjt = 0, n_elements(time)-1 DO var_nsc_wcy[*, *, jjt] = var_nsc_wcy[*, *, jjt]*mask[firstxt:lastxt, firstyt:lastyt]
      help, var_nsc_wcy
      ith = where(var_nsc_wcy EQ miss_val)
      IF ith[0] NE -1 THEN var_nsc_wcy[ith] = !values.f_nan
;  End Read data without sc and store in var_nsc_wcy
      
            
;  Begin Calculate the distance of the ocean points from the cyclone center
      lon_dom = glamt[firstxt:lastxt, firstyt:lastyt] & lat_dom = gphit[firstxt:lastxt, firstyt:lastyt]
      d3_dist_pts_TC = reform(map_npoints(lon_dom, lat_dom, d1_lon[jtc,0:i_lstc], d1_lat[jtc,0:i_lstc]), (size(lon_dom))[1], (size(lon_dom))[2], i_lstc+1) / 1000.
;  End Calculate the distance of the ocean points from the cyclone center


;  Begin Calculate related 1d variables and time series for each cyclone track position
      FOR iid = 0, i_lstc DO BEGIN


;  Begin Define spatial and temporal array indices for calculation
        IF string(mean_radius) NE 'RMW' THEN index_radius = where(d3_dist_pts_TC[*, *, iid] LE mean_radius) ELSE index_radius = where(d3_dist_pts_TC[*, *, iid] LE d1_rmw_wrf[jtc, iid])
        index_before = where(time GE d1_juld[jtc, iid]+day_before[0] AND time LT d1_juld[jtc, iid]+day_before[1])
        index_after  = where(time GE d1_juld[jtc, iid]+day_after[0]  AND time LT d1_juld[jtc, iid]+day_after[1])
        index_during = where(time GE d1_juld[jtc, iid]-1.5           AND time LT d1_juld[jtc, iid]+1.5) ; current day
;  End Define spatial and temporal array indices for calculation


;  Begin Select only the value within a given radius from the center
          tocalc = (index_radius[0] NE -1) AND (index_before[0] NE -1) AND (index_after[0] NE -1) 
	  IF tocalc THEN BEGIN    ; calcule seulement s'il y a des valeurs bef 3jp et dans les 200km
	  array_index_radius = array_indices([(size(var_nsc_wcy))[1], (size(var_nsc_wcy))[2]], index_radius, /dim)
          wrk_nsc_wcy = fltarr(n_elements(index_radius), n_elements(time)); & help, wrk_nsc_wcy
          wrk_wsc_wcy = fltarr(n_elements(index_radius), n_elements(time)); & help, wrk_wsc_wcy
          FOR iloc = 0, n_elements(index_radius)-1 DO wrk_nsc_wcy[iloc, *] = var_nsc_wcy[array_index_radius[0, iloc], array_index_radius[1, iloc], *]
          FOR iloc = 0, n_elements(index_radius)-1 DO wrk_wsc_wcy[iloc, *] = var_wsc_wcy[array_index_radius[0, iloc], array_index_radius[1, iloc], *] 
;  End Select only the value within a given radius from the center


;  Begin Calculate anomaly with respect to 1 week before
          var_1v_nsc_wcy  = m_mean(m_mean(wrk_nsc_wcy[*, index_before], dim = 1, /nan), dim = 1, /nan)
  	  d1_var_ano_mean_bef[ jtc, iid] = m_mean(m_mean(wrk_nsc_wcy[*, index_after], dim = 1, /nan), dim = 1, /nan) - var_1v_nsc_wcy
          d1_var_ano_min_bef[ jtc, iid]  = min(m_mean(wrk_nsc_wcy[*, index_after],    dim = 1, /nan), /nan) - var_1v_nsc_wcy
          d1_var_ano_max_bef[ jtc, iid]  = max(m_mean(wrk_nsc_wcy[*, index_after],    dim = 1, /nan), /nan) - var_1v_nsc_wcy
          d1_var_ano_dur_bef[ jtc, iid]  = m_mean(m_mean(wrk_nsc_wcy[*, index_during], dim = 1, /nan), dim = 1, /nan) - var_1v_nsc_wcy
;  End Calculate anomaly with respect to 1 week before


;  Begin Calculate absolute values cycle before, during and after cyclone
          d1_var_before[jtc, iid] = m_mean(m_mean(wrk_wsc_wcy[*, index_before], dim = 1, /nan), dim = 1, /nan)
          d1_var_after[jtc, iid]  = m_mean(m_mean(wrk_wsc_wcy[*, index_after],  dim = 1, /nan), dim = 1, /nan)
          d1_var_during[jtc, iid] = m_mean(m_mean(wrk_wsc_wcy[*, index_during], dim = 1, /nan), dim = 1, /nan)   
;  End Calculate absolute values cycle before, during and after cyclone
            
         
;  Begin Calculate difference between two exp with respect to 1 week before
          dt_axis_pv = time - d1_juld[jtc, iid]; & help, dt_axis_pv
	  IF n_elements(m_mean(wrk_wsc_wcy,dim=1,/nan)) LE n_elements(where(finite(m_mean(wrk_wsc_wcy,dim=1,/nan)) EQ 1)) THEN BEGIN
            var_tms_wsc_wcy = INTERPOL(m_mean(temporary(wrk_wsc_wcy), dim = 1, /nan), dt_axis_pv, dt_axis)  
            var_tms_nsc_wcy = INTERPOL(m_mean(temporary(wrk_nsc_wcy), dim = 1, /nan), dt_axis_pv, dt_axis)
            imrd = where((dt_axis) LT min(dt_axis_pv))
            IF imrd[0] NE -1 THEN BEGIN
              var_tms_wsc_wcy[imrd] = !values.f_nan
              var_tms_nsc_wcy[imrd] = !values.f_nan
            ENDIF
            dt_var_wsc_wcy[jtc, iid, *] = temporary(var_tms_wsc_wcy)
            dt_var_nsc_wcy[jtc, iid, *] = var_tms_nsc_wcy
            dt_var_ano_bef[jtc, iid, *] = temporary(var_tms_nsc_wcy) - temporary(var_1v_nsc_wcy)
	  ENDIF
;  End Calculate difference between two exp with respect to 1 week before


        ENDIF           ;/tolac
      ENDFOR            ;/iid

;  End Calculate related 1d variables and time series for each cyclone track position
    ENDIF                       ;/d0_time_gen

      print, string(jtc)+'/'+ string(n_elements(igt_tc_oc)-1)
      notcdata:
    
    ENDFOR                      ;/jtc loop
  
  
;  Begin Save outputs
    save, dt_axis, dt_nday, igt_tc_oc, dt_var_nsc_wcy, dt_var_wsc_wcy, $
          dt_var_ano_bef, d1_var_before, d1_var_after, d1_var_during, $
          d1_var_ano_max_bef, d1_var_ano_min_bef, d1_var_ano_mean_bef, d1_var_ano_dur_bef, $
          filename = fileout, /VERBOSE
;  End Save outputs


END
