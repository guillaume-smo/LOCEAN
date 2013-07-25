PRO build_database_tc_WRF_WPI
; build database from WRF to store wind speed, pressure, WPi,location....
  @common

  
; Begin choice of the data to work on
  expname  = 'COUPLED_SW2_KF' & help, expname
  ucrit    = 17.5            & help, ucrit
  vorcrit  = 30.e-5          & help, vorcrit
  vorelax  = 30.e-5          & help, vorelax
  tempcrit = 1.              & help, tempcrit
  period   = '1990-2009'     & help, period
  lgth_TC  = 200
  basin    = 'IO'
  datedeb  = 19900101
  datefin  = 20091231

  indir = '/net/adonis/usr/adonis/varclim/gslod/IDL/TRACKER/EXP_'+ expname +'/DATA/' & help, indir
  oudir = indir & help, oudir
  dadir = '/net/adonis/usr/adonis/varclim/gslod/EXP_'+expname+'/TRACKER/'
  spawn, 'echo "creation du repertoire '+ oudir +'"' & spawn, 'mkdir -p '+ oudir

  ficd0 = 'd0_TRACKS_WPI_treal_rvm_nico_mask_'+expname+'_'+basin+'_'+period+'.dat' & help, ficd0
  ficd1 = 'd1_TRACKS_WPI_treal_rvm_nico_mask_'+expname+'_'+basin+'_'+period+'.dat' & help, ficd1
  fictr = 'tracker_light_TREAL_RVM_NICO_MASK_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2) +'_'+ period+ '.idl' & help, fictr & STOP 
; End choice of the data to work on
 
 
; Begin Initialization mask + time axis
  maskfile = '/usr/home/gslod/IDL/MASK_WRF_IND4_GRIDT.nc'
  initncdf, maskfile
  fid = ncdf_open(dadir+'uv10_psfc_'+period+'.nc', /nowrite)
  tid = ncdf_dimid(fid,'time')
  ncdf_diminq, fid, tid, tname, tsize
  ncdf_close, fid
  juld_ini  = date2jul(19900101.00d)
  time_juld = double(juld_ini + indgen(tsize)*0.25) & help, time_juld & STOP
; End Initialization mask + time axis


; Begin Open the tracks file and extract the relevant informations
  restore, filename = indir + fictr, /VERBOSE
     
  ingv = where(finite(juldcycn) EQ 0)
  caldat, juldcycn, traj_mth, traj_day, traj_yr, traj_hr, traj_sec
  help,   juldcycn, traj_mth, traj_day, traj_yr, traj_hr, traj_sec

  traj_mth[ingv] = -1
  traj_day[ingv] = -1
  traj_yr[ingv]  = -1
  traj_hr[ingv]  = -1
  traj_sec[ingv] = -1

  startime_trueyr = reform(traj_yr[*,0])*10000+reform(traj_mth[*,0])*double(100)+reform(traj_day[*,0])+reform(traj_hr[*,0])/24.
  trajtime = traj_yr*10000.+traj_mth*double(100.)+traj_day+traj_hr/24. & help, trajtime 
  trajtime[where(trajtime LT 0)] = !values.f_nan

  startlat = reform(latcycn[*,0]) & help, startlat
  startlon = reform(loncycn[*,0]) & help, startlon & STOP
; End Open the tracks file and extract the relevant informations


; Begin Select cyclones that fullfills the time and location criteria
  IF basin EQ 'IO' THEN BEGIN
    minlon = 30 & maxlon = 130 & minlat = -30 & maxlat = 30
  ENDIF
  IF basin EQ 'SIO' THEN BEGIN
    minlon = 30 & maxlon = 130 & minlat = -30 & maxlat = 0
  ENDIF
  IF basin EQ 'NIO' THEN BEGIN
    minlon = 50 & maxlon = 100 & minlat = 0 & maxlat = 30
  ENDIF

  igt_tc = (where(startime_trueyr GE datedeb AND startime_trueyr LT datefin AND startlon GE minlon AND startlon LE maxlon AND startlat GE minlat AND startlat LE maxlat AND finite(juldcycn[*,1]) EQ 1)) & help, igt_tc & STOP
; End Select cyclones that fullfills the time and location criteria


; Begin Initialize 1 dimension variable
      dlat1 = !pi*6374./180.
      d1_max_wnd     = fltarr(n_elements(igt_tc), lgth_TC) & d1_max_wnd[*, *] = !values.f_nan
      d1_time_jul    = dblarr(n_elements(igt_tc), lgth_TC) & d1_time_jul[*, *] = !values.f_nan
      d1_time_trueyr = dblarr(n_elements(igt_tc), lgth_TC) & d1_time_trueyr[*, *] = !values.f_nan
      d1_time_yr     = intarr(n_elements(igt_tc), lgth_TC) & d1_time_yr[*, *] = !values.f_nan
      d1_time_mth    = intarr(n_elements(igt_tc), lgth_TC) & d1_time_mth[*, *] = !values.f_nan
      d1_time_day    = intarr(n_elements(igt_tc), lgth_TC) & d1_time_day[*, *] = !values.f_nan
      d1_time_hr     = intarr(n_elements(igt_tc), lgth_TC) & d1_time_hr[*, *] = !values.f_nan
      d1_lon         = fltarr(n_elements(igt_tc), lgth_TC) & d1_lon[*, *] = !values.f_nan
      d1_lat         = fltarr(n_elements(igt_tc), lgth_TC) & d1_lat[*, *] = !values.f_nan
      d1_dist        = fltarr(n_elements(igt_tc), lgth_TC) & d1_dist[*, *] = !values.f_nan & d1_dist[*, 0] = 0
      d1_speed       = fltarr(n_elements(igt_tc), lgth_TC) & d1_speed[*, *] = !values.f_nan
      d1_speed_dir   = fltarr(n_elements(igt_tc), lgth_TC) & d1_speed_dir[*, *] = !values.f_nan & cor_dir = [0, 180, -180, 0]
      d1_pres        = fltarr(n_elements(igt_tc), lgth_TC) & d1_pres[*, *] = !values.f_nan
      d1_RMW_WRF     = fltarr(n_elements(igt_tc), lgth_TC) & d1_RMW_WRF[*, *] = !values.f_nan
      d1_RMW_ANA     = fltarr(n_elements(igt_tc), lgth_TC) & d1_RMW_ANA[*, *] = !values.f_nan
      d1_nrj_v3_WRF  = dblarr(n_elements(igt_tc), lgth_TC) & d1_nrj_v3_WRF[*, *] = !values.f_nan
      d1_nrj_v3_ANA  = dblarr(n_elements(igt_tc), lgth_TC) & d1_nrj_v3_ANA[*, *] = !values.f_nan
      d1_nrj_v3_WND  = dblarr(n_elements(igt_tc), lgth_TC) & d1_nrj_v3_WND[*, *] = !values.f_nan
      d1_nrj_v2_WRF  = dblarr(n_elements(igt_tc), lgth_TC) & d1_nrj_v2_WRF[*, *] = !values.f_nan
      d1_nrj_v2_ANA  = dblarr(n_elements(igt_tc), lgth_TC) & d1_nrj_v2_ANA[*, *] = !values.f_nan
      d1_nrj_v2_WND  = dblarr(n_elements(igt_tc), lgth_TC) & d1_nrj_v3_WND[*, *] = !values.f_nan
      d0_time_start  = dblarr(n_elements(igt_tc)) & d0_time_start[*] = !values.f_nan      
      d0_time_end    = dblarr(n_elements(igt_tc)) & d0_time_end[*] = !values.f_nan
      num_obs        = fltarr(n_elements(igt_tc)) & num_obs[*] = !values.f_nan
      uv10loc        = fltarr(n_elements(igt_tc), lgth_TC, lgth_TC) & uv10loc[*,*,*] = !values.f_nan
      uv10loc_1h     = fltarr(lgth_TC, lgth_TC*6) & uv10loc_1h[*,*] = !values.f_nan
; End Initialize 1 dimension variable      
      
      
; Begin loop on each TC
      FOR  jtc = 0, n_elements(igt_tc)-1 DO BEGIN

        idtc          = igt_tc[jtc]                      ; cyclone identifier
	num_obs[idtc] = n_elements(where(finite(loncycn[idtc,*]) EQ 1))
        i_lstc = num_obs[idtc]-1                  ; number of timestep for identified cyclone
        d1_time_jul[jtc, *] = juldcycn[idtc,*]    ; time period for identified cyclone 
        caldat, d1_time_jul[jtc, 0:i_lstc], m, d, y, h
        d1_time_yr[jtc,  0:i_lstc] = y  ; year for identified cyclone 
        d1_time_mth[jtc, 0:i_lstc] = m  ; month for identified cyclone 
        d1_time_day[jtc, 0:i_lstc] = d  ; day for identified cyclone 
        d1_time_hr[jtc,  0:i_lstc] = h  ; hour for identified cyclone 
        d1_time_trueyr[jtc, 0:i_lstc] = d1_time_yr[jtc, 0:i_lstc]*10000.+d1_time_mth[jtc, 0:i_lstc]*$
                                        double(100)+d1_time_day[jtc, 0:i_lstc]+d1_time_hr[jtc, 0:i_lstc]/24. ; time in format YYYYMMDDHH
        d0_time_start[jtc] = d1_time_trueyr[jtc, 0]                                                          ; first timestep in format YYYYMMDDHH
        d0_time_end[jtc]   = d1_time_trueyr[jtc, i_lstc]                                                     ; last timestep in format YYYYMMDDHH
        d1_lon[jtc, *]     = loncycn[idtc,*]                                                                 ; longitude for identified cyclone 
        d1_lat[jtc, *]     = latcycn[idtc,*]                                                                 ; latitude for identified cyclone
        d1_max_wnd[jtc, *] = uv10cycn[idtc,*]       							     ; Maximum wind (m/s) over 1-min for identified cyclone 
        d1_pres[jtc, *]    = mslpcycn[idtc,*]  							             ; Minimum pressure for identified cyclone
	d1_RMW_WRF[jtc, *] = rvmcycn[idtc,*]								     ; RMW from TRACKER
        d1_RMW_ANA[jtc, *] = 46.4 * exp(-0.0155*d1_max_wnd[jtc, *] + 0.0169*abs(d1_lat[jtc, *]))             ; RMW from WILLOUGHBY


;  Begin Calculate cyclone displacement for each timestep
        FOR iid = 1, i_lstc DO BEGIN ; calcule distance parcourue sur le tronÃ§on prÃ©cÃ©dent
          dely = ( d1_lat[jtc, iid] - d1_lat[jtc, iid-1] ) *dlat1
          delx = ( d1_lon[jtc, iid] - d1_lon[jtc, iid-1] ) *dlat1 * cos(!pi/180*(d1_lat[jtc, iid]+d1_lat[jtc, iid-1])/2.)
          d1_dist[jtc, iid] = sqrt( (delx)^2. + (dely)^2.)
        ENDFOR
;  End Calculate cyclone displacement for each timestep
        
;  Begin Calculate cyclone direction for each timestep
        FOR iid = 0, i_lstc DO BEGIN 
          npp = 1
          npm = 1
          IF iid EQ 0 THEN npm = 0
          IF iid EQ i_lstc THEN npp = 0
          dely = ( d1_lat[jtc, iid+npp] - d1_lat[jtc, iid-npm] )
          delx = ( d1_lon[jtc, iid+npp] - d1_lon[jtc, iid-npm] ) * cos(!pi/180*(d1_lat[jtc, iid+npp]+d1_lat[jtc, iid-npm])/2.)
          IF delx GE 0 AND dely GT 0 THEN d1_speed_dir[jtc, iid] =      abs(180/!PI*ATAN(delx / dely)) ;icas = 0
          IF delx LT 0 AND dely GT 0 THEN d1_speed_dir[jtc, iid] =  360-abs(180/!PI*ATAN(delx / dely)) ;icas = 3
          IF delx GT 0 AND dely LE 0 THEN d1_speed_dir[jtc, iid] =  90 +abs(180/!PI*ATAN(dely / delx)) ;icas = 1
          IF delx LT 0 AND dely LE 0 THEN d1_speed_dir[jtc, iid] =  270-abs(180/!PI*ATAN(dely / delx)) ;icas = 2
          IF delx EQ 0 AND dely LE 0 THEN d1_speed_dir[jtc, iid] =  180
        ENDFOR 
;  End Calculate cyclone direction for each timestep
        
;  Begin Calculate cyclone speed
        FOR iid = 1, i_lstc-1 DO d1_speed[jtc, iid] = ( d1_dist[jtc, iid] + d1_dist[jtc, iid+1] )*1/12.
        d1_speed[jtc, 0] = ( d1_dist[jtc, 1] )*1/6. & d1_speed[jtc, i_lstc] = ( d1_dist[jtc, i_lstc] )*1/6.
;  End Calculate cyclone speed       

;  Begin Test if there is enough timestep to perform calculation
        IF total(finite(d1_max_wnd[jTC, *])) GE 2 THEN BEGIN
          
;  Begin Find the beginning and ending timesteps of the cyclone considered
          ivd = 0
          ivf = long(i_lstc)
          WHILE finite(d1_max_wnd[jTC, ivd]) EQ 0 DO ivd = ivd + 1
          WHILE finite(d1_max_wnd[jTC, ivf]) EQ 0 DO ivf = ivf - 1
;  End Find the beginning and ending timesteps of the cyclone considered

;  Extraction du vent en chaque point de la trajectoire sur toute la duree de vie du cyclone
        fid = ncdf_open(dadir+'uv10_psfc_'+period+'.nc', /nowrite)
        FOR iid = ivd, ivf DO BEGIN 
          iloc = where(glamt[*,0] EQ d1_lon[jtc, iid])
          jloc = where(gphit[0,*] EQ d1_lat[jtc, iid])
	  FOR lid = ivd, ivf DO BEGIN
          lloc = where(time_juld EQ d1_time_jul[jtc, lid])
          ncdf_varget, fid, 'U10', u10, count=[1,1,1], offset=[iloc,jloc,lloc]
          ncdf_varget, fid, 'V10', v10, count=[1,1,1], offset=[iloc,jloc,lloc]
	  uv10loc[jtc,iid,lid] = reform((u10^2+v10^2)^0.5)
        ENDFOR
	ENDFOR
        ncdf_close,  fid	
;  End

;  Begin Interpolation from 6h to hourly timestep
          x_6 = indgen( n_elements(d1_lon[jTC, ivd:ivf])  )*6.
          x_1 = indgen((n_elements(d1_lon[jTC, ivd:ivf])-1)*6.+1)
          d1_time_jul_1h  = interpol(d1_time_jul[jTC, ivd:ivf], x_6, x_1)
          d1_lon_1h       = interpol(d1_lon[jTC, ivd:ivf], x_6, x_1)
          d1_lat_1h       = interpol(d1_lat[jTC, ivd:ivf], x_6, x_1)
          d1_max_wnd_1h = interpol(d1_max_wnd[jTC, ivd:ivf], x_6, x_1)
          d1_rmw_wrf_1h = interpol(d1_rmw_wrf[jTC, ivd:ivf], x_6, x_1)

          FOR iid = ivd, ivf DO BEGIN 
            uv10loc_1h[iid,ivd*6:ivf*6] = interpol(uv10loc[jtc,iid,ivd:ivf], x_6, x_1)
          ENDFOR

	  ineg = where(d1_max_wnd_1h LT 1.e-4)
          IF ineg[0] NE -1 THEN d1_max_wnd_1h[ineg] = 0.
          itid_6 = indgen( ivf-ivd   +1)
          itid_1 = indgen((ivf-ivd)*6+1)
;  End Interpolation from 6h to hourly timestep
          
;  Begin Calculate distance between points 2 by 2
          dist_2A2_1h = map_npoints(d1_lon_1h, d1_lat_1h, d1_lon_1h, d1_lat_1h) / 1000.
;  End Calculate distance between points 2 by 2
          
;  Begin Calculate related 1d atmospheric variables for each cyclone track position
          FOR iid_6 = 0, ivf-ivd DO BEGIN

            trc_1 = where(x_1 NE x_6[iid_6])
            iid_1 = where(x_1 EQ x_6[iid_6])
            
            IF trc_1[0] NE -1 THEN BEGIN
              niid_1 = itid_1[trc_1]
;  Begin Calculate cyclone wind spatial structure
              phi_ts     = d1_lat_1h[niid_1]
              wnd_ts     = d1_max_wnd_1h[niid_1]
              dst_ts     = reform( (dist_2A2_1h[*, niid_1])[iid_1, *] )
	      RMW_WRF_ts = d1_rmw_wrf_1h[niid_1]
              RMW_ANA_ts = 46.4 * exp(-0.0155*wnd_ts + 0.0169*abs(phi_ts))        
              XX2 = 25                                                                   ; km fixed "near-eye" exponential decay

              XX1 = 287.6 - 1.9420* wnd_ts + 7.7990*alog(RMW_WRF_ts) + 1.8190*abs(phi_ts)        ; caracteristic decay length of the outer exponential
              nn = 2.1340 + 0.0077* wnd_ts - 0.4522*alog(RMW_WRF_ts) - 0.0038*abs(phi_ts)     ; power law in the eyewall
              AA = 0.5913 + 0.0029* wnd_ts - 0.1361*alog(RMW_WRF_ts) - 0.0042*abs(phi_ts)   > 0 ; power law outside the eyewall
              wnd_ts_loc_WRF = wnd_ts * ( (1-AA) * exp(- (dst_ts-RMW_WRF_ts)/XX1 ) + AA * exp(- (dst_ts-RMW_WRF_ts)/XX2 ) )
	      
              FOR ipt = 0, n_elements(dst_ts)-1 DO BEGIN
                IF ( dst_ts[ipt] LT RMW_WRF_ts[ipt] ) THEN wnd_ts_loc_WRF[ipt] = wnd_ts[ipt] * ( (dst_ts[ipt]/RMW_WRF_ts[ipt])^nn[ipt] )	
              ENDFOR

	      XX1 = 287.6 - 1.9420* wnd_ts + 7.7990*alog(RMW_ANA_ts) + 1.8190*abs(phi_ts)        ; caracteristic decay length of the outer exponential
              nn = 2.1340 + 0.0077* wnd_ts - 0.4522*alog(RMW_ANA_ts) - 0.0038*abs(phi_ts)     ; power law in the eyewall
              AA = 0.5913 + 0.0029* wnd_ts - 0.1361*alog(RMW_ANA_ts) - 0.0042*abs(phi_ts)   > 0 ; power law outside the eyewall
              wnd_ts_loc_ANA = wnd_ts * ( (1-AA) * exp(- (dst_ts-RMW_ANA_ts)/XX1 ) + AA * exp(- (dst_ts-RMW_ANA_ts)/XX2 ) )
              FOR ipt = 0, n_elements(dst_ts)-1 DO BEGIN
                IF ( dst_ts[ipt] LT RMW_ANA_ts[ipt] ) THEN wnd_ts_loc_ANA[ipt] = wnd_ts[ipt] * ( (dst_ts[ipt]/RMW_ANA_ts[ipt])^nn[ipt] )	
              ENDFOR
;  End Calculate cyclone wind spatial structure

;  Begin compute drag coefficient as a function of local wind
              C_drag_ts_loc_WRF = 1E-3 * (2.7/wnd_ts_loc_WRF + 0.142 + wnd_ts_loc_WRF/13.09 - 3.14807 * 1E-10 * wnd_ts_loc_WRF^6.)
              ig33 = where(wnd_ts_loc_WRF GE 33)
              IF ig33[0] NE -1 THEN C_drag_ts_loc_WRF[ig33] = 1E-3 * 2.34

	      C_drag_ts_loc_ANA  = 1E-3 * (2.7/wnd_ts_loc_ANA + 0.142 + wnd_ts_loc_ANA/13.09 - 3.14807 * 1E-10 * wnd_ts_loc_ANA^6.)
              ig33 = where(wnd_ts_loc_ANA GE 33)
              IF ig33[0] NE -1 THEN C_drag_ts_loc_ANA[ig33] = 1E-3 * 2.34

	      C_drag_ts_loc_WND  = 1E-3 * (2.7/uv10loc_1h[iid_6,niid_1] + 0.142 + uv10loc_1h[iid_6,niid_1]/13.09 - 3.14807 * 1E-10 * uv10loc_1h[iid_6,niid_1]^6.)
              ig33 = where(uv10loc_1h[iid_6,niid_1] GE 33)
              IF ig33[0] NE -1 THEN C_drag_ts_loc_WND[ig33] = 1E-3 * 2.34
              C_drag_ts_loc_WND = reform(C_drag_ts_loc_WND)
;  End compute drag coefficient as a function of local wind

              d1_nrj_v3_WRF[jtc, iid_6+ivd] = total( C_drag_ts_loc_WRF * wnd_ts_loc_WRF^3. ) 
              d1_nrj_v2_WRF[jtc, iid_6+ivd] = total( dst_ts * C_drag_ts_loc_WRF * wnd_ts_loc_WRF^3. )

	      d1_nrj_v3_ANA[jtc, iid_6+ivd] = total( C_drag_ts_loc_ANA * wnd_ts_loc_ANA^3. ) 
              d1_nrj_v2_ANA[jtc, iid_6+ivd] = total( dst_ts * C_drag_ts_loc_ANA * wnd_ts_loc_ANA^3. )

	      d1_nrj_v3_WND[jtc, iid_6+ivd] = total( C_drag_ts_loc_WND * uv10loc_1h[iid_6,niid_1]^3. ) 
              d1_nrj_v2_WND[jtc, iid_6+ivd] = total( dst_ts * C_drag_ts_loc_WND * uv10loc_1h[iid_6,niid_1]^3. )


              
            ENDIF
          ENDFOR       
;  End Calculate related 1d atmospheric variables for each cyclone track position
          
        ENDIF ELSE BEGIN
          d1_nrj_v3_WRF[jtc, *] = !values.f_nan ; if not enough timestep no wind power calculation	
          d1_nrj_v3_ANA[jtc, *] = !values.f_nan ; if not enough timestep no wind power calculation
        ENDELSE
;  Begin Test if there is enough timestep to perform calculation
        
        
        
        print, string(jtc+1)+' sur '+string(n_elements(igt_tc))
        notcdata:
        
      ENDFOR
; End loop on each TC

      
;  Begin compute final d1 output variables
      d1_dist_cum = total(d1_dist, 2, /cumulative)
      iout = where(abs(d1_lat) GT 40)
      IF iout[0] NE -1 THEN BEGIN
        d1_nrj_v3_WRF[iout] = !values.f_nan
        d1_nrj_v2_WRF[iout] = !values.f_nan
	d1_nrj_v3_ANA[iout] = !values.f_nan
        d1_nrj_v2_ANA[iout] = !values.f_nan

      ENDIF
;  End compute final d1 output variables

      
;  Begin save d1 variables
      print, oudir+ficd1
      save, d1_time_trueyr, d1_time_jul, d1_time_yr, d1_time_mth, d1_time_day, d1_time_hr, $
            d1_lon, d1_lat, $
            d1_max_wnd, d1_nrj_v3_WRF, d1_nrj_v3_ANA, d1_nrj_v3_WND, d1_nrj_v2_WRF, d1_nrj_v2_ANA, d1_nrj_v2_WND, d1_speed, d1_speed_dir, $
            d1_dist, d1_dist_cum, d1_RMW_WRF, d1_RMW_ANA, d1_pres, $
            filename = oudir+ficd1
;  End save d1 variables

      
;  Begin compute final d0 output variables
;      storm_name = storm_name[igt_TC]
;      storm_sn = storm_sn[igt_TC]
      num_obs = num_obs[igt_TC]
      d0_time_gen = d1_time_trueyr[*, 0] & d0_time_gen_jul = d1_time_jul[*, 0]
      d0_lon_gen = d1_lon[*, 0] & d0_lat_gen = d1_lat[*, 0]
      caldat, d0_time_gen_jul, m, d, y, h
      d0_time_gen_season = d0_time_gen
      ilsSH = where(m GE 07 AND d0_lat_gen LT 0)
      IF ilsSH[0] NE -1 THEN d0_time_gen_season[ilsSH] = d0_time_gen_season[ilsSH]+10000.
      d0_dist = total(d1_dist, 2, /nan)
      d0_max_wnd = max(d1_max_wnd, dimension = 2, /nan)
      d0_speed_min = min(d1_speed, /nan, dimension = 2)
      d0_speed_moy = total(d1_speed, 2, /nan)/total(finite(d1_speed), 2)
;  End compute final d0 output variables

      
;  Begin save d0 variables
      print, oudir+ficd0
      save, igt_tc, d0_time_gen, d0_time_end, d0_time_gen_jul, d0_time_gen_season, $
            d0_lon_gen, d0_lat_gen, $
            d0_dist, d0_max_wnd, d0_speed_min, d0_speed_moy, $
            num_obs, $ ;storm_name, storm_sn,  $
            filename = oudir+ficd0
;  End save d0 variables

END
