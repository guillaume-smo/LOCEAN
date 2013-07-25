PRO build_database_tc_IBTRACS_WPI
; build database from WRF to store wind speed, pressure, WPi,location....
  @common


; parametres
  expname = 'IBTRACS'   & help, expname
  version = 'v03r03'    & help, version
  basin   = 'IO'
  minlon  =  30.
  maxlon  =  130.
  minlat  = -30.
  maxlat  =  30.
  datedeb =  19980101.00d
  datefin =  20100101.00d
  period  = '1998-2009'
  indir   = '/net/adonis/usr/adonis/varclim/gslod/IBTRACS/WMO/' & help, indir
  oudir   = '/net/adonis/usr/adonis/varclim/gslod/IDL/TRACKER/EXP_'+ expname +'/DATA/' & help, oudir
  file    = 'Allstorms.ibtracs_wmo.'+version+'.nc' & help, file
  nbtime  =  137 ; max number of time steps per tc
  ficd1   = 'd1_'+expname+version+'_WPI_'+basin+'_'+period+'.dat' & help, ficd1
  fictr   = 'Allstorms.ibtracs_wmo.'+version+'.nc' & help, file


; lecture data + missing value
  juld          = ncdf_lec(file, iodir=indir, var='time_wmo') + date2jul(18581117.00d) & help, juld
  lat           = ncdf_lec(file, iodir=indir, var='lat_wmo')  * 0.01 & help, lat
  lon           = ncdf_lec(file, iodir=indir, var='lon_wmo')  * 0.01 & help, lon
  wind_kt       = ncdf_lec(file, iodir=indir, var='wind_wmo') * 0.1 & help, wind_kt
  pres          = ncdf_lec(file, iodir=indir, var='pres_wmo') * 0.1 & help, pres
  ibad_time     = where(juld GE 9.9692100e+36) & help, ibad_time
  ibad_wind     = where(wind_kt LE -3276.70) & help, ibad_wind
  ibad_pres     = where(pres LE 3276.70) & help, ibad_pres
  ibad_lon      = where(lon LE -327.669) & help, ibad_lon
  ibad_lat      = where(lat LE -327.669) & help, ibad_lat
  ibad          = ibad_wind & help, ibad
  juld[ibad]    = !values.f_nan
  wind_kt[ibad] = !values.f_nan
  pres[ibad]    = !values.f_nan
  lon[ibad]     = !values.f_nan
  lat[ibad]     = !values.f_nan
  wind          = wind_kt * 0.5144
  date          = jul2date(juld, hour=hour, day=day, month=month, year=year)


; selection data
  iok1 = where(lon[0,*] GE minlon AND lon[0,*] LE maxlon AND lat[0,*] GE minlat AND lat[0,*] LE 0.)
  iok2 = where(lon[0,*] GE minlon AND lon[0,*] LE 100.   AND lat[0,*] GE 0. AND lat[0,*] LE maxlat)
  iok3 = [iok1,iok2]
  iok4 = where(date[0,*] GE datedeb AND date[0,*] LT datefin)
  iok  = intersect(iok3,iok4)
  cptok = n_elements(iok)
  help, iok1, iok2, iok3, iok4, iok & STOP


; mise en forme data + calcul rmw analytique
  d1_wind = transpose(wind[*,iok]) & help, d1_wind
  d1_juld = transpose(juld[*,iok]) & help, d1_juld
  d1_date = transpose(date[*,iok]) & help, d1_date
  caldat, d1_juld, d1_month, d1_day, d1_year, d1_hour, d1_sec
  help, d1_month, d1_day, d1_year, d1_hour, d1_sec
  d1_lon   = transpose( lon[*,iok]) & help, d1_lon
  d1_lat   = transpose( lat[*,iok]) & help, d1_lat
  d1_pres  = transpose(pres[*,iok]) & help, d1_pres
  d1_rmw_w = 46.4 * exp(-0.0155*d1_wind + 0.0169*abs(d1_lat)) & help, d1_rmw_w
  d1_rmw_h = 51.6 * exp(-0.0223*d1_wind + 0.0281*abs(d1_lat)) & help, d1_rmw_h
  d1_dir   = fltarr(n_elements(d1_pres[*,0]),n_elements(d1_pres[0,*])) + !values.f_nan & help, d1_dir
  dlat    = !pi*6374./180.


; declarations
  d1_nrj_v3_h  = dblarr(cptok, nbtime) & d1_nrj_v3_h[*, *] = !values.f_nan
  d1_nrj_v3_w  = dblarr(cptok, nbtime) & d1_nrj_v3_w[*, *] = !values.f_nan
  d1_nrj_v2_h  = dblarr(cptok, nbtime) & d1_nrj_v2_h[*, *] = !values.f_nan
  d1_nrj_v2_w  = dblarr(cptok, nbtime) & d1_nrj_v2_w[*, *] = !values.f_nan
  num_obs        = fltarr(cptok) & num_obs[*] = !values.f_nan
     
 
; loop TC
  FOR itc = 0, cptok-1 DO BEGIN

	num_obs[itc] = n_elements(where(finite(d1_pres[itc,*]) EQ 1))
        i_lstc = num_obs[itc]-1                  ; number of timestep for identified cyclone

        IF num_obs[itc] GE 2 THEN BEGIN
          
          ivd = 0
          ivf = long(i_lstc)
          WHILE finite(d1_wind[itc, ivd]) EQ 0 DO ivd = ivd + 1
          WHILE finite(d1_wind[itc, ivf]) EQ 0 DO ivf = ivf - 1

          x_6 = indgen( n_elements(d1_lon[itc, ivd:ivf])  )*6.
          x_1 = indgen((n_elements(d1_lon[itc, ivd:ivf])-1)*6.+1)
          d1_juld_1h  = interpol(d1_juld[itc, ivd:ivf], x_6, x_1)
          d1_lon_1h   = interpol(d1_lon[itc, ivd:ivf], x_6, x_1)
          d1_lat_1h   = interpol(d1_lat[itc, ivd:ivf], x_6, x_1)
          d1_wind_1h  = interpol(d1_wind[itc, ivd:ivf], x_6, x_1)
          d1_rmw_h_1h = interpol(d1_rmw_h[itc, ivd:ivf], x_6, x_1)
          d1_rmw_w_1h = interpol(d1_rmw_w[itc, ivd:ivf], x_6, x_1)

	  ineg = where(d1_wind_1h LT 1.e-4)
          IF ineg[0] NE -1 THEN d1_wind_1h[ineg] = !values.f_nan
          itid_6 = indgen( ivf-ivd   +1)
          itid_1 = indgen((ivf-ivd)*6+1)
          
          dist_2A2_1h = map_npoints(d1_lon_1h, d1_lat_1h, d1_lon_1h, d1_lat_1h) / 1000.
         
          ; loop pas de temps TC (6h) 
          FOR iid_6 = 0, ivf-ivd DO BEGIN

            trc_1 = where(x_1 NE x_6[iid_6])
            iid_1 = where(x_1 EQ x_6[iid_6])
            
            IF trc_1[0] NE -1 THEN BEGIN
              niid_1 = itid_1[trc_1]

              phi_ts     = d1_lat_1h[niid_1]
              wnd_ts     = d1_wind_1h[niid_1]
              dst_ts     = reform( (dist_2A2_1h[*, niid_1])[iid_1, *] )
	      rmw_h_ts   = d1_rmw_h_1h[niid_1]
              rmw_w_ts   = d1_rmw_w_1h[niid_1]
              XX2 = 25                                                                   ; km fixed "near-eye" exponential decay

              XX1 = 287.6 - 1.9420* wnd_ts + 7.7990*alog(rmw_h_ts) + 1.8190*abs(phi_ts)        ; caracteristic decay length of the outer exponential
              nn = 2.1340 + 0.0077* wnd_ts - 0.4522*alog(rmw_h_ts) - 0.0038*abs(phi_ts)     ; power law in the eyewall
              AA = 0.5913 + 0.0029* wnd_ts - 0.1361*alog(rmw_h_ts) - 0.0042*abs(phi_ts)   > 0 ; power law outside the eyewall
              wnd_ts_loc_h = wnd_ts * ( (1-AA) * exp(- (dst_ts-rmw_h_ts)/XX1 ) + AA * exp(- (dst_ts-rmw_h_ts)/XX2 ) )
              FOR ipt = 0, n_elements(dst_ts)-1 DO BEGIN
                IF ( dst_ts[ipt] LT rmw_h_ts[ipt] ) THEN wnd_ts_loc_h[ipt] = wnd_ts[ipt] * ( (dst_ts[ipt]/rmw_h_ts[ipt])^nn[ipt] )	
              ENDFOR
              ieq0 = where(wnd_ts LT 1.e-4)
              IF ieq0[0] NE -1 THEN wnd_ts_loc_h[ieq0] = !values.f_nan


	      XX1 = 287.6 - 1.9420* wnd_ts + 7.7990*alog(rmw_w_ts) + 1.8190*abs(phi_ts)        ; caracteristic decay length of the outer exponential
              nn = 2.1340 + 0.0077* wnd_ts - 0.4522*alog(rmw_w_ts) - 0.0038*abs(phi_ts)     ; power law in the eyewall
              AA = 0.5913 + 0.0029* wnd_ts - 0.1361*alog(rmw_w_ts) - 0.0042*abs(phi_ts)   > 0 ; power law outside the eyewall
              wnd_ts_loc_w = wnd_ts * ( (1-AA) * exp(- (dst_ts-rmw_w_ts)/XX1 ) + AA * exp(- (dst_ts-rmw_w_ts)/XX2 ) )
              FOR ipt = 0, n_elements(dst_ts)-1 DO BEGIN
                IF ( dst_ts[ipt] LT rmw_w_ts[ipt] ) THEN wnd_ts_loc_w[ipt] = wnd_ts[ipt] * ( (dst_ts[ipt]/rmw_w_ts[ipt])^nn[ipt] )	
              ENDFOR
              ieq0 = where(wnd_ts LT 1.e-4)
              IF ieq0[0] NE -1 THEN wnd_ts_loc_w[ieq0] = !values.f_nan

              C_drag_ts_loc_h = 1E-3 * (2.7/wnd_ts_loc_h + 0.142 + wnd_ts_loc_h/13.09 - 3.14807 * 1E-10 * wnd_ts_loc_h^6.)
              ig33 = where(wnd_ts_loc_h GE 33)
              ieq0 = where(wnd_ts LT 1.e-4)
              IF ig33[0] NE -1 THEN C_drag_ts_loc_h[ig33] = 1E-3 * 2.34
              IF ieq0[0] NE -1 THEN C_drag_ts_loc_h[ieq0] = !values.f_nan

	      C_drag_ts_loc_w  = 1E-3 * (2.7/wnd_ts_loc_w + 0.142 + wnd_ts_loc_w/13.09 - 3.14807 * 1E-10 * wnd_ts_loc_w^6.)
              ig33 = where(wnd_ts_loc_w GE 33)
              ieq0 = where(wnd_ts LT 1.e-4)
              IF ig33[0] NE -1 THEN C_drag_ts_loc_w[ig33] = 1E-3 * 2.34
              IF ieq0[0] NE -1 THEN C_drag_ts_loc_h[ieq0] = !values.f_nan

              d1_nrj_v3_h[itc, iid_6+ivd] = total( C_drag_ts_loc_h * wnd_ts_loc_h^3. , /nan) 
              d1_nrj_v2_h[itc, iid_6+ivd] = total( dst_ts * C_drag_ts_loc_h * wnd_ts_loc_h^3. , /nan)

	      d1_nrj_v3_w[itc, iid_6+ivd] = total( C_drag_ts_loc_w * wnd_ts_loc_w^3. , /nan)
              d1_nrj_v2_w[itc, iid_6+ivd] = total( dst_ts * C_drag_ts_loc_w * wnd_ts_loc_w^3., /nan)

            ENDIF
          ENDFOR       
         
        ENDIF ELSE BEGIN

          d1_nrj_v2_h[itc, *] = !values.f_nan ; if not enough timestep no wind power calculation        
          d1_nrj_v2_w[itc, *] = !values.f_nan ; if not enough timestep no wind power calculation
          d1_nrj_v2_h[itc, *] = !values.f_nan ; if not enough timestep no wind power calculation	
          d1_nrj_v2_w[itc, *] = !values.f_nan ; if not enough timestep no wind power calculation

        ENDELSE
        
        print, string(itc+1)+' sur '+string(cptok)

      ENDFOR

;  Begin compute final d1 output variables
      iout = where(abs(d1_lat) GT 40)
      IF iout[0] NE -1 THEN BEGIN
        d1_nrj_v3_h[iout] = !values.f_nan
        d1_nrj_v2_h[iout] = !values.f_nan
	d1_nrj_v3_w[iout] = !values.f_nan
        d1_nrj_v2_w[iout] = !values.f_nan
      ENDIF
;  End compute final d1 output variables


;  Begin save d1 variables
      print, '% SAVE: ', oudir+ficd1
      save, d1_date, d1_juld, d1_year, d1_month, d1_day, d1_hour, $
            d1_lon, d1_lat, $
            d1_wind, d1_nrj_v3_h, d1_nrj_v3_w, d1_nrj_v2_h, d1_nrj_v2_w, $
            d1_rmw_w, d1_rmw_h, d1_pres, $
            filename = oudir+ficd1, /VERBOSE
;  End save d1 variables

      
;  Begin compute final d0 output variables
;      storm_name = storm_name[iok]
;      storm_sn = storm_sn[iok]
;      num_obs = num_obs[iok]
;      d0_time_gen = d1_date[*, 0] & d0_time_gen_jul = d1_juld[*, 0]
;      d0_lon_gen = d1_lon[*, 0] & d0_lat_gen = d1_lat[*, 0]
;      caldat, d0_time_gen_jul, m, d, y, h
;      d0_time_gen_season = d0_time_gen
;      ilsSH = where(m GE 07 AND d0_lat_gen LT 0)
;      IF ilsSH[0] NE -1 THEN d0_time_gen_season[ilsSH] = d0_time_gen_season[ilsSH]+10000.
;      d0_max_wnd = max(d1_wind, dimension = 2, /nan)
;  End compute final d0 output variables

      
;  Begin save d0 variables
;      print, oudir+ficd0
;      save, iok, d0_time_gen, d0_time_gen_jul, d0_time_gen_season, $
;            d0_lon_gen, d0_lat_gen, $
;            d0_dist, d0_max_wnd, d0_speed_min, d0_speed_moy, $
;            num_obs, $ ;storm_name, storm_sn,  $
;            filename = oudir+ficd0
;  End save d0 variables

STOP
END
