PRO colloc_data2D_to_tc, path_file_wsc, path_file_nsc, var_nm, datebeg_sf, datebeg_ok, dateend_ok, expname, freq, path_file_tc_d1, fileout, model, mean_radius
 
  @common
  
; Begin define constants
  dxy         = 25.             ; resolution en km
  dlat        = !pi*6374./180.
  dx_axis = indgen(2*mean_radius/dxy+1) * dxy - mean_radius
  dy_axis = dx_axis
; End define constants  
 

; Begin retreive mask + cyclone parameters
  IF model EQ 'WRF' THEN BEGIN
    maskfile = '/usr/home/gslod/IDL/MASK_WRF_IND4_GRIDT.nc'
    initncdf, maskfile, /fullCgrid
    landmask = read_ncdf('LANDMASK', filename = maskfile, /nostruct)
    mask = landmask & mask[where(landmask EQ 0)] = 1 & mask[where(landmask EQ 1)] = !values.f_nan
  ENDIF 
  IF model EQ 'NEMO' THEN BEGIN
    initncdf, path_file_wsc, /fullCgrid
    var = read_ncdf(var_nm, 0,0, /timestep, filename = path_file_nsc, /nostruct)
    mask = var & mask[where(finite(var) EQ 1)] = 1 & mask[where(finite(var) EQ 0)] = !values.f_nan
  ENDIF
  IF model EQ 'OBS' THEN BEGIN
    initncdf, path_file_wsc
;    var = read_ncdf(var_nm, 0,0, /timestep, filename = path_file_nsc, /nostruct)
;    mask = var & mask[where(finite(var) EQ 1)] = 1 & mask[where(finite(var) EQ 0)] = !values.f_nan
    mask = read_ncdf('mask', 0,0, /timestep, filename = path_file_wsc, /nostruct)
  ENDIF
  lat = reform(gphit[0,*]) & lon = glamt[*,0]
  print, 'DOMAIN: ', min(lon), max(lon), min(lat), max(lat)
 
;  restore, path_file_tc_d0, /VERBOSE
  restore, path_file_tc_d1, /VERBOSE
;  ntc  = n_elements(d0_time_gen) & help, ntc
  ntc  = n_elements(d1_lon[*,0]) & help, ntc
  nloc = n_elements(d1_lon[0,*]) & help, nloc
; End retreive mask + cyclone parameters


; Begin Initialize 2D variables
  dxy_var_wsc = fltarr(ntc, nloc, round(2*mean_radius/dxy)+1, round(2*mean_radius/dxy)+1) + !values.f_nan & help, dxy_var_wsc
  dxy_var_nsc = fltarr(ntc, nloc, round(2*mean_radius/dxy)+1, round(2*mean_radius/dxy)+1) + !values.f_nan & help, dxy_var_nsc
  dxy_var_wsc_rot = fltarr(ntc, nloc, round(2*mean_radius/dxy)+1, round(2*mean_radius/dxy)+1) + !values.f_nan & help, dxy_var_wsc_rot
  dxy_var_nsc_rot = fltarr(ntc, nloc, round(2*mean_radius/dxy)+1, round(2*mean_radius/dxy)+1) + !values.f_nan & help, dxy_var_nsc_rot
  lonout = fltarr(round(2*mean_radius/dxy)+1,round(2*mean_radius/dxy)+1)
  latout = fltarr(round(2*mean_radius/dxy)+1,round(2*mean_radius/dxy)+1)
; End Initialize 2D variables


; Begin Read full time axis for surface data
  fid = ncdf_open(path_file_wsc, /nowrite)
  tid = ncdf_dimid(fid,'time')
  juld_ini  = date2jul(double(datebeg_sf)) & help, juld_ini
  ncdf_diminq, fid, tid, tname, tsize & help, tsize
  ncdf_close, fid
  IF freq EQ '6H' AND tsize EQ 13148 THEN time_full = float(juld_ini + findgen(tsize)*0.25)
  IF freq EQ '6H' AND tsize EQ 29220 THEN time_full = float(juld_ini + findgen(tsize)*0.25)
  IF freq EQ '1D' AND tsize EQ 7305  THEN time_full = float(juld_ini + findgen(tsize)*1.)
  IF freq EQ '1D' AND tsize EQ 4383  THEN time_full = float(juld_ini + findgen(tsize)*1.)
  help, time_full
; End Read full time axis


; Begin loop on each TC
  FOR  itc = 0, ntc-1 DO  BEGIN

    print, string(itc)+'/'+ string(ntc-1)
    nlocok  = (reverse(where(finite(d1_juld[itc, *]) EQ 1)))[0] & help, nlocok
;    print, 'DATE BEG:',  d1_date[itc,0], ' DATE END:', d1_date[itc,nlocok]
    IF d1_date[itc,0] GE datebeg_ok AND d1_date[itc,nlocok-1] LE dateend_ok THEN BEGIN

;  Begin Define time period and region on which data are extracted
    datedeb_jul = d1_juld[itc,0] & help, datedeb_jul
    datefin_jul = d1_juld[itc,nlocok-1] & help, datefin_jul 
    caldat, datedeb_jul, m, d, y, h & datedeb = (10000.*double(y)+100.*m+d+h/24.) & help, datedeb
    caldat, datefin_jul, m, d, y, h & datefin = (10000.*double(y)+100.*m+d+h/24.) & help, datefin
;  End Define time period and region on which data are extracted


;  Begin Calculate related 1d variables and time series for each cyclone track position
      FOR iloc = 0, nlocok DO BEGIN

;  Begin Define time period and region on which data are extracted
        it = where(time_full EQ float(d1_juld[itc,iloc]), cntit) 
        print, d1_juld[itc,iloc], jul2date(d1_juld[itc,iloc]), format='(f10.2,1x,f11.2)', cntit & STOP
        IF cntit NE 0 THEN BEGIN
          ilon = where(lon EQ d1_lon[itc,iloc]); & print, ilon
          ilat = where(lat EQ d1_lat[itc,iloc]); & print, ilat
        IF ilon[0] EQ -1 OR ilat[0] EQ -1 THEN print, 'TC OUT OF DOMAIN !', d1_lon[itc,iloc], d1_lat[itc,iloc]
        IF ilon[0] EQ -1 OR ilat[0] EQ -1 THEN CONTINUE
        ilonmin1 = ilon - round(mean_radius/dxy) - 7; & print, ilonmin1
        ilonmax1 = ilon + round(mean_radius/dxy) + 7; & print, ilonmax1
        ilatmin1 = ilat - round(mean_radius/dxy) - 7; & print, ilatmin1
        ilatmax1 = ilat + round(mean_radius/dxy) + 7; & print, ilatmax1
        ilonmin2 = max([ilonmin1, 0]); & print, ilonmin2
        ilonmax2 = min([ilonmax1, n_elements(lon)-1]); & print, ilonmax2
        ilatmin2 = max([ilatmin1, 0]); & print, ilatmin2
        ilatmax2 = min([ilatmax1, n_elements(lat)-1]); & print, ilatmax2
        IF ilonmin1 NE ilonmin2 THEN offset_lonmin = ilonmin2-ilonmin1 ELSE offset_lonmin = 0
        IF ilatmin1 NE ilatmin2 THEN offset_latmin = ilatmin2-ilatmin1 ELSE offset_latmin = 0
        IF ilonmax1 NE ilonmax2 THEN offset_lonmax = ilonmax1-ilonmax2 ELSE offset_lonmax = 0
        IF ilatmax1 NE ilatmax2 THEN offset_latmax = ilatmax1-ilatmax2 ELSE offset_latmax = 0
        print, 'TC DOM:', lon[ilonmin2], lon[ilonmax2], lat[ilatmin2], lat[ilatmax2]
;        print, ilonmax2-ilonmin2+1, ilatmax2-ilatmin2+1
        domdef, lon[ilonmin2], lon[ilonmax2], lat[ilatmin2], lat[ilatmax2], /memindices
;        domdef, ilonmin, ilonmax, ilatmin, ilatmax, /xindex, /yindex
;  End Define time period and region on which data are extracted


;  Begin Read data with sc with cyclones and store in var_wsc
        wsctmp = read_ncdf(var_nm, it, it, /timestep, filename = path_file_wsc, /nostruct)*mask[firstxt:lastxt,firstyt:lastyt]
        nsctmp = read_ncdf(var_nm, it, it, /timestep, filename = path_file_nsc, /nostruct)*mask[firstxt:lastxt,firstyt:lastyt] 
        STOP
;  End Read data without sc and store in var_nsc


;  Begin Calculate the distance of the 2D points from the cyclone center
        lon_dom = fltarr(round(2*mean_radius/dxy)+1+14, round(2*mean_radius/dxy)+1+14) + !values.f_nan
        lat_dom = fltarr(round(2*mean_radius/dxy)+1+14, round(2*mean_radius/dxy)+1+14) + !values.f_nan
        lon_dom[offset_lonmin:n_elements(lon_dom[*,0])-1-offset_lonmax,offset_latmin:n_elements(lat_dom[0,*])-1-offset_latmax] = glamt[firstxt:lastxt, firstyt:lastyt]; & help, lon_dom 
        lat_dom[offset_lonmin:n_elements(lon_dom[*,0])-1-offset_lonmax,offset_latmin:n_elements(lat_dom[0,*])-1-offset_latmax] = gphit[firstxt:lastxt, firstyt:lastyt]; & help, lat_dom

;        IF string(mean_radius) NE 'RMW' THEN ibad = where(dist_tc GT mean_radius) ELSE ibad = where(dist_tc GT d1_rmw_wrf[itc, iloc])
;        ijbad = array_indices(dist_tc, ibad)
;        FOR imask = 0, n_elements(ibad)-1 DO BEGIN
;          dxy_var_wsc[itc,iloc,ijbad[0,imask], ijbad[1,imask]] = !values.f_nan
;          dxy_var_nsc[itc,iloc,ijbad[0,imask], ijbad[1,imask]] = !values.f_nan
;        ENDFOR
;  End Define spatial and temporal array indices for calculation

        ; rotation dans l'axe de deplacement du cyclone         
         lat0=d1_lat[itc,iloc]
         lon0=d1_lon[itc,iloc]
         angl=d1_speed_dir[itc,iloc] ;& IF angl gt 180  THEN angl=angl-360
         angl=angl*!pi/180-!pi/2
            
         FOR iy=0, n_elements(dx_axis)-1 DO BEGIN
           d=dx_axis[iy]
           lat1=lat0+d*cos(angl+!pi/2)*1/ dlat
           lon1=lon0+d*sin(angl+!pi/2)*1/(dlat*cos(!pi/180*d1_lat[itc,iloc]))
              
            FOR ix=0, n_elements(dx_axis)-1 DO BEGIN
              d=dx_axis[ix]
              hms=-lat1/abs(lat1)
              lat2=lat1+hms*d*cos(angl)*1/ dlat
              lon2=lon1+hms*d*sin(angl)*1/(dlat*cos(!pi/180*d1_lat[itc,iloc]))
              
              latout[ix,iy]=lat2
              lonout[ix,iy]=lon2
            ENDFOR
          ENDFOR

        dxy_var_wsc_rot[itc,iloc,*,*] = fromreg('bilinear', wsctmp, lon_dom[offset_lonmin:n_elements(lon_dom[*,0])-1-offset_lonmax,offset_latmin:n_elements(lat_dom[0,*])-1-offset_latmax], lat_dom[offset_lonmin:n_elements(lon_dom[*,0])-1-offset_lonmax,offset_latmin:n_elements(lat_dom[0,*])-1-offset_latmax], lonout, latout)
        dxy_var_nsc_rot[itc,iloc,*,*] = fromreg('bilinear', nsctmp, lon_dom[offset_lonmin:n_elements(lon_dom[*,0])-1-offset_lonmax,offset_latmin:n_elements(lat_dom[0,*])-1-offset_latmax], lat_dom[offset_lonmin:n_elements(lon_dom[*,0])-1-offset_lonmax,offset_latmin:n_elements(lat_dom[0,*])-1-offset_latmax], lonout, latout)

        ENDIF ; it
      ENDFOR ; iloc
    ENDIF ; d0_time_gen
    ENDFOR                      ;/itc loop


; Begin Save outputs
;  save, dxy_var_wsc, dxy_var_nsc, dxy_var_wsc_rot, dxy_var_nsc_rot, dist_tc, filename = fileout, /VERBOSE
; End Save outputs

STOP

END
