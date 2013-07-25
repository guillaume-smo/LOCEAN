PRO colloc_TRMM3B42_2D_to_tc, path_file_wsc, var_nm, basin, datebeg_ok, dateend_ok, $
freq, path_file_tc_d1, fileout, mean_radius
 
  @common
  
; Begin define constants
  dxy         = 25.             ; resolution en km
  dlat        = !pi*6374./180.
  dx_axis = indgen(2*mean_radius/dxy+1) * dxy - mean_radius
  dy_axis = dx_axis
; End define constants  
 

; Begin retreive mask + cyclone parameters
  maskfile = '/usr/home/gslod/IDL/MASK_WRF_IND4_GRIDT.nc' & help, maskfile
  initncdf, maskfile
  landmask = read_ncdf('LANDMASK', filename = maskfile, /nostruct)
  mask = landmask & mask[where(landmask EQ 0)] = 1 & mask[where(landmask EQ 1)] = !values.f_nan
  glamt_old = glamt
  gphit_old = gphit
  initncdf, path_file_wsc
  mask = fromreg('bilinear',mask,glamt_old,gphit_old,glamt,gphit) & help, mask

  restore, path_file_tc_d1, /VERBOSE
  ntc  = n_elements(d1_lon_3h[*,0])  & help, ntc
  nloc = n_elements(d1_lon_3h[0,*]) & help, nloc
; End retreive mask + cyclone parameters



; Begin Read full time axis for surface data
  nbhours = ncdf_lec(path_file_wsc, var='time')
  lon     = ncdf_lec(path_file_wsc, var='x')
  lat     = ncdf_lec(path_file_wsc, var='y')
  juld    = date2jul(19980101.00d) + dindgen(n_elements(nbhours)) * 0.125d
  date    = jul2date(juld)
  help, juld
; End Read full time axis



; Begin Initialize 2D variables
  dxy_var_wsc_rot = fltarr(ntc, nloc, round(2*mean_radius/dxy)+1, round(2*mean_radius/dxy)+1) + !values.f_nan & help, dxy_var_wsc_rot
  lonout = fltarr(round(2*mean_radius/dxy)+1,round(2*mean_radius/dxy)+1)
  latout = fltarr(round(2*mean_radius/dxy)+1,round(2*mean_radius/dxy)+1)
; End Initialize 2D variables



; Begin loop on each TC
  FOR itc = 0, ntc-1 DO  BEGIN
    print, string(itc)+'/'+ string(ntc-1)
    iok  = where(finite(d1_juld_3h[itc,*]) EQ 1, cntok)

;  Begin Calculate related 1d variables and time series for each cyclone track position
      it = 0
      FOR iloc = 0, cntok-1 DO BEGIN

;  Begin Define time period and region on which data are extracted
        print, d1_juld_3h[itc,iloc], juld[where(abs(juld-d1_juld_3h[itc,iloc]) LT 0.05)], format='(F12.4,1X,F12.4)'
        it = reform(where(abs(juld-d1_juld_3h[itc,iloc]) LT 0.001))
        IF it EQ -1 THEN STOP
        IF it NE -1 THEN BEGIN
        ilon = (where(abs(lon-d1_lon_3h[itc,iloc]) LE 0.125))[0];  & help, ilon
        ilat = (where(abs(lat-d1_lat_3h[itc,iloc]) LE 0.125))[0];  & help, ilat
        IF ilon[0] EQ -1 OR ilat[0] EQ -1 THEN STOP
        IF mask[ilon,ilat] EQ !values.f_nan THEN STOP
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
;        print, lon[ilonmin], lon[ilonmax], lat[ilatmin], lat[ilatmax]
;        print, ilonmax2-ilonmin2+1, ilatmax2-ilatmin2+1
        domdef, lon[ilonmin2], lon[ilonmax2], lat[ilatmin2], lat[ilatmax2], /memindices
;        domdef, ilonmin, ilonmax, ilatmin, ilatmax, /xindex, /yindex
;  End Define time period and region on which data are extracted


;  Begin Read data with sc with cyclones and store in var_wsc
        wsctmp = read_ncdf(var_nm, it, it, /timestep, filename = path_file_wsc, /nostruct)*mask[firstxt:lastxt,firstyt:lastyt]
;  End Read data without sc and store in var_nsc


;  Begin Calculate the distance of the 2D points from the cyclone center
        lon_dom = fltarr(round(2*mean_radius/dxy)+1+14, round(2*mean_radius/dxy)+1+14) + !values.f_nan
        lat_dom = fltarr(round(2*mean_radius/dxy)+1+14, round(2*mean_radius/dxy)+1+14) + !values.f_nan
        lon_dom[offset_lonmin:n_elements(lon_dom[*,0])-1-offset_lonmax,offset_latmin:n_elements(lat_dom[0,*])-1-offset_latmax] = glamt[firstxt:lastxt, firstyt:lastyt]; & help, lon_dom 
        lat_dom[offset_lonmin:n_elements(lon_dom[*,0])-1-offset_lonmax,offset_latmin:n_elements(lat_dom[0,*])-1-offset_latmax] = gphit[firstxt:lastxt, firstyt:lastyt]; & help, lat_dom

;  End Define spatial and temporal array indices for calculation

        ; rotation dans l'axe de deplacement du cyclone         
         lat0=d1_lat_3h[itc,iloc]
         lon0=d1_lon_3h[itc,iloc]
         angle=d1_dir_3h[itc,iloc]
            
        ; creation de la grille cyclone
        FOR iy=0, n_elements(dy_axis)-1 DO BEGIN
          d=dy_axis[iy]
          lat1=lat0+d*cos(angle+!pi/2)*1/ dlat
          lon1=lon0+d*sin(angle+!pi/2)*1/(dlat*cos(!pi/180*d1_lat_3h[itc,iloc]))
          FOR ix=0, n_elements(dx_axis)-1 DO BEGIN
            d=dx_axis[ix]
            hms=-lat1/abs(lat1)
            lat2=lat1+hms*d*cos(angle)*1/ dlat
            lon2=lon1+hms*d*sin(angle)*1/(dlat*cos(!pi/180*d1_lat_3h[itc,iloc]))
            latout[n_elements(dx_axis)-1-ix,n_elements(dy_axis)-1-iy]=lat2
            lonout[ix,iy]=lon2
          ENDFOR
        ENDFOR

        IF n_elements(where(finite(wsctmp))) GE floor(n_elements(wsctmp)*1./2.) THEN $
        dxy_var_wsc_rot[itc,iloc,*,*] = fromreg('bilinear', temporary(wsctmp), lon_dom[offset_lonmin:n_elements(lon_dom[*,0])-1-offset_lonmax,offset_latmin:n_elements(lat_dom[0,*])-1-offset_latmax], lat_dom[offset_lonmin:n_elements(lon_dom[*,0])-1-offset_lonmax,offset_latmin:n_elements(lat_dom[0,*])-1-offset_latmax], lonout, latout)

        ENDIF ; it
      ENDFOR ; iloc
    ENDFOR                      ;/itc loop


; Begin Save outputs
  save, dxy_var_wsc_rot, filename = fileout, /VERBOSE
; End Save outputs


END
