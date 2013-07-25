PRO colloc_UV2D_to_tc,tcdir, path_file_wsc,basin,datebeg_sf,datebeg_ok,dateend_ok,expname,freq,path_file_tc_d0,path_file_tc_d1,fileout,model,mean_radius
 
  @common
  
; Begin define constants
  dlat    = !pi*6374./180.
  rad     = !PI/180.
  r       = 6371.
  dxy     = 25             ; resolution approximative du modele en km
  drad    = 50             ; resolution radiale pour les moyennes azimuthales en km
  dx_axis = indgen(2*mean_radius/dxy+1) * dxy - mean_radius
  dy_axis = dx_axis
; End define constants  
 

; Begin retreive mask + cyclone parameters
  IF model EQ 'WRF' THEN BEGIN
    maskfile = '/usr/home/gslod/IDL/MASK_WRF_IND4_GRIDT.nc'
    initncdf, maskfile, /fullCgrid
    landmask = read_ncdf('LANDMASK', filename = maskfile, /nostruct)
    mask = landmask & mask[where(landmask EQ 0)] = 1 & mask[where(landmask EQ 1)] = !values.f_nan
    lat = gphit[0,*] & lon = glamt[*,0]
  ENDIF
  restore, path_file_tc_d0, /VERBOSE
  restore, path_file_tc_d1, /VERBOSE
  ntc  = n_elements(d0_time_gen) & help, ntc
  nloc = n_elements(d1_lon[0, *]) & help, nloc
; End retreive mask + cyclone parameters


; Begin Initialize 2D variables
  ux_rot = fltarr(ntc, nloc, round(2*mean_radius/dxy)+1, round(2*mean_radius/dxy)+1) + !values.f_nan & help, ux_rot
  vy_rot = fltarr(ntc, nloc, round(2*mean_radius/dxy)+1, round(2*mean_radius/dxy)+1) + !values.f_nan
  ut_rot = fltarr(ntc, nloc, round(2*mean_radius/dxy)+1, round(2*mean_radius/dxy)+1) + !values.f_nan
  vr_rot = fltarr(ntc, nloc, round(2*mean_radius/dxy)+1, round(2*mean_radius/dxy)+1) + !values.f_nan
  lonout = fltarr(round(2*mean_radius/dxy)+1,round(2*mean_radius/dxy)+1)
  latout = fltarr(round(2*mean_radius/dxy)+1,round(2*mean_radius/dxy)+1)
; End Initialize 2D variables


; Begin Read full time axis for surface data
  fid = ncdf_open(path_file_wsc, /nowrite)
  tid = ncdf_dimid(fid,'time')
  juld_ini  = date2jul(double(datebeg_sf)) & help, juld_ini
  ncdf_diminq, fid, tid, tname, tsize & help, tsize
  ncdf_close, fid
  IF freq EQ '6H' AND tsize EQ 13148 THEN time_full = double(juld_ini + indgen(tsize)*0.25)
  IF freq EQ '6H' AND tsize EQ 29220 THEN time_full = double(juld_ini + indgen(tsize)*0.25)
  IF freq EQ '1D' AND tsize EQ 7305  THEN time_full = double(juld_ini + indgen(tsize)*1.)
  help, time_full
; End Read full time axis


; Begin loop on each TC
  FOR  itc = 0, ntc-1 DO  BEGIN
    print, string(itc)+'/'+ string(ntc-1)
    IF d0_time_gen[itc] GE datebeg_ok AND d0_time_end[itc] LE dateend_ok THEN BEGIN


;  Begin Define time period and region on which data are extracted
      datedeb_jul = d0_time_gen_jul[itc] & help, datedeb_jul
      datefin_jul = date2jul(d0_time_end[itc]) & help, datefin_jul 
      caldat, datedeb_jul, m, d, y, h & datedeb = (10000.*double(y)+100.*m+d+h/24.) & help, datedeb
      caldat, datefin_jul, m, d, y, h & datefin = (10000.*double(y)+100.*m+d+h/24.) & help, datefin
;  End Define time period and region on which data are extracted


;  Begin Calculate related 1d variables and time series for each cyclone track position
      nlocok  = (reverse(where(finite(d1_time_jul[itc, *]) EQ  1)))[0]
      FOR iloc = 0, nlocok DO BEGIN

;  Begin Define time period and region on which data are extracted
        it = where(time_full EQ d1_time_jul[itc,iloc]); & help, it
        ilon = where(lon EQ d1_lon[itc,iloc]); & print, ilon
        ilat = where(lat EQ d1_lat[itc,iloc]); & print, ilat
        IF ilon[0] EQ -1 OR ilat[0] EQ -1 THEN stop
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
        utmp = read_ncdf('U10', it, it, /timestep, filename = path_file_wsc, /nostruct)*mask[firstxt:lastxt,firstyt:lastyt]; & help, utmp
        vtmp = read_ncdf('V10', it, it, /timestep, filename = path_file_wsc, /nostruct)*mask[firstxt:lastxt,firstyt:lastyt]; & help, vtmp
;  End Read data without sc and store in var_nsc


;  Begin Calculate the distance of the 2D points from the cyclone center
        lon_dom = fltarr(round(2*mean_radius/dxy)+1+14, round(2*mean_radius/dxy)+1+14) + !values.f_nan
        lat_dom = fltarr(round(2*mean_radius/dxy)+1+14, round(2*mean_radius/dxy)+1+14) + !values.f_nan
        lon_dom[offset_lonmin:n_elements(lon_dom[*,0])-1-offset_lonmax,offset_latmin:n_elements(lat_dom[0,*])-1-offset_latmax] = glamt[firstxt:lastxt, firstyt:lastyt]; & help, lon_dom 
        lat_dom[offset_lonmin:n_elements(lon_dom[*,0])-1-offset_lonmax,offset_latmin:n_elements(lat_dom[0,*])-1-offset_latmax] = gphit[firstxt:lastxt, firstyt:lastyt]; & help, lat_dom

        ; calcul de langle de deplacement du cyclone
        lon0 = d1_lon[itc,iloc] & lonp = d1_lon[itc,max([iloc-1,0])] & lonn = d1_lon[itc,min([iloc+1,nlocok])]
        lat0 = d1_lat[itc,iloc] & latp = d1_lat[itc,max([iloc-1,0])] & latn = d1_lat[itc,min([iloc+1,nlocok])]
        distx = map_2points(lonp,lat0,lonn,lat0,/meters) / 1000. * sign(lonn-lonp)
        disty = map_2points(lon0,latp,lon0,latn,/meters) / 1000. * sign(latn-latp)
        angle = atan(disty,distx)

        ; creation de la grille cyclone
        FOR iy=0, n_elements(dy_axis)-1 DO BEGIN
          d=dy_axis[iy]
          lat1=lat0+d*cos(angle+!pi/2)*1/ dlat
          lon1=lon0+d*sin(angle+!pi/2)*1/(dlat*cos(!pi/180*d1_lat[itc,iloc]))
          FOR ix=0, n_elements(dx_axis)-1 DO BEGIN
            d=dx_axis[ix]
            hms=-lat1/abs(lat1)
            lat2=lat1+hms*d*cos(angle)*1/ dlat
            lon2=lon1+hms*d*sin(angle)*1/(dlat*cos(!pi/180*d1_lat[itc,iloc]))
            latout[n_elements(dx_axis)-1-ix,n_elements(dy_axis)-1-iy]=lat2
            lonout[ix,iy]=lon2
          ENDFOR
        ENDFOR

        ; projection des composantes du vent depuis le repere wrf vers le repere cyclone
        udir = -1. * utmp * sin(angle) + vtmp * cos(angle) & udir = -1. * udir
        vdir = utmp * cos(angle) + vtmp * sin(angle)

        ; interpolation des vitesses depuis la grille wrf vers la grille cyclone
        ux_rot[itc,iloc,*,*] = fromreg('bilinear', udir, lon_dom[offset_lonmin:n_elements(lon_dom[*,0])-1-offset_lonmax,offset_latmin:n_elements(lat_dom[0,*])-1-offset_latmax], lat_dom[offset_lonmin:n_elements(lon_dom[*,0])-1-offset_lonmax,offset_latmin:n_elements(lat_dom[0,*])-1-offset_latmax], lonout, latout)
        vy_rot[itc,iloc,*,*] = fromreg('bilinear', vdir, lon_dom[offset_lonmin:n_elements(lon_dom[*,0])-1-offset_lonmax,offset_latmin:n_elements(lat_dom[0,*])-1-offset_latmax], lat_dom[offset_lonmin:n_elements(lon_dom[*,0])-1-offset_lonmax,offset_latmin:n_elements(lat_dom[0,*])-1-offset_latmax], lonout, latout)

        ; on masque les valeurs extrapolees par fromreg
        ibad = where(lonout LT lon[ilonmin2] OR lonout GT lon[ilonmax2] OR latout LT lat[ilatmin2] OR latout GT lat[ilatmax2])
        IF ibad[0] NE -1 THEN BEGIN
          lonout[ibad] = !values.f_nan
          latout[ibad] = !values.f_nan
          vartmp = reform(ux_rot[itc,iloc,*,*]) & vartmp[ibad] = !values.f_nan
          ux_rot[itc,iloc,*,*] = vartmp
          vartmp = reform(vy_rot[itc,iloc,*,*]) & vartmp[ibad] = !values.f_nan
          vy_rot[itc,iloc,*,*] = vartmp
        ENDIF

        ; projection des composantes carthesiennes du vent en composantes cylindriques (tangentiel + radial)
        dist_tc = reform(map_npoints(lonout, latout, d1_lon[itc,iloc], d1_lat[itc,iloc]), (size(lonout))[1], (size(lonout))[2]) / 1000.
        x = (findgen(n_elements(lonout[*,0]))-(n_elements(lonout[*,0])-1)/2.) # replicate(1,n_elements(latout[0,*]))
        y = replicate(1,n_elements(lonout[0,*])) # (findgen(n_elements(latout[*,0]))-(n_elements(latout[*,0])-1)/2.)
        theta = atan(y,x)
        ut_rot[itc,iloc,*,*] = -1.*ux_rot[itc,iloc,*,*] * sin(theta) + vy_rot[itc,iloc,*,*] * cos(theta)
        vr_rot[itc,iloc,*,*] = ux_rot[itc,iloc,*,*] * cos(theta) + vy_rot[itc,iloc,*,*] * sin(theta)

      ENDFOR            ;/iloc
    ENDIF                       ;/d0_time_gen
    ENDFOR                      ;/itc loop


; Begin Save outputs
  save, ux_rot, vy_rot, ut_rot, vr_rot, dist_tc, filename = fileout, /VERBOSE
; End Save outputs


END
