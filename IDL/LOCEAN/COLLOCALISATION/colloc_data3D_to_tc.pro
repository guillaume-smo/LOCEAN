PRO colloc_data3D_to_tc,tcdir,var_nm,basin,datebeg_sf,datebeg_ok,dateend_ok,expname,freq,path_file_tc_d0,path_file_tc_d1,fileout,model,mean_radius,indir
 
  @common
  
; Begin define constants
  dxy         = 25             ; resolution approximative du modele en km
  drad        = 50             ; resolution radiale pour les moyennes azimuthales
  nb_levz     = 27             ; nombre de niveau verticaux unstag
; End define constants  
 

; Begin retreive mask + cyclone parameters
  IF model EQ 'WRF' THEN BEGIN
    maskfile = '/usr/home/gslod/IDL/MASK_WRF_GRID_T.nc'
    initncdf, maskfile, /fullCgrid
    landmask = read_ncdf('LANDMASK', filename = maskfile, /nostruct)
    mask = landmask & mask[where(landmask EQ 0)] = 1 & mask[where(landmask EQ 1)] = !values.f_nan
    lat = gphit[0,*] & lon = glamt[*,0]
    tmask = reform(reform(mask, n_elements(lon)*n_elements(lat))#replicate(1,nb_levz), n_elements(lon), n_elements(lat), nb_levz)
    nb_levz     = 27             ; nombre de niveau verticaux unstag
    jpk = nb_levz
    gdept = findgen(nb_levz)
    gdepw = findgen(nb_levz)
    e3t = findgen(nb_levz)
  ENDIF ELSE BEGIN
    initncdf, path_file_wsc, /fullCgrid
    var = read_ncdf(var_nm, 0,0, /timestep, filename = path_file_nsc, /nostruct)
    mask = var & mask[where(finite(var) EQ 1)] = 1 & mask[where(finite(var) EQ 0)] = !values.f_nan
  ENDELSE
  restore, path_file_tc_d0, /VERBOSE
  restore, path_file_tc_d1, /VERBOSE
  ntc  = n_elements(d0_time_gen)  & help, ntc
  nloc = n_elements(d1_lon[0, *]) & help, nloc
; End retreive mask + cyclone parameters


; Begin Initialize 3D variables
  var_3D = fltarr(round(2*mean_radius/dxy)+1, round(2*mean_radius/dxy)+1, nb_levz) + !values.f_nan & help, var_3D
  var_2D = fltarr(ntc, nloc, round(mean_radius/drad), nb_levz) + !values.f_nan & help, var_2D
; End Initialize 2D variables



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
        IF D1_TIME_MTH[itc,iloc] LT 10 THEN month = '0'+strtrim(D1_TIME_MTH[itc,iloc],2) ELSE month = strtrim(D1_TIME_MTH[itc,iloc],2) & help, month
        IF D1_TIME_DAY[itc,iloc] LT 10 THEN day   = '0'+strtrim(D1_TIME_DAY[itc,iloc],2) ELSE day   = strtrim(D1_TIME_DAY[itc,iloc],2) & help, day
        path_file_wsc = indir+'wrfout_d01_'+strtrim(D1_TIME_YR[itc,iloc],2)+'-'+month+'-'+day+'_00:00:00' & help, path_file_wsc

; Begin Read full time axis for surface data
        fid = ncdf_open(path_file_wsc, /nowrite)
        tid = ncdf_dimid(fid,'Time')
        juld_ini  = date2jul(double(datebeg_sf)) & help, juld_ini
        ncdf_diminq, fid, tid, tname, tsize & help, tsize
        ncdf_close, fid
        IF freq EQ '6H' THEN time_full = double(juld_ini + indgen(tsize)*0.25)
        IF freq EQ '1D' THEN time_full = double(juld_ini + indgen(tsize)*1.)
; End Read full time axis

        it = where(time_full EQ d1_time_jul[itc,iloc]); & help, it
        ilon = where(lon EQ d1_lon[itc,iloc]); & print, ilon
        ilat = where(lat EQ d1_lat[itc,iloc]); & print, ilat
        IF ilon[0] EQ -1 OR ilat[0] EQ -1 THEN stop
        ilonmin = ilon - round(mean_radius/dxy); & print, ilonmin
        ilonmax = ilon + round(mean_radius/dxy); & print, ilonmax
        ilatmin = ilat - round(mean_radius/dxy); & print, ilatmin
        ilatmax = ilat + round(mean_radius/dxy); & print, ilatmax
        domdef, ilonmin, ilonmax, ilatmin, ilatmax, 0, nb_levz-1, /xindex, /yindex, /zindex
;  End Define time period and region on which data are extracted


;  Begin Read data with sc with cyclones and store in var_wsc
        var_3D = read_ncdf(var_nm, it, it, /timestep, filename = path_file_wsc, /nostruct, gridtype='T')*tmask[firstxt:lastxt,firstyt:lastyt,0:nb_levz-1]
;  End Read data without sc and store in var_nsc


;  Begin Calculate the distance of the 2D points from the cyclone center
        lon_dom = glamt[firstxt:lastxt, firstyt:lastyt] 
        lat_dom = gphit[firstxt:lastxt, firstyt:lastyt]
        dist_tc = reform(map_npoints(lon_dom, lat_dom, d1_lon[itc,iloc], d1_lat[itc,iloc]), (size(lon_dom))[1], (size(lon_dom))[2]) / 1000.
;        IF string(mean_radius) NE 'RMW' THEN ibad = where(dist_tc GT mean_radius) ELSE ibad = where(dist_tc GT d1_rmw_wrf[itc, iloc])
;        ijbad = array_indices(dist_tc, ibad)
;        FOR imask = 0, n_elements(ibad)-1 DO BEGIN
;          var_3D_wsc[itc,iloc,ijbad[0,imask], ijbad[1,imask]] = !values.f_nan
;          var_3D_nsc[itc,iloc,ijbad[0,imask], ijbad[1,imask]] = !values.f_nan
;        ENDFOR
;  End Define spatial and temporal array indices for calculation


   drad = 50.
   FOR j = 0, nb_levz-1 DO BEGIN
   FOR i = 0, round(mean_radius/drad)-1 DO BEGIN
     print, i, i*drad, (i+1)*drad
     iradok = where(dist_tc GE i*drad AND dist_tc LT (i+1)*drad) & help, iradok
     vartmp = var_3D[*,*,j]
     IF iradok[0] NE -1 THEN var_2D[itc,iloc,i,j] = mean(vartmp[iradok],/nan)
   ENDFOR
   ENDFOR


      ENDFOR            ;/iloc
    ENDIF                       ;/d0_time_gen
;    computegrid,-200,-200,25,25,17,17
;    stop
    ENDFOR                      ;/itc loop


; Begin Save outputs
  save, var_2D, filename = fileout, /VERBOSE
; End Save outputs


END
