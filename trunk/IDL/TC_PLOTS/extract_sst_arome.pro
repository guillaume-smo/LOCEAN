print, 'EXTRACT SST AROME ALONG BEST-TRACK...'



wnd10m   = W10M_SEA_0
mslp     = MSLP_SEA_0
lon_mslp = lon_mslp_0
lat_mslp = lat_mslp_0
tdim     = tdim_0  


FOR l = 0, nb_date-1 DO BEGIN

  cmd = execute( 'file = file_'+strtrim(l+1,2) )
  cmd = execute( 'xdim = xdim_'+strtrim(l+1,2) )
  cmd = execute( 'ydim = ydim_'+strtrim(l+1,2) )
;  cmd = execute( 'tdim = tdim_'+strtrim(l+1,2) )  
  cmd = execute( ' res =  res_'+strtrim(l+1,2) )

  isst = n_elements(sst_list)-1 + l
  cmd = execute( 'SST'+strtrim(isst,2)+'_1DTC_0 = fltarr(tdim) & help, SST'+strtrim(isst,2)+'_1DTC_0' )

  initncdf, file & domdef, dom_tc
  glamt  = glamt[firstxt:lastxt,firstyt:lastyt]
  gphit  = gphit[firstxt:lastxt,firstyt:lastyt]
  nb_pts = ceil(2.*radius / res)
  IF nb_pts MOD 2 EQ 0 THEN nb_pts = nb_pts-1


  FOR j = 0, tdim-1 DO BEGIN

    @calcul_index_extract

    ; sauvegarde
    cmd = execute( 'tmp = SST_'+strtrim(l+1,2)+'[*,*,0]' )
    tmp[ind_bad] = !Values.F_NAN
    cmd = execute( 'SST'+strtrim(isst,2)+'_1DTC_0[j] = avg(tmp[imin:imax,jmin:jmax], /nan)' )

  ENDFOR

ENDFOR
