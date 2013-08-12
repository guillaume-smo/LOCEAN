print, '' & print, 'EXTRACT NEMO CPL ALONG MODEL TRACK...'


; definitions
cmd = execute( 'wnd10m   = W10M_SEA_'+strtrim(i,2) )
cmd = execute( 'mslp     = MSLP_SEA_'+strtrim(i,2) )
cmd = execute( 'lon_mslp = lon_mslp_'+strtrim(i,2) )
cmd = execute( 'lat_mslp = lat_mslp_'+strtrim(i,2) )
cmd = execute( 'tdim     = tdim_'+strtrim(i,2) )
cmd = execute( 'file     = filen_'+strtrim(i,2) )
cmd = execute( 'xdim     = xdimn_'+strtrim(i,2) )
cmd = execute( 'ydim     = ydimn_'+strtrim(i,2) )
cmd = execute( 'res      = resn_'+strtrim(i,2) )
cmd = execute( 'var      = '+var_name+'_'+strtrim(i,2) )


; setup
initncdf, file & domdef, dom_tc
glamt  = glamt[firstxt:lastxt,firstyt:lastyt]
gphit  = gphit[firstxt:lastxt,firstyt:lastyt]
nb_pts = ceil(2.*radius / res)
IF nb_pts MOD 2 EQ 0 THEN nb_pts = nb_pts-1
cmd = execute( var_name+'_1DTC_'+strtrim(i,2)+' = fltarr(tdim)' )


; extraction
FOR j = 0, tdim-1 DO BEGIN
  @calcul_index_extract
  ; sauvegarde
  tmp = var[*,*,j]
  tmp[ind_bad] = !Values.F_NAN
  cmd = execute( var_name+'_1DTC_'+strtrim(i,2)+'[j] = avg(tmp[imin:imax,jmin:jmax], /nan)' )
ENDFOR
cmd = execute( 'help, '+var_name+'_1DTC_'+strtrim(i,2) )


print, 'EXTRACT OK' & print, ''
