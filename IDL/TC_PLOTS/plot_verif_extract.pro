FOR i = 1, nb_exp-1 DO BEGIN
  cmd = execute( 'tdim = tdim_'+strtrim(i,2) )
  FOR j = 0, tdim-1 DO BEGIN
    cmd = execute( 'computegrid, xaxis=lon_2dtc_'+strtrim(i,2)+'[*,*,j], yaxis=lat_2dtc_'+strtrim(i,2)+'[*,*,j]' )
    cmd = execute( 'plt, w10m_sea_2dtc_'+strtrim(i,2)+'[*,*,j], title=date_'+strtrim(i,2)+'[j], win=0' )
    cmd = execute( 'oplot, [lon_mslp_'+strtrim(i,2)+'[j],lon_mslp_'+strtrim(i,2)+'[j]], [lat_mslp_'+strtrim(i,2)+'[j],lat_mslp_'+strtrim(i,2)+'[j]], psym=1, color=0, thick=4, symsize=4' )
    cmd = execute( 'plt, mslp_sea_2dtc_'+strtrim(i,2)+'[*,*,j], title=date_'+strtrim(i,2)+'[j], win=1' )
    cmd = execute( 'oplot, [lon_mslp_'+strtrim(i,2)+'[j],lon_mslp_'+strtrim(i,2)+'[j]], [lat_mslp_'+strtrim(i,2)+'[j],lat_mslp_'+strtrim(i,2)+'[j]], psym=1, color=0, thick=4, symsize=4' )
    IF j EQ tdim-1 THEN STOP
  ENDFOR
ENDFOR
STOP
