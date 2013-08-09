print, '' & print, 'EXTRACT SST GLORYS2V1+ALADIN ALONG BEST-TRACK...'


; best-track
wnd10m   = W10M_SEA_0
mslp     = MSLP_SEA_0
lon_mslp = lon_mslp_0
lat_mslp = lat_mslp_0
tdim     = tdim_0

; analyse aladin
file     = file_sstg3a
xdim     = xdim_sstg3a
ydim     = ydim_sstg3a
res      = res_sstg3a
var      = sstg3a_0

; setup
initncdf, file & domdef, dom_tc
glamt  = glamt[firstxt:lastxt,firstyt:lastyt]
gphit  = gphit[firstxt:lastxt,firstyt:lastyt]
nb_pts = ceil(2.*radius / res)
IF nb_pts MOD 2 EQ 0 THEN nb_pts = nb_pts-1
cmd = execute( 'SST'+strtrim(k,2)+'_1DTC_0 = fltarr(tdim)' )

FOR j = 0, tdim-1 DO BEGIN

  @calcul_index_extract

  ; sauvegarde
  tmp = var[*,*,j]
  tmp[ind_bad] = !Values.F_NAN
  cmd = execute( 'SST'+strtrim(k,2)+'_1DTC_0[j] = avg(tmp[imin:imax,jmin:jmax], /nan)' )

ENDFOR


print, 'OK' & print, ''
