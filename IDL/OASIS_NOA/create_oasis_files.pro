PRO create_oasis_files
COMPILE_OPT IDL2, STRICTARRSUBS


dir = '/home/gsamson/WORK/OASIS/IVAN2km/'
file_aro = dir + 'MASK_AROME_IVAN2km.nc'
file_pgd = dir + 'PGD_IVAN2km.nc'
file_nem = dir + 'MASK_NEMO_IVAN12.nc'
file_cpl = dir + 'AROMOUT_SURF_IVAN2km.nc'
date_sst = '20080214'
file_rst = '/sortref/modele/reg2/IVAN12/GLORYS2V1/RESTART/AFTSPUP_NOAPR/restart_'+date_sst+'.nc'
ext_zone = 24



;-------------------------------------------------
; grid file
;-------------------------------------------------

  fileg = dir+'grids_arom_nemo_r8'

  lon1D = double(ncdf_lec(file_aro, var = 'lon'))
  lat1D = double(ncdf_lec(file_aro, var = 'lat'))

  cixdim = n_elements(lon1D)
  ciydim = n_elements(lat1D)
  ciexdim = cixdim + ext_zone
  cieydim = ciydim + ext_zone

  alon = dblarr(ciexdim,cieydim)
  alat = dblarr(ciexdim,cieydim)

  alon[0:cixdim-1,*] = replicate_array(lon1D, cieydim)
  FOR i = cixdim, ciexdim-1 DO BEGIN
    alon[i,*] = alon[i-1,*]+(alon[i-1,*]-alon[i-2,*])
  ENDFOR

  alat[*,0:ciydim-1] = reverse(transpose(replicate_array(lat1D, ciexdim)),2)
  FOR j = ciydim, cieydim-1 DO BEGIN
    alat[*,j] = alat[*,j-1]+(alat[*,j-1]-alat[*,j-2])
  ENDFOR


  tlon = double(ncdf_lec(file_nem, var = 'glamt'))
  tlat = double(ncdf_lec(file_nem, var = 'gphit'))
  ulon = double(ncdf_lec(file_nem, var = 'glamu'))
  ulat = double(ncdf_lec(file_nem, var = 'gphiu'))
  vlon = double(ncdf_lec(file_nem, var = 'glamv'))
  vlat = double(ncdf_lec(file_nem, var = 'gphiv'))

;  write_oasis, fileg, 'atmt.lon', alon
;  write_oasis, fileg, 'atmt.lat', alat, /append
;  write_oasis, fileg, 'ocet.lon', tlon, /append
;  write_oasis, fileg, 'ocet.lat', tlat, /append
;  write_oasis, fileg, 'oceu.lon', ulon, /append
;  write_oasis, fileg, 'oceu.lat', ulat, /append
;  write_oasis, fileg, 'ocev.lon', vlon, /append
;  write_oasis, fileg, 'ocev.lat', vlat, /append

  write_ncdf, alon, alat, tlon, tlat, ulon, ulat, vlon, vlat, $
  varname = ['atmt.lon', 'atmt.lat', 'ocet.lon', 'ocet.lat', 'oceu.lon', 'oceu.lat', 'ocev.lon', 'ocev.lat'], $
  filename = fileg+'.nc'

  spawn, 'ncatted -O -h -a ,global,d,, '+fileg+'.nc'
  spawn, 'ncatted -O -h -a valid_min_max,,d,, '+fileg+'.nc'
  spawn, 'ncatted -O -h -a valid_min,,d,, '+fileg+'.nc'
  spawn, 'ncatted -O -h -a valid_max,,d,, '+fileg+'.nc'


;-------------------------------------------------
; grids size
;-------------------------------------------------

;  sz = size(alon, /dimensions)
;  jpia = sz[0]
;  jpja = sz[1]

  sz = size(tlon, /dimensions)
  jpio = sz[0]
  jpjo = sz[1]


;-------------------------------------------------
; mask file
;-------------------------------------------------
; WARNING: OASIS convention: 1  = masked (over land), 0 = not masked (over ocean)

  filem = dir+'masks_arom_nemo_i4'

;  amskt = lonarr(ciexdim,cieydim) + 1l
;  amskt[0:cixdim-1,0:ciydim-1] = long(reverse(ncdf_lec(file_aro, var = 'MASK'),2))

  frac_sea = double(reform(ncdf_lec(file_pgd, var = 'FRAC_SEA')))
  amskt    = lonarr(ciexdim,cieydim) + 1l
  tmp      = amskt[0:cixdim-1,0:ciydim-1]
  tmp[where(frac_sea EQ 0d)]= 1l
  tmp[where(frac_sea GT 0d)]= 0l
  amskt[0:cixdim-1,0:ciydim-1] = temporary(tmp)

  omskt = 1l - long( ncdf_lec(file_nem, var = 'tmask', count = [jpio, jpjo, 1, 1]) )
  omsku = 1l - long( ncdf_lec(file_nem, var = 'umask', count = [jpio, jpjo, 1, 1]) )
  omskv = 1l - long( ncdf_lec(file_nem, var = 'vmask', count = [jpio, jpjo, 1, 1]) )

;  write_oasis, filem, 'atmt.msk', amskt, /i4
;  write_oasis, filem, 'ocet.msk', omskt, /i4, /append
;  write_oasis, filem, 'oceu.msk', omsku, /i4, /append
;  write_oasis, filem, 'ocev.msk', omskv, /i4, /append

  write_ncdf, amskt, omskt, omsku, omskv, $
  varname = ['atmt.msk','ocet.msk', 'oceu.msk', 'ocev.msk'], $
  filename = filem+'.nc'

  spawn, 'ncatted -O -h -a ,global,d,, '+filem+'.nc'
  spawn, 'ncatted -O -h -a valid_min_max,,d,, '+filem+'.nc'
  spawn, 'ncatted -O -h -a valid_min,,d,, '+filem+'.nc'
  spawn, 'ncatted -O -h -a valid_max,,d,, '+filem+'.nc'


;-------------------------------------------------
; areas file
;-------------------------------------------------

  filer = dir+'areas_arom_nemo_r8'

  @cm_4mesh
  computegrid, XAXIS = alon, YAXIS = alat

;  areat = double(ncdf_lec(filea, var = 'e1t')*ncdf_lec(filea, var = 'e2t'))

  areat = dblarr(ciexdim,cieydim)
  frac_sea = double(reform(ncdf_lec(file_pgd, var = 'FRAC_SEA')))
  dx = double(ncdf_lec(file_pgd, var = 'W_E_direction'))
  dy = double(ncdf_lec(file_pgd, var = 'S_N_direction'))
  dx[1:n_elements(dx)-1] = dx[1:n_elements(dx)-1] - (shift(dx,1))[1:n_elements(dx)-1]
  dy[1:n_elements(dy)-1] = dy[1:n_elements(dy)-1] - (shift(dy,1))[1:n_elements(dy)-1]  
  dx = replicate_array(dx, n_elements(dy))
  dy = transpose(replicate_array(dy, n_elements(dx[*,0])))

  areat[0:cixdim-1,0:ciydim-1] = dx * dy * frac_sea
  oreat = double(ncdf_lec(file_nem, var = 'e1t')*ncdf_lec(file_nem, var = 'e2t'))
  oreau = double(ncdf_lec(file_nem, var = 'e1u')*ncdf_lec(file_nem, var = 'e2u'))
  oreav = double(ncdf_lec(file_nem, var = 'e1v')*ncdf_lec(file_nem, var = 'e2v'))

;  write_oasis, filer, 'atmt.srf', areat
;  write_oasis, filer, 'ocet.srf', oreat, /append
;  write_oasis, filer, 'oceu.srf', oreau, /append
;  write_oasis, filer, 'ocev.srf', oreav, /append

  write_ncdf, areat, oreat, oreau, oreav, $
  varname = ['atmt.srf','ocet.srf', 'oceu.srf', 'ocev.srf'], $
  filename = filer+'.nc'

  spawn, 'ncatted -O -h -a ,global,d,, '+filer+'.nc'
  spawn, 'ncatted -O -h -a valid_min_max,,d,, '+filer+'.nc'
  spawn, 'ncatted -O -h -a valid_min,,d,, '+filer+'.nc'
  spawn, 'ncatted -O -h -a valid_max,,d,, '+filer+'.nc'


;-------------------------------------------------
; sst
;-------------------------------------------------

  sstoce = double((reform((ncdf_lec(file_rst, var = 'tb'))))[*,*,0] + 273.15)
  sstoce[where(omskt EQ 1)] = 0.

;  write_oasis, dir+'sstocean', 'O_SSTSST', sstoce
  write_ncdf, sstoce, varname = ['O_SSTSST'], filename = dir+'sstoc_'+date_sst+'.nc'

  spawn, 'ncatted -O -h -a ,global,d,, '+dir+'sstoc_'+date_sst+'.nc'
  spawn, 'ncatted -O -h -a valid_min_max,,d,, '+dir+'sstoc_'+date_sst+'.nc'
  spawn, 'ncatted -O -h -a valid_min,,d,, '+dir+'sstoc_'+date_sst+'.nc'
  spawn, 'ncatted -O -h -a valid_max,,d,, '+dir+'sstoc_'+date_sst+'.nc'

STOP


;-------------------------------------------------
; flx
;-------------------------------------------------

;  write_oasis, dir+'flxatmos', 'COZOTAUX', dblarr(jpia, jpja)
;  write_oasis, dir+'flxatmos', 'COMETAUU', dblarr(jpia, jpja), /append
;  write_oasis, dir+'flxatmos', 'COZOTAUV', dblarr(jpia, jpja), /append
;  write_oasis, dir+'flxatmos', 'COMETAUY', dblarr(jpia, jpja), /append
;  write_oasis, dir+'flxatmos', 'CONSFTOT', dblarr(jpia, jpja), /append
;  write_oasis, dir+'flxatmos', 'COSHFTOT', dblarr(jpia, jpja), /append
;  write_oasis, dir+'flxatmos', 'COWATFLU', dblarr(jpia, jpja), /append

  tmp = dblarr(ciexdim,cieydim)

  write_ncdf, tmp, tmp, tmp, tmp, tmp, tmp, tmp, $
  varname = ['COZOTAUX', 'COMETAUU', 'COZOTAUV', 'COMETAUY', 'CONSFTOT', 'COSHFTOT', 'COWATFLU'], $
  filename =  dir+'flxat.nc'

  spawn, 'ncatted -O -h -a ,global,d,, '+dir+'flxat.nc'
  spawn, 'ncatted -O -h -a valid_min_max,,d,, '+dir+'flxat.nc'
  spawn, 'ncatted -O -h -a valid_min,,d,, '+dir+'flxat.nc'
  spawn, 'ncatted -O -h -a valid_max,,d,, '+dir+'flxat.nc'


STOP

;-------------------------------------------------
; mozaic oce -> atm
;-------------------------------------------------

  fo2a = dir+'mozaic_ind4_wrf4_i4r8'

  addr = lonarr(jpia, jpja)
  oce = where(omsk EQ 1b)
  lonoce = olon[oce]
  latoce = olat[oce]
  FOR jj = 0, jpja-1 DO BEGIN
    IF jj MOD 10 EQ 0 THEN print, jj
    FOR ji = 0, jpia-1 DO BEGIN
      tmp = neighbor(alon[ji, jj], alat[ji, jj], lonoce, latoce)
      addr[ji, jj] = oce[tmp]
    ENDFOR
  ENDFOR

  write_oasis, fo2a, 'WEIGHTS1', replicate(1., jpia, jpja)
  write_oasis, fo2a, 'ADRESSE1', round(temporary(addr))+1L, /append, /i4

;-------------------------------------------------
; mozaic atm -> oce
;-------------------------------------------------


  fa2o = dir+'mozaic_wrf4_ind4_i4r8'

  addr = lonarr(jpio, jpjo)
  oce = where(amsk EQ 1b)
  lonoce = alon[oce]
  latoce = alat[oce]
  FOR jj = 0, jpjo-1 DO BEGIN
    IF jj MOD 10 EQ 0 THEN print, jj
    FOR ji = 0, jpio-1 DO BEGIN
      tmp = neighbor(olon[ji, jj], olat[ji, jj], lonoce, latoce)
      addr[ji, jj] = oce[tmp]
    ENDFOR
  ENDFOR

  write_oasis, fa2o, 'WEIGHTS2', replicate(1., jpio, jpjo)
  write_oasis, fa2o, 'ADRESSE2', round(temporary(addr))+1L, /append, /i4

  RETURN
END
