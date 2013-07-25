PRO oasis_input_arome_nemo
COMPILE_OPT IDL2, STRICTARRSUBS


dir   = '/home/gsamson/WORK/SURFEX/'
filea = dir + 'MASK_AROME_CIE.nc'
fileo = dir + 'mesh_mask.nc'
filez = dir + 'AREAS_AROME_CIE.nc'

;-------------------------------------------------
; grid file
;-------------------------------------------------

  fileg = dir+'grids_arom_nemo_r8'

  alon = double(ncdf_lec(filea, var = 'lon2D'))
  alat = double(ncdf_lec(filea, var = 'lat2D'))

  tlon = double(ncdf_lec(fileo, var = 'glamt'))
  tlat = double(ncdf_lec(fileo, var = 'gphit'))
  ulon = double(ncdf_lec(fileo, var = 'glamu'))
  ulat = double(ncdf_lec(fileo, var = 'gphiu'))
  vlon = double(ncdf_lec(fileo, var = 'glamv'))
  vlat = double(ncdf_lec(fileo, var = 'gphiv'))

  write_oasis, fileg, 'atmt.lon', alon
  write_oasis, fileg, 'atmt.lat', alat, /append
  write_oasis, fileg, 'ocet.lon', tlon, /append
  write_oasis, fileg, 'ocet.lat', tlat, /append
  write_oasis, fileg, 'oceu.lon', ulon, /append
  write_oasis, fileg, 'oceu.lat', ulat, /append
  write_oasis, fileg, 'ocev.lon', vlon, /append
  write_oasis, fileg, 'ocev.lat', vlat, /append

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

  sz = size(alon, /dimensions)
  jpia = sz[0]
  jpja = sz[1]

  sz = size(tlon, /dimensions)
  jpio = sz[0]
  jpjo = sz[1]


;-------------------------------------------------
; mask file
;-------------------------------------------------
; WARNING: OASIS convention: 1  = masked (over land), 0 = not masked (over ocean)

  filem = dir+'masks_arom_nemo_i4'

  amskt = long( ncdf_lec(filea, var = 'mask') )

  omskt = 1l - long( ncdf_lec(fileo, var = 'tmask', count = [jpio, jpjo, 1, 1]) )
  omsku = 1l - long( ncdf_lec(fileo, var = 'umask', count = [jpio, jpjo, 1, 1]) )
  omskv = 1l - long( ncdf_lec(fileo, var = 'vmask', count = [jpio, jpjo, 1, 1]) )

  write_oasis, filem, 'atmt.msk', amskt, /i4
  write_oasis, filem, 'ocet.msk', omskt, /i4, /append
  write_oasis, filem, 'oceu.msk', omsku, /i4, /append
  write_oasis, filem, 'ocev.msk', omskv, /i4, /append

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

;  @cm_4mesh
;  computegrid, XAXIS = alon, YAXIS = alat

;  areat = double(ncdf_lec(filea, var = 'e1t')*ncdf_lec(filea, var = 'e2t'))
  areat = double(ncdf_lec(filez, var = 'areas_fracsea'))
  oreat = double(ncdf_lec(fileo, var = 'e1t')*ncdf_lec(fileo, var = 'e2t'))
  oreau = double(ncdf_lec(fileo, var = 'e1u')*ncdf_lec(fileo, var = 'e2u'))
  oreav = double(ncdf_lec(fileo, var = 'e1v')*ncdf_lec(fileo, var = 'e2v'))

  write_oasis, filer, 'atmt.srf', areat
  write_oasis, filer, 'ocet.srf', oreat, /append
  write_oasis, filer, 'oceu.srf', oreau, /append
  write_oasis, filer, 'ocev.srf', oreav, /append

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

  filein = dir+'output.init_0000.nc'
  sstoce = double((ncdf_lec(filein, var = 'votemper'))[*,*,0] + 273.15)
  sstoce[where(omskt EQ 1)] = 0.
  help, sstoce

  write_oasis, dir+'sstocean', 'O_SSTSST', sstoce
  write_ncdf, sstoce, varname = ['O_SSTSST'], filename = dir+'sstoc.nc'

  spawn, 'ncatted -O -h -a ,global,d,, '+dir+'sstoc.nc'
  spawn, 'ncatted -O -h -a valid_min_max,,d,, '+dir+'sstoc.nc'
  spawn, 'ncatted -O -h -a valid_min,,d,, '+dir+'sstoc.nc'
  spawn, 'ncatted -O -h -a valid_max,,d,, '+dir+'sstoc.nc'


;-------------------------------------------------
; flx
;-------------------------------------------------

  write_oasis, dir+'flxatmos', 'COZOTAUX', dblarr(jpia, jpja)
  write_oasis, dir+'flxatmos', 'COMETAUU', dblarr(jpia, jpja), /append
  write_oasis, dir+'flxatmos', 'COZOTAUV', dblarr(jpia, jpja), /append
  write_oasis, dir+'flxatmos', 'COMETAUY', dblarr(jpia, jpja), /append
  write_oasis, dir+'flxatmos', 'CONSFTOT', dblarr(jpia, jpja), /append
  write_oasis, dir+'flxatmos', 'COSHFTOT', dblarr(jpia, jpja), /append
  write_oasis, dir+'flxatmos', 'COWATFLU', dblarr(jpia, jpja), /append

  tmp = dblarr(jpia, jpja)

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
