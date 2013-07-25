PRO oasisinput_arom_nemo
COMPILE_OPT IDL2, STRICTARRSUBS


dir   = '/home/gsamson/WORK/SURFEX/'
filea = dir + 'MASK_AROME.nc'
fileo = dir + 'mesh_mask.nc'


;-------------------------------------------------
; grid file
;-------------------------------------------------

  ngr = dir+'grids_arom_nemo_r8'

  alon = ncdf_lec(filea, var = 'lon2D')
  alat = ncdf_lec(filea, var = 'lat2D')

  write_oasis, ngr, 'arom.lon', alon
  write_oasis, ngr, 'arom.lat', alat, /append

  olon = ncdf_lec(fileo, var = 'glamt')
  olat = ncdf_lec(fileo, var = 'gphit')

  write_oasis, ngr, 'nemo.lon', olon, /append
  write_oasis, ngr, 'nemo.lat', olat, /append


;-------------------------------------------------
; grids size
;-------------------------------------------------

  sz = size(alon, /dimensions)
  jpia = sz[0]
  jpja = sz[1]

  sz = size(olon, /dimensions)
  jpio = sz[0]
  jpjo = sz[1]


;-------------------------------------------------
; mask file
;-------------------------------------------------
; WARNING: OASIS convention: 1 over land, 0 over Ocean

  nmsk = dir+'masks_arom_nemo_i4'

  amsk = byte( ncdf_lec(filea, var = 'mask') )
  write_oasis, nmsk, 'arom.msk', amsk, /i4

  omsk = byte( ncdf_lec(fileo, var = 'tmask', count = [jpio, jpjo, 1, 1]) )
  write_oasis, nmsk, 'nemo.msk', 1b - omsk, /i4, /append


;-------------------------------------------------
; areas file
;-------------------------------------------------

  nar = dir+'areas_arom_nemo_r8'
  @cm_4mesh

  computegrid, XAXIS = alon, YAXIS = alat
  write_oasis, nar, 'arom.srf', ncdf_lec(filea, var = 'e1t')*ncdf_lec(filea, var = 'e2t')
  write_oasis, nar, 'nemo.srf', ncdf_lec(fileo, var = 'e1t')*ncdf_lec(fileo, var = 'e2t'), /append


;-------------------------------------------------
; sst
;-------------------------------------------------

  filein = dir+'output.init_0000.nc'
  sstoce = (ncdf_lec(filein, var = 'votemper'))[*,*,0] + 273.15
  help, sstoce
  write_oasis, dir+'sstocean', 'OCE__SST', sstoce


;-------------------------------------------------
; flx
;-------------------------------------------------

  write_oasis, dir+'flxatmos', 'ATM_TAUT', fltarr(jpia, jpja)
  write_oasis, dir+'flxatmos', 'ATM_TAUX', fltarr(jpia, jpja), /append
  write_oasis, dir+'flxatmos', 'ATM_TAUY', fltarr(jpia, jpja), /append
  write_oasis, dir+'flxatmos', 'ATM_EVPR', fltarr(jpia, jpja), /append
  write_oasis, dir+'flxatmos', 'ATM__QSR', fltarr(jpia, jpja), /append
  write_oasis, dir+'flxatmos', 'ATM__QNS', fltarr(jpia, jpja), /append

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
