PRO oasisinput_wrf4_ind4
COMPILE_OPT IDL2, STRICTARRSUBS


dir   = '/home/gsamson/WORK/SURFEX/'
filea = dir + 'MASK_AROME.nc'
fileo = dir + 'mesh_mask.nc'


;-------------------------------------------------
; grid file
;-------------------------------------------------

  ngr = dir+'grids_arom4_ivan12_r8'

  alon = (ncdf_lec(filea, var = 'XLONG_M'))[*, 0]
  jpia = n_elements(alon)
  dx = alon[jpia-1] - alon[jpia-2]
  alon = [alon, alon[jpia-1]+dx]

  alat = ncdf_lec(filea, var = 'XLAT_M')
  alat = reform(alat[0, *])
  jpja = n_elements(alat)
  dy = alat[jpja-1] - alat[jpja-2]
  alat = [alat, alat[jpja-1]+dy]

  jpia = jpia+1
  jpja = jpja+1
  alon = alon#replicate(1., jpja)
  alat = replicate(1., jpia)#alat

  write_oasis, ngr, 'wrf4.lon', alon
  write_oasis, ngr, 'wrf4.lat', alat, /append

  olon = ncdf_lec(fileo, var = 'glamt')
  olat = ncdf_lec(fileo, var = 'gphit')

  write_oasis, ngr, 'ind4.lon', olon, /append
  write_oasis, ngr, 'ind4.lat', olat, /append


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

  nmsk = dir+'masks_arom4_ivan12_i4'

  amsk = byte( ncdf_lec(filea, var = 'MASK_T') )
  amsk = [amsk, bytarr(1, jpja-1)]
  amsk = [ [amsk], [bytarr(jpia, 1)] ]
  write_oasis, nmsk, 'wrf4.msk', 1b - amsk, /i4

  omsk = byte( ncdf_lec(fileo, var = 'tmask', count = [jpio, jpjo, 1, 1]) )

; patch
  omsk[*, 1] = 0b
  omsk[*, 245] = 0b
  omsk[jpio-2, *] = 0b

  write_oasis, nmsk, 'ind4.msk', 1b - omsk, /i4, /append

;-------------------------------------------------
; areas file
;-------------------------------------------------

  nar = dir+'areas_wrf4_ind4_r8'
  @cm_4mesh

  computegrid, XAXIS = alon, YAXIS = alat
  write_oasis, nar, 'wrf4.srf', e1t*e2t
  write_oasis, nar, 'ind4.srf', ncdf_lec(fileo, var = 'e1t')*ncdf_lec(fileo, var = 'e2t'), /append


;-------------------------------------------------
; sst
;-------------------------------------------------

  filein = '/Users/smasson/tmp/sstat.nc'
  sst = ncdf_lec(filein, var = 'SST')
  sst = [sst, fltarr(1, jpja-1)]
  sst = [ [sst], [fltarr(jpia, 1)] ]
  add = read_oasis(dir+'mozaic_wrf4_ind4_i4r8', 'ADRESSE2', jpio, jpjo, /i4)-1L
  sstoce = sst[add]
  write_oasis, dir+'sstocean', 'O_SSTSST', sstoce

;; add = read_oasis(dir+'mozaic_ind4_wrf4_i4r8', 'ADRESSE1', jpia, jpja, /i4)-1L
;; sstatm = sstoce[add]


;-------------------------------------------------
; flx
;-------------------------------------------------

  write_oasis, dir+'flxatmos', 'AWRFTAUT', fltarr(jpia, jpja)
  write_oasis, dir+'flxatmos', 'AWRFTAUX', fltarr(jpia, jpja), /append
  write_oasis, dir+'flxatmos', 'AWRFTAUY', fltarr(jpia, jpja), /append
  write_oasis, dir+'flxatmos', 'AWRFEVPR', fltarr(jpia, jpja), /append
  write_oasis, dir+'flxatmos', 'A_WRFQSR', fltarr(jpia, jpja), /append
  write_oasis, dir+'flxatmos', 'A_WRFQNS', fltarr(jpia, jpja), /append


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

  wght = replicate(1., jpia, jpja)
  wght[jpia-1, *] = 0.
  wght[*, jpja-1] = 0.

  write_oasis, fo2a, 'WEIGHTS1', wght
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
