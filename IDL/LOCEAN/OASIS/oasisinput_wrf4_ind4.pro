PRO oasisinput_wrf4_ind4
;
  compile_opt idl2, strictarrsubs
;
  dir = '/usr/home/gslod/IDL/'
  filea = dir + 'MASK_WRF_IND4_GRIDT.nc'
  fileo = dir + 'MASK_NEMO_IND4_GRIDT.nc
;
;-------------------------------------------------
; grid file
;-------------------------------------------------
;
  ngr = dir+'grids_wrf4_ind4_r8'
;
  alon = (ncdf_lec(filea, var = 'longitude'))[*, 0]
  jpia = n_elements(alon)
  dx = alon[jpia-1] - alon[jpia-2]
  alon = [alon, alon[jpia-1]+dx] & help, alon

  alat = ncdf_lec(filea, var = 'latitude')
;  alat = reform(alat[0, *])
  jpja = n_elements(alat)
  dy = alat[jpja-1] - alat[jpja-2]
  alat = [alat, alat[jpja-1]+dy] & help, alat

  jpia = jpia+1
  jpja = jpja+1
  alon = alon#replicate(1., jpja) & help, alon
  alat = replicate(1., jpia)#alat & help, alat

  olon = ncdf_lec(fileo, var = 'nav_lon') & help, olon
  olat = ncdf_lec(fileo, var = 'nav_lat') & help, olat

  write_oasis, ngr, 'wrf4.lon', alon
  write_oasis, ngr, 'wrf4.lat', alat, /append
  write_oasis, ngr, 'ind4.lon', olon, /append
  write_oasis, ngr, 'ind4.lat', olat, /append

  write_ncdf, alon, alat, olon, olat, $
  varname = ['wrf4.lon', 'wrf4.lat', 'ind4.lon', 'ind4.lat'], $
  filename = ngr+'.nc'

  spawn, 'ncatted -O -h -a ,global,d,, '+ngr+'.nc'
  spawn, 'ncatted -O -h -a valid_min_max,,d,, '+ngr+'.nc'
  spawn, 'ncatted -O -h -a valid_min,,d,, '+ngr+'.nc'
  spawn, 'ncatted -O -h -a valid_max,,d,, '+ngr+'.nc'

;
;-------------------------------------------------
; grids size
;-------------------------------------------------
;
  sz = size(alon, /dimensions)
  jpia = sz[0]
  jpja = sz[1]

  sz = size(olon, /dimensions)
  jpio = sz[0]
  jpjo = sz[1]
;
;-------------------------------------------------
; mask file
;-------------------------------------------------
; WARNING: OASIS convention: 1 over land, 0 over Ocean
;
  nmsk = dir+'masks_wrf4_ind4_i4'
;
  amsk = long( ncdf_lec(filea, var = 'LANDMASK') )
  omsk = long( ncdf_lec(fileo, var = 'mask' ))
  help, amsk, omsk

  amsk = [amsk, lonarr(1, jpja-1)+1l]
  amsk = [ [amsk], [lonarr(jpia, 1)+1l] ]
  omsk[where(omsk LT 0)] = 0l

; patch
;  omsk[*, 1] = 0l
;  omsk[*, 245] = 0l
;  omsk[jpio-2, *] = 0l
;
  help, amsk, omsk

  write_oasis, nmsk, 'wrf4.msk', amsk, /i4
  write_oasis, nmsk, 'ind4.msk', 1l - omsk, /i4, /append
;
  write_ncdf, amsk, 1l - omsk, $
  varname = ['wrf4.msk','ind4.msk'], $
  filename = nmsk+'.nc'

  spawn, 'ncatted -O -h -a ,global,d,, '+nmsk+'.nc'
  spawn, 'ncatted -O -h -a valid_min_max,,d,, '+nmsk+'.nc'
  spawn, 'ncatted -O -h -a valid_min,,d,, '+nmsk+'.nc'
  spawn, 'ncatted -O -h -a valid_max,,d,, '+nmsk+'.nc'

;-------------------------------------------------
; areas file
;-------------------------------------------------
;
  nar = dir+'areas_wrf4_ind4_r8'
@cm_4mesh
;
  computegrid, XAXIS = alon, YAXIS = alat
  e1ta=e1t
  e2ta=e2t
  computegrid, XAXIS = olon, YAXIS = olat
  e1to=e1t
  e2to=e2t

  write_oasis, nar, 'wrf4.srf', e1ta*e2ta
  write_oasis, nar, 'ind4.srf', e1to*e2to, /append
;  write_oasis, nar, 'ind4.srf', ncdf_lec(fileo, var = 'e1t')*ncdf_lec(fileo, var = 'e2t'), /append

  write_ncdf, e1ta*e2ta, e1to*e2to, $
  varname = ['wrf4.srf','ind4.srf'], $
  filename = nar+'.nc'

  spawn, 'ncatted -O -h -a ,global,d,, '+nar+'.nc'
  spawn, 'ncatted -O -h -a valid_min_max,,d,, '+nar+'.nc'
  spawn, 'ncatted -O -h -a valid_min,,d,, '+nar+'.nc'
  spawn, 'ncatted -O -h -a valid_max,,d,, '+nar+'.nc'


;
;-------------------------------------------------
; mozaic oce -> atm
;-------------------------------------------------
;

;  fo2a = dir+'mozaic_ind4_wrf4_i4r8'

;  addr = lonarr(jpia, jpja)
;  oce = where(omsk EQ 1l)
;  lonoce = olon[oce]
;  latoce = olat[oce]
;  FOR jj = 0, jpja-1 DO BEGIN
;    IF jj MOD 10 EQ 0 THEN print, jj
;    FOR ji = 0, jpia-1 DO BEGIN
;      tmp = neighbor(alon[ji, jj], alat[ji, jj], lonoce, latoce)
;      addr[ji, jj] = oce[tmp]
;    ENDFOR
;  ENDFOR

;  wght = replicate(1., jpia, jpja)
;  wght[jpia-1, *] = 0.
;  wght[*, jpja-1] = 0.

;  write_oasis, fo2a, 'WEIGHTS1', wght
;  write_oasis, fo2a, 'ADRESSE1', round(addr)+1L, /append, /i4

;  write_ncdf, wght, round(temporary(addr))+1L, $
;  varname = ['WEIGHTS1','ADRESSE1'], $
;  filename = fo2a+'.nc
;
;-------------------------------------------------
; mozaic atm -> oce
;-------------------------------------------------
;

;  fa2o = dir+'mozaic_wrf4_ind4_i4r8'

;  addr = lonarr(jpio, jpjo)
;  oce = where(amsk EQ 0l)
;  lonoce = alon[oce]
;  latoce = alat[oce]
;  FOR jj = 0, jpjo-1 DO BEGIN
;    IF jj MOD 10 EQ 0 THEN print, jj
;    FOR ji = 0, jpio-1 DO BEGIN
;      tmp = neighbor(olon[ji, jj], olat[ji, jj], lonoce, latoce)
;      addr[ji, jj] = oce[tmp]
;    ENDFOR
;  ENDFOR

;  write_oasis, fa2o, 'WEIGHTS2', replicate(1., jpio, jpjo)
;  write_oasis, fa2o, 'ADRESSE2', round(addr)+1L, /append, /i4

;  write_ncdf, replicate(1., jpio, jpjo), round(temporary(addr))+1L, $
;  varname = ['WEIGHTS2','ADRESSE2'], $
;  filename = fa2o+'.nc

;STOP
;
;-------------------------------------------------
; current
;-------------------------------------------------
;
  write_oasis, dir+'curocean', 'O_OCurx1', fltarr(jpio, jpjo)
  write_oasis, dir+'curocean', 'O_OCury1', fltarr(jpio, jpjo), /append

STOP
;
;-------------------------------------------------
; sst
;-------------------------------------------------
;
  filein = '/Users/smasson/tmp/sstat.nc'
  sst = ncdf_lec(filein, var = 'SST')
  sst = [sst, fltarr(1, jpja-1)]
  sst = [ [sst], [fltarr(jpia, 1)] ]
  add = read_oasis(dir+'mozaic_wrf4_ind4_i4r8', 'ADRESSE2', jpio, jpjo, /i4)-1L
  sstoce = sst[add]
  write_oasis, dir+'sstocean', 'O_SSTSST', sstoce

;; add = read_oasis(dir+'mozaic_ind4_wrf4_i4r8', 'ADRESSE1', jpia, jpja, /i4)-1L
;; sstatm = sstoce[add]
;; ;
;-------------------------------------------------
; flx
;-------------------------------------------------
;
  write_oasis, dir+'flxatmos', 'AWRFTAUT', fltarr(jpia, jpja)
  write_oasis, dir+'flxatmos', 'AWRFTAUX', fltarr(jpia, jpja), /append
  write_oasis, dir+'flxatmos', 'AWRFTAUY', fltarr(jpia, jpja), /append
  write_oasis, dir+'flxatmos', 'AWRFEVPR', fltarr(jpia, jpja), /append
  write_oasis, dir+'flxatmos', 'A_WRFQSR', fltarr(jpia, jpja), /append
  write_oasis, dir+'flxatmos', 'A_WRFQNS', fltarr(jpia, jpja), /append

  return
end

PRO verif_interp

  compile_opt idl2, strictarrsubs
;
  dir = '/Volumes/data/DATAin/grids/'
  filea = dir + 'wrf_tmask.nc'
  fileo = dir + 'meshmask_ind4.nc'
;
;-------------------------------------------------
; grid file
;-------------------------------------------------
;
  ngr = dir+'grids_wrf4_ind4_r8'
;
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

  olon = ncdf_lec(fileo, var = 'glamt')
  olat = ncdf_lec(fileo, var = 'gphit')
;
;-------------------------------------------------
; grids size
;-------------------------------------------------
;
  sz = size(alon, /dimensions)
  jpia = sz[0]
  jpja = sz[1]

  sz = size(olon, /dimensions)
  jpio = sz[0]
  jpjo = sz[1]
;
;-------------------------------------------------
; mask file
;-------------------------------------------------
; WARNING: OASIS convention: 1 over land, 0 over Ocean
;
  nmsk = dir+'masks_wrf4_ind4_i4'
;
  amsk = byte( ncdf_lec(filea, var = 'MASK_T') )
  amsk = [amsk, bytarr(1, jpja-1)]
  amsk = [ [amsk], [bytarr(jpia, 1)] ]
;
  omsk = byte( ncdf_lec(fileo, var = 'tmask', count = [jpio, jpjo, 1, 1]) )
;
;-------------------------------------------------
; sst
;-------------------------------------------------
;
  filein = '/Users/smasson/tmp/sstat.nc'
  sst = ncdf_lec(filein, var = 'SST')
  sst = [sst, fltarr(1, jpja-1)]
  sst = [ [sst], [fltarr(jpia, 1)] ]

  sst = sst-273.15

  add = read_oasis(dir+'mozaic_wrf4_ind4_i4r8', 'ADRESSE2', jpio, jpjo, /i4)-1L
  sstoce = sst[add]
  add = read_oasis(dir+'mozaic_ind4_wrf4_i4r8', 'ADRESSE1', jpia, jpja, /i4)-1L
  sstatm = sstoce[add]

;;   tvplus, sst*amsk, min = 15, max = 31
;;   tvplus, sstoce*omsk, min = 15, max = 31, win = 1
;;   tvplus, sstatm*amsk, min = 15, max = 31, win = 2

nn = 10
sst = bosses(jpia/nn,jpja/nn)
sst=sst[*]#replicate(1.,(nn+1))
sst=reform(transpose(reform(sst,jpia/nn,jpja/nn,(nn+1)),[0,2,1]),jpia/nn*(nn+1),jpja/nn)
sst=replicate(1.,(nn+1))#sst[*]
sst=reform(transpose(reform(sst,(nn+1),jpia/nn*(nn+1),jpja/nn),[1,2,0]),jpia/nn*(nn+1),jpja/nn*(nn+1))
sst1 = sst[0:jpia-1, 0:jpja-1]

nn = 4
sst = dist(jpia/nn,jpja/nn)
sst=sst[*]#replicate(1.,(nn+1))
sst=reform(transpose(reform(sst,jpia/nn,jpja/nn,(nn+1)),[0,2,1]),jpia/nn*(nn+1),jpja/nn)
sst=replicate(1.,(nn+1))#sst[*]
sst=reform(transpose(reform(sst,(nn+1),jpia/nn*(nn+1),jpja/nn),[1,2,0]),jpia/nn*(nn+1),jpja/nn*(nn+1))
sst = sst[0:jpia-1, 0:jpja-1]*sst1
sst = shift(sst, 0, 30) 

  add = read_oasis(dir+'mozaic_wrf4_ind4_i4r8', 'ADRESSE2', jpio, jpjo, /i4)-1L
  sstoce = sst[add]
  add = read_oasis(dir+'mozaic_ind4_wrf4_i4r8', 'ADRESSE1', jpia, jpja, /i4)-1L
  sstatm = sstoce[add]

  tvplus, sst*amsk
  tvplus, sstoce*omsk, win = 1
  tvplus, sstatm*amsk, win = 2


stop

return
end
