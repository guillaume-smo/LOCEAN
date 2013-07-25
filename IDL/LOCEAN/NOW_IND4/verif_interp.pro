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
