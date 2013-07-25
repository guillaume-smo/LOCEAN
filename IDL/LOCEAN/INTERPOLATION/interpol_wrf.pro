PRO interpol_wrf, pression

;pression = 800.

maskfile = 'mask_wrf.nc'
initncdf,  maskfile, /fullcgrid
;mask = ncdf_lec(maskfile, var = 'LANDMASK') & help, mask
lon = float(ncdf_lec(maskfile,var='longitude'))
lat = float(ncdf_lec(maskfile,var='latitude'))
;lon2D = lon # replicate(1., n_elements(lat))
;lat2D = replicate(1., n_elements(lon)) # lat


file_in = '/Volumes/TIME_MACHINE/EXP_COUPLED_SW2_BMJ/UV_800/wrfout_d01_1990-01-01_00:00:00_UVPB_.nc'
p = ncdf_lec(file_in,var='PB') / 100.
u = ncdf_lec(file_in,var='U')
v = ncdf_lec(file_in,var='V')
;uv = complex(u,v)

vor  = dblarr(n_elements(lon),n_elements(lat),n_elements(u[0,0,0,*]))
unew = dblarr(n_elements(lon),n_elements(lat),n_elements(u[0,0,0,*]))
vnew = dblarr(n_elements(lon),n_elements(lat),n_elements(u[0,0,0,*]))
;uvnew = complex(unew,vnew)

FOR l = 0, n_elements(u[0,0,0,*])-1 DO BEGIN
  FOR i = 0, n_elements(lon)-1 DO BEGIN
    FOR j = 0, n_elements(lat)-1 DO BEGIN

      unew[i,j,l] = interpol(u[i,j,*,l], p[i,j,*,0], pression)
      vnew[i,j,l] = interpol(v[i,j,*,l], p[i,j,*,0], pression)
;      uvnew[i,j,l] = interpol(uv[i,j,*,l], p[i,j,*,0], new.)
     
    ENDFOR
  ENDFOR

  vor[*,*,l] = curl(unew[*,*,l],vnew[*,*,l])

ENDFOR

;unew= real_part(uvnew)
;vnew= imaginary(uvnew)

stop
END
