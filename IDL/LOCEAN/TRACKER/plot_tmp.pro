PRO plot_tmp


restore, filename='traj_cyc.idl', /verbose
help, uv10cycn


;-----------------------------------------------------------------------------------------
; lecture masque+lon+lat (1 sur terre, 0 sur mer)

initncdf, '/Volumes/Iomega_HDD/EXP_COUPLED_SW2_BMJ/WRF/mask_wrf.nc'
maskfile = '/Volumes/Iomega_HDD/EXP_COUPLED_SW2_BMJ/WRF/mask_wrf.nc'
mask = ncdf_lec(maskfile, var = 'LANDMASK')
lon = float(ncdf_lec(maskfile,var='longitude'))
lat = float(ncdf_lec(maskfile,var='latitude'))
;help,lon,lat
lon2D = lon # replicate(1., n_elements(lat))
lat2D = replicate(1., n_elements(lon)) # lat
;help,lon2D,lat2D


;-----------------------------------------------------------------------------------------
; creation d'un nouveau masque "elargi" pour eviter les effets cotiers et les forçages orographiques
; (ile de la reunion typiquement)

newmask = mask

FOR i = 1, n_elements(lon)-2 DO BEGIN
  FOR j = 1, n_elements(lat)-2 DO BEGIN
    IF mask[i,j] EQ 1 THEN BEGIN
      newmask[i-1,j-1] = 1
      newmask[i-1,j  ] = 1
      newmask[i-1,j+1] = 1
      newmask[i,j-1] = 1
      newmask[i,j+1] = 1
      newmask[i+1,j-1] = 1
      newmask[i+1,j  ] = 1
      newmask[i+1,j+1] = 1
    ENDIF
  ENDFOR
ENDFOR

mask = newmask


;-----------------------------------------------------------------------------------------
; lecture pression de surface + vent à 10m

file_in = '/Volumes/Iomega_HDD/EXP_COUPLED_SW2_BMJ/WRF/POST/LIGHT/wrfout_light.19900103.nc'
mslp = ncdf_lec(file_in,var='PSFC')
u10 = ncdf_lec(file_in,var='U10')
v10 = ncdf_lec(file_in,var='V10')
;help, mslp, u10, v10


;-----------------------------------------------------------------------------------------
; conversion 2D -> 3D

mask3D = reform(reform(mask, n_elements(lon)*n_elements(lat))#replicate(1,n_elements(mslp[0,0,*])), n_elements(lon), n_elements(lat), n_elements(mslp[0,0,*]))


;-----------------------------------------------------------------------------------------
; application du masque aux variables

mslp[where(mask3D EQ 1)] = !values.f_nan
u10[where(mask3D EQ 1)] = !values.f_nan
v10[where(mask3D EQ 1)] = !values.f_nan

uv10 = (u10^2+v10^2)^0.5


;-----------------------------------------------------------------------------------------
; boucle temporelle

;FOR t = 0, n_elements(mslp[0,0,*]) - 1, 4 DO BEGIN
FOR t = 268, 272 DO BEGIN


plt, uv10[*,*,t], 15, 40, int=1, /nocontour, /realcont, small=[1,2,1]
plt, mslp[*,*,t]/100., 950, 1050, int=10, /realcont, small=[1,2,2], /noerase

saveimage, 'uv10_mslp_'+strtrim(t,2)+'.gif'

ENDFOR

END
