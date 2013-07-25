PRO heat_content_glorys
@all_cm
 
exp_name = 'GLORYS'

pathin = '/Volumes/TIME_MACHINE/DATA/GLORYS/'
filein = 'GLORYS2V1_IND4_monthly_clim_1992-2009_grid_T_NAN.nc'
      
initncdf, pathin+filein, /fullCgrid
;box = [30,130,-30,25]
;domdef,box
domdef, 0, 19, /zindex

; water heat capacity (J.g-1.degC-1)
cp = 4.1855

temp = read_ncdf('votemper',0,11,/timestep, filename=pathin+filein, /nostruct)
temp = temp[*,*,*,*] & help, temp

filein = 'GLORYS2V1_IND4_monthly_clim_1992-2009_grid_S_NAN.nc'
sal = read_ncdf('vosaline',0,11,/timestep, filename=pathin+filein, /nostruct)
sal = sal[*,*,*,*] & help, sal

mask = where(temp[*,*,0,*] LT 26.)
bad = where(temp LT 26.) & help, bad
temp[bad] = !values.f_nan & sal[bad] = !values.f_nan

rho = rhon(temporary(sal),temp) & help, rho

ohc26 = fltarr(nxt, nyt, 12) & ohc26[*,*,*] = 0. & help, ohc26
;iso26 = fltarr(nxt, nyt, 12) & iso26[*,*,*] = !values.f_nan & help, iso26
;lowbnd = gdept + e3t/2

FOR l = 0, n_elements(temp[0,0,0,*])-1 DO BEGIN
  ohc26[*,*,l] = cp * moyenne(rho[*,*,*,l]*(temp[*,*,*,l]-26.), 'z', /integration, /nan) / 10000.
ENDFOR

ohc26[mask] = 0.
;FOR j = 0, n_elements(temp[0,*,0,0])-1 DO BEGIN
;FOR i = 0, n_elements(temp[*,0,0,0])-1 DO BEGIN

;  IF finite(temp[i,j,0,l]) EQ 1 THEN BEGIN
;    iso26[i,j] = interpol(gdept[*],temp[i,j,*,l],26.)
;    lastdz = iso26[i,j] - lowbnd[max(where(gdept+e3t/2 LE iso26[i,j]))]
;  ENDIF

;ENDFOR
;ENDFOR

fid = NCDF_CREATE(file_out, /CLOBBER)
xid = NCDF_DIMDEF(fid, 'longitude', n_elements(lon))
yid = NCDF_DIMDEF(fid, 'latitude', n_elements(lat))
tid = NCDF_DIMDEF(fid, 'time', /UNLIMITED)
vid = NCDF_VARDEF(fid, 'tpot'+pression, [xid, yid, tid], /DOUBLE)
NCDF_CONTROL, fid, /ENDEF
NCDF_VARPUT, fid, vid, tpotnew
NCDF_CLOSE, fid


END
