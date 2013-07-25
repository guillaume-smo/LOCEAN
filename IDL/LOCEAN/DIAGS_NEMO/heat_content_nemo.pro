PRO heat_content_nemo
@all_cm
 
exp_name = 'COUPLED_SW2_KF'

pathin = '/Volumes/TIME_MACHINE/EXP_'+exp_name+'/NEMO/POST/MONTHLY/DATA/'
pathou = '/Volumes/TIME_MACHINE/EXP_'+exp_name+'/WRF/INDICES/MONTHLY/'
filein = 'nemo_out_monthly_clim_1990-2009.nc'
fileou = 'ohc26_monthly_clim_1990-2009.nc'


; GRILLE + MASQUE WRF
maskfile = '/Users/gslod/WORK/IDL/MASK_WRF_GRID_T.nc'
initncdf, maskfile, /fullcgrid
landmask = read_ncdf('LANDMASK',filename=maskfile,/nostruct)
mask=landmask & mask[where(landmask EQ 1)]=0 & mask[where(landmask EQ 0)]=1
gphit_w = gphit[firstxt:lastxt,firstyt:lastyt] & help, gphit_w
glamt_w = glamt[firstxt:lastxt,firstyt:lastyt] & help, glamt_w
mask_w  = mask & help, mask_w
ohc26_gridwrf = fltarr([size(glamt_w,/dim),12]) & help, ohc26_gridwrf


; GRILLE NEMO
initncdf, pathin+filein, /fullCgrid
domdef, 0, 7, /zindex
gphit_o = gphit[firstxt:lastxt,firstyt:lastyt] & help, gphit_o
glamt_o = glamt[firstxt:lastxt,firstyt:lastyt] & help, glamt_o


; CALCUL OHC
cp = 4.1855 ; water heat capacity (J.g-1.degC-1)

temp = read_ncdf('thetao',0,11,/timestep, filename=pathin+filein, /nostruct)
mask3D  = fltarr(size(glamt_o,/dim))+1 & mask3D[where(temp[*,*,0,*] EQ 0)] = 0
mask_o  = fltarr(size(glamt_o,/dim))+1 & mask_o[where(temp[*,*,0,0] EQ 0)]=0 & help, mask_o
temp = temp[*,*,*,*] & temp[where(temp EQ 0)] = !values.f_nan & help, temp
sal = read_ncdf('so',0,11,/timestep, filename=pathin+filein, /nostruct)
sal = sal[*,*,*,*] & sal[where(sal EQ 0)] = !values.f_nan & help, sal

mask = where(temp[*,*,0,*] LT 26.)
bad = where(temp LT 26.) & help, bad
temp[bad] = !values.f_nan & sal[bad] = !values.f_nan

rho = rhon(temporary(sal),temp) & help, rho

ohc26 = fltarr(nxt, nyt, 12) & ohc26[*,*,*] = 0. & help, ohc26
iso26 = fltarr(nxt, nyt, 12) & iso26[*,*,*] = !values.f_nan & help, iso26
lowbnd = gdept + e3t/2

FOR l = 0, n_elements(temp[0,0,0,*])-1 DO BEGIN
  ohc26[*,*,l] = cp * moyenne(rho[*,*,*,l]*(temp[*,*,*,l]-26.), 'z', /integration, /nan) / 10000.
ENDFOR

ohc26[where(mask3D eq 0)] = !values.f_nan & ohc26[mask] = 0. & help, ohc26

FOR l = 0, n_elements(temp[0,0,0,*])-1 DO BEGIN
  ohc26_gridwrf[*,*,l] = fromirr('bilinear', ohc26[*,*,l], glamt_o, gphit_o, mask_o, glamt_w, gphit_w, mask_w)
ENDFOR
help, ohc26_gridwrf


; AUTRE METHODE CALCUL OHC + PRECISE

;FOR j = 0, n_elements(temp[0,*,0,0])-1 DO BEGIN
;FOR i = 0, n_elements(temp[*,0,0,0])-1 DO BEGIN

;  IF finite(temp[i,j,0,l]) EQ 1 THEN BEGIN
;    iso26[i,j] = interpol(gdept[*],temp[i,j,*,l],26.)
;    lastdz = iso26[i,j] - lowbnd[max(where(gdept+e3t/2 LE iso26[i,j]))]
;  ENDIF

;ENDFOR
;ENDFOR
;ENDFOR

fid = NCDF_CREATE(pathou+fileou, /CLOBBER)
xid = NCDF_DIMDEF(fid, 'longitude', n_elements(gphit_w[*,0]))
yid = NCDF_DIMDEF(fid, 'latitude', n_elements(gphit_w[0,*]))
tid = NCDF_DIMDEF(fid, 'time', /UNLIMITED)
vid = NCDF_VARDEF(fid, 'ohc26', [xid, yid, tid], /FLOAT)
NCDF_CONTROL, fid, /ENDEF
NCDF_VARPUT, fid, vid, ohc26_gridwrf
NCDF_CLOSE, fid


END
