PRO pgd_ascii2netcdf
COMPILE_OPT IDL2, STRICTARRSUBS
; CREATION DES FICHIERS MASKS ET AREAS OASIS (NETCDF) A PARTIR DU FICHIER PGD SURFEX (ASCII)
; ATTENTION: TABLEAUX SURFEX DE DIMENSION C+I ET TABLEAUX OASIS DE DIMENSION C+I+E


; LECTURE FICHIER PGD ASCII
all = getfile('PGD.txt')
dum = strsplit(all, /extract)
allines = strarr(n_elements(dum))
FOR i = 0, n_elements(dum)-1 DO allines[i]=strjoin(dum[i], ' ')


; DECLARATIONS
tmp  = fltarr(4500*50) ; format ascii pgd
idim = (fix(allines[where(STRMATCH(allines,'&FULL IMAX'))+2]))[0]
jdim = (fix(allines[where(STRMATCH(allines,'&FULL JMAX'))+2]))[0]
icie = idim + 15
jcie = jdim + 15
lon  = fltarr(icie,jcie)
lat  = fltarr(icie,jcie)
mask = fltarr(icie,jcie)
fsea = fltarr(icie,jcie)
e1t  = fltarr(icie,jcie) ; delta x en 2D
e2t  = fltarr(icie,jcie) ; delta y en 2D
area = fltarr(icie,jcie)
arfs = fltarr(icie,jcie)
orog = fltarr(icie,jcie)


; LONGITUDES
ideb = (where(STRMATCH(allines,'&FULL XLON')) + 2)[0]
ifin = ideb + 4500 -1
FOR i = ideb, ifin DO BEGIN
  tmp1 = all[i]
  tmp2 = strsplit(tmp1, /extract)
  tmp3 = float(tmp2)
  tmp[(i-ideb)*50:(i-ideb+1)*50-1] = tmp3
ENDFOR
FOR j = 0, jdim-1 DO BEGIN
  lon[0:idim-1,j] = tmp[j*idim:(j+1)*idim-1]
ENDFOR
FOR j = jdim, jcie-1 DO BEGIN
  lon[*,j] = lon[*,j-1]+(lon[*,j-1]-lon[*,j-2])
ENDFOR
FOR i = idim, icie-1 DO BEGIN
  lon[i,*] = lon[i-1,*]+(lon[i-1,*]-lon[i-2,*])
ENDFOR


; LATITUDES
ideb = (where(STRMATCH(allines,'&FULL XLAT')) + 2)[0]
ifin = ideb + 4500 -1
FOR i = ideb, ifin DO BEGIN
  tmp1 = all[i]
  tmp2 = strsplit(tmp1, /extract)
  tmp3 = float(tmp2)
  tmp[(i-ideb)*50:(i-ideb+1)*50-1] = tmp3
ENDFOR
FOR j = 0, jdim-1 DO BEGIN
  lat[0:idim-1,j] = tmp[j*idim:(j+1)*idim-1]
ENDFOR
FOR j = jdim, jcie-1 DO BEGIN
  lat[*,j] = lat[*,j-1]+(lat[*,j-1]-lat[*,j-2])
ENDFOR
FOR i = idim, icie-1 DO BEGIN
  lat[i,*] = lat[i-1,*]+(lat[i-1,*]-lat[i-2,*])
ENDFOR


; MASK (1:masked/land, 0:not masked/sea)
ideb = (where(STRMATCH(allines,'&FULL FRAC_SEA')) + 2)[0]
ifin = ideb + 4500 -1
FOR i = ideb, ifin DO BEGIN
  tmp1 = all[i]
  tmp2 = strsplit(tmp1, /extract)
  tmp3 = float(tmp2)
  tmp[(i-ideb)*50:(i-ideb+1)*50-1] = tmp3
ENDFOR
FOR j = 0, jdim-1 DO BEGIN
  fsea[0:idim-1,j] = tmp[j*idim:(j+1)*idim-1]
ENDFOR
FOR j = jdim, jcie-1 DO BEGIN
  fsea[*,j] = 0.
ENDFOR
FOR i = idim, icie-1 DO BEGIN
  fsea[i,*] = 0.
ENDFOR
mask = 1 - fsea
mask[where(mask GT 0. AND mask LT 1.)] = 0. ; frac_sea > 0 = SST


; E1T
ideb = (where(STRMATCH(allines,'&FULL XX')) + 2)[0]
ifin = ideb + 4500 -1
FOR i = ideb, ifin DO BEGIN
  tmp1 = all[i]
  tmp2 = strsplit(tmp1, /extract)
  tmp3 = float(tmp2)
  tmp[(i-ideb)*50:(i-ideb+1)*50-1] = tmp3
ENDFOR
FOR j = 0, jdim-1 DO BEGIN
  e1t[0:idim-1,j] = tmp[j*idim:(j+1)*idim-1]
ENDFOR
e1t[0:idim-1,0:jdim-1] = [e1t[0,0:jdim-1],(e1t-shift(e1t,1))[1:idim-1,0:jdim-1]]


; E2T
ideb = (where(STRMATCH(allines,'&FULL YY')) + 2)[0]
ifin = ideb + 4500 -1
FOR i = ideb, ifin DO BEGIN
  tmp1 = all[i]
  tmp2 = strsplit(tmp1, /extract)
  tmp3 = float(tmp2)
  tmp[(i-ideb)*50:(i-ideb+1)*50-1] = tmp3
ENDFOR
FOR j = 0, jdim-1 DO BEGIN
  e2t[0:idim-1,j] = tmp[j*idim:(j+1)*idim-1]
ENDFOR
e2t[0:idim-1,0:jdim-1] = [[e2t[0:idim-1,0]],[(e2t-shift(e2t,0,1))[0:idim-1,1:jdim-1]]]

area = e1t * e2t
arfs = (e1t * e2t) * fsea


; OROGRAPHY
ideb = (where(STRMATCH(allines,'&FULL ZS')) + 2)[0]
ifin = ideb + 4500 -1
FOR i = ideb, ifin DO BEGIN
  tmp1 = all[i]
  tmp2 = strsplit(tmp1, /extract)
  tmp3 = float(tmp2)
  tmp[(i-ideb)*50:(i-ideb+1)*50-1] = tmp3
ENDFOR
FOR j = 0, jdim-1 DO BEGIN
  orog[0:idim-1,j] = tmp[j*idim:(j+1)*idim-1]
ENDFOR
FOR j = jdim, jcie-1 DO BEGIN
  orog[*,j] = 0.
ENDFOR
FOR i = idim, icie-1 DO BEGIN
  orog[i,*] = 0.
ENDFOR


STOP


; NETCDF
fid = NCDF_CREATE('MASK_AROME_CIE.nc', /CLOBBER)
xid = NCDF_DIMDEF(fid, 'lon', n_elements(lon[*,0]))
yid = NCDF_DIMDEF(fid, 'lat', n_elements(lat[0,*]))
vid1 = NCDF_VARDEF(fid, 'mask', [xid, yid], /SHORT)
vid2 = NCDF_VARDEF(fid, 'e1t', [xid, yid], /SHORT)
vid3 = NCDF_VARDEF(fid, 'e2t', [xid, yid], /SHORT)
vid4 = NCDF_VARDEF(fid, 'lon', [xid], /FLOAT)
vid5 = NCDF_VARDEF(fid, 'lat', [yid], /FLOAT)
vid6 = NCDF_VARDEF(fid, 'lon2D', [xid,yid], /FLOAT)
vid7 = NCDF_VARDEF(fid, 'lat2D', [xid,yid], /FLOAT)
vid8 = NCDF_VARDEF(fid, 'orog', [xid,yid], /FLOAT)
NCDF_ATTPUT,  fid, vid1, 'units', '1:masked/land, 0:not masked/sea'
NCDF_ATTPUT,  fid, vid2, 'units', 'meters'
NCDF_ATTPUT,  fid, vid3, 'units', 'meters'
NCDF_ATTPUT,  fid, vid4, 'units', 'degrees_east'
NCDF_ATTPUT,  fid, vid5, 'units', 'degrees_north'
NCDF_ATTPUT,  fid, vid6, 'units', 'degrees_east'
NCDF_ATTPUT,  fid, vid7, 'units', 'degrees_north'
NCDF_ATTPUT,  fid, vid8, 'units', 'meters'
NCDF_CONTROL, fid, /ENDEF
NCDF_VARPUT,  fid, vid1, mask
NCDF_VARPUT,  fid, vid2, e1t
NCDF_VARPUT,  fid, vid3, e2t
NCDF_VARPUT,  fid, vid4, lon[*,0]
NCDF_VARPUT,  fid, vid5, reform(lat[0,*])
NCDF_VARPUT,  fid, vid6, lon
NCDF_VARPUT,  fid, vid7, lat
NCDF_VARPUT,  fid, vid8, orog
NCDF_CLOSE,   fid

STOP

fid = NCDF_CREATE('AREAS_AROME_CIE.nc', /CLOBBER)
xid = NCDF_DIMDEF(fid, 'lon', n_elements(lon[*,0]))
yid = NCDF_DIMDEF(fid, 'lat', n_elements(lat[0,*]))
vid1 = NCDF_VARDEF(fid, 'areas', [xid, yid], /FLOAT)
vid2 = NCDF_VARDEF(fid, 'areas_fracsea', [xid, yid], /FLOAT)
vid4 = NCDF_VARDEF(fid, 'lon' , [xid], /FLOAT)
vid5 = NCDF_VARDEF(fid, 'lat' , [yid], /FLOAT)
vid6 = NCDF_VARDEF(fid, 'lon2D' , [xid,yid], /FLOAT)
vid7 = NCDF_VARDEF(fid, 'lat2D' , [xid,yid], /FLOAT)
NCDF_ATTPUT,  fid, vid1, 'units', '1:masked/land, 0:not masked/sea'
NCDF_ATTPUT,  fid, vid2, 'units', 'meters square'
NCDF_ATTPUT,  fid, vid3, 'units', 'meters square'
NCDF_ATTPUT,  fid, vid4, 'units', 'degrees_east'
NCDF_ATTPUT,  fid, vid5, 'units', 'degrees_north'
NCDF_ATTPUT,  fid, vid6, 'units', 'degrees_east'
NCDF_ATTPUT,  fid, vid7, 'units', 'degrees_north'
NCDF_CONTROL, fid, /ENDEF
NCDF_VARPUT,  fid, vid1, area
NCDF_VARPUT,  fid, vid2, arfs
NCDF_VARPUT,  fid, vid4, lon[*,0]
NCDF_VARPUT,  fid, vid5, reform(lat[0,*])
NCDF_VARPUT,  fid, vid6, lon
NCDF_VARPUT,  fid, vid7, lat
NCDF_CLOSE,   fid


RETURN
END
