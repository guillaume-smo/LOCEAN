PRO aladin_grib2netcdf

dirlist = file_search('DBLE/200802*', /FULLY_QUALIFY_PATH)
basenam = 'GRIDHSTBOURBON01+0006'


FOR i = 0, n_elements(dirlist)-1 DO BEGIN


file = dirlist[i] + '/forecast/' + basenam
print, file

varnames = grib_get_parameternames(file)
print, varnames

inv  = grib_inventory(file, /multi_support)
xdisplayfile, text=inv

tmp1 = grib_get_record(file, 1, /structure)
tmp2 = grib_get_record(file, 2, /structure)
STOP


; VARIABLES GRIB
surft = reverse((grib_get_record(file, 1, /structure)).values,2) & help, surft ; surface temperature (K) (11)
dswrf = reverse((grib_get_record(file, 2, /structure)).values,2) & help, dswrf ; downward surface short-wave radiation flux accumulated (J.m**-2) (111)
dlwrf = reverse((grib_get_record(file, 3, /structure)).values,2) & help, dlwrf ; downward surface long-wave radiation flux accumulated (J.m**-2) (112)
acpcp = reverse((grib_get_record(file, 4, /structure)).values,2) & help, acpcp ; convective precipitation (kg.m**-2) (62 ou 63)
aclsp = reverse((grib_get_record(file, 5, /structure)).values,2) & help, aclsp ; large-scale precipitation (kg.m**-2) (62 ou 63)
aclhf = reverse((grib_get_record(file, 6, /structure)).values,2) & help, aclhf ; latent heat flux accumulated (J.m**-2) (121)
acshf = reverse((grib_get_record(file, 7, /structure)).values,2) & help, acshf ; sensible heat flux accumulated (J.m**-2) (122)
taux  = reverse((grib_get_record(file, 8, /structure)).values,2) & help, taux ; U-component of time-integrated stress (J.m**-2) (130)
tauy  = reverse((grib_get_record(file, 9, /structure)).values,2) & help, tauy ; V-component of time-integrated stress (J.m**-2) (131)
mslp  = reverse((grib_get_record(file, 10, /structure)).values,2) & help, mslp ; mslp (Pa) (2)
u10m  = reverse((grib_get_record(file, 11, /structure)).values,2) & help, u10m ; 10 metre U wind component (m.s**-1) (33)
v10m  = reverse((grib_get_record(file, 12, /structure)).values,2) & help, v10m ; 10 metre V wind component (m.s**-1) (34)
t2m   = reverse((grib_get_record(file, 13, /structure)).values,2) & help, t2m ; 2 metre temperature (K) (11)
rh2m  = reverse((grib_get_record(file, 14, /structure)).values,2) & help, rh2m ; 2 metre relative humidity (%) (52)
uraf  = reverse((grib_get_record(file, 15, /structure)).values,2) & help, uraf ; u wind gust (m.s**-1) (163)
vraf  = reverse((grib_get_record(file, 16, /structure)).values,2) & help, vraf ; v wind gust (m.s**-1) (164)


; CONVERSIONS "NEMO FRIENDLY"
iswrf = dswrf / (6. * 3600.) ; (W/m2)
ilwrf = dlwrf / (6. * 3600.) ; (W/m2)
itotp = (acpcp + aclsp) / (6. * 3600.) ; (mm/s)
ilhf  = aclhf / (6. * 3600.) ; (W/m2)
ishf  = acshf / (6. * 3600.) ; (W/m2)
itaux = taux / (6. * 3600.) ; (N/m2)
itauy = tauy / (6. * 3600.) ; (N/m2)
t2mc  = t2m - 273.15 ; (degC)
rh2m  = rh2m / 100.  ; (%)


; COORDONNEES GRILLE LON-LAT
lon1D = (grib_get_record(file, 1, /structure)).distinctlongitudes
lat1D = (grib_get_record(file, 1, /structure)).distinctlatitudes

tmplon = (grib_get_record(file, 1, /structure)).longitudes
tmplat = (grib_get_record(file, 1, /structure)).latitudes
lon2D  = dblarr(n_elements(lon1D),n_elements(lat1D))
lat2D  = dblarr(n_elements(lon1D),n_elements(lat1D))
help, lon2D, lat2D

FOR j = 0, n_elements(lat1D)-1 DO BEGIN
  lon2D[*,j] = tmplon[j*n_elements(lon1D):(j+1)*n_elements(lon1D)-1]
  lat2D[*,j] = tmplat[j*n_elements(lon1D):(j+1)*n_elements(lon1D)-1]
ENDFOR
lat2D = reverse(lat2D,2)


; ECRITURE NETCDF
fid = NCDF_CREATE(file+'.nc', /CLOBBER)
xid = NCDF_DIMDEF(fid, 'lon', n_elements(lon2D[*,0]))
yid = NCDF_DIMDEF(fid, 'lat', n_elements(lat2D[0,*]))
vid1 = NCDF_VARDEF(fid, 'lon', [xid], /FLOAT)
vid2 = NCDF_VARDEF(fid, 'lat', [yid], /FLOAT)
vid3 = NCDF_VARDEF(fid, 'nav_lon', [xid,yid], /FLOAT)
vid4 = NCDF_VARDEF(fid, 'nav_lat', [xid,yid], /FLOAT)
vid5 = NCDF_VARDEF(fid, 'sst', [xid,yid], /FLOAT)
vid6 = NCDF_VARDEF(fid, 'sosudosw', [xid,yid], /FLOAT)
vid7 = NCDF_VARDEF(fid, 'sosudolw', [xid,yid], /FLOAT)
vid8 = NCDF_VARDEF(fid, 'sowaprec', [xid,yid], /FLOAT)
vid9 = NCDF_VARDEF(fid,  'qla', [xid,yid], /FLOAT)
vid10 = NCDF_VARDEF(fid, 'qsb', [xid,yid], /FLOAT)
vid11 = NCDF_VARDEF(fid,'utau', [xid,yid], /FLOAT)
vid12 = NCDF_VARDEF(fid,'vtau', [xid,yid], /FLOAT)
vid13 = NCDF_VARDEF(fid, 'somslpre', [xid,yid], /FLOAT)
vid14 = NCDF_VARDEF(fid, 'uwspd10', [xid,yid], /FLOAT)
vid15 = NCDF_VARDEF(fid, 'vwspd10', [xid,yid], /FLOAT)
vid16 = NCDF_VARDEF(fid, 'sotemair', [xid,yid], /FLOAT)
vid17 = NCDF_VARDEF(fid, 'sohumrel', [xid,yid], /FLOAT)
vid18 = NCDF_VARDEF(fid, 'ugust10', [xid,yid], /FLOAT)
vid19 = NCDF_VARDEF(fid, 'vgust10', [xid,yid], /FLOAT)

NCDF_ATTPUT,  fid, vid1, 'units', 'degrees_east'
NCDF_ATTPUT,  fid, vid2, 'units', 'degrees_north'
NCDF_ATTPUT,  fid, vid3, 'units', 'degrees_east'
NCDF_ATTPUT,  fid, vid4, 'units', 'degrees_north'
NCDF_ATTPUT,  fid, vid5, 'units', 'K'
NCDF_ATTPUT,  fid, vid6, 'units', 'W.m**-2'
NCDF_ATTPUT,  fid, vid7, 'units', 'W.m**-2'
NCDF_ATTPUT,  fid, vid8, 'units', 'mm'
NCDF_ATTPUT,  fid, vid9, 'units', 'W.m**-2'
NCDF_ATTPUT,  fid, vid10, 'units', 'W.m**-2'
NCDF_ATTPUT,  fid, vid11, 'units', 'N.m**-2'
NCDF_ATTPUT,  fid, vid12, 'units', 'N.m**-2'
NCDF_ATTPUT,  fid, vid13, 'units', 'Pa'
NCDF_ATTPUT,  fid, vid14, 'units', 'm.s**-1'
NCDF_ATTPUT,  fid, vid15, 'units', 'm.s**-1'
NCDF_ATTPUT,  fid, vid16, 'units', 'C'
NCDF_ATTPUT,  fid, vid17, 'units', '%'
NCDF_ATTPUT,  fid, vid18, 'units', 'm.s**-1'
NCDF_ATTPUT,  fid, vid19, 'units', 'm.s**-1'
NCDF_CONTROL, fid, /ENDEF

NCDF_VARPUT,  fid, vid1, lon2D[*,0]
NCDF_VARPUT,  fid, vid2, reform(lat2D[0,*])
NCDF_VARPUT,  fid, vid3, lon2D
NCDF_VARPUT,  fid, vid4, lat2D
NCDF_VARPUT,  fid, vid5, surft
NCDF_VARPUT,  fid, vid6, iswrf
NCDF_VARPUT,  fid, vid7, ilwrf
NCDF_VARPUT,  fid, vid8, itotp
NCDF_VARPUT,  fid, vid9, ilhf
NCDF_VARPUT,  fid, vid10, ishf
NCDF_VARPUT,  fid, vid11, itaux
NCDF_VARPUT,  fid, vid12, itauy
NCDF_VARPUT,  fid, vid13, mslp
NCDF_VARPUT,  fid, vid14, u10m
NCDF_VARPUT,  fid, vid15, v10m
NCDF_VARPUT,  fid, vid16, t2mc
NCDF_VARPUT,  fid, vid17, rh2m
NCDF_VARPUT,  fid, vid18, uraf
NCDF_VARPUT,  fid, vid19, vraf
NCDF_CLOSE,   fid

ENDFOR

STOP
RETURN
END
