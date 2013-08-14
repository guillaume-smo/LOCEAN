PRO calcul_ohc_MERCATOR
@all_cm


; parametres
product = 'GLORYS2V3'
tc_name = 'GIOVANNA'
cp = 4.1855 ; water heat capacity (J.g-1.degC-1)


; init
path_t500 = '/home/gsamson/WORK/DATA/T500_'+product+'_'+tc_name+'/'
file_t500 = FILE_SEARCH( path_t500 + 'T500_'+product+'_1dAV_*-*_gridT.nc' )
print, file_t500
path_s500 = '/home/gsamson/WORK/DATA/S500_'+product+'_'+tc_name+'/'
file_s500 = FILE_SEARCH( path_s500 + 'S500_'+product+'_1dAV_*-*_gridT.nc' )
print, file_s500
initncdf, file_t500


; lecture
t500 = REFORM( read_ncdf( 'votemper', /all, filename=file_t500, /nostruct))
help, t500
s500 = REFORM( read_ncdf( 'vosaline', /all, filename=file_s500, /nostruct))
help, s500


; mask
t500[where(t500 LT 26.)] = !values.f_nan
s500[where(t500 LT 26.)] = !values.f_nan
r500 = rhon(temporary(s500),t500) & help, r500


; calcul
ohc26 = fltarr(nxt, nyt, jpt) + !values.f_nan & help, ohc26
dept26 = fltarr(nxt, nyt, jpt) + !values.f_nan & help, dept26
FOR l = 0, jpt-1 DO BEGIN
  ohc26[*,*,l] = cp * moyenne( r500[*,*,*,l] * (t500[*,*,*,l]-26.), 'z', /integration, /nan) / 10000.
  FOR j = 0, nyt-1 DO BEGIN
    FOR i = 0, nxt-1 DO BEGIN
      indok = where(finite(t500[i,j,*,l]) EQ 1)
      IF indok[0] NE -1 THEN dept26[i,j,l] = gdept[max(indok, /nan)]
    ENDFOR
  ENDFOR
ENDFOR


; netcdf
path_ohc26 = '/home/gsamson/WORK/DATA/OHC26_'+product+'_'+tc_name+'/'
file_ohc26 = 'OHC26' + strmid((strsplit(file_t500, '/', count=nb, /extract))[nb-1],4)
SPAWN, 'mkdir -p '+path_ohc26
fid   = NCDF_CREATE(path_ohc26+file_ohc26, /CLOBBER)
xid   = NCDF_DIMDEF(fid, 'longitude', n_elements(gphit[*,0]))
yid   = NCDF_DIMDEF(fid, 'latitude', n_elements(gphit[0,*]))
tid   = NCDF_DIMDEF(fid, 'time', /UNLIMITED)
vid1  = NCDF_VARDEF(fid,  'ohc26', [xid, yid, tid], /FLOAT)
vid2  = NCDF_VARDEF(fid, 'dept26', [xid, yid, tid], /FLOAT)
lonid = NCDF_VARDEF(fid, 'longitude', [xid, yid], /FLOAT)
latid = NCDF_VARDEF(fid,  'latitude', [xid, yid], /FLOAT)
NCDF_CONTROL, fid, /ENDEF
NCDF_VARPUT, fid, vid1, ohc26
NCDF_VARPUT, fid, vid2, dept26
NCDF_VARPUT, fid, lonid, glamt
NCDF_VARPUT, fid, latid, gphit
NCDF_CLOSE, fid

STOP
END
