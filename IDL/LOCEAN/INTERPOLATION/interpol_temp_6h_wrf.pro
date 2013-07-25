PRO interpol_temp_6h_wrf, year, pression, expname

maskfile = 'mask_wrf.nc'
initncdf,  maskfile, /fullcgrid
lon = float(ncdf_lec(maskfile,var='longitude')) & help, lon
lat = float(ncdf_lec(maskfile,var='latitude')) & help, lat

;FOR y = 1990, 1999 DO BEGIN

;year = strtrim(y,2) & help, year
path = '/u/rech/eee/reee959/EXP_'+ expname +'/WRF/'+ year +'/'

FOR m = 1, 9 DO BEGIN

IF m LT 10 THEN month = '0' + strtrim(long(m),2)
IF m GE 10 THEN month = strtrim(long(m),2)
help, month

file_list = file_search(path + 'wrfout_d01_' + year + '-' + month + '*',/FULLY_QUALIFY_PATH)

FOR d = 0, n_elements(file_list)-1 DO BEGIN

;print, 'FILE NUMBER:', d, '/', n_elements(file_list)-1
file_in = file_list[d]
;print, 'FILE IN:', file_in
str = strsplit(file_in, '_', /EXTRACT)
;print, 'DATE:', str[5]
file_out = path +'temp'+ strtrim(pression,2) +'_'+ str[5] + '.nc'
;print, 'FILE OUT:', file_out

pb = ncdf_lec(file_in,var='PB') / 100.
p = ncdf_lec(file_in,var='P') / 100.
t = ncdf_lec(file_in,var='T')
p = p + pb
tpot = t + 300.
 
tpotnew  = fltarr(n_elements(lon),n_elements(lat),n_elements(t[0,0,0,*]))

FOR l = 0, n_elements(t[0,0,0,*])-1 DO BEGIN
  FOR i = 0, n_elements(lon)-1 DO BEGIN
    FOR j = 0, n_elements(lat)-1 DO BEGIN

      tpotnew[i,j,l] = interpol(tpot[i,j,*,l], p[i,j,*,0], float(pression))
     
    ENDFOR
  ENDFOR
ENDFOR

fid = NCDF_CREATE(file_out, /CLOBBER)
xid = NCDF_DIMDEF(fid, 'longitude', n_elements(lon))
yid = NCDF_DIMDEF(fid, 'latitude', n_elements(lat))
tid = NCDF_DIMDEF(fid, 'time', /UNLIMITED)
vid = NCDF_VARDEF(fid, 'tpot'+pression, [xid, yid, tid], /DOUBLE)
NCDF_CONTROL, fid, /ENDEF
NCDF_VARPUT, fid, vid, tpotnew
NCDF_CLOSE, fid

ENDFOR ; day
ENDFOR ; month
;ENDFOR ; year

nextyear = long(year) + 1
;SPAWN, 'export year='+strtrim(nextyear,2)+'; export prog=interpol_temp_6h_wrf; export pression='+pression+'; llsubmit idl.ll'

END
