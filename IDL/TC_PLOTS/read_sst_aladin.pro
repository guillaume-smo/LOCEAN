print, '' & print, 'READ SST ANALYSE ALADIN...'

ech='6'
;path_aladana = '/home/gsamson/WORK/ALADIN/OPER_ASSIM_ECH'+ech+'/'
path_aladana = '/homelocal-px/px-126/gsamson/WORK/ALADIN/OPER_ASSIM_ECH'+ech+'/'

CASE tc_name OF
  'IVAN'     : file_aladana = '200802/ICMSHALAD+000'+ech+'_200802_v2.nc'
  'GAEL'     : file_aladana = '200902/ICMSHALAD+000'+ech+'_200902_v2.nc'
  'GELANE'   : file_aladana = '201002/ICMSHALAD+000'+ech+'_201002_v2.nc'
  'GIOVANNA' : file_aladana = '201202/ICMSHALAD+000'+ech+'_201202_v2.nc'
  'FELLENG'  : file_aladana = '201301/ICMSHALAD+000'+ech+'_201301_v3.nc'  
ENDCASE
print, path_aladana+file_aladana
initncdf, path_aladana+file_aladana
domdef, dom_tc

juld_aladana = ncdf_gettime(path_aladana+file_aladana)
juldini_aladana = juld_aladana[0]
dateini_aladana = jul2date(juldini_aladana)
juld_aladana = juldini_aladana + dindgen(jpt)*0.25d
date_aladana = jul2date(juld_aladana)
print, 'PERIOD: ', dateini_aladana, date_aladana[jpt-1], f='(A8,F11.2,1X,F11.2)'
ibeg = max(where(date_aladana LE date_beg_obs))
iend = min(where(date_aladana GE date_end_obs))
IF ibeg[0] EQ -1 OR iend[0] EQ -1 THEN STOP
date_aladana    = date_aladana[ibeg:iend]
juldini_aladana = juld_aladana[ibeg]
jpt = iend-ibeg+1
tdim_aladana = n_elements(date_aladana) & help, tdim_aladana
lon_aladana  = glamt[firstxt:lastxt,0] & help,lon_aladana
lat_aladana  = reform(gphit[0,firstyt:lastyt]) & help, lat_aladana
xdim_aladana = n_elements(lon_aladana) & help, xdim_aladana
ydim_aladana = n_elements(lat_aladana) & help, ydim_aladana
res_aladana  = max(e1t, /nan)/1000. & help, res_aladana
sst_aladana  = read_ncdf('SST', ibeg, iend, timestep=1, filename=path_aladana+file_aladana, /nostruct)
mask_aladana = read_ncdf('IND.TERREMER', ibeg, iend, timestep=1, filename=path_aladana+file_aladana, /nostruct)
mask_aladana[where(mask_aladana EQ 1.)] = !VALUES.F_NAN
mask_aladana[where(mask_aladana EQ 0.)] = 1.
sst_aladana = sst_aladana * mask_aladana
help, sst_aladana, mask_aladana

print, 'READ SST ANALYSE ALADIN OK' & print, ''
