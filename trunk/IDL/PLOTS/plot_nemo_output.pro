PRO plot_nemo_output
@all_cm

path='/home/gsamson/WORK/AROME/TEST_CPL/EXP6/'
file='IVAN12_6h_20080213_20080216_grid_T.nc'
initncdf, path+file

lon2D = read_ncdf('nav_lon', filename=path+file, /nostruct) & help, lon2D
lat2D = read_ncdf('nav_lat', filename=path+file, /nostruct) & help, lat2D
sst  = read_ncdf('sst', filename=path+file, /nostruct, /allrecords) & help, sst
sss  = read_ncdf('sss', filename=path+file, /nostruct, /allrecords) & help, sss
ssh  = read_ncdf('ssh', filename=path+file, /nostruct, /allrecords) & help, ssh
emp  = read_ncdf('empmr', filename=path+file, /nostruct, /allrecords) & help, emp
qt   = read_ncdf('qt', filename=path+file, /nostruct, /allrecords) & help, qt
qns  = read_ncdf('qns', filename=path+file, /nostruct, /allrecords) & help, qns
qsr  = read_ncdf('qsr', filename=path+file, /nostruct, /allrecords) & help, qsr
;qlw  = read_ncdf('qlw', filename=path+file, /nostruct, /allrecords) & help, qlw
;qsb  = read_ncdf('qsb', filename=path+file, /nostruct, /allrecords) & help, qsb
;qla  = read_ncdf('qla', filename=path+file, /nostruct, /allrecords) & help, qla
wspd = read_ncdf('windspd', filename=path+file, /nostruct, /allrecords) & help, wspd
tau  = read_ncdf('taum', filename=path+file, /nostruct, /allrecords) & help, tau

mask = sst*0.+1. & mask[where(sst EQ 0.)] = !VALUES.F_NAN & help, mask

dateini = 20080213.00d & help, dateini
juldini = date2jul(dateini)
nbdt    = n_elements(sst[0,0,*])
juld    = juldini + dindgen(nbdt)/4.
date    = jul2date(juld, hour=hour, day=day, month=month, year=year)

msst = mask * sst & help, msst

STOP
END
