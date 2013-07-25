PRO compare_IVAN12
@all_cm

write_ps = 0

;path_aladin = '/nemo/data1/gsamson/IVAN12/DIRRUN_IVAN12/ALADIN/'
;path_ecmwf  = '/nemo/data1/gsamson/IVAN12/DIRRUN_IVAN12/ECMWF/'
path_aladin = '/data/rd_exchange/gsamson/OUTPUT_IVAN12/DIRRUN_IVAN12_ALADIN/'
path_ecmwf  = '/data/rd_exchange/gsamson/OUTPUT_IVAN12/DIRRUN_IVAN12_ECMWF/'
file_aladin = 'IVAN12_3h_20080206_20080220_grid_T.nc'
file_ecmwf  = 'IVAN12_3h_20080206_20080220_grid_T.nc'

; lecture
initncdf, path_aladin+file_aladin
lon2D   = read_ncdf('nav_lon', filename=path_aladin+file_aladin, /nostruct) & help, lon2D
lat2D   = read_ncdf('nav_lat', filename=path_aladin+file_aladin, /nostruct) & help, lat2D
sst_ala = read_ncdf('sst', filename=path_aladin+file_aladin, /nostruct, /allrecords) & help, sst_ala
sss_ala = read_ncdf('sss', filename=path_aladin+file_aladin, /nostruct, /allrecords) & help, sss_ala
ssh_ala = read_ncdf('ssh', filename=path_aladin+file_aladin, /nostruct, /allrecords) & help, ssh_ala
emp_ala = read_ncdf('empmr', filename=path_aladin+file_aladin, /nostruct, /allrecords) & help, emp_ala
qt_ala  = read_ncdf('qt', filename=path_aladin+file_aladin, /nostruct, /allrecords) & help, qt_ala
qns_ala = read_ncdf('qns', filename=path_aladin+file_aladin, /nostruct, /allrecords) & help, qns_ala
qsr_ala = read_ncdf('qsr', filename=path_aladin+file_aladin, /nostruct, /allrecords) & help, qsr_ala
qlw_ala = read_ncdf('qlw', filename=path_aladin+file_aladin, /nostruct, /allrecords) & help, qlw_ala
qsb_ala = read_ncdf('qsb', filename=path_aladin+file_aladin, /nostruct, /allrecords) & help, qsb_ala
qla_ala = read_ncdf('qla', filename=path_aladin+file_aladin, /nostruct, /allrecords) & help, qla_ala
wspd_ala = read_ncdf('windspd', filename=path_aladin+file_aladin, /nostruct, /allrecords) & help, wspd_ala
tau_ala = read_ncdf('taum', filename=path_aladin+file_aladin, /nostruct, /allrecords) & help, tau_ala

; time axis
dateini = 20080206.00d & help, dateini
juldini = date2jul(dateini)
nbdt    = n_elements(sst_ala[0,0,*])
juld    = juldini + dindgen(nbdt)/8.
date    = jul2date(juld, hour=hour, day=day, month=month, year=year)

; spatial average
msst_ala = grossemoyenne(sst_ala,'xy', /nan)
msss_ala = grossemoyenne(sss_ala,'xy', /nan)
mssh_ala = grossemoyenne(ssh_ala,'xy', /nan)
memp_ala = grossemoyenne(emp_ala,'xy', /nan)
mqt_ala = grossemoyenne(qt_ala,'xy', /nan)
mqns_ala = grossemoyenne(qns_ala,'xy', /nan)
mqsr_ala = grossemoyenne(qsr_ala,'xy', /nan)
mqlw_ala = grossemoyenne(qlw_ala,'xy', /nan)
mqsb_ala = grossemoyenne(qsb_ala,'xy', /nan)
mqla_ala = grossemoyenne(qla_ala,'xy', /nan)
mwspd_ala = grossemoyenne(wspd_ala,'xy', /nan)
mtau_ala  = grossemoyenne(tau_ala,'xy', /nan)

; temporal average
tsst_ala = grossemoyenne(sst_ala,'t', /nan)
tsss_ala = grossemoyenne(sss_ala,'t', /nan)
tssh_ala = grossemoyenne(ssh_ala,'t', /nan)
temp_ala = grossemoyenne(emp_ala,'t', /nan)
tqt_ala = grossemoyenne(qt_ala,'t', /nan)
tqns_ala = grossemoyenne(qns_ala,'t', /nan)
tqsr_ala = grossemoyenne(qsr_ala,'t', /nan)
tqlw_ala = grossemoyenne(qlw_ala,'t', /nan)
tqsb_ala = grossemoyenne(qsb_ala,'t', /nan)
tqla_ala = grossemoyenne(qla_ala,'t', /nan)
twspd_ala = grossemoyenne(wspd_ala,'t', /nan)
ttau_ala  = grossemoyenne(tau_ala,'t', /nan)

; lecture
initncdf, path_ecmwf+file_ecmwf
sst_ifs = read_ncdf('sst', filename=path_ecmwf+file_ecmwf, /nostruct, /allrecords) & help, sst_ifs
sss_ifs = read_ncdf('sss', filename=path_ecmwf+file_ecmwf, /nostruct, /allrecords) & help, sss_ifs
ssh_ifs = read_ncdf('ssh', filename=path_ecmwf+file_ecmwf, /nostruct, /allrecords) & help, ssh_ifs
emp_ifs = read_ncdf('empmr', filename=path_ecmwf+file_ecmwf, /nostruct, /allrecords) & help, emp_ifs
qt_ifs  = read_ncdf('qt', filename=path_ecmwf+file_ecmwf, /nostruct, /allrecords) & help, qt_ifs
qns_ifs = read_ncdf('qns', filename=path_ecmwf+file_ecmwf, /nostruct, /allrecords) & help, qns_ifs
qsr_ifs = read_ncdf('qsr', filename=path_ecmwf+file_ecmwf, /nostruct, /allrecords) & help, qsr_ifs
qlw_ifs = read_ncdf('qlw', filename=path_ecmwf+file_ecmwf, /nostruct, /allrecords) & help, qlw_ifs
qsb_ifs = read_ncdf('qsb', filename=path_ecmwf+file_ecmwf, /nostruct, /allrecords) & help, qsb_ifs
qla_ifs = read_ncdf('qla', filename=path_ecmwf+file_ecmwf, /nostruct, /allrecords) & help, qla_ifs
wspd_ifs = read_ncdf('windspd', filename=path_ecmwf+file_ecmwf, /nostruct, /allrecords) & help, wspd_ifs
tau_ifs = read_ncdf('taum', filename=path_ecmwf+file_ecmwf, /nostruct, /allrecords) & help, tau_ifs

; spatial average
msst_ifs = grossemoyenne(sst_ifs,'xy', /nan)
msss_ifs = grossemoyenne(sss_ifs,'xy', /nan)
mssh_ifs = grossemoyenne(ssh_ifs,'xy', /nan)
memp_ifs = grossemoyenne(emp_ifs,'xy', /nan)
mqt_ifs = grossemoyenne(qt_ifs,'xy', /nan)
mqns_ifs = grossemoyenne(qns_ifs,'xy', /nan)
mqsr_ifs = grossemoyenne(qsr_ifs,'xy', /nan)
mqlw_ifs = grossemoyenne(qlw_ifs,'xy', /nan)
mqsb_ifs = grossemoyenne(qsb_ifs,'xy', /nan)
mqla_ifs = grossemoyenne(qla_ifs,'xy', /nan)
mwspd_ifs = grossemoyenne(wspd_ifs,'xy', /nan)
mtau_ifs  = grossemoyenne(tau_ifs,'xy', /nan)

; temporal average
tsst_ifs = grossemoyenne(sst_ifs,'t', /nan)
tsss_ifs = grossemoyenne(sss_ifs,'t', /nan)
tssh_ifs = grossemoyenne(ssh_ifs,'t', /nan)
temp_ifs = grossemoyenne(emp_ifs,'t', /nan)
tqt_ifs = grossemoyenne(qt_ifs,'t', /nan)
tqns_ifs = grossemoyenne(qns_ifs,'t', /nan)
tqsr_ifs = grossemoyenne(qsr_ifs,'t', /nan)
tqlw_ifs = grossemoyenne(qlw_ifs,'t', /nan)
tqsb_ifs = grossemoyenne(qsb_ifs,'t', /nan)
tqla_ifs = grossemoyenne(qla_ifs,'t', /nan)
twspd_ifs = grossemoyenne(wspd_ifs,'t', /nan)
ttau_ifs  = grossemoyenne(tau_ifs,'t', /nan)

; plots
var1=msst_ala
var2=msst_ifs
min_plot = floor(min([var1,var2]))
max_plot = ceil(max([var1,var2]))
IF write_ps THEN openps, filename='SST_compar_IVAN12.ps'
splot, var1, yrange=[min_plot,max_plot], win=0, title='sea surface temperature', xtickformat='(I8)', ytitle='sst (degC)'
oplot, var1, thick=3, color=0
oplot, var2, thick=3, color=250
xyouts, 0.125, 0.175, 'ALADIN', /normal, charsize=2, charthick=2, color=0
xyouts, 0.125, 0.15, 'ECMWF', /normal, charsize=2, charthick=2, color=250
IF write_ps THEN closeps

var1=mtau_ala
var2=mtau_ifs
min_plot = floor(min([var1,var2]))
max_plot = ceil(max([var1,var2]))
IF write_ps THEN openps, filename='TAU_compar_IVAN12.ps'
splot, date, var1, yrange=[min_plot,max_plot], win=1, title='wind stress module', xtickformat='(I8)', ytitle='taum (N/m^2)'
oplot, date, var1, thick=3, color=0
oplot, date, var2, thick=3, color=250
xyouts, 0.125, 0.175, 'ALADIN', /normal, charsize=2, charthick=2, color=0
xyouts, 0.125, 0.15, 'ECMWF', /normal, charsize=2, charthick=2, color=250
IF write_ps THEN closeps

var1=mqt_ala
var2=mqt_ifs
min_plot = floor(min([var1,var2]))
max_plot = ceil(max([var1,var2]))
IF write_ps THEN openps, filename='net_downward_heat_flux_compar_IVAN12.ps'
splot, date, var1, yrange=[min_plot,max_plot], win=2, title='Net Downward Heat Flux', xtickformat='(I8)', ytitle='qt (W/m^2)'
oplot, date, var1, thick=3, color=0
oplot, date, var2, thick=3, color=250
xyouts, 0.125, 0.175, 'ALADIN', /normal, charsize=2, charthick=2, color=0
xyouts, 0.125, 0.15, 'ECMWF', /normal, charsize=2, charthick=2, color=250
IF write_ps THEN closeps

var1=mqns_ala
var2=mqns_ifs
min_plot = floor(min([var1,var2]))
max_plot = ceil(max([var1,var2]))
IF write_ps THEN openps, filename='nonsolar_downward_heat_flux_compar_IVAN12.ps'
splot, date, var1, yrange=[min_plot,max_plot], win=3, title='non solar Downward Heat Flux', xtickformat='(I8)', ytitle='qns (W/m^2)'
oplot, date, var1, thick=3, color=0
oplot, date, var2, thick=3, color=250
xyouts, 0.125, 0.175, 'ALADIN', /normal, charsize=2, charthick=2, color=0
xyouts, 0.125, 0.15, 'ECMWF', /normal, charsize=2, charthick=2, color=250
IF write_ps THEN closeps

var1=mqsr_ala
var2=mqsr_ifs
min_plot = floor(min([var1,var2],/nan))
max_plot = ceil(max([var1,var2],/nan))
IF write_ps THEN openps, filename='shortwave_radiation_compar_IVAN12.ps'
splot, date, var1, yrange=[min_plot,max_plot], win=4, title='Shortwave Radiation', xtickformat='(I8)', ytitle='qsr (W/m^2)'
oplot, date, var1, thick=3, color=0
oplot, date, var2, thick=3, color=250
xyouts, 0.125, 0.175, 'ALADIN', /normal, charsize=2, charthick=2, color=0
xyouts, 0.125, 0.15, 'ECMWF', /normal, charsize=2, charthick=2, color=250
IF write_ps THEN closeps

var1=mqlw_ala
var2=mqlw_ifs
min_plot = floor(min([var1,var2]))
max_plot = ceil(max([var1,var2]))
IF write_ps THEN openps, filename='longwave_downward_heat_flux_compar_IVAN12.ps'
splot, date, var1, yrange=[min_plot,max_plot], win=5, title='Longwave Downward Heat Flux', xtickformat='(I8)', ytitle='qlw (W/m^2)'
oplot, date, var1, thick=3, color=0
oplot, date, var2, thick=3, color=250
xyouts, 0.125, 0.175, 'ALADIN', /normal, charsize=2, charthick=2, color=0
xyouts, 0.125, 0.15, 'ECMWF', /normal, charsize=2, charthick=2, color=250
IF write_ps THEN closeps

var1=mqsb_ala
var2=mqsb_ifs
min_plot = floor(min([var1,var2]))
max_plot = ceil(max([var1,var2]))
IF write_ps THEN openps, filename='sensible_downward_heat_flux_compar_IVAN12.ps'
splot, date, var1, yrange=[min_plot,max_plot], win=6, title='Sensible Downward Heat Flux', xtickformat='(I8)', ytitle='qsb (W/m^2)'
oplot, date, var1, thick=3, color=0
oplot, date, var2, thick=3, color=250
xyouts, 0.125, 0.175, 'ALADIN', /normal, charsize=2, charthick=2, color=0
xyouts, 0.125, 0.15, 'ECMWF', /normal, charsize=2, charthick=2, color=250
IF write_ps THEN closeps

var1=mqla_ala
var2=mqla_ifs
min_plot = floor(min([var1,var2]))
max_plot = ceil(max([var1,var2]))
IF write_ps THEN openps, filename='latent_downward_heat_flux_compar_IVAN12.ps'
splot, date, var1, yrange=[min_plot,max_plot], win=7, title='Latent Downward Heat Flux', xtickformat='(I8)', ytitle='qla (W/m^2)'
oplot, date, var1, thick=3, color=0
oplot, date, var2, thick=3, color=250
xyouts, 0.125, 0.175, 'ALADIN', /normal, charsize=2, charthick=2, color=0
xyouts, 0.125, 0.15, 'ECMWF', /normal, charsize=2, charthick=2, color=250
IF write_ps THEN closeps


STOP
END
