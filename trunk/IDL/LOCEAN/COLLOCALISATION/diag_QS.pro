PRO diag_QS
@common
computegrid,-200,-200,25,25,17,17

machine = 'cratos'
var_typ = 'WSC' ; wsc / nsc / ano_bef

explist = ['FORCED_SW2_KF','COUPLED_SW2_KF']

basin   = 'SIO'
freq    = '6H'
datebeg = '19900101' & help, datebeg


FOR iexp = 0, n_elements(explist)-1 DO BEGIN

expname = explist[iexp] & help, expname

IF expname EQ 'FORCED_SW2_KF' THEN period  = '1990-1998' ELSE period  = '1990-2009' & help, period
IF expname EQ 'FORCED_SW2_KF' THEN dateend = '19990101' ELSE dateend = '20100101' & help, dateend

pathin  = '/net/adonis/usr/adonis/varclim/gslod/IDL/COLLOCALISATION/EXP_'+expname+'/DATA/' & help, pathin

print, pathin + 'Q2_2D_500km_' + expname +'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat'
restore, pathin + 'Q2_2D_500km_' + expname +'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat', /VERBOSE
dxy_q2wsc = dxy_var_wsc_rot
dxy_q2nsc = dxy_var_nsc_rot

print, pathin + 'T2_2D_500km_' + expname +'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat'
restore, pathin + 'T2_2D_500km_' + expname +'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat', /VERBOSE
dxy_t2wsc = dxy_var_wsc_rot
dxy_t2nsc = dxy_var_nsc_rot

print, pathin + 'SST_2D_500km_' + expname +'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat'
restore, pathin + 'SST_2D_500km_' + expname +'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat', /VERBOSE
dxy_sstwsc = dxy_var_wsc_rot
dxy_sstnsc = dxy_var_nsc_rot

print, pathin + 'PSFC_2D_500km_' + expname +'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat'
restore, pathin + 'PSFC_2D_500km_' + expname +'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat', /VERBOSE
dxy_psfcwsc = dxy_var_wsc_rot
dxy_psfcnsc = dxy_var_nsc_rot

;pvapsat2wsc = 6.1094 * exp( 17.625 * (dxy_t2wsc  - 273.15) / (dxy_t2wsc  - 273.15 + 243.04))
;pvapsat0wsc = 6.1094 * exp( 17.625 * (dxy_sstwsc - 273.15) / (dxy_sstwsc - 273.15 + 243.04))
;qvapsat2wsc = 0.622 * pvapsat2wsc / (dxy_psfcwsc / 100. - pvapsat2wsc) & help, qvapsat2wsc
;qvapsat0wsc = 0.622 * pvapsat0wsc / (dxy_psfcwsc / 100. - pvapsat0wsc) & help, qvapsat0wsc

;pvapsat2nsc = 6.1094 * exp( 17.625 * (dxy_t2nsc)  / (dxy_t2nsc  + 243.04))
;pvapsat0nsc = 6.1094 * exp( 17.625 * (dxy_sstnsc) / (dxy_sstnsc + 243.04))
;qvapsat2nsc = 0.622 * pvapsat2nsc / (dxy_psfcnsc / 100. - pvapsat2nsc) & help, qvapsat2nsc
;qvapsat0nsc = 0.622 * pvapsat0nsc / (dxy_psfcnsc / 100. - pvapsat0nsc) & help, qvapsat0nsc

; Magnus Tetens Formula (K,hPa)
psatwsc = 6.1078 * exp(17.2694 * (dxy_sstwsc - 273.16) / (dxy_sstwsc - 35.86))
psatnsc = 6.1078 * exp(17.2694 * (dxy_sstnsc - 273.16) / (dxy_sstnsc - 35.86))

; Wikipedia / Tropflux Formula (hPa,kg/kg)
qsatwsc = 0.622 * psatwsc / (dxy_psfcwsc / 100. - 0.378 * psatwsc)
qsatnsc = 0.622 * psatnsc / (dxy_psfcnsc / 100. - 0.378 * psatnsc)

;dxy_var_wsc_rot = pvapsat2wsc - dxy_q2wsc * 1000.
;dxy_var_nsc_rot = pvapsat2nsc - dxy_q2nsc * 1000.
;save, dxy_var_wsc_rot, dxy_var_nsc_rot, filename = pathin + 'QS2-Q2_2D_500km_' + expname +'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat', /VERBOSE

dxy_var_wsc_rot = dxy_q2wsc - qsatwsc
dxy_var_nsc_rot = dxy_q2nsc - qsatnsc
save, dxy_var_wsc_rot, dxy_var_nsc_rot, filename = pathin + 'Q2-QS0_2D_500km_' + expname +'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat', /VERBOSE

dxy_var_wsc_rot = qsatwsc
dxy_var_nsc_rot = qsatnsc
save, dxy_var_wsc_rot, dxy_var_nsc_rot, filename = pathin + 'QS0_2D_500km_' + expname +'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat', /VERBOSE

dxy_var_wsc_rot = dxy_t2wsc - dxy_sstwsc
dxy_var_nsc_rot = dxy_t2nsc - dxy_sstnsc
save, dxy_var_wsc_rot, dxy_var_nsc_rot, filename = pathin + 'T2-SST_2D_500km_' + expname +'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat', /VERBOSE


ENDFOR; exp
stop
END
