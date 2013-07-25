PRO diag_RAIN
@common

computegrid,-200,-200,25,25,17,17

explist = ['FORCED_SW2_KF','COUPLED_SW2_KF','FORCED_SW2_BMJ','COUPLED_SW2_BMJ']
basin   = 'SIO'
freq    = '6H'
datebeg = '19900101' & help, datebeg


FOR iexp = 0, n_elements(explist)-1 DO BEGIN

expname = explist[iexp] & help, expname

IF expname EQ 'FORCED_SW2_KF' THEN period  = '1990-1998' ELSE period  = '1990-2009' & help, period
IF expname EQ 'FORCED_SW2_KF' THEN dateend = '19990101' ELSE dateend = '20100101' & help, dateend

pathin  = '/net/adonis/usr/adonis/varclim/gslod/IDL/COLLOCALISATION/EXP_'+expname+'/DATA/' & help, pathin

print, pathin + 'RAINC_2D_' + expname +'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat'
restore, pathin + 'RAINC_2D_' + expname +'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat', /VERBOSE
dxy_raincwsc = dxy_var_wsc_rot
dxy_raincnsc = dxy_var_nsc_rot

print, pathin + 'RAINNC_2D_' + expname +'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat'
restore, pathin + 'RAINNC_2D_' + expname +'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat', /VERBOSE
dxy_rainncwsc = dxy_var_wsc_rot
dxy_rainncnsc = dxy_var_nsc_rot

dxy_var_wsc_rot = dxy_raincwsc + dxy_rainncwsc
dxy_var_nsc_rot = dxy_raincnsc + dxy_rainncnsc

save, dxy_var_wsc_rot, dxy_var_nsc_rot, dist_tc, filename = pathin + 'RAIN_2D_' + expname +'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat', /VERBOSE

ENDFOR; exp

END
