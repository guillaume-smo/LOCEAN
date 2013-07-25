PRO diag_HFXLH
@common


explist = ['FORCED_SW2_KF','COUPLED_SW2_KF']
basin   = 'SIO'
freq    = '6H'
datebeg = '19900101' & help, datebeg


FOR iexp = 0, n_elements(explist)-1 DO BEGIN

expname = explist[iexp] & help, expname

IF expname EQ 'FORCED_SW2_KF' THEN period  = '1990-1998' ELSE period  = '1990-2009' & help, period
IF expname EQ 'FORCED_SW2_KF' THEN dateend = '19990101' ELSE dateend = '20100101' & help, dateend

pathin  = '/net/adonis/usr/adonis/varclim/gslod/IDL/COLLOCALISATION/EXP_'+expname+'/DATA/' & help, pathin

print, pathin + 'HFX_2D_500km_' + expname +'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat'
restore, pathin + 'HFX_2D_500km_' + expname +'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat', /VERBOSE
dxy_var1_wsc = dxy_var_wsc_rot
dxy_var1_nsc = dxy_var_nsc_rot

print, pathin + 'LH_2D_500km_' + expname +'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat'
restore, pathin + 'LH_2D_500km_' + expname +'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat', /VERBOSE
dxy_var2_wsc = dxy_var_wsc_rot
dxy_var2_nsc = dxy_var_nsc_rot

dxy_var_wsc_rot = dxy_var1_wsc + dxy_var2_wsc
dxy_var_nsc_rot = dxy_var1_nsc + dxy_var2_nsc

save, dxy_var_wsc_rot, dxy_var_nsc_rot, dist_tc, filename = pathin + 'HFXLH_2D_500km_' + expname +'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat', /VERBOSE

ENDFOR; exp

END
