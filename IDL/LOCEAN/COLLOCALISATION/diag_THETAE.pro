PRO diag_THETAE
@common

explist = ['FORCED_SW2_KF','COUPLED_SW2_KF'];'FORCED_SW2_BMJ','COUPLED_SW2_BMJ']

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

print, pathin + 'PSFC_2D_500km_' + expname +'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat'
restore, pathin + 'PSFC_2D_500km_' + expname +'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat', /VERBOSE
dxy_psfcwsc = dxy_var_wsc_rot / 100.
dxy_psfcnsc = dxy_var_nsc_rot / 100.


; calcul theta-e d'apres Bolton 1980 et Holland 1997

; vapor pressure (hPa)
vpwsc = dxy_psfcwsc * dxy_q2wsc / (0.622 + dxy_q2wsc) & help, vpwsc
vpnsc = dxy_psfcnsc * dxy_q2nsc / (0.622 + dxy_q2nsc) & help, vpnsc

; temperature at the lifting condensation level (K)
tlwsc = (2840 / (3.5 * alog(dxy_t2wsc) - alog(vpwsc) - 4.805)) + 55 & help, tlwsc
tlnsc = (2840 / (3.5 * alog(dxy_t2nsc) - alog(vpnsc) - 4.805)) + 55 & help, tlnsc

; equivalent potential temperature (K)
tewsc = dxy_t2wsc * exp( (3.376 / tlwsc - 0.00254) * dxy_q2wsc * 1000 * (1 + 0.81 * dxy_q2wsc)) & help, tewsc
tensc = dxy_t2nsc * exp( (3.376 / tlnsc - 0.00254) * dxy_q2wsc * 1000 * (1 + 0.81 * dxy_q2nsc)) & help, tensc

dxy_var_wsc_rot = tewsc
dxy_var_nsc_rot = tensc
save, dxy_var_wsc_rot, dxy_var_nsc_rot, dist_tc, filename = pathin + 'THETAE_2D_500km_' + expname +'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat', /VERBOSE

ENDFOR; exp

END
