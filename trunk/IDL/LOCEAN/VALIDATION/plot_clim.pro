PRO plot_clim
; read & plot seasonal cycle or spatial clim fields for all bassins
@common


; MPI, RH600, VOR850, SHEAR, CAPE, GPI
var_list = ['RH600','VOR850','SHEAR','MPI','GPI']
exp_list = ['ERA-I','COUPLED_SW2_KF','FORCED_SW2_KF']
bas_list = ['SIO']

write_ps   = 1 ; ecriture figures ps
plot_2D    = 0 ; plot 2D
plot_1D    = 1 ; plot 1D


;--------------------------------------------------------------------------------------


; declarations
allvar1D_sc_sio = fltarr(n_elements(exp_list),12)
allvar1D_sc_nio = fltarr(n_elements(exp_list),12)
allvar1D_sc_bob = fltarr(n_elements(exp_list),12)
allvar1D_sc_ars = fltarr(n_elements(exp_list),12)
allvar1D_sc_swi = fltarr(n_elements(exp_list),12)
allvar1D_sc_sei = fltarr(n_elements(exp_list),12)


; loops
FOR v = 0, n_elements(var_list)-1 DO BEGIN
FOR e = 0, n_elements(exp_list)-1 DO BEGIN

varn = var_list[v] & help, varn
expn = exp_list[e] & help, expn


; path + file
path = '/net/adonis/usr/adonis/varclim/gslod/EXP_'+expn+'/'
file = varn+'_CLIM_1990-2009.nc'


; definition bassins
box_io  = [30,130,-25,25]
box_sio = [30,130,-25, 0]
box_nio = [50,100,  0,25]
box_bob = [80,100,  0,25]
box_ars = [50, 80,  0,25]
box_swi = [30, 80,-25, 0]
box_sei = [80,130,-25, 0]


; init + mask
IF expn EQ 'ERA-I' THEN BEGIN
  initncdf, path+file
  sst  =  read_ncdf('sstk', 0, 0, /timestep, filename=path+'var3D_monthly_clim_1990-2009.nc', /nostruct)
  mask = sst & mask[where(finite(sst) EQ 1)] = 1 & mask[where(finite(sst) EQ 0)] = !VALUES.F_NAN & help, mask
ENDIF

IF expn NE 'ERA-I' THEN BEGIN
  initncdf, '/usr/home/gslod/IDL/MASK_WRF_IND4_GRIDT.nc'
  landmask = read_ncdf('LANDMASK',filename='/usr/home/gslod/IDL/MASK_WRF_IND4_GRIDT.nc',/nostruct) & help, landmask
  mask = landmask & mask[where(landmask EQ 0)] = 1 & mask[where(landmask EQ 1)] = !VALUES.F_NAN & help, mask
ENDIF


; lecture var
IF varn EQ 'GPI' THEN BEGIN
  IF expn EQ 'ERA-I' THEN BEGIN
    rh600  = read_ncdf('r', /allrecords, filename=path+file, /nostruct)
    rh600  = reform(rh600[*,*,1,*])
    var1   = read_ncdf('u', /allrecords, filename=path+file, /nostruct)
    var2   = read_ncdf('v', /allrecords, filename=path+file, /nostruct)
    shear  = reform(sqrt((var1[*,*,0,*]-var1[*,*,1,*])^2+(var2[*,*,0,*]-var2[*,*,1,*])^2))
    vor850 = read_ncdf('vo', /allrecords, filename=path+file, /nostruct)
    vor850 = reform(vor850[*,*,1,*])
    mpi    = reform(read_ncdf('vpot', /allrecords, filename=path+file, /nostruct))
  ENDIF
  IF expn NE 'ERA-I' THEN BEGIN
    rh600  = read_ncdf('rh600', /allrecords, filename=path+file, /nostruct) * 100. ; (%)
    var1   = read_ncdf('u200', /allrecords, filename=path+file, /nostruct)
    var2   = read_ncdf('u850', /allrecords, filename=path+file, /nostruct)
    var3   = read_ncdf('v200', /allrecords, filename=path+file, /nostruct)
    var4   = read_ncdf('v850', /allrecords, filename=path+file, /nostruct)
    shear  = sqrt((var1 - var2)^2+(var3 - var4)^2)
    vor850 = read_ncdf('vor850', /allrecords, filename=path+file, /nostruct)
    mpi    = reform(read_ncdf('vpot', /allrecords, filename=path+file, /nostruct))
  ENDIF
  help, rh600, shear, vor850, mpi
  vorf = 2*(2*!pi/86400.)*sin(!pi/180.*gphit[firstxt:lastxt,firstyt:lastyt]) & help, vorf
  var  = mpi*0. + !VALUES.F_NAN
  FOR i = 0,11 DO var[*,*,i] = abs(1.e5*(vor850[*,*,i]+vorf))^(3/2.)*(rh600[*,*,i]/50.)^3*(mpi[*,*,i]/70.)^3*(1+0.1*shear[*,*,i])^(-2)
  help, var
ENDIF

IF varn EQ 'RH600' THEN BEGIN
  IF expn EQ 'ERA-I' THEN var = reform(read_ncdf('r', /allrecords, filename=path+file, /nostruct))
  IF expn EQ 'COUPLED_SW2_BMJ' OR expn EQ 'COUPLED_SW2_KF' OR expn EQ 'FORCED_SW2_KF' OR expn EQ 'FORCED_SW2_BMJ' THEN var = read_ncdf('rh600', /allrecords, filename=path+file, /nostruct) * 100. ; (%)
ENDIF

IF varn EQ 'SHEAR' THEN BEGIN
  IF expn EQ 'ERA-I' THEN BEGIN
    var1 = read_ncdf('u', /allrecords, filename=path+file, /nostruct)
    var2 = read_ncdf('v', /allrecords, filename=path+file, /nostruct)
    var  = reform(sqrt((var1[*,*,0,*]-var1[*,*,1,*])^2+(var2[*,*,0,*]-var2[*,*,1,*])^2))
  ENDIF
  IF expn EQ 'COUPLED_SW2_BMJ' OR expn EQ 'COUPLED_SW2_KF' OR expn EQ 'FORCED_SW2_KF' OR expn EQ 'FORCED_SW2_BMJ' THEN BEGIN
    var1 = read_ncdf('u200', /allrecords, filename=path+file, /nostruct)
    var2 = read_ncdf('u850', /allrecords, filename=path+file, /nostruct)
    var3 = read_ncdf('v200', /allrecords, filename=path+file, /nostruct)
    var4 = read_ncdf('v850', /allrecords, filename=path+file, /nostruct)
    var  = sqrt((var1 - var2)^2+(var3 - var4)^2)
  ENDIF
ENDIF

IF varn EQ 'VOR850' THEN BEGIN
  IF expn EQ 'ERA-I' THEN var = reform(read_ncdf('vo', /allrecords, filename=path+file, /nostruct)) * 1000000.
  IF expn EQ 'COUPLED_SW2_BMJ' OR expn EQ 'COUPLED_SW2_KF' OR expn EQ 'FORCED_SW2_KF' OR expn EQ 'FORCED_SW2_BMJ' THEN var = read_ncdf('vor850', /allrecords, filename=path+file, /nostruct) * 1000000.
ENDIF

IF varn EQ 'MPI' THEN  var = reform(read_ncdf('vpot', /allrecords, filename=path+file, /nostruct))
IF varn EQ 'CAPE' THEN var = reform(read_ncdf('cape', /allrecords, filename=path+file, /nostruct))
help, var & STOP
;plt, m_mean(var[*,*,[11,0,1,2]],dim=3,/nan)*mask, /nocont, /realcont & stop


; sauvegarde grille + mask
IF expn EQ 'ERA-I' THEN BEGIN
  pathfile_obs = path+file
  glamt_obs = glamt[firstxt:lastxt,firstyt:lastyt] & help, glamt_obs
  gphit_obs = gphit[firstxt:lastxt,firstyt:lastyt] & help, gphit_obs
  mask_obs = mask & help, mask_obs
  var_obs = var & help, var_obs
ENDIF


; interpolation grille modele -> obs
IF expn EQ 'COUPLED_SW2_BMJ' OR expn EQ 'COUPLED_SW2_KF' OR expn EQ 'FORCED_SW2_KF' OR expn EQ 'FORCED_SW2_BMJ' THEN BEGIN
  glamt_mod = glamt[firstxt:lastxt,firstyt:lastyt] & help, glamt_mod
  gphit_mod = gphit[firstxt:lastxt,firstyt:lastyt] & help, gphit_mod
  var_gridobs = fltarr(n_elements(glamt_obs[*,0]),n_elements(glamt_obs[0,*]),12)
  FOR i = 0, 11 DO var_gridobs[*,*,i] = fromreg('bilinear',var[*,*,i],glamt_mod[*,0],reform(gphit_mod[0,*]),glamt_obs[*,0],reform(gphit_obs[0,*]))
  mask_gridobs = fromreg('bilinear',mask,glamt_mod[*,0],reform(gphit_mod[0,*]),glamt_obs[*,0],reform(gphit_obs[0,*]))
  var = var_gridobs & help, var
ENDIF


; cycle saisonnier 1D par boite
initncdf, pathfile_obs
FOR i = 0, 11 DO BEGIN
  allvar1D_sc_sio[e,i] = moyenne(var[*,*,i]*mask_obs, 'xy', /nan , box=box_sio)
  allvar1D_sc_nio[e,i] = moyenne(var[*,*,i]*mask_obs, 'xy', /nan , box=box_nio)
  allvar1D_sc_ars[e,i] = moyenne(var[*,*,i]*mask_obs, 'xy', /nan , box=box_ars)
  allvar1D_sc_bob[e,i] = moyenne(var[*,*,i]*mask_obs, 'xy', /nan , box=box_bob)
  allvar1D_sc_swi[e,i] = moyenne(var[*,*,i]*mask_obs, 'xy', /nan , box=box_swi)
  allvar1D_sc_sei[e,i] = moyenne(var[*,*,i]*mask_obs, 'xy', /nan , box=box_sei)
ENDFOR


; plots 2D
IF plot_2D THEN BEGIN

  SET_PLOT, 'X'
  DEVICE, decomposed=0, retain=0
  lct, 33
  initncdf, pathfile_obs & domdef, box

  IF varn EQ 'GPI' THEN BEGIN & min=0 & max=10 & ENDIF
  IF varn EQ 'RH600' THEN BEGIN & min=10 & max=80 & ENDIF
  IF varn EQ 'SHEAR' THEN BEGIN & min=0 & max=40 & ENDIF
  IF varn EQ 'VOR850' THEN BEGIN & min=-3 & max=3 & ENDIF

  IF write_ps THEN openps, filename=varn+'_DJFM_'+expn+'.ps'
  plt, smooth(m_mean(var[*,*,[11,0,1,2]],dim=3,/nan),5,/nan)*mask_gridobs, /nocont, /realcont, min=min, max=max, $
  title=varn+' '+expn+' DJFM', xtitle='', ytitle='', format='(F4.1)'
  IF write_ps THEN closeps
  stop

  IF write_ps THEN openps, filename=varn+'_JJAS_'+expn+'.ps'
  plt, smooth(m_mean(var[*,*,[5,6,7,8]],dim=3,/nan),5,/nan)*mask_gridobs, /nocont, /realcont, min=min, max=max, $
  title=varn+' '+expn+' JJAS', xtitle='', ytitle='', format='(F4.1)'
  IF write_ps THEN closeps
  stop

ENDIF

ENDFOR ; exp


; plots 1D
IF plot_1D THEN BEGIN

SET_PLOT, 'X'
DEVICE, decomposed=0, retain=0
lct, 33

month_clim1 = findgen(12) + 1
month_clim4 = findgen(4*12)/4. + 0.25

FOR i = 0, n_elements(bas_list)-1 DO BEGIN

  bas_name = bas_list[i]
 
  IF write_ps THEN openps, filename=varn+'_CLIM_'+bas_name+'.ps'
  IF write_ps THEN thc = 6 ELSE thc = 2

  tmp  = execute('var2plot = allvar1D_sc_'+strlowcase(bas_name)) & help, var2plot
  ymin = floor(min(var2plot))
  ymax = ceil(max(var2plot))

  splot,   month_clim4, smooth(interpol(var2plot[0,*],4*12),3), thick=thc, color=0, charsize = 1.5, xrange=[1,12], xminor=1, xticks=11, xstyle=1, yrange=[ymin,ymax], title=varn+' '+bas_name $
  & oplot, month_clim4, smooth(interpol(var2plot[1,*],4*12),3), thick=thc, color=50  $
  & oplot, month_clim4, smooth(interpol(var2plot[2,*],4*12),3), thick=thc, color=225 $
  ;& oplot, month_clim4, smooth(interpol(var2plot[3,*],4*12),3), thick=thc, color=150 $
  ;& oplot, month_clim4, smooth(interpol(var2plot[4,*],4*12),3), thick=thc, color=100 $
  & xyouts, 0.15, 0.15 , exp_list[0], /normal, color=0  , charsize=1.5 $
  & xyouts, 0.15, 0.125, exp_list[1], /normal, color=50 , charsize=1.5 $
  & xyouts, 0.15, 0.1  , exp_list[2], /normal, color=225, charsize=1.5 
  ;& xyouts, 0.15, 0.075, exp_list[3], /normal, color=150, charsize=2 $
  ;& xyouts, 0.15, 0.05 , exp_list[4], /normal, color=100, charsize=2
  IF write_ps THEN closeps & STOP

ENDFOR

ENDIF


ENDFOR ; varn

STOP
END
