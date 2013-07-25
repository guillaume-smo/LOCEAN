PRO compute_plot_clim_WRF
; compute & plot seasonal cycle or spatial clim fields for all bassins
@common


; PARAMS
; vars: SST, SSH, UV10, RAIN, OLR, RH600, VOR850, SHEAR, MPI, GPI, UVSURF
var_list = ['RAIN']
exp_list = ['OBS','COUPLED_SW2_KF','COUPLED_SW2_BMJ']
bas_list = ['TST']
freq     = 'DAILY' ; frequence des fichiers input "modele"
maskfile = '/usr/home/gslod/IDL/MASK_WRF_IND4_GRIDT.nc'

; NETCDF
gridobs    = 0 ; lecture data modele deja interpolees sur grid obs
interp     = 0 ; interpolation data modele sur grid obs
write_ncdf = 0 ; ecriture data modele regridees sur grid obs
write_clim = 0 ; ecriture data modele clim
write_nosc = 0 ; ecriture data obs sans seasonal cycle

; PLOTS
write_ps   = 0 ; ecriture figures ps
plot_2D    = 0 ; plot 2D
plot_1D    = 0 ; plot 1D
plot_HM    = 0 ; plot Hovmuller


;--------------------------------------------------------------------------------------


; declarations
varsc1D_sio = fltarr(n_elements(exp_list),12) 
varsc1D_nio = fltarr(n_elements(exp_list),12)
persc1D_sio = fltarr(n_elements(exp_list),12,2)
persc1D_nio = fltarr(n_elements(exp_list),12,2)


; loops
;FOR v = 0, n_elements(var_list)-1 DO BEGIN
FOR b = 0, n_elements(bas_list)-1 DO BEGIN
FOR e = 0, n_elements(exp_list)-1 DO BEGIN
FOR v = 0, n_elements(var_list)-1 DO BEGIN

print, '' & print, 'INFOS:'
basn = bas_list[b] & help, basn
varn = var_list[v] & help, varn
expn = exp_list[e]


; obs definitions
IF varn EQ 'SST'    AND expn EQ 'OBS' THEN expn = 'TMI-AMSRE'
IF varn EQ 'SSH'    AND expn EQ 'OBS' THEN expn = 'AVISO'
IF varn EQ 'RAIN'   AND expn EQ 'OBS' THEN expn = 'TRMM3B43'
IF varn EQ 'UV10'   AND expn EQ 'OBS' THEN expn = 'QSCAT'
IF varn EQ 'RH600'  AND expn EQ 'OBS' THEN expn = 'ERA-I'
IF varn EQ 'VOR850' AND expn EQ 'OBS' THEN expn = 'ERA-I'
IF varn EQ 'SHEAR'  AND expn EQ 'OBS' THEN expn = 'ERA-I'
IF varn EQ 'MPI'    AND expn EQ 'OBS' THEN expn = 'ERA-I'
IF varn EQ 'GPI'    AND expn EQ 'OBS' THEN expn = 'ERA-I'
IF varn EQ 'UVSURF' AND expn EQ 'OBS' THEN expn = 'SURCOUF'
help, expn


; path + file
path = '/net/adonis/usr/adonis/varclim/gslod/EXP_'+expn+'/'
IF gridobs THEN BEGIN
  IF expn EQ 'COUPLED_SW2_KF'  THEN file = varn+'_'+freq+'_1990-2009_GRIDOBS.nc'
  IF expn EQ 'FORCED_SW2_KF'   THEN file = varn+'_'+freq+'_1990-2009_GRIDOBS.nc'
  IF expn EQ 'COUPLED_SW2_BMJ' THEN file = varn+'_'+freq+'_1990-2009_GRIDOBS.nc'
  IF expn EQ 'CPL_TDK_V33'     THEN file = varn+'_'+freq+'_1990-1996_GRIDOBS.nc'
  IF expn EQ 'CPL_NSAS_V33'    THEN file = varn+'_'+freq+'_1990-1996_GRIDOBS.nc'
ENDIF ELSE BEGIN
  IF expn EQ 'COUPLED_SW2_KF'  THEN file = varn+'_'+freq+'_1990-2009.nc'
  IF expn EQ 'FORCED_SW2_KF'   THEN file = varn+'_'+freq+'_1990-2009.nc'
  IF expn EQ 'COUPLED_SW2_BMJ' THEN file = varn+'_'+freq+'_1990-2009.nc'
  IF expn EQ 'CPL_TDK_V33'     THEN file = varn+'_'+freq+'_1990-1996.nc'
  IF expn EQ 'CPL_NSAS_V33'    THEN file = varn+'_'+freq+'_1990-1996.nc'
ENDELSE
IF expn EQ 'TROPFLUX'     THEN file = varn+'_DAILY_1989-2009.nc'
IF expn EQ 'TMI-AMSRE'    THEN file = varn+'_DAILY_1998-2009.nc'
IF expn EQ 'TRMM3B43'     THEN file = varn+'_MONTHLY_1998-2009.nc'
IF expn EQ 'CCMP'         THEN file = varn+'_MONTHLY_1990-2009.nc'
IF expn EQ 'QSCAT'        THEN file = varn+'_MONTHLY_2000-2009.nc'
IF expn EQ 'NOAA'         THEN file = varn+'_DAILY_1974-2011.nc'
IF expn EQ 'AVISO'        THEN file = varn+'_WEEKLY_19921014-20091230.nc'
IF expn EQ 'GPCP'         THEN file = varn+'_DAILY_1996-2009.nc'
IF expn EQ 'ERA-I'        THEN file = varn+'_MONTHLY_1990-2009.nc'
IF expn EQ 'SURCOUF'      THEN file = varn+'_MONTHLY_1993-2010.nc'
IF varn EQ 'GPI'          THEN file = 'VOR850_MONTHLY_1990-2009.nc'
help, file


; date_ini
date_ini = 19900101d 
IF expn EQ 'TROPFLUX'  THEN date_ini = 19890101d
IF expn EQ 'TMI-AMSRE' THEN date_ini = 19980101d
IF expn EQ 'TRMM3B43'  THEN date_ini = 19980101d
IF expn EQ 'QSCAT'     THEN date_ini = 20000101d
IF expn EQ 'NOAA'      THEN date_ini = 19740601d
IF expn EQ 'AVISO'     THEN date_ini = 19921014d
IF expn EQ 'GPCP'      THEN date_ini = 19960101d
IF expn EQ 'ERA-I'     THEN date_ini = 19900101d
IF expn EQ 'SURCOUF'   THEN date_ini = 19930101d
juld_ini = date2jul(date_ini)
help, date_ini


; date_end
date_end = 20091231.75d
IF expn EQ 'CPL_TDK_V33'  THEN date_end = 19961231.75d 
IF expn EQ 'CPL_NSAS_V33' THEN date_end = 19961231.75d
IF expn EQ 'SURCOUF'      THEN date_end = 20101231.75d
help, date_end


; bassin
IF basn EQ 'ALL' THEN box = [25,145,-40,35]
IF basn EQ  'IO' THEN box = [30,130,-25,25]
IF basn EQ 'SIO' THEN box = [30,130,-25, 0]
IF basn EQ 'NIO' THEN box = [30,100,  0,25]
IF basn EQ 'BOB' THEN box = [80,100,  0,25]
IF basn EQ 'ARS' THEN box = [50, 80,  0,25]
IF basn EQ 'SWI' THEN box = [40, 80,-25, 0]
IF basn EQ 'SEI' THEN box = [80,120,-25, 0]
IF basn EQ 'SUM' THEN box = [80,100,-10, 5]
IF basn EQ 'ISV' THEN box = [35,130,-22.5,22.5]
IF basn EQ 'HME' THEN box = [40,130, -5, 5] ; hovmuller equatorial
IF basn EQ 'TST' THEN box = [75, 80, -5, 0] 


; initncdf + domdef + mask + definition grille
IF expn EQ 'NOAA' OR expn EQ 'TRMM3B43' OR expn EQ 'GPCP' THEN BEGIN
  initncdf, maskfile & domdef, box
  landmask = read_ncdf('LANDMASK',filename=maskfile,/nostruct) & help, landmask
  mask = landmask & mask[where(landmask EQ 0)] = 1. 
  IF n_elements(where(landmask EQ 1)) GT 1 THEN mask[where(landmask EQ 1)] = !VALUES.F_NAN & help, mask
  glamt_old = glamt[firstxt:lastxt,firstyt:lastyt]
  gphit_old = gphit[firstxt:lastxt,firstyt:lastyt] 
  initncdf, path+file & domdef, box
  glamt_new = glamt[firstxt:lastxt,firstyt:lastyt]
  gphit_new = gphit[firstxt:lastxt,firstyt:lastyt]
  mask = fromreg('bilinear',mask,glamt_old,gphit_old,glamt_new,gphit_new) & help, mask
  initncdf, path+file,XMINMESH=firstxt,YMINMESH=firstyt,XMAXMESH=lastxt,YMAXMESH=lastyt
ENDIF

IF expn EQ 'TROPFLUX' OR expn EQ 'CCMP' OR expn EQ 'QSCAT' OR expn EQ 'TMI-AMSRE' THEN BEGIN
  initncdf, path+file & domdef, box
  initncdf, path+file,XMINMESH=firstxt,YMINMESH=firstyt,XMAXMESH=lastxt,YMAXMESH=lastyt
ENDIF

IF expn EQ 'ERA-I' THEN BEGIN
  initncdf, path+file & domdef, box
  sst  =  read_ncdf('sstk', 0, 0, /timestep, filename=path+'var3D_monthly_clim_1990-2009.nc', /nostruct)
  mask = sst & mask[where(finite(sst) EQ 1)] = 1 & mask[where(finite(sst) EQ 0)] = !VALUES.F_NAN & help, mask
  initncdf, path+file,XMINMESH=firstxt,YMINMESH=firstyt,XMAXMESH=lastxt,YMAXMESH=lastyt
ENDIF

IF expn EQ 'COUPLED_SW2_BMJ' OR expn EQ 'COUPLED_SW2_KF' OR expn EQ 'FORCED_SW2_KF' OR expn EQ 'CPL_TDK_V33' OR expn EQ 'CPL_NSAS_V33' THEN BEGIN
  IF gridobs THEN BEGIN & initncdf, path+file & domdef, box & ENDIF ELSE BEGIN
    initncdf, maskfile & domdef, box
    landmask = read_ncdf('LANDMASK',filename=maskfile,/nostruct) & help, landmask
    mask = landmask & mask[where(landmask EQ 0)] = 1 & mask[where(landmask EQ 1)] = !VALUES.F_NAN & help, mask
  ENDELSE
  initncdf, maskfile,XMINMESH=firstxt,YMINMESH=firstyt,XMAXMESH=lastxt,YMAXMESH=lastyt
ENDIF

IF varn EQ 'SSH' OR varn EQ 'UVSURF' THEN BEGIN
  initncdf, path+file & domdef, box
  initncdf, path+file,XMINMESH=firstxt,YMINMESH=firstyt,XMAXMESH=lastxt,YMAXMESH=lastyt
ENDIF


; lecture data+mask
print, '' & print, 'READING DATA...'
@read_data


; creation axe temps
nbt  = n_elements(var[0,0,*])
IF freq EQ 'DAILY' THEN juld = juld_ini + dindgen(nbt)
IF freq EQ '6H'    THEN juld = juld_ini + dindgen(nbt)/4.
IF expn EQ 'ERA-I' THEN juld = time
IF expn EQ 'GPCP' THEN juld = time
IF expn EQ 'TRMM3B43' THEN juld = time
IF expn EQ 'CCMP' THEN juld = time
IF expn EQ 'NOAA' THEN juld = time
IF expn EQ 'AVISO' THEN juld = juld_ini + dindgen(nbt)*7.
IF expn EQ 'QSCAT' OR varn EQ 'MPI' OR varn EQ 'GPI' OR expn EQ 'SURCOUF'THEN BEGIN
  year = floor(floor(date_ini / 10000) + indgen(nbt)/12)
  month = indgen(nbt) mod 12 + 1
  day = lonarr(nbt) + 1
  date = day + 100 * month + 10000 * year
  juld = date2jul(date)
ENDIF

date    = jul2date(juld, HOUR=hour, DAY=day, MONTH=month, YEAR=year) & help, date
period  = [max([date_ini,19900101d]),min([date_end,20091231.75d])]
IF expn EQ 'AVISO' THEN period  = [19930101d,min([date_end,20091231.75d])]
print, 'PERIOD = ', period
iok     = where(date GE period[0] AND date LE period[1], cntok) & help, iok
var     = var[*,*,iok] & help, var
date    = date[iok] & year = year[iok] & month = month[iok] & day = day[iok] & hour = hour[iok] & juld = juld[iok] & time = juld
nbyear  = long(period[1]/10000) - long(period[0]/10000) + 1  & help, nbyear
nbmonth = nbyear * 12.
;nbmonth = 12-(long(period[0]/100) - long(period[0]/10000)*100)+1 + (nbyear-2)*12 + (long(period[1]/100) - long(period[1]/10000)*100) & help, nbmonth


; PATCH RAIN BUG KF
IF varn EQ 'RAIN' AND expn EQ 'COUPLED_SW2_KF' AND freq EQ '6H' THEN BEGIN
  var[*,*,where(year EQ 1999)] = var[*,*,where(year EQ 1999)] - shift(var[*,*,where(year EQ 1999)],[0,0,1]) >0
  var[*,*,13384] = 0. ; bug restart 1999
  IF interp THEN var_smooth = smooth(var,[5,5,1],/nan) >0 ; smooth sur la grille GPCP avant interpolation
  IF interp THEN var = var_smooth
ENDIF


; sauvegarde grille obs
IF expn EQ 'TROPFLUX' OR expn EQ 'TMI-AMSRE' OR expn EQ 'TRMM3B43' OR expn EQ 'CCMP' OR expn EQ 'SURCOUF' $
OR expn EQ 'QSCAT' OR expn EQ 'NOAA' OR expn EQ 'GPCP' OR expn EQ 'AVISO' OR expn EQ 'ERA-I' THEN BEGIN
  pathfile_obs = path+file
  var_obs   = var & help, var_obs
  glamt_obs = glamt[firstxt:lastxt,firstyt:lastyt] & help, glamt_obs
  gphit_obs = gphit[firstxt:lastxt,firstyt:lastyt] & help, gphit_obs
  mask_obs  = mask[firstxt:lastxt,firstyt:lastyt]  & help, mask_obs
  tmp = execute('glamt_'+strlowcase(varn)+'= glamt_obs & help, glamt_'+strlowcase(varn))
  tmp = execute('gphit_'+strlowcase(varn)+'= gphit_obs & help, gphit_'+strlowcase(varn))
  tmp = execute('mask_' +strlowcase(varn)+'= mask_obs  & help, mask_'+strlowcase(varn))
  tmp = execute('pathfile_' +strlowcase(varn)+'= pathfile_obs& help, pathfile_' +strlowcase(varn))
  IF write_ncdf THEN CONTINUE
ENDIF


; interpolation grille modele vers grille obs
IF expn EQ 'COUPLED_SW2_BMJ' OR expn EQ 'COUPLED_SW2_KF' OR expn EQ 'FORCED_SW2_KF' OR expn EQ 'CPL_TDK_V33' OR expn EQ 'CPL_NSAS_V33' THEN BEGIN

  glamt_mod = glamt[firstxt:lastxt,firstyt:lastyt]
  gphit_mod = gphit[firstxt:lastxt,firstyt:lastyt]
  mask_mod  = mask
  help, var, glamt_mod, gphit_mod, mask_mod

  IF interp THEN BEGIN
    print, '' & print, 'SPATIAL INTERPOLATION GRID-MOD -> GRID-OBS...'
    var_gridobs = fltarr(n_elements(glamt_obs[*,0]),n_elements(glamt_obs[0,*]),cntok)
    FOR i = 0, cntok-1 DO var_gridobs[*,*,i] = fromreg('bilinear',var[*,*,i],glamt_mod[*,0],reform(gphit_mod[0,*]),glamt_obs[*,0],reform(gphit_obs[0,*]))
    var  = var_gridobs
    mask = mask_obs
  ENDIF

  ; ecriture NETCDF
  IF write_ncdf THEN BEGIN
    print, 'WRITING NEW NETCDF FILE...'
    IF interp THEN fid = NCDF_CREATE(path+varn+'_'+freq+'_'+expn+'_GRIDOBS.nc', /CLOBBER) ELSE fid = NCDF_CREATE(path+varn+'_'+freq+'_'+expn+'.nc', /CLOBBER)
    IF interp THEN xid = NCDF_DIMDEF(fid, 'lon', n_elements(glamt_obs[*,0])) ELSE xid = NCDF_DIMDEF(fid, 'lon', n_elements(glamt_mod[*,0]))
    IF interp THEN yid = NCDF_DIMDEF(fid, 'lat', n_elements(gphit_obs[0,*])) ELSE yid = NCDF_DIMDEF(fid, 'lat', n_elements(gphit_mod[0,*]))
    tid = NCDF_DIMDEF(fid, 'time', /UNLIMITED)
    vid1 = NCDF_VARDEF(fid, varn,   [xid, yid, tid], /FLOAT)
    vid2 = NCDF_VARDEF(fid, 'lon',  [xid], /FLOAT)
    vid3 = NCDF_VARDEF(fid, 'lat',  [yid], /FLOAT)
    vid4 = NCDF_VARDEF(fid, 'mask', [xid, yid], /FLOAT)
    vid5 = NCDF_VARDEF(fid, 'time', [tid], /DOUBLE)
    IF varn EQ 'OLR'  THEN NCDF_ATTPUT,  fid, vid1, 'units', 'W/m^2'
    IF varn EQ 'RAIN' THEN NCDF_ATTPUT,  fid, vid1, 'units', 'mm/day'
    NCDF_ATTPUT,  fid, vid2, 'units', 'degrees'
    NCDF_ATTPUT,  fid, vid3, 'units', 'degrees'
    NCDF_ATTPUT,  fid, vid5, 'units', 'days since 1900-01-01 00:00:00'
    NCDF_CONTROL, fid, /ENDEF
    IF interp THEN NCDF_VARPUT, fid, vid1, var_gridobs ELSE NCDF_VARPUT, fid, vid1, var
    IF interp THEN NCDF_VARPUT, fid, vid2, glamt_obs[*,0] ELSE NCDF_VARPUT, fid, vid2, glamt_mod[*,0]
    IF interp THEN NCDF_VARPUT, fid, vid3, reform(gphit_obs[0,*]) ELSE NCDF_VARPUT, fid, vid3, reform(gphit_mod[0,*])
    IF interp THEN NCDF_VARPUT, fid, vid4, mask_obs ELSE NCDF_VARPUT, fid, vid4, mask_mod
    NCDF_VARPUT,  fid, vid5, time-date2jul(19000101.00d)
    NCDF_CLOSE,   fid
    CONTINUE
  ENDIF
ENDIF


;--------------------------------------------------------------------------------------

; calcul spectre
print, '' & print, 'SPECTRUM COMPUTATION...'
var1D = grossemoyenne(var, 'xy', /nan , box=box) & help, var1D
var_sp = spectra(var1D, nbyear, 12, 1, 1, 1) & help, var_sp
splot, var_sp.time, var_sp.spec & STOP


; calcul cycle saisonnier en monthly sur grid OBS
print, '' & print, 'SC2D COMPUTATION...'
var_sc = fltarr(n_elements(var[*,0,0]),n_elements(var[0,*,0]),12) & help, var_sc
IF varn EQ 'UV10' OR varn EQ 'UVSURF' THEN var1_sc = fltarr(n_elements(var[*,0,0]),n_elements(var[0,*,0]),12)
IF varn EQ 'UV10' OR varn EQ 'UVSURF' THEN var2_sc = fltarr(n_elements(var[*,0,0]),n_elements(var[0,*,0]),12)
FOR i = 0, 12-1 DO BEGIN
  iave = where(month EQ i+1) & help, iave
  var_sc[*,*,i] = m_mean(var[*,*,iave],dim=3,/nan) * mask
  IF varn EQ 'UV10' OR varn EQ 'UVSURF' THEN var1_sc[*,*,i] = m_mean(var1[*,*,iave],dim=3,/nan) * mask
  IF varn EQ 'UV10' OR varn EQ 'UVSURF' THEN var2_sc[*,*,i] = m_mean(var2[*,*,iave],dim=3,/nan) * mask
ENDFOR
IF expn EQ 'COUPLED_SW2_BMJ' OR expn EQ 'COUPLED_SW2_KF' OR expn EQ 'FORCED_SW2_KF' OR $
   expn EQ 'CPL_TDK_V33'     OR expn EQ 'CPL_NSAS_V33' THEN BEGIN
  IF interp EQ 0 THEN BEGIN ; si var pas interpolee, alors on interpole uniquement le sc 2D de var
    tmp = execute('glamt_obs = glamt_'+strlowcase(varn))
    tmp = execute('gphit_obs = gphit_'+strlowcase(varn))
    tmp = execute('mask_obs  = mask_'+strlowcase(varn))
    var_sc_gridobs = fltarr(n_elements(glamt_obs[*,0]),n_elements(glamt_obs[0,*]),12)
    IF varn EQ 'UV10'   THEN var1_sc_gridobs = fltarr(n_elements(glamt_uv10[*,0]),n_elements(glamt_uv10[0,*]),12)
    IF varn EQ 'UVSURF' THEN var1_sc_gridobs = fltarr(n_elements(glamt_uvsurf[*,0]),n_elements(glamt_uvsurf[0,*]),12)
    IF varn EQ 'UV10'   THEN var2_sc_gridobs = fltarr(n_elements(glamt_uv10[*,0]),n_elements(glamt_uv10[0,*]),12)
    IF varn EQ 'UVSURF' THEN var2_sc_gridobs = fltarr(n_elements(glamt_uvsurf[*,0]),n_elements(glamt_uvsurf[0,*]),12)
    FOR i = 0, 12-1 DO var_sc_gridobs[*,*,i] = fromreg('bilinear',var_sc[*,*,i],glamt_mod[*,0],reform(gphit_mod[0,*]),glamt_obs[*,0],reform(gphit_obs[0,*]))
    IF varn EQ 'UV10' THEN BEGIN
      var1_sc_gridobs = fltarr(n_elements(glamt_uv10[*,0]),n_elements(glamt_uv10[0,*]),12)
      var2_sc_gridobs = fltarr(n_elements(glamt_uv10[*,0]),n_elements(glamt_uv10[0,*]),12)
      FOR i = 0, 12-1 DO var1_sc_gridobs[*,*,i] = fromreg('bilinear',var1_sc[*,*,i],glamt_mod[*,0],reform(gphit_mod[0,*]),glamt_uv10[*,0],reform(gphit_uv10[0,*]))
      FOR i = 0, 12-1 DO var2_sc_gridobs[*,*,i] = fromreg('bilinear',var2_sc[*,*,i],glamt_mod[*,0],reform(gphit_mod[0,*]),glamt_uv10[*,0],reform(gphit_uv10[0,*]))
      var1_sc = var1_sc_gridobs
      var2_sc = var2_sc_gridobs
    ENDIF
    IF varn EQ 'UVSURF' THEN BEGIN
      var1_sc_gridobs = fltarr(n_elements(glamt_uvsurf[*,0]),n_elements(glamt_uvsurf[0,*]),12)
      var2_sc_gridobs = fltarr(n_elements(glamt_uvsurf[*,0]),n_elements(glamt_uvsurf[0,*]),12)
      FOR i = 0, 12-1 DO var1_sc_gridobs[*,*,i] = fromreg('bilinear',var1_sc[*,*,i],glamt_mod[*,0],reform(gphit_mod[0,*]),glamt_uvsurf[*,0],reform(gphit_uvsurf[0,*]))
      FOR i = 0, 12-1 DO var2_sc_gridobs[*,*,i] = fromreg('bilinear',var2_sc[*,*,i],glamt_mod[*,0],reform(gphit_mod[0,*]),glamt_uvsurf[*,0],reform(gphit_uvsurf[0,*]))
      var1_sc = var1_sc_gridobs
      var2_sc = var2_sc_gridobs
    ENDIF
    var_sc  = var_sc_gridobs
    mask = mask_obs
  ENDIF
ENDIF
help, var_sc


; ecriture netcdf fichier clim en monthly sur grid obs
IF write_clim AND (expn EQ 'COUPLED_SW2_BMJ' OR expn EQ 'COUPLED_SW2_KF' OR expn EQ 'FORCED_SW2_KF' OR expn EQ 'CPL_TDK_V33' OR expn EQ 'CPL_NSAS_V33') THEN BEGIN
  print, '' & print, 'WRITING NEW NETCDF FILE...'
  time_clim = 19000101d + dindgen(12)*100.
  fid = NCDF_CREATE(path+varn+'_MONTHLY_CLIM_'+expn+'_GRIDOBS.nc', /CLOBBER)
  xid = NCDF_DIMDEF(fid, 'lon', n_elements(glamt_obs[*,0]))
  yid = NCDF_DIMDEF(fid, 'lat', n_elements(gphit_obs[0,*]))
  tid = NCDF_DIMDEF(fid, 'time', /UNLIMITED)
  vid1 = NCDF_VARDEF(fid, varn,   [xid, yid, tid], /FLOAT)
  vid2 = NCDF_VARDEF(fid, 'lon',  [xid], /FLOAT)
  vid3 = NCDF_VARDEF(fid, 'lat',  [yid], /FLOAT)
  vid4 = NCDF_VARDEF(fid, 'mask', [xid, yid], /FLOAT)
  vid5 = NCDF_VARDEF(fid, 'time', [tid], /DOUBLE)
  IF varn EQ 'OLR'  THEN NCDF_ATTPUT,  fid, vid1, 'units', 'W/m^2'
  IF varn EQ 'RAIN' THEN NCDF_ATTPUT,  fid, vid1, 'units', 'mm/day'
  NCDF_ATTPUT, fid, vid2, 'units', 'degrees'
  NCDF_ATTPUT, fid, vid3, 'units', 'degrees'
  NCDF_ATTPUT, fid, vid5, 'units', 'days since 1900-01-01 00:00:00'
  NCDF_CONTROL,fid, /ENDEF
  NCDF_VARPUT, fid, vid1, var_sc_gridobs
  NCDF_VARPUT, fid, vid2, glamt_obs[*,0]
  NCDF_VARPUT, fid, vid3, reform(gphit_obs[0,*])
  NCDF_VARPUT, fid, vid4, mask_obs
  NCDF_VARPUT, fid, vid5, time_clim-date2jul(19000101.00d)
  NCDF_CLOSE,  fid
ENDIF
tmp = execute(strlowcase(varn)+'_sc = var_sc & help, '+strlowcase(varn)+'_sc')
tmp = execute('mask_'+strlowcase(varn)+' = mask & help, mask_'+strlowcase(varn))


; serie temporelle 1D cycle saisonnier monthly sur grid OBS
print, '' & print, 'SC1D COMPUTATION...'
initncdf, pathfile_obs & domdef, box
initncdf, pathfile_obs,XMINMESH=firstxt,YMINMESH=firstyt,XMAXMESH=lastxt,YMAXMESH=lastyt

FOR i = 0, 11 DO BEGIN
  IF expn EQ 'TMI-AMSRE' AND i EQ 3 THEN BEGIN & tmp = var_sc[*,*,i] & tmp[where(finite(var_sc[*,*,2]) EQ 0)] = !VALUES.F_NAN & var_sc[*,*,i] = tmp & ENDIF
  varsc1D_sio[e,i] = moyenne(var_sc[*,*,i]*mask, 'xy', /nan , box=[30,130,-25, 0])
  varsc1D_nio[e,i] = moyenne(var_sc[*,*,i]*mask, 'xy', /nan , box=[30,100,  0,25])

  ; quartiles
  IF interp THEN BEGIN
    iave = where(month EQ i+1, nbave) & help, iave
    tmp1 = fltarr(nbave)
    tmp2 = fltarr(nbave)
    FOR j = 0, nbave-1 DO BEGIN
      tmp1[j] = moyenne(var[*,*,iave[j]]*mask, 'xy', /nan , box=[30,130,-25, 0])
      tmp2[j] = moyenne(var[*,*,iave[j]]*mask, 'xy', /nan , box=[30,100,  0,25])
    ENDFOR
    persc1D_sio[e,i,*] = percentile(tmp1,0.25)
    persc1D_nio[e,i,*] = percentile(tmp2,0.25)
  ENDIF
ENDFOR
help, varsc1D_sio, varsc1D_nio



; hovmuller cycle saisonnier
  print, '' & print, 'HOVMULLER COMPUTATION...'
  var_hm = fltarr(n_elements(var_sc[*,0]),12) & help, var_hm
  msk_hm = fltarr(n_elements(mask[*,0]),12) & help, msk_hm
  IF varn EQ 'UV10' OR varn EQ 'UVSURF' THEN var1_hm = fltarr(n_elements(var1_sc[*,0]),12)
  IF varn EQ 'UV10' OR varn EQ 'UVSURF' THEN var2_hm = fltarr(n_elements(var2_sc[*,0]),12)
  iave = where(gphit_obs[0,*] GE box[2] AND gphit_obs[0,*] LE box[3]) & help, iave
  ieq  = (where(round(gphit_obs[0,*]) EQ 0))[0] & help, ieq

  FOR i = 0, n_elements(glamt_obs[*,ieq])-1 DO BEGIN
    imsk = n_elements(where(finite(mask[i,iave]) EQ 0)); & print, glamt_obs[i,ieq], imsk
    IF imsk LT n_elements(iave)/2. THEN var_hm[i,*] = m_mean(var_sc[i,iave,*],dim=2,/nan) ELSE var_hm[i,*] = !VALUES.F_NAN
    IF imsk LT n_elements(iave)/2. THEN msk_hm[i,*] = 1. ELSE msk_hm[i,*] = !VALUES.F_NAN
    IF varn EQ 'UV10' OR varn EQ 'UVSURF' THEN BEGIN
      IF imsk[0] LT n_elements(iave)/2. THEN var1_hm[i,*] = m_mean(smooth(var1_sc[i,iave,*],3,/nan),dim=2,/nan) ELSE var1_hm[i,*] = !VALUES.F_NAN
      IF imsk[0] LT n_elements(iave)/2. THEN var2_hm[i,*] = m_mean(smooth(var2_sc[i,iave,*],3,/nan),dim=2,/nan) ELSE var2_hm[i,*] = !VALUES.F_NAN
    ENDIF
; version non maskee
;    var_hm[i,*] = m_mean(var_sc[i,iave,*],dim=2,/nan)
;    IF varn EQ 'UV10' THEN var1_hm[i,*] = m_mean(var1_sc[i,iave,*],dim=2,/nan)
;    IF varn EQ 'UV10' THEN var2_hm[i,*] = m_mean(var2_sc[i,iave,*],dim=2,/nan)
  ENDFOR

  
; sauvegarde variables hovmuller
IF varn EQ 'SST'  THEN BEGIN & sst_hm = var_hm & msk_sst = msk_hm & ENDIF
IF varn EQ 'SSH'  THEN BEGIN & ssh_hm = var_hm & msk_ssh = msk_hm & ENDIF
IF varn EQ 'UV10' THEN BEGIN & u10_hm = var1_hm & msk_uv10 = msk_hm & ENDIF
IF varn EQ 'UV10' THEN v10_hm = var2_hm
IF varn EQ 'UVSURF' THEN BEGIN & usurf_hm = var1_hm & msk_uvsurf = msk_hm & ENDIF
IF varn EQ 'UVSURF' THEN vsurf_hm = var2_hm
IF varn EQ 'RAIN' THEN BEGIN & rain_hm = var_hm & msk_rain = msk_hm & ENDIF


CONTINUE


; calcul cycle saisonnier en daily
  var_clim_daily     = fltarr(n_elements(var[*,0,0]),n_elements(var[0,*,0]),365)
  var_clim_daily_bis = fltarr(n_elements(var[*,0,0]),n_elements(var[0,*,0]),366)
  date_clim          = jul2date(date2jul(period[0]) + dindgen(365), DAY=day_clim, MONTH=month_clim)
  FOR i = 0, 365-1 DO BEGIN
    iave = where(month EQ month_clim[i] AND day EQ day_clim[i]); & help, iave
    var_clim_daily[*,*,i] = m_mean(var[*,*,iave],dim=3,/nan) * mask
  ENDFOR
  var_clim_daily_bis[*,*,0:364] = var_clim_daily & var_clim_daily_bis[*,*,365] = var_clim_daily[*,*,364]


; remove daily seasonal cycle
  var_nosc = var
  FOR i = 0, nbyear-1 DO BEGIN
    cury = long(period[0]/10000) + i
    iok  = where(year EQ cury, cntok); & help, i, cury, iok
    IF cntok EQ 365 THEN var_nosc[*,*,iok] = var[*,*,iok] - var_clim_daily
    IF cntok EQ 366 THEN var_nosc[*,*,iok] = var[*,*,iok] - var_clim_daily_bis
  ENDFOR


; ecriture fichier data OBS sans daily seasonal cycle
  IF write_nosc THEN BEGIN
    print, 'WRITING NEW NETCDF FILE...'
    fid = NCDF_CREATE(path+varn+'_NOSC_'+freq+'_'+expn+'.nc', /CLOBBER)
    xid = NCDF_DIMDEF(fid, 'lon', n_elements(glamt_obs[*,0]))
    yid = NCDF_DIMDEF(fid, 'lat', n_elements(gphit_obs[0,*]))
    tid = NCDF_DIMDEF(fid, 'time', /UNLIMITED)
    vid1 = NCDF_VARDEF(fid, varn,   [xid, yid, tid], /FLOAT)
    vid2 = NCDF_VARDEF(fid, 'lon',  [xid], /FLOAT)
    vid3 = NCDF_VARDEF(fid, 'lat',  [yid], /FLOAT)
    vid4 = NCDF_VARDEF(fid, 'mask', [xid, yid], /FLOAT)
    vid5 = NCDF_VARDEF(fid, 'time', [tid], /DOUBLE)
    IF varn EQ 'OLR'  THEN NCDF_ATTPUT,  fid, vid1, 'units', 'W/m^2'
    IF varn EQ 'RAIN' THEN NCDF_ATTPUT,  fid, vid1, 'units', 'mm/day'
    IF varn EQ 'SST'  THEN NCDF_ATTPUT,  fid, vid1, 'units', 'K'
    NCDF_ATTPUT,  fid, vid2, 'units', 'degrees'
    NCDF_ATTPUT,  fid, vid3, 'units', 'degrees'
    NCDF_ATTPUT,  fid, vid5, 'units', 'days since 1900-01-01 00:00:00'
    NCDF_CONTROL, fid, /ENDEF
    NCDF_VARPUT, fid, vid1, var_nosc
    NCDF_VARPUT, fid, vid2, glamt_obs[*,0]
    NCDF_VARPUT, fid, vid3, reform(gphit_obs[0,*])
    NCDF_VARPUT, fid, vid4, mask_obs
    NCDF_VARPUT,  fid, vid5, time-date2jul(19000101.00d)
    NCDF_CLOSE,   fid
    STOP & CONTINUE
  ENDIF


; intraseasonal variability
  var_isv = time_filter(var_nosc,juld,30,90)
  var_isv_stddev = fltarr(n_elements(var[*,0,0]),n_elements(var[0,*,0]))
  var_isv_stddev_djfm = fltarr(n_elements(var[*,0,0]),n_elements(var[0,*,0]))
  var_isv_stddev_jjas = fltarr(n_elements(var[*,0,0]),n_elements(var[0,*,0]))
  var_isv_var = fltarr(n_elements(var[*,0,0]),n_elements(var[0,*,0]))
  var_isv_var_djfm = fltarr(n_elements(var[*,0,0]),n_elements(var[0,*,0]))
  var_isv_var_jjas = fltarr(n_elements(var[*,0,0]),n_elements(var[0,*,0]))
  ijjas = where(month EQ  6 OR month EQ 7 OR month EQ 8 OR month EQ 9) & help, ijjas
  idjfm = where(month EQ 12 OR month EQ 1 OR month EQ 2 OR month EQ 3) & help, idjfm
  FOR i = 0, n_elements(var[*,0,0])-1 DO BEGIN
  FOR j = 0, n_elements(var[0,*,0])-1 DO BEGIN
    var_isv_stddev[i,j] = stddev(var_isv[i,j,*], /nan)
    var_isv_stddev_djfm[i,j] = stddev(var_isv[i,j,idjfm], /nan) 
    var_isv_stddev_jjas[i,j] = stddev(var_isv[i,j,ijjas], /nan)
    var_isv_var[i,j] = variance(var_isv[i,j,*], /nan)
    var_isv_var_djfm[i,j] = variance(var_isv[i,j,idjfm], /nan)
    var_isv_var_jjas[i,j] = variance(var_isv[i,j,ijjas], /nan)
  ENDFOR
  ENDFOR
  help, var_isv_stddev


ENDFOR ; var
print, '' & print, 'EXIT VAR-LOOP...'


;--------------------------------------------------------------------------------------

; PLOTS
set_plot, 'X'
device, retain=0, decomposed=0


; PLOTS HOVMULLER USURF-U10
IF plot_HM THEN BEGIN

  set_plot, 'X'
  device, retain=0, decomposed=0

  print, '' & print, 'PLOT HOVMULLER USURF+U10...'
  initncdf, pathfile_uvsurf & jpt = 12 & domdef, box & help, usurf_hm
  pltt, usurf_hm, 'xt', -0.5, 0.5, /nocont, lct=64, ytickname=['JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC'], win=0

  initncdf, pathfile_uv10 & jpt = 12 & domdef, box & help, u10_hm
  pltt, u10_hm, 'xt', -5, 5, /nocont, lct=64, ytickname=['JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC'], win=1
  initncdf, pathfile_uvsurf & jpt = 12 & domdef, box
;  velovect, usurf_hm, vsurf_hm, color=0, missing=!VALUES.F_NAN, /dots, length=10, /overplot
;  ajoutvectt, {u:{a:usurf_hm, g:'T'}, v:{a:vsurf_hm, g:'T'}}, normeref=0.5, cmref=1, vectthick=2;, unvectsur = [1,1]
STOP 


  ; interpolation courant sur la grille "hovmuller" vent
  initncdf, pathfile_uv10 & jpt = 12 & domdef, box
  usurf_hm_int = fltarr(nxt,12)
  vsurf_hm_int = fltarr(nxt,12)
  FOR t = 0, 12-1 DO BEGIN
    usurf_hm_int[*,t] = interpol(usurf_hm[*,t],nxt)
    vsurf_hm_int[*,t] = interpol(vsurf_hm[*,t],nxt)
  ENDFOR
  usurf_hm_int = usurf_hm_int * msk_uv10 & help, usurf_hm_int
  vsurf_hm_int = vsurf_hm_int * msk_uv10 & help, vsurf_hm_int

  IF write_ps THEN openps, filename='HOV_USURF_U10_CLIM_'+expn
  pltt, u10_hm, 'xt', -5, 5, lct=64, $
  vecteur={u:{a:usurf_hm_int, g:'T'}, v:{a:vsurf_hm_int, g:'T'}}, normeref=0.5, cmref=1, vectthick=2, $
;  pltt, usurf_hm_int, 'xt', -0.5, 0.5, lct=64, $
;  contour=u10_hm, contmin=-6, contmax=6, contint=1, $
;  c_thick=[4,1,1,4,1,1,4,1,1,4,1,1,4], c_label=[1,0,0,1,0,0,1,0,0,1,0,0,1], c_lines=[2,2,2,2,2,2,0,0,0,0,0,0,0], $
  title=expn+' USURF + U10 - HOVMULLER', xtitle='', ytitle='', format='(F5.1)', $
  ytickname = ['JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC'], win=2
  IF write_ps THEN closeps & STOP

;  initncdf, pathfile_sst & jpt = 12 & domdef, box
;  IF write_ps THEN openps, filename='HOV_SST_RAIN_CLIM_'+expn
;  pltt, smooth(sst_hm,3,/nan)*msk_sst, 'xt', 25.,31., lct=33, $
;  contour=smooth(rain_hm,3,/nan)*msk_rain, contint=1., contmin=0., contmax=10., $
;  c_thick=[4,1,1,4,1,1,4,1,1,4,1], c_label=[1,0,0,1,0,0,1,0,0,1,0], $
;  title=expn+' SST + RAIN - HOVMULLER', xtitle='', ytitle='', format='(F4.1)', $
;  ytickname = ['JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC']
;  IF write_ps THEN closeps & STOP

ENDIF


; PLOTS HOVMULLER SSH-U10
;IF plot_HM THEN BEGIN

;  initncdf, pathfile_ssh & jpt = 12 & domdef, box
;  pltt, ssh_hm, 'xt', -15, 15, lct=64 & STOP

;  initncdf, pathfile_uv10 & jpt = 12 & domdef, box
;  pltt, u10_hm, 'xt', -5, 5, lct=64 & STOP

;  initncdf, pathfile_uv10 & jpt = 12 & domdef, box
;  IF write_ps THEN openps, filename='HOV_SSH_U10_CLIM_'+expn
;  pltt, ssh_hm[0:n_elements(u10_hm[*,0])-1,*], 'xt', -15, 15, lct=64, $
;  contour=u10_hm, contmin=-6, contmax=6, contint=1, $
;  c_thick=[4,1,1,4,1,1,4,1,1,4,1,1,4], c_label=[1,0,0,1,0,0,1,0,0,1,0,0,1], c_lines=[2,2,2,2,2,2,0,0,0,0,0,0,0], $
;  title=expn+' SSH + U10 - HOVMULLER', xtitle='', ytitle='', format='(F5.1)', $
;  ytickname = ['JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC']
;  IF write_ps THEN closeps & STOP

;  initncdf, pathfile_sst & jpt = 12 & domdef, box
;  IF write_ps THEN openps, filename='HOV_SST_RAIN_CLIM_'+expn
;  pltt, smooth(sst_hm,3,/nan)*msk_sst, 'xt', 25.,31., lct=33, $
;  contour=smooth(rain_hm,3,/nan)*msk_rain, contint=1., contmin=0., contmax=10., $
;  c_thick=[4,1,1,4,1,1,4,1,1,4,1], c_label=[1,0,0,1,0,0,1,0,0,1,0], $
;  title=expn+' SST + RAIN - HOVMULLER', xtitle='', ytitle='', format='(F4.1)', $
;  ytickname = ['JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC']
;  IF write_ps THEN closeps & STOP

;ENDIF


; PLOTS 2D
IF plot_2D THEN BEGIN

  lct,33
  initncdf, pathfile_obs & domdef, box

;  IF write_ps THEN openps, filename='SST_UV10_DJFM_'+expn+'.eps'
;  plt, smooth(m_mean(sst_sc[*,*,[11,0,1,2]],dim=3,/nan),5,/nan)*mask_sst, min=24, max=31, int=0.25, /realcont, lct=33, /nocont, $
;  title=expn+' SST + UV10 - DJFM', xtitle='', ytitle='', format='(F4.1)'
;  initncdf, pathfile_uv10 & domdef, box
;  ajoutvect, {u:{a:m_mean(u10_sc[*,*,[11,0,1,2]],dim=3,/nan), g:'T'}, v:{a:m_mean(v10_sc[*,*,[11,0,1,2]],dim=3,/nan), g:'T'}}, unvectsur = [20,20], normeref=10, cmref=1, vectthick=2 & stop
;  IF write_ps THEN closeps


;  IF write_ps THEN openps, filename='SST_UV10_JJAS_'+expn+'.eps'
;  plt, smooth(m_mean(sst_sc[*,*,[5,6,7,8]],dim=3,/nan),5,/nan)*mask_sst, min=24, max=31, int=0.25, /realcont, lct=33, /nocont, $
;  title=expn+' SST + UV10 - JJAS', xtitle='', ytitle='', format='(F4.1)'
;  initncdf, pathfile_uv10 & domdef, box
;  ajoutvect, {u:{a:m_mean(u10_sc[*,*,[5,6,7,8]],dim=3,/nan), g:'T'}, v:{a:m_mean(v10_sc[*,*,[5,6,7,8]],dim=3,/nan), g:'T'}}, unvectsur = [20,20], normeref=10, cmref=1, vectthick=2 & stop
;  IF write_ps THEN closeps
;  CONTINUE

  IF write_ps THEN openps, filename='SST_RAIN_DJFM_'+expn
  plt, smooth(m_mean(sst_sc[*,*,[11,0,1,2]],dim=3,/nan),7,/nan)*mask_sst, min=24, max=31, int=0.25, /realcont, lct=33, $
  contour=smooth(m_mean(rain_sc[*,*,[11,0,1,2]],dim=3,/nan),7,/nan)*mask_rain, contintervalle=2., contmin=0., contmax=20., $
  c_thick=[1,1,4,1,1,4,1,1,4,1,1], c_label=[0,0,1,0,0,1,0,0,1,0,0], $
  title=expn+' SST + RAIN - DJFM', xtitle='', ytitle='', format='(F4.1)'
  IF write_ps THEN closeps & stop


  IF write_ps THEN openps, filename='SST_RAIN_JJAS_'+expn
  plt, smooth(m_mean(sst_sc[*,*,[5,6,7,8]],dim=3,/nan),7,/nan)*mask_sst, min=24, max=31, int=0.25, /realcont, lct=33, $
  contour=smooth(m_mean(rain_sc[*,*,[5,6,7,8]],dim=3,/nan),7,/nan)*mask_rain, contintervalle=2., contmin=0., contmax=20., $
  c_thick=[1,1,4,1,1,4,1,1,4,1,1], c_label=[0,0,1,0,0,1,0,0,1,0,0], $
  title=expn+' SST + RAIN - JJAS', xtitle='', ytitle='', format='(F4.1)'
  IF write_ps THEN closeps & stop


;  IF write_ps THEN openps, filename='SSH_UV10_DJFM_'+expn+'.eps'
;  initncdf, pathfile_ssh & domdef, box
;  plt, smooth(m_mean(ssh_sc[*,*,[11,0,1,2]],dim=3,/nan),5,/nan)*mask_ssh, min=-20, max=20, /realcont, lct=64, /nocont, $
;  title=expn+' SSH + UV10 - DJFM', xtitle='', ytitle='', format='(I3)'
;  initncdf, pathfile_uv10 & domdef, box
;  ajoutvect, {u:{a:m_mean(u10_sc[*,*,[11,0,1,2]],dim=3,/nan), g:'T'}, v:{a:m_mean(v10_sc[*,*,[11,0,1,2]],dim=3,/nan), g:'T'}}, unvectsur = [20,20], normeref=10, cmref=1, vectthick=2 & stop
;  IF write_ps THEN closeps


;  IF write_ps THEN openps, filename='SSH_UV10_JJAS_'+expn+'.eps'
;  initncdf, pathfile_ssh & domdef, box
;  plt, smooth(m_mean(ssh_sc[*,*,[5,6,7,8]],dim=3,/nan),5,/nan)*mask_ssh, min=-20, max=20, /realcont, lct=64, /nocont, $
;  title=expn+' SSH + UV10 - JJAS', xtitle='', ytitle='', format='(I3)'
;  initncdf, pathfile_uv10 & domdef, box
;  ajoutvect, {u:{a:m_mean(u10_sc[*,*,[5,6,7,8]],dim=3,/nan), g:'T'}, v:{a:m_mean(v10_sc[*,*,[5,6,7,8]],dim=3,/nan), g:'T'}}, unvectsur = [20,20], normeref=10, cmref=1, vectthick=2 & stop
;  IF write_ps THEN closeps


ENDIF

ENDFOR ; exp
print, '' & print, 'EXIT EXP-LOOP...'


; PLOTS 1D
IF plot_1D THEN BEGIN

month_clim1 = findgen(12) + 1
month_clim4 = findgen(4*12)/4. + 0.25

IF write_ps THEN openps, filename=varn+'_CLIM_SIO'
IF write_ps THEN thc = 4 ELSE thc = 2
ymin = floor(min(persc1D_sio))
ymax = ceil(max(persc1D_sio))
splot, month_clim4, smooth(interpol(varsc1D_sio[0,*],4*12),3), thick=thc, color=0, charsize = 1.5, xrange=[1,12], xstyle=1, yrange=[ymin,ymax], title=varn+' SIO' $
& oplot, month_clim4, smooth(interpol(varsc1D_sio[1,*],4*12),3), thick=thc, color=50  $
& oplot, month_clim4, smooth(interpol(varsc1D_sio[2,*],4*12),3), thick=thc, color=225
;& oplot, month_clim4, smooth(interpol(varsc1D_sio[3,*],4*12),3), thick=thc, color=150 $
;& oplot, month_clim4, smooth(interpol(varsc1D_sio[4,*],4*12),3), thick=thc, color=100 $

oplot, month_clim4, smooth(interpol(persc1D_sio[0,*,0],4*12),3), linestyle=2, thick=thc-1, color=0
oplot, month_clim4, smooth(interpol(persc1D_sio[0,*,1],4*12),3), linestyle=2, thick=thc-1, color=0
oplot, month_clim4, smooth(interpol(persc1D_sio[1,*,0],4*12),3), linestyle=2, thick=thc-1, color=50
oplot, month_clim4, smooth(interpol(persc1D_sio[1,*,1],4*12),3), linestyle=2, thick=thc-1, color=50
oplot, month_clim4, smooth(interpol(persc1D_sio[2,*,0],4*12),3), linestyle=2, thick=thc-1, color=225
oplot, month_clim4, smooth(interpol(persc1D_sio[2,*,1],4*12),3), linestyle=2, thick=thc-1, color=225

& xyouts, 0.15, 0.15 , exp_list[0], /normal, color=0  , charsize=2 $
& xyouts, 0.15, 0.125, exp_list[1], /normal, color=50 , charsize=2 $
& xyouts, 0.15, 0.1  , exp_list[2], /normal, color=225, charsize=2 
;& xyouts, 0.15, 0.075, exp_list[3], /normal, color=150, charsize=2 $
;& xyouts, 0.15, 0.05 , exp_list[4], /normal, color=100, charsize=2
IF write_ps THEN closeps & STOP


IF write_ps THEN openps, filename=varn+'_CLIM_NIO'
IF write_ps THEN thc = 4 ELSE thc = 2
ymin = floor(min(varsc1D_nio))
ymax = ceil(max(varsc1D_nio))
splot, month_clim4, smooth(interpol(varsc1D_nio[0,*],4*12),3), thick=thc, color=0, charsize = 1.5, xrange=[1,12], xstyle=1, yrange=[ymin,ymax], title=varn+' NIO' $
& oplot, month_clim4, smooth(interpol(varsc1D_nio[1,*],4*12),3), thick=thc, color=50 $
& oplot, month_clim4, smooth(interpol(varsc1D_nio[2,*],4*12),3), thick=thc, color=225 $ 
;& oplot, month_clim4, smooth(interpol(varsc1D_nio[3,*],4*12),3), thick=thc, color=150 $
;& oplot, month_clim4, smooth(interpol(varsc1D_nio[4,*],4*12),3), thick=thc, color=100 $
& xyouts, 0.15, 0.15 , exp_list[0], /normal, color=0  , charsize=2 $
& xyouts, 0.15, 0.125, exp_list[1], /normal, color=50 , charsize=2 $
& xyouts, 0.15, 0.1  , exp_list[2], /normal, color=225, charsize=2 
;& xyouts, 0.15, 0.075, exp_list[3], /normal, color=150, charsize=2 $
;& xyouts, 0.15, 0.05 , exp_list[4], /normal, color=100, charsize=2
IF write_ps THEN closeps & STOP


ENDIF


ENDFOR ; basin
print, '' & print, 'EXIT BASIN-LOOP...'

;ENDFOR ; var
;print, '' & print, 'EXIT VAR-LOOP...'



STOP
END
