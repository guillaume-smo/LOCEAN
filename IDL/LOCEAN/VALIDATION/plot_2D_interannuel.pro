PRO plot_2D_interannuel
; plot composite 2D interannuel
@common

; vars: SST, RAIN, SSH, UV10
var_list = ['SST','RAIN']
exp_list = ['OBS','COUPLED_SW2_KF','COUPLED_SW2_BMJ']
bas_list = ['IO']

maskfile   = '/usr/home/gslod/IDL/MASK_WRF_IND4_GRIDT.nc'
write_ps   = 1 ; ecriture figures ps
plot_2D    = 1 ; plot 2D
plot_1D    = 0 ; plot 1D


;--------------------------------------------------------------------------------------


FOR e = 0, n_elements(exp_list)-1 DO BEGIN
FOR v = 0, n_elements(var_list)-1 DO BEGIN
FOR b = 0, n_elements(bas_list)-1 DO BEGIN

  varn = var_list[v] & help, varn
  expn = exp_list[e] & help, expn
  basn = bas_list[b] & help, basn


; bassins
  IF basn EQ 'IO'  THEN box = [30,130,-25,25]
  IF basn EQ 'SIO' THEN box = [30,130,-30, 0]
  IF basn EQ 'NIO' THEN box = [60,100,  0,25]
  IF basn EQ 'BOB' THEN box = [80,100,  0,25]
  IF basn EQ 'ARS' THEN box = [60, 80,  0,25]
  IF basn EQ 'SWI' THEN box = [40, 80,-25, 0]
  IF basn EQ 'SEI' THEN box = [80,120,-25, 0]


; IOD years
  IF expn NE 'COUPLED_SW2_BMJ' AND expn NE 'COUPLED_SW2_KF' THEN BEGIN
    iodp_year = [1982,1983,1987,1991,1994,1997,2002]
    iodn_year = [1980,1981,1984,1992,1996,1998]
  ENDIF

  IF expn EQ 'COUPLED_SW2_KF' THEN BEGIN
    iodp_year = [1990,1994,1997,1999,2002]
    iodn_year = [1991,1992,1996,1998,2001]
  ENDIF

  IF expn EQ 'COUPLED_SW2_BMJ' THEN BEGIN
    iodp_year = [1997,2002,2006]
    iodn_year = [1996,1998,2003]
  ENDIF


; ENSO years
  ensop_year = [1991,1997,2002,2006];2009]
  enson_year = [1998,1999,2007];,2010]


; path+file
  IF varn EQ 'SSH'  AND expn EQ 'OBS' THEN expn = 'AVISO'
  IF varn EQ 'RAIN' AND expn EQ 'OBS' THEN expn = 'GPCP_MONTHLY'
  IF varn EQ 'SST'  AND expn EQ 'OBS' THEN expn = 'ERA-I'
  IF varn EQ 'UV10' AND expn EQ 'OBS' THEN expn = 'ERA-I'
  path = '/usr/adonis/varclim/gslod/EXP_'+expn+'/' & help, path

  IF varn EQ 'SSH' THEN BEGIN
    IF expn EQ 'COUPLED_SW2_KF' OR expn EQ 'COUPLED_SW2_BMJ' THEN file = 'SSH_DAILY_1990-2009.nc'
    IF expn EQ 'AVISO' THEN file = 'SSHANO_WEEKLY_19921014-20091230.nc'
  ENDIF
  IF varn EQ 'RAIN' THEN BEGIN
    IF expn EQ 'COUPLED_SW2_KF' OR expn EQ 'COUPLED_SW2_BMJ' THEN file = 'RAIN_DAILY_1990-2009.nc'
    IF expn EQ 'GPCP_MONTHLY' THEN file = 'RAIN_MONTHLY_1979-2010.nc' 
  ENDIF
  IF varn EQ 'SST' THEN BEGIN
    IF expn EQ 'COUPLED_SW2_KF' OR expn EQ 'COUPLED_SW2_BMJ' THEN file = 'SST_DAILY_1990-2009.nc'
    IF expn EQ 'ERA-I' THEN file = 'SST_MONTHLY_1990-2009.nc'
  ENDIF
  IF varn EQ 'UV10' THEN BEGIN
    IF expn EQ 'COUPLED_SW2_KF' OR expn EQ 'COUPLED_SW2_BMJ' THEN file = 'UV10_DAILY_1990-2009.nc'
    IF expn EQ 'ERA-I' THEN file = 'UV10_MONTHLY_1990-2009.nc'
  ENDIF


; date
  date_ini = 19900101d
  IF expn EQ 'ERA-I' THEN date_ini = 19900101d
  IF expn EQ 'AVISO' THEN date_ini = 19921014d
  IF expn EQ 'GPCP_MONTHLY' THEN date_ini = 19780101d
  date_end = 20091231.75d
  help, date_ini, date_end

; init
  IF expn EQ 'COUPLED_SW2_KF' OR expn EQ 'COUPLED_SW2_BMJ' THEN BEGIN
    IF varn NE 'SSH' THEN initncdf, '/usr/home/gslod/IDL/MASK_WRF_IND4_GRIDT.nc' ELSE initncdf, path+file
  ENDIF ELSE BEGIN 
    initncdf, path+file
  ENDELSE
  domdef, box

; var + mask
  IF expn EQ 'ERA-I' THEN mask = read_ncdf('SST', 0, 0, /timestep, filename=path+'SST_MONTHLY_1990-2009.nc', /nostruct)*0. + 1.
  IF expn EQ 'COUPLED_SW2_BMJ' OR expn EQ 'COUPLED_SW2_KF' THEN BEGIN  
    landmask = read_ncdf('LANDMASK',filename='/usr/home/gslod/IDL/MASK_WRF_IND4_GRIDT.nc',/nostruct)
    mask = landmask & mask[where(landmask EQ 0)] = 1 & mask[where(landmask EQ 1)] = !VALUES.F_NAN
  ENDIF

  IF varn EQ 'SST' THEN var = read_ncdf('SST', /allrecords, filename=path+file, /nostruct) - 273.15 ; degK -> degC

  IF varn EQ 'UV10' THEN BEGIN
    var1 = read_ncdf('U10', /allrecords, filename=path+file, /nostruct) & help, var1
    var2 = read_ncdf('V10', /allrecords, filename=path+file, /nostruct) & help, var2
    var = (var1^2+var2^2)^0.5
  ENDIF

  IF expn EQ 'ERA-I' THEN juld = time
    
  IF varn EQ 'SSH' THEN BEGIN
    var = read_ncdf('zos', /allrecords, filename=path+file, /nostruct)
    IF expn NE 'AVISO' THEN var = var * 100. ; m -> cm
    IF expn NE 'AVISO' THEN BEGIN mask  = var[*,*,0]*0. + 1. & mask[where(var[*,*,0] EQ 0.)] = 0. & ENDIF
    IF expn EQ 'AVISO' THEN BEGIN
      mask  = var[*,*,0] * 0. + 1. & mask[where(finite(var[*,*,0]) EQ 0.)] = !VALUES.F_NAN
    ENDIF
  ENDIF

  IF varn EQ 'RAIN' THEN BEGIN
    IF expn EQ 'COUPLED_SW2_KF' OR expn EQ 'COUPLED_SW2_BMJ' THEN var = read_ncdf('RAIN', /allrecords, filename=path+file, /nostruct) * 4. ; mm/6h -> mm/d
    IF expn EQ 'COUPLED_SW2_BMJ' OR expn EQ 'COUPLED_SW2_KF' THEN var[where(var GT 100.)] = !VALUES.F_NAN
    IF expn EQ 'GPCP_MONTHLY' THEN var = read_ncdf('RAIN', /allrecords, filename=path+file, /nostruct)
    IF expn EQ 'GPCP_MONTHLY' THEN juld = time
    initncdf, maskfile & domdef, box
    landmask = read_ncdf('LANDMASK',filename=maskfile,/nostruct)
    mask = landmask & mask[where(landmask EQ 0)] = 1 & mask[where(landmask EQ 1)] = !VALUES.F_NAN
    IF expn EQ 'GPCP_MONTHLY' THEN BEGIN
      maskin = mask
      lonin = glamt[firstxt:lastxt,firstyt:lastyt]
      latin = gphit[firstxt:lastxt,firstyt:lastyt]
      initncdf, path+file & domdef, box
      mask = fromreg('bilinear',maskin,lonin,latin,glamt[firstxt:lastxt,firstyt:lastyt],gphit[firstxt:lastxt,firstyt:lastyt])
    ENDIF
  ENDIF
  help, var
  help, mask 

; temps
  nbt  = n_elements(var[0,0,*])
  juld_ini = date2jul(date_ini)
  IF expn EQ 'AVISO' THEN juld = juld_ini + dindgen(nbt)*7. 
  IF expn EQ 'COUPLED_SW2_KF' OR expn EQ 'COUPLED_SW2_BMJ' THEN juld = juld_ini + dindgen(nbt) & help, juld
  IF expn EQ 'AVISO' THEN date_ini = 19930101d
  period  = [max([date_ini,19900101d]),min([date_end,20091231.75d])]
  date = jul2date(juld, month=month, year=year) & help, date
  iok  = where(date GE period[0] AND date LE period[1], cntok) & help, iok
  juld  = juld[iok] & help, juld
  date  = date[iok] & help, date
  month = month[iok] & help, month
  nbday = juld[n_elements(juld)-1] - juld[0] + 1 & help, nbday
  nbweek = floor(nbday / 7.) & help, nbweek
  nbyear  = long(period[1]/10000) - long(period[0]/10000) + 1 & help, nbyear
  nbmonth = 12-(long(period[0]/100) - long(period[0]/10000)*100)+1 + (nbyear-2)*12 + (long(period[1]/100) - long(period[1]/10000)*100) & help, nbmonth
  var = var[*,*,iok] & help, var
  IF varn EQ 'UV10' THEN var1 = var1[*,*,iok]
  IF varn EQ 'UV10' THEN var2 = var2[*,*,iok]
;  plt, m_mean(var, dim=3, /nan), /nocont, realcont=2, title='ssh mean - '+expn, lct=64, min=-5, max=5 & stop

; monthly
  IF expn EQ 'GPCP_MONTHLY' OR expn EQ 'ERA-I' THEN BEGIN
    var_monthly = var
    IF varn EQ 'UV10' THEN var1_monthly = var1
    IF varn EQ 'UV10' THEN var2_monthly = var2
    date_monthly  = date
  ENDIF ELSE BEGIN
    var_monthly = fltarr(n_elements(var[*,0,0]),n_elements(var[0,*,0]),nbmonth)
    IF varn EQ 'UV10' THEN var1_monthly = fltarr(n_elements(var[*,0,0]),n_elements(var[0,*,0]),nbmonth)
    IF varn EQ 'UV10' THEN var2_monthly = fltarr(n_elements(var[*,0,0]),n_elements(var[0,*,0]),nbmonth)
    date_monthly = dblarr(nbmonth)
    FOR i = 0, nbmonth-1 DO BEGIN
;      m = ((long(period[0]/100) - long(period[0]/10000)*100) + i)
      datemin = (long(period[0]/10000)+floor((i+0)/12.))*10000+(i mod 12 + 1) *100+1
      datemax = (long(period[0]/10000)+floor((i+1)/12.))*10000+((i+1) mod 12 + 1) *100+1
      iave = where(date GE datemin AND date LT datemax, cntave)
;      print, datemin, datemax, cntave
      IF iave[0] NE -1 THEN var_monthly[*,*,i] = m_mean(var[*,*,iave], dim=3, /nan) ELSE STOP
      IF iave[0] NE -1 AND varn EQ 'UV10' THEN var1_monthly[*,*,i] = m_mean(var1[*,*,iave], dim=3, /nan)
      IF iave[0] NE -1 AND varn EQ 'UV10' THEN var2_monthly[*,*,i] = m_mean(var2[*,*,iave], dim=3, /nan)
      date_monthly[i] = double(datemin)
    ENDFOR
;    fid = NCDF_CREATE(path+varn+'_MONTHLY_1990-2009.nc', /CLOBBER)
;    xid = NCDF_DIMDEF(fid, 'lon', n_elements(glamt[*,0]))
;    yid = NCDF_DIMDEF(fid, 'lat', n_elements(gphit[0,*]))
;    tid = NCDF_DIMDEF(fid, 'time', /UNLIMITED)
;    IF varn NE 'UV10' THEN BEGIN
;      vid = NCDF_VARDEF(fid, varn, [xid, yid, tid], /FLOAT)
;   ENDIF ELSE BEGIN
;      vid1 = NCDF_VARDEF(fid, 'U10', [xid, yid, tid], /FLOAT)
;      vid2 = NCDF_VARDEF(fid, 'V10', [xid, yid, tid], /FLOAT)
;    ENDELSE
;    lon = NCDF_VARDEF(fid, 'lon', [xid, yid], /FLOAT)
;    lat = NCDF_VARDEF(fid, 'lat', [xid, yid], /FLOAT)
;    tim = NCDF_VARDEF(fid, 'time', [tid], /DOUBLE)
;    NCDF_ATTPUT, fid, tim, "calendar", "proleptic_gregorian"
;    NCDF_ATTPUT, fid, tim, "units", "day as %Y%m%d.%f"
;    NCDF_CONTROL, fid, /ENDEF
;    IF varn NE 'UV10' THEN BEGIN
;      NCDF_VARPUT, fid, vid, var_monthly
;    ENDIF ELSE BEGIN
;      NCDF_VARPUT, fid, vid1, var1_monthly
;      NCDF_VARPUT, fid, vid2, var2_monthly
;    ENDELSE
;    NCDF_VARPUT, fid, lon, glamt
;    NCDF_VARPUT, fid, lat, gphit
;    NCDF_VARPUT, fid, tim, date_monthly
;    NCDF_CLOSE, fid
  ENDELSE
  juld_monthly = date2jul(date_monthly)
  help, var_monthly
;  plt, m_mean(var_monthly, dim=3, /nan), /nocont, realcont=2, title='ssh_monthly mean - '+expn, lct=64, min=-5, max=5 & stop


; SON average
  var_son = fltarr(n_elements(var_monthly[*,0,0]),n_elements(var_monthly[0,*,0]),nbyear)
  IF varn EQ 'UV10' THEN var1_son = fltarr(n_elements(var_monthly[*,0,0]),n_elements(var_monthly[0,*,0]),nbyear)
  IF varn EQ 'UV10' THEN var2_son = fltarr(n_elements(var_monthly[*,0,0]),n_elements(var_monthly[0,*,0]),nbyear)
  FOR i = 0, nbyear-1 DO BEGIN
    iave = where(date_monthly GE (long(period[0]/10000)+i)*10000 + 9*100 + 1 AND date_monthly LT (long(period[0]/10000)+i)*10000 + 12*100 + 1, cntave)
    print, long(period[0]/10000)+i, (long(period[0]/10000)+i)*10000 + 9*100 + 1, (long(period[0]/10000)+i)*10000 + 12*100 + 1, cntave
    IF iave[0] NE -1 THEN var_son[*,*,i] = m_mean(var_monthly[*,*,iave], dim=3, /nan) ELSE STOP
    IF iave[0] NE -1 AND varn EQ 'UV10' THEN var1_son[*,*,i] = m_mean(var1_monthly[*,*,iave], dim=3, /nan)
    IF iave[0] NE -1 AND varn EQ 'UV10' THEN var2_son[*,*,i] = m_mean(var2_monthly[*,*,iave], dim=3, /nan)
  ENDFOR
  help, var_son


; DJFMA average
  var_djfma = fltarr(n_elements(var_monthly[*,0,0]),n_elements(var_monthly[0,*,0]),nbyear)
  IF varn EQ 'UV10' THEN var1_djfma = fltarr(n_elements(var_monthly[*,0,0]),n_elements(var_monthly[0,*,0]),nbyear)
  IF varn EQ 'UV10' THEN var2_djfma = fltarr(n_elements(var_monthly[*,0,0]),n_elements(var_monthly[0,*,0]),nbyear)
  FOR i = 0, nbyear-2 DO BEGIN
    iave = where(date_monthly GE (long(period[0]/10000)+i)*10000 + 12*100 + 1 AND date_monthly LT (long(period[0]/10000)+i+1)*10000 + 5*100 + 1, cntave)
    print, long(period[0]/10000)+i, (long(period[0]/10000)+i)*10000 + 12*100 + 1, (long(period[0]/10000)+i+1)*10000 + 5*100 + 1, cntave
    IF iave[0] NE -1 THEN var_djfma[*,*,i] = m_mean(var_monthly[*,*,iave], dim=3, /nan) ELSE STOP
    IF iave[0] NE -1 AND varn EQ 'UV10' THEN var1_djfma[*,*,i] = m_mean(var1_monthly[*,*,iave], dim=3, /nan)
    IF iave[0] NE -1 AND varn EQ 'UV10' THEN var2_djfma[*,*,i] = m_mean(var2_monthly[*,*,iave], dim=3, /nan)
  ENDFOR
  help, var_djfma


; IOD anomaly
  yearly = floor(period[0]/10000.) + findgen(nbyear)
  iodp   = intarr(n_elements(intersect(yearly,iodp_year))) & help, iodp
  iodn   = intarr(n_elements(intersect(yearly,iodn_year))) & help, iodn
  FOR i = 0, n_elements(intersect(yearly,iodp_year))-1 DO iodp[i] = where(yearly EQ (intersect(yearly,iodp_year))[i])
  FOR i = 0, n_elements(intersect(yearly,iodn_year))-1 DO iodn[i] = where(yearly EQ (intersect(yearly,iodn_year))[i])
  print, yearly[iodp], yearly[iodn]
  var_iod = m_mean(var_son[*,*,iodp], dim=3, /nan) - m_mean(var_son[*,*,iodn], dim=3, /nan)
  var_iodp = m_mean(var_son[*,*,iodp], dim=3, /nan)
  var_iodn = m_mean(var_son[*,*,iodn], dim=3, /nan)
  IF varn EQ 'UV10' THEN BEGIN
    var1_iod = m_mean(var1_son[*,*,iodp], dim=3, /nan) - m_mean(var1_son[*,*,iodn], dim=3, /nan)
    var2_iod = m_mean(var2_son[*,*,iodp], dim=3, /nan) - m_mean(var2_son[*,*,iodn], dim=3, /nan)
    var1_iodp = m_mean(var1_son[*,*,iodp], dim=3, /nan)
    var2_iodp = m_mean(var2_son[*,*,iodp], dim=3, /nan)
    var1_iodn = m_mean(var1_son[*,*,iodn], dim=3, /nan)
    var2_iodn = m_mean(var2_son[*,*,iodn], dim=3, /nan)
  ENDIF


; ENSO anomaly
  ensop = intarr(n_elements(intersect(yearly,ensop_year))) & help, ensop
  enson = intarr(n_elements(intersect(yearly,enson_year))) & help, enson
  FOR i = 0, n_elements(intersect(yearly,ensop_year))-1 DO ensop[i] = where(yearly EQ (intersect(yearly,ensop_year))[i])
  FOR i = 0, n_elements(intersect(yearly,enson_year))-1 DO enson[i] = where(yearly EQ (intersect(yearly,enson_year))[i])
  print, yearly[ensop], yearly[enson]
  var_enso = m_mean(var_djfma[*,*,ensop], dim=3, /nan) - m_mean(var_djfma[*,*,enson], dim=3, /nan)
  IF varn EQ 'UV10' THEN BEGIN
    var1_enso = m_mean(var1_djfma[*,*,ensop], dim=3, /nan) - m_mean(var1_djfma[*,*,enson], dim=3, /nan)
    var2_enso = m_mean(var2_djfma[*,*,ensop], dim=3, /nan) - m_mean(var2_djfma[*,*,enson], dim=3, /nan)
  ENDIF


; interpolation
  IF expn EQ 'AVISO' OR expn EQ 'GPCP_MONTHLY' OR expn EQ 'ERA-I' THEN BEGIN
    pathfile_obs = path+file
    glamt_obs = glamt[firstxt:lastxt,firstyt:lastyt] & help, glamt_obs
    gphit_obs = gphit[firstxt:lastxt,firstyt:lastyt] & help, gphit_obs
    mask_obs  = mask & help, mask_obs
    tmp = execute('glamt_'+strlowcase(varn)+'= glamt_obs & help, glamt_'+strlowcase(varn))
    tmp = execute('gphit_'+strlowcase(varn)+'= gphit_obs & help, gphit_'+strlowcase(varn))
    tmp = execute('mask_' +strlowcase(varn)+'= mask_obs  & help, mask_'+strlowcase(varn))
    tmp = execute('pathfile_' +strlowcase(varn)+'= pathfile_obs & help, pathfile_'+strlowcase(varn))
  ENDIF
  
  IF expn EQ 'COUPLED_SW2_BMJ' OR expn EQ 'COUPLED_SW2_KF' THEN BEGIN
    tmp = execute('glamt_obs = glamt_'+strlowcase(varn)+' & help, glamt_obs')
    tmp = execute('gphit_obs = gphit_'+strlowcase(varn)+' & help, gphit_obs')
    glamt_mod = glamt[firstxt:lastxt,firstyt:lastyt] & help, glamt_mod
    gphit_mod = gphit[firstxt:lastxt,firstyt:lastyt] & help, gphit_mod
    var_iod = fromreg('bilinear',var_iod,glamt_mod[*,0],reform(gphit_mod[0,*]),glamt_obs[*,0],reform(gphit_obs[0,*]))
    var_iodp = fromreg('bilinear',var_iodp,glamt_mod[*,0],reform(gphit_mod[0,*]),glamt_obs[*,0],reform(gphit_obs[0,*]))
    var_iodn = fromreg('bilinear',var_iodn,glamt_mod[*,0],reform(gphit_mod[0,*]),glamt_obs[*,0],reform(gphit_obs[0,*]))
    var_enso = fromreg('bilinear',var_enso,glamt_mod[*,0],reform(gphit_mod[0,*]),glamt_obs[*,0],reform(gphit_obs[0,*]))
    IF varn EQ 'UV10' THEN BEGIN
      var1_iod = fromreg('bilinear',var1_iod,glamt_mod[*,0],reform(gphit_mod[0,*]),glamt_obs[*,0],reform(gphit_obs[0,*]))
      var2_iod = fromreg('bilinear',var2_iod,glamt_mod[*,0],reform(gphit_mod[0,*]),glamt_obs[*,0],reform(gphit_obs[0,*]))
      var1_iodp = fromreg('bilinear',var1_iodp,glamt_mod[*,0],reform(gphit_mod[0,*]),glamt_obs[*,0],reform(gphit_obs[0,*]))
      var2_iodp = fromreg('bilinear',var2_iodp,glamt_mod[*,0],reform(gphit_mod[0,*]),glamt_obs[*,0],reform(gphit_obs[0,*]))
      var1_iodn = fromreg('bilinear',var1_iodn,glamt_mod[*,0],reform(gphit_mod[0,*]),glamt_obs[*,0],reform(gphit_obs[0,*]))
      var2_iodn = fromreg('bilinear',var2_iodn,glamt_mod[*,0],reform(gphit_mod[0,*]),glamt_obs[*,0],reform(gphit_obs[0,*]))
      var1_enso = fromreg('bilinear',var1_enso,glamt_mod[*,0],reform(gphit_mod[0,*]),glamt_obs[*,0],reform(gphit_obs[0,*]))
      var2_enso = fromreg('bilinear',var2_enso,glamt_mod[*,0],reform(gphit_mod[0,*]),glamt_obs[*,0],reform(gphit_obs[0,*]))
    ENDIF
    mask = fromreg('bilinear',mask,glamt_mod[*,0],reform(gphit_mod[0,*]),glamt_obs[*,0],reform(gphit_obs[0,*]))
  ENDIF
  

  IF varn EQ 'SSH'  THEN BEGIN
    ssh_iod  = var_iod
    ssh_enso = var_enso
  ENDIF
  IF varn EQ 'RAIN' THEN BEGIN
    rain_iod  = fromreg('bilinear',var_iod,glamt_obs,gphit_obs,glamt_sst,gphit_sst)
    rain_enso = fromreg('bilinear',var_enso,glamt_obs,gphit_obs,glamt_sst,gphit_sst)
  ENDIF
  IF varn EQ 'SST'  THEN BEGIN
    sst_iod  = var_iod
    sst_iodp = var_iodp
    sst_iodn = var_iodn
    sst_enso = var_enso
  ENDIF
  IF varn EQ 'UV10' THEN BEGIN
    uv10_iod  = var_iod
    uv10_iodp = var_iodp
    uv10_iodn = var_iodn
    u10_iod   = var1_iod*mask
    u10_iodp  = var1_iodp*mask
    u10_iodn  = var1_iodn*mask
    v10_iod   = var2_iod*mask
    v10_iodp  = var2_iodp*mask
    v10_iodn  = var2_iodn*mask
    uv10_enso = var_enso
    u10_enso  = var1_enso*mask
    v10_enso  = var2_enso*mask
;    u10_iod   = fromreg('bilinear',u10_iod,glamt_obs,gphit_obs,glamt_ssh,gphit_ssh)
;    v10_iod   = fromreg('bilinear',v10_iod,glamt_obs,gphit_obs,glamt_ssh,gphit_ssh)
;    u10_enso  = fromreg('bilinear',u10_enso,glamt_obs,gphit_obs,glamt_ssh,gphit_ssh)
;    v10_enso  = fromreg('bilinear',v10_enso,glamt_obs,gphit_obs,glamt_ssh,gphit_ssh)
  ENDIF

ENDFOR ; b



; PLOTS 2D (toujours sur grille obs)
IF plot_2D THEN BEGIN

  SET_PLOT, 'X'
  DEVICE, decomposed=0, retain=0
  lct, 33

  ; coef de smooth
  IF expn EQ 'COUPLED_SW2_KF' OR expn EQ 'COUPLED_SW2_BMJ' THEN smt = 5 ELSE smt = 1


  IF varn EQ 'RAIN' THEN BEGIN
  IF write_ps THEN openps, filename='SST_RAIN_ENSO_'+expn+'.ps'
  initncdf, pathfile_sst & domdef, box
  plt, smooth(sst_enso,smt,/nan), min=-2.5, max=2.5, /realcont, lct=64, $
  contour=smooth(rain_enso,smt,/nan)*mask_sst, contintervalle=2., contmin=-16., contmax=16., $
  c_thick=[4,1,1,4,1,1,4,1,4,1,4,1,1,4,1,1,4], $
  c_label=[1,0,0,1,0,0,1,0,1,0,1,0,0,1,0,0,1], $
  c_lines=[2,2,2,2,2,2,2,2,0,0,0,0,0,0,0,0,0], $
  title=expn+' SST + RAIN - ENSO ANOMALY', xtitle='', ytitle='', format='(F4.1)'
  IF write_ps THEN closeps & stop


  IF write_ps THEN openps, filename='SST_RAIN_IOD_'+expn+'.ps'
  initncdf, pathfile_sst & domdef, box
  plt, smooth(sst_iod,smt,/nan), min=-2.5, max=2.5, /realcont, lct=64, $
  contour=smooth(rain_iod,smt,/nan)*mask_sst, contintervalle=2., contmin=-16., contmax=16., $
  c_thick=[4,1,1,4,1,1,4,1,4,1,4,1,1,4,1,1,4], $
  c_label=[1,0,0,1,0,0,1,0,1,0,1,0,0,1,0,0,1], $
  c_lines=[2,2,2,2,2,2,2,2,0,0,0,0,0,0,0,0,0], $
  title=expn+' SST + RAIN - IOD ANOMALY', xtitle='', ytitle='', format='(F4.1)'
  IF write_ps THEN closeps & stop
  ENDIF


  IF varn EQ 'UV10' THEN BEGIN
  IF write_ps THEN openps, filename='SSH_UV10_ENSO_'+expn+'.ps'
  initncdf, pathfile_ssh & domdef, box
  plt, smooth(ssh_enso,5,/nan), min=-25, max=25, /realcont, lct=64, /nocont, $
  title=expn+' SSH + UV10 - ENSO ANOMALY', xtitle='', ytitle='', format='(I3)'
  initncdf, pathfile_uv10 & domdef, box
  ajoutvect, {u:{a:u10_enso, g:'T'}, v:{a:v10_enso, g:'T'}}, unvectsur = [7,7], normeref=5, cmref=1, vectthick=2
  IF write_ps THEN closeps & stop


  IF write_ps THEN openps, filename='SSH_UV10_IOD_'+expn+'.ps'
  initncdf, pathfile_ssh & domdef, box
  plt, smooth(ssh_iod,5,/nan), min=-25, max=25, /realcont, lct=64, /nocont, $
  title=expn+' SSH + UV10 - IOD ANOMALY', xtitle='', ytitle='', format='(I3)'
  initncdf, pathfile_uv10 & domdef, box
  ajoutvect, {u:{a:u10_iod, g:'T'}, v:{a:v10_iod, g:'T'}}, unvectsur = [7,7], normeref=5, cmref=1, vectthick=2
  IF write_ps THEN closeps & stop
  ENDIF

ENDIF

ENDFOR ; v
ENDFOR ; e


STOP
END
