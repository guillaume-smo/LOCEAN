PRO plot_SSH
@common

exp_list = ['AVISO','COUPLED_SW2_BMJ','COUPLED_SW2_KF']
period   = [19930101d,20091231d]
bas_list = ['IO']

;--------------------------------------------------------------------------------------

nbyear  = long(period[1]/10000) - long(period[0]/10000) + 1 & help, nbyear
nbmonth = 12-(long(period[0]/100) - long(period[0]/10000)*100)+1 + (nbyear-2)*12 + (long(period[1]/100) - long(period[1]/10000)*100) & help, nbmonth

FOR e = 0, n_elements(exp_list)-1 DO BEGIN
FOR b = 0, n_elements(bas_list)-1 DO BEGIN

  expn = exp_list[e] & help, expn
  basn = bas_list[b] & help, basn

; bassins
  IF basn EQ 'IOD' THEN box = [40,110,-20,20]
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
  ensop_year = [1991,1997,2002,2006,2009]
  enson_year = [1998,1999,2007,2010]

; init
  path = '/net/adonis/usr/adonis/varclim/gslod/EXP_'+expn+'/' & help, path
  IF expn NE 'AVISO' THEN file = 'SSHANO_DAILY_1990-2009.nc' ELSE file = 'SSHANO_WEEKLY_19921014-20091230.nc'
  initncdf, path+file & domdef, box
  IF expn EQ 'AVISO' THEN pathfile_obs = path+file

; var + mask
  var2D = read_ncdf('zos', /allrecords, filename=path+file, /nostruct) & help, var2D
  IF expn NE 'AVISO' THEN var2D = var2D * 100. ; m -> cm
  IF expn NE 'AVISO' THEN BEGIN mask  = var2D[*,*,0]*0. + 1. & mask[where(var2D[*,*,0] EQ 0.)] = 0. & help, mask & ENDIF
  IF expn EQ 'AVISO' THEN BEGIN
    mask_obs  = var2D[*,*,0] * 0. + 1. & mask_obs[where(finite(var2D[*,*,0]) EQ 0.)] = 0. & help, mask_obs
    gphit_obs = gphit[firstxt:lastxt,firstyt:lastyt] & help, gphit_obs
    glamt_obs = glamt[firstxt:lastxt,firstyt:lastyt] & help, glamt_obs
  ENDIF

; temps
  nbt  = n_elements(var2D[0,0,*])
  IF expn EQ 'AVISO' THEN date_ini = 19921014d ELSE date_ini = 19900101d
  juld_ini = date2jul(date_ini)
  IF expn EQ 'AVISO' THEN juld = juld_ini + dindgen(nbt)*7. ELSE juld = juld_ini + dindgen(nbt) & help, juld
  date = jul2date(juld, month=month, year=year) & help, date
  iok  = where(date GE period[0] AND date LE period[1], cntok) & help, iok
  juld  = juld[iok] & help, juld
  date  = date[iok] & help, date
  month = month[iok] & help, month
  nbday = juld[n_elements(juld)-1] - juld[0] & help, nbday
  nbweek = floor(nbday / 7.) & help, nbweek
  var2D = var2D[*,*,iok] & help, var2D
;  plt, m_mean(var2D, dim=3, /nan), /nocont, realcont=2, title='ssh mean - '+expn, lct=64, min=-5, max=5 & stop

; weekly
  IF expn EQ 'AVISO' THEN BEGIN
    var2D_weekly = var2D
    juld_weekly  = juld
  ENDIF ELSE BEGIN
    var2D_weekly = fltarr(n_elements(var2D[*,0,0]),n_elements(var2D[0,*,0]),nbweek)
    juld_weekly  = dblarr(nbweek)
    FOR i = 0, nbweek-1 DO BEGIN
      datemin = date[i*7]
      datemax = date[i*7+7]
      iave = where(date GE datemin AND date LT datemax, cntave)
;      print, datemin, datemax, cntave
      IF iave[0] NE -1 THEN var2D_weekly[*,*,i] = m_mean(var2D[*,*,iave], dim=3, /nan) ELSE STOP
      juld_weekly[i] = juld[i*7]+3.5
    ENDFOR
  ENDELSE

; monthly
  var2D_monthly = fltarr(n_elements(var2D[*,0,0]),n_elements(var2D[0,*,0]),nbmonth)
  date_monthly = dblarr(nbmonth)
  FOR i = 0, nbmonth-1 DO BEGIN
;    m = ((long(period[0]/100) - long(period[0]/10000)*100) + i)
    datemin = (long(period[0]/10000)+floor((i+0)/12.))*10000+(i mod 12 + 1) *100+1
    datemax = (long(period[0]/10000)+floor((i+1)/12.))*10000+((i+1) mod 12 + 1) *100+1
    iave = where(date GE datemin AND date LT datemax, cntave)
;    print, datemin, datemax, cntave
    IF iave[0] NE -1 THEN var2D_monthly[*,*,i] = m_mean(var2D[*,*,iave], dim=3, /nan) ELSE STOP
    date_monthly[i] = double(datemin)
  ENDFOR
  juld_monthly = date2jul(date_monthly)
  IF expn EQ 'AVISO' THEN obs2D_monthly = var2D_monthly
  help, var2D_monthly
;  plt, m_mean(var2D_monthly, dim=3, /nan), /nocont, realcont=2, title='ssh_monthly mean - '+expn, lct=64, min=-5, max=5 & stop

; yearly
  var2D_yearly = fltarr(n_elements(var2D[*,0,0]),n_elements(var2D[0,*,0]),nbyear)
  FOR y = 0, nbyear-1 DO BEGIN
    iave = where(date GE (long(period[0]/10000)+y)*10000 AND date LT (long(period[0]/10000)+y+1)*10000, cntave)
;    print, (long(period[0]/10000)+y)*10000, (long(period[0]/10000)+y+1)*10000, cntave
    IF iave[0] NE -1 THEN var2D_yearly[*,*,y] = m_mean(var2D[*,*,iave], dim=3, /nan) ELSE STOP
  ENDFOR
  IF expn EQ 'AVISO' THEN obs2D_yearly = var2D_yearly
  help, var2D_yearly
;  plt, m_mean(var2D_yearly, dim=3, /nan), /nocont, realcont=2, title='ssh_yearly mean - '+expn, lct=64, min=-5, max=5 & stop

; std dev yearly
  var2D_yearly_stddev = fltarr(n_elements(var2D[*,0,0]),n_elements(var2D[0,*,0]))
  FOR i = 0, n_elements(var2D[*,0,0])-1 DO BEGIN
  FOR j = 0, n_elements(var2D[0,*,0])-1 DO BEGIN
    var2D_yearly_stddev[i,j] = stddev(var2D_yearly[i,j,*], /nan)
  ENDFOR
  ENDFOR
  IF expn EQ 'AVISO' THEN obs2D_yearly_stddev = var2D_yearly_stddev
  help, var2D_yearly_stddev
;  plt, var2D_yearly_stddev, /nocont, realcont=2, title='ssh_yearly_stddev - '+expn, lct=20, min=0, max=10 & stop

; seasonal cycle
  var2D_sc = fltarr(n_elements(var2D[*,0,0]),n_elements(var2D[0,*,0]),12)
  FOR i = 0, 12-1 DO BEGIN
    iok = indgen(nbyear)*12+i
    var2D_sc[*,*,i] = m_mean(var2D_monthly[*,*,iok], dim=3, /nan)
  ENDFOR
  IF expn EQ 'AVISO' THEN obs2D_sc = var2D_sc
  help, var2D_sc
;  plt, m_mean(var2D_sc[*,*,[11,0,1,2]], dim=3, /nan), /nocont, realcont=2, title='ssh clim DJFM - '+expn, lct=64, min=-20, max=20, $
;  xtitle='', ytitle='', format='(F5.1)' & stop
;  plt, m_mean(var2D_sc[*,*,[5,6,7,8]], dim=3, /nan), /nocont, realcont=2, title='ssh clim JJAS - '+expn, lct=64, min=-20, max=20, $
;  xtitle='', ytitle='', format='(F5.1)' & stop

; seasonal cycle std dev
  var2D_sc_stddev = fltarr(n_elements(var2D[*,0,0]),n_elements(var2D[0,*,0]))
  FOR i = 0, n_elements(var2D[*,0,0])-1 DO BEGIN
  FOR j = 0, n_elements(var2D[0,*,0])-1 DO BEGIN
    var2D_sc_stddev[i,j] = stddev(var2D_sc[i,j,*], /nan)
  ENDFOR
  ENDFOR
  IF expn EQ 'AVISO' THEN obs2D_sc_stddev = var2D_sc_stddev
  help, var2D_sc_stddev
;  plt, var2D_sc_stddev, /nocont, realcont=2, title='ssh_sc_stddev - '+expn, lct=20, min=0, max=10 & stop

; intraseasonal variability
;  var2D_isv =  time_filter(var2D_weekly,juld_weekly,30,90)
;  var2D_isv_stddev = fltarr(n_elements(var2D[*,0,0]),n_elements(var2D[0,*,0]))
;  FOR i = 0, n_elements(var2D[*,0,0])-1 DO BEGIN
;  FOR j = 0, n_elements(var2D[0,*,0])-1 DO BEGIN
;    var2D_isv_stddev[i,j] = stddev(var2D_isv[i,j,*], /nan)
;  ENDFOR
;  ENDFOR
;  IF expn EQ 'AVISO' THEN obs2D_isv_stddev = var2D_isv_stddev
;  help, var2D_isv_stddev
;  plt, var2D_isv_stddev, /nocont, realcont=2, title='ssh_isv_stddev - '+expn, lct=20, min=0, max=10 & stop

; seasonal cycle correlation
  IF expn NE 'AVISO' THEN BEGIN
    var2D_sc_gridobs = fltarr(n_elements(obs2D_sc[*,0,0]),n_elements(obs2D_sc[0,*,0]),n_elements(obs2D_sc[0,0,*])) & help, var2D_sc_gridobs
;    FOR i = 0, n_elements(var2D_sc[0,0,*])-1 DO var2D_sc_gridobs[*,*,i] = fromirr('bilinear', var2D_sc[*,*,i], glamt[firstxt:lastxt,firstyt:lastyt], gphit[firstxt:lastxt,firstyt:lastyt], mask, glamt_obs, gphit_obs, mask_obs) & stop
    FOR i = 0, n_elements(var2D_sc[0,0,*])-1 DO var2D_sc_gridobs[*,*,i] = fromreg('bilinear', var2D_sc[*,*,i], glamt[firstxt:lastxt,0], reform(gphit[0,firstyt:lastyt]), glamt_obs[*,0], reform(gphit_obs[0,*]))
    sc_cor = fltarr(n_elements(var2D_sc_gridobs[*,0,0]),n_elements(var2D_sc_gridobs[0,*,0]))
    FOR i = 0, n_elements(var2D_sc_gridobs[*,0,0])-1 DO BEGIN
    FOR j = 0, n_elements(var2D_sc_gridobs[0,*,0])-1 DO BEGIN
      sc_cor[i,j] = correlate(var2D_sc_gridobs[i,j,*],obs2D_sc[i,j,*])
    ENDFOR
    ENDFOR
;    initncdf, pathfile_obs & plt, sc_cor, /nocont, realcont=2, title='ssh_sc_cor - '+expn, lct=64, min=-1, max=1 & stop
  ENDIF


ENDFOR; experience
ENDFOR; bassin

stop
END
