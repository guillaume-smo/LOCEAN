PRO plot_SSH_interannuel
@common

exp_list = ['COUPLED_SW2_BMJ','COUPLED_SW2_KF']
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

; SON average
  var2D_son = fltarr(n_elements(var2D_monthly[*,0,0]),n_elements(var2D_monthly[0,*,0]),nbyear)
  FOR i = 0, nbyear-1 DO BEGIN
    iave = where(date_monthly GE (long(period[0]/10000)+i)*10000 + 9*100 + 1 AND date_monthly LT (long(period[0]/10000)+i)*10000 + 12*100 + 1, cntave)
;    print, (long(period[0]/10000)+i)*10000 + 9*100 + 1, (long(period[0]/10000)+i)*10000 + 12*100 + 1, cntave
    IF iave[0] NE -1 THEN var2D_son[*,*,i] = m_mean(var2D_monthly[*,*,iave], dim=3, /nan) ELSE STOP
  ENDFOR
  help, var2D_son

; IOD anomaly
  yearly = floor(period[0]/10000.) + findgen(nbyear)
  iodp   = intarr(n_elements(intersect(yearly,iodp_year))) & help, iodp
  iodn   = intarr(n_elements(intersect(yearly,iodn_year))) & help, iodn
  FOR i = 0, n_elements(intersect(yearly,iodp_year))-1 DO iodp[i] = where(yearly EQ (intersect(yearly,iodp_year))[i])
  FOR i = 0, n_elements(intersect(yearly,iodn_year))-1 DO iodn[i] = where(yearly EQ (intersect(yearly,iodn_year))[i])
  var2D_iod = m_mean(var2D_son[*,*,iodp], dim=3, /nan) - m_mean(var2D_son[*,*,iodn], dim=3, /nan)
  plt, var2D_iod, /nocont, /realcont, lct=64, min=-30, max=30, $
  title='IOD SSH ANOMALY '+expn, xtitle='', ytitle='', format='(F5.1)' & stop

ENDFOR
ENDFOR

END
