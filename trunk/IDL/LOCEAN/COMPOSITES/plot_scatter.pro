PRO plot_scatter
@common
computegrid,-200,-200,25,25,17,17

machine = 'cratos'
explist = ['KF','BMJ']
param   = 'FORCED'
basin   = 'SIO'
freq    = '6H'
windbin = [40,60]

var1_name = 'UST'
var1_unit = '(g/kg)'
var1_type = 'WSC'
bin1_deb  = -1.
bin1_size = 0.25
bin1_num  = 6
var1_bin  = bin1_deb+findgen(bin1_num)*bin1_size

var2_name = 'LH'
var2_unit = '(W/m^2)'
var2_type = 'WSC'
bin2_deb  = 0
bin2_size = 20
bin2_num  = 20
var2_bin  = bin2_deb+findgen(bin2_num)*bin2_size

datebeg = '19900101' & help, datebeg

leftfront = where(glamt LE 0 AND gphit GE 0)
rightfront = where(glamt GE 0 AND gphit GE 0)
leftrear = where(glamt LE 0 AND gphit LE 0)
rightrear = where(glamt GE 0 AND gphit LE 0)
ijlf = array_indices(glamt, leftfront)
ijrf = array_indices(glamt, rightfront)
ijlr = array_indices(glamt, leftrear)
ijrr = array_indices(glamt, rightrear)


FOR iexp = 0, n_elements(explist)-1 DO BEGIN

exp = explist[iexp] & help, exp
expname = param + '_SW2_'+ exp & help, expname
IF exp EQ 'KF' AND param EQ 'FORCED' THEN period  = '1990-1998' ELSE period  = '1990-2009' & help, period
IF exp EQ 'KF' AND param EQ 'FORCED' THEN dateend = '19990101'  ELSE dateend = '20100101' & help, dateend

pathin  = '/net/adonis/usr/adonis/varclim/gslod/IDL/COLLOCALISATION/EXP_'+expname+'/DATA/' & help, pathin
restore, pathin + 'd1_TRACKS_'+ expname +'_IO_'+datebeg+'-'+dateend+'.dat', /VERBOSE
IF basin EQ 'SIO'  THEN idom = where(d1_lon GT 30. AND d1_lon LT 130. AND d1_lat LT 0.)
IF basin EQ 'SWIO' THEN idom = where(d1_lon GT 30. AND d1_lon LT 80. AND d1_lat LT 0.)
IF basin EQ 'SEIO' THEN idom = where(d1_lon GT 80. AND d1_lon LT 130. AND d1_lat LT 0.)
IF basin EQ 'NIO'  THEN idom = where(d1_lon GT 50. AND d1_lon LT 100. AND d1_lat GT 0.)
IF basin EQ 'NWIO' THEN idom = where(d1_lon GT 50. AND d1_lon LT 80. AND d1_lat GT 0.)
IF basin EQ 'NEIO' THEN idom = where(d1_lon GT 80. AND d1_lon LT 100. AND d1_lat GT 0.)
help, idom
iwind = where(d1_max_wnd GE windbin[0] AND d1_max_wnd LE windbin[1]) & help, iwind
iok =  intersect(idom,iwind) & help, iok
ijok = array_indices([(size(d1_lon))[1], (size(d1_lon))[2]], iok, /dim) & help, ijok
stop
print, pathin + var1_name +'_2D_' + expname +'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat'
restore, pathin + var1_name +'_2D_' + expname +'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat', /VERBOSE
IF var1_type EQ 'WSC' THEN dxy_var1 = temporary(dxy_var_wsc_rot)
IF var1_type EQ 'NSC' THEN dxy_var1 = temporary(dxy_var_nsc_rot)
var1 = fltarr(n_elements(iok), 17,17)
var1_moy = fltarr(n_elements(iok))
var1_min = fltarr(n_elements(iok))
var1_max = fltarr(n_elements(iok))
var1_moylf = fltarr(n_elements(iok))
var1_moyrf = fltarr(n_elements(iok))
var1_moylr = fltarr(n_elements(iok))
var1_moyrr = fltarr(n_elements(iok))
FOR itmp = 0, n_elements(iok)-1 DO BEGIN
  var1[itmp,*,*] = dxy_var1[ijok[0, itmp], ijok[1, itmp],*,*]
  var1_moy[itmp] = mean(var1[itmp,*,*], /nan)
  var1_min[itmp] = min(var1[itmp,*,*], /nan)
  var1_max[itmp] = max(var1[itmp,*,*], /nan)
  var1_moylf[itmp] = mean(var1[itmp,ijlf[0,*],ijlf[1,*]], /nan)
  var1_moyrf[itmp] = mean(var1[itmp,ijrf[0,*],ijrf[1,*]], /nan)
  var1_moylr[itmp] = mean(var1[itmp,ijlr[0,*],ijlr[1,*]], /nan)
  var1_moyrr[itmp] = mean(var1[itmp,ijrr[0,*],ijrr[1,*]], /nan)
ENDFOR

print, pathin + var2_name +'_2D_' + expname +'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat'
restore, pathin + var2_name +'_2D_' + expname +'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat', /VERBOSE
IF var2_type EQ 'WSC' THEN dxy_var2 = temporary(dxy_var_wsc_rot)
IF var2_type EQ 'NSC' THEN dxy_var2 = temporary(dxy_var_nsc_rot)
var2 = fltarr(n_elements(iok), 17,17)
var2_moy = fltarr(n_elements(iok))
var2_min = fltarr(n_elements(iok))
var2_max = fltarr(n_elements(iok))
var2_moylf = fltarr(n_elements(iok))
var2_moyrf = fltarr(n_elements(iok))
var2_moylr = fltarr(n_elements(iok))
var2_moyrr = fltarr(n_elements(iok))
FOR itmp = 0, n_elements(iok)-1 DO BEGIN
  var2[itmp,*,*] = dxy_var2[ijok[0, itmp], ijok[1, itmp],*,*]
  var2_moy[itmp] = mean(var2[itmp,*,*], /nan)
  var2_min[itmp] = min(var2[itmp,*,*], /nan)
  var2_max[itmp] = max(var2[itmp,*,*], /nan)
  var2_moylf[itmp] = mean(var2[itmp,ijlf[0,*],ijlf[1,*]], /nan)
  var2_moyrf[itmp] = mean(var2[itmp,ijrf[0,*],ijrf[1,*]], /nan)
  var2_moylr[itmp] = mean(var2[itmp,ijlr[0,*],ijlr[1,*]], /nan)
  var2_moyrr[itmp] = mean(var2[itmp,ijrr[0,*],ijrr[1,*]], /nan)
ENDFOR

stop

  diffvar = var1_moy-var2_moy
  diffvarn = var1_moy/max(abs(var1_moy)) - var2_moy/max(abs(var2_moy))

  pathfig = '/usr/'+machine+'/varclim/gslod/IDL/COLLOCALISATION/FIGS_COMP_'+exp+'/'
  spawn, 'echo "creation du repertoire '+ pathfig +'"' & spawn, 'mkdir -p '+ pathfig  
  lct,44 
  plt, var1_moy/max(abs(var1_moy)), vnmin, vnmax, title='NORM: '+var_name+' '+var_unit+' - '+var_typ+' - '+expname, subtitle='min: '+strtrim(min(var1_moy),2)+' - max: '+strtrim(max(var1_moy),2), charsize=1., small=[3,1,1], /landscape
  plt, var2_moy/max(abs(var2_moy)), vnmin, vnmax, title='NORM: '+var_name+' '+var_unit+' - '+var_typ+' - '+expname, subtitle='min: '+strtrim(min(var2_moy),2)+' - max: '+strtrim(max(var2_moy),2), charsize=1., small=[3,1,2], /noerase
  plt, diffvarn, title='DIFF CPL-FRC: '+var_name+' '+var_unit+' - '+var_typ+' - '+exp, subtitle='min: '+strtrim(min(var1_moy-var2_moy),2)+' - max: '+strtrim(max(diffvar),2), charsize=1., small=[3,1,3], /noerase
  
  xyouts, 0.1, 0.7, strtrim(m_mean(var1_moy[leftfront],dim=1,/nan),2), align = 0, /normal, color=0, charsize=1.5,charthick=2
  xyouts, 0.25, 0.7, strtrim(m_mean(var1_moy[rightfront],dim=1,/nan),2), align = 0, /normal, color=0, charsize=1.5,charthick=2
  xyouts, 0.1, 0.35, strtrim(m_mean(var1_moy[leftrear],dim=1,/nan),2), align = 0, /normal, color=0, charsize=1.5,charthick=2
  xyouts, 0.25, 0.35, strtrim(m_mean(var1_moy[rightrear],dim=1,/nan),2), align = 0, /normal, color=0, charsize=1.5,charthick=2

  xyouts, 0.4, 0.7, strtrim(m_mean(var2_moy[leftfront],dim=1,/nan),2), align = 0, /normal, color=0, charsize=1.5,charthick=2
  xyouts, 0.55, 0.7, strtrim(m_mean(var2_moy[rightfront],dim=1,/nan),2), align = 0, /normal, color=0, charsize=1.5,charthick=2
  xyouts, 0.4, 0.35, strtrim(m_mean(var2_moy[leftrear],dim=1,/nan),2), align = 0, /normal, color=0, charsize=1.5,charthick=2
  xyouts, 0.55, 0.35, strtrim(m_mean(var2_moy[rightrear],dim=1,/nan),2), align = 0, /normal, color=0, charsize=1.5,charthick=2

  xyouts, 0.7, 0.7, strtrim(m_mean(diffvar[leftfront],dim=1,/nan),2), align = 0, /normal, color=0, charsize=1.5,charthick=2
  xyouts, 0.85, 0.7, strtrim(m_mean(diffvar[rightfront],dim=1,/nan),2), align = 0, /normal, color=0, charsize=1.5,charthick=2
  xyouts, 0.7, 0.35, strtrim(m_mean(diffvar[leftrear],dim=1,/nan),2), align = 0, /normal, color=0, charsize=1.5,charthick=2
  xyouts, 0.85, 0.35, strtrim(m_mean(diffvar[rightrear],dim=1,/nan),2), align = 0, /normal, color=0, charsize=1.5,charthick=2

  xyouts, 0.15, 0.22, 'SAMPLE SIZE: '+strtrim(n_elements(iok),2), align = 0, /normal, color=0, charsize=1.5
  xyouts, 0.45, 0.22, 'SAMPLE SIZE: '+strtrim(n_elements(iok),2), align = 0, /normal, color=0, charsize=1.5
  saveimage, pathfig+'composite_norme_2D_'+var_name+'_'+strtrim(windbin[0],2)+'-'+strtrim(windbin[1],2)+'ms_'+var_typ+'_'+exp+'_'+freq+'_'+basin+'.gif', quality=100


  plt, var1_moy, vmin, vmax, title='COMP: '+var_name+' '+var_unit+' - '+var_typ+' - '+expname, subtitle='min: '+strtrim(min(var1_moy),2)+' - max: '+strtrim(max(var1_moy),2), charsize=1., small=[3,1,1], /landscape
  plt, var2_moy, vmin, vmax, title='COMP: '+var_name+' '+var_unit+' - '+var_typ+' - '+expname, subtitle='min: '+strtrim(min(var2_moy),2)+' - max: '+strtrim(max(var2_moy),2), charsize=1., small=[3,1,2], /noerase
  plt, diffvar, title='DIFF CPL-FRC: '+var_name+' '+var_unit+' - '+var_typ+' - '+exp, subtitle='min: '+strtrim(min(var1_moy-var2_moy),2)+' - max: '+strtrim(max(diffvar),2), charsize=1., small=[3,1,3], /noerase

  xyouts, 0.1, 0.7, strtrim(m_mean(var1_moy[leftfront],dim=1,/nan),2), align = 0, /normal, color=0, charsize=1.5,charthick=2
  xyouts, 0.25, 0.7, strtrim(m_mean(var1_moy[rightfront],dim=1,/nan),2), align = 0, /normal, color=0, charsize=1.5,charthick=2
  xyouts, 0.1, 0.35, strtrim(m_mean(var1_moy[leftrear],dim=1,/nan),2), align = 0, /normal, color=0, charsize=1.5,charthick=2
  xyouts, 0.25, 0.35, strtrim(m_mean(var1_moy[rightrear],dim=1,/nan),2), align = 0, /normal, color=0, charsize=1.5,charthick=2

  xyouts, 0.4, 0.7, strtrim(m_mean(var2_moy[leftfront],dim=1,/nan),2), align = 0, /normal, color=0, charsize=1.5,charthick=2
  xyouts, 0.55, 0.7, strtrim(m_mean(var2_moy[rightfront],dim=1,/nan),2), align = 0, /normal, color=0, charsize=1.5,charthick=2
  xyouts, 0.4, 0.35, strtrim(m_mean(var2_moy[leftrear],dim=1,/nan),2), align = 0, /normal, color=0, charsize=1.5,charthick=2
  xyouts, 0.55, 0.35, strtrim(m_mean(var2_moy[rightrear],dim=1,/nan),2), align = 0, /normal, color=0, charsize=1.5,charthick=2

  xyouts, 0.7, 0.7, strtrim(m_mean(diffvar[leftfront],dim=1,/nan),2), align = 0, /normal, color=0, charsize=1.5,charthick=2
  xyouts, 0.85, 0.7, strtrim(m_mean(diffvar[rightfront],dim=1,/nan),2), align = 0, /normal, color=0, charsize=1.5,charthick=2
  xyouts, 0.7, 0.35, strtrim(m_mean(diffvar[leftrear],dim=1,/nan),2), align = 0, /normal, color=0, charsize=1.5,charthick=2
  xyouts, 0.85, 0.35, strtrim(m_mean(diffvar[rightrear],dim=1,/nan),2), align = 0, /normal, color=0, charsize=1.5,charthick=2

  xyouts, 0.15, 0.22, 'SAMPLE SIZE: '+strtrim(n_elements(iok),2), align = 0, /normal, color=0, charsize=1.5
  xyouts, 0.45, 0.22, 'SAMPLE SIZE: '+strtrim(n_elements(iok),2), align = 0, /normal, color=0, charsize=1.5
  saveimage, pathfig+'composite_2D_'+var_name+'_'+strtrim(windbin[0],2)+'-'+strtrim(windbin[1],2)+'ms_'+var_typ+'_'+exp+'_'+freq+'_'+basin+'.gif', quality=100


;  plt, varstd_1, smin, smax, title='STD DEV: '+var_name+' '+var_unit+' - '+var_typ+' - '+expname, subtitle='min: '+strtrim(min(varstd_1),2)+' - max: '+strtrim(max(varstd_1),2), charsize=1., small=[1,2,1]
;  plt, varstd_2, smin, smax, title='STD DEV: '+var_name+' '+var_unit+' - '+var_typ+' - '+expname, subtitle='min: '+strtrim(min(varstd_2),2)+' - max: '+strtrim(max(varstd_2),2), charsize=1., small=[1,2,2], /noerase
;  xyouts, 0., 0.9, 'SAMPLE SIZE: '+strtrim(n_elements(iok),2), align = 0, /normal, color=0, charsize=1.5
;  xyouts, 0., 0.45, 'SAMPLE SIZE: '+strtrim(n_elements(iok),2), align = 0, /normal, color=0, charsize=1.5
;  saveimage, pathfig+'stddev_2D_'+var_name+'_'+strtrim(windbin[0],2)+'-'+strtrim(windbin[1],2)+'ms_'+var_typ+'_'+exp+'_'+freq+'_'+basin+'.gif'


;  plt, varstd_1, smin, smax, title='STD DEV: '+var_name+' '+var_unit+' - '+var_typ+' - '+expname, subtitle='min: '+strtrim(min(varstd_1),2)+' - max: '+strtrim(max(varstd_1),2), charsize=1., small=[1,2,1]
;  plt, varstd_2, smin, smax, title='STD DEV: '+var_name+' '+var_unit+' - '+var_typ+' - '+expname, subtitle='min: '+strtrim(min(varstd_2),2)+' - max: '+strtrim(max(varstd_2),2), charsize=1., small=[1,2,2], /noerase
;  xyouts, 0., 0.9, 'SAMPLE SIZE: '+strtrim(n_elements(iok),2), align = 0, /normal, color=0, charsize=1.5
;  xyouts, 0., 0.45, 'SAMPLE SIZE: '+strtrim(n_elements(iok),2), align = 0, /normal, color=0, charsize=1.5
;  saveimage, pathfig+'stddev_2D_'+var_name+'_'+strtrim(windbin[0],2)+'-'+strtrim(windbin[1],2)+'ms_'+var_typ+'_'+exp+'_'+freq+'_'+basin+'.gif'

 
;  plt, abs(var1_moy)/max(var1_moy), vnmin, vnmax, title='COMPOSITE NORME: '+var_name+' '+var_unit+' - '+var_typ+' - '+expname, subtitle='min: '+strtrim(min(var1_moy),2)+' - max: '+strtrim(max(var1_moy),2), charsize=1., small=[1,2,1]
;  plt, abs(var2_moy)/max(var2_moy), vnmin, vnmax, title='COMPOSITE NORME: '+var_name+' '+var_unit+' - '+var_typ+' - '+expname, subtitle='min: '+strtrim(min(var2_moy),2)+' - max: '+strtrim(max(var2_moy),2), charsize=1., small=[1,2,2], /noerase
;  xyouts, 0., 0.90, 'SAMPLE SIZE: '+strtrim(n_elements(iok),2), align = 0, /normal, color=0, charsize=1.5
;  xyouts, 0., 0.45, 'SAMPLE SIZE: '+strtrim(n_elements(iok),2), align = 0, /normal, color=0, charsize=1.5
;  saveimage, pathfig+'composite_norme_2D_'+var_name+'_'+strtrim(windbin[0],2)+'-'+strtrim(windbin[1],2)+'ms_'+var_typ+'_'+exp+'_'+freq+'_'+basin+'.gif'


;  plt, varstd_1/max(varstd_1), snmin, snmax, title='STD DEV NORME: '+var_name+' '+var_unit+' - '+var_typ+' - '+expname, subtitle='min: '+strtrim(min(varstd_1),2)+' - max: '+strtrim(max(varstd_1),2), charsize=1., small=[1,2,1]
;  plt, varstd_2/max(varstd_2), snmin, snmax, title='STD DEV NORME: '+var_name+' '+var_unit+' - '+var_typ+' - '+expname, subtitle='min: '+strtrim(min(varstd_2),2)+' - max: '+strtrim(max(varstd_2),2), charsize=1., small=[1,2,2], /noerase
;  xyouts, 0., 0.9, 'SAMPLE SIZE: '+strtrim(n_elements(iok),2), align = 0, /normal, color=0, charsize=1.5
;  xyouts, 0., 0.45, 'SAMPLE SIZE: '+strtrim(n_elements(iok),2), align = 0, /normal, color=0, charsize=1.5
;  saveimage, pathfig+'stddev_norme_2D_'+var_name+'_'+strtrim(windbin[0],2)+'-'+strtrim(windbin[1],2)+'ms_'+var_typ+'_'+exp+'_'+freq+'_'+basin+'.gif'


ENDFOR; exp

END
