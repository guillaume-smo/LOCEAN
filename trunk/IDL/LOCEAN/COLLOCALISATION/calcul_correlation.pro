PRO calcul_correlation
@common
computegrid,-200,-200,25,25,17,17

machine = 'cratos'
explist = ['KF','BMJ']
parlist = ['COUPLED','FORCED']
basin   = 'SIO'
freq    = '6H'
windbin = [17.5,65]
datebeg = '19900101' 
diagazi = 0

;var1_name = 'UV10'
;var1_unit = '(m/s)'
;var1_type = 'WSC'
;bin1_deb  = 0.
;bin1_size = 10.
;bin1_fin  = 60.

;var1_name = 'HFX'
;var1_unit = '(W/m^2)'
;var1_type = 'WSC'
;bin1_deb  = 0.
;bin1_size = 100.
;bin1_fin  = 1000.

;var1_name = 'LH'
;var1_unit = '(W/m^2)'
;var1_type = 'WSC'
;bin1_deb  = 0.
;bin1_size = 500.
;bin1_fin  = 3000.

;var1_name = 'Q2'
;var1_unit = '(g/kg)'
;var1_type = 'WSC'
;bin1_deb  = 0.
;bin1_size = 5.
;bin1_fin  = 40.

;var2_name = 'QS0'
;var2_unit = '(g/kg)'
;var2_type = 'WSC'
;bin2_deb  = 0.
;bin2_size = 5.
;bin2_fin  = 40.

var1_name = 'HFXLH'
var1_unit = '(W/m^2)'
var1_type = 'WSC'
bin1_deb  = 0.
bin1_size = 200.
bin1_fin  = 2000.

var2_name = 'THETAE2'
var2_unit = '(degK)'
var2_type = 'WSC'
bin2_deb  = 290.
bin2_size = 2.
bin2_fin  = 310.

;var2_name = 'RAIN'
;var2_unit = '(mm/h)'
;var2_type = 'WSC'
;bin2_deb  = 0.
;bin2_size = 5000.
;bin2_fin  = 50000.

;var2_name = 'treal500'
;var2_unit = '(degK)'
;var2_type = 'WSC'
;bin2_deb  = 270.
;bin2_size = 2.
;bin2_fin  = 286.

;var2_name = 'treal500'
;var2_unit = '(degK)'
;var2_type = 'NSC'
;bin2_deb  = 0.
;bin2_size = 2.
;bin2_fin  = 20.

bin1_num  = (bin1_fin - bin1_deb) / bin1_size
var1_bin  = bin1_deb+findgen(bin1_num)*bin1_size
print, var1_bin
bin2_num  = (bin2_fin - bin2_deb) / bin2_size
var2_bin  = bin2_deb+findgen(bin2_num)*bin2_size
print, var2_bin

leftfront = where(glamt LE 0 AND gphit GE 0)
rightfront = where(glamt GE 0 AND gphit GE 0)
leftrear = where(glamt LE 0 AND gphit LE 0)
rightrear = where(glamt GE 0 AND gphit LE 0)
ijlf = array_indices(glamt, leftfront)
ijrf = array_indices(glamt, rightfront)
ijlr = array_indices(glamt, leftrear)
ijrr = array_indices(glamt, rightrear)

;-------------------------------------------------------------------------------------------------------------------------

FOR iexp = 0, n_elements(explist)-1 DO BEGIN
exp = explist[iexp] & help, exp
FOR ipar = 0, n_elements(parlist)-1 DO BEGIN
param = parlist[ipar] & help, param
expname = param + '_SW2_'+ exp & help, expname
IF exp EQ 'KF' AND param EQ 'FORCED' THEN period  = '1990-1998' ELSE period  = '1990-2009' & help, period
IF exp EQ 'KF' AND param EQ 'FORCED' THEN dateend = '19990101'  ELSE dateend = '20100101' & help, dateend
pathin  = '/net/adonis/usr/adonis/varclim/gslod/IDL/COLLOCALISATION/EXP_'+expname+'/DATA/' & help, pathin
restore, pathin + 'd1_TRACKS_'+ expname +'_IO_'+datebeg+'-'+dateend+'.dat', /VERBOSE

IF basin EQ 'SIO'  THEN idom = where(transpose(d1_lon) GT 30. AND transpose(d1_lon) LT 130. AND transpose(d1_lat) LT 0.)
IF basin EQ 'SWIO' THEN idom = where(transpose(d1_lon) GT 30. AND transpose(d1_lon) LT 80. AND transpose(d1_lat) LT 0.)
IF basin EQ 'SEIO' THEN idom = where(transpose(d1_lon) GT 80. AND transpose(d1_lon) LT 130. AND transpose(d1_lat) LT 0.)
IF basin EQ 'NIO'  THEN idom = where(transpose(d1_lon) GT 50. AND transpose(d1_lon) LT 100. AND transpose(d1_lat) GT 0.)
IF basin EQ 'NWIO' THEN idom = where(transpose(d1_lon) GT 50. AND transpose(d1_lon) LT 80. AND transpose(d1_lat) GT 0.)
IF basin EQ 'NEIO' THEN idom = where(transpose(d1_lon) GT 80. AND transpose(d1_lon) LT 100. AND transpose(d1_lat) GT 0.)
help, idom
ifin = where(finite(transpose(d1_pres)) eq 1 AND finite(transpose(d1_max_wnd)) eq 1) & help, ifin
iok = intersect(ifin,idom) & help, iok
iwind = where(transpose(d1_max_wnd) GE windbin[0] AND transpose(d1_max_wnd) LE windbin[1]) & help, iwind
iok =  intersect(iok,iwind) & help, iok
ijok = array_indices(transpose(d1_pres), iok) & help, ijok

cor_mslp_maxuv = correlate((transpose(d1_pres))[iok],(transpose(d1_max_wnd))[iok]) & print, cor_mslp_maxuv
dpresdt = smooth(transpose(shift((transpose(d1_pres))[iok],-2)-shift((transpose(d1_pres))[iok],2))/24.,1,/nan)
dwinddt = smooth(transpose(shift((transpose(d1_max_wnd))[iok],-2)-shift((transpose(d1_max_wnd))[iok],2))/24.,1,/nan) 
cor_dt = correlate(dpresdt,dwinddt) & print, cor_dt

ipinc = where(dpresdt LT 0.) & help, ipinc
iwinc = where(dwinddt GT 0.) & help, iwinc
ipwinc = where(dpresdt LT 0. AND dwinddt GT 0.) & help, ipwinc
ipdec = where(dpresdt GT 0.) & help, ipdec
iwdec = where(dwinddt LT 0.) & help, iwdec
ipwdec = where(dpresdt GT 0. AND dwinddt LT 0.) & help, ipwdec

;-------------------------------------------------------------------------------------------------------------------------

print, pathin + var1_name +'_2D_' + expname +'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat'
restore, pathin + var1_name +'_2D_' + expname +'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat', /VERBOSE
IF var1_type EQ 'WSC' THEN dxy_var1 = temporary(dxy_var_wsc_rot)
IF var1_type EQ 'NSC' THEN dxy_var1 = temporary(dxy_var_nsc_rot)
IF var1_name EQ 'Q2'  THEN dxy_var1 = dxy_var1 * 1000.
IF var1_name EQ 'PSFC' THEN dxy_var1 = dxy_var1 / 100.

var1 = fltarr(n_elements(iok), 17,17)
var1_moy = fltarr(n_elements(iok))
var1_min = fltarr(n_elements(iok))
var1_max = fltarr(n_elements(iok))
var1_sum = fltarr(n_elements(iok))
FOR itmp = 0, n_elements(iok)-1 DO BEGIN
  var1[itmp,*,*] = dxy_var1[ijok[1, itmp], ijok[0, itmp],*,*]
  var1_moy[itmp] = mean(var1[itmp,*,*], /nan)
  var1_min[itmp] = min(var1[itmp,*,*], /nan)
  var1_max[itmp] = max(var1[itmp,*,*], /nan)
  var1_sum[itmp] = total(total(reform(var1[itmp,*,*]),1,/nan),1,/nan)
ENDFOR
IF diagazi THEN BEGIN
var1_moylf = fltarr(n_elements(iok))
var1_moyrf = fltarr(n_elements(iok))
var1_moylr = fltarr(n_elements(iok))
var1_moyrr = fltarr(n_elements(iok))
FOR itmp = 0, n_elements(iok)-1 DO BEGIN
  var1_moylf[itmp] = mean(var1[itmp,ijlf[0,*],ijlf[1,*]], /nan)
  var1_moyrf[itmp] = mean(var1[itmp,ijrf[0,*],ijrf[1,*]], /nan)
  var1_moylr[itmp] = mean(var1[itmp,ijlr[0,*],ijlr[1,*]], /nan)
  var1_moyrr[itmp] = mean(var1[itmp,ijrr[0,*],ijrr[1,*]], /nan)
ENDFOR
ENDIF

print, pathin + var2_name +'_2D_' + expname +'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat'
restore, pathin + var2_name +'_2D_' + expname +'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat', /VERBOSE
IF var2_type EQ 'WSC' THEN dxy_var2 = temporary(dxy_var_wsc_rot)
IF var2_type EQ 'NSC' THEN dxy_var2 = temporary(dxy_var_nsc_rot)
IF var2_name EQ 'Q2'  THEN dxy_var2 = dxy_var2 * 1000.
IF var2_name EQ 'PSFC'  THEN dxy_var2 = dxy_var2 / 100.

var2 = fltarr(n_elements(iok), 17,17)
var2_moy = fltarr(n_elements(iok))
var2_min = fltarr(n_elements(iok))
var2_max = fltarr(n_elements(iok))
var2_sum = fltarr(n_elements(iok))
FOR itmp = 0, n_elements(iok)-1 DO BEGIN
  var2[itmp,*,*] = dxy_var2[ijok[1, itmp], ijok[0, itmp],*,*]
  var2_moy[itmp] = mean(var2[itmp,*,*], /nan)
  var2_min[itmp] = min(var2[itmp,*,*], /nan)
  var2_max[itmp] = max(var2[itmp,*,*], /nan)
  var2_sum[itmp] = total(total(reform(var2[itmp,*,*]),1,/nan),1,/nan)
ENDFOR
IF diagazi THEN BEGIN
var2_moylf = fltarr(n_elements(iok))
var2_moyrf = fltarr(n_elements(iok))
var2_moylr = fltarr(n_elements(iok))
var2_moyrr = fltarr(n_elements(iok))
FOR itmp = 0, n_elements(iok)-1 DO BEGIN
  var2_moylf[itmp] = mean(var2[itmp,ijlf[0,*],ijlf[1,*]], /nan)
  var2_moyrf[itmp] = mean(var2[itmp,ijrf[0,*],ijrf[1,*]], /nan)
  var2_moylr[itmp] = mean(var2[itmp,ijlr[0,*],ijlr[1,*]], /nan)
  var2_moyrr[itmp] = mean(var2[itmp,ijrr[0,*],ijrr[1,*]], /nan)
ENDFOR
ENDIF

var1_den = var1_moy
var2_den = var2_max
ifin = where(finite(var1_den) EQ 1 AND finite(var2_den) EQ 1)
print, correlate(var1_den[ifin],var2_den[ifin])
print, c_correlate(var1_den[ifin],var2_den[ifin], [-4,-3,-2,-1,0,1,2,3,4])

densite = fltarr(bin1_num,bin2_num)
FOR ibin1 = 0, bin1_num-1 DO BEGIN
  index1 = where(var1_den GE var1_bin[ibin1] AND var1_den LT var1_bin[ibin1]+bin1_size)
  IF index1[0] NE -1 THEN densite[ibin1,*] = histogram(var2_den[index1], binsize = bin2_size, min = bin2_deb, nbins = bin2_num)
ENDFOR

;-------------------------------------------------------------------------------------------------------------------------

computegrid,bin1_deb+bin1_size/2.,bin2_deb+bin2_size/2.,bin1_size,bin2_size,bin1_num,bin2_num
denlog = alog10(densite)
denlog[where(finite(denlog) EQ 0)] = 0
moy = hist_2D_moment_bin(var1_den,var2_den,glamt[*,0],tmoment='moy')
ifin = where(finite(moy[0,*]) EQ 1)
fit_a = regress(glamt[ifin],reform(moy[0,ifin]),const=fit_b)

pathfig = '/usr/'+machine+'/varclim/gslod/IDL/COLLOCALISATION/FIGS_PDF/'
spawn, 'echo "creation du repertoire '+ pathfig +'"' & spawn, 'mkdir -p '+ pathfig

plt, denlog/max(denlog), 0., 1., /rempli, xrange=[bin1_deb, bin1_fin], yrange=[bin2_deb, bin2_fin], $
xtitle = var1_name+' '+var1_unit, ytitle = var2_name+' '+var2_unit, $
ytickformat = '', xtickformat = '', subtitle='', title=var1_name+'-'+var1_type+' VS '+var2_name+'-'+var2_type+' - '+expname
oplot, glamt[*, 0], moy[0, *], thick = 2
oplot, glamt[*,0],fit_b[0]+fit_a[0]*glamt[*,0],thick = 2
saveimage, pathfig + 'pdf_'+var1_name+var1_type+'-'+var2_name+var2_type+'_'+expname+'_'+freq+'_'+basin+'.gif'

ENDFOR
ENDFOR
END
