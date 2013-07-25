PRO plot_composite_1D
; plot dune serie temporelle composite par rapport aux positions des cyclones selectionnes
@common


; parametres
explist  = ['IBTRACS'];,'COUPLED_SW2_KF','FORCED_SW2_KF']
varname  = 'SST'    ; variable a tracer en fn du temps
basin    = 'SIO'
freq     = '1D'
dt_bnd   = [-10,40] ; intervalle temporel en jours par rapport au passage du cyclone a t=0 (axe x)
write_ps = 0


; declarations
varmoy = fltarr(n_elements(explist),dt_bnd[1]-dt_bnd[0]+1)   & help, varmoy
varstd = fltarr(n_elements(explist),dt_bnd[1]-dt_bnd[0]+1)   & help, varstd
varper = fltarr(n_elements(explist),dt_bnd[1]-dt_bnd[0]+1,2) & help, varper
varcnt = intarr(n_elements(explist),1)
vartmt = fltarr(dt_bnd[1]-dt_bnd[0]+1,2) & help, vartmt
varrst = fltarr(dt_bnd[1]-dt_bnd[0]+1,2) & help, varrst


; loop exp
FOR iexp = 0, n_elements(explist)-1 DO BEGIN

  expname = explist[iexp] & help, expname
  IF expname EQ 'FORCED_SW2_KF' THEN period  = '1990-1998' ELSE period  = '1990-2009' & help, period
  IF expname EQ 'FORCED_SW2_KF' THEN dateend = '19990101' ELSE dateend = '20100101' & help, dateend
  IF expname EQ 'IBTRACS' THEN datebeg = '19980101' ELSE datebeg = '19900101' & help, datebeg
  IF expname EQ 'IBTRACS' THEN period = '1998-2009'

  ; lecture tracks + fichier collocalise

  ; OLD VERSION
;  pathtc  = '/net/adonis/usr/adonis/varclim/gslod/IDL/COLLOCALISATION/EXP_'+expname+'/DATA/' & help, pathin
;  restore, pathtc+'d1_TRACKS_'+expname+'_IO_'+datebeg+'-'+dateend+'.dat', /VERBOSE

  ; NEW VERSION
  IF expname EQ 'IBTRACS' THEN BEGIN
    pathtc  = '/net/adonis/usr/adonis/varclim/gslod/IDL/TRACKER/EXP_'+expname+'/DATA/' & help, pathin
    restore, pathtc+'d1_'+expname+'v03r03_WPI_IO_'+period+'.dat', /VERBOSE
  ENDIF ELSE BEGIN
    pathtc  = '/net/adonis/usr/adonis/varclim/gslod/IDL/COLLOCALISATION/EXP_'+expname+'/DATA/' & help, pathin
    restore, pathtc+'d1_TRACKS_'+expname+'_IO_'+datebeg+'-'+dateend+'.dat', /VERBOSE
  ENDELSE


  IF expname EQ 'IBTRACS' AND varname EQ 'SST' THEN BEGIN
    pathin = '/net/adonis/usr/adonis/varclim/gslod/IDL/COLLOCALISATION/EXP_TMI-AMSRE/DATA/' & help, pathin
    restore, pathin+varname+'_1D_200km_TMI-AMSRE_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat', /VERBOSE
  ENDIF ELSE BEGIN
    pathin = '/net/adonis/usr/adonis/varclim/gslod/IDL/COLLOCALISATION/EXP_'+expname+'/DATA/' & help, pathin
    restore, pathin+varname+'_1D_200km_'+expname+'_'+freq+'_IO_'+datebeg+'-'+dateend+'.dat', /VERBOSE
  ENDELSE


  ; var 1D = anomalie par rapport a "before" = (nbcyc,nbpts,temps)
  dt_var = dt_var_ano_bef[*,*,where(dt_axis GE dt_bnd[0] AND dt_axis LE dt_bnd[1])] & help, dt_var

  IF basin EQ 'SIO' THEN  iok = where(d1_lon GT 30. AND d1_lon LT 130. AND d1_lat LT 0., cntok)
  IF basin EQ 'NIO' THEN  iok = where(d1_lon GT 50. AND d1_lon LT 100. AND d1_lat GT 0., cntok)
  IF basin EQ 'SWIO' THEN iok = where(d1_lon GT 40. AND d1_lon LT 80. AND d1_lat LT 0., cntok)
  IF basin EQ 'SEIO' THEN iok = where(d1_lon GT 80. AND d1_lon LT 130. AND d1_lat LT 0., cntok)
  IF basin EQ 'DOME' THEN iok = where(d1_lon GT 40. AND d1_lon LT 80. AND d1_lat LT 0. AND d1_lat GT -15., cntok)
  IF basin EQ 'PLAT' THEN iok = where(d1_lon GT 110. AND d1_lon LT 130. AND d1_lat LT 0., cntok)
  varcnt[iexp,0] = cntok

  ; var(nbcyc,nbpts,temps) -> var(nbcyc*nbpts,temps)
  ijok = array_indices([(size(d1_lon))[1], (size(d1_lon))[2]], iok, /dim)
  var = fltarr(n_elements(iok), n_elements(dt_var[0,0,*])) & help, var
  FOR itmp = 0, n_elements(iok)-1 DO var[itmp,*] = dt_var[ijok[0,itmp],ijok[1,itmp],*]
  tmp = execute('var'+strtrim(iexp,2)+'= var & help, var'+strtrim(iexp,2))

  ; calcul moyenne + stddev + quartiles
  varmoy[iexp,*] = m_mean(var, dim = 1, /nan) & help, varmoy
  FOR itmp = 0, n_elements(var[0,*])-1 DO BEGIN
    varstd[iexp,itmp]   = stddev(var[*,itmp], /nan)
    varper[iexp,itmp,*] = percentile(var[*,itmp],0.25)
  ENDFOR

ENDFOR

; calcul incertitudes
FOR itmp = 0, n_elements(var[0,*])-1 DO BEGIN
  iok0 = where(finite(var0[*,itmp]) EQ 1)
  iok1 = where(finite(var1[*,itmp]) EQ 1)
  varok0 = var0[iok0,itmp]
  varok1 = var1[iok1,itmp]
  vartmt[itmp,*] = tm_test(varok0[*],varok1[*],/unequal)
  varrst[itmp,*] = rs_test(varok0[*],varok1[*])
ENDFOR
tmtok = fltarr(n_elements(var[0,*]))
rstok = fltarr(n_elements(var[0,*]))
tmtok[where(vartmt[*,1] LT 0.05)] = 1 & print, tmtok
rstok[where(varrst[*,1] LT 0.05)] = 1 & print, rstok

STOP


; PLOT 1D
reinitplt
SET_PLOT, 'X'
DEVICE, decomposed=0, retain=0
lct, 33

IF write_ps THEN thc = 6 ELSE thc = 2
IF write_ps THEN openps, filename='COOLING_OBS_KF_'+basin+'_NEW.ps'

dt_axis = dt_axis[where(dt_axis GE dt_bnd[0] AND dt_axis LE dt_bnd[1])]
ymin = -1.2   ;floor(min(varmoy,/nan))
ymax =  0.5   ;ceil(max(varmoy,/nan))

splot, dt_axis, varmoy[0,*], color=0, thick=thc, $
xrange=[dt_bnd[0],dt_bnd[1]], yrange=[ymin,ymax], xtitle='time relative to TC passage (days)', ytitle='cooling (degC)', xstyle=1, ystyle=1
oplot, dt_axis, varmoy[1,*], color=50, thick=thc
oplot, dt_axis, varmoy[2,*], color=225, thick=thc
oplot, [dt_bnd[0],dt_bnd[1]], [0,0]
oplot, [0,0],[ymin,ymax]
errplot, dt_axis[0:*:2], varper[0,0:*:2,0], varper[0,0:*:2,1], color=  0, thick=thc
errplot, dt_axis[0:*:2], varper[1,0:*:2,0], varper[1,0:*:2,1], color= 50, thick=thc
;errplot, dt_axis[0:*:2], varper[2,0:*:2,0], varper[2,0:*:2,1], color=225, thick=thc

xyouts, dt_axis , -1.15, strtrim(long(tmtok),2), align=0.5
xyouts, 0.75, 0.20, 'IBTRACS', /normal, charsize=1.5, color=0  ;  ('+strtrim(varcnt[0,0],2)+' pts)
xyouts, 0.75, 0.15, 'COUPLED', /normal, charsize=1.5, color=50
xyouts, 0.75, 0.10, 'FORCED', /normal, charsize=1.5, color=225
;saveimage, 'COOLING_OBS_KF_'+basin+'.gif'

IF write_ps THEN closeps & STOP


END
