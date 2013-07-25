PRO plot_traj_brut


expname  = 'FORCED_SW2_KF'
ucrit    = 17.5
vorcrit  = 30.e-5
vorelax  = 30.e-5
temptype = 'treal'
tempcrit = 1. 
period   = '1990-2009'
nbyear   = 20
datebeg  = 19900101d
dateend  = 20100101d


IF expname EQ 'IBTRACS' THEN BEGIN
  @read_ibtracs.pro
ENDIF ELSE BEGIN
  path_data = '/net/adonis/usr/adonis/varclim/gslod/IDL/TRACKER/EXP_'+ expname +'/DATA/' & help, path_data 
  file_in   = 'tracker_'+temptype+'_new_cst_radius_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2) +'_'+ period +'.idl' & help, file_in
  restore, filename = path_data + file_in, /VERBOSE
  monthcycn = long(datecycn/100.) - long(datecycn/10000.)*100.
  yearcycn  = long(datecycn/10000.)
ENDELSE

isio = where(loncycn[*,0] GE 30 AND loncycn[*,0] LE 140 AND latcycn[*,0] GE -30 AND latcycn[*,0] LE 0, cntsio)
inio = where(loncycn[*,0] GE 30 AND loncycn[*,0] LE 100 AND latcycn[*,0] GE   0 AND latcycn[*,0] LE 30, cntnio)
iok  = [isio,inio]
loncycn = loncycn[iok,*]
latcycn = latcycn[iok,*]
uv10cycn = uv10cycn[iok,*]
yearcycn = yearcycn[iok,*]
monthcycn = monthcycn[iok,*]

;path_fig = 'EXP_'+ expname +'/FIGS_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2) +'/TRAJ_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2) +'/'
;spawn, 'echo "creation du repertoire '+ path_fig +'"' & spawn, 'mkdir -p '+ path_fig



;-----------------------------------------------------------------------------------------
; plot trajectoires


initncdf, '/usr/home/gslod/IDL/MASK_WRF_IND4_GRIDT.nc'
domdef, [30,130,-30,30]
@cm_4mesh
SET_PLOT, 'X'
DEVICE, decomposed=0, retain=0
lct, 33
min_wind =  0.
max_wind = 50.

;plt, glamt, /nodata, /realcont, min=min_wind, max=max_wind, title='', subtitle='', xtitle='', ytitle=''

FOR k = 0, nbyear-1 DO BEGIN

y      = 1990 + k
iyear  = where(yearcycn[*,0] EQ y)
isioy  = intersect(iyear,isio)
inioy  = intersect(iyear,inio)
cntsio = n_elements(isioy)
cntnio = n_elements(inioy)
iok    = [isioy,inioy]
cntok  = n_elements(iok)
print, y, cntok, cntsio, cntnio

plt, glamt, /nodata, /realcont, min=min_wind, max=max_wind, title=expname+' '+temptype+' '+strtrim(y,2)+': '+strtrim(cntok,2)+' TCs ('+strtrim(cntsio,2)+'SI+'+strtrim(cntnio,2)+'NI)', subtitle='', xtitle='', ytitle=''

FOR i = 0, n_elements(uv10cycn[iok,0])-1 DO BEGIN

  indok = where(finite(uv10cycn[iok[i],*]) EQ 1, cntok)

  IF cntok GE 8 THEN BEGIN

    IF expname NE 'IBTRACS' THEN BEGIN  
      lons = smooth(reform(loncycn[iok[i],indok]),7,/nan)
;      help, loncycn[iok[i],indok], lons
      lats = smooth(reform(latcycn[iok[i],indok]),7,/nan)
;      help, latcycn[iok[i],indok], lats
      uv10s = smooth(reform(uv10cycn[iok[i],indok]),7,/nan)
    ENDIF ELSE BEGIN
      lons = loncycn[iok[i],indok]
      lats = latcycn[iok[i],indok]
      uv10s = uv10cycn[iok[i],indok]
    ENDELSE

;  domdef, [floor(min(loncycn[iok[i],*], /nan)),ceil(max(loncycn[iok[i],*], /nan)),floor(min(latcycn[iok[i],*], /nan)),ceil(max(latcycn[iok[i],*], /nan))]
;  plt, glamt, /nodata, /realcont, min=min_wind, max=max_wind

    FOR j = 0, cntok-2 DO BEGIN
;      oplot, [loncycn[iok[i],indok[j]],loncycn[iok[i],indok[j+1]]] , [latcycn[iok[i],indok[j]],latcycn[iok[i],indok[j+1]]], $
;      color= min([(uv10cycn[iok[i],indok[j]]-min_wind)/(max_wind-min_wind)*255,255]), thick = 2
      oplot, [lons[indok[j]],lons[indok[j+1]]] , [lats[indok[j]],lats[indok[j+1]]], $
      color= min([max([uv10s[indok[j]]-min_wind,0])*255/max_wind,255]), thick=2
    ENDFOR
;    xyouts, loncycn[i, indok[0]], latcycn[i, indok[0]], strtrim(long(datecycn[i,indok[0]]), 2), charsize = 1.5
;    xyouts, loncycn[i, indok[cntok-1]], latcycn[i, indok[cntok-1]], strtrim(long(datecycn[i,indok[cntok-1]]), 2), charsize = 1.5
;    saveimage, path_fig + 'traj_light_RVM_NICO_MASK_'+ strtrim(long(datecycn[i,indok[0]]), 2) +'_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2) +'.gif'
  ENDIF

ENDFOR

stop
ENDFOR

STOP

;ENDFOR; mois


;-----------------------------------------------------------------------------------------
; quelques chiffres

;print, 'HS'
;qsd = where(latcycn[*,0] LE 0., cnt)
;print, 'NOMBRE DE cycn > '+ strtrim(long(ucrit),2) +' m/s :', cnt
;aze = where(uv10cycn gt 20 AND latcycn LE 0., cnt)
;print, 'NOMBRE DE JOURS cycnLONIQUES > 20 m/s:', cnt/4
;aze = where(uv10cycn gt 22 AND latcycn LE 0., cnt)
;print, 'NOMBRE DE JOURS cycnLONIQUES > 22 m/s:', cnt/4
;
;print, 'HN'
;qsd = where(latcycn[*,0] GE 0., cnt)
;print, 'NOMBRE DE cycn > '+ strtrim(long(ucrit),2) +' m/s :', cnt
;aze = where(uv10cycn gt 20 AND latcycn GE 0., cnt)
;print, 'NOMBRE DE JOURS cycnLONIQUES > 20 m/s:', cnt/4
;aze = where(uv10cycn gt 22 AND latcycn GE 0., cnt)
;print, 'NOMBRE DE JOURS cycnLONIQUES > 22 m/s:', cnt/4

;ENDFOR; annee

END
