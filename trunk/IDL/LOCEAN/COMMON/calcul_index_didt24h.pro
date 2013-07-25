PRO calcul_index_didt24h,d1_pres,d1_max_wnd,ind_int24h,ind_dec24h,d1_wind_smooth_24h,dwinddt,dpresdt,i_int24h,i_dec24h

;----------------------------------------------
; intensification/decay continue sur 24h (4 pts) 
;-----------------------------------------------

; declarations
nb_cyc = n_elements(d1_pres[*,0])
nb_ech = n_elements(d1_pres[0,*])
ipres_int24h = d1_pres * 0.
iwind_int24h = d1_pres * 0.
ipres_dec24h = d1_pres * 0.
iwind_dec24h = d1_pres * 0.
dpresdt = d1_pres * 0.
dwinddt = d1_pres * 0.
d1_wind_smooth_12h = d1_pres * 0.
d1_wind_smooth_24h = d1_pres * 0.
dpresdt_smooth_12h = d1_pres * 0.
dpresdt_smooth_24h = d1_pres * 0.
dwinddt_smooth_12h = d1_pres * 0.
dwinddt_smooth_24h = d1_pres * 0.


FOR i = 0, nb_cyc-1 DO BEGIN

  ; lissage vent
;  d1_wind_smooth_12h[i,*] = ts_smooth(reform(d1_max_wnd[i,*]),3)
  d1_wind_smooth_24h[i,*] = ts_smooth(reform(d1_max_wnd[i,*]),5)

  ; calcul taux d'intensification sur 6h forward
;  iok = where(finite(d1_pres[i,*]) EQ 1, cntok)
;  FOR j = 0, cntok-2 DO BEGIN
;    dpresdt[i,iok[j]] = d1_pres[i,iok[j+1]] - d1_pres[i,iok[j]]
;    dwinddt[i,iok[j]] = d1_wind_smooth_24h[i,iok[j+1]] - d1_wind_smooth_24h[i,iok[j]]
  FOR j = 0, nb_ech-2 DO BEGIN
    dpresdt[i,j] = d1_pres[i,j+1] - d1_pres[i,j]
;    dwinddt[i,j] = d1_max_wnd[i,j+1] - d1_max_wnd[i,j]
;    dwinddt[i,j] = d1_wind_smooth_12h[i,j+1] - d1_wind_smooth_12h[i,j]
    dwinddt[i,j] = d1_wind_smooth_24h[i,j+1] - d1_wind_smooth_24h[i,j]
  ENDFOR
  dwinddt_smooth_12h[i,*] = ts_smooth(reform(dwinddt[i,*]),3)
;  dwinddt_smooth_24h[i,*] = ts_smooth(reform(dwinddt[i,*]),5)
  dwinddt = dwinddt_smooth_12h

  ; index intensification/decay sur 6h
  ; forward + backward
;  i_int = where(d1_pres[i,*] GT shift(d1_pres[i,*],-1) OR d1_pres[i,*] LT shift(d1_pres[i,*],1))
;  i_dec = where(d1_pres[i,*] LT shift(d1_pres[i,*],-1) OR d1_pres[i,*] GT shift(d1_pres[i,*],1))
  ; backward
;  i_int = where(d1_pres[i,*] LT shift(d1_pres[i,*],1))
;  i_dec = where(d1_pres[i,*] GT shift(d1_pres[i,*],1))
  ; forward
  ipres_int = where(dpresdt[i,*] LT 0.)
  ipres_dec = where(dpresdt[i,*] GT 0.)
  iwind_int = where(dwinddt[i,*] GT 0.)
  iwind_dec = where(dwinddt[i,*] LT 0.)

 
  ; intensification sur 24h consecutives
  FOR j = 0, n_elements(ipres_int)-4 DO BEGIN
    IF ipres_int[j]+1 EQ ipres_int[j+1] AND ipres_int[j]+2 EQ ipres_int[j+2] AND ipres_int[j]+3 EQ ipres_int[j+3] AND $
    d1_pres[i,ipres_int[j]] GT d1_pres[i,ipres_int[j+1]] AND d1_pres[i,ipres_int[j+1]] GT d1_pres[i,ipres_int[j+2]] AND d1_pres[i,ipres_int[j+2]] GT d1_pres[i,ipres_int[j+3]] THEN BEGIN $
    ipres_int24h[i,ipres_int[j]] = 1 & ipres_int24h[i,ipres_int[j+1]] = 1 & ipres_int24h[i,ipres_int[j+2]] = 1 & ipres_int24h[i,ipres_int[j+3]] = 1
    ENDIF
  ENDFOR

  FOR j = 0, n_elements(iwind_int)-4 DO BEGIN
    IF iwind_int[j]+1 EQ iwind_int[j+1] AND iwind_int[j]+2 EQ iwind_int[j+2] AND iwind_int[j]+3 EQ iwind_int[j+3] AND $
    d1_wind_smooth_24h[i,iwind_int[j]] LT d1_wind_smooth_24h[i,iwind_int[j+1]] AND d1_wind_smooth_24h[i,iwind_int[j+1]] LT d1_wind_smooth_24h[i,iwind_int[j+2]] AND d1_wind_smooth_24h[i,iwind_int[j+2]] LT d1_wind_smooth_24h[i,iwind_int[j+3]] THEN BEGIN $
    iwind_int24h[i,iwind_int[j]] = 1 & iwind_int24h[i,iwind_int[j+1]] = 1 & iwind_int24h[i,iwind_int[j+2]] = 1 & iwind_int24h[i,iwind_int[j+3]] = 1
    ENDIF
  ENDFOR


  ; decay sur 24h consecutives
  FOR j = 0, n_elements(ipres_dec)-4 DO BEGIN
    IF ipres_dec[j]+1 EQ ipres_dec[j+1] AND ipres_dec[j]+2 EQ ipres_dec[j+2] AND ipres_dec[j]+3 EQ ipres_dec[j+3] AND $
    d1_pres[i,ipres_dec[j]] LT d1_pres[i,ipres_dec[j+1]] AND d1_pres[i,ipres_dec[j+1]] LT d1_pres[i,ipres_dec[j+2]] AND d1_pres[i,ipres_dec[j+2]] LT d1_pres[i,ipres_dec[j+3]] THEN BEGIN $
    ipres_dec24h[i,ipres_dec[j]] = 1 & ipres_dec24h[i,ipres_dec[j+1]] = 1 & ipres_dec24h[i,ipres_dec[j+2]] = 1 & ipres_dec24h[i,ipres_dec[j+3]] = 1
    ENDIF
  ENDFOR

  FOR j = 0, n_elements(iwind_dec)-4 DO BEGIN
    IF iwind_dec[j]+1 EQ iwind_dec[j+1] AND iwind_dec[j]+2 EQ iwind_dec[j+2] AND iwind_dec[j]+3 EQ iwind_dec[j+3] AND $
    d1_wind_smooth_24h[i,iwind_dec[j]] GT d1_wind_smooth_24h[i,iwind_dec[j+1]] AND d1_wind_smooth_24h[i,iwind_dec[j+1]] GT d1_wind_smooth_24h[i,iwind_dec[j+2]] AND d1_wind_smooth_24h[i,iwind_dec[j+2]] GT d1_wind_smooth_24h[i,iwind_dec[j+3]] THEN BEGIN $
    iwind_dec24h[i,iwind_dec[j]] = 1 & iwind_dec24h[i,iwind_dec[j+1]] = 1 & iwind_dec24h[i,iwind_dec[j+2]] = 1 & iwind_dec24h[i,iwind_dec[j+3]] = 1
    ENDIF
  ENDFOR

ENDFOR


; index final
print, n_elements(d1_pres)
indpres_int24h = where(ipres_int24h EQ 1) & help, indpres_int24h
indpres_dec24h = where(ipres_dec24h EQ 1) & help, indpres_dec24h
indwind_int24h = where(iwind_int24h EQ 1) & help, indwind_int24h
indwind_dec24h = where(iwind_dec24h EQ 1) & help, indwind_dec24h
ind_int24h = where(ipres_int24h EQ 1 AND iwind_int24h EQ 1) & help, ind_int24h
ind_dec24h = where(ipres_dec24h EQ 1 AND iwind_dec24h EQ 1) & help, ind_dec24h


; verification
IF (where(dpresdt[indpres_int24h] GE 0.))[0] NE -1 THEN stop
IF (where(dpresdt[indpres_dec24h] LE 0.))[0] NE -1 THEN stop
IF (where(dpresdt[ind_int24h] GE 0.))[0] NE -1 THEN stop
IF (where(dpresdt[ind_dec24h] LE 0.))[0] NE -1 THEN stop
IF (where(dwinddt[indwind_int24h] LE 0.))[0] NE -1 THEN stop
IF (where(dwinddt[indwind_dec24h] GE 0.))[0] NE -1 THEN stop
IF (where(dwinddt[ind_int24h] LE 0.))[0] NE -1 THEN stop
IF (where(dwinddt[ind_dec24h] GE 0.))[0] NE -1 THEN stop


; tests correlations vent-pression
print, correlate(d1_pres[ind_int24h],d1_max_wnd[ind_int24h])
print, correlate(d1_pres[ind_dec24h],d1_max_wnd[ind_dec24h])

;iok = where(finite(d1_wind_smooth_12h[ind_int24h]) EQ 1 AND finite(d1_pres[ind_int24h]) EQ 1) & help, iok
;print, correlate(d1_pres[ind_int24h[iok]],d1_wind_smooth_12h[ind_int24h[iok]])

iok = where(finite(d1_wind_smooth_24h[ind_int24h]) EQ 1 AND finite(d1_pres[ind_int24h]) EQ 1) & help, iok
print, correlate(d1_pres[ind_int24h[iok]],d1_wind_smooth_24h[ind_int24h[iok]])

;iok = where(finite(d1_wind_smooth_12h[ind_dec24h]) EQ 1 AND finite(d1_pres[ind_dec24h]) EQ 1) & help, iok
;print, correlate(d1_pres[ind_dec24h[iok]],d1_wind_smooth_12h[ind_dec24h[iok]])

iok = where(finite(d1_wind_smooth_24h[ind_dec24h]) EQ 1 AND finite(d1_pres[ind_dec24h]) EQ 1) & help, iok
print, correlate(d1_pres[ind_dec24h[iok]],d1_wind_smooth_24h[ind_dec24h[iok]])

END
