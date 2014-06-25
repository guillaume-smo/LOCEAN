PRO filter_sc, ts, sc ;, coef

;----------------------------------------------------------------------------------------;
; input  : ts   (time series to decompose, can have missing values, assumed to be daily)
; output : sc   (seasonal cycle, based on a least-square fit of two or three or four harmonics)
;        : coef (fitted values of coef used in function ann_harmo)
;----------------------------------------------------------------------------------------;

  nt  = n_elements(ts)
  sc  = fltarr(nt) + !values.f_nan
  t   = FINDGEN(nt)
  iok = where( finite( ts), nbok)
  IF (nbok le 365.*4.) THEN print, 'WARNING: LESS THAN 4 YEARS OF DATA : ', nbok / 365

  t  = TEMPORARY( t[iok])
  ts = TEMPORARY(ts[iok])
  m  = MOMENT(t) ; mean
  s  = m[1]      ; variance

  ; least-square fit of two harmonics
  ;a=[ m(0) ,s*.75, s*.25, s*.1, s*.05]    ; first-guess values for coefs
  ;res=svdfit(t_d,ts,a=a,function_name='ann_harmo2',yfit=ts_fit)

  ; least-square fit of three harmonics
  a = [ m(0), s*.75, s*.25, s*.1, s*.05, s*.05, s*.025]    ; first-guess values for coefs
  coef = SVDFIT( t, ts, a=a, function_name='ann_harmo3', yfit=ts_fit)

  ; least-square fit of four harmonics
  ;a = [ m[0], s*.75, s*.25, s*.1, s*.05, s*.05, s*.025, s*.025, s*.0125]    ; first-guess values for coefs
  ;coef = SVDFIT( t, ts, a=a, function_name='ann_harmo4', yfit=ts_fit)

  sc[iok] = ts_fit

END
