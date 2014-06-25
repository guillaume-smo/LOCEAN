FUNCTION time_filter, tab, taxis, Thf, Tlf, tsamp=tsamp, valmask=valmask, cut=cut


; tab is a 3D array (x,y,t) (the field to filter)
; taxis is the time-axis (it should be evenly spaced)
; Thf is the high-frequency cutoff (expressed in same unit than time axis)
; Tlf is the  low-frequency cutoff (expressed in same unit than time axis)
; if the tsamp keyword is defined, it is assumed to be the uniform sampling interval, otherwise, it is computed from taxis
; valmask is the value of missing data (assumed to be always missing at same locations): infinite (NaN) values if not specified
; if the cut keyword is activated, both ends of tab result and taxis are truncated of Tlf/2 points


  ; define tab dimensions
  si = SIZE(tab)
  nx = si[1]
  ny = si[2]
  nt = si[3]
  help, tab, nx, ny, ny

  ; check time dimension
  IF n_elements(taxis) NE nt THEN BEGIN
    print, 'The first two arguments of time_filter need to have the same number of samples.'
    help, tab, taxis
    STOP
  ENDIF

  ; check time step
  IF NOT keyword_set(tsamp) THEN BEGIN
    dt  = taxis[1:nt-1] - taxis[0:nt-2]
    dt1 = MIN( dt, MAX = dt2)
    IF dt1 NE dt2 THEN BEGIN
      print, 'The time sampling needs to be uniform.'
      print, 'Min sampling interval ',dt1
      print, 'Max sampling interval ',dt2
      STOP
    ENDIF
    tsamp = TEMPORARY(dt1)
  ENDIF
  help, tsamp

  ; define frequence and period axis & valid ranges
  freq = FINDGEN(nt/2+1) / (nt*tsamp)
  freq = [freq,-1.*REVERSE(freq[1:nt/2])]
  peri = 1. / (ABS(freq) > 1.e-20) * (2 * (freq GE 0) - 1.)
  fmask = FLOAT((ABS(peri) GE Thf) AND (ABS(peri) LE Tlf))
  help, freq, peri, fmask

  ; vectorize array & select valid data
  tab2 = REFORM( TEMPORARY(tab), nx*ny, nt)
  IF n_elements(valmask) EQ 0 THEN BEGIN
    iok = WHERE( FINITE( tab2[*,0] ), nbok)
    valmask = !values.f_nan
  ENDIF ELSE BEGIN
    iok = WHERE( tab2[*,0] NE valmask, nbok)
  ENDELSE
  tab2r  = TEMPORARY(tab2[iok,*])
  help, tab2r
  print, 'Number of values ', nx*ny
  print, 'Number of valid values ', nbok
  STOP

  ; fft + frequency mask + inverse fft
  tab2rf = tab2r * 0.
  FOR n = 0, nbok-1 DO BEGIN
    print, 'Filtering ', n,' / ', nbok-1
    ts  = REFORM(tab2r[n,*])
    ps  = ABS(FFT(ts))
    psm = ps * fmask
    tab2rf[n,*] = FLOAT( FFT( psm, /INVERSE))
  ENDFOR

  ; put back data in form
  tabf = FLTARR(nx*ny,nt) + valmask
  tabf[iok,*] = TEMPORARY(tab2rf)
  tabf = REFORM( TEMPORARY(tabf), nx, ny, nt)

  ; keep only valid data
  IF keyword_set(cut) THEN BEGIN
    Nm    = Tlf / 2
    taxis = taxis[Nm:nt-1-Nm]
    tabf  = TEMPORARY( tabf[*,*,Nm:nt-1-Nm])
  ENDIF

  RETURN, tabf

END
