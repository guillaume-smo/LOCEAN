PRO calcul_fft, x, data_type, pwrspec, period

  IF data_type EQ '6h' THEN dt = 0.25
  IF data_type EQ '1d' THEN dt = 1.00

  n = n_elements(x)
 
  hwindow = HANNING(n, ALPHA=1, /DOUBLE)
  ;pwrspec = ABS(fft(x))^2
  pwrspec = ABS(FFT(hwindow*x))^2
  freq    = FINDGEN(n)/(n*dt)
  period  = 1. / freq
 
END
