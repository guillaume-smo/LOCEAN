FUNCTION spectra, datin, nyears, tsamp, tukey, running, full

; --------------------------------------------------------------------------
;  Program to extract data, compute anomaly, do time series analysis.
;  Computes detrended data using a linear regression and
;  produces autocovariances and correlograms.
;  Then computes spectrum with and without
;  the Tukey window for various values of m.
;
;  Adapted by Eric Guilyardi from a fortran program by Julia Slingo
;  (c) CGAM January 2002
 
; Input:
; ------
;      datin : 1D input data (monthly time serie)
;      nyears: number of years in data
;      tukey : tukey window in years
;      tsamp : number of timestep in one year
;      running: running window in years for standard deviation
;               variations
;      full: 1 = no anomaly, 0: anomaly wrt SC

; Output:
; -------
;     spectra: structure { std: standard deviation
;                          mean: mean of serie
;                          sc: mean seasonal cycle
;                          sc_range: range of mean SC
;                          sc_std: seasonal cycle STD
;                          anom: anomaly time serie
;                          spec: spectrum
;                          spectw: normalized spectrum with Tukey window
;                          time: spectra time array   
;                          fmax: frequency of maximum power 
;                          run_std: running standard deviation
;                          time_std: associated time axis 
;                        } 
;
; usage : field=spectra(datin, nyears, tukey)
;         then field.std is the standard deviation
;              field.spectw the normalized spectrum, etc...
;
;; --------------------------------------------------------------------------
;
; 0. Initializations
; ------------------

   ntime = nyears*tsamp            ; size of time serie
   lfreq = ntime/2              ; number of wavelenghts
   ntim1=ntime-1

   period = fltarr(lfreq)       ; frequency axis
   amean = fltarr(tsamp)           ; mean seasonal cycle
   datad = fltarr(ntime)        ; anomaly data
   stdsc = fltarr(tsamp)           ; Seasonal cycle of STD DEV
   weight = fltarr(ntime+1)
   cx = fltarr(ntim1+1)
   rx = fltarr(ntim1+1)
   powerux = fltarr(lfreq)      ; spectra
   powerwx = fltarr(lfreq)      ; spectra with Tukey window

   run_std = fltarr(ntime-(running*tsamp)+1)              ; time serie of STD DEV
   time_std = (findgen(ntime-(running*tsamp)+1)+(running*tsamp)/2)/tsamp

   pi = 3.141592654
   twopin=2.0*pi/float(ntime)
   pirm=pi/float(ntime)
   
   period = (float(ntime)/(float(findgen(lfreq))+1.))/tsamp

;
; 1. Compute monthly means and anomalies
; --------------------------------------
;
; mean of time serie
;
   smean = mean(datin) & help, smean
;
; mean seasonal cycle
;
   FOR t = 0, tsamp-1 DO BEGIN
;      print, long(findgen(ntime/tsamp)*tsamp+t)
      amean(t) = mean(datin(long(findgen(ntime/tsamp)*tsamp+t)))
   ENDFOR
   help, amean
;
; range of mean SC
;
   sc_range = max(amean)-min(amean)
;
; interannual anomaly
;
   CASE full OF
      '0': datad = datin - reform(amean#replicate(1, long(ntime/tsamp)), ntime)
      '1': datad = datin
   ENDCASE
   help, datad
   xmean = mean(datad)
;
; standard deviation
;
   sdev = sqrt((moment(datad))[1]) & help, sdev
;
; standard deviation in running window
;
   print, ntime-(running*tsamp)
   FOR t = 0, ntime-(running*tsamp) DO BEGIN 
      run_std(t) = sqrt((moment(datad[t:t+(running*tsamp)-1]))[1])
   ENDFOR 
;   run_std = smooth(run_std, running*tsamp)
;   run_std[0:(running*tsamp)/2] = 0.
;   run_std[ntime-(running*tsamp)/2, ntime-1] = 0.
;
; standard deviation SC
;
   FOR t = 0, tsamp-1 DO BEGIN
      stdsc(t) = sqrt((moment(datin(long(findgen(ntime/tsamp)*tsamp+t))))[1])
   ENDFOR
;
; Normalize data by standard deviation
;
   datad = datad-xmean
   datad = datad/sdev

;
; 2. Spectra
; ----------
;
; Compute autocovariance coefficients
;
   FOR k = 0, ntim1 DO BEGIN
      cx(k) = (total(datad(0:ntime-k-1)*(shift(datad, -k))(0:ntime-k-1)))/float(ntime)
   ENDFOR
;   
; Compute autocorrelations for plotting
;   
   rx = cx/cx(0)
;
;  Compute the spectrum with no windowing
;
   FOR i=0,lfreq-2 DO BEGIN
      omega=twopin*float(i+1)
      powerux(i) = (cx(0)+total((2.0*shift(cx, -1)*cos(omega*float(findgen(ntim1)+1)))(0:ntim1-1)))/pi
   ENDFOR
;
; Compute the spectrum with windowing
;
   mm=tukey*tsamp
   pirm=pi/float(mm)
;
; compute the weights for the Tukey window
;
   weight=0.5*(1.0+cos(pirm*findgen(mm+1)))
;
; Compute windowed spectrum
;
   FOR i=0,lfreq-2 DO BEGIN 
      omega=twopin*float(i+1)
      powerwx(i) = (weight(0)*cx(0)+total((2.0*shift(weight, -1)*shift(cx, -1)*cos(omega*float(findgen(mm)+1)))(0:mm-1)))/pi
   ENDFOR

   pmax = max(powerwx)

;
; Find max amplitude freq
;
   fmax = max(powerwx/pmax)
   index = where(powerwx/pmax EQ fmax)
   fmax = period(index) & help, fmax

;
; 3. Organise output
; ------------------
;

   spectra = {std:sdev, mean:smean, sc:amean, sc_range:sc_range, sc_std:stdsc, anom:datad, spec:powerux, spectw:powerwx/pmax, time:period, fmax:fmax, run_std:run_std, time_std:time_std}

   return, spectra

END 
