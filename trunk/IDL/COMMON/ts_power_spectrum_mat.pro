FUNCTION Chi_sqr, p, d
;#pv-wave @math_startup
;#pv-wave RETURN, chisqcdf(p, d, /inverse)
RETURN,chisqr_cvf(p,d) ;#idl
END

FUNCTION Spcarp, a, deltat, frq, pmax
; evaluates the power spectral density over the frequencies defined in frq
; corresponding to an AR process u with coefficients, a, where
; u_t = a(0)*z_t + a(1)*u_{t-deltat} + a(2)*u_{t-2*deltat} + ... etc.
s=size(a) & p=s(1)-1
s=size(frq) & nfe=s(1)
; set up omega i
omegai=complex(0.,2.*!pi*frq*deltat)
; work out the denominator
sumcof=complex(fltarr(nfe)+1.,fltarr(nfe))
for k=1, p do begin
 sumcof=sumcof-a(k)*exp(-k*omegai)
endfor
denom=(abs(sumcof))^2
; check for psd greater than pmax
denom=denom>a(0)/pmax
; and work out the power spectral density
psd=a(0)^2/denom
return,psd
end

FUNCTION Covfun, X, Y, lag, ibias
; compute the cross-covariance function c(nlags) where nlags=n_elements(lag)
; of a pair of scalar series X(n) & Y(n)
; c(k)=covariance with X leading Y by lag(k)
; if ibias=0 use n in denominator (biased but guaranteed positive-definite)
; if ibias=1 use (n-k) in denominator (unbiased, higher variance estimate)
; uses simple convolution -- fft will be much faster if nlags=O(n)
s=size(X) & n=s(1)
nlags=n_elements(lag)
c=fltarr(nlags)
; print,'ibias:',ibias
for k=0, nlags-1 do begin
; work out the series cross covariance function
; get the limits on the sum
; if k ge 0 then X leads Y, so we start from k, otherwise start from 0
 l1=0>lag(k)
; if k ge 0 then X leads Y, so we end at n-1, otherwise end at n+k-1
 l2=(n-1)<(n+lag(k)-1)
 case ibias of
  0: denom=n 
  1: denom=l2-l1+1
  else: stop,'Unknown ibias flag entered in covfun'
 endcase
; print,lag(k),l1,l2,denom
 c(k)=transpose(X(l1:l2))#Y(l1-lag(k):l2-lag(k))/denom
endfor
return,c
end

FUNCTION Xsdxcf, X1, X2, wfn, cov, dof
; evaluates cross spectral density over the Nyquist interval assuming unit
; sampling rate using the 
; cross-covariance function and a lag window function wfn 
; defined from -mww to mww-1
; X is input as vector
; use biased estimate of autocovariance function, as in Priestley
; wfn entered as a 2*mww vector
; output is a complex vector of rank mww+1 being amplitude and phase
; of cospectrum from frequency 0 to 0.5 at intervals of 1/2*mww
ibias=0
n=n_elements(X1)
; window contains 2*mww elements
mww=n_elements(wfn)/2
; check that mww+1 th element of wfn is unity (lag-0)
if (wfn(mww) ne 1.) then $
 print,'Warning: wfn(mww) ne 1: dof estimates invalid'
; compute the autocovariance function from -mww to mww-1
lag=indgen(2*mww)-mww
cov=covfun(X1,X2,lag,ibias)
; multiply by the window function & take the Fourier transform
cft=fft(cov*wfn,1)
; compute the Cross Spectral Density, correcting signs on Fourier components
; (necessary to get the phases right)
cft=cft*(1.-2.*(findgen(2*mww) mod 2))
amp=abs(cft(0:mww))
phs=atan(imaginary(cft(0:mww)),float(cft(0:mww)))
phs(0)=0.
psd=complex(amp,phs)
; compute the degrees of freedom of the spectral estimate
dof=(2.*n)/total(wfn^2)
; degrees of freedom cannot be less than 2.
dof=dof>2.
return,psd
end

FUNCTION Wfngen, mww, wintyp
; generates a window function for spectral analysis
; wintyp: 1=square, 2=Bartlett, 3=Tukey-Hanning, 4=Parzen
; define the window equal to lag, from -mww to mww-1
; Assymetric window used to keep track of phase information in 
; Fourier transform
wfn=findgen(2*mww)-mww                     
if (wintyp eq 1) then $                            
wfn(*)=1. $                           
else if (wintyp eq 2) then $                            
wfn=1.-abs(wfn)/mww $                                      
else if (wintyp eq 3) then $                            
wfn=0.5*(1.+cos(!pi*wfn/mww)) $                          
else if (wintyp eq 4) then $                            
wfn=(1.-6*(wfn/mww)^2+6*(abs(wfn)/mww)^3)<(2*(1.-abs(wfn)/mww)^3) $
else begin print,'Unknown window code entered' & wfn(*)=1. & end
return,wfn
end

FUNCTION Autocv, X, m
; compute the lag-covariance matrix of a scalar time-series X(n)
s=size(X) & n=s(1)
c=fltarr(m,m)
for k=0, m-1 do begin
; work out the series covariance function
 cvk=transpose(X(0:n-k-1))#X(k:n-1)/(n-k)
; and fill in upper triangle of the covariance matrix
 for j=0, m-k-1 do begin
  c(j,j+k)=cvk
 endfor
endfor
; fill in the lower triangle
for k=0, m-1 do begin
 for j=k+1, m-1 do begin
  c(j,k)=c(k,j)
 endfor
endfor
return,c
end

FUNCTION Invert1, A, istat, pv_wave=pv_wave
; computes the inverse of a square matrix, allowing for 1-D matrices
s=size(A)
if (s(0) gt 2 or s(1) ne s(s(0))) then begin 
 print,'Array dimensions incompatible with invert1:',s
 stop
endif
istat=0
B=A
if (s(1) eq 1) then if (A(0) ne 0.) then B=1./A else istat=1 else $
 if (keyword_set(pv_wave)) then B=invert(A) else B=invert(A,istat)
return,B
end

FUNCTION Prewhi, a, X
; Prewhitening operation
; INPUT
; a=coefficients of AR model
; a(0)=amplitude (std. deviation) of white noise forcing
; if a(0) le 0. then unit-amplitude noise forcing assumed
; a(k)=lag-k correlation
; X=n,m input array
; OUTPUT
; Y=PX, where P is prewhitening matrix.
s=size(a) & p=s(1)-1
s=size(X) & n=s(1)
Y=X
for k=1, p do Y(k:n-1,*)=Y(k:n-1,*)-a(k)*X(0:n-k-1,*)
; if a(0) gt 0. then scale by inverse noise amplitude
if (a(0) gt 0.) then Y=Y/a(0)
return,Y
end

FUNCTION Regarp, X, y, a, yfit, covb, u, z, avspe, istat, pv_wave=pv_wave
; Performs multiple linear regression with a prescribed autoregressive,
; AR(p), noise model. 
; Model: y = Xb + u where 
; u_{t} = z_t + \sum_{k=1}^{p} a(k)*u_{t-k}
; N.B. z_t is not unit-variance -- different notation from the notes, but
; I ran out of letters.
; Version 17-Feb-1995, uniform weighting with no missing values
; No regression constant: input 1s in one column of X for constant term
; Indices follow standard maths notation: i.e. (row,column)
; INPUT
; X = n*m array of independent variable values
; y = n-rank vector of dependent variable values
; a = (p+1)-rank vector of AR model coefficients
; OUTPUT
; function value = b = m-rank vector of regression coefficients, GLS estimates
; if a(0) input le 0. then output a(0) = sqrt(avspe) = est. noise forcing
; yfit = n-rank vector, predict. values of dependent variable in best-fit model
; covb = m*m array of covariances of regression coeffs.
; if a(0) input le 0. covb scaled by avspe (residual variance)
; u = n-rank vector of predicted values of AR noise
; z = n-rank vector of estimated noise forcing (prediction error)
; avspe = average squared prediction error
; istat = 0 if it's all OK, 1 otherwise
if (not keyword_set(pv_wave)) then pv_wave=0
; Work out how big everything is
sx=size(X)
if (sx(0) gt 2) then begin 
 print,'Only 2-D arrays of indep variables allowed' 
 stop
endif
if (sx(0) eq 1) then X=reform(X,n_elements(X),1) 
s=size(X) & n=s(1) & m=s(2)
; Prewhitening step:
Py=prewhi(a,y)
PX=prewhi(a,X)
; Compute inverse Hessian, (X^T P^T P X)^{-1}
; # is matrix multiplication summing over inner index (standard maths)
HI=invert1(transpose(PX)#PX,istat1,pv_wave=pv_wave)
if (istat1 ne 0) then begin
 print,'Warning: singular or near-singular Hessian'
 istat=1
 return,transpose(PX)#PX
endif
; Compute b=(X^T P^T P X)^{-1} X^T P^T P y
b=HI#transpose(PX)#Py
; Compute yfit
yfit=X#b
; Compute u, the AR noise residual
u=y-yfit
; Compute z the white noise forcing of the AR model
z=Py-PX#b
; compute mean square prediction error = 
; unbiased estimate of variance of noise forcing, taking into account 
; no. of independent variables
avspe=total(z^2)/(n-m)
if (a(0) le 0.) then begin 
 a(0)=sqrt(avspe)
; Scale inverse Hessian by avspe
 covb=avspe*HI
endif else begin
; if a(0) gt 0 use input noise forcing amplitude in error estimates
; Scale mean squared prediction error by input noise variance
 avspe=avspe*a(0)^2
; and no need to scale inverse Hessian
 covb=HI
endelse
istat=0
return,b
end






FUNCTION Regarq, X, y, a, yfit, covb, u, z, avspe, rtceq, istat, pv_wave=pv_wave
; Performs multiple linear regression with an unknown autoregressive,
; AR(p), noise model of prescribed order. 
; Model: y = Xb + u where 
; u_{t} = z_t + \sum_{k=1}^{p} a(k)*u_{t-k}
; N.B. z_t is not unit-variance -- different notation from the notes, but
; I ran out of letters.
; Version 17-Feb-1995, uniform weighting with no missing values
; No regression constant: input 1s in one column of X for a constant term
; Indices follow standard maths notation: i.e. (row,column)
; INPUT
; X = n*m array of independent variable values
; y = n-rank vector of dependent variable values
; a = (p+1)-rank vector, input as first guess
; if a(0) not used -- noise forcing variance estimated from data avspe
; OUTPUT
; function value = b = m-rank vector of reg. coeffs., GLS estimates at minimum
; a = output as est. AR model coefficients, a(0) = sqrt(avspe)
; covb = m*m array of covariances of regression coeffs. at minimum
; yfit = n-rank vector, predict. values of dependent variable in best-fit model
; u = n-rank vector of predicted values of AR noise
; z = n-rank vector estimated noise forcing (prediction error)
; avspe = minimum mean square prediction error = total(z^2)/(n-m)
; rtceq = complex vector, rank p = roots of characteristic equation
; istat = 0 for sucessful completion
; istat = 1 if ols estimates were used
; istat = 2 if roots on or inside unit circle
istat=0
if (not keyword_set(pv_wave)) then pv_wave=0
; Work out how big everything is
sx=size(X)
if (sx(0) gt 2) then begin 
 print,'Only 2-D arrays of indep variables allowed' 
 stop
endif
if (sx(0) eq 1) then X=reform(X,n_elements(X),1) 
s=size(X) & n=s(1) & m=s(2)
s=size(a) & p=s(1)-1
rtceq=complex(0,0)
if (p gt 0) then begin
; set the tolerance for the estimation of the AR parameters
 tol=1.e-6/p
; g = gradient of cost-function w.r.t. AR model coefficients
 g=fltarr(p+1)
 for try=1, 40 do begin
; compute value of cost function using routine regarp & a(1:p)
; force routine to recalculate noise variance
  a(0)=0.
  b=regarp(X,y,a,yfit,covb,u,z,avspe,istat1,pv_wave=pv_wave)
  if (istat1 ne 0) then begin
   print,'Error: singular matrix found in regarp'
   goto,fail
  endif
  J=avspe/2.
; compute -ve gradient w.r.t. a(k) (step direction) =
; covariance between z(i) & u(i-k)
; as z => white noise, g => 0.
  for k=1, p do g(k)=transpose(u(0:n-k-1))#z(k:n-1)/(n-k)
; Compute step length = d^2 J / d^2 a_j a_k
; = autocovariance matrix of u (take my word for it)
  h=autocv(u,p)
  status=0
  if (p gt 1) then h=invert1(h,status,pv_wave=pv_wave) else $
  if (h(0) gt 0) then h=1./h else status=1
  if (status ne 0) then begin
   print,'Error: singular matrix found in regarq'
   goto,fail
  endif
; Compute the step = h#g
; g(0) not used, since we don't search over a(0)
  g(1:p)=h#g(1:p)
; print,try,a,b,avspe,g,total(g(1:p)^2)
; Check for convergence
  if (total(g(1:p)^2) lt tol) then goto,escape
  a(1:p)=a(1:p)+g(1:p)
 endfor
 print,'Warning: failed to converge in regarq'
 fail: istat=1
endif
; if p=0, or if algorithm fails to converge, use OLS estimators
a(*)=0.
b=regarp(X,y,a,yfit,covb,u,z,avspe,istat1)
if (istat1 ne 0) then istat=1
escape:
; Check the roots of the characteristic equation after updating a
; Roots are values of t for which
; 0=1-a(1)t-a(2)t^2-a(3)t^3 etc. (Box & Jenkins, 1970, 3.2)
if ((p gt 0 and istat eq 0) and (not keyword_set(pv_wave))) then begin
 ceq=-a
 ceq(0)=1.
;Comment out, so that code will compile in pv_wave.
; rtceq=fz_roots(ceq,eps=tol)
 rtceq=2
; change the +ve sign in the following to - if you want to 
; suppress warnings if roots found on the unit circle
 if (min(abs(rtceq)) le 1.+tol) then begin
  print,'WARNING: Roots of characteristic equation on or inside unit circle'
  print,'Roots:',rtceq
  print,'Abs.r:',abs(rtceq)
  print,'AR cf:',a
  print,'We will use them anyway, but you have been warned'
  istat=2
 endif
endif
return,b
end




FUNCTION Psdnoi, data, frq, dof, $
pnoise=pnoise,psdlim=psdlim,datdtr=datdtr,$
wintyp=wintyp,winwid=winwid,arpcof=arpcof,$
pvalue=pvalue,deltat=deltat,indvar=indvar,$
pv_wave=pv_wave

; returns an estimate of the power spectrum of the data series 
; together with an estimated AR(p) noise spectrum
sz=size(reform(data))
if (sz(0) ne 1) then begin
 print,'psdnoi only set up for 1-D arrays'
 stop
endif
ndp=n_elements(data)
; check the keywords
; default window type is square
if not keyword_set(wintyp) then wintyp=0
; default window width is half data series (for unsmoothed ft^2)
if not keyword_set(winwid) then winwid=ndp/2
; default AR model is white noise -- order of model=n_elements(arpcof)-1
if not keyword_set(arpcof) then arpcof=fltarr(1)
; default P_value is 0.025 (one-tailed=0.05 two-tailed)
if not keyword_set(pvalue) then pvalue=0.025
; default sampling interval=1.
if not keyword_set(deltat) then deltat=1.
; default indvar is just to remove the series mean
if not keyword_set(indvar) then indvar=replicate(1.,ndp)
if not keyword_set(pv_wave) then pv_wave=0
; dt0=data with indvars knocked out
; regarq also estimates AR noise model parameters
b=regarq(indvar,reform(data),arpcof,yfit,covb,dt0,z,avspe,rtceq,istat,$
         pv_wave=pv_wave)
; generate the lag-window
wfn=wfngen(winwid,wintyp)
; compute the power spectrum
psd=float(xsdxcf(dt0,dt0,wfn,cov,dof))
; compute the frequencies of the spectral estimates
if n_params() gt 1 then frq=findgen(winwid+1)/2./winwid/deltat
if keyword_set(pnoise) then begin
; compute the AR noise spectrum and confidence intervals
 pnoise=fltarr(winwid+1,3)
 pmax=10.*max(psd)
 pnoise(*,1)=spcarp(arpcof,deltat,frq,pmax)
 pnoise(*,0)=pnoise(*,1)*chi_sqr(pvalue,dof)/dof    
 pnoise(*,2)=pnoise(*,1)*chi_sqr((1.-pvalue),dof)/dof    
endif
if keyword_set(psdlim) then begin
; compute the AR noise spectrum and confidence intervals
 psdlim=fltarr(winwid+1,2)
 psdlim(*,0)=psd*chi_sqr(pvalue,dof)/dof    
 psdlim(*,1)=psd*chi_sqr((1.-pvalue),dof)/dof    
endif
if keyword_set(datdtr) then datdtr=dt0
return,psd
end


FUNCTION Ts_power_spectrum_mat, ts, wintyp=wintyp, winwid=winwid, $
                            freq=freq, period=period, pnoise=pnoise, $
                            arpcof=arpcof, pvalue=pvalue,deltat = deltat
  
;+
; Name: ts_power_spectrum_mat
;
; PURPOSE: Power spectrum of a time series with significance testing
; Method: Takes FFT of time series and applies windowing to get an
;         Unbiased estimate of the power spectrum. Significance test
;         against an AR(n) process. See Chatfield - Analysis of Time Series
;         (Chapmann and Hall - copy in Hadley Centre Library)
;
; History:
; Version Date     Comment
; ------- -------- -------
; 1.0     14/2/00  Original code. Myles Allen and Mat Collins
; 2.0     10/10/09 Adapted by Matthieu Lengaigne
; Code Description: PV-WAVE
; Category: Statistics
; Classification keywords: <Keyword description>
; Calling sequence: psd=ts_power_spectrum(ts)
; Examples: psd=ts_power_spectrum(nao, arpcof=fltarr(2), pnoise=pnoise)
;
; Inputs:           ts         Time series (vector)
; Optional Inputs:  wintyp     Window type 1=square, 2=Bartlett, 
;                                          3=Tukey-Hanning (default), 4=Parzen.
;                   winwid     Window width, assumed to be half the 
;                              length of the time series if not set.
;                              Use shorter window widths to get smoother, 
;                              less biased power spectra.
;                   arpcof     Coefficients of AR(n) model to test against.
;                              Set as an array of zeros of length n+1 to
;                              calculate the coefficients from the time 
;                              series - defaults to n=1 and arpcof is also
;                              output.
;                   pvalue     Percentile for significance testing 
;                              default is 0.025 giving 2-sided 95% limits.
;                              Remember to think hard about what significance
;                              limits tell you - even "random data" will
;                              have some significant peaks by definition.
;                   deltat     sampling interval in days 
;Optional Outputs: freq       Frequency in years^-1.
;                   period     Period (1./frequency) in years.
;                   dof        Degrees of freedom.
;                   arpcof     See above.
;                   pnoise     Array of size (winwid,3) where row
;                              0 is the lower percentile of the AR(n) fit
;                              1 is the mean value of the AR(n) fit
;                              2 is the upper percentile of the AR(n) fit.
; Return Value:     psd        Power specctral density array.
; Common Blocks:    none
; Side Effects:     none
; Restrictions:     requires maths library
;-
; Year 2000 compliant: Yes
;
; Platform independent: Yes

;STEP 1 - check time series to make sure it is continious and work out deltat


s = size(ts)
IF NOT(KEYWORD_SET(deltat)) THEN deltat = 1

; sampling interval from days to years

deltatn = deltat/365.25

;STEP 2 - Set up keywords which are not already set

IF NOT(KEYWORD_SET(wintyp)) THEN wintyp = 3
IF NOT(KEYWORD_SET(winwid)) THEN winwid = s(1)/2
IF NOT(KEYWORD_SET(arpcof)) THEN arpcof = FLTARR(2)
IF NOT(KEYWORD_SET(pvalue)) THEN p_value = 0.025
IF winwid LE 0. THEN winwid = s(1)/2
print, 'Window width =', winwid

;STEP 3 - compute psd

pnoise = 1
freq = 1

psd = psdnoi(REFORM(ts), freq, wintyp = 3, dof, $
             winwid = winwid, deltat = deltatn, $
             pnoise = pnoise, arpcof = arpcof, $
             pvalue=pvalue, /pv_wave)
psd = psd(1:winwid)
freq = freq(1:winwid)
period = 1./freq
pnoise = pnoise(1:winwid, *)

;STEP 4 - that's it

RETURN, psd

END

