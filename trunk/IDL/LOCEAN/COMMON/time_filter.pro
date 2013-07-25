function time_filter, tab,tt,Thf,Tlf,tsamp=tsamp,valmask=valmask,cut=cut

;
; tab is a 3D array (x,y,t) (the field to filter)
; tt is the time-axis (it should be evenly spaced)
; Thf is the high-frequency cutoff (expressed in same unit than time axis)
; Tlf is the low-frequency cutoof  (expressed in same unit than time axis)
; if the tsamp keyword is defined, it is assumed to be the uniform sampling interval, otherwise, it is computed from tt
; valmask is the value of missing data (assumed to be always missing at same locations): infinite values if not specified
; if the cut keyword is activated, both ends of tab and tt are truncated of Tlf/2 points
;

si=size(tab) & nx=si(1) & ny=si(2) & nt=si(3)
if (n_elements(tt) ne nt) then begin
  print, 'The first two arguments of time_filter need to have the same number of samples.'
  help, tab, tt
  stop
endif
if (not keyword_set(tsamp)) then begin
  dt=tt(1:nt-1)-tt(0:nt-2)
  dt1=min(dt,max=dt2)
  if (dt1 ne dt2) then begin
    print, 'The time sampling needs to be uniform.'
    print, 'Min sampling interval ',dt1
    print, 'Max sampling interval ',dt2
    stop
  endif
  tsamp=dt1
endif 

freq=findgen(nt/2+1)/(nt*tsamp)
freq=[freq,-1.*reverse(freq(1:nt/2))]
peri=1./(abs(freq) > 1.e-20)*(2*(freq ge 0)-1.)
msk_spec=float((abs(peri) ge Thf) and (abs(peri) le Tlf))


tab2=reform(tab,nx*ny,nt)
if (n_elements(valmask) eq 0) then begin
  iii=where(finite(tab2(*,0)))
  valmask=!values.f_nan
endif else begin
  iii=where(tab2(*,0) ne valmask)
endelse
NN=n_elements(iii)
tab2r=tab2(iii,*)
tab2rf=tab2r*0.
print, 'Number of values ',nx*ny
print, 'Number of valid values ',NN
for n=0l,NN-1 do begin
  print, 'Filtering ',n,' / ',NN-1
  ts=reform(tab2r(n,*))
  ff_ts=fft(ts)
  ff_ts_f=ff_ts*msk_spec
  tab2rf(n,*)=float(fft(ff_ts_f,/inverse))
endfor
tabf=fltarr(nx*ny,nt)+valmask
tabf(iii,*)=tab2rf
tabf=reform(tabf,nx,ny,nt)

if keyword_set(cut) then begin
  Nm=(Tlf/2)
  tt=tt(Nm:nt-1-Nm)
  tabf=tabf(*,*,Nm:nt-1-Nm)
endif

return, tabf 

end
