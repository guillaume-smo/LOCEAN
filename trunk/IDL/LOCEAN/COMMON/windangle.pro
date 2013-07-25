;+
; NAME:
;       windangle
;
; PURPOSE:
;       Calculate the windangle (and optionally the windspeed) from given
;       values of u and v
;       
; CATEGORY:
;       FUNCTION
;
; CALLING SEQUENCE:
;       windangle,u,v,windspeed=windspeed
;
; EXAMPLE:
;       windangle(u,v)
;
; INPUTS: 
;       u       flt or fltarr: the east(ward) component of the wind vector
;       v       flt or fltarr: the north(ward) component of the wind vector
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;
; OUTPUTS
;       windspeed       (flt or fltarr): the windspeed
;
; COMMON BLOCKS:
;
; SIDE EFFECTS: 
;
; RESTRICTIONS:
;       
; PROCEDURE:
;       
; MODIFICATION HISTORY:
;       first implementation Sep 9, 1997 by Dominik Brunner
;-

FUNCTION windangle,u,v,windspeed=windspeed

; check for input

IF (n_elements(u) NE n_elements(v)) OR (n_elements(u) EQ 0) THEN BEGIN
   print,'Error in windangle() calculation'
   return,-1
ENDIF

r2d=180./!pi

windspeed=sqrt(u^2+v^2)
wangle=90.-atan(-v,-u)*r2d

return,wangle

end
