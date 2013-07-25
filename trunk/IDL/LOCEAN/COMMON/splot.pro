;+
;
; @file_comments
; Same thing than plot but compatible with the whole environment 
; (<pro>common</pro> included)
;
; @categories
; Graphics
;
; @keyword NOREINITPLT
; We active it if we do not want environment variables !p, !x, !y, !z
; to be reinitilalized by the procedure <pro>reinitplt</pro>
;
; @keyword _EXTRA
; Used to pass keywords
;
; @uses
; <pro>common</pro>
;
; @restrictions
; If NOREINITPLT is not activated, all environment
; variables !p, !x, !y, !z are reinitialized by the procedure 
; <pro>reinitplt</pro>
;
; @examples
;
;   IDL> splot, indgen(10),ystyle=1,small=[1,2,1],/portrait
;   IDL> splot, -indgen(10),ystyle=1,small=[1,2,2],/noerase
;   IDL> \@ps
;
; @history
; Sebastien Masson (smasson\@lodyc.jussieu.fr)
;                      18/10/1999
;
; @version
; $Id: splot.pro 370 2008-08-07 07:59:15Z pinsard $
;
;-
PRO splot, x, y, NOREINITPLT=noreinitplt, _EXTRA=ex
;
  compile_opt idl2, strictarrsubs
;
@common
; 1) I reinitialize the graphic environment (variables !x, !y and !p):
   if NOT keyword_set(NOREINITPLT) then reinitplt, _extra = ex
; 2) i put the drawing on the screen like on the postscript
   placedessin, 'autre', _extra = ex
; 3) Drawing
   if n_elements(y) EQ 0 then plot,  x, xstyle = 1, ystyle = 1, _EXTRA = ex $
   ELSE plot,  x, y, xstyle = 1, ystyle = 1, _EXTRA = ex
; 4) End of drawing
   terminedessin, _extra=ex
;
   return
end
