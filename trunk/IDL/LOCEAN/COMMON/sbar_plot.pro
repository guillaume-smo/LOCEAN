;+
;
; @file_comments
; Same thing that bar_plot but compatible with the whole environment 
; (<pro>common</pro> included)
;
; @categories Graphics
;
; @keyword COLORS {type=vector}
; A vector, the same size as VALUES, containing the color index
; to be used for each bar.  If not specified, the colors are
; selected based on spacing the color indices as widely as
; possible within the available colors (specified by D.N_COLORS).
;
; @keyword COLORS {type=integer}
; I gives color of all colorbars. (contrarily to colors
; which is a vector giving the color of each colorbar).
;
; @keyword NOREINITPLT
; We active it if we do not want environment variables !p, !x, !y, !z
; to be reinitialized by the procedure <pro>reinitplt</pro>
;
; @keyword _EXTRA
; Used to pass keyword
;
; @uses
; <pro>common</pro>
;
; @restrictions
; If NOREINITPLT is not activated, all environment
; variables !p, !x, !y, !z are reinitialized by the procedure <pro>reinitplt</pro>
;
; @examples
;
;   IDL> sbar_plot, indgen(10),small = [2,2,2],/rempli
;   IDL> sbar_plot, indgen(10),small = [2,2,3],/noerase
;   IDL> \@ps
;
; @history
; Sebastien Masson (smasson\@lodyc.jussieu.fr)
;                      10/10/1999
;
; @version
; $Id: sbar_plot.pro 471 2011-12-12 15:54:47Z smasson $
;
;-
PRO sbar_plot, values, COLORS=colors, NOREINITPLT=noreinitplt, YMIN=ymin, YMAX=ymax, TICKV=tickv, OVERPLOT=overplot, _EXTRA=ex
;
  compile_opt idl2, strictarrsubs
;
@common

; 1) I reinitialize the graphic environment (variables !x, !y and !p):
   if NOT keyword_set(NOREINITPLT) then reinitplt, _extra = ex

; 2) I place the drawing on the screen like on the postscript
   IF overplot EQ 0 THEN placedessin, 'autre', _extra = ex $
   ELSE IF overplot EQ 1 THEN placedessin, 'autre', /noerase, _extra = ex

; 3) Drawing
   if n_elements(COLORS) NE 0 then BEGIN
      if n_elements(COLORS) EQ n_elements(Values) then col = colors $
       ELSE col = replicate(colors[0], n_elements(Values))
   ENDIF ELSE col = congrid(indgen(!d.n_colors < 256), n_elements(Values))
;
   help, ymin, ymax, overplot
   !y.range=[ymin,ymax]
   bar_plot, Values, background = !p.background, colors = col , tickv=tickv, overplot=overplot $
             , _extra = ex
; 4) End of drawing
   terminedessin, _extra=ex

   return
end
