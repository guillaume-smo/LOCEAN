help, nb_plot, minvar, maxvar, ncontour


IF nb_plot GT 1 THEN BEGIN

  ;IF nb_plot EQ 1 THEN $
  ;barrecouleur, colnumb, minvar,  maxvar, ncontour/2, position=[0.15, 0.15, 0.90, 0.20], format=fmt, cb_charsize=1


  IF key_portrait THEN BEGIN

    IF nb_plot EQ 2 THEN $
    barrecouleur, colnumb, minvar,  maxvar, ncontour/2, position=[0.05, 0.10, 0.09, 0.90], /vertical, format=fmt, cb_charsize=1

    IF nb_plot EQ 3 THEN $
    barrecouleur, colnumb, minvar,  maxvar, ncontour/2, position=[0.05, 0.10, 0.10, 0.90], /vertical, format=fmt, cb_charsize=1

    IF nb_plot EQ 4 THEN $
    barrecouleur, colnumb, minvar,  maxvar, ncontour/2, position=[0.10, 0.475, 0.90, 0.525], format=fmt, cb_charsize=1

    IF nb_plot EQ 5 THEN $
    barrecouleur, colnumb, minvar,  maxvar, ncontour/2, position=[0.55, 0.15, 0.95, 0.20], format=fmt, cb_charsize=1

  ENDIF ELSE BEGIN

    IF nb_plot EQ 2 THEN $
    barrecouleur, colnumb, minvar,  maxvar, ncontour/2, position=[0.20, 0.49, 0.80, 0.54], format=fmt, cb_charsize=1

    IF nb_plot EQ 3 THEN $
    barrecouleur, colnumb, minvar,  maxvar, ncontour/2, position=[0.15, 0.03, 0.90, 0.07], format=fmt, cb_charsize=1

    IF nb_plot EQ 4 THEN $
    barrecouleur, colnumb, minvar,  maxvar, ncontour/2, position=[0.03, 0.10, 0.06, 0.90], /vertical, format=fmt, cb_charsize=1

    IF nb_plot EQ 5 THEN $
    barrecouleur, colnumb, minvar,  maxvar, ncontour/2, position=[0.55, 0.15, 0.95, 0.20], format=fmt, cb_charsize=1

  ENDELSE


  IF nb_plot EQ 6 THEN $
  barrecouleur, colnumb, minvar, maxvar, ncontour/2, position=[0.05, 0.03, 0.95, 0.06], format=fmt, cb_charsize=1

  IF nb_plot EQ 7 THEN $
  barrecouleur, colnumb, minvar, maxvar, ncontour/2, position=[0.05, 0.03, 0.95, 0.06], format=fmt, cb_charsize=1


ENDIF
