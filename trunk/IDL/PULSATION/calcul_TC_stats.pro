
; 1y TIME SERIES
nbcg_1y    = FLTARR(nbyear)*!values.f_nan
nbct_1y    = FLTARR(nbyear)*!values.f_nan
nbtcday_1y = FLTARR(nbyear)*!values.f_nan
nbtgday_1y = FLTARR(nbyear)*!values.f_nan

FOR y = 0, nbyear-1 DO BEGIN
  ind_year = ind_period + y*12.
  ind_year = ind_year[ WHERE( ind_year LE nbmonth-1)]
  ind_year = ind_year + 1.
  ind_cg = !NULL
  ind_ct = !NULL
  ind_tg = !NULL
  ind_tc = !NULL
  FOR m = 0, n_elements(ind_year)-1 DO BEGIN
    ind_cg = [ ind_cg, WHERE( nbmonthcg[*,0] EQ ind_year[m], /NULL)]
    ind_ct = [ ind_ct, WHERE( nbmonthtc[*,0] EQ ind_year[m], /NULL)]
    ind_tg = [ ind_tg, WHERE( nbmonthcg      EQ ind_year[m], /NULL)]
    ind_tc = [ ind_tc, WHERE( nbmonthtc      EQ ind_year[m], /NULL)]
  ENDFOR
  nbcg_1y[y]    = n_elements( ind_cg)
  nbct_1y[y]    = n_elements( ind_ct)
  nbtcday_1y[y] = n_elements( ind_tc) / 4.
  nbtgday_1y[y] = n_elements( ind_tg) / 4.
  print, listyear[y], nbcg_1y[y], nbct_1y[y], nbtgday_1y[y], nbtcday_1y[y]
; rajouter season pour compositer facielemnt
ENDFOR

nbcg_1y[WHERE( nbcg_1y EQ 0.)] = !values.f_nan
nbct_1y[WHERE( nbct_1y EQ 0.)] = !values.f_nan
nbtcday_1y[WHERE( nbtcday_1y EQ 0.)] = !values.f_nan
nbtgday_1y[WHERE( nbtgday_1y EQ 0.)] = !values.f_nan
help, nbcg_1y, nbct_1y, nbtcday_1y, nbtgday_1y


; 1y ANOMALY
ano_nbcg_1y    = FLTARR(nbyear)
ano_nbct_1y    = FLTARR(nbyear)
ano_nbtcday_1y = FLTARR(nbyear)
ano_nbtgday_1y = FLTARR(nbyear)

FOR y = 0, nbyear-1 DO BEGIN
  ano_nbcg_1y[y]    = nbcg_1y[y] - MEAN(nbcg_1y, /NAN)
  ano_nbct_1y[y]    = nbct_1y[y] - MEAN(nbct_1y, /NAN)
  ano_nbtcday_1y[y] = nbtcday_1y[y] - MEAN(nbtcday_1y, /NAN)
  ano_nbtgday_1y[y] = nbtgday_1y[y] - MEAN(nbtgday_1y, /NAN)
ENDFOR

help, ano_nbcg_1y, ano_nbtcday_1y, ano_nbtcday_1y, ano_nbtgday_1y
