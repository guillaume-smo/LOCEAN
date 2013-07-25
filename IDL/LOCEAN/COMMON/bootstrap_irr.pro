bt_lev   = 0.05
nbt      = 1000
igs      = indgen(nbobs)
histo    = fltarr(nbt,bin_nb)

FOR ibt = 0, nbt-1 DO BEGIN
  ibtcg = resample(nbobs,nbobs)
  ings  = igs[ibtcg]
;  histo[ibt,*] = histogram(obs[ings,*],min=bin_min, max=bin_max, binsize=bin_size, /NAN)
  FOR j = 0, bin_nb-1 DO BEGIN
    histo[ibt,j] = n_elements(where(obs[ings,*] GE bin[j] AND obs[ings,*] LT bin[j+1]))
  ENDFOR
ENDFOR

mom_bin = fltarr(3,bin_nb)
;mom_bin[0,*] = histogram(obs, min=bin_min, max=bin_max, binsize=bin_size, location=bin_loc, /NAN)
FOR j = 0, bin_nb-1 DO BEGIN
  print, bin[j], bin[j+1]
  mom_bin[0,j] = n_elements(where(obs GE bin[j] AND obs LT bin[j+1]))
ENDFOR
print, reform(mom_bin[0,*])

FOR i = 0, bin_nb-1 DO BEGIN
  print, i
  low_up_bnd_95 = percentile(histo[*,i],bt_lev)
  mom_bin[1,i] = low_up_bnd_95[0]
  mom_bin[2,i] = low_up_bnd_95[1]
ENDFOR
