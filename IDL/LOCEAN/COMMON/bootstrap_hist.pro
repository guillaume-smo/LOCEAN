FUNCTION bootstrap_hist, obs, nbobs, bin_nb, bin_min, bin_size

siglev   = 0.05 ; niveau de significativite
nbt      = 1000 ; nombre de tirage aleatoire
distrib  = fltarr(nbt,bin_nb) & help, distrib
mom_bin  = fltarr(  3,bin_nb) & help, mom_bin


; generate nbt random distributions
FOR ibt = 0, nbt-1 DO BEGIN

  irand = long(resample(nbobs,nbobs)) ; change aleatoirement l'ordre des obs

  ; histogram IDL
  ;distrib[ibt,*] = histogram(obs[irand,*],min=bin_min, max=bin_max, binsize=bin_size, /NAN)

  ; histogram homemade
  FOR j = 0, bin_nb-1 DO BEGIN
    distrib[ibt,j] = n_elements(where(obs[irand,*] GE bin_min+j*bin_size AND obs[irand,*] LT bin_min+(j+1)*bin_size))
  ENDFOR

ENDFOR

; intervalle confiance 95%
FOR i = 0, bin_nb-1 DO BEGIN
  mom_bin[1,i] = (percentile(distrib[*,i],siglev))[0]
  mom_bin[2,i] = (percentile(distrib[*,i],siglev))[1]
ENDFOR


; histogram IDL
;mom_bin[0,*] = histogram(obs, min=bin_min, max=bin_max, binsize=bin_size, location=bin_loc, /NAN)

; histogram homemade
FOR j = 0, bin_nb-1 DO BEGIN
  print, bin_min+j*bin_size, bin_min+(j+1)*bin_size
  mom_bin[0,j] = n_elements(where(obs GE bin_min+j*bin_size AND obs LT bin_min+(j+1)*bin_size))
ENDFOR

print, mom_bin
RETURN, mom_bin

END
