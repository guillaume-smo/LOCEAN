FUNCTION bootstrap_swen, obs, nbobs, bin_nb, bin_min, bin_size

siglev   = 0.05 ; niveau de significativite
nbt      = 1000 ; nombre de tirage aleatoire
distrib  = fltarr(nbt,bin_nb) & help, distrib
errbars  = fltarr(  2,bin_nb) & help, errbars


; generate nbt random combination of distributions
FOR ibt = 0, nbt-1 DO BEGIN

  irand = long(resample(nbobs,nbobs)) ; change aleatoirement l'ordre des obs
  FOR j = 0, bin_nb-1 DO BEGIN
    FOR i = 0, nbobs-1 DO distrib[ibt,j] = obs[j,irand[i]]
  ENDFOR

ENDFOR

; intervalle confiance
FOR i = 0, bin_nb-1 DO BEGIN
  errbars[0,i] = (percentile(distrib[*,i],siglev))[0]
  errbars[1,i] = (percentile(distrib[*,i],siglev))[1]
ENDFOR

;print, errbars & STOP
RETURN, errbars

END
