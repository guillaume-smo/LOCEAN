FUNCTION hist_1D, obs, bin_nb, bin_min, bin_size

  hist = fltarr(bin_nb) & help, hist

  ; histogram IDL
  ; hist = histogram(obs, min=bin_min, max=bin_max, binsize=bin_size, location=bin_loc, /NAN)

  ; histogram homemade
  FOR j = 0, bin_nb-1 DO BEGIN
    hist[j] = n_elements( where( obs GE bin_min+j*bin_size AND obs LT bin_min+(j+1)*bin_size ) )
  ENDFOR

  RETURN, hist

END
