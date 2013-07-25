FUNCTION percentile,arr,pcts

  ;   function PERCENTILE  -  returns percentile values of array'
  ;
  ;   calling sequence: Z=PERCENTILE(arr,PCTS)'
  ;     arr:  name of array'
  ;     PCTS: percentiles between 0 and 0.49
  ;     Z:    output data values corresponding to percentiles'

  pcts = [pcts,1.-pcts(0)]  ;make symmetric
  npct = 2
  z    = fltarr(npct)
  igv  = where(finite(arr) EQ 1)

  IF n_elements(igv) GT 1 THEN BEGIN
    nim  = n_elements(igv)
    k    = sort(arr[igv]); & help, k
    z(0) = (arr[igv])[k(nim*pcts[0])]
    z(1) = (arr[igv])[k(nim*pcts[1])]
  ENDIF
  return,z
END
