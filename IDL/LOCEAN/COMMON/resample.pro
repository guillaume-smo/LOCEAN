;+
; NAME:
; resample.pro
;
;
; PURPOSE:
; create a uniform resampling of indicies from 0 to n_elements()-1
;
;
; INPUTS:
; n: the number of points to have in the resampling
; tot: the number of elements in the array to resample
;
;
; OPTIONAL INPUTS:
; none
;
;
; KEYWORD PARAMETERS:
; SEED: the seed value to use in the randomu command
; UNIQUE: forces the return indicies to be unique within the space
; SORT: return the with replacement array sorted, this is the default
; 			for the unique case
;
; OUTPUTS:
; rs: resampled array with n elements
;
;
; RESTRICTIONS:
; indicies must be [0:n_elements()-1], will add later the ability to
; choose from input indicies if needed
;
;
; PROCEDURE:
; choose random points on [0, n_elements()] then truncate them all them all,
; and they may or may not be all unique
;
;
; EXAMPLE:
; print, resample(10,100)
; 7    12    16    22    52    60    71    73    74    87
;
; for a "resample with replacement" make both n and tot the same
; print, resample(10,10)
; 2           9           0           4           7           7           2           3
; 0           2
;
;
; MODIFICATION HISTORY:
;
;	26July2007 Brian Larsen
;		rewriten in accordance with
; 		http://www.dfanning.com/code_tips/randomindex.html
; 		which just makes it faster
;       Thu Mar 30 15:07:20 2006, Brian Larsen
;		added UNIQUE keyword so that you can get non unique
;		indicies from the procedure, also fixed issues caused
;		by rounding instead of truncating
;       Thu Mar 16 09:40:44 2006, Brian Larsen
;		written and tested
;
;-
FUNCTION resample, n, tot, SEED=seed, UNIQUE=unique, SORT=sort

  IF n GT tot THEN BEGIN
    message, /ioerror, 'n must be less than total (resample)'
  ENDIF
  
  n_ = long(n)
  tot_ = long(tot)
  
  
  IF KEYWORD_SET(unique)  THEN BEGIN
    rs = LonArr(n_, /NOZERO)
    n_uni = n_
    WHILE n_uni GT 0 DO BEGIN
      rs[n_-n_uni] = Long(RandomU(seed, n_uni)*tot_)
      rs = rs[Sort(rs)]
      u = Uniq(rs)
      n_uni = n_ - N_Elements(u)
      rs[0] = rs[u]
    ENDWHILE
  ENDIF ELSE BEGIN
    rs = long(randomu(seed, n_)*(tot_))
    IF n_elements(sort) NE 0 THEN $
      rs=rs[sort(rs)]
  ENDELSE
  
  RETURN, rs
END

