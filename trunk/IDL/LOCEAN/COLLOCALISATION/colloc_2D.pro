PRO colloc_2D
@all_cm

maskfile = '/Users/gslod/WORK/IDL/MASK_WRF_GRID_T.nc'
initncdf,  maskfile

expname = 'COUPLED_SW2_KF'
period = '1990-2009'

ucrit    = 17.5
vorcrit  = 30.e-5
vorelax  = 30.e-5
tempcrit = 0.

pathin = '/Users/gslod/WORK/IDL/TRACKER_IDL/EXP_'+expname+'/DATA/' & help, pathin
filein = 'maxima_TREAL_RVM_NICO_MASK_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_'+ period +'.idl' & help, filein
fileou = 'maxima_UST_TREAL_RVM_NICO_MASK_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_'+ period +'.idl' & help, fileou
restore, pathin + filein, /VERBOSE

;indtok = where(anomtcyc GE newtcrit)
;uv10cyc  = uv10cyc[indtok]
;lonuvcyc = lonuvcyc[indtok]
;latuvcyc = latuvcyc[indtok]


pathda = '/Volumes/TIME_MACHINE/EXP_'+expname+'/WRF/UST/' & help, pathda
fileda = 'ust_'+period+'.nc' & help, fileda

ustcyc = fltarr(size(uv10cyc, /dim)) + !values.f_nan & help, ustcyc
nt = n_elements(uv10cyc[*,0])


FOR i = 0, nt-1 DO BEGIN
  
  print, i
  ust = read_ncdf('UST', i, i, /TIMESTEP, FILENAME=pathda+fileda, /NOSTRUCT); & help, ust
  indok = where(finite(lonuvcyc[i,*]) EQ 1, cntlon)
  IF indok[0] NE -1 THEN lon = lonuvcyc[i,indok]
  IF indok[0] NE -1 THEN lat = latuvcyc[i,indok]

  FOR j = 0, cntlon-1 DO BEGIN

    indx = where(glamt[*,0] EQ lon[j])
    indy = where(gphit[0,*] EQ lat[j])
    ustcyc[i,j] = ust[indx,indy]

  ENDFOR

ENDFOR

save, loncyc, latcyc, mslpcyc, lonuvcyc, latuvcyc, uv10cyc, ustcyc, lonvorcyc, latvorcyc, vorcyc, anomtcyc, rvmcyc, timejuld, timedate, /VERBOSE, filename = pathin + fileou
      
END
