
; DECLARATIONS
nlon    = n_elements(lon_0)
nlat    = n_elements(lat_0)
nwetmax = 122 ; JJAS days number

DATA_STRUCT = { $
        duration   : fltarr(nwetmax), $
        daydeb     : fltarr(nwetmax), $
        dayfin     : fltarr(nwetmax), $
        maxrain    : fltarr(nwetmax), $
        totalrain  : fltarr(nwetmax)  }


FOR e = 0, n_elements(exp_list)-1 DO BEGIN

  IF e EQ 0 THEN cmd = execute( 'tmpa = var_'+STRTRIM(e,2)+'[*,*,ind_mean1d_'+STRTRIM(e,2)+']' ) $
  ELSE cmd = execute( 'tmpa = var_'+STRTRIM(e,2)+'_gridobs'+'[*,*,ind_mean1d_'+STRTRIM(e,2)+']' )
  help, tmpa
  wet    = REPLICATE( DATA_STRUCT, nlon, nlat)
  tab    = LONARR( nlon, nlat, n_elements( tmpa[0,0,*]))
  nspell = LONARR( nlon, nlat)
  season_tmpa = FLTARR( nlon, nlat)

  iwet = 0l
  FOR j = 0, nlat-1 DO BEGIN
    FOR i = 0, nlon-1 DO BEGIN

      iwet=0l
      ; mean
      season_tmpa[i,j] = MEAN( tmpa[i,j,*], /NAN)
      ; wet spells
      tab[i,j,*]  = LABEL_REGION( REFORM( tmpa[i,j,*]) GT 1) ; seuil detection pluie: 1 mm/day
      nspell[i,j] = MAX( tab[i,j,*])

      FOR ii = 1, nspell[i,j] DO BEGIN

        ind = WHERE( tab[i,j,*] EQ ii, c)         ; important
        IF c GT nwetmax/2 THEN print,i, j, ii, c  ; duration > nwetmax/2 days = problem
        IF c GT nwetmax   THEN STOP

        wet[i,j].duration[iwet]  = c
        wet[i,j].daydeb[iwet]    = MIN( ind)
        wet[i,j].dayfin[iwet]    = MAX( ind)
        wet[i,j].maxrain[iwet]   = MAX( tmpa[i,j,ind])
        wet[i,j].totalrain[iwet] = TOTAL( tmpa[i,j,ind])

        iwet = iwet + 1l

      ENDFOR ; ii
    
    ENDFOR ; i
  ENDFOR ; j


  carte  = FLTARR( nlon, nlat, nwetmax)
  carte2 = carte

  FOR j = 0, nlat-1 DO BEGIN
    FOR i = 0, nlon-1 DO BEGIN 
      FOR k = 0, nwetmax-1 DO BEGIN

        ii = wet[i,j].duration[k]-1 ; Important ici on range les wet spell
        carte[i,j,ii]  = carte[i,j,ii] + wet[i,j].totalrain[k]
        carte2[i,j,ii] = carte2[i,j,ii] + 1 ; carte2(i,j,ii)+wet(i,j).duration(k)

      ENDFOR ; k
    ENDFOR ; i
  ENDFOR ; j

  cmd = execute( 'wsr_'+STRTRIM(e,2)+' = TEMPORARY( carte)' )   ; wet spell rainfall (mm / ws duration)
  cmd = execute( 'wsd_'+STRTRIM(e,2)+' = TEMPORARY( carte2)' )  ; wet spell duration (number of days / ws duration)

ENDFOR


; PLOTS VERIF TOTAL RAIN AMOUNT
;plt, TOTAL(CARTE_0, 3) / n_elements(ind_mean1d_0), /nocont, realcont=2, lct=22, min=0, max=25
STOP
