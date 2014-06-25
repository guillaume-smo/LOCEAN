
      ; 1m DIAGS LIST

      ; var_year : 2D(=spatial) 1y(=interannual) SEASONAL(=period) AVERAGE
      ; ano_year : 2D(=spatial) 1y(=interannual) SEASONAL(=period) ANOMALY FROM SEASONAL CLIMATOLOGY
      ; var_ts1y : TS(=time series) 1y SEASONAL & SPATIAL AVERAGE
      ; ano_ts1y : TS(=time series) 1y SEASONAL & SPATIAL ANOMALY
      ; ano_sc1m_ts1m :     TS 1m MONTHLY  ANOMALY FROM SC1M 
      ; ano_sc1m_ts1y :     TS 1y SEASONAL ANOMALY FROM SC1M
      ; ano_sc1m_2D1m :     2D 1m MONTHLY  ANOMALY FROM SC1M
      ; anoyear_sc1m_2D1y : 2D 1y SEASONAL ANOMALY FROM SC1M



      IF data_type EQ '1m' THEN BEGIN


        IF flag_obs THEN BEGIN
          listyear = listyear_obs
          nbyear   = nbyear_obs
          nbmonth  = nbmonth_obs
          nblon    = nblon_obs
          nblat    = nblat_obs
          listy    = listyear_mod
        ENDIF ELSE BEGIN
          listyear = listyear_mod
          nbyear   = nbyear_mod
          nbmonth  = nbmonth_mod
          nblon    = nblon_mod
          nblat    = nblat_mod
          listy    = listyear_obs
        ENDELSE


        ; DECLARATIONS
        interp_list = !NULL
        ;indok    = LISTMATCH( listyear, listy)
	indok = WHERE( LIST(listyear, /EXTRACT) EQ LIST(listy, /EXTRACT))
        var_year = FLTARR( nblon, nblat, nbyear)
        ano_year = FLTARR( nblon, nblat, nbyear)
        var_ts1y = FLTARR( nbyear)
        ano_ts1y = FLTARR( nbyear)
        ano_sc1m_ts1m = FLTARR( nbyear*12) * !VALUES.F_NAN
        ano_sc1m_ts1y = FLTARR( nbyear) * !VALUES.F_NAN
        ano_sc1m_2D1m = FLTARR( nblon, nblat, nbyear*12) * !VALUES.F_NAN
        anoyear_sc1m_2D1y = FLTARR( nblon, nblat, nbyear) * !VALUES.F_NAN


        ; DIAGS 1y
        FOR y = 0, nbyear-1 DO BEGIN
          ; SEASONAL AVERAGE
          ind_year = ind_period + indok[y,0]*12
          ind_year = ind_year[ WHERE( ind_year LE nbmonth-1)]
          IF n_elements(ind_year) GT 1 THEN var_year[*,*,y] = MEAN(var[*,*,ind_year], DIMENSION=3, /NAN) $
          ELSE var_year[*,*,y] = var[*,*,ind_year]
          var_ts1y[y] = MEAN( var_year[*,*,y], /NAN)
          ano_year[*,*,y] = var_year[*,*,y] - var_mean
          ; SEASONAL ANOMALY FROM SC1M
          ibeg = y * 12
          iend = (y+1) * 12 - 1
          ano_sc1m_ts1m[ibeg:iend]     = var_ts[ibeg:iend]  - var_sc1m
          ano_sc1m_2D1m[*,*,ibeg:iend] = var[*,*,ibeg:iend] - var2D_sc1m
          IF n_elements(ind_year) GT 1 THEN ano_sc1m_ts1y[y] = MEAN( ano_sc1m_ts1m[ind_year], /NAN) $
          ELSE ano_sc1m_ts1y[y] = ano_sc1m_ts1m[ind_year]
        ENDFOR
        help, ano_sc1m_ts1m, ano_sc1m_2D1m, ano_year, ano_sc1m_ts1y, var_ts1y

        FOR y = 0, nbyear-1 DO BEGIN
          ano_ts1y[y] = var_ts1y[y] - MEAN( var_ts1y, /NAN)
          ind_year = ind_period + indok[y,0]*12
          ind_year = ind_year[ WHERE( ind_year LE nbmonth-1)]
          IF n_elements(ind_year) GT 1 THEN anoyear_sc1m_2D1y[*,*,y] = MEAN( ano_sc1m_2D1m[*,*,ind_year], DIMENSION=3, /NAN) $
          ELSE anoyear_sc1m_2D1y[*,*,y] = ano_sc1m_2D1m[*,*,ind_year]
        ENDFOR
        help, ano_ts1y, anoyear_sc1m_2D1y


        ; CALCUL MONTHLY
        std_ano_sc1m = FLTARR(12)
        FOR m = 0, 12-1 DO BEGIN
          ind_month    = FINDGEN( nbyear) * 12 + m
          std_ano_sc1m[m] = STDDEV( ano_sc1m_ts1m[ind_month], /NAN)
        ENDFOR
        help, std_ano_sc1m


        ; ADD VARS TO INTERPOLATE AFTER
        interp_list = [ interp_list, 'var_year', 'ano_year', 'anoyear_sc1m_2D1y'] & help, interp_list


      ENDIF ; 1m DIAGS
