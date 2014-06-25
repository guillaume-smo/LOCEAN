
IF v EQ 0 THEN BEGIN

  print, '' & print, 'read_TC_ibtracks ...'
  flag_TC  = 1 & help, flag_TC
  debug_TC = 1


  ; FILE+DIRECTORY DEFINITIONS
  version = 'v03r05'
  tcdir   = '/Users/guillaumesamson/WORK/DATASETS/' &
  tcfile  = 'Allstorms.ibtracs_all.'+version+'.nc'
  centres = 'hurdat_atl td9636 reunion atcf ds824_sh ds824_ni bom ds824_au jtwc_sh jtwc_wp td9635 ds824_wp jtwc_io cma hurdat_epa jtwc_ep ds824_ep jtwc_cp tokyo neumann hko cphc wellington newdelhi nadi'
  centres = STRSPLIT( centres, /EXTRACT)
  help, tcdir, tcfile, centres


  ; 1D VARS
  name          = string(ncdf_lec(tcfile, iodir = tcdir, var = 'name'))
  season        = ncdf_lec(tcfile, iodir = tcdir, var = 'season')
  genesis_basin = ncdf_lec(tcfile, iodir = tcdir, var = 'genesis_basin')
  print, '' & help, name, season, genesis_basin


  ; 2D VARS
  time   = ncdf_lec(tcfile, iodir = tcdir, var = 'source_time')
  lat    = ncdf_lec(tcfile, iodir = tcdir, var = 'source_lat') *0.01
  lon    = ncdf_lec(tcfile, iodir = tcdir, var = 'source_lon') *0.01
  windkt = ncdf_lec(tcfile, iodir = tcdir, var = 'source_wind')*0.1
  pres   = ncdf_lec(tcfile, iodir = tcdir, var = 'source_pres')*0.1
  print, '' & help, time, lon, lat, windkt, pres


  ; MISSING VALUES
  print, 'TIME MISSING VALUES:'
  bad       = where(time LE -99999.00, cnt) & help, bad
  IF cnt EQ 0 THEN stop
  time      = time + julday(11,17,1858,00,00,00)
  time[bad] = !values.f_nan
  lat[bad]  = !values.f_nan
  lon[bad]  = !values.f_nan
  windkt[bad] = !values.f_nan
  pres[bad] = !values.f_nan

  print, 'POSITION MISSING VALUES:'
  bad = where(lon EQ -300.00, cnt) & help, bad
  IF cnt EQ 0 THEN stop
  lat[bad]  = !values.f_nan
  lon[bad]  = !values.f_nan
  windkt[bad] = !values.f_nan
  pres[bad] = !values.f_nan

  print, 'INTENSITY MISSING VALUES:'
  bad = where(pres EQ -999.00, cnt) & help, bad
  IF cnt EQ 0 THEN stop
  lat[bad]  = !values.f_nan
  lon[bad]  = !values.f_nan
  windkt[bad] = !values.f_nan
  pres[bad] = !values.f_nan


  ; WIND UNITS CONVERSION
  windms      = windkt * 0.5144  ; kt -> m/s
  ;IF windave EQ '1min' THEN BEGIN
  ;  windms = windms * 1./0.88 ; 10 min -> 1 min
  ;  windkt = windkt * 1./0.88 ; 10 min -> 1 min
  ;ENDIF


  ; DATES GENERATION
  caldat, time, month, day, year, hour
  year  = FLOAT(year)
  month = FLOAT(month)
  day   = FLOAT(day)
  hour  = FLOAT(hour)
  bad   = WHERE( FINITE( time) EQ 0) & help, bad
  month[bad] = !values.f_nan
  day[bad]   = !values.f_nan
  year[bad]  = !values.f_nan
  hour[bad]  = !values.f_nan
  nbmonth    = (year - yearini_obs) * 12. + month
  date       = year*10000. + month*double(100.) + day + hour/24.
  season     = FLOAT( season)
  season[ WHERE( FINITE( date[0,*]) EQ 0)] = !values.f_nan
  print, '' & help, year, month, day, hour, date, nbmonth, season


  ; PERIOD SELECTION
  print, '' & print, 'SELECTION PERIOD:'
  datebeg = dates[0] & help, datebeg
  dateend = dates[1] & help, dateend
  datecg  = date[0,*]
  indcg   = where( datecg GE datebeg AND datecg LE dateend)
  indtc   = where( date   GE datebeg AND date   LE dateend)
  help, indcg, indtc

  jcg    = indcg
  loncg  = transpose(lon[*,*,jcg], [2, 1, 0]) & help, loncg
  latcg  = transpose(lat[*,*,jcg], [2, 1, 0]) & help, latcg
  monthcg = transpose(month[*,jcg]) & help, monthcg
  yearcg = transpose(year[*,jcg]) & help, yearcg
  uv10cg = transpose(windms[*,*,jcg], [2, 1, 0]) & help, uv10cg
  mslpcg = transpose(pres[*,*,jcg], [2, 1, 0]) & help, mslpcg
  namecg = name[jcg] & help, namecg
  seasoncg = season[jcg] & help, seasoncg
  datecg = transpose(date[*,jcg]) & help, datecg
  nbmonthcg = transpose(nbmonth[*,jcg]) & help, nbmonthcg

  ijtc   = ARRAY_INDICES( date, indtc)
  jtc    = REFORM(ijtc[1,*])
  jtc    = jtc[ UNIQ( jtc, SORT( jtc))]
  lontc  = transpose(lon[*,*,jtc], [2, 1, 0]) & help, lontc
  lattc  = transpose(lat[*,*,jtc], [2, 1, 0]) & help, lattc
  monthtc = transpose(month[*,jtc]) & help, monthtc
  yeartc = transpose(year[*,jtc]) & help, yeartc
  uv10tc = transpose(windms[*,*,jtc], [2, 1, 0]) & help, uv10tc
  mslptc = transpose(pres[*,*,jtc], [2, 1, 0]) & help, mslptc
  nametc = name[jtc] & help, nametc
  seasontc = season[jtc] & help, seasontc
  datetc = transpose(date[*,jtc]) & help, datetc
  nbmonthtc = transpose(nbmonth[*,jtc]) & help, nbmonthtc


  ; CENTER SELECTION
  print, '' & print, 'SELECTION CENTRE:'

  FOR icg = 0, n_elements(indcg)-1 DO BEGIN
    indok = WHERE( FINITE( loncg[ icg,0,*]) EQ 1, /NULL)
    IF n_elements(indok) GT 1 THEN BEGIN
      loncg[ icg, *, 0]  = MEAN(  loncg[ icg, *, indok], DIMENSION=3, /NAN)
      latcg[ icg, *, 0]  = MEAN(  latcg[ icg, *, indok], DIMENSION=3, /NAN)
      uv10cg[ icg, *, 0] = MEAN( uv10cg[ icg, *, indok], DIMENSION=3, /NAN)
      mslpcg[ icg, *, 0] = MEAN( mslpcg[ icg, *, indok], DIMENSION=3, /NAN)
    ENDIF
		IF n_elements(indok) EQ 1 AND indok NE !NULL THEN BEGIN
			loncg[ icg, *, 0]  = loncg[ icg, *, indok]
      latcg[ icg, *, 0]  = latcg[ icg, *, indok]
      uv10cg[ icg, *, 0] = uv10cg[ icg, *, indok]
      mslpcg[ icg, *, 0] = mslpcg[ icg, *, indok]
		ENDIF
  ENDFOR
	loncg  = loncg[*,*,0]
	latcg  = latcg[*,*,0]
	uv10cg = uv10cg[*,*,0]
	mslpcg = mslpcg[*,*,0]
	help, loncg, latcg, uv10cg, mslpcg

  FOR itc = 0, n_elements(jtc)-1 DO BEGIN
    indok = WHERE( FINITE( lontc[ itc,0,*]) EQ 1, /NULL)
    IF n_elements(indok) GT 1 THEN BEGIN
      lontc[ itc, *, 0]  = MEAN(  lontc[ itc, *, indok], DIMENSION=3, /NAN)
      lattc[ itc, *, 0]  = MEAN(  lattc[ itc, *, indok], DIMENSION=3, /NAN)
      uv10tc[ itc, *, 0] = MEAN( uv10tc[ itc, *, indok], DIMENSION=3, /NAN)
      mslptc[ itc, *, 0] = MEAN( mslptc[ itc, *, indok], DIMENSION=3, /NAN)
    ENDIF
    IF n_elements(indok) EQ 1 AND indok NE !NULL THEN BEGIN
      lontc[ itc, *, 0]  = lontc[ itc, *, indok]
      lattc[ itc, *, 0]  = lattc[ itc, *, indok]
      uv10tc[ itc, *, 0] = uv10tc[ itc, *, indok]
      mslptc[ itc, *, 0] = mslptc[ itc, *, indok]
    ENDIF
  ENDFOR
  lontc  = lontc[*,*,0]
  lattc  = lattc[*,*,0]
  uv10tc = uv10tc[*,*,0]
  mslptc = mslptc[*,*,0]
  help, lontc, lattc, uv10tc, mslptc


  ; ZONE SELECTION
  print, '' & print, 'SELECTION ZONE:'
  indcg =  WHERE( loncg[*, 0] GE box[0] AND loncg[*, 0] LE box[1] AND latcg[*, 0] GE box[2] AND latcg[*, 0] LE box[3])
  indtc =  WHERE( lontc GE box[0] AND lontc LE box[1] AND lattc GE box[2] AND lattc LE box[3])
  help, indcg, indtc

  icg = indcg
  loncg = loncg[icg,*,*] & help, loncg
  latcg = latcg[icg,*] & help, latcg
  monthcg = monthcg[icg,*] & help, monthcg
  yearcg = yearcg[icg,*] & help, yearcg
  uv10cg = uv10cg[icg,*] & help, uv10cg
  mslpcg = mslpcg[icg,*] & help, mslpcg
  namecg = namecg[icg] & help, namecg
  seasoncg = seasoncg[icg] & help, seasoncg
  datecg = datecg[icg,*] & help, datecg
  nbmonthcg = nbmonthcg[icg,*] & help, nbmonthcg

  ijtc = ARRAY_INDICES( lontc, indtc)
  itc = REFORM(ijtc[0,*])
  itc = itc[ UNIQ( itc, SORT( itc))]
  lontc = lontc[itc,*] & help, lontc
  lattc = lattc[itc,*] & help, lattc
  monthtc = monthtc[itc,*] & help, monthtc
  yeartc = yeartc[itc,*] & help, yeartc
  uv10tc = uv10tc[itc,*] & help, uv10tc
  mslptc = mslptc[itc,*] & help, mslptc
  nametc = nametc[itc] & help, nametc
  seasontc = seasontc[itc] & help, seasontc
  datetc = datetc[itc,*] & help, datetc
  nbmonthtc = nbmonthtc[itc,*] & help, nbmonthtc


  ; TS only (> 17 m/s)
  print, '' & print, 'SELECTION TS:'
  indts = intarr( n_elements( uv10cg[*,0]))*0
  FOR i = 0, n_elements(uv10cg[*,0])-1 DO IF (where(uv10cg[i,*] GE 17.22))[0] NE -1 THEN indts[i] = 1
  indok = where(indts EQ 1)
  latcg = latcg[indok,*] & help, latcg
  loncg = loncg[indok,*] & help, loncg
  monthcg = monthcg[indok,*] & help, monthcg
  yearcg = yearcg[indok,*] & help, yearcg
  uv10cg = uv10cg[indok,*] & help, uv10cg
  mslpcg = mslpcg[indok,*] & help, mslpcg
  namecg = namecg[indok] & help, namecg
  seasoncg = seasoncg[indok] & help, seasoncg
  datecg = datecg[indok,*] & help, datecg
  nbmonthcg = nbmonthcg[indok,*] & help, nbmonthcg

  indts = intarr( n_elements( uv10tc[*,0]))*0
  FOR i = 0, n_elements(uv10tc[*,0])-1 DO IF (where(uv10tc[i,*] GE 17.22))[0] NE -1 THEN indts[i] = 1
  indok = where(indts EQ 1)
  lattc = lattc[indok,*] & help, lattc
  lontc = lontc[indok,*] & help, lontc
  monthtc = monthtc[indok,*] & help, monthtc
  yeartc = yeartc[indok,*] & help, yeartc
  uv10tc = uv10tc[indok,*] & help, uv10tc
  mslptc = mslptc[indok,*] & help, mslptc
  nametc = nametc[indok] & help, nametc
  seasontc = seasontc[indok] & help, seasontc
  datetc = datetc[indok,*] & help, datetc
  nbmonthtc = nbmonthtc[indok,*] & help, nbmonthtc
	IF debug_TC THEN STOP


  ; calcul de vdep
  ;vdeptc = lontc*0. & help, vdeptc 
  ;FOR i = 0, n_elements(lontc[*,0])-1 DO BEGIN
  ;  indok = where(finite(lontc[i,*]) EQ 1, cntok)
  ;  lon1 = reform(lontc[i,indok])
  ;  lon2 = reform(shift(lontc[i,indok],-1)) & lon2[cntok-1] = lon2[cntok-2]
  ;  lat1 = reform(lattc[i,indok])
  ;  lat2 = reform(shift(lattc[i,indok],-1)) & lat2[cntok-1] = lat2[cntok-2]
  ;  vdeptc[i,indok] = map_npoints(lon1,lat1,lon2,lat2,/TWO_BY_TWO) / (6.*3600.)
  ;ENDFOR



  bin_size   = 5.  ; en degres
  cg_density = HIST_2D( loncg[*,0], latcg[*,0], bin1=bin_size, min1=box[0], max1=box[1], bin2=bin_size, min2=box[2], max2=box[3]) / float(nbyear_obs-1)
  tc_density = HIST_2D( lontc     , lattc     , bin1=bin_size, min1=box[0], max1=box[1], bin2=bin_size, min2=box[2], max2=box[3]) / (float(nbyear_obs-1) * 4.)
  help, bin_size, nbyear_obs, cg_density, tc_density

  nbbin_lon = ( box[1] - box[0] ) / bin_size + 1
  nbbin_lat = ( box[3] - box[2] ) / bin_size + 1
  bins_lon  = box[0] + findgen(nbbin_lon) * bin_size
  bins_lat  = box[2] + findgen(nbbin_lat) * bin_size
  help, nbbin_lat, nbbin_lon, bins_lat, bins_lon

  c_smooth = 1 ; coef smoothing
  scg_density = cg_density * 0.
  scg_density[*,where(bins_lat LT 0.)] = SMOOTH( cg_density[*,where(bins_lat LT 0.)], c_smooth)
  scg_density[*,where(bins_lat GT 0.)] = SMOOTH( cg_density[*,where(bins_lat GT 0.)], c_smooth)
  stc_density = tc_density * 0.
  stc_density[*,where(bins_lat LT 0.)] = SMOOTH( tc_density[*,where(bins_lat LT 0.)], c_smooth)
  stc_density[*,where(bins_lat GT 0.)] = SMOOTH( tc_density[*,where(bins_lat GT 0.)], c_smooth)

	@plot_TC2D

  nbyear   = nbyear_obs
  nbmonth  = nbmonth_obs
  listyear = listyear_obs
  @calcul_TC_stats


  ; SAUVEGARDE
  tcvar_list = [ 'ano_nbcg_1y', 'ano_nbct_1y', 'ano_nbtcday_1y', 'ano_nbtgday_1y', 'nbct_1y', 'nbtgday_1y', 'nbcg_1y', 'nbtcday_1y', 'stc_density', 'scg_density', 'lattc', 'lontc', 'monthtc', 'yeartc', 'uv10tc', 'mslptc', 'datetc', 'nbmonthtc', 'nbmonthcg', 'loncg', 'latcg', 'yearcg','seasoncg','seasontc']
  FOR l = 0, n_elements(tcvar_list)-1 DO BEGIN
    cmd = execute( tcvar_list[l] + '_' + STRTRIM(e,2) + ' = ' + tcvar_list[l] )
    cmd = execute( 'help, '+tcvar_list[l] + '_' + STRTRIM(e,2) )
  ENDFOR

  print, 'read_TC_ibtracks OK' & print, ''

ENDIF
