PRO plot_valid2D
@all_cm

exp_name = 'COUPLED_SW2_KF'
var_list = ['UV10']
bassin   = 'IO'
season_list = ['DJFM','JJAS']

IF bassin EQ  'IO' THEN box = [30,130,-30,25]
IF bassin EQ 'SIO' THEN box = [30,130,-30, 0]
IF bassin EQ 'NIO' THEN box = [30,130,  0,25]


FOR j = 0, n_elements(season_list)-1 DO BEGIN
FOR i = 0, n_elements(var_list)-1 DO BEGIN

  season = season_list[j]
  var_name = var_list[i]

  IF var_name EQ 'UV10' THEN BEGIN

    path_obs = '/Users/gslod/WORK/DATA/WIND-QSCAT/'
    file_obs = 'wind_QSCAT_monthly_clim_2000-2008.nc'
    initncdf, path_obs + file_obs
    domdef, box

    var_obs   = read_ncdf('U',0,11, /timestep, filename=path_obs+file_obs, /nostruct) & help, var_obs
    IF season EQ 'DJFM' THEN var_obs = (var_obs[*,*,11] + total(var_obs[*,*,0:2],3)) / 4
    IF season EQ 'JJAS' THEN var_obs =  total(var_obs[*,*,5:8],3) / 4 & help, var_obs
    u10_obs   = var_obs
    var_obs   = read_ncdf('V',0,11, /timestep, filename=path_obs+file_obs, /nostruct) & help, var_obs
    IF season EQ 'DJFM' THEN var_obs = (var_obs[*,*,11] + total(var_obs[*,*,0:2],3)) / 4
    IF season EQ 'JJAS' THEN var_obs =  total(var_obs[*,*,5:8],3) / 4 & help, var_obs
    v10_obs   = var_obs
    uv10_obs  = sqrt(u10_obs^2+v10_obs^2)
    angle_obs = windangle(u10_obs,v10_obs)
    gphit_obs = gphit[firstxt:lastxt,firstyt:lastyt] & help, gphit_obs
    glamt_obs = glamt[firstxt:lastxt,firstyt:lastyt] & help, glamt_obs

    path_mod  = '/Volumes/TIME_MACHINE/EXP_'+exp_name+'/WRF/POST/MONTHLY/'
    file_mod = 'wrfout_monthly_clim_1990-2009.nc'
    maskfile = '/Users/gslod/WORK/IDL/MASK_WRF_GRID_T.nc'
    mask = ncdf_lec(maskfile, var = 'LANDMASK')
    initncdf, maskfile
    domdef, box

    var_mod = read_ncdf('U10',0,11, /timestep, filename=path_mod+file_mod, /nostruct) & help, var_mod
    IF season EQ 'DJFM' THEN var_mod = (var_mod[*,*,11] + total(var_mod[*,*,0:2],3)) / 4
    IF season EQ 'JJAS' THEN var_mod =  total(var_mod[*,*,5:8],3) / 4 & help, var_mod
    u10_mod = var_mod & u10_mod[where(mask[firstxt:lastxt,firstyt:lastyt] EQ 1)] = !values.f_nan
    var_mod = read_ncdf('V10',0,11, /timestep, filename=path_mod+file_mod, /nostruct) & help, var_mod
    IF season EQ 'DJFM' THEN var_mod = (var_mod[*,*,11] + total(var_mod[*,*,0:2],3)) / 4
    IF season EQ 'JJAS' THEN var_mod =  total(var_mod[*,*,5:8],3) / 4 & help, var_mod
    v10_mod = var_mod & v10_mod[where(mask[firstxt:lastxt,firstyt:lastyt] EQ 1)] = !values.f_nan
    uv10_mod  = sqrt(u10_mod^2+v10_mod^2)
    gphit_mod = gphit[firstxt:lastxt,firstyt:lastyt] & help, gphit_mod
    glamt_mod = glamt[firstxt:lastxt,firstyt:lastyt] & help, glamt_mod
    u10_mod_gridobs = fromreg('bilinear', u10_mod, glamt_mod, gphit_mod, glamt_obs, gphit_obs)
    v10_mod_gridobs = fromreg('bilinear', v10_mod, glamt_mod, gphit_mod, glamt_obs, gphit_obs)
    uv10_mod_gridobs = fromreg('bilinear', uv10_mod, glamt_mod, gphit_mod, glamt_obs, gphit_obs)
    angle_mod_gridobs = windangle(u10_mod_gridobs,v10_mod_gridobs)

    initncdf, path_obs + file_obs
    domdef, box
    plt, uv10_mod_gridobs, 0, 15, /realcont, lct=34, title='', subtitle=' ', xtitle='', ytitle='', cb_charsize=1.2, format='(I3)'
    ajoutvect, {u:{a:u10_mod_gridobs, g:'T'}, v:{a:v10_mod_gridobs, g:'T'}}, unvectsur = [20, 20], vectmin = 0, vectmax = 11
    saveimage, 'FIGURES/'+var_name+'_mod_2D_'+season+'_'+exp_name+'.gif'
    plt, uv10_obs, 0, 15, /realcont, lct=34, title='', subtitle=' ', xtitle='', ytitle='', cb_charsize=1.2, format='(I3)'
    ajoutvect, {u:{a:u10_obs, g:'T'}, v:{a:v10_obs, g:'T'}}, unvectsur = [20, 20], vectmin = 0, vectmax = 11
    saveimage, 'FIGURES/'+var_name+'_obs_2D_'+season+'_'+exp_name+'.gif'
    plt, uv10_mod_gridobs - uv10_obs, -5, 5, /realcont, lct=77, title='',subtitle=' ', xtitle='', ytitle='', cb_charsize=1.2, format='(I3)'
    saveimage, 'FIGURES/'+var_name+'_diff_2D_'+season+'_'+exp_name+'.gif'

    plt, angle_mod_gridobs+90, /realcont, lct=34, title='', subtitle=' ', xtitle='', ytitle='', cb_charsize=1.2, format='(I3)'
    saveimage, 'FIGURES/'+'ANGLE_mod_2D_'+season+'_'+exp_name+'.gif'
    plt, angle_obs+90, /realcont, lct=34, title='', subtitle=' ', xtitle='', ytitle='', cb_charsize=1.2, format='(I3)'
    saveimage, 'FIGURES/'+'ANGLE_obs_2D_'+season+'_'+exp_name+'.gif'
    plt, angle_mod_gridobs - angle_obs, -360, 360, /realcont, lct=77, title='',subtitle=' ', xtitle='', ytitle='', cb_charsize=1.2, format='(I4)'
    saveimage, 'FIGURES/'+'ANGLE_diff_2D_'+season+'_'+exp_name+'.gif'

  ENDIF


  IF var_name EQ 'SST' THEN BEGIN

    path_obs = '/Users/gslod/WORK/DATA/SST-TMI/'
    file_obs = 'SST_SC_MONTHLY_TMI-AMSR_1998-2009_IO.nc'
    initncdf, path_obs + file_obs
    domdef, box

    var_obs   = read_ncdf(var_name,0,11, /timestep, filename=path_obs+file_obs, /nostruct) & help, var_obs
    IF season EQ 'DJFM' THEN var_obs = (var_obs[*,*,11] + total(var_obs[*,*,0:2],3)) / 4
    IF season EQ 'JJAS' THEN var_obs = total(var_obs[*,*,5:8],3) / 4 & help, var_obs
    gphit_obs = gphit[firstxt:lastxt,firstyt:lastyt] & help, gphit_obs
    glamt_obs = glamt[firstxt:lastxt,firstyt:lastyt] & help, glamt_obs
    mask_obs  = fltarr(size(glamt_obs, /dim))+1

    path_mod  = '/Volumes/TIME_MACHINE/EXP_'+exp_name+'/NEMO/POST/MONTHLY/'
    file_mod = 'nemo_out_monthly_clim_1990-2009.nc'
    initncdf, path_mod + file_mod
    domdef, box

    var_mod = read_ncdf('thetao',0,11, /timestep, filename=path_mod+file_mod, /nostruct) & help, var_mod
    var_mod[where(var_mod EQ 0)] = !values.f_nan
    var_mod = reform(var_mod[*,*,0,*]) & help, var_mod
    IF season EQ 'DJFM' THEN var_mod = (var_mod[*,*,11] + total(var_mod[*,*,0:2],3)) / 4 & help, var_mod
    IF season EQ 'JJAS' THEN var_mod =  total(var_mod[*,*,5:8],3) / 4 & help, var_mod
    gphit_mod = gphit[firstxt:lastxt,firstyt:lastyt] & help, gphit_mod
    glamt_mod = glamt[firstxt:lastxt,firstyt:lastyt] & help, glamt_mod
    mask_mod  = fltarr(size(glamt_mod, /dim))+1
    var_mod_gridobs = fromirr('bilinear', var_mod, glamt_mod, gphit_mod, mask_mod, glamt_obs, gphit_obs, mask_obs) & help, var_mod_gridobs
 

    initncdf, path_obs + file_obs
    domdef, box
    plt, var_mod_gridobs, 21, 31, /realcont, lct=34, title='', subtitle=' ', xtitle='', ytitle='', cb_charsize=1.2, format='(I2)'
    saveimage, 'FIGURES/'+var_name+'_mod_2D_'+season+'_'+exp_name+'.gif'
    plt, var_obs, 21, 31, /realcont, lct=34, title='', subtitle=' ', xtitle='', ytitle='', cb_charsize=1.2, format='(I2)'
    saveimage, 'FIGURES/'+var_name+'_obs_2D_'+season+'_'+exp_name+'.gif'
    plt, var_mod_gridobs - var_obs, -2, 2, /realcont, lct=77, title='',subtitle=' ', cb_charsize=1.2, format='(F4.1)'
    saveimage, 'FIGURES/'+var_name+'_diff_2D_'+season+'_'+exp_name+'.gif'

  ENDIF


  IF var_name EQ 'D20' THEN BEGIN

    path_obs = '/Users/gslod/WORK/DATA/WOA_2009/DATA/'
    file_obs = 'woa09_T_monthly_world.nc'
    initncdf, path_obs + file_obs
    domdef, box

    var_obs   = read_ncdf('temp',0,11, /timestep, filename=path_obs+file_obs, /nostruct) & help, var_obs
    IF season EQ 'DJFM' THEN var_obs = (var_obs[*,*,*,11] + total(var_obs[*,*,*,0:2],4)) / 4
    IF season EQ 'JJAS' THEN var_obs = total(var_obs[*,*,*,5:8],4) / 4 & help, var_obs
    gphit_obs = gphit[firstxt:lastxt,firstyt:lastyt] & help, gphit_obs
    glamt_obs = glamt[firstxt:lastxt,firstyt:lastyt] & help, glamt_obs
    mask_obs  = fltarr(size(glamt_obs, /dim))+1
    d20_obs   = fltarr(size(glamt_obs, /dim))

    FOR iy=0,nyt-1 DO BEGIN
      FOR ix=0,nxt-1 DO BEGIN
        IF finite(var_obs[ix,iy,0]) EQ 1 THEN BEGIN
          profil_t = reform(var_obs[ix,iy,*])
          diff_t = profil_t - 20.
          ig20 = where(diff_t GE 0.)
          il20 = where(diff_t LT 0.)
          IF ig20[0] NE -1 AND il20[0] NE -1 THEN BEGIN
            d20_obs[ix,iy] = interpol(gdept,diff_t,0.)
            IF d20_obs[ix,iy] LT 0 THEN stop
          ENDIF
        ENDIF
      ENDFOR
    ENDFOR
    var_obs = d20_obs
    var_obs[where(var_obs EQ 0)] = !values.f_nan

    path_mod  = '/Volumes/TIME_MACHINE/EXP_'+exp_name+'/NEMO/POST/MONTHLY/'
    file_mod = 'nemo_out_monthly_clim_1990-2009.nc'
    initncdf, path_mod + file_mod
    domdef, box

    var_mod = read_ncdf('thetao',0,11, /timestep, filename=path_mod+file_mod, /nostruct) & help, var_mod
    var_mod[where(var_mod EQ 0)] = !values.f_nan
    IF season EQ 'DJFM' THEN var_mod = (var_mod[*,*,*,11] + total(var_mod[*,*,*,0:2],4)) / 4 & help, var_mod
    IF season EQ 'JJAS' THEN var_mod =  total(var_mod[*,*,*,5:8],4) / 4 & help, var_mod
    gphit_mod = gphit[firstxt:lastxt,firstyt:lastyt] & help, gphit_mod
    glamt_mod = glamt[firstxt:lastxt,firstyt:lastyt] & help, glamt_mod
    mask_mod  = fltarr(size(glamt_mod, /dim))+1
    d20_mod   = fltarr(size(glamt_mod, /dim))

    FOR iy = 0,nyt-1 DO BEGIN
      FOR ix = 0,nxt-1 DO BEGIN
        IF finite(var_mod[ix,iy,0]) EQ 1 THEN BEGIN
          profil_t = reform(var_mod[ix,iy,*])
          diff_t = profil_t - 20.
          ig20 = where(diff_t GE 0.)
          il20 = where(diff_t LT 0.)
          IF ig20[0] NE -1 AND il20[0] NE -1 THEN BEGIN
            d20_mod[ix,iy] = interpol(gdept,diff_t,0.)
            IF d20_mod[ix,iy] LT 0 THEN stop
          ENDIF
        ENDIF
     ENDFOR
   ENDFOR
    var_mod = d20_mod
    var_mod[where(var_mod EQ 0)] = !values.f_nan
    var_mod_gridobs = fromirr('bilinear', var_mod, glamt_mod, gphit_mod, mask_mod, glamt_obs, gphit_obs, mask_obs) & help, var_mod_gridobs


    initncdf, path_obs + file_obs
    domdef, box
    plt, var_mod_gridobs, 0, 200, /realcont, lct=34, title='', subtitle=' ', xtitle='', ytitle='', cb_charsize=1.2, format='(I3)'
    saveimage, 'FIGURES/'+var_name+'_mod_2D_'+season+'_'+exp_name+'.gif'
    plt, var_obs, 0, 200, /realcont, lct=34, title='', subtitle=' ', xtitle='', ytitle='', cb_charsize=1.2, format='(I3)'
    saveimage, 'FIGURES/'+var_name+'_obs_2D_'+season+'_'+exp_name+'.gif'
    plt, var_mod_gridobs - var_obs, -50, 50, /realcont, lct=77, title='',subtitle=' ', cb_charsize=1.2, format='(F5.1)'
    saveimage, 'FIGURES/'+var_name+'_diff_2D_'+season+'_'+exp_name+'.gif'

  ENDIF


  IF var_name EQ 'CI' THEN BEGIN

    delta_sst = 2
    path_obs = '/Users/gslod/WORK/DATA/WOA_2009/DATA/'
    file_obs = 'woa09_T_monthly_world.nc'
    initncdf, path_obs + file_obs
    domdef, box

    var_obs   = read_ncdf('temp',0,11, /timestep, filename=path_obs+file_obs, /nostruct) & help, var_obs
    IF season EQ 'DJFM' THEN var_obs = (var_obs[*,*,*,11] + total(var_obs[*,*,*,0:2],4)) / 4
    IF season EQ 'JJAS' THEN var_obs = total(var_obs[*,*,*,5:8],4) / 4 & help, var_obs
    tmp_obs = var_obs
    path_obs = '/Users/gslod/WORK/DATA/WOA_2009/DATA/'
    file_obs = 'woa09_S_monthly_world.nc'
    var_obs   = read_ncdf('s',0,11, /timestep, filename=path_obs+file_obs, /nostruct) & help, var_obs
    IF season EQ 'DJFM' THEN var_obs = (var_obs[*,*,*,11] + total(var_obs[*,*,*,0:2],4)) / 4
    IF season EQ 'JJAS' THEN var_obs = total(var_obs[*,*,*,5:8],4) / 4 & help, var_obs
    sal_obs = var_obs
    gphit_obs = gphit[firstxt:lastxt,firstyt:lastyt] & help, gphit_obs
    glamt_obs = glamt[firstxt:lastxt,firstyt:lastyt] & help, glamt_obs
    mask_obs  = fltarr(size(glamt_obs, /dim))+1
    rho_obs = rhon(sal_obs,tmp_obs) & help, rho_obs
    ci_obs  = fltarr(nxt,nyt) & ci_obs[*,*] = !values.f_nan
      
    FOR iy=0,nyt-1 DO BEGIN
      FOR ix=0,nxt-1 DO BEGIN
        profil_t = reform(tmp_obs[ix,iy,*])
        IF total(finite(profil_t)) GE 2 THEN BEGIN
          profil_rho = reform(rho_obs[ix,iy,*])
          profil_dept = gdept
          epot = calc_delta_epot_pour_1_delta_sst(delta_sst,profil_t+273.15,profil_rho,profil_dept,e3t,h_CM=h_cm,prof_t10_m2=prof_t10_m2,/interp_2m)
          ci_obs[ix,iy] = epot^(1./3.)
        ENDIF
      ENDFOR
    ENDFOR
    var_obs = ci_obs

    path_mod  = '/Volumes/TIME_MACHINE/EXP_'+exp_name+'/NEMO/POST/MONTHLY/'
    file_mod = 'nemo_out_monthly_clim_1990-2009.nc'
    initncdf, path_mod + file_mod
    domdef, box

    var_mod = read_ncdf('thetao',0,11, /timestep, filename=path_mod+file_mod, /nostruct) & help, var_mod
    var_mod[where(var_mod EQ 0)] = !values.f_nan
    IF season EQ 'DJFM' THEN var_mod = (var_mod[*,*,*,11] + total(var_mod[*,*,*,0:2],4)) / 4 & help, var_mod
    IF season EQ 'JJAS' THEN var_mod =  total(var_mod[*,*,*,5:8],4) / 4 & help, var_mod
    tmp_mod = var_mod
    var_mod = read_ncdf('so',0,11, /timestep, filename=path_mod+file_mod, /nostruct) & help, var_mod
    var_mod[where(var_mod EQ 0)] = !values.f_nan
    IF season EQ 'DJFM' THEN var_mod = (var_mod[*,*,*,11] + total(var_mod[*,*,*,0:2],4)) / 4 & help, var_mod
    IF season EQ 'JJAS' THEN var_mod =  total(var_mod[*,*,*,5:8],4) / 4 & help, var_mod
    sal_mod = var_mod
    gphit_mod = gphit[firstxt:lastxt,firstyt:lastyt] & help, gphit_mod
    glamt_mod = glamt[firstxt:lastxt,firstyt:lastyt] & help, glamt_mod
    mask_mod  = fltarr(size(glamt_mod, /dim))+1
    rho_mod   = rhon(sal_mod,tmp_mod) & help, rho_mod
    ci_mod    = fltarr(size(glamt_mod, /dim))

    FOR iy=0,nyt-1 DO BEGIN
      FOR ix=0,nxt-1 DO BEGIN
        profil_t = reform(tmp_mod[ix,iy,*])
        IF total(finite(profil_t)) GE 2 THEN BEGIN
          profil_rho = reform(rho_mod[ix,iy,*])
          profil_dept = gdept
          epot = calc_delta_epot_pour_1_delta_sst(delta_sst,profil_t+273.15,profil_rho,profil_dept,e3t, $
          h_CM=h_cm,prof_t10_m2=prof_t10_m2,/interp_2m)
          ci_mod[ix,iy] = epot^(1./3.)
        ENDIF
      ENDFOR
    ENDFOR

    var_mod = ci_mod
    var_mod_gridobs = fromirr('bilinear', var_mod, glamt_mod, gphit_mod, mask_mod, glamt_obs, gphit_obs, mask_obs) & help, var_mod_gridobs


    initncdf, path_obs + file_obs
    domdef, box
    plt, var_mod_gridobs, 0, 50, /realcont, lct=34, title='', subtitle=' ', xtitle='', ytitle='', cb_charsize=1.2, format='(I3)'
    saveimage, 'FIGURES/'+var_name+'_mod_2D_'+season+'_'+exp_name+'.gif'
    plt, var_obs, 0, 50, /realcont, lct=34, title='', subtitle=' ', xtitle='', ytitle='', cb_charsize=1.2, format='(I3)'
    saveimage, 'FIGURES/'+var_name+'_obs_2D_'+season+'_'+exp_name+'.gif'
    plt, var_mod_gridobs - var_obs, -10, 10, /realcont, lct=77, title='',subtitle=' ', cb_charsize=1.2, format='(F5.1)'
    saveimage, 'FIGURES/'+var_name+'_diff_2D_'+season+'_'+exp_name+'.gif'

  ENDIF


  IF var_name EQ 'MLD' THEN BEGIN

    path_obs = '/Users/gslod/WORK/DATA/MLD-CLEM/DATA/'
    file_obs = 'mld_DT02.nc'
    initncdf, path_obs + file_obs
    domdef, box

    var_obs   = read_ncdf('mld',0,11, /timestep, filename=path_obs+file_obs, /nostruct) & help, var_obs
    var_obs[where(var_obs GT 500)] = !values.f_nan
    IF season EQ 'DJFM' THEN var_obs = (var_obs[*,*,11] + total(var_obs[*,*,0:2],3)) / 4
    IF season EQ 'JJAS' THEN var_obs = total(var_obs[*,*,5:8],3) / 4 & help, var_obs
    gphit_obs = gphit[firstxt:lastxt,firstyt:lastyt] & help, gphit_obs
    glamt_obs = glamt[firstxt:lastxt,firstyt:lastyt] & help, glamt_obs
    mask_obs  = fltarr(size(glamt_obs, /dim))+1

    path_mod  = '/Volumes/TIME_MACHINE/EXP_'+exp_name+'/NEMO/POST/MONTHLY/'
    file_mod = 'nemo_out_monthly_clim_1990-2009.nc'
    initncdf, path_mod + file_mod
    domdef, box

    var_mod = read_ncdf('thetao',0,11, /timestep, filename=path_mod+file_mod, /nostruct) & help, var_mod
    var_mod[where(var_mod EQ 0)] = !values.f_nan
    IF season EQ 'DJFM' THEN var_mod = (var_mod[*,*,*,11] + total(var_mod[*,*,*,0:2],4)) / 4 & help, var_mod
    IF season EQ 'JJAS' THEN var_mod =  total(var_mod[*,*,*,5:8],4) / 4 & help, var_mod
    gphit_mod = gphit[firstxt:lastxt,firstyt:lastyt] & help, gphit_mod
    glamt_mod = glamt[firstxt:lastxt,firstyt:lastyt] & help, glamt_mod
    mask_mod  = fltarr(size(glamt_mod, /dim))+1
    mld_mod   = fltarr(size(glamt_mod, /dim))

    FOR iy = 0,nyt-1 DO BEGIN
      FOR ix = 0,nxt-1 DO BEGIN
        IF finite(var_mod[ix,iy,0]) EQ 1 THEN BEGIN 
          profil_t = reform(var_mod[ix,iy,*])
          temp_crit = profil_t[1] - 0.2
          diff_t = profil_t - temp_crit[0]
          igmldt = where(diff_t GE 0.)
          ilmldt = where(diff_t LT 0.)
          IF igmldt[0] NE -1 AND ilmldt[0] NE -1 THEN BEGIN
            mldt = interpol(gdept,profil_t,temp_crit[0])
            mld_mod[ix,iy] = mldt
            IF mldt LT 0 THEN stop
          ENDIF
        ENDIF
     ENDFOR
   ENDFOR

    var_mod = mld_mod
    var_mod_gridobs = fromirr('bilinear', var_mod, glamt_mod, gphit_mod, mask_mod, glamt_obs, gphit_obs, mask_obs) & help, var_mod_gridobs


    initncdf, path_obs + file_obs
    domdef, box
    plt, var_mod_gridobs, 0, 100, /realcont, lct=34, title='', subtitle=' ', xtitle='', ytitle='', cb_charsize=1.2, format='(I3)'
    saveimage, 'FIGURES/'+var_name+'_mod_2D_'+season+'_'+exp_name+'.gif'
    plt, var_obs, 0, 100, /realcont, lct=34, title='', subtitle=' ', xtitle='', ytitle='', cb_charsize=1.2, format='(I3)'
    saveimage, 'FIGURES/'+var_name+'_obs_2D_'+season+'_'+exp_name+'.gif'
    plt, var_mod_gridobs - var_obs, -30, 30, /realcont, lct=77, title='',subtitle=' ', cb_charsize=1.2, format='(F5.1)'
    saveimage, 'FIGURES/'+var_name+'_diff_2D_'+season+'_'+exp_name+'.gif'

  ENDIF


  IF var_name EQ 'SSS' THEN BEGIN

    path_obs = '/Users/gslod/WORK/DATA/WOA_2009/DATA/'
    file_obs = 'woa09_S_monthly_world.nc'
    initncdf, path_obs + file_obs
    domdef, box

    var_obs = read_ncdf('s',0,11, /timestep, filename=path_obs+file_obs, /nostruct) & help, var_obs
    var_obs = reform(var_obs[*,*,0,*])
    IF season EQ 'DJFM' THEN var_obs = (var_obs[*,*,11] + total(var_obs[*,*,0:2],3)) / 4
    IF season EQ 'JJAS' THEN var_obs = total(var_obs[*,*,5:8],3) / 4 & help, var_obs
    gphit_obs = gphit[firstxt:lastxt,firstyt:lastyt] & help, gphit_obs
    glamt_obs = glamt[firstxt:lastxt,firstyt:lastyt] & help, glamt_obs
    mask_obs  = fltarr(size(glamt_obs, /dim))+1

    path_mod  = '/Volumes/TIME_MACHINE/EXP_'+exp_name+'/NEMO/POST/MONTHLY/'
    file_mod = 'nemo_out_monthly_clim_1990-2009.nc'
    initncdf, path_mod + file_mod
    domdef, box

    var_mod = read_ncdf('so',0,11, /timestep, filename=path_mod+file_mod, /nostruct) & help, var_mod
    var_mod[where(var_mod EQ 0)] = !values.f_nan
    var_mod = reform(var_mod[*,*,0,*]) & help, var_mod
    IF season EQ 'DJFM' THEN var_mod = (var_mod[*,*,11] + total(var_mod[*,*,0:2],3)) / 4 & help, var_mod
    IF season EQ 'JJAS' THEN var_mod =  total(var_mod[*,*,5:8],3) / 4 & help, var_mod
    gphit_mod = gphit[firstxt:lastxt,firstyt:lastyt] & help, gphit_mod
    glamt_mod = glamt[firstxt:lastxt,firstyt:lastyt] & help, glamt_mod
    mask_mod  = fltarr(size(glamt_mod, /dim))+1
    var_mod_gridobs = fromirr('bilinear', var_mod, glamt_mod, gphit_mod, mask_mod, glamt_obs, gphit_obs, mask_obs) & help, var_mod_gridobs


    initncdf, path_obs + file_obs
    domdef, box
    plt, var_mod_gridobs, 31, 37, /realcont, lct=34, title='', subtitle=' ', xtitle='', ytitle='', cb_charsize=1.2, format='(I2)'
    saveimage, 'FIGURES/'+var_name+'_mod_2D_'+season+'_'+exp_name+'.gif'
    plt, var_obs, 31, 37, /realcont, lct=34, title='', subtitle=' ', xtitle='', ytitle='', cb_charsize=1.2, format='(I2)'
    saveimage, 'FIGURES/'+var_name+'_obs_2D_'+season+'_'+exp_name+'.gif'
    plt, var_mod_gridobs - var_obs, -2, 2, /realcont, lct=77, title='',subtitle=' ', cb_charsize=1.2, format='(F4.1)'
    saveimage, 'FIGURES/'+var_name+'_diff_2D_'+season+'_'+exp_name+'.gif'

  ENDIF


  IF var_name EQ 'RAIN' THEN BEGIN

;    path_obs = '/Users/gslod/WORK/DATA/PRECIP-TRMM_3B43/'
;    file_obs = 'precip_trmm_clim_monthly_1998-2010.nc'
    path_obs = '/Users/gslod/WORK/DATA/PRECIP-GPCP/'
    file_obs = 'precip.mon.ltm.nc'
    initncdf, path_obs + file_obs
    domdef, box

;    var_obs = read_ncdf('pcp',0,11, /timestep, filename=path_obs+file_obs, /nostruct) & help, var_obs
    var_obs = read_ncdf('precip',0,11, /timestep, filename=path_obs+file_obs, /nostruct) & help, var_obs 
    IF season EQ 'DJFM' THEN var_obs = (var_obs[*,*,11] + total(var_obs[*,*,0:2],3)) / 4
    IF season EQ 'JJAS' THEN var_obs = total(var_obs[*,*,5:8],3) / 4 & help, var_obs
    gphit_obs = gphit[firstxt:lastxt,firstyt:lastyt] & help, gphit_obs
    glamt_obs = glamt[firstxt:lastxt,firstyt:lastyt] & help, glamt_obs


    path_mod  = '/Volumes/TIME_MACHINE/EXP_'+exp_name+'/WRF/POST/MONTHLY/'
    file_mod = 'wrfout_monthly_clim_1990-2009.nc'
    maskfile = '/Users/gslod/WORK/IDL/MASK_WRF_GRID_T.nc'
    initncdf, maskfile
    domdef, box

    var_mod = read_ncdf('RAINC',0,11, /timestep, filename=path_mod+file_mod, /nostruct) & help, var_mod
    IF season EQ 'DJFM' THEN var_mod = (var_mod[*,*,11] + total(var_mod[*,*,0:2],3)) / 4 & help, var_mod
    IF season EQ 'JJAS' THEN var_mod =  total(var_mod[*,*,5:8],3) / 4 & help, var_mod
    gphit_mod = gphit[firstxt:lastxt,firstyt:lastyt] & help, gphit_mod
    glamt_mod = glamt[firstxt:lastxt,firstyt:lastyt] & help, glamt_mod
    var_mod_gridobs = fromreg('bilinear', var_mod, glamt_mod, gphit_mod, glamt_obs, gphit_obs) & help, var_mod_gridobs


    initncdf, path_obs + file_obs
    domdef, box
    plt, var_mod_gridobs, 0, 20, /realcont, lct=34, title='', subtitle=' ', xtitle='', ytitle='', cb_charsize=1.2, format='(I2)'
    saveimage, 'FIGURES/'+var_name+'_mod_2D_'+season+'_'+exp_name+'.gif'
    plt, var_obs, 0, 20, /realcont, lct=34, title='', subtitle=' ', xtitle='', ytitle='', cb_charsize=1.2, format='(I2)'
    saveimage, 'FIGURES/'+var_name+'_obs_2D_'+season+'_'+exp_name+'.gif'
    plt, var_mod_gridobs - var_obs, -10, 10, /realcont, lct=77, title='',subtitle=' ', cb_charsize=1.2, format='(I3)'
    saveimage, 'FIGURES/'+var_name+'_diff_2D_'+season+'_'+exp_name+'.gif'

  ENDIF


  IF var_name EQ 'NSW' THEN BEGIN

    path_obs = '/Users/gslod/WORK/DATA/TROPFLUX/'
    file_obs = 'swr_tropflux_1m_clim.nc'
    initncdf, path_obs + file_obs
    domdef, box

    var_obs = read_ncdf('swr',0,11, /timestep, filename=path_obs+file_obs, /nostruct) & help, var_obs
    IF season EQ 'DJFM' THEN var_obs = (var_obs[*,*,11] + total(var_obs[*,*,0:2],3)) / 4
    IF season EQ 'JJAS' THEN var_obs = total(var_obs[*,*,5:8],3) / 4 & help, var_obs
    gphit_obs = gphit[firstxt:lastxt,firstyt:lastyt] & help, gphit_obs
    glamt_obs = glamt[firstxt:lastxt,firstyt:lastyt] & help, glamt_obs


    path_mod  = '/Volumes/TIME_MACHINE/EXP_'+exp_name+'/WRF/POST/MONTHLY/'
    file_mod = 'wrfout_monthly_clim_1990-2009.nc'
    maskfile = '/Users/gslod/WORK/IDL/MASK_WRF_GRID_T.nc'
    initncdf, maskfile
    domdef, box

    var_mod = read_ncdf('GSW',0,11, /timestep, filename=path_mod+file_mod, /nostruct) -273.15 & help, var_mod
    IF season EQ 'DJFM' THEN var_mod = (var_mod[*,*,11] + total(var_mod[*,*,0:2],3)) / 4 & help, var_mod
    IF season EQ 'JJAS' THEN var_mod =  total(var_mod[*,*,5:8],3) / 4 & help, var_mod
    gphit_mod = gphit[firstxt:lastxt,firstyt:lastyt] & help, gphit_mod
    glamt_mod = glamt[firstxt:lastxt,firstyt:lastyt] & help, glamt_mod
    var_mod_gridobs = fromreg('bilinear', var_mod, glamt_mod, gphit_mod, glamt_obs, gphit_obs) & help, var_mod_gridobs


    initncdf, path_obs + file_obs
    domdef, box
    plt, var_mod_gridobs, /realcont, lct=34, title='', subtitle=' ', xtitle='', ytitle='', cb_charsize=1.2, format='(I4)'
    saveimage, 'FIGURES/'+var_name+'_mod_2D_'+season+'_'+exp_name+'.gif'
    plt, var_obs, /realcont, lct=34, title='', subtitle=' ', xtitle='', ytitle='', cb_charsize=1.2, format='(I4)'
    saveimage, 'FIGURES/'+var_name+'_obs_2D_'+season+'_'+exp_name+'.gif'
    plt, var_mod_gridobs - var_obs, /realcont, lct=77, title='',subtitle=' ', cb_charsize=1.2, format='(I4)'
    saveimage, 'FIGURES/'+var_name+'_diff_2D_'+season+'_'+exp_name+'.gif'

  ENDIF


  IF var_name EQ 'T2M' THEN BEGIN

    path_obs = '/Users/gslod/WORK/DATA/TROPFLUX/'
    file_obs = 't2m_tropflux_1m_clim.nc'
    initncdf, path_obs + file_obs
    domdef, box

    var_obs = read_ncdf('t2m',0,11, /timestep, filename=path_obs+file_obs, /nostruct) & help, var_obs
    IF season EQ 'DJFM' THEN var_obs = (var_obs[*,*,11] + total(var_obs[*,*,0:2],3)) / 4
    IF season EQ 'JJAS' THEN var_obs = total(var_obs[*,*,5:8],3) / 4 & help, var_obs
    gphit_obs = gphit[firstxt:lastxt,firstyt:lastyt] & help, gphit_obs
    glamt_obs = glamt[firstxt:lastxt,firstyt:lastyt] & help, glamt_obs


    path_mod  = '/Volumes/TIME_MACHINE/EXP_'+exp_name+'/WRF/POST/MONTHLY/'
    file_mod = 'wrfout_monthly_clim_1990-2009.nc'
    maskfile = '/Users/gslod/WORK/IDL/MASK_WRF_GRID_T.nc'
    initncdf, maskfile
    domdef, box

    var_mod = read_ncdf('T2',0,11, /timestep, filename=path_mod+file_mod, /nostruct) -273.15 & help, var_mod
    IF season EQ 'DJFM' THEN var_mod = (var_mod[*,*,11] + total(var_mod[*,*,0:2],3)) / 4 & help, var_mod
    IF season EQ 'JJAS' THEN var_mod =  total(var_mod[*,*,5:8],3) / 4 & help, var_mod
    gphit_mod = gphit[firstxt:lastxt,firstyt:lastyt] & help, gphit_mod
    glamt_mod = glamt[firstxt:lastxt,firstyt:lastyt] & help, glamt_mod
    var_mod_gridobs = fromreg('bilinear', var_mod, glamt_mod, gphit_mod, glamt_obs, gphit_obs) & help, var_mod_gridobs


    initncdf, path_obs + file_obs
    domdef, box
    plt, var_mod_gridobs, 18, 30, /realcont, lct=34, title='', subtitle=' ', xtitle='', ytitle='', cb_charsize=1.2, format='(I2)'
    saveimage, 'FIGURES/'+var_name+'_mod_2D_'+season+'_'+exp_name+'.gif'
    plt, var_obs, 18, 30, /realcont, lct=34, title='', subtitle=' ', xtitle='', ytitle='', cb_charsize=1.2, format='(I2)'
    saveimage, 'FIGURES/'+var_name+'_obs_2D_'+season+'_'+exp_name+'.gif'
    plt, var_mod_gridobs - var_obs, -2, 2, /realcont, lct=77, title='',subtitle=' ', cb_charsize=1.2, format='(I3)'
    saveimage, 'FIGURES/'+var_name+'_diff_2D_'+season+'_'+exp_name+'.gif'

  ENDIF


  IF var_name EQ 'Q2M' THEN BEGIN

    path_obs = '/Users/gslod/WORK/DATA/TROPFLUX/'
    file_obs = 'q2m_tropflux_1m_clim.nc'
    initncdf, path_obs + file_obs
    domdef, box

    var_obs = read_ncdf('q2m',0,11, /timestep, filename=path_obs+file_obs, /nostruct) & help, var_obs
    IF season EQ 'DJFM' THEN var_obs = (var_obs[*,*,11] + total(var_obs[*,*,0:2],3)) / 4
    IF season EQ 'JJAS' THEN var_obs = total(var_obs[*,*,5:8],3) / 4 & help, var_obs
    gphit_obs = gphit[firstxt:lastxt,firstyt:lastyt] & help, gphit_obs
    glamt_obs = glamt[firstxt:lastxt,firstyt:lastyt] & help, glamt_obs


    path_mod  = '/Volumes/TIME_MACHINE/EXP_'+exp_name+'/WRF/POST/MONTHLY/'
    file_mod = 'wrfout_monthly_clim_1990-2009.nc'
    maskfile = '/Users/gslod/WORK/IDL/MASK_WRF_GRID_T.nc'
    initncdf, maskfile
    domdef, box

    var_mod = read_ncdf('Q2',0,11, /timestep, filename=path_mod+file_mod, /nostruct) * 1000. & help, var_mod
    IF season EQ 'DJFM' THEN var_mod = (var_mod[*,*,11] + total(var_mod[*,*,0:2],3)) / 4 & help, var_mod
    IF season EQ 'JJAS' THEN var_mod =  total(var_mod[*,*,5:8],3) / 4 & help, var_mod
    gphit_mod = gphit[firstxt:lastxt,firstyt:lastyt] & help, gphit_mod
    glamt_mod = glamt[firstxt:lastxt,firstyt:lastyt] & help, glamt_mod
    var_mod_gridobs = fromreg('bilinear', var_mod, glamt_mod, gphit_mod, glamt_obs, gphit_obs) & help, var_mod_gridobs


    initncdf, path_obs + file_obs
    domdef, box
    plt, var_mod_gridobs, 8, 22, /realcont, lct=34, title='', subtitle=' ', xtitle='', ytitle='', cb_charsize=1.2, format='(I2)'
    saveimage, 'FIGURES/'+var_name+'_mod_2D_'+season+'_'+exp_name+'.gif'
    plt, var_obs, 8, 22, /realcont, lct=34, title='', subtitle=' ', xtitle='', ytitle='', cb_charsize=1.2, format='(I2)'
    saveimage, 'FIGURES/'+var_name+'_obs_2D_'+season+'_'+exp_name+'.gif'
    plt, var_mod_gridobs - var_obs, -2, 2, /realcont, lct=77, title='',subtitle=' ', cb_charsize=1.2, format='(F4.1)'
    saveimage, 'FIGURES/'+var_name+'_diff_2D_'+season+'_'+exp_name+'.gif'

  ENDIF


  IF var_name EQ 'THF' THEN BEGIN

    path_obs = '/Users/gslod/WORK/DATA/TROPFLUX/'
    file_obs = 'shf_tropflux_1m_clim.nc'
    initncdf, path_obs + file_obs
    domdef, box
    var_obs = read_ncdf('shf',0,11, /timestep, filename=path_obs+file_obs, /nostruct) & help, var_obs
    IF season EQ 'DJFM' THEN var_obs = (var_obs[*,*,11] + total(var_obs[*,*,0:2],3)) / 4
    IF season EQ 'JJAS' THEN var_obs = total(var_obs[*,*,5:8],3) / 4 & help, var_obs
    shf_obs = var_obs

    path_obs = '/Users/gslod/WORK/DATA/TROPFLUX/'
    file_obs = 'lhf_tropflux_1m_clim.nc'
    var_obs = read_ncdf('lhf',0,11, /timestep, filename=path_obs+file_obs, /nostruct) & help, var_obs
    IF season EQ 'DJFM' THEN var_obs = (var_obs[*,*,11] + total(var_obs[*,*,0:2],3)) / 4
    IF season EQ 'JJAS' THEN var_obs = total(var_obs[*,*,5:8],3) / 4 & help, var_obs
    lhf_obs = var_obs
    var_obs = -1. * (shf_obs + lhf_obs)
    gphit_obs = gphit[firstxt:lastxt,firstyt:lastyt] & help, gphit_obs
    glamt_obs = glamt[firstxt:lastxt,firstyt:lastyt] & help, glamt_obs


    path_mod  = '/Volumes/TIME_MACHINE/EXP_'+exp_name+'/WRF/POST/MONTHLY/'
    file_mod = 'wrfout_monthly_clim_1990-2009.nc'
    maskfile = '/Users/gslod/WORK/IDL/MASK_WRF_GRID_T.nc'
    initncdf, maskfile
    domdef, box

    var_mod = read_ncdf('HFX',0,11, /timestep, filename=path_mod+file_mod, /nostruct) & help, var_mod
    IF season EQ 'DJFM' THEN var_mod = (var_mod[*,*,11] + total(var_mod[*,*,0:2],3)) / 4 & help, var_mod
    IF season EQ 'JJAS' THEN var_mod =  total(var_mod[*,*,5:8],3) / 4 & help, var_mod
    shf_mod = var_mod
    var_mod = read_ncdf('LH',0,11, /timestep, filename=path_mod+file_mod, /nostruct) & help, var_mod
    IF season EQ 'DJFM' THEN var_mod = (var_mod[*,*,11] + total(var_mod[*,*,0:2],3)) / 4 & help, var_mod
    IF season EQ 'JJAS' THEN var_mod =  total(var_mod[*,*,5:8],3) / 4 & help, var_mod
    lhf_mod = var_mod
    var_mod = shf_mod + lhf_mod
    gphit_mod = gphit[firstxt:lastxt,firstyt:lastyt] & help, gphit_mod
    glamt_mod = glamt[firstxt:lastxt,firstyt:lastyt] & help, glamt_mod
    var_mod_gridobs = fromreg('bilinear', var_mod, glamt_mod, gphit_mod, glamt_obs, gphit_obs) & help, var_mod_gridobs


    initncdf, path_obs + file_obs
    domdef, box
    plt, var_mod_gridobs, 0, 300, /realcont, lct=34, title='', subtitle=' ', xtitle='', ytitle='', cb_charsize=1.2, format='(I3)'
    saveimage, 'FIGURES/'+var_name+'_mod_2D_'+season+'_'+exp_name+'.gif'
    plt, var_obs, 0, 300, /realcont, lct=34, title='', subtitle=' ', xtitle='', ytitle='', cb_charsize=1.2, format='(I3)'
    saveimage, 'FIGURES/'+var_name+'_obs_2D_'+season+'_'+exp_name+'.gif'
    plt, var_mod_gridobs - var_obs, -100, 100, /realcont, lct=77, title='',subtitle=' ', cb_charsize=1.2, format='(I4)'
    saveimage, 'FIGURES/'+var_name+'_diff_2D_'+season+'_'+exp_name+'.gif'

  ENDIF




  IF var_name EQ 'OHC' THEN BEGIN

    cp = 4.1855 ; water heat capacity (J.g-1.degC-1)
    path_obs = '/Users/gslod/WORK/DATA/GLORYS/'
    file_obs = 'GLORYS2V1_IND4_monthly_clim_1992-2009_grid_T_NAN.nc'
    initncdf, path_obs + file_obs
    domdef, box

    tmp_obs = read_ncdf('votemper',0, 11, /timestep, filename=path_obs+file_obs, /nostruct)
    tmp_obs = (tmp_obs[*,*,*,11] + total(tmp_obs[*,*,*,0:3],4)) / 4
    file_obs = 'GLORYS2V1_IND4_monthly_clim_1992-2009_grid_S_NAN.nc'
    sal_obs  = read_ncdf('vosaline', 0, 11, /timestep, filename=path_obs+file_obs, /nostruct)
    sal_obs = (sal_obs[*,*,*,11] + total(sal_obs[*,*,*,0:3],4)) / 4
    mask = where(tmp_obs[*,*,0] LT 26.)
    bad  = where(tmp_obs LT 26.)
    miss = where(tmp_obs GT 50.)
    tmp_obs[bad] = !values.f_nan & sal_obs[bad] = !values.f_nan
    IF n_elements(miss) GT 1 THEN tmp_obs[miss] = !values.f_nan
    IF n_elements(miss) GT 1 THEN sal_obs[miss] = !values.f_nan
    rho_obs = rhon(temporary(sal_obs),tmp_obs) & help, rho_obs
    var_obs = fltarr(nxt, nyt) & var_obs[*,*] = !values.f_nan & help, var_obs
    var_obs = cp * moyenne(rho_obs*(tmp_obs-26.), 'z', /integration, /nan) / 10000.
    IF mask[0] NE -1 THEN var_obs[mask] = 0.
    gphit_obs = gphit[firstxt:lastxt,firstyt:lastyt] & help, gphit_obs
    glamt_obs = glamt[firstxt:lastxt,firstyt:lastyt] & help, glamt_obs
    mask_obs  = fltarr(size(glamt_obs, /dim))+1


    path_mod  = '/Volumes/TIME_MACHINE/EXP_'+exp_name+'/NEMO/POST/MONTHLY/'
    file_mod = 'nemo_out_monthly_clim_1990-2009.nc'
    initncdf, path_mod + file_mod
    domdef, box

    tmp_mod = read_ncdf('thetao',0, 11, /timestep, filename=path_mod+file_mod, /nostruct)
    tmp_mod[where(tmp_mod EQ 0)] = !values.f_nan
    tmp_mod = (tmp_mod[*,*,*,11] + total(tmp_mod[*,*,*,0:3],4)) / 4
    sal_mod  = read_ncdf('so', 0, 11, /timestep, filename=path_mod+file_mod, /nostruct)
    sal_mod[where(sal_mod EQ 0)] = !values.f_nan
    sal_mod = (sal_mod[*,*,*,11] + total(sal_mod[*,*,*,0:3],4)) / 4
    mask = where(tmp_mod[*,*,0] LT 26.)
    bad  = where(tmp_mod LT 26.)
    tmp_mod[bad] = !values.f_nan & sal_mod[bad] = !values.f_nan
    rho_mod = rhon(temporary(sal_mod),tmp_mod) & help, rho_mod
    var_mod = fltarr(nxt, nyt) & var_mod[*,*] = !values.f_nan & help, var_mod
    var_mod = cp * moyenne(rho_mod*(tmp_mod-26.), 'z', /integration, /nan) / 10000.
    IF mask[0] NE -1 THEN var_mod[mask] = 0.
    gphit_mod = gphit[firstxt:lastxt,firstyt:lastyt] & help, gphit_mod
    glamt_mod = glamt[firstxt:lastxt,firstyt:lastyt] & help, glamt_mod
    mask_mod  = fltarr(size(glamt_mod, /dim))+1
;    var_mod_gridobs = fromirr('bilinear', var_mod, glamt_mod, gphit_mod, mask_mod, glamt_obs, gphit_obs, mask_obs)
    var_mod_gridobs = var_mod

    initncdf, path_obs + file_obs
    domdef, box
    plt, var_mod_gridobs, 0, 500, lct=34, /realcont, title='', subtitle=' ', xtitle='', ytitle='', small=[3,4,(i*3)+1], /noerase, /rempli, cb_charsize=1.2, format='(I3)'
    plt, var_obs, 0, 500, lct=34, /realcont, title='', subtitle=' ', xtitle='', ytitle='', small=[3,4,(i*3)+2], /noerase, /rempli, cb_charsize=1.2, format='(I3)'
    plt, var_mod_gridobs - var_obs, -200, 200, /realcont, lct=77, title='',subtitle=' ', xtitle='', ytitle='', small=[3,4,(i*3)+3], /noerase, /rempli, cb_charsize=1.2, format='(I4)'

  ENDIF


  IF var_name EQ 'OLR' THEN BEGIN
  
    path_obs = '/Users/gslod/WORK/DATA/OLR-NOAA/'
    file_obs = 'olr.mon.ltm.nc'
    initncdf, path_obs + file_obs
    domdef, box

    var_obs   = read_ncdf('olr',0,11, /timestep, filename=path_obs+file_obs, /nostruct) & help, var_obs
    IF season EQ 'DJFM' THEN var_obs = (var_obs[*,*,11] + total(var_obs[*,*,0:2],3)) / 4
    IF season EQ 'JJAS' THEN var_obs = total(var_obs[*,*,5:8],3) / 4 & help, var_obs
    gphit_obs = gphit[firstxt:lastxt,firstyt:lastyt] & help, gphit_obs
    glamt_obs = glamt[firstxt:lastxt,firstyt:lastyt] & help, glamt_obs

    path_mod  = '/Volumes/TIME_MACHINE/EXP_'+exp_name+'/WRF/POST/MONTHLY/'
    file_mod = 'wrfout_monthly_clim_1990-2009.nc'
    maskfile = '/Users/gslod/WORK/IDL/MASK_WRF_GRID_T.nc'
    initncdf, maskfile
    domdef, box

    var_mod = read_ncdf('OLR',0,11, /timestep, filename=path_mod+file_mod, /nostruct) & help, var_mod
    IF season EQ 'DJFM' THEN var_mod = (var_mod[*,*,11] + total(var_mod[*,*,0:2],3)) / 4 & help, var_mod
    IF season EQ 'JJAS' THEN var_mod =  total(var_mod[*,*,5:8],3) / 4 & help, var_mod
    gphit_mod = gphit[firstxt:lastxt,firstyt:lastyt] & help, gphit_mod
    glamt_mod = glamt[firstxt:lastxt,firstyt:lastyt] & help, glamt_mod
    var_mod_gridobs = fromreg('bilinear', var_mod, glamt_mod, gphit_mod, glamt_obs, gphit_obs)
 

    initncdf, path_obs + file_obs
    domdef, box
    plt, var_mod_gridobs, 150, 300, /realcont, lct=34, title='', subtitle=' ', xtitle='', ytitle='', cb_charsize=1.2, format='(I3)'
    saveimage, 'FIGURES/'+var_name+'_mod_2D_'+season+'_'+exp_name+'.gif'
    plt, var_obs, 150, 300, /realcont, lct=34, title='', subtitle=' ', xtitle='', ytitle='', cb_charsize=1.2, format='(I3)'
    saveimage, 'FIGURES/'+var_name+'_obs_2D_'+season+'_'+exp_name+'.gif'
    plt, var_mod_gridobs - var_obs, -50, 50, /realcont, lct=77, title='',subtitle=' ', xtitle='', ytitle='', cb_charsize=1.2, format='(I3)'
    saveimage, 'FIGURES/'+var_name+'_diff_2D_'+season+'_'+exp_name+'.gif'

  ENDIF


  IF var_name EQ 'SHEAR' THEN BEGIN
  
    path_obs = '/Users/gslod/WORK/DATA/INDICES/ERAI-MONTHLY/'
    file_obs = 'uv_vor850_monthly_clim_1990-2009.nc'
    initncdf, path_obs + file_obs
    domdef, box

    var_obs  = read_ncdf('u',0,11, /timestep, filename=path_obs+file_obs, /nostruct) & help, var_obs
    var_obs = reform(var_obs) & help, var_obs
    IF season EQ 'DJFM' THEN var_obs = (var_obs[*,*,11] + total(var_obs[*,*,0:2],3)) / 4
    IF season EQ 'JJAS' THEN var_obs = total(var_obs[*,*,5:8],3) / 4 & help, var_obs
    u850_obs = var_obs
    var_obs  = read_ncdf('v',0,11, /timestep, filename=path_obs+file_obs, /nostruct) & help, var_obs
    var_obs = reform(var_obs) & help, var_obs    
    IF season EQ 'DJFM' THEN var_obs = (var_obs[*,*,11] + total(var_obs[*,*,0:2],3)) / 4
    IF season EQ 'JJAS' THEN var_obs = total(var_obs[*,*,5:8],3) / 4 & help, var_obs
    v850_obs = var_obs
    file_obs = 'uv_vor200_monthly_clim_1990-2009.nc'
    var_obs  = read_ncdf('u',0,11, /timestep, filename=path_obs+file_obs, /nostruct) & help, var_obs
    var_obs = reform(var_obs) & help, var_obs
    IF season EQ 'DJFM' THEN var_obs = (var_obs[*,*,11] + total(var_obs[*,*,0:2],3)) / 4
    IF season EQ 'JJAS' THEN var_obs = total(var_obs[*,*,5:8],3) / 4 & help, var_obs
    u200_obs = var_obs
    var_obs  = read_ncdf('v',0,11, /timestep, filename=path_obs+file_obs, /nostruct) & help, var_obs
    var_obs = reform(var_obs) & help, var_obs    
    IF season EQ 'DJFM' THEN var_obs = (var_obs[*,*,11] + total(var_obs[*,*,0:2],3)) / 4
    IF season EQ 'JJAS' THEN var_obs = total(var_obs[*,*,5:8],3) / 4 & help, var_obs
    v200_obs = var_obs
    var_obs  = sqrt((u200_obs-u850_obs)^2+(v200_obs-v850_obs)^2)   
    gphit_obs = gphit[firstxt:lastxt,firstyt:lastyt] & help, gphit_obs
    glamt_obs = glamt[firstxt:lastxt,firstyt:lastyt] & help, glamt_obs


    path_mod = '/Volumes/TIME_MACHINE/EXP_'+exp_name+'/WRF/INDICES/MONTHLY/'
    file_mod = 'wrf_indices_monthly_clim_1990-2009.nc'
    maskfile = '/Users/gslod/WORK/IDL/MASK_WRF_GRID_T.nc'
    initncdf, maskfile
    domdef, box

    var_mod  = read_ncdf('u850',0,11, /timestep, filename=path_mod+file_mod, /nostruct) & help, var_mod
    IF season EQ 'DJFM' THEN var_mod = (var_mod[*,*,11] + total(var_mod[*,*,0:2],3)) / 4 & help, var_mod
    IF season EQ 'JJAS' THEN var_mod =  total(var_mod[*,*,5:8],3) / 4 & help, var_mod
    u850_mod = var_mod
    var_mod  = read_ncdf('v850',0,11, /timestep, filename=path_mod+file_mod, /nostruct) & help, var_mod
    IF season EQ 'DJFM' THEN var_mod = (var_mod[*,*,11] + total(var_mod[*,*,0:2],3)) / 4 & help, var_mod
    IF season EQ 'JJAS' THEN var_mod =  total(var_mod[*,*,5:8],3) / 4 & help, var_mod
    v850_mod = var_mod
    var_mod  = read_ncdf('u200',0,11, /timestep, filename=path_mod+file_mod, /nostruct) & help, var_mod
    IF season EQ 'DJFM' THEN var_mod = (var_mod[*,*,11] + total(var_mod[*,*,0:2],3)) / 4 & help, var_mod
    IF season EQ 'JJAS' THEN var_mod =  total(var_mod[*,*,5:8],3) / 4 & help, var_mod
    u200_mod = var_mod
    var_mod  = read_ncdf('v200',0,11, /timestep, filename=path_mod+file_mod, /nostruct) & help, var_mod
    IF season EQ 'DJFM' THEN var_mod = (var_mod[*,*,11] + total(var_mod[*,*,0:2],3)) / 4 & help, var_mod
    IF season EQ 'JJAS' THEN var_mod =  total(var_mod[*,*,5:8],3) / 4 & help, var_mod
    v200_mod = var_mod  
    var_mod  = sqrt((u200_mod-u850_mod)^2+(v200_mod-v850_mod)^2)
    gphit_mod = gphit[firstxt:lastxt,firstyt:lastyt] & help, gphit_mod
    glamt_mod = glamt[firstxt:lastxt,firstyt:lastyt] & help, glamt_mod
    var_mod_gridobs = fromreg('bilinear', var_mod, glamt_mod, gphit_mod, glamt_obs, gphit_obs)
 

    initncdf, path_obs + file_obs
    domdef, box
    plt, var_mod_gridobs, 0, 50, /realcont, lct=34, title='', subtitle=' ', xtitle='', ytitle='', cb_charsize=1.2, format='(I2)'
    saveimage, 'FIGURES/'+var_name+'_mod_2D_'+season+'_'+exp_name+'.gif'
    plt, var_obs, 0, 50, /realcont, lct=34, title='', subtitle=' ', xtitle='', ytitle='', cb_charsize=1.2, format='(I2)'
    saveimage, 'FIGURES/'+var_name+'_obs_2D_'+season+'_'+exp_name+'.gif'
    plt, var_mod_gridobs - var_obs, -20, 20, /realcont, lct=77, title='',subtitle=' ', xtitle='', ytitle='', cb_charsize=1.2, format='(I3)'
    saveimage, 'FIGURES/'+var_name+'_diff_2D_'+season+'_'+exp_name+'.gif'
  ENDIF

ENDFOR
ENDFOR

stop

END
