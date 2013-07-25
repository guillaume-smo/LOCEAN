PRO validate_NEMO_GLORYS_3M
  @all_cm
 

  run_nm='COUPLED_SW2_KF'
  obs_nm='GLORYS'
 
  month_list = ['DJF','MAM','JJA','SON']
  boxglorys = [30,130,-30,25,0,60]

  ;water heat capacity (J.g-1.degC-1)
  cp = 4.1855

  jOHC = 1
  

  FOR m = 0, n_elements(month_list)-1 DO BEGIN

    month = month_list[m]


    IF jOHC THEN BEGIN

      ;;;;;;;;;;;;;;;;;;;;;; OBS ;;;;;;;;;;;;;;;;;;;;;;;;
      ;-------------------------------------------------;
      pathin = '/Volumes/TIME_MACHINE/DATA/GLORYS/'
      filein = 'GLORYS2V1_IND4_monthly_clim_1992-2009_grid_T_NAN.nc'
      initncdf, pathin+filein, /fullcgrid
      domdef, boxglorys

      vari=read_ncdf('votemper',0,11,/timestep, filename=pathin+filein, /nostruct)
      IF month EQ 'DJF' THEN vari = (vari[*,*,*,11] + total(vari[*,*,*,0:1],4)) / 3.
      IF month EQ 'MAM' THEN vari = total(vari[*,*,*,2:4],4) / 3.
      IF month EQ 'JJA' THEN vari = total(vari[*,*,*,5:7],4) / 3.
      IF month EQ 'SON' THEN vari = total(vari[*,*,*,8:10],4) / 3.
      temp_o = vari & help, temp_o & vari = 0

      filein = 'GLORYS2V1_IND4_monthly_clim_1992-2009_grid_S_NAN.nc'
      vari=read_ncdf('vosaline',0,11,/timestep, filename=pathin+filein, /nostruct)
      IF month EQ 'DJF' THEN vari = (vari[*,*,*,11] + total(vari[*,*,*,0:1],4)) / 3.
      IF month EQ 'MAM' THEN vari = total(vari[*,*,*,2:4],4) / 3.
      IF month EQ 'JJA' THEN vari = total(vari[*,*,*,5:7],4) / 3.
      IF month EQ 'SON' THEN vari = total(vari[*,*,*,8:10],4) / 3.
      sal_o = vari & help, sal_o & vari = 0

      mask = where(temp_o[*,*,0] LT 26.)
      bad = where(temp_o LT 26.)
      miss = where(temp_o GT 50.)
      temp_o[bad] = !values.f_nan & sal_o[bad] = !values.f_nan
      IF n_elements(miss) GT 1 THEN temp_o[miss] = !values.f_nan
      IF n_elements(miss) GT 1 THEN sal_o[miss] = !values.f_nan


      rho_o = rhon(temporary(sal_o),temp_o) & help, rho_o

      ohc26_o = fltarr(nxt, nyt) & ohc26_o[*,*] = !values.f_nan & help, ohc26_o
      ohc26_o = cp * moyenne(rho_o*(temp_o-26.), 'z', /integration, /nan) / 10000.
      ohc26_o[mask] = 0.

      ; sauve grille + masque pour interpolation
      gphit_o = gphit[firstxt:lastxt,firstyt:lastyt] & help, gphit_o
      glamt_o = glamt[firstxt:lastxt,firstyt:lastyt] & help, glamt_o
      mask_o  = fltarr(size(glamt_o,/dim))+1 & mask_o[where(finite(ohc26_o) EQ 0)]=0



      ;;;;;;;;;;;;;;;;;;;;;;  MODELE ;;;;;;;;;;;;;;;;;;;;
      ;-------------------------------------------------;

      pathin = '/Volumes/TIME_MACHINE/EXP_'+run_nm+'/NEMO/POST/MONTHLY/DATA/'
      filein = 'nemo_out_monthly_clim_1990-2009.nc'
      initncdf, pathin+filein, /fullCgrid
      boxnemo = [30,130,-30,25,0,60]
      domdef, boxnemo

      vari=read_ncdf('thetao',0,11,/timestep, filename=pathin+filein, /nostruct)
      vari[where(vari EQ 0)] = !values.f_nan
      IF month EQ 'DJF' THEN vari = (vari[*,*,*,11] + total(vari[*,*,*,0:1],4)) / 3.
      IF month EQ 'MAM' THEN vari = total(vari[*,*,*,2:4],4) / 3.
      IF month EQ 'JJA' THEN vari = total(vari[*,*,*,5:7],4) / 3.
      IF month EQ 'SON' THEN vari = total(vari[*,*,*,8:10],4) / 3.
      temp_w = vari & help, temp_w & vari = 0

      vari=read_ncdf('so',0,11,/timestep, filename=pathin+filein, /nostruct)
      vari[where(vari EQ 0)] = !values.f_nan      
      IF month EQ 'DJF' THEN vari = (vari[*,*,*,11] + total(vari[*,*,*,0:1],4)) / 3.
      IF month EQ 'MAM' THEN vari = total(vari[*,*,*,2:4],4) / 3.
      IF month EQ 'JJA' THEN vari = total(vari[*,*,*,5:7],4) / 3.
      IF month EQ 'SON' THEN vari = total(vari[*,*,*,8:10],4) / 3.
      sal_w = vari & help, sal_w & vari = 0

      mask = where(temp_w[*,*,0] LT 26.)
      bad = where(temp_w LT 26.)      
      temp_w[bad] = !values.f_nan & sal_w[bad] = !values.f_nan

      rho_w = rhon(temporary(sal_w),temp_w) & help, rho_w

      ohc26_w = fltarr(nxt, nyt) & ohc26_w[*,*] = !values.f_nan & help, ohc26_w
      ohc26_w = cp * moyenne(rho_w*(temp_w-26.), 'z', /integration, /nan) / 10000.
      ohc26_w[mask] = 0.

      ; sauv grille + masque pour interp sorties modele sur la grille des obs
      gphit_w = gphit[firstxt:lastxt,firstyt:lastyt] & help, gphit_w
      glamt_w = glamt[firstxt:lastxt,firstyt:lastyt] & help, glamt_w
      mask_w  = fltarr(size(glamt_w,/dim))+1 & mask_w[where(finite(ohc26_w) EQ 0)]=0 

      ; interp sorties modele sur la grille des obs
;      ohc26_w_grido = fromirr('bilinear', ohc_w, glamt_w, gphit_w, mask_w, glamt_o, gphit_o, mask_o)

      ; plot
      pathin = '/Volumes/TIME_MACHINE/DATA/GLORYS/'
      filein = 'GLORYS2V1_IND4_monthly_clim_1992-2009_grid_T_NAN.nc'
      initncdf, pathin+filein, /fullcgrid
      domdef, boxglorys

      title = 'ohc26_clim_'+ month +'_'+ obs_nm+'_'+ run_nm
      openps, filename=title, /portrait, /keep_pfont
      plt,ohc26_o,0,200,/nocont,realcont=2,lct=60,marge=[0,0,0,0],small=[1,3,1],title=obs_nm,/carte,/rempli
      plt,ohc26_w,0,200,/nocont,realcont=2,lct=60,marge=[0,0,0,0],small=[1,3,2],/noer,title=run_nm,/carte,/rempli
      plt,ohc26_w-ohc26_o,-50,50,/nocont,realcont=2,lct=77,marge=[0,0,0,0],small=[1,3,3],/noer,title=run_nm+'-'+obs_nm,/carte,/rempli
      closeps

    ENDIF

  ENDFOR

END
