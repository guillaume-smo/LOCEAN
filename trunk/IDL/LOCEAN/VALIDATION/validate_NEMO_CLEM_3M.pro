PRO validate_NEMO_CLEM_3M
  @all_cm
 

  run_nm='COUPLED_SW2_KF'
  obs_nm='CLEM'
 
  month_list = ['DJF','MAM','JJA','SON']
  box=[30,130,-30,25]

  jMLD  = 1
  
  datedeb=19900101
  datefin=20100101
  datedeb_f=strtrim(string(datedeb),2)
  datefin_f=strtrim(string(datefin),2)
  yeardeb_f=strmid(string(datedeb),4,4)
  yearfin_f=strmid(string(datefin),4,4)
 

  FOR m = 0, n_elements(month_list)-1 DO BEGIN

    month = month_list[m]

    ;;;;;;;;;;;;;;;;;;;;;;  ;;;;;;;;;;;;;;;;;;;;;;  ;;;;;;;;;;;;;;;;;;;;;;  ;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;  MLD ;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;  ;;;;;;;;;;;;;;;;;;;;;;  ;;;;;;;;;;;;;;;;;;;;;;  ;;;;;;;;;;;;;;;;;;;;;;    
    IF jMLD THEN BEGIN
      ;;;;;;;;;;;;;;;;;;;;;;  BUILD MDL ;;;;;;;;;;;;;;;;;   OBS WOA
      ;-------------------------------------------------;
      pathout  = '/Volumes/TIME_MACHINE/DATA/MLD-CLEM/DATA/'
      pathfile = 'mld_DT02.nc'
      initncdf,pathout+pathfile,/fullCgrid,glam=[0,360]
      domdef,box

      vari=read_ncdf('mld',0,11,/timestep, filename=pathout+pathfile, /nostruct) & help, vari
      mask_o = read_ncdf('mask', filename=pathout+pathfile, /nostruct) & help, mask_o
      IF month EQ 'DJF' THEN vari = (vari[*,*,11] + total(vari[*,*,0:1],3)) / 3.
      IF month EQ 'MAM' THEN vari = total(vari[*,*,2:4],3) / 3.
      IF month EQ 'JJA' THEN vari = total(vari[*,*,5:7],3) / 3.
      IF month EQ 'SON' THEN vari = total(vari[*,*,8:10],3) / 3.
      mldt_o = vari[*,*] & mldt_o[where(mask_o EQ 0)]=!values.f_nan  & vari = 0
      
      pathout  = '/Volumes/TIME_MACHINE/DATA/MLD-CLEM/DATA/'
      pathfile = 'mld_DR003.nc'
      initncdf,pathout+pathfile,/fullCgrid,glam=[0,360]
      domdef,box

      vari=read_ncdf('mld',0,11,/timestep, filename=pathout+pathfile, /nostruct) & help, vari
      IF month EQ 'DJF' THEN vari = (vari[*,*,11] + total(vari[*,*,0:1],3)) / 3.
      IF month EQ 'MAM' THEN vari = total(vari[*,*,2:4],3) / 3.
      IF month EQ 'JJA' THEN vari = total(vari[*,*,5:7],3) / 3.
      IF month EQ 'SON' THEN vari = total(vari[*,*,8:10],3) / 3.
      mldr_o = vari[*,*] & mldr_o[where(mask_o EQ 0)]=!values.f_nan & vari = 0

      ; sauve grille pour interpolation
      gphit_o =  gphit[firstxt:lastxt,firstyt:lastyt]
      glamt_o =  glamt[firstxt:lastxt,firstyt:lastyt]
;      mask_o  = fltarr(size(glamt_o,/dim))+1 & mask_o[where(mldt_o EQ 1.e+09)]=0 & mask_o[where(mldt_o EQ 1.e+09)]=0
      
      
      
      ;;;;;;;;;;;;;;;;;;;;;;  BUILD MLD ;;;;;;;;;;;;;;;;;;;;;;   MODELE
      ;-------------------------------------------------;
      pathout  = '/Volumes/TIME_MACHINE/EXP_'+run_nm+'/NEMO/POST/MONTHLY/DATA/'
      pathfile = 'nemo_out_monthly_clim_1990-2008.nc'
      initncdf,pathout+pathfile,/fullCgrid
      domdef,box

      vari=read_ncdf('thetao',0,11,/timestep, filename=pathout+pathfile, /nostruct)
      vari[where(vari EQ 0)]=!values.f_nan & help, vari    
      IF month EQ 'DJF' THEN vari = (vari[*,*,*,11] + total(vari[*,*,*,0:1],4)) / 3.
      IF month EQ 'MAM' THEN vari = total(vari[*,*,*,2:4],4) / 3.
      IF month EQ 'JJA' THEN vari = total(vari[*,*,*,5:7],4) / 3.
      IF month EQ 'SON' THEN vari = total(vari[*,*,*,8:10],4) / 3.
      temp_w = vari[*,*,*] & vari = 0
      
      vari=read_ncdf('so',0,11,/timestep, filename=pathout+pathfile, /nostruct) 
      vari[where(vari EQ 0)]=!values.f_nan & help, vari    
      IF month EQ 'DJF' THEN vari = (vari[*,*,*,11] + total(vari[*,*,*,0:1],4)) / 3.
      IF month EQ 'MAM' THEN vari = total(vari[*,*,*,2:4],4) / 3.
      IF month EQ 'JJA' THEN vari = total(vari[*,*,*,5:7],4) / 3.
      IF month EQ 'SON' THEN vari = total(vari[*,*,*,8:10],4) / 3.
      sal_w = vari[*,*,*] & vari = 0

      rho_w=rhon(sal_w,temp_w) & help, rho_w

      mldt_w=fltarr(nxt,nyt) & mldt_w[*,*]=!values.f_nan
      mldr_w=fltarr(nxt,nyt) & mldr_w[*,*]=!values.f_nan

      FOR iy=0,nyt-1 DO BEGIN
        FOR ix=0,nxt-1 DO BEGIN

        IF finite(temp_w[ix,iy,0]) EQ 1 THEN BEGIN 
          profil_T = reform(temp_w[ix,iy,*])
	  temp_crit = profil_T[1] - 0.2
	  diff_T = profil_T - temp_crit[0]
	  igmldt = where(diff_T GE 0.)
          ilmldt = where(diff_T LT 0.)

          IF igmldt[0] NE -1 AND ilmldt[0] NE -1 THEN BEGIN
            mldt = interpol(gdept,profil_T,temp_crit[0])
            mldt_w[ix,iy] = mldt
            IF mldt LT 0 THEN stop
          ENDIF

          profil_rho = reform(rho_w[ix,iy,*])
	  rho_crit = profil_rho[1] + 0.03
          igmldr = where(profil_rho[1:n_elements(profil_rho)-1] GE rho_crit[0])
          ilmldr = where(profil_rho[1:n_elements(profil_rho)-1] LT rho_crit[0])

          IF igmldr[0] NE -1 AND ilmldr[0] NE -1 THEN BEGIN
            mldr = interpol(gdept[igmldr[0]-1:igmldr[0]+1],profil_rho[igmldr[0]-1:igmldr[0]+1],rho_crit[0])
            mldr_w[ix,iy] = mldr
            IF mldr LT 0 THEN stop
          ENDIF

        ENDIF  
        ENDFOR
      ENDFOR

      ; sauv grilles pour interp sorties modele sur la grille des obs
      gphit_w =  gphit[firstxt:lastxt,firstyt:lastyt]
      glamt_w =  glamt[firstxt:lastxt,firstyt:lastyt]
      mask_w  = fltarr(size(glamt_w,/dim))+1 & mask_w[where(finite(mldt_w) EQ 0)]=0
      
      ; interp sorties modele sur la grille des obs
      mldt_w_grido = fromirr('bilinear', mldt_w, glamt_w, gphit_w, mask_w, glamt_o, gphit_o, mask_o)
      mldr_w_grido = fromirr('bilinear', mldr_w, glamt_w, gphit_w, mask_w, glamt_o, gphit_o, mask_o)

      ; plot
      pathout  = '/Volumes/TIME_MACHINE/DATA/MLD-CLEM/DATA/'
      pathfile = 'mld_DT02.nc'
      initncdf,pathout+pathfile,/fullCgrid,glam=[0,360]
      domdef,box

      title = 'mldt_clim_'+ month +'_'+ obs_nm+'_'+ run_nm
      openps, filename=title, /portrait, /keep_pfont
      plt,mldt_o,0,130,/nocont,realcont=2,lct=5,marge=[0,0,0,0],small=[1,3,1],title=obs_nm,/carte,/rempli
      plt,mldt_w_grido,0,130,/nocont,realcont=2,lct=5,marge=[0,0,0,0],small=[1,3,2],/noer,title=run_nm,/carte,/rempli
      plt,mldt_w_grido-mldt_o,-50,50,/nocont,realcont=2,lct=77,marge=[0,0,0,0],small=[1,3,3],/noer,title=run_nm+'-'+obs_nm,/carte,/rempli
      closeps

      title = 'mldr_clim_'+ month +'_'+ obs_nm+'_'+ run_nm
      openps, filename=title, /portrait, /keep_pfont
      plt,mldr_o,0,130,/nocont,realcont=2,lct=5,marge=[0,0,0,0],small=[1,3,1],title=obs_nm,/carte,/rempli
      plt,mldr_w_grido,0,130,/nocont,realcont=2,lct=5,marge=[0,0,0,0],small=[1,3,2],/noer,title=run_nm,/carte,/rempli
      plt,mldr_w_grido-mldr_o,-50,50,/nocont,realcont=2,lct=77,marge=[0,0,0,0],small=[1,3,3],/noer,title=run_nm+'-'+obs_nm,/carte,/rempli
      closeps

    ENDIF

  ENDFOR

END
