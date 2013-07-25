PRO validate_NEMO_WOA_3M
  @all_cm
 

  run_nm='COUPLED_SW2_BMJ'
  obs_nm='WOA'
 
  month_list = ['DJF','MAM','JJA','SON']
  box=[30,130,-30,25]

  jhTm2 = 0
  jCI   = 1
  jD26  = 0
  jMLD  = 0
  jSST  = 0
  
  datedeb=19900101
  datefin=20100101
  datedeb_f=strtrim(string(datedeb),2)
  datefin_f=strtrim(string(datefin),2)
  yeardeb_f=strmid(string(datedeb),4,4)
  yearfin_f=strmid(string(datefin),4,4)
 

  FOR m = 0, n_elements(month_list)-1 DO BEGIN

    month = month_list[m]

    IF jhTm2 THEN BEGIN
      ;;;;;;;;;;;;;;;;;;;;;;  BUILD depth of T10_m2 ;;;;;;;;;;;;;;;;;;;;;;   OBS WOA
      ;-------------------------------------------------;
      pathout  = '/Volumes/TIME_MACHINE/DATA/WOA_2009/DATA/'
      pathfile = 'woa09_T_monthly_world.nc'
      initncdf,pathout+pathfile,/fullCgrid,glam=[0,360]
      domdef,box
      vari=read_ncdf('temp', 0, 11, /timestep, filename=pathout+pathfile, /nostruct)
      
      vari_JFM = ( total(vari[*,*,*,0:2],4) )/3.
      vari_JAS = ( total(vari[*,*,*,6:8],4) )/3.

      temp_o=fltarr(nxt,nyt,nzt)
      iSH = where(gphit[0,firstyt:lastyt] LT 0)
      temp_o[*,iSH,*]=vari_JFM[*,iSH,*]
      iNH = where(gphit[0,firstyt:lastyt] GT 0)
      temp_o[*,iNH,*]=vari_JAS[*,iNH,*]
      ;  iEQ = where(gphit[0,*] eq 0)
      ;  vari_o[*,iEQ,*]=!values.f_nan
      
      vari=0
      
      hT10m2_o=fltarr(nxt,nyt) & hT10m2_o[*,*]=!values.f_nan & help, hT10m2_o
      
      FOR iy=0,nyt-1 DO BEGIN
        FOR ix=0,nxt-1 DO BEGIN
          profil_T = reform(temp_o[ix,iy,*])
          
          T10 = profil_T[1]
          T10m2 = T10 -2.
          
          
          ig2 = where(profil_T GE T10m2)
          il2 = where(profil_T LT T10m2)
          IF ig2[0] NE -1 AND il2[0] NE -1 THEN BEGIN
            d2=interpol(gdept,profil_T,T10m2)
            hT10m2_o[ix,iy] = d2
          ;  IF d2 LT 0 THEN stop
          ENDIF
          
          
        ENDFOR
      ENDFOR
      plt,hT10m2_o,0,150,/nocontour,lct=60,win=2,title='',/carte
      
      saveimage,'hT10m2_clim_JFM_SH_JAS_NH_'+obs_nm+'.gif'
      
      ; sauve grille pour interpolation
      gphit_o =  gphit[firstxt:lastxt,firstyt:lastyt]
      glamt_o =  glamt[firstxt:lastxt,firstyt:lastyt]
      

      
      ;;;;;;;;;;;;;;;;;;;;;;  BUILD hT10m2 ;;;;;;;;;;;;;;;;;;;;;;   MODELE
      ;-------------------------------------------------;
      pathout  = '/Volumes/TIME_MACHINE/EXP_'+run_nm+'/NEMO/POST/MONTHLY/DATA/'
      pathfile = 'nemo_out_monthly_clim_1990-2008.nc'
      initncdf,pathout+pathfile,/fullCgrid
      domdef,box

      vari=read_ncdf('thetao',0,11,/timestep, filename=pathout+pathfile, /nostruct)
      vari[where(vari EQ 0)]=!values.f_nan
      
      vari_JFM = ( total(vari[*,*,*,0:2],4) )/3.
      vari_JAS = ( total(vari[*,*,*,6:8],4) )/3.
      
      temp_w=fltarr(nxt,nyt,nzt)
      iSH = where(gphit[0,firstyt:lastyt] LT 0)
      temp_w[*,iSH,*]=vari_JFM[*,iSH,*]
      iNH = where(gphit[0,firstyt:lastyt] GT 0)
      temp_w[*,iNH,*]=vari_JAS[*,iNH,*]
      iEQ = where(gphit[0,firstyt:lastyt] EQ 0)
      temp_w[*,iEQ,*]=!values.f_nan
      
      vari=0
      
      hT10m2_w=fltarr(nxt,nyt) & hT10m2_w[*,*]=!values.f_nan & help, hT10m2_w
      
      FOR iy=0,nyt-1 DO BEGIN
        FOR ix=0,nxt-1 DO BEGIN
          profil_T = reform(temp_w[ix,iy,*])
          
          T10 = profil_T[1]
          T10m2 = T10 -2.
          
          
          ig2 = where(profil_T GE T10m2)
          il2 = where(profil_T LT T10m2)
          IF ig2[0] NE -1 AND il2[0] NE -1 THEN BEGIN
            d2=interpol(gdept,profil_T,T10m2)
            hT10m2_w[ix,iy] = d2
          ;  IF d2 LT 0 THEN stop
          ENDIF
          
          
          
        ENDFOR
      ENDFOR
      
      
      plt,hT10m2_w,0,150,/nocontour,lct=60,win=3,title='',/carte,/cell_fill
      saveimage,'hT10m2_clim_JFM_SH_JAS_NH_'+run_nm+'.gif'
      
      gphit_w =  gphit[firstxt:lastxt,firstyt:lastyt]
      glamt_w =  glamt[firstxt:lastxt,firstyt:lastyt]
      
      
      
      
      
      ; sauv grilles pour interp sorties modele sur la grille des obs
      mask_o = fltarr(size(glamt_o,/dim))+1 & mask_o[where(finite(hT10m2_o) EQ 0)]=0 ;& mask_o[where(gphit_o EQ 0)]=0
      mask_w = fltarr(size(glamt_w,/dim))+1 & mask_w[where(finite(hT10m2_w) EQ 0)]=0 & mask_w[where(gphit_w EQ 0)]=0
      
      
      ; interp sorties modele sur la grille des obs
      help, hT10m2_w, glamt_w, gphit_w, mask_w, glamt_o, gphit_o, mask_o
      hT10m2_w_grido = fromirr('bilinear', hT10m2_w, glamt_w, gphit_w, mask_w, glamt_o, gphit_o, mask_o)
      
      pathout  = '/Volumes/TIME_MACHINE/DATA/WOA_2009/DATA/'
      pathfile = 'woa09_T_monthly_world.nc'
      initncdf,pathout+pathfile,/fullCgrid,glam=[0,360]
      domdef,box
      
      ;hT10m2_w_grido[where(hT10m2_w_grido EQ 0)]=!values.f_nan
      plt,hT10m2_w_grido,0,150,/nocontour,lct=60,win=4,title='',/carte,/cell_fill
      saveimage,'hT10m2_clim_JFM_SH_JAS_NH_'+run_nm+'_grid_obs.gif'
      
      plt,hT10m2_w_grido-hT10m2_o,-50,50,lct=77,/carte,box=box,/nocontour,win=6
      saveimage,'hT10m2_clim_JFM_SH_JAS_NH_'+run_nm+'-'+obs_nm+'.gif'
      
    ENDIF
    
    
    
    
    ;;;;;;;;;;;;;;;;;;;;;;  ;;;;;;;;;;;;;;;;;;;;;;  ;;;;;;;;;;;;;;;;;;;;;;  ;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;  CI ;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;  ;;;;;;;;;;;;;;;;;;;;;;  ;;;;;;;;;;;;;;;;;;;;;;  ;;;;;;;;;;;;;;;;;;;;;;
    
    IF jCI THEN BEGIN

      delta_SST = 2

      ;;;;;;;;;;;;;;;;;;;;;;  BUILD CI ;;;;;;;;;;;;;;;;;;;;;;   OBS WOA
      ;-------------------------------------------------;
      pathout  = '/Volumes/TIME_MACHINE/DATA/WOA_2009/DATA/'
      pathfile = 'woa09_T_monthly_world.nc'
      initncdf,pathout+pathfile,/fullCgrid,glam=[0,360]
      domdef,box
      vari=read_ncdf('temp',0,11,/timestep, filename=pathout+pathfile, /nostruct) & help, vari
      IF month EQ 'DJF' THEN vari = (vari[*,*,*,11] + total(vari[*,*,*,0:1],4)) / 3.
      IF month EQ 'MAM' THEN vari = total(vari[*,*,*,2:4],4) / 3.
      IF month EQ 'JJA' THEN vari = total(vari[*,*,*,5:7],4) / 3.
      IF month EQ 'SON' THEN vari = total(vari[*,*,*,8:10],4) / 3.
      temp_o = vari[*,*,*] & vari = 0
      
      pathfile = 'woa09_S_monthly_world.nc'
      vari=read_ncdf('s',0,11,/timestep, filename=pathout+pathfile, /nostruct) & help, vari
      IF month EQ 'DJF' THEN vari = (vari[*,*,*,11] + total(vari[*,*,*,0:1],4)) / 3.
      IF month EQ 'MAM' THEN vari = total(vari[*,*,*,2:4],4) / 3.
      IF month EQ 'JJA' THEN vari = total(vari[*,*,*,5:7],4) / 3.
      IF month EQ 'SON' THEN vari = total(vari[*,*,*,8:10],4) / 3.
      sal_o = vari[*,*,*] & vari = 0

      rho_o=rhon(sal_o,temp_o) & help, rho_o      
      CI_o=fltarr(nxt,nyt) & CI_o[*,*]=!values.f_nan
      
      FOR iy=0,nyt-1 DO BEGIN
        FOR ix=0,nxt-1 DO BEGIN

          profil_T = reform(temp_o[ix,iy,*])
          
          IF total(finite(profil_T) ) GE 2 THEN BEGIN
            profil_rho = reform(rho_o[ix,iy,*])
            profil_dept = gdept
            
            Epot=calc_delta_Epot_pour_1_delta_SST(delta_SST,profil_T+273.15,profil_rho,profil_dept,e3t,$
            h_CM=h_CM,prof_t10_m2=prof_t10_m2,/interp_2m)
              
            ci_o[ix,iy] = Epot^(1./3.)
            
          ENDIF
        ENDFOR
      ENDFOR
      
      ; sauve grille pour interpolation
      gphit_o =  gphit[firstxt:lastxt,firstyt:lastyt]
      glamt_o =  glamt[firstxt:lastxt,firstyt:lastyt]
      mask_o = fltarr(size(glamt_o,/dim))+1 & mask_o[where(finite(ci_o) EQ 0)]=0
      
      
      ;;;;;;;;;;;;;;;;;;;;;;  BUILD CI ;;;;;;;;;;;;;;;;;;;;;;   MODELE
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
      CI_w=fltarr(nxt,nyt) & CI_w[*,*]=!values.f_nan
      
      FOR iy=0,nyt-1 DO BEGIN
        FOR ix=0,nxt-1 DO BEGIN

          profil_T = reform(temp_w[ix,iy,*])
          
          IF total(finite(profil_T) ) GE 2 THEN BEGIN
            profil_rho = reform(rho_w[ix,iy,*])
            profil_dept = gdept
            
            Epot=calc_delta_Epot_pour_1_delta_SST(delta_SST,profil_T+273.15,profil_rho,profil_dept,e3t,$
            h_CM=h_CM,prof_t10_m2=prof_t10_m2,/interp_2m)
              
            ci_w[ix,iy] = Epot^(1./3.)
            
          ENDIF
        ENDFOR
      ENDFOR
      
     
      ; sauv grilles pour interp sorties modele sur la grille des obs      
      gphit_w = gphit[firstxt:lastxt,firstyt:lastyt]
      glamt_w = glamt[firstxt:lastxt,firstyt:lastyt]     
      mask_w  = fltarr(size(glamt_w,/dim))+1 & mask_w[where(finite(ci_w) EQ 0)]=0
      
      
      ; interp sorties modele sur la grille des obs
      CI_w_grido = fromirr('bilinear', CI_w, glamt_w, gphit_w, mask_w, glamt_o, gphit_o, mask_o)
     
      ; plot
      pathout  = '/Volumes/TIME_MACHINE/DATA/WOA_2009/DATA/'
      pathfile = 'woa09_T_monthly_world.nc'
      initncdf,pathout+pathfile,/fullCgrid,glam=[0,360]
      domdef,box

      title = 'ci'+strtrim(delta_SST,2)+'_clim_'+ month +'_'+ obs_nm+'_'+ run_nm
      openps, filename=title, /portrait, /keep_pfont
      plt,ci_o,0,50,/nocont,realcont=2,lct=60,marge=[0,0,0,0],small=[1,3,1],title=obs_nm,/carte,/rempli
      plt,ci_w_grido,0,50,/nocont,realcont=2,lct=60,marge=[0,0,0,0],small=[1,3,2],/noer,title=run_nm,/carte,/rempli
      plt,ci_w_grido-ci_o,-15,15,/nocont,realcont=2,lct=77,marge=[0,0,0,0],small=[1,3,3],/noer,title=run_nm+'-'+obs_nm,/carte,/rempli
      closeps
      
    ENDIF
    
    
    
    
    ;;;;;;;;;;;;;;;;;;;;;;  ;;;;;;;;;;;;;;;;;;;;;;  ;;;;;;;;;;;;;;;;;;;;;;  ;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;  d20 d26 ;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;  ;;;;;;;;;;;;;;;;;;;;;;  ;;;;;;;;;;;;;;;;;;;;;;  ;;;;;;;;;;;;;;;;;;;;;;
    IF jd26 THEN BEGIN
      ;;;;;;;;;;;;;;;;;;;;;;  BUILD d20 d26 ;;;;;;;;;;;;;;;;;;;;;;   OBS WOA
      ;-------------------------------------------------;
      pathout  = '/Volumes/TIME_MACHINE/DATA/WOA_2009/DATA/'
      pathfile = 'woa09_T_monthly_world.nc'
      initncdf,pathout+pathfile,/fullCgrid,glam=[0,360]
      domdef,box

      vari=read_ncdf('temp',0,11,/timestep, filename=pathout+pathfile, /nostruct) & help, vari
      IF month EQ 'DJF' THEN vari = (vari[*,*,*,11] + total(vari[*,*,*,0:1],4)) / 3.
      IF month EQ 'MAM' THEN vari = total(vari[*,*,*,2:4],4) / 3.
      IF month EQ 'JJA' THEN vari = total(vari[*,*,*,5:7],4) / 3.
      IF month EQ 'SON' THEN vari = total(vari[*,*,*,8:10],4) / 3.
      temp_o = vari[*,*,*] & vari = 0
      
      d26_o=fltarr(nxt,nyt) & d26_o[*,*]=!values.f_nan
      d20_o=fltarr(nxt,nyt) & d20_o[*,*]=!values.f_nan
      
      FOR iy=0,nyt-1 DO BEGIN
        FOR ix=0,nxt-1 DO BEGIN

          profil_T = reform(temp_o[ix,iy,*])
          diff_T = profil_T - 26.
          ig26 = where(diff_T GE 0.)
          il26 = where(diff_T LT 0.)

          IF ig26[0] NE -1 AND il26[0] NE -1 THEN BEGIN
            d26=interpol(gdept,diff_T,0.)
            d26_o[ix,iy] = d26
;            IF d26 LT 0 THEN stop
          ENDIF
         
          diff_T = profil_T - 20.
          ig20 = where(diff_T GE 0.)
          il20 = where(diff_T LT 0.)

          IF ig20[0] NE -1 AND il20[0] NE -1 THEN BEGIN
            d20=interpol(gdept,diff_T,0.)
            d20_o[ix,iy] = d20
;            IF d20 LT 0 THEN stop
          ENDIF
          
        ENDFOR
      ENDFOR

;      d26_o[where(finite(d26_o) EQ 0)]=!values.f_nan
;      d20_o[where(finite(d20_o) EQ 0)]=!values.f_nan
      
      ; sauve grille pour interpolation
      gphit_o =  gphit[firstxt:lastxt,firstyt:lastyt]
      glamt_o =  glamt[firstxt:lastxt,firstyt:lastyt]
      maskd26_o  = fltarr(size(glamt_o,/dim))+1 & maskd26_o[where(finite(d26_o) EQ 0)]=0
      maskd20_o  = fltarr(size(glamt_o,/dim))+1 & maskd20_o[where(finite(d20_o) EQ 0)]=0
      
      
      
      ;;;;;;;;;;;;;;;;;;;;;;  BUILD d20 d26 ;;;;;;;;;;;;;;;;;;;;;;   MODELE
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
      
      d26_w=fltarr(nxt,nyt) & d26_w[*,*]=!values.f_nan
      d20_w=fltarr(nxt,nyt) & d20_w[*,*]=!values.f_nan
      
      FOR iy=0,nyt-1 DO BEGIN
        FOR ix=0,nxt-1 DO BEGIN

          profil_T = reform(temp_w[ix,iy,*])
          diff_T = profil_T - 26.
          ig26 = where(diff_T GE 0.)
          il26 = where(diff_T LT 0.)

          IF ig26[0] NE -1 AND il26[0] NE -1 THEN BEGIN
            d26=interpol(gdept,diff_T,0.)
            d26_w[ix,iy] = d26
;            IF d26 LT 0 THEN stop
          ENDIF
         
          diff_T = profil_T - 20.
          ig20 = where(diff_T GE 0.)
          il20 = where(diff_T LT 0.)

          IF ig20[0] NE -1 AND il20[0] NE -1 THEN BEGIN
            d20=interpol(gdept,diff_T,0.)
            d20_w[ix,iy] = d20
;            IF d20 LT 0 THEN stop
          ENDIF
          
        ENDFOR
      ENDFOR

;      d26_w[where(finite(d26_w) EQ 0)]=0
;      d20_w[where(finite(d20_w) EQ 0)]=0
      
      ; sauv grilles pour interp sorties modele sur la grille des obs
      gphit_w = gphit[firstxt:lastxt,firstyt:lastyt]
      glamt_w = glamt[firstxt:lastxt,firstyt:lastyt]
      maskd26_w = fltarr(size(glamt_w,/dim))+1 & maskd26_w[where(finite(d26_w) EQ 0)]=0
      maskd20_w = fltarr(size(glamt_w,/dim))+1 & maskd20_w[where(finite(d20_w) EQ 0)]=0
      
      ; interp sorties modele sur la grille des obs
      d26_w_grido = fromirr('bilinear', d26_w, glamt_w, gphit_w, maskd26_w, glamt_o, gphit_o, maskd26_o)
      d20_w_grido = fromirr('bilinear', d20_w, glamt_w, gphit_w, maskd20_w, glamt_o, gphit_o, maskd20_o)
      
      ; plot
      pathout  = '/Volumes/TIME_MACHINE/DATA/WOA_2009/DATA/'
      pathfile = 'woa09_T_monthly_world.nc'
      initncdf,pathout+pathfile,/fullCgrid,glam=[0,360]
      domdef,box

      title = 'd26_clim_'+ month +'_'+ obs_nm+'_'+ run_nm
      openps, filename=title, /portrait, /keep_pfont
      plt,d26_o,0,200,/nocont,realcont=2,lct=5,marge=[0,0,0,0],small=[1,3,1],title=obs_nm,/carte,/rempli
      plt,d26_w_grido,0,200,/nocont,realcont=2,lct=5,marge=[0,0,0,0],small=[1,3,2],/noer,title=run_nm,/carte,/rempli
      plt,d26_w_grido-d26_o,-50,50,/nocont,realcont=2,lct=77,marge=[0,0,0,0],small=[1,3,3],/noer,title=run_nm+'-'+obs_nm,/carte,/rempli
      closeps

      title = 'd20_clim_'+ month +'_'+ obs_nm+'_'+ run_nm
      openps, filename=title, /portrait, /keep_pfont
      plt,d20_o,0,200,/nocont,realcont=2,lct=5,marge=[0,0,0,0],small=[1,3,1],title=obs_nm,/carte,/rempli
      plt,d20_w_grido,0,200,/nocont,realcont=2,lct=5,marge=[0,0,0,0],small=[1,3,2],/noer,title=run_nm,/carte,/rempli
      plt,d20_w_grido-d20_o,-50,50,/nocont,realcont=2,lct=77,marge=[0,0,0,0],small=[1,3,3],/noer,title=run_nm+'-'+obs_nm,/carte,/rempli
      closeps

    ENDIF



    ;;;;;;;;;;;;;;;;;;;;;;  ;;;;;;;;;;;;;;;;;;;;;;  ;;;;;;;;;;;;;;;;;;;;;;  ;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;  MLD ;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;  ;;;;;;;;;;;;;;;;;;;;;;  ;;;;;;;;;;;;;;;;;;;;;;  ;;;;;;;;;;;;;;;;;;;;;;    
    IF jMLD THEN BEGIN
      ;;;;;;;;;;;;;;;;;;;;;;  BUILD MDL ;;;;;;;;;;;;;;;;;   OBS WOA
      ;-------------------------------------------------;
      pathout  = '/Volumes/TIME_MACHINE/DATA/WOA_2009/DATA/'
      pathfile = 'woa09_T_monthly_world.nc'
      initncdf,pathout+pathfile,/fullCgrid,glam=[0,360]
      domdef,box

      vari=read_ncdf('temp',0,11,/timestep, filename=pathout+pathfile, /nostruct) & help, vari
      IF month EQ 'DJF' THEN vari = (vari[*,*,*,11] + total(vari[*,*,*,0:1],4)) / 3.
      IF month EQ 'MAM' THEN vari = total(vari[*,*,*,2:4],4) / 3.
      IF month EQ 'JJA' THEN vari = total(vari[*,*,*,5:7],4) / 3.
      IF month EQ 'SON' THEN vari = total(vari[*,*,*,8:10],4) / 3.
      temp_o = vari[*,*,*] & vari = 0
      
      pathfile = 'woa09_S_monthly_world.nc'
      vari=read_ncdf('s',0,11,/timestep, filename=pathout+pathfile, /nostruct) & help, vari
      IF month EQ 'DJF' THEN vari = (vari[*,*,*,11] + total(vari[*,*,*,0:1],4)) / 3.
      IF month EQ 'MAM' THEN vari = total(vari[*,*,*,2:4],4) / 3.
      IF month EQ 'JJA' THEN vari = total(vari[*,*,*,5:7],4) / 3.
      IF month EQ 'SON' THEN vari = total(vari[*,*,*,8:10],4) / 3.
      sal_o = vari[*,*,*] & vari = 0

      rho_o=rhon(sal_o,temp_o) & help, rho_o

      mldt_o=fltarr(nxt,nyt) & mldt_o[*,*]=!values.f_nan
      mldr_o=fltarr(nxt,nyt) & mldr_o[*,*]=!values.f_nan
      
      FOR iy=0,nyt-1 DO BEGIN
        FOR ix=0,nxt-1 DO BEGIN

        IF finite(temp_o[ix,iy,0]) EQ 1 THEN BEGIN 
          profil_T = reform(temp_o[ix,iy,*])
	  temp_crit = profil_T[where(gdept EQ 10.)] - 0.2
 	  diff_T = profil_T - temp_crit[0]
;	  igmldt = where(diff_T GE 0.)
          ilmldt = where(diff_T LT 0.) & ilmldt = ilmldt[0]
          igmldt = ilmldt - 1

          IF igmldt[0] NE -1 AND ilmldt[0] NE -1 THEN BEGIN
            mldt = interpol(gdept[igmldt:ilmldt],diff_T[igmldt:ilmldt],0.)
            mldt_o[ix,iy] = mldt
            IF mldt LT 0 THEN stop
          ENDIF
        ENDIF

          profil_rho = reform(rho_o[ix,iy,*])
	  rho_crit = profil_rho[where(gdept EQ 10.)] + 0.03
          igmldr = where(profil_rho GE rho_crit[0])
          ilmldr = where(profil_rho LT rho_crit[0])

          IF igmldr[0] NE -1 AND ilmldr[0] NE -1 THEN BEGIN
            mldr = interpol(gdept,profil_rho,rho_crit[0])
            mldr_o[ix,iy] = mldr
            IF mldr LT 0 THEN stop
          ENDIF

        ENDFOR
      ENDFOR
      
      ; sauve grille pour interpolation
      gphit_o =  gphit[firstxt:lastxt,firstyt:lastyt]
      glamt_o =  glamt[firstxt:lastxt,firstyt:lastyt]
      mask_o = fltarr(size(glamt_o,/dim))+1 & mask_o[where(finite(mldt_o) EQ 0)]=0
      
      
      
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
      pathout  = '/Volumes/TIME_MACHINE/DATA/WOA_2009/DATA/'
      pathfile = 'woa09_T_monthly_world.nc'
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


    ;;;;;;;;;;;;;;;;;;;;;;  ;;;;;;;;;;;;;;;;;;;;;;  ;;;;;;;;;;;;;;;;;;;;;;  ;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;  SST + SSS ;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;  ;;;;;;;;;;;;;;;;;;;;;;  ;;;;;;;;;;;;;;;;;;;;;;  ;;;;;;;;;;;;;;;;;;;;;;

    IF jSST THEN BEGIN
      ;;;;;;;;;;;;;;;;;;;;;;  BUILD SST + SSS ;;;;;;;;;;;;;;;;;;;;;;   OBS WOA
      ;-------------------------------------------------;
      pathout  = '/Volumes/TIME_MACHINE/DATA/WOA_2009/DATA/'
      pathfile = 'woa09_T_monthly_world.nc'
      initncdf,pathout+pathfile,/fullCgrid,glam=[0,360]
      domdef,box

      vari=read_ncdf('temp',0,11,/timestep, filename=pathout+pathfile, /nostruct) & help, vari
      IF month EQ 'DJF' THEN vari = (vari[*,*,*,11] + total(vari[*,*,*,0:1],4)) / 3.
      IF month EQ 'MAM' THEN vari = total(vari[*,*,*,2:4],4) / 3.
      IF month EQ 'JJA' THEN vari = total(vari[*,*,*,5:7],4) / 3.
      IF month EQ 'SON' THEN vari = total(vari[*,*,*,8:10],4) / 3.
      sst_o = vari[*,*,0] & vari = 0
      
      pathfile = 'woa09_S_monthly_world.nc'
      vari=read_ncdf('s',0,11,/timestep, filename=pathout+pathfile, /nostruct) & help, vari
      IF month EQ 'DJF' THEN vari = (vari[*,*,*,11] + total(vari[*,*,*,0:1],4)) / 3.
      IF month EQ 'MAM' THEN vari = total(vari[*,*,*,2:4],4) / 3.
      IF month EQ 'JJA' THEN vari = total(vari[*,*,*,5:7],4) / 3.
      IF month EQ 'SON' THEN vari = total(vari[*,*,*,8:10],4) / 3.
      sss_o = vari[*,*,0] & vari = 0

      ; sauve grille pour interpolation
      gphit_o =  gphit[firstxt:lastxt,firstyt:lastyt]
      glamt_o =  glamt[firstxt:lastxt,firstyt:lastyt]
      mask_o = fltarr(size(glamt_o,/dim))+1 & mask_o[where(finite(sst_o) EQ 0)]=0 ;& mask_o[where(gphit_o EQ 0)]=0
      
      
      ;;;;;;;;;;;;;;;;;;;;;;  BUILD SST + SSS ;;;;;;;;;;;;;;;;;;;;;;   MODELE
      ;-------------------------------------------------;
      pathout  = '/Volumes/TIME_MACHINE/EXP_'+run_nm+'/NEMO/POST/MONTHLY/DATA/'
      pathfile = 'nemo_out_monthly_clim_1990-2008.nc'
      initncdf,pathout+pathfile,/fullCgrid;,glam=[0,360]
      domdef,box

      vari=read_ncdf('thetao',0,11,/timestep, filename=pathout+pathfile, /nostruct)
      vari[where(vari EQ 0)]=!values.f_nan & help, vari    
      IF month EQ 'DJF' THEN vari = (vari[*,*,*,11] + total(vari[*,*,*,0:1],4)) / 3.
      IF month EQ 'MAM' THEN vari = total(vari[*,*,*,2:4],4) / 3.
      IF month EQ 'JJA' THEN vari = total(vari[*,*,*,5:7],4) / 3.
      IF month EQ 'SON' THEN vari = total(vari[*,*,*,8:10],4) / 3.
      sst_w = vari[*,*,0] & vari = 0
      
      vari=read_ncdf('so',0,11,/timestep, filename=pathout+pathfile, /nostruct) 
      vari[where(vari EQ 0)]=!values.f_nan & help, vari    
      IF month EQ 'DJF' THEN vari = (vari[*,*,*,11] + total(vari[*,*,*,0:1],4)) / 3.
      IF month EQ 'MAM' THEN vari = total(vari[*,*,*,2:4],4) / 3.
      IF month EQ 'JJA' THEN vari = total(vari[*,*,*,5:7],4) / 3.
      IF month EQ 'SON' THEN vari = total(vari[*,*,*,8:10],4) / 3.
      sss_w = vari[*,*,0] & vari = 0

      ; sauv grilles pour interp sorties modele sur la grille des obs
      gphit_w =  gphit[firstxt:lastxt,firstyt:lastyt]
      glamt_w =  glamt[firstxt:lastxt,firstyt:lastyt]
      mask_w = fltarr(size(glamt_w,/dim))+1 & mask_w[where(finite(sst_w) EQ 0)]=0 & mask_w[where(gphit_w EQ 0)]=0
           
      ; interp sorties modele sur la grille des obs
      sst_w_grido = fromirr('bilinear', sst_w, glamt_w, gphit_w, mask_w, glamt_o, gphit_o, mask_o)
      sss_w_grido = fromirr('bilinear', sss_w, glamt_w, gphit_w, mask_w, glamt_o, gphit_o, mask_o)
      

      ; plot
      pathout  = '/Volumes/TIME_MACHINE/DATA/WOA_2009/DATA/'
      pathfile = 'woa09_T_monthly_world.nc'
      initncdf,pathout+pathfile,/fullCgrid,glam=[0,360]
      domdef,box

      title = 'sst_clim_'+ month +'_'+ obs_nm+'_'+ run_nm
      openps, filename=title, /portrait, /keep_pfont
      plt,sst_o,21,31,/nocont,realcont=2,lct=60,marge=[0,0,0,0],small=[1,3,1],title=obs_nm,/carte,/rempli
      plt,sst_w_grido,21,31,/nocont,realcont=2,lct=60,marge=[0,0,0,0],small=[1,3,2],/noer,title=run_nm,/carte,/rempli
      plt,sst_w_grido-sst_o,-5,5,/nocont,realcont=2,lct=77,marge=[0,0,0,0],small=[1,3,3],/noer,title=run_nm+'-'+obs_nm,/carte,/rempli
      closeps


      title = 'sss_clim_'+ month +'_'+ obs_nm+'_'+ run_nm
      openps, filename=title, /portrait, /keep_pfont
      plt,sss_o,28,38,/nocont,realcont=2,lct=60,marge=[0,0,0,0],small=[1,3,1],title=obs_nm,/carte,/rempli
      plt,sss_w_grido,28,38,/nocont,realcont=2,lct=60,marge=[0,0,0,0],small=[1,3,2],/noer,title=run_nm,/carte,/rempli
      plt,sss_w_grido-sss_o,-5,5,/nocont,realcont=2,lct=77,marge=[0,0,0,0],small=[1,3,3],/noer,title=run_nm+'-'+obs_nm,/carte,/rempli
      closeps

    ENDIF

  ENDFOR

END
