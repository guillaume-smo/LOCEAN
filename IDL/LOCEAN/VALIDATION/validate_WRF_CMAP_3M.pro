PRO validate_WRF_CMAP_3M
  @all_cm
 

  run_nm='COUPLED_SW2_BMJ'
  obs_nm='CMAP-STD'
 
  month_list = ['DJF','MAM','JJA','SON']
  box = [30,130,-30,25]

  jRAIN  = 1
  
  datedeb = 19900101
  datefin = 20100101
  datedeb_f = strtrim(string(datedeb),2)
  datefin_f = strtrim(string(datefin),2)
  yeardeb_f = strmid(string(datedeb),4,4)
  yearfin_f = strmid(string(datefin),4,4)
 

  FOR m = 0, n_elements(month_list)-1 DO BEGIN

    month = month_list[m]

    IF jRAIN THEN BEGIN
      ;;;;;;;;;;;;;;;;;;;;;;  OBS ;;;;;;;;;;;;;;;;;;;;;;; 
      ;-------------------------------------------------;
      pathout  = '/Volumes/TIME_MACHINE/DATA/PRECIP-CMAP/'
      pathfile = 'precip.mon.ltm_standard.nc'
      initncdf, pathout+pathfile, /fullcgrid
      domdef, box

      vari=read_ncdf('precip',0,11,/timestep, filename=pathout+pathfile, /nostruct) & help, vari
      IF month EQ 'DJF' THEN vari = (vari[*,*,11] + total(vari[*,*,0:1],3)) / 3.
      IF month EQ 'MAM' THEN vari = total(vari[*,*,2:4],3) / 3.
      IF month EQ 'JJA' THEN vari = total(vari[*,*,5:7],3) / 3.
      IF month EQ 'SON' THEN vari = total(vari[*,*,8:10],3) / 3.
      rain_o = vari[*,*] & help, rain_o & vari = 0

      ; sauve grille + masque pour interpolation
      gphit_o = gphit[firstxt:lastxt,firstyt:lastyt] & help, gphit_o
      glamt_o = glamt[firstxt:lastxt,firstyt:lastyt] & help, glamt_o
      mask_o  = fltarr(size(glamt_o,/dim))+1; & mask_o[where(finite(rain_o) EQ 0)]=0


      ;;;;;;;;;;;;;;;;;;;;;;  MODELE ;;;;;;;;;;;;;;;;;;;;
      ;-------------------------------------------------;
      maskfile = '/Users/gslod/WORK/IDL/MASK_WRF_GRID_T.nc'
      initncdf, maskfile, /fullcgrid
      domdef, box

      pathout  = '/Volumes/TIME_MACHINE/EXP_'+run_nm+'/WRF/POST/MONTHLY/DATA/'
      pathfile = 'wrfout_monthly_clim_1990-2009.nc'
      vari=read_ncdf('RAIN',0,11,/timestep, filename=pathout+pathfile, /nostruct) & help, vari
;      vari[where(vari EQ 0)] = !values.f_nan & help, vari    
      IF month EQ 'DJF' THEN vari = (vari[*,*,11] + total(vari[*,*,0:1],3)) / 3.
      IF month EQ 'MAM' THEN vari = total(vari[*,*,2:4],3) / 3.
      IF month EQ 'JJA' THEN vari = total(vari[*,*,5:7],3) / 3.
      IF month EQ 'SON' THEN vari = total(vari[*,*,8:10],3) / 3.
      rain_w = vari[*,*] & help, precip_w & vari = 0

      ; sauv grille + masque pour interp sorties modele sur la grille des obs
      gphit_w = gphit[firstxt:lastxt,firstyt:lastyt] & help, gphit_w
      glamt_w = glamt[firstxt:lastxt,firstyt:lastyt] & help, glamt_w
      mask_w  = fltarr(size(glamt_w,/dim))+1; & mask_w[where(finite(rain_w) EQ 0)]=0 

      ; interp sorties modele sur la grille des obs
      rain_w_grido = fromirr('bilinear', rain_w, glamt_w, gphit_w, mask_w, glamt_o, gphit_o, mask_o)

      ; plot
      pathout  = '/Volumes/TIME_MACHINE/DATA/PRECIP-CMAP/'
      pathfile = 'precip.mon.ltm.nc'
      initncdf, pathout+pathfile, /fullcgrid
      domdef, box

      title = 'precip_clim_'+ month +'_'+ obs_nm+'_'+ run_nm
      openps, filename=title, /portrait, /keep_pfont
      plt,rain_o,0,40,/nocont,realcont=2,lct=60,marge=[0,0,0,0],small=[1,3,1],title=obs_nm,/carte,/rempli
      plt,rain_w_grido,0,40,/nocont,realcont=2,lct=60,marge=[0,0,0,0],small=[1,3,2],/noer,title=run_nm,/carte,/rempli
      plt,rain_w_grido-rain_o,-10,10,/nocont,realcont=2,lct=77,marge=[0,0,0,0],small=[1,3,3],/noer,title=run_nm+'-'+obs_nm,/carte,/rempli
      closeps

    ENDIF

  ENDFOR

END
