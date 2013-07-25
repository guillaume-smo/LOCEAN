PRO validate_NEMO_AVISO_3M
  @all_cm
 

  run_nm='COUPLED_SW2_BMJ'
  obs_nm='AVISO'
 
  month_list = ['DJF','MAM','JJA','SON']
  box = [30,130,-30,25]

  jSLA  = 1
  
  datedeb = 19900101
  datefin = 20100101
  datedeb_f = strtrim(string(datedeb),2)
  datefin_f = strtrim(string(datefin),2)
  yeardeb_f = strmid(string(datedeb),4,4)
  yearfin_f = strmid(string(datefin),4,4)
 

  FOR m = 0, n_elements(month_list)-1 DO BEGIN

    month = month_list[m]

    ;;;;;;;;;;;;;;;;;;;;;;  ;;;;;;;;;;;;;;;;;;;;;;  ;;;;;;;;;;;;;;;;;;;;;;  ;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;  SLA ;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;  ;;;;;;;;;;;;;;;;;;;;;;  ;;;;;;;;;;;;;;;;;;;;;;  ;;;;;;;;;;;;;;;;;;;;;;

    IF jSLA THEN BEGIN
      ;;;;;;;;;;;;;;;;;;;;;;  BUILD SLA ;;;;;;;;;;;;;;;;;;;;;;   OBS WOA
      ;-------------------------------------------------;
      pathout  = '/Volumes/TIME_MACHINE/DATA/SLA-AVISO/'
      pathfile = 'sla_sc_monthly_AVISO_1992-2009_IO.nc'
      initncdf, pathout+pathfile, /fullcgrid
      domdef, box

      vari=read_ncdf('SLA',0,11,/timestep, filename=pathout+pathfile, /nostruct) & help, vari
      IF month EQ 'DJF' THEN vari = (vari[*,*,11] + total(vari[*,*,0:1],3)) / 3.
      IF month EQ 'MAM' THEN vari = total(vari[*,*,2:4],3) / 3.
      IF month EQ 'JJA' THEN vari = total(vari[*,*,5:7],3) / 3.
      IF month EQ 'SON' THEN vari = total(vari[*,*,8:10],3) / 3.
      sla_o = vari[*,*] & help, sla_o & vari = 0

      ; sauve grille + masque pour interpolation
      gphit_o = gphit[firstxt:lastxt,firstyt:lastyt] & help, gphit_o
      glamt_o = glamt[firstxt:lastxt,firstyt:lastyt] & help, glamt_o
      mask_o  = fltarr(size(glamt_o,/dim))+1 & mask_o[where(finite(sla_o) EQ 0)]=0


      ;;;;;;;;;;;;;;;;;;;;;;  BUILD SLA ;;;;;;;;;;;;;;;;;;;;;;   MODELE
      ;-------------------------------------------------;
      pathout  = '/Volumes/TIME_MACHINE/EXP_'+run_nm+'/NEMO/POST/MONTHLY/DATA/'
      pathfile = 'nemo_out_monthly_clim_1990-2008.nc'
      initncdf, pathout+pathfile, /fullcgrid
      domdef, box

      vari=read_ncdf('zos',0,11,/timestep, filename=pathout+pathfile, /nostruct)
      vari[where(vari EQ 0.)] = !values.f_nan & help, vari    
      IF month EQ 'DJF' THEN vari = (vari[*,*,11] + total(vari[*,*,0:1],3)) / 3.
      IF month EQ 'MAM' THEN vari = total(vari[*,*,2:4],3) / 3.
      IF month EQ 'JJA' THEN vari = total(vari[*,*,5:7],3) / 3.
      IF month EQ 'SON' THEN vari = total(vari[*,*,8:10],3) / 3.
      sla_w = vari[*,*]*100. & help, sla_w & vari = 0

      ; sauv grille + masque pour interp sorties modele sur la grille des obs
      gphit_w = gphit[firstxt:lastxt,firstyt:lastyt] & help, gphit_w
      glamt_w = glamt[firstxt:lastxt,firstyt:lastyt] & help, glamt_w
      mask_w  = fltarr(size(glamt_w,/dim))+1 & mask_w[where(finite(sla_w) EQ 0)]=0 

      ; interp sorties modele sur la grille des obs
      sla_w_grido = fromirr('bilinear', sla_w, glamt_w, gphit_w, mask_w, glamt_o, gphit_o, mask_o)

      ; plot
      pathout  = '/Volumes/TIME_MACHINE/DATA/SLA-AVISO/'
      pathfile = 'sla_sc_monthly_AVISO_1992-2009_IO.nc'
      initncdf, pathout+pathfile, /fullcgrid
      domdef, box

      title = 'sla_clim_'+ month +'_'+ obs_nm+'_'+ run_nm
      openps, filename=title, /portrait, /keep_pfont
      plt,sla_o,-50,50,/nocont,realcont=2,lct=77,marge=[0,0,0,0],small=[1,3,1],title=obs_nm,/carte,/rempli
      plt,sla_w_grido,-50,50,/nocont,realcont=2,lct=77,marge=[0,0,0,0],small=[1,3,2],/noer,title=run_nm,/carte,/rempli
      plt,sla_w_grido-sla_o,-10,10,/nocont,realcont=2,lct=77,marge=[0,0,0,0],small=[1,3,3],/noer,title=run_nm+'-'+obs_nm,/carte,/rempli
      closeps

    ENDIF

  ENDFOR

END
