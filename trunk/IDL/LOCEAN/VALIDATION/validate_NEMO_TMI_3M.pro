PRO validate_NEMO_TMI_3M
  @all_cm
 

  run_nm='COUPLED_SW2_BMJ'
  obs_nm='TMI-AMSR'
 
;  month_list = ['DJF','MAM','JJA','SON']
  month_list = ['DJF']
 
  bassin = 'SIO'

  IF bassin EQ  'IO' THEN box = [30,130,-30,25]
  IF bassin EQ 'SIO' THEN box = [30,130,-30, 0]
  IF bassin EQ 'NIO' THEN box = [30,130,  0,25]

  jSST  = 1
  
  datedeb = 19900101
  datefin = 20100101
  datedeb_f = strtrim(string(datedeb),2)
  datefin_f = strtrim(string(datefin),2)
  yeardeb_f = strmid(string(datedeb),4,4)
  yearfin_f = strmid(string(datefin),4,4)
 

  FOR m = 0, n_elements(month_list)-1 DO BEGIN

    month = month_list[m]

    IF jSST THEN BEGIN

      ; OBS
;      pathout  = '/Volumes/TIME_MACHINE/DATA/SST-TMI/'
      pathout  = '/Users/gslod/WORK/DATA/SST-TMI/'
      pathfile = 'SST_SC_MONTHLY_TMI-AMSR_1998-2009_IO.nc'
      initncdf, pathout+pathfile, /fullcgrid
      domdef, box

      vari=read_ncdf('SST',0,11,/timestep, filename=pathout+pathfile, /nostruct) & help, vari
      IF month EQ 'DJF' THEN vari = (vari[*,*,11] + total(vari[*,*,0:1],3)) / 3.
      IF month EQ 'MAM' THEN vari = total(vari[*,*,2:4],3) / 3.
      IF month EQ 'JJA' THEN vari = total(vari[*,*,5:7],3) / 3.
      IF month EQ 'SON' THEN vari = total(vari[*,*,8:10],3) / 3.
      sst_o = vari[*,*] & help, sst_o & vari = 0
      gphit_o = gphit[firstxt:lastxt,firstyt:lastyt] & help, gphit_o
      glamt_o = glamt[firstxt:lastxt,firstyt:lastyt] & help, glamt_o
      mask_o  = fltarr(size(glamt_o,/dim))+1 & mask_o[where(finite(sst_o) EQ 0)]=0


      ; MODELE
      pathout  = '/Volumes/TIME_MACHINE/EXP_'+run_nm+'/NEMO/POST/MONTHLY/DATA/'
      pathfile = 'nemo_out_monthly_clim_1990-2009.nc'
      initncdf, pathout+pathfile, /fullcgrid
      domdef, box

      vari=read_ncdf('thetao',0,11,/timestep, filename=pathout+pathfile, /nostruct)
      vari[where(vari EQ 0)] = !values.f_nan & help, vari    
      IF month EQ 'DJF' THEN vari = (vari[*,*,*,11] + total(vari[*,*,*,0:1],4)) / 3.
      IF month EQ 'MAM' THEN vari = total(vari[*,*,*,2:4],4) / 3.
      IF month EQ 'JJA' THEN vari = total(vari[*,*,*,5:7],4) / 3.
      IF month EQ 'SON' THEN vari = total(vari[*,*,*,8:10],4) / 3.
      sst_w = vari[*,*,0] & help, sst_w & vari = 0
      gphit_w = gphit[firstxt:lastxt,firstyt:lastyt] & help, gphit_w
      glamt_w = glamt[firstxt:lastxt,firstyt:lastyt] & help, glamt_w
      mask_w  = fltarr(size(glamt_w,/dim))+1 & mask_w[where(finite(sst_w) EQ 0)]=0 

      sst_w_grido = fromirr('bilinear', sst_w, glamt_w, gphit_w, mask_w, glamt_o, gphit_o, mask_o)

      ; plot
      pathout  = '/Volumes/TIME_MACHINE/DATA/SST-TMI/'
      pathfile = 'SST_SC_MONTHLY_TMI-AMSR_1998-2009_IO.nc'
      initncdf, pathout+pathfile, /fullcgrid
      domdef, box

;      title = 'sst_clim_'+ month +'_'+ obs_nm+'_'+ run_nm
;      openps, filename=title, /landscape
;      plt,sst_o,21,31,/nocont,realcont=2,lct=60,marge=[0,0,0,0],small=[1,3,1],title=obs_nm,subtitle=''
;      plt,sst_w_grido,21,31,/nocont,realcont=2,lct=60,marge=[0,0,0,0],small=[1,3,2],/noer,title=run_nm,subtitle=''
;      plt,sst_w_grido-sst_o,-5,5,/nocont,realcont=2,lct=77,marge=[0,0,0,0],small=[1,3,3],/noer,title=run_nm+'-'+obs_nm,subtitle=''
;      closeps

      title = 'sst_clim_'+ month +'_'+ obs_nm & openps, filename=title, /landscape      
      plt,sst_o,21,31,/realcont,lct=60,title=obs_nm,subtitle='' & closeps
      title = 'sst_clim_'+ month +'_'+ run_nm & openps, filename=title, /landscape      
      plt,sst_w_grido,21,31,/realcont,lct=60,title=run_nm,subtitle='' & closeps
      title = 'diff_sst_clim_'+ month +'_'+ obs_nm+'-'+ run_nm & openps, filename=title, /landscape      
      plt,sst_w_grido-sst_o,-5,5,/realcont,lct=77,title=run_nm+'-'+obs_nm,subtitle='' & closeps

    ENDIF

  ENDFOR

END
