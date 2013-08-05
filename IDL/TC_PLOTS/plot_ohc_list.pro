print, '' & print, 'PLOT OHC & DEPT26 FROM SST-LIST 1DTC...'

IF tc_name EQ 'IVAN' OR tc_name EQ 'GAEL' THEN BEGIN

  ig1 = where(sst_list EQ 'GLORYS2V1')
  ig3 = where(sst_list EQ 'GLORYS2V3')

  IF ig1[0] NE -1 AND ig3[0] NE -1 THEN BEGIN

    lct,60
    color_factor=70
    key_portrait=1
    write_ps = 1
    IF write_ps THEN thc = 6 ELSE thc = 2


    initncdf, file_ohcg1 & domdef, dom_tc

    ; OHC
    maxplot=max([ohcg1_1dtc_0, ohcg3_1dtc_0], /nan)
    minplot=min([ohcg1_1dtc_0, ohcg3_1dtc_0], /nan)
    maxplot = maxplot + 0.05*(maxplot-minplot)
    minplot = minplot - 0.05*(maxplot-minplot)
    help, minplot, maxplot

    ig1 = where(sst_list EQ 'GLORYS2V1')
    ig3 = where(sst_list EQ 'GLORYS2V3')

    IF write_ps THEN openps, filename=plt_path+'GLORYS_OHC_1DTC'
    time = juld_0 + 0.50d
    jpt = n_elements(time)
    pltt, ohcg1_1dtc_0, 't', minplot, maxplot, xminor=4, title=tc_name+' BEST-TRACK GLORYS OHC', subtitle='', ytitle='OHC', charsize=1.5, charthick=2
    pltt, ohcg1_1dtc_0, 't', color=color_factor*3 MOD 256, thick=thc, /ov1D    
    pltt, ohcg3_1dtc_0, 't', color=color_factor*4 MOD 256, thick=thc, /ov1D
    xyouts, 0.125, 0.150-0.020*0, sst_list[ig1], /normal, charsize=1.5, charthick=2, color=color_factor*3  MOD 256
    xyouts, 0.125, 0.150-0.020*1, sst_list[ig3], /normal, charsize=1.5, charthick=2, color=color_factor*4  MOD 256    
    IF write_ps THEN closeps ELSE saveimage, plt_path+'GLORYS_OHC_1DTC.gif', quality=100


    ; DEPT26
    maxplot=max([dept26g1_1dtc_0, dept26g3_1dtc_0], /nan)
    minplot=min([dept26g1_1dtc_0, dept26g3_1dtc_0], /nan)
    maxplot = maxplot + 0.05*(maxplot-minplot)
    minplot = minplot - 0.05*(maxplot-minplot)
    help, minplot, maxplot

    ig1 = where(sst_list EQ 'GLORYS2V1')
    ig3 = where(sst_list EQ 'GLORYS2V3')

    IF write_ps THEN openps, filename=plt_path+'GLORYS_DEPT26_1DTC'
    time = juld_0 + 0.50d
    jpt = n_elements(time)
    pltt, dept26g1_1dtc_0, 't', minplot, maxplot, xminor=4, title=tc_name+' BEST-TRACK GLORYS DEPTH T26', subtitle='', ytitle='DEPTH (m)', charsize=1.5, charthick=2
    pltt, dept26g1_1dtc_0, 't', color=color_factor*3 MOD 256, thick=thc, /ov1D
    pltt, dept26g3_1dtc_0, 't', color=color_factor*4 MOD 256, thick=thc, /ov1D
    xyouts, 0.125, 0.150-0.020*0, sst_list[ig1], /normal, charsize=1.5, charthick=2, color=color_factor*3  MOD 256
    xyouts, 0.125, 0.150-0.020*1, sst_list[ig3], /normal, charsize=1.5, charthick=2, color=color_factor*4  MOD 256    
    IF write_ps THEN closeps ELSE saveimage, plt_path+'GLORYS_DEPT26_1DTC.gif', quality=100


  ENDIF

ENDIF

print, 'PLOT OHC & DEPT26 FROM SST-LIST 1DTC OK' & print, ''
