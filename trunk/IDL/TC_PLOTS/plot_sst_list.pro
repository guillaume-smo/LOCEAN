print, '' & print, 'PLOT SSTLIST 1DTC...'

lct,60
color_factor=70
key_portrait=1
write_ps = 1
IF write_ps THEN thc = 6 ELSE thc = 2


IF (where(sst_list EQ 'AROME'))[0] NE -1 THEN nb_sst = n_elements(sst_list)+nb_date-1 ELSE nb_sst = n_elements(sst_list)
initncdf, file_sstm & domdef, dom_tc

maxplot = !Values.F_NAN & minplot = !Values.F_NAN & var = !Values.F_NAN
FOR i = 0, nb_sst-1 DO BEGIN
  cmd = execute('maxplot=max([maxplot,SST'+strtrim(i,2)+'_1DTC_0-273.15], /nan)')
  cmd = execute('minplot=min([minplot,SST'+strtrim(i,2)+'_1DTC_0-273.15], /nan)')
ENDFOR
maxplot = maxplot + 0.05*(maxplot-minplot)
minplot = minplot - 0.05*(maxplot-minplot)
help, minplot, maxplot

IF write_ps THEN openps, filename=plt_path+'ALL_SSTLIST_1DTC'
time = juld_0 + 0.50d
jpt = n_elements(time)
FOR i = 0, nb_sst-1 DO BEGIN
  cmd = execute('var = SST'+strtrim(i,2)+'_1DTC_0')
  min = string( min(var-SST0_1DTC_0, /nan), format='(F4.1)')
  max = string( max(var-SST0_1DTC_0, /nan), format='(F4.1)')
  ave = string(mean(abs(var-SST0_1DTC_0), /nan), format='(F4.1)')
  IF i EQ 0 THEN pltt, var-273.15, 't', minplot, maxplot, xminor=4, title=tc_name+' BEST-TRACK SSTS', subtitle='', ytitle='SST (degC)', thick=thc, charsize=1.5, charthick=2 $
  ELSE pltt, var-273.15, 't', color=color_factor*i MOD 256, thick=thc, /ov1D
  oplot, [time[0],time[0]], [var[0]-273.15,var[0]-273.15], psym=1, thick=thc, symsize=2, color=color_factor*i MOD 256
  xyouts, 0.125, 0.180-0.020*i, sst_list[i], /normal, charsize=1.5, charthick=2, color=color_factor*i  MOD 256
  IF i GE 1 THEN xyouts, 0.400, 0.180-0.020*i, 'MIN/AVE/MAX ERROR: '+min+'/'+ave+'/'+max+' K', /normal, charsize=1.5, charthick=2, color=i*color_factor MOD 256
;  IF i LT n_elements(sst_list)-1 THEN sst_name = sst_list[i] ELSE sst_name = 'AROME_FIXE'  
;  IF i GE n_elements(sst_list)-1 THEN xyouts, 0.400, 0.180-0.020*i, '('+date_list[i-n_elements(sst_list)+1]+')', /normal, charsize=1.5, charthick=2, color=color_factor*i
ENDFOR
IF write_ps THEN closeps ELSE saveimage, plt_path+'ALL_SSTLIST_1DTC.gif', quality=100


print, 'PLOT SSTLIST 1DTC OK' & print, ''
