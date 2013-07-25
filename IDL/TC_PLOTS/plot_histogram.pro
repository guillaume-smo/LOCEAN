lct,60
color_factor = 70
key_portrait = 1
;write_ps = 1
IF write_ps THEN thc = 6 ELSE thc = 2


; CALCUL HISTOGRAMMES
print, '' & print, 'CALCUL HISTOGRAMMES...'

minbin = 0.
maxbin = 100.
binsiz = 10.
nb_bin = FIX((maxbin-minbin)/binsiz)

FOR j = 0, n_elements(sup_list)-1 DO BEGIN

  FOR i = 1, n_elements(exp_list)-1 DO BEGIN
    cmd = execute('test = n_elements('+sup_list[j]+'_'+strtrim(i,2)+')')
    cmd = execute('w10m = W10M_SEA_'+strtrim(i,2))
    cmd = execute('var  = '+sup_list[j]+'_'+strtrim(i,2))
    unt = usp_list[j]
    cmd = execute('his_'+sup_list[j]+'_'+strtrim(i,2)+' = fltarr(nb_bin)')
    FOR k = 0, nb_bin-1 DO BEGIN
      indok = where( w10m GE k*binsiz+minbin AND w10m LT (k+1)*binsiz+minbin, nbok)
      IF nbok GE 10 THEN cmd = execute('his_'+sup_list[j]+'_'+strtrim(i,2)+'[k] = mean( var[indok], /nan)') $
      ELSE cmd = execute('his_'+sup_list[j]+'_'+strtrim(i,2)+'[k] = !VALUES.F_NAN')
    ENDFOR
    cmd = execute('help, his_'+sup_list[j]+'_'+strtrim(i,2))
  ENDFOR

  l = 0
  FOR i = 0, nb_par-1 DO BEGIN
    cmd = execute( ' all_his_'+sup_list[j]+'_'+par_list[i]+' = fltarr(nb_date,nb_bin) + !VALUES.F_NAN' )
    cmd = execute( 'mean_his_'+sup_list[j]+'_'+par_list[i]+' = fltarr(nb_bin) + !VALUES.F_NAN' )
    cmd = execute( 'help, mean_his_'+sup_list[j]+'_'+par_list[i])
    FOR k = 0, nb_date-1 DO BEGIN
      l = l+1
      cmd = execute('all_his_'+sup_list[j]+'_'+par_list[i]+'[k,*] = his_'+sup_list[j]+'_'+strtrim(l,2) )
    ENDFOR
    FOR k = 0, nb_bin-1 DO cmd = execute( 'mean_his_'+sup_list[j]+'_'+par_list[i]+'[k] = mean( all_his_'+sup_list[j]+'_'+par_list[i]+'[*,k], /nan)' )
  ENDFOR

ENDFOR
print, 'CALCUL OK' & print, ''


; PLOT
FOR j = 0, n_elements(sup_list)-1 DO BEGIN


  cmd = execute('var = his_'+sup_list[j]+'_1')
  maxplot = max(var, /NAN)
  minplot = min(var, /NAN)
  FOR i = 2, n_elements(exp_list)-1 DO BEGIN
    cmd = execute('var = his_'+sup_list[j]+'_'+strtrim(i,2))
    maxplot = max([maxplot,var], /NAN)
    minplot = min([minplot,var], /NAN)
  ENDFOR
  maxplot = maxplot + 0.05*(maxplot-minplot)
  minplot = minplot - 0.05*(maxplot-minplot)

  IF write_ps THEN openps, filename=plt_path+'ALL_HIST_'+sup_list[j]
  cmd = execute('var = his_'+sup_list[j]+'_1')  
  cmd = execute('splot, minbin+binsiz/2.+findgen(nb_bin)*binsiz, var, XTICKFORMAT="(I3)", XTICKINTERVAL=10, xminor=1, xstyle=1, yrange=[minplot,maxplot],xrange=[minbin,maxbin], title="ALL HIST '+sup_list[j]+'", xtitle="WIND BINS", thick=thc, xgridstyle=2, xticklen=1.0, ytitle="'+sup_list[j]+' ('+usp_list[j]+')", charsize=1.5, charthick=2')

  FOR i = 1, n_elements(exp_list)-1 DO BEGIN
    cmd = execute('var = his_'+sup_list[j]+'_'+strtrim(i,2))  
    oplot, minbin+binsiz/2.+findgen(nb_bin)*binsiz, var, color=color_factor*i MOD 256, thick=thc
    oplot, [minbin,maxbin], [0,0], color=0
    xyouts, 0.125, 0.180-0.020*i, exp_list[i], /normal, charsize=1.5, charthick=2, color=color_factor*i MOD 256
    xyouts, 0.375, 0.180-0.020*i, '('+alt_list[i]+')', /normal, charsize=1.5, charthick=2, color=color_factor*i
  ENDFOR
  IF write_ps THEN closeps ELSE saveimage, plt_path+'ALL_HIST_'+sup_list[j]+'.gif', quality=100

  cmd = execute('var = mean_his_'+sup_list[j]+'_'+par_list[0])
  maxplot = max(var, /NAN)
  minplot = min(var, /NAN)
  FOR i = 1, n_elements(par_list)-1 DO BEGIN
    cmd = execute('var = mean_his_'+sup_list[j]+'_'+par_list[i])
    maxplot = max([maxplot,var], /NAN)
    minplot = min([minplot,var], /NAN)
  ENDFOR
  maxplot = maxplot + 0.05*(maxplot-minplot)
  minplot = minplot - 0.05*(maxplot-minplot)

  IF write_ps THEN openps, filename=plt_path+'MEAN_HIST_'+sup_list[j]
  cmd = execute('var = mean_his_'+sup_list[j]+'_'+par_list[0])  
  cmd = execute('splot, minbin+binsiz/2.+findgen(nb_bin)*binsiz, var, XTICKFORMAT="(I3)", XTICKINTERVAL=10, xminor=1, xstyle=1, yrange=[minplot,maxplot],xrange=[minbin,maxbin], title="MEAN HIST '+sup_list[j]+'", xtitle="WIND BINS", thick=thc, xgridstyle=2, xticklen=1.0, ytitle="'+sup_list[j]+' ('+usp_list[j]+')", charsize=1.5, charthick=2')

  FOR i = 0, n_elements(par_list)-1 DO BEGIN
    cmd = execute('var = mean_his_'+sup_list[j]+'_'+par_list[i])  
    oplot, minbin+binsiz/2.+findgen(nb_bin)*binsiz, var, color=color_factor*i MOD 256, thick=thc
    oplot, [minbin,maxbin], [0,0]
    xyouts, 0.125, 0.180-0.020*i, par_list[i], /normal, charsize=1.5, charthick=2, color=color_factor*i MOD 256
  ENDFOR
  IF write_ps THEN closeps ELSE saveimage, plt_path+'MEAN_HIST_'+sup_list[j]+'.gif', quality=100


ENDFOR
