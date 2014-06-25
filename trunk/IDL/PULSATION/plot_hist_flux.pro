min_wind  = 0.
max_wind  = 15.
bin_wind  = 1.
nbin_wind = (max_wind-min_wind)/bin_wind
axe_wind  = min_wind + indgen(nbin_wind+1)*bin_wind

IF var_name EQ 'LH' THEN BEGIN
  min_bin = 0.
  max_bin = 300.
  siz_bin = 20.
  nb_bin  = (max_bin-min_bin)/siz_bin
  axe_bin = min_bin + indgen(nb_bin+1)*siz_bin
ENDIF

IF var_name EQ 'DELTAQ' THEN BEGIN
  min_bin = 0.
  max_bin = 10.
  siz_bin = 1.
  nb_bin  = (max_bin-min_bin)/siz_bin
  axe_bin = min_bin + indgen(nb_bin+1)*siz_bin
ENDIF

IF var_name EQ 'LHDELTAQ' THEN BEGIN
  min_bin = 0.
  max_bin = 50.
  siz_bin = 5.
  nb_bin  = (max_bin-min_bin)/siz_bin
  axe_bin = min_bin + indgen(nb_bin+1)*siz_bin
ENDIF

IF var_name EQ 'HFX' THEN BEGIN
  min_bin = 0.
  max_bin = 50.
  siz_bin = 5.
  nb_bin  = (max_bin-min_bin)/siz_bin
  axe_bin = min_bin + indgen(nb_bin+1)*siz_bin
ENDIF


FOR e = 0, n_elements(exp_list)-1 DO BEGIN

  cmd = execute( 'uv10 = uv10_'+data_type+'_'+strtrim(e,2) )
  IF var_name EQ 'LHDELTAQ' THEN BEGIN
    cmd = execute( 'DELTAQ_1D_'+strtrim(e,2)+'[where(ABS(DELTAQ_1D_'+strtrim(e,2)+') LT 0.01)] = !VALUES.F_NAN' )
    cmd = execute( 'var  = LH_'+data_type+'_'+strtrim(e,2)+' / DELTAQ_'+data_type+'_'+strtrim(e,2) )
  ENDIF ELSE BEGIN 
    cmd = execute( 'var  = -1.*'+var_name+'_'+data_type+'_'+strtrim(e,2) )
  ENDELSE

  ; MEAN PER WIND BIN
  mean_bin = fltarr(nbin_wind)
  nbpt_bin = lonarr(nbin_wind)
  FOR i = 0, nbin_wind-1 DO BEGIN
    indok = WHERE( uv10 GE axe_wind[i] AND uv10 LT axe_wind[i+1], nbok)
    nbpt_bin[i] = nbok
    IF var_name NE 'LHDELTAQM' THEN mean_bin[i] = MEAN( var[indok], /NAN) $
    ELSE cmd = execute( 'mean_bin[i] = MEAN( LH_'+data_type+'_'+strtrim(e,2)+'[indok], /NAN) / MEAN( DELTAQ_'+data_type+'_'+strtrim(e,2)+'[indok], /NAN)' )
  ENDFOR
  cmd = execute( 'mean_bin_'+strtrim(e,2)+' = mean_bin' )

ENDFOR



; MIN/MAX
var_plot = 'mean_bin'
cmd = execute( 'minvar = MIN('+var_plot+'_0, /NAN) & maxvar = MAX('+var_plot+'_0, /NAN)' )
FOR e = 1, n_elements(exp_list) -1 DO BEGIN
  cmd = execute( 'minvar = MIN([minvar,'+var_plot+'_'+strtrim(e,2)+'], /NAN)' )
  cmd = execute( 'maxvar = MAX([maxvar,'+var_plot+'_'+strtrim(e,2)+'], /NAN)' )
ENDFOR
maxvar = maxvar + 0.05*(maxvar-minvar)
minvar = minvar - 0.05*(maxvar-minvar)
lct, 60 & fmt='(F6.1)'


fig_name = var_plot+'_'+data_type+'_'+zone
IF write_ps THEN openps, filename=plot_dir+'/'+STRUPCASE(data_type)+'/'+zone+'/'+var_name+'/'+fig_name
IF write_ps THEN thc = 6 ELSE thc = 2
IF write_ps THEN chs = 1 ELSE chs = 1.5
color_list = [0, 50, 250, 150, 200, 100, 25]


axe_x = axe_wind[0:nbin_wind-1]+bin_wind/2.
cmd = execute( 'var_y = '+var_plot+'_0' )
splot, axe_x, var_y, xtitle='WIND BINS', title='WIND vs '+var_name+' - '+zone, charsize=1.5, ystyle=1, yrange=[minvar,maxvar], ytitle=var_name, lct=39, charthick=1.5, XTicklen=1, YTicklen=1, XGridStyle=1, YGridStyle=1, xrange=[min_wind,max_wind], /nodata
FOR e = 0, n_elements(exp_list)-1 DO BEGIN
  cmd = execute( 'var_y = '+var_plot+'_'+strtrim(e,2) )
  oplot, axe_x, var_y, thick=thc, color=color_list[e]
  IF exp_list[e] EQ 'OBS' THEN xyouts, 0.08, 0.175-e*0.025, obs_name, /NORMAL, charsize=chs, color=color_list[e], charthick=1 ELSE xyouts, 0.08, 0.175-e*0.025, exp_list[e], /NORMAL, charsize=chs, color=color_list[e], charthick=1
ENDFOR
FOR i = 0, nbin_wind-1 DO xyouts, axe_wind[0:nbin_wind-1], maxvar, STRTRIM(nbpt_bin,2), /DATA

IF write_ps THEN closeps ELSE STOP
