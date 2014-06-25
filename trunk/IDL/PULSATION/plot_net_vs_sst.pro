key_portait = 1

fig_name ='NETvsSST_sc1m_'+zone
IF write_ps THEN openps, filename=plot_dir+'/'+STRUPCASE(data_type)+'/'+zone+'/'+fig_name
IF write_ps THEN thc = 6 ELSE thc = 2
IF write_ps THEN chs = 1 ELSE chs = 1.5
color_list = [0, 50, 250, 150, 200, 100, 25]
lct, 60

sst_list = sst_sc1m_1 & FOR e = 2, n_elements(exp_list)-1 DO cmd = execute( 'sst_list = [sst_list, sst_sc1m_'+strtrim(e,2)+']' )
net_list = net_sc1m_1 & FOR e = 2, n_elements(exp_list)-1 DO cmd = execute( 'net_list = [net_list, net_sc1m_'+strtrim(e,2)+']' )

xmin = FLOOR(MIN([net_sc1m_0,net_list], /NAN))-5
xmax = CEIL( MAX([net_sc1m_0,net_list], /NAN))+5
ymin = FLOOR(MIN([sst_sc1m_0,sst_list], /NAN))-1
ymax = CEIL( MAX([sst_sc1m_0,sst_list], /NAN))+1
splot, [xmin, xmax], [ymin, ymax], /NODATA, xtitle='SURFACE NET HEAT FLUX (W/m2)', ytitle='SST (degC)', title=zone+' AVERAGE'

FOR e = 0, n_elements(exp_list)-1 DO BEGIN
  cmd = execute( 'oplot, [net_sc1m_'+strtrim(e,2)+',net_sc1m_'+strtrim(e,2)+'], [sst_sc1m_'+strtrim(e,2)+',sst_sc1m_'+strtrim(e,2)+'], psym = 1, color=color_list[e], thick=thc, symsize=2' )
  xyouts, 0.11, 0.175-e*0.025, exp_list[e], /NORMAL, charsize=chs, color=color_list[e], charthick=1
ENDFOR

linfit = LINFIT(net_list, sst_list) 
oplot, [xmin,xmax], linfit[0]+([xmin,xmax])*linfit[1], thick=1, color=200
oplot, [xmin,xmax], [sst_sc1m_0,sst_sc1m_0], thick=1, color=0
oplot, [net_sc1m_0,net_sc1m_0], [ymin,ymax], thick=1, color=0
xyouts, xmin, ymin, ' COEF:'+strtrim(linfit[1],2), /DATA, charsize=chs, color=200, charthick=1

IF write_ps THEN closeps ELSE STOP
