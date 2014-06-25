;-------------------------------------------------------------------------------------------------
; DISPERSION INTER-MODELES (ERROR STD DEV)
;-------------------------------------------------------------------------------------------------

all_errvar_mean = !NULL
FOR e = 1, n_elements(exp_list)-1 DO cmd = execute( 'all_errvar_mean =  [[[all_errvar_mean]], [[errvar_mean_'+strtrim(e,2)+']]]' )
std_errvar_mean =  STDDEV(all_errvar_mean, DIMENSION=3, /NAN)
help, std_errvar_mean

fig_name = var_name+'_ERROR_STDDEV_'+data_type+'_'+zone+'_'+period
IF write_ps THEN openps, filename=plot_dir+'/'+STRUPCASE(data_type)+'/'+zone+'/'+var_name+'/'+fig_name

var_plot = std_errvar_mean
minvar = MIN(var_plot, /NAN) & maxvar = MAX(var_plot, /NAN)
maxvar = maxvar - 0.10*(maxvar-minvar)
IF var_name EQ 'RAIN' THEN maxvar = 5.
IF var_name EQ 'SKT'  THEN maxvar = 5.
intvar=(maxvar-minvar)/20. & lct, 22
IF var_name EQ 'STRESS' THEN fmt='(F7.2)' ELSE fmt='(F6.1)'

plt, var_plot, min=minvar, max=maxvar, int=intvar, realcont=2, /nocont, title=var_name+' ERROR STD DEV - '+zone+' - '+period, subtitle='', xtitle='', ytitle='', charsize=1, cell_fill=clf

IF write_ps THEN closeps ELSE STOP
