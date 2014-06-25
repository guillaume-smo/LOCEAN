
print, 'CALCUL TIME FILTERING...'


FOR e = 0, n_elements(exp_list)-1 DO BEGIN


  ; IDL FILE DEFINITION
  IF e EQ 0 THEN exp_name = obs_name ELSE exp_name = exp_list[e]
  IF e GT 0 THEN BEGIN
    @def_model_gridmask
    save_dir = store_dir + grid +'_'+ model + '/outputs/' + exp_name + '/'
  ENDIF ELSE BEGIN
    save_dir = store_dir + 'OBS/outputs/' + exp_name + '/'
    FILE_MKDIR, save_dir
  ENDELSE
  tf_file  = var_name +'_'+ data_type +'TF_'+ zone +'_'+ mask_title +'_'+ period +'_'+ STRTRIM(FIX(yearini_mod),2) +'-'+ STRTRIM(FIX(yearend_mod),2) +'.idl'
  help, save_dir, tf_file


  IF FILE_TEST(save_dir+tf_file) THEN BEGIN

    RESTORE, save_dir+tf_file, /VERBOSE

  ENDIF ELSE BEGIN

    ; temporary var 
    print, '' & print, exp_list[e]
    IF e EQ 0 THEN var = var_0 ELSE cmd = execute( 'var = var_'+STRTRIM(e,2)+'_gridobs' ) 
    cmd = execute( 'time = time_'+STRTRIM(e,2) )
    cmd = execute( 'ind_mean1d = ind_mean1d_'+STRTRIM(e,2) )

    tmp = time_filter( var, date2jul(time), MIN(iso_filt), MAX(iso_filt), /cut)
    STOP

    ; dimensions & timestep (sampling frequency)
    nx = (SIZE(var))[1] & help, nx
    ny = (SIZE(var))[2] & help, ny
    nt = (SIZE(var))[3] & help, nt
    dt = date2jul(time[1])-date2jul(time[0]) & help, dt

    ; frequency & period axis
    x  = (FINDGEN((nt - 1)/2) + 1)
    nt_even = (nt MOD 2) EQ 0 & help, nt_even
    IF (nt_even) THEN freq = [0.0, x, nt/2, -nt/2 + x] / (nt*dt) $
                 ELSE freq = [0.0, x, -(nt/2 + 1) + x] / (nt*dt)
    riodpe = 1 / freq
    help, freq, riodpe

    ; cutoff frequencies
    indpos   = WHERE( riodpe GE 0.)
    fhig_iso = freq[ indpos[ MIN( WHERE( riodpe[indpos] LE MIN(iso_filt)))]] & help, fhig_iso
    flow_iso = freq[ indpos[ MAX( WHERE( riodpe[indpos] GE MAX(iso_filt)))]] & help, flow_iso
    indiso   = WHERE( ABS(freq) GE flow_iso AND ABS(freq) LE fhig_iso) & help, indiso
    fhig_syn = freq[ indpos[ MIN( WHERE( riodpe[indpos] LE MIN(syn_filt)))]] & help, fhig_syn
    flow_syn = freq[ indpos[ MAX( WHERE( riodpe[indpos] GE MAX(syn_filt)))]] & help, flow_syn
    indsyn   = WHERE( ABS(freq) GE flow_syn AND ABS(freq) LE fhig_syn) & help, indsyn
    STOP

    ; vectorize array
    tmp   = REFORM( var, nx*ny, nt)
    indok = WHERE( (FINITE( tmp[*,0]) EQ 1) AND (MEAN( tmp[*,ind_mean1d], DIMENSION=2, /NAN) GT 1.), nbok) & help, indok

    ; time filtering
    var_iso  = FLTARR( nx*ny, nt) * !VALUES.F_NAN
    var_syn  = FLTARR( nx*ny, nt) * !VALUES.F_NAN
    var_nosc = FLTARR( nx*ny, nt) * !VALUES.F_NAN
    fft_iso  = FLTARR(nt) * 0.
    fft_syn  = FLTARR(nt) * 0.
    var_iso[indok,*] = 0.
    var_syn[indok,*] = 0.
 
    FOR i = 0, nbok-1 DO BEGIN
      print, i, '/', nbok-1
      ;filter_sc, tmp[indok[i],*], var_sc1d
      var_nosc[indok[i],*] = tmp[indok[i],*] ;- var_sc1d
      ;var_iso[indok[i],*] = BANDPASS_FILTER( var_nosc[indok[i],*], flow_iso, fhig_iso, /GAUSSIAN)
      tmp_fft = ABS( FFT( REFORM( var_nosc[indok[i],*] ) ) )
      fft_iso[indiso] = tmp_fft[indiso]
      fft_syn[indsyn] = tmp_fft[indsyn]
      var_iso[indok[i],*] = FLOAT( FFT( fft_iso, /INVERSE))
      var_syn[indok[i],*] = FLOAT( FFT( fft_syn, /INVERSE))
      STOP
    ENDFOR

    ; diags
    var_iso    =   REFORM( var_iso, nx, ny, nt, /OVERWRITE)
    var_isots  = MEAN( MEAN( var_iso, DIMENSION=1, /NAN), DIMENSION=1, /NAN)
    var_isosd  =   STDDEV( var_iso[*,*,ind_mean1d], DIMENSION=3, /NAN)
    var_isovar = VARIANCE( var_iso[*,*,ind_mean1d], DIMENSION=3, /NAN)
    var_iso    =     MEAN( var_iso[*,*,ind_mean1d], DIMENSION=3, /NAN)
    var_totvar = VARIANCE( TEMPORARY(var[*,*,ind_mean1d]), DIMENSION=3, /NAN)
    help, var_iso, var_isots, var_isosd, var_isovar, var_totvar
    var_syn    =   REFORM( var_syn, nx, ny, nt, /OVERWRITE)
    var_synts  = MEAN( MEAN( var_syn, DIMENSION=1, /NAN), DIMENSION=1, /NAN)
    var_synsd  =   STDDEV( var_syn[*,*,ind_mean1d], DIMENSION=3, /NAN)
    var_synvar = VARIANCE( var_syn[*,*,ind_mean1d], DIMENSION=3, /NAN)
    var_syn    =     MEAN( var_syn[*,*,ind_mean1d], DIMENSION=3, /NAN)
    help, var_syn, var_synts, var_synsd, var_synvar

    ; WRITE IDL FILE
    ;SAVE, var_iso, var_isots, var_isosd, var_isovar, var_syn, var_synts, var_synsd, var_synvar, var_totvar, FILENAME=save_dir+tf_file, /VERBOSE

  ENDELSE

  ; SAUVEGARDE
  cmd = execute( 'var_iso_'+STRTRIM(e,2)+'    = TEMPORARY(var_iso)' )
  cmd = execute( 'var_isots_'+STRTRIM(e,2)+'  = TEMPORARY(var_isots)' )
  cmd = execute( 'var_isosd_'+STRTRIM(e,2)+'  = TEMPORARY(var_isosd)' )
  cmd = execute( 'var_isovar_'+STRTRIM(e,2)+' = TEMPORARY(var_isovar)' )
  cmd = execute( 'var_totvar_'+STRTRIM(e,2)+' = TEMPORARY(var_totvar)' )
  cmd = execute( 'help, var_iso_'+STRTRIM(e,2) )
  cmd = execute( 'help, var_isots_'+STRTRIM(e,2) )
  cmd = execute( 'help, var_isosd_'+STRTRIM(e,2) )
  cmd = execute( 'help, var_isovar_'+STRTRIM(e,2) )
  cmd = execute( 'help, var_totvar_'+STRTRIM(e,2) )
  cmd = execute( 'var_syn_'+STRTRIM(e,2)+'    = TEMPORARY(var_syn)' )
  cmd = execute( 'var_synts_'+STRTRIM(e,2)+'  = TEMPORARY(var_synts)' )
  cmd = execute( 'var_synsd_'+STRTRIM(e,2)+'  = TEMPORARY(var_synsd)' )
  cmd = execute( 'var_synvar_'+STRTRIM(e,2)+' = TEMPORARY(var_synvar)' )
  cmd = execute( 'help, var_syn_'+STRTRIM(e,2) )
  cmd = execute( 'help, var_synts_'+STRTRIM(e,2) )
  cmd = execute( 'help, var_synsd_'+STRTRIM(e,2) )
  cmd = execute( 'help, var_synvar_'+STRTRIM(e,2) )

ENDFOR ; EXP LOOP



; PLOTS

var_plot = 'var_iso'
@plot_2D

FOR e = 0, n_elements(exp_list)-1 DO $
IF e EQ 0 THEN cmd = execute( 'ratio_isotot_'+STRTRIM(e,2)+' = var_iso_'+STRTRIM(e,2)+' / MEAN(var_'+STRTRIM(e,2)+'[*,*,ind_mean1d_'+STRTRIM(e,2)+'], DIMENSION=3, /NAN) * 100.' ) $
ELSE cmd = execute( 'ratio_isotot_'+STRTRIM(e,2)+' = var_iso_'+STRTRIM(e,2)+' / MEAN(var_'+STRTRIM(e,2)+'_gridobs[*,*,ind_mean1d_'+STRTRIM(e,2)+'], DIMENSION=3, /NAN) * 100.' )

var_plot = 'ratio_isotot'
@plot_2D


var_plot = 'var_syn'
@plot_2D

FOR e = 0, n_elements(exp_list)-1 DO $
IF e EQ 0 THEN cmd = execute( 'ratio_syntot_'+STRTRIM(e,2)+' = var_syn_'+STRTRIM(e,2)+' / MEAN(var_'+STRTRIM(e,2)+'[*,*,ind_mean1d_'+STRTRIM(e,2)+'], DIMENSION=3, /NAN) * 100.' ) $
ELSE cmd = execute( 'ratio_syntot_'+STRTRIM(e,2)+' = var_syn_'+STRTRIM(e,2)+' / MEAN(var_'+STRTRIM(e,2)+'_gridobs[*,*,ind_mean1d_'+STRTRIM(e,2)+'], DIMENSION=3, /NAN) * 100.' )

var_plot = 'ratio_syntot'
@plot_2D


var_plot = 'var_isosd'
@plot_2D

var_plot = 'var_synsd'
@plot_2D

var_plot = 'var_isovar'
@plot_2D

var_plot = 'var_synvar'
@plot_2D

FOR e = 0, n_elements(exp_list)-1 DO cmd = execute( 'ratio_isototvar_'+STRTRIM(e,2)+' = var_isovar_'+STRTRIM(e,2)+' / var_totvar_'+STRTRIM(e,2)+' * 100.' )
var_plot = 'ratio_isototvar'
@plot_2D

FOR e = 0, n_elements(exp_list)-1 DO cmd = execute( 'ratio_syntotvar_'+STRTRIM(e,2)+' = var_synvar_'+STRTRIM(e,2)+' / var_totvar_'+STRTRIM(e,2)+' * 100.' )
var_plot = 'ratio_syntotvar'
@plot_2D

print, 'OK' & print, ''

