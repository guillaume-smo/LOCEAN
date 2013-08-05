PRO master
; programme d'analyse et de plot de simulations "cyclone"
; necessite SAXO (LOCEAN)
@all_cm


;------------------------------------------------------------------------------------------------------------------


; PARAMETERS
        tc_name       =  'IVAN' ; 'IVAN' ; 'GAEL' ; 'FELLENG' ; 'GIOVANNA' ; 'GELANE' (!BAD FORECAST @ 20100216H06!) ; 'BINGIZA' (!NOT WORKING!)
        @def_dates_obs
	radius        = 150. ; RAYON POUR MOYENNE AUTOUR DU CYCLONE (km)
        res_rad       = 10.  ; resolution radiale des moyennes azimuthales (km)
	dom_tc        = [45.50,68.00,-21.70,-9.20] ; definition domaine data
        force_rsmc    = 1    ; force to read rmsc best-track (even if present in ibtracs)
	degrad_surfex = 0    ; DEGRADATION RESOLUTION MODELES (0 ou 1)
	degrad_aladin = 0
        use_ald_anal  = 0    ; rajoute les analyses  aladin
        use_ald_oper  = 0    ; rajoute les forecasts aladin
	restore_extract_data = 1 ; read model data already extracted from idl files
        read_data     = 0    ; lecture des fichiers netcdf 

; SST PRODUCTS 
        sst_list  = ['REMSS-MW', 'REMSS-MWIR', 'PSY3V3R1', 'ALADIN', 'GLORYS2V3', 'GLORYS2V1']


; CRITERES MOYENNE D'ENSEMBLE
        par_list = [ 'IVAN2km_ECUME_AROME', 'IVAN2km_ECUGLO2V3_CPL' ]


; LISTE RESEAUX FORECAST ALADIN+AROME
        @def_reseaux_exps
        @generate_list_exps


; LISTE VARIABLES A EXTRAIRE
        ; variables obligatoires (ne pas modifier)
        var_list = [ 'SST','W10M_SEA','ZON10M_SEA','MER10M_SEA','MSLP_SEA','SST' ]
	unt_list = [ 'm/s','m/s','m/s','hPa','K' ]
        ; variables additionnelles	
	sup_list = [ 'LE_SEA','H_SEA','HLE_SEA','FM_SEA','CH_SEA','CE_SEA','CD_SEA' ]
	usp_list = [ 'W/m2','W/m2','W/m2','kg/ms2','W/s','W/s/K','W/s2' ]

; LISTE COMPLETE DES VARIABLES + UNITES
	;var_list = ['SST','FMT_SEA','W10M_SEA','ZON10M_SEA','MER10M_SEA','LE_SEA','H_SEA','T2M_SEA', $
	;            'Q2M_SEA','HU2M_SEA','RN_SEA','LWD_SEA','LWU_SEA','SWD_SEA','SWU_SEA']
	;unt_list = ['K','N/m2','m/s','m/s','m/s','W/m2','W/m2','K','g/kg','%','W/m2','W/m2','W/m2','W/m2','W/m2']


; SETUP FIGURES
        write_ps = 1 ; ecriture fichier postscript
	dom_plt = dom_tc & plt_path = ''
	IF n_elements(par_list) EQ 2 THEN plt_path = '/home/gsamson/WORK/IDL/FIGURES/'+date_list[0]+'_'+par_list[0]+'_vs_'+par_list[1]+'/'
	IF n_elements(par_list) EQ 1 AND par_list[0] NE '' THEN plt_path = '/home/gsamson/WORK/IDL/FIGURES/'+par_list[0]+'/'
        IF n_elements(par_list) EQ 1 AND par_list[0] EQ '' THEN plt_path = '/home/gsamson/WORK/IDL/FIGURES/'+tc_name+'/'
        IF n_elements(date_list) EQ 1 THEN plt_path = '/home/gsamson/WORK/IDL/FIGURES/'+date_list[0]+'_'+tc_name+'/'
	IF n_elements(par_list) GT 2 AND n_elements(date_list) GT 1 THEN plt_path = '/home/gsamson/WORK/IDL/FIGURES/'+tc_name+'_'+date_list[0]+'-'+date_list[n_elements(date_list)-1]+'/'	
	IF n_elements(date_list) GT 1 AND  n_elements(par_list) EQ 2 THEN plt_path = '/home/gsamson/WORK/IDL/FIGURES/'+par_list[0]+'_vs_'+par_list[1]+'/'
        IF date_list[0] EQ '' AND par_list[0] EQ '' THEN plt_path = '/home/gsamson/WORK/IDL/FIGURES/'+tc_name+'/'
	IF plt_path EQ '' THEN STOP ELSE FILE_MKDIR, plt_path, /NOEXPAND_PATH


;------------------------------------------------------------------------------------------------------------------


; READ VARIABLES
var_list = [ var_list, sup_list ]
unt_list = [ unt_list, usp_list ]
nb_exp = n_elements(exp_list) & help, nb_exp
nb_var = n_elements(var_list) & help, nb_var
nb_par = n_elements(par_list) & help, nb_par

FOR i = 0, nb_exp-1 DO BEGIN

  exp_name = exp_list[i]

  IF exp_name EQ 'BEST-TRACK' THEN BEGIN
    @read_best_track
    FOR l = 0, n_elements(sst_list)-1 DO BEGIN
      @read_ssts
    ENDFOR
  ENDIF

  IF read_data THEN BEGIN
    IF exp_name EQ 'ECMWF' THEN BEGIN
      @read_ecmwf 
    ENDIF
    IF exp_name EQ 'ALADIN-ANA' THEN BEGIN
      @read_aladin_ana
    ENDIF
    IF exp_name EQ 'ALADIN-OPER' THEN BEGIN
      @read_aladin_oper 
    ENDIF
    IF STRMID(exp_name,4,3) EQ '2km' OR STRMID(exp_name,4,3) EQ '4km' THEN BEGIN
      @read_surfex
  ;    @read_nemo
    ENDIF
  ENDIF

ENDFOR
;@read_nemo_rst
print, 'LECTURE OK' & print, ''


; PLOT SST-LIST COOLING
FOR l = 0, n_elements(sst_list)-1 DO BEGIN
  IF sst_list[l] NE '' AND sst_list[l] NE 'AROME' THEN BEGIN 
    sst_name = sst_list[l]
    @plot_cooling_sstlist
  ENDIF
ENDFOR

; PLOT HISTOGRAM UV10 vs FLUX
IF par_list[0] NE '' AND read_data THEN BEGIN
  @plot_histogram
ENDIF


; EXTRACTION
FOR i = 0, nb_exp-1 DO BEGIN

  exp_name = exp_list[i]
  print, 'EXTRACTION: ', exp_name

  IF exp_name NE 'BEST-TRACK' THEN BEGIN

    IF restore_extract_data THEN BEGIN
      @restore_extract_data
    ENDIF ELSE BEGIN
      ; ARRAYS DECLARATION
      @init_arrays
      ; TRACKING+EXTRACTION CYCLONE
      @extract_data_model
      @save_extract_data
    ENDELSE

  ENDIF ELSE BEGIN

    ; EXTRACTION SST+BEST-TRACK
    FOR k = 0, n_elements(sst_list)-1 DO BEGIN
      @extract_ssts
    ENDFOR

  ENDELSE
ENDFOR ; loop EXP
;@extract_sst_nemo_ald
;@extract_sst_rst_ald
; VERIF EXTRACT PLOT
;@plot_verif_extract
print, 'EXTRACTION OK' & print, ''



; PLOT CPL EXP COOLING
IF read_data THEN BEGIN
  FOR i = 0, nb_exp-1 DO BEGIN
    IF strmatch(exp_list[i], '*CPL*') EQ 1 THEN BEGIN
      exp_name = exp_list[i]
      @plot_cooling_cpl
    ENDIF
  ENDFOR
ENDIF
STOP


; PLOT SST+OHC LIST ALONG BEST-TRACK (1DTC)
@plot_sst_list
@plot_ohc_list
;STOP

; PLOT DES TRAJECTOIRES PAR RESEAU POUR VERIFICATION
@plot_all_tracks
;STOP

; PLOT DE TOUS LES MEMBRES DE LENSEMBLE
@plot_all_members
;STOP

; PLOT DE LA MOYENNE D'ENSEMBLE
@calcul_ensemble_mean
@plot_ensemble_mean
;STOP

; PLOT ERREURS DE PREVISION D'ENSEMBLE
@calcul_ensemble_error
@plot_all_errors
@save_ensemble_error
@plot_ensemble_error

SPAWN, 'for f in '+plt_path+'*.ps; do mv -f $f ${f%.*}.eps; done'

STOP


; CALCUL+PLOTS 1D "VARS+ERROR DOMAIN+TC / ANALYSE ALADIN-OPER"
@plot_1D
print, '' & print, 'PLOT_1D OK'
STOP

; PLOT 1D "MSLP vs WIND"
@plot_wind_mslp
STOP


END
