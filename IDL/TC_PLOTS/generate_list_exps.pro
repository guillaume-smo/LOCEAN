; LISTE NOMS EXPS + RESEAU


; OBS + ANALYSE ALADIN
exp_list = ['BEST-TRACK']
alt_list = ['IBTRACS/RSMC']

; ALADIN-ANALYSE
IF use_ald_oper THEN BEGIN
  exp_list = [ explist, 'ALADIN-ANA']
  alt_list = [ alt_list, 'ANALYSE-ASSIM']
ENDIF

; ALADIN-OPER
IF use_ald_oper THEN BEGIN
  FOR i = 0, n_elements(date_list)-1 DO BEGIN
    exp_list = [ exp_list, 'ALADIN-OPER' ]
    alt_list = [ alt_list, date_list[i] ]
  ENDFOR
ENDIF

; EXPS AROME
nb_par = n_elements(par_list)
FOR k = 0, nb_par-1 DO BEGIN
FOR i = 0, n_elements(date_list)-1 DO BEGIN
;	  FOR j = 0, n_elements(res_list)-1 DO BEGIN
;    FOR k = 0, n_elements(par_list)-1 DO BEGIN
      IF par_list[k] NE '' THEN BEGIN
;         exp_list = [ exp_list, STRMID(date_list[i], 6, 2)+'_'+res_list[j]+'_'+par_list[k] ]
;	 exp_list = [ exp_list, STRMID(date_list[i], 6, 2)+'_'+par_list[k] ]
	 exp_list = [ exp_list, par_list[k]  ]
	 alt_list = [ alt_list, date_list[i] ]
       ENDIF
;     ENDFOR
;	  ENDFOR
ENDFOR
ENDFOR

help, exp_list, alt_list
