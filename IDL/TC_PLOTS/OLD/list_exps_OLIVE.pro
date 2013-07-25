; LISTE EXPS ABSOLUE + NOM ALTERNATIF


	exp_list = ['IBTRACS','ALADIN-ANA']
	alt_list = ['v03r03-WMO','ANALYSE-ASSIM']
;	exp_list = ['IBTRACS','ALADIN-ANA','ALADIN-OPER','ALADIN-OPER','ALADIN-OPER','ALADIN-OPER','ALADIN-OPER','ALADIN-OPER','ALADIN-OPER']
;	alt_list = ['v03r03-WMO','ANALYSE-ASSIM','20080212R00','20080212R12','20080213R00','20080213R12','20080214R00','20080214R12','20080215R00']


;       LISTE DATES FORECAST ALADIN+AROME ROUTINE PLOT_TRAJ
;        date_list = [ '20080212R00','20080212R12','20080213R00','20080213R12','20080214R00' ]

        FOR i = 0, n_elements(date_list)-1 DO BEGIN
          exp_list = [ exp_list, 'ALADIN-OPER' ]
          alt_list = [ alt_list, date_list[i] ]
	ENDFOR


        FOR i = 0, n_elements(date_list)-1 DO BEGIN
	  FOR j = 0, n_elements(res_list)-1 DO BEGIN
	    FOR k = 0, n_elements(par_list)-1 DO BEGIN
              exp_list = [ exp_list, res_list[j]+'_'+STRMID(date_list[i], 6, 2)+'_'+par_list[k] ]
              alt_list = [ alt_list, date_list[i] ]
;              exp_list = [ exp_list, resaro + '_' + STRMID(date_list[i], 6, 2) + '_' + param1, resaro + '_' + STRMID(date_list[i], 6, 2) + '_' + param2 ]
;              alt_list = [ alt_list, date_list[i] , date_list[i] ]
             ENDFOR
	  ENDFOR
	ENDFOR
	help, exp_list, alt_list



; 	GLORYS2 + ECUME/COARE
        IF (param1 EQ 'ECUME') AND (param2 EQ 'COARE') THEN BEGIN
        exp_list = [ exp_list, $
	            '9AZ2+9AZ1','9B0I+9B0J','9AWS+9AWT','9B09+9B0A','9AZ4+9AZ3', $
            	    '9AZN+9AZO','9B0B+9B0C','9AXW+9AXX','9B0D+9B0E','9AZP+9AZQ'  ]
        alt_list = [ alt_list, $
	            '20080212R00+GLORYS2_fix+ECUME','20080212R12+GLORYS2_fix+ECUME', $
                    '20080213R00+GLORYS2_fix+ECUME','20080213R12+GLORYS2_fix+ECUME', $
		    '20080214R00+GLORYS2_fix+ECUME', $
                    '20080212R00+GLORYS2_fix+COARE','20080212R12+GLORYS2_fix+COARE', $
                    '20080213R00+GLORYS2_fix+COARE','20080213R12+GLORYS2_fix+COARE', $
         	    '20080214R00+GLORYS2_fix+COARE'  ]
	ENDIF

; TODO
;        IF (param1 EQ 'ECUME') THEN BEGIN
;        exp_list1 = [ '9AZ2+9AZ1','9B0I+9B0J','9AWS+9AWT','9B09+9B0A','9AZ4+9AZ3' ]
;        alt_list1 = [ '20080212R00+GLORYS2_fix+ECUME','20080212R12+GLORYS2_fix+ECUME', $
;                      '20080213R00+GLORYS2_fix+ECUME','20080213R12+GLORYS2_fix+ECUME', $
;		      '20080214R00+GLORYS2_fix+ECUME' ]
;        ENDIF


; 	ECUME + SSTALAD_fix/GLORYS2_fix
        IF (param1 EQ 'SSTALAD_fix') AND (param2 EQ 'GLORYS2_fix') THEN BEGIN
        exp_list = [ exp_list, $
             	    '9B15+9B16','9B18+9B17','9AXV+9AXU','9B19+9B1A','9B13+9B12', $
            	    '9B1L+9B1M','9B1O+9B1N','9AWS+9AWT','9B1P+9B1Q','9B1R+9B1S'  ]
        alt_list = [ alt_list, $
		    '20080212R00+ECUME+SSTALAD_fix','20080212R12+ECUME+SSTALAD_fix', $
		    '20080213R00+ECUME+SSTALAD_fix','20080213R12+ECUME+SSTALAD_fix', $
		    '20080214R00+ECUME+SSTALAD_fix', $
	            '20080212R00+ECUME+GLORYS2_fix','20080212R12+ECUME+GLORYS2_fix', $
	            '20080213R00+ECUME+GLORYS2_fix','20080213R12+ECUME+GLORYS2_fix', $
      	            '20080214R00+ECUME+GLORYS2_fix'  ]

	ENDIF

; 	ECUME + SSTALAD_fix/GLORYS2_var
        IF (param1 EQ 'ECUME+SSTALAD_fix') AND (param2 EQ 'ECUME+GLORYS2_var') THEN BEGIN
        exp_list = [ exp_list, $
             	    '9B15+9B16','9B18+9B17','9AXV+9AXU','9B19+9B1A','9B13+9B12', $
            	    '9B1C+9B1D','9B1F+9B1G','9AX1+9AX2','9B1H+9B1I','9B1J+9B1K'  ]
        alt_list = [ alt_list, $
                    '20080212R00+ECUME+SSTALAD_fix','20080212R12+ECUME+SSTALAD_fix', $
                    '20080213R00+ECUME+SSTALAD_fix','20080213R12+ECUME+SSTALAD_fix', $
	            '20080214R00+ECUME+SSTALAD_fix', $
                    '20080212R00+ECUME+GLORYS2_var','20080212R12+ECUME+GLORYS2_var', $
                    '20080213R00+ECUME+GLORYS2_var','20080213R12+ECUME+GLORYS2_var', $
        	    '20080214R00+ECUME+GLORYS2_var'  ]
        ENDIF

; 	COARE3 + SSTALAD_fix/GLORYS2_var
        IF (param1 EQ 'COARE+SSTALAD_fix') AND (param2 EQ 'COARE+GLORYS2_var') THEN BEGIN
        exp_list = [ exp_list, $
             	    '9AZN+9AZO','9B0B+9B0C','9AXW+9AXX','9B0D+9B0E','9AZP+9AZQ', $
            	    '9B3L+9B3M','9B3N+9B3O','9B3P+9B3Q','9B3R+9B3S','9B3T+9B3U'  ]
        alt_list = [ alt_list, $
                    '20080212R00+COARE+SSTALAD_fix','20080212R12+COARE+SSTALAD_fix', $
                    '20080213R00+COARE+SSTALAD_fix','20080213R12+COARE+SSTALAD_fix', $
	            '20080214R00+COARE+SSTALAD_fix', $
                    '20080212R00+COARE+GLORYS2_var','20080212R12+COARE+GLORYS2_var', $
                    '20080213R00+COARE+GLORYS2_var','20080213R12+COARE+GLORYS2_var', $
        	    '20080214R00+COARE+GLORYS2_var'  ]
        ENDIF

;	COARE3 + SSTALAD_fix/REMSS-MWIR_var
        IF (param1 EQ 'COARE+SSTALAD_fix') AND (param2 EQ 'COARE+REMSS-MWIR_var') THEN BEGIN
        exp_list = [ exp_list, $
             	    '9AZN+9AZO','9B0B+9B0C','9AXW+9AXX','9B0D+9B0E','9AZP+9AZQ', $
            	    '9B4T+9B4S','9B4X+9B4W','9B4Y+9B4Z','9B51+9B50','9B53+9B52'  ]
        alt_list = [ alt_list, $
                    '20080212R00+COARE+SSTALAD_fix','20080212R12+COARE+SSTALAD_fix', $
                    '20080213R00+COARE+SSTALAD_fix','20080213R12+COARE+SSTALAD_fix', $
	            '20080214R00+COARE+SSTALAD_fix', $
                    '20080212R00+COARE+REMSS-MWIR_var','20080212R12+COARE+REMSS-MWIR_var', $
                    '20080213R00+COARE+REMSS-MWIR_var','20080213R12+COARE+REMSS-MWIR_var', $
        	    '20080214R00+COARE+REMSS-MWIR_var'  ]
        ENDIF


;	COARE3 + SSTALAD_fix / COARE3 + CPL
        IF (param1 EQ 'COARE+SSTALAD_fix') AND (param2 EQ 'COARE+CPL') THEN BEGIN
        exp_list = [ exp_list, $
             	    '9AZN+9AZO','9B0B+9B0C','9AXW+9AXX','9B0D+9B0E','9AZP+9AZQ', $
            	    'NOPRES_COARE_12','NOPRES_COARE_13','NOPRES_COARE_14' ]
        alt_list = [ alt_list, $
                    '20080212R00+COARE+SSTALAD_fix','20080212R12+COARE+SSTALAD_fix', $
                    '20080213R00+COARE+SSTALAD_fix','20080213R12+COARE+SSTALAD_fix', $
	            '20080214R00+COARE+SSTALAD_fix', $
                    '20080212R00+COARE+CPL','20080213R00+COARE+CPL','20080214R00+COARE+CPL' ]
        ENDIF


;	ECUME + SSTALAD_fix / ECUME + CPL
        IF (param1 EQ 'ECUME+SSTALAD_fix') AND (param2 EQ 'ECUME+CPL') THEN BEGIN
        exp_list = [ exp_list, $
             	    '9B15+9B16','9B18+9B17','9AXV+9AXU','9B19+9B1A','9B13+9B12', $
            	    'NOPRES_ECUME_12','NOPRES_ECUME_13','NOPRES_ECUME_14' ]
        alt_list = [ alt_list, $
                    '20080212R00+ECUME+SSTALAD_fix','20080212R12+ECUME+SSTALAD_fix', $
                    '20080213R00+ECUME+SSTALAD_fix','20080213R12+ECUME+SSTALAD_fix', $
	            '20080214R00+ECUME+SSTALAD_fix', $
                    '20080212R00+ECUME+CPL','20080213R00+ECUME+CPL','20080214R00+ECUME+CPL' ]
        ENDIF


;	ALADIN ONLY
	IF (param1 EQ '') AND (param2 EQ '') THEN BEGIN
          exp_list = [ exp_list ]
          alt_list = [ alt_list ]
	ENDIF
