CASE sst_list[l] OF
    'REMSS-MW'  : BEGIN 
		  @read_sst_remss_mw
		  END
    'REMSS-MWIR': BEGIN
		  @read_sst_remss_mwir
		  END
    'PSY3V3R1'  : BEGIN
		  @read_sst_psy3v3r1
		  END
    'GLORYS2V1' : BEGIN
		  IF tc_name EQ 'IVAN' THEN BEGIN ;OR tc_name EQ 'GAEL' THEN BEGIN
		    @read_sst_glorys2v1
		    @read_ohc_glorys2v1
		  ENDIF
		  END
    'GLORYS2V3' : BEGIN
		  @read_sst_glorys2v3
		  IF tc_name EQ 'IVAN' THEN BEGIN ;OR tc_name EQ 'GAEL' THEN BEGIN
		    @read_ohc_glorys2v3
		  ENDIF
		  END
  'GLO2V1ALAD'  : BEGIN
		  IF tc_name EQ 'IVAN' THEN BEGIN ;OR tc_name EQ 'GAEL' THEN BEGIN
	            @read_sst_glo2v1alad
		  ENDIF
		  END
  'GLO2V3ALAD'  : BEGIN
		  IF tc_name EQ 'IVAN' THEN BEGIN ;OR tc_name EQ 'GAEL' THEN BEGIN
		    @read_sst_glo2v3alad
		  ENDIF
		  END
    'ALADIN'    : BEGIN
		  @read_sst_aladin
		  END
    'AROME'     : print, 'AROME SST READ WITH MODEL DATA'

    ''          : print, 'NO SST READ'
ENDCASE
