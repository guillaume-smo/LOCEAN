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
		@read_sst_glorys2v1
		IF tc_name EQ 'IVAN' OR tc_name EQ 'GAEL' THEN BEGIN
		  @read_ohc_glorys2v1
		ENDIF
		END
  'GLORYS2V3' : BEGIN
		@read_sst_glorys2v3
		IF tc_name EQ 'IVAN' OR tc_name EQ 'GAEL' THEN BEGIN
		  @read_ohc_glorys2v3
		ENDIF
		END
  'ALADIN'    : BEGIN
		@read_sst_aladin
		END
  'AROME'     : print, 'AROME SST READ WITH MODEL DATA'

  ''          : print, 'NO SST READ'
ENDCASE
