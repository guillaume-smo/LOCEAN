FOR j = 0, n_elements(sst_list)-1 DO BEGIN
  CASE sst_list[j] OF
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
		END
  'GLORYS2V3' : BEGIN
                @read_sst_glorys2v3
		END
		

  ''          : print, 'NO SST READ'
  ENDCASE
ENDFOR
