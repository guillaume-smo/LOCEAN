CASE sst_list[k] OF
'REMSS-MW'  : BEGIN 
	      @extract_sst_remss_mw
	      cmd = execute('SST'+strtrim(k,2)+'_1DTC_0 = SSTM_1DTC_0 & help, SST'+strtrim(k,2)+'_1DTC_0')
	      END
'REMSS-MWIR': BEGIN
	      @extract_sst_remss_mwir
	      cmd = execute('SST'+strtrim(k,2)+'_1DTC_0 = SSTI_1DTC_0 & help, SST'+strtrim(k,2)+'_1DTC_0')
	      END
'PSY3V3R1'  : BEGIN
	      @extract_sst_psy3v3r1
	      cmd = execute('SST'+strtrim(k,2)+'_1DTC_0 = SSTP_1DTC_0 & help, SST'+strtrim(k,2)+'_1DTC_0')
	      END
'GLORYS2V1' : BEGIN
	      @extract_sst_glorys2v1
	      cmd = execute('SST'+strtrim(k,2)+'_1DTC_0 = SSTG1_1DTC_0 & help, SST'+strtrim(k,2)+'_1DTC_0')	      
	      IF tc_name EQ 'IVAN' OR tc_name EQ 'GAEL' THEN BEGIN
	        @extract_ohc26_glorys2v1
	        @extract_dept26_glorys2v1
	      ENDIF
	      cmd = execute('SST'+strtrim(k,2)+'_1DTC_0 = SSTG1_1DTC_0 & help, SST'+strtrim(k,2)+'_1DTC_0')
	      END		
'GLORYS2V3' : BEGIN
	      @extract_sst_glorys2v3
	      cmd = execute('SST'+strtrim(k,2)+'_1DTC_0 = SSTG3_1DTC_0 & help, SST'+strtrim(k,2)+'_1DTC_0')	      
	      IF tc_name EQ 'IVAN' OR tc_name EQ 'GAEL' THEN BEGIN
	        @extract_ohc26_glorys2v3	      
	        @extract_dept26_glorys2v3
	      ENDIF
	      END
'AROME'     : BEGIN
	      @extract_sst_arome
	      END
'ALADIN'    : BEGIN
	      @extract_sst_aladana
	      END		
''          : print, 'NO SST EXTRACTION'
ENDCASE
