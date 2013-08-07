print, '' & print, 'RESTORING DATA...'


IF i EQ 1 THEN idate = -1
idate     = (idate + 1) MOD n_elements(date_list)
save_path = '/home/gsamson/WORK/AROME/TEST_CPL/EXPS_'+STRMID(exp_name, 0, 7)+'/EXP_'+exp_name+'_'+date_list[idate]+'/'
expid     = strtrim(i,2)

save_file = 'DATA_TC_R'+STRTRIM(ROUND(radius),2)+'km.idl'
RESTORE, FILENAME=save_path+save_file, /VERBOSE

obj = OBJ_NEW('IDL_Savefile', save_path+save_file)  ; & help, obj, /st
contents = obj->IDL_Savefile::Contents()	    ; & help, contents, /st
IF contents.n_var GE 1 THEN restore_list = obj->IDL_Savefile::Names()
OBJ_DESTROY, obj
FOR j = 0, n_elements(restore_list)-1 DO cmd = execute( restore_list[j]+'_'+strtrim(i,2)+'='+restore_list[j] )
;save_id = FIX((STRSPLIT(restore_list[0], '_', COUNT=cnt, /EXTRACT))[cnt-1])
;IF save_id NE i THEN BEGIN
;  FOR j = 0, n_elements(restore_list)-1 DO BEGIN
;    var = STRMID(restore_list[j], 0, STRPOS( restore_list[j], '_', /REVERSE_SEARCH))
;    cmd = execute( var+'_'+strtrim(i,2)+' = '+var+'_'+strtrim(save_id,2) )
;    print, var+'_'+strtrim(i,2)+' = '+var+'_'+strtrim(save_id,2)
;    cmd = execute( 'undefine, '+var+'_'+strtrim(save_id,2) )
;  ENDFOR
;ENDIF


save_file = 'VAR_1DTC_R'+STRTRIM(ROUND(radius),2)+'km.idl'
RESTORE, FILENAME=save_path+save_file, /VERBOSE

obj = OBJ_NEW('IDL_Savefile', save_path+save_file)  ; & help, obj, /st
contents = obj->IDL_Savefile::Contents()	    ; & help, contents, /st
IF contents.n_var GE 1 THEN restore_list = obj->IDL_Savefile::Names()
OBJ_DESTROY, obj
FOR j = 0, n_elements(restore_list)-1 DO cmd = execute( restore_list[j]+'_'+strtrim(i,2)+'='+restore_list[j] )
;save_id = FIX((STRSPLIT(restore_list[0], '_', COUNT=cnt, /EXTRACT))[cnt-1])
;IF save_id NE i THEN BEGIN
;  FOR j = 0, n_elements(restore_list)-1 DO BEGIN
;    var = STRMID(restore_list[j], 0, STRPOS( restore_list[j], '_', /REVERSE_SEARCH))
;    cmd = execute( var+'_'+strtrim(i,2)+' = '+var+'_'+strtrim(save_id,2) )
;    print, var+'_'+strtrim(i,2)+' = '+var+'_'+strtrim(save_id,2)
;    cmd = execute( 'undefine, '+var+'_'+strtrim(save_id,2) )
;  ENDFOR
;ENDIF


save_file = 'VAR_2DTC_R'+STRTRIM(ROUND(radius),2)+'km.idl'
RESTORE, FILENAME=save_path+save_file, /VERBOSE

obj = OBJ_NEW('IDL_Savefile', save_path+save_file)  ; & help, obj, /st
contents = obj->IDL_Savefile::Contents()	    ; & help, contents, /st
IF contents.n_var GE 1 THEN restore_list = obj->IDL_Savefile::Names()
OBJ_DESTROY, obj
FOR j = 0, n_elements(restore_list)-1 DO cmd = execute( restore_list[j]+'_'+strtrim(i,2)+'='+restore_list[j] )
save_id = FIX((STRSPLIT(restore_list[0], '_', COUNT=cnt, /EXTRACT))[cnt-1])
;IF save_id NE i THEN BEGIN
;  FOR j = 0, n_elements(restore_list)-1 DO BEGIN
;    var = STRMID(restore_list[j], 0, STRPOS( restore_list[j], '_', /REVERSE_SEARCH))
;    cmd = execute( var+'_'+strtrim(i,2)+' = '+var+'_'+strtrim(save_id,2) )
;    print, var+'_'+strtrim(i,2)+' = '+var+'_'+strtrim(save_id,2)
;    cmd = execute( 'undefine, '+var+'_'+strtrim(save_id,2) )
;  ENDFOR
;ENDIF


save_file = 'VAR_1D.idl'
RESTORE, FILENAME=save_path+save_file, /VERBOSE

obj = OBJ_NEW('IDL_Savefile', save_path+save_file)  ; & help, obj, /st
contents = obj->IDL_Savefile::Contents()	    ; & help, contents, /st
IF contents.n_var GE 1 THEN restore_list = obj->IDL_Savefile::Names()
OBJ_DESTROY, obj
FOR j = 0, n_elements(restore_list)-1 DO cmd = execute( restore_list[j]+'_'+strtrim(i,2)+'='+restore_list[j] )
;save_id = FIX((STRSPLIT(restore_list[0], '_', COUNT=cnt, /EXTRACT))[cnt-1])
;IF save_id NE i THEN BEGIN
;  FOR j = 0, n_elements(restore_list)-1 DO BEGIN
;    var = STRMID(restore_list[j], 0, STRPOS( restore_list[j], '_', /REVERSE_SEARCH))
;    cmd = execute( var+'_'+strtrim(i,2)+' = '+var+'_'+strtrim(save_id,2) )
;    print, var+'_'+strtrim(i,2)+' = '+var+'_'+strtrim(save_id,2)
;    cmd = execute( 'undefine, '+var+'_'+strtrim(save_id,2) )
;  ENDFOR
;ENDIF


print, 'RESTORE OK'
