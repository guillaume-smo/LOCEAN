FUNCTION restaure, file, varname, var
; restore only selected variables

  myfile = OBJ_NEW('IDL_Savefile', file) 
  savefileInfo = myfile->Contents() 
  ;help savefileInfo, /stucture
  print, 'VARIABLES AVAILABLE:', myfile->Names()
  test = size(varname, /type)
  IF test NE 0 THEN myfile->restore, varname
  OBJ_DESTROY, myfile
  IF test NE 0 THEN BEGIN
    tmp = execute('var ='+varname)
    print, 'VARIABLE '+varname+' LOADED'
  ENDIF
 
RETURN, var
END
