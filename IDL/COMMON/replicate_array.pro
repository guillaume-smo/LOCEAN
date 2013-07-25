FUNCTION replicate_array, array, number

   ; Obtain information about the array.
   
ndims = Size(array, /N_Dimensions)
dims = Size(array, /Dimensions)
type = Size(array, /TName)
  
   ; Initialize some variables.
   
snumber = StrTrim(number,2)
newArray = 0

   ; Create the new array and populate it with the old array.
   
CASE ndims OF
   1: BEGIN
      command = 'newArray = Make_Array(' + StrTrim(dims[0],2) + ',' $
         + snumber + ',' + type + '=1)'
      ok = Execute(command)
      FOR j=0,number-1 DO newArray[*,j] = array
      END
   2: BEGIN
      command = 'newArray = Make_Array(' + StrTrim(dims[0],2) + ',' + $
         StrTrim(dims[1],2) + ',' + snumber + ',' + type + '=1)'
      ok = Execute(command)
      FOR j=0,number-1 DO newArray[*,*,j] = array
      END
   3: BEGIN
      command = 'newArray = Make_Array(' + StrTrim(dims[0],2) + ',' + $
         StrTrim(dims[1],2) + ',' + StrTrim(dims[2],2) + ','  + snumber $
         + ',' + type + '=1)'
      ok = Execute(command)
      FOR j=0,number-1 DO newArray[*,*,*,j] = array
      END
   ELSE: Print, 'Sorry, figure this out yourself. :-)'
ENDCASE

RETURN, newArray
END
