PRO tmp

x=2
help, x
 
CASE x OF
   1: PRINT, 'one'
   (x EQ 2) OR (x EQ 3): PRINT, 'two or three'
   3: PRINT, 'three'
   4: PRINT, 'four'
   ELSE: PRINT, 'Not one through four'
ENDCASE

END
