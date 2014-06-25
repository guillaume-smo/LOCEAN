month_list = [ 'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC' ]
ind_period = where( period EQ month_list)

IF ind_period EQ -1 THEN BEGIN
  ;IF period EQ 'DJFM' THEN ind_period = [0,1,2,11]
  IF period EQ 'JFM'  THEN ind_period = [0,1,2]
  IF period EQ 'FMA'  THEN ind_period = [1,2,3]
  IF period EQ 'MAM'  THEN ind_period = [2,3,4]
  IF period EQ 'JJA'  THEN ind_period = [5,6,7]
  IF period EQ 'JA'   THEN ind_period = [6,7]
  IF period EQ 'JJAS' THEN ind_period = [5,6,7,8]
  IF period EQ 'AMJJ' THEN ind_period = [3,4,5,6]
  IF period EQ 'SONDJF' THEN IF data_type NE 'c1m' THEN ind_period = [8,9,10,11,12,13]   ELSE ind_period = [8,9,10,11,0,1]
  IF period EQ 'NDJFMA' THEN IF data_type NE 'c1m' THEN ind_period = [10,11,12,13,14,15] ELSE ind_period = [10,11,0,1,2,3]
  IF period EQ 'DJFMA'  THEN IF data_type NE 'c1m' THEN ind_period = [11,12,13,14,15]    ELSE ind_period = [11,0,1,2,3]
  IF period EQ 'ALL'  OR period EQ 'J2D' THEN ind_period = INDGEN(12)
ENDIF
;print, period, ind_period

IF ind_period[0] EQ -1 THEN STOP
