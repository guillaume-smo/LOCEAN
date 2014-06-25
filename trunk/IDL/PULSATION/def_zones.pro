
; INDIAN OCEAN
IF zone EQ 'XIE'  THEN box = [30, 105,-35,  0]
IF zone EQ 'TRIO' THEN box = [50,  70,-12, -5] ;box = [50,  90,-15, -5]

IF zone EQ 'NIO'  THEN box = [31, 119,-29, 29] ; [38, 130,-15, 35]
IF zone EQ 'BOB'  THEN box = [78, 100,  5, 25]
IF zone EQ 'SRI'  THEN box = [65,   91, 4, 15]
IF zone EQ 'GAT'  THEN box = [68, 80,  8, 21]
IF zone EQ 'ARS'  THEN box = [50, 70, 5, 30] ; [43,  80,  0, 26]
IF zone EQ 'BOX1' THEN box = [40, 80, 15, 35]
IF zone EQ 'BOX2' THEN box = [40, 80, -10, 10]
IF zone EQ 'SEI'  THEN box = [80, 120,-25,  0]
IF zone EQ 'SWI'  THEN box = [35,  90,-30,  0]
IF zone EQ 'UPW'  THEN box = [40,  75,-10, 20] ; [43,  70,  0, 26]
IF zone EQ 'IO'   THEN box = [30, 130,-25, 25] ; domaine ind025
IF zone EQ 'IOB'  THEN box = [30, 130,-30, 36]
IF zone EQ 'IND'  THEN box = [66,  96,  5, 35]
IF zone EQ 'NIND'  THEN box = [ 66,  96,  22, 35]
IF zone EQ 'SAS'  THEN box = [ 40,  80,  5, 15]
IF zone EQ 'JAVA' THEN box = [110, 130,-18, -8]
IF zone EQ 'INDPAC' THEN box = [30, 290,-30, 36]
IF zone EQ 'INDBOB' THEN box = [ 60, 100,  0, 36]
IF zone EQ 'HYM' THEN box = [ 75, 90, 20, 35]


; HOVMULLER MONSOON
IF zone EQ 'HOV'  THEN box = [ 40, 100,  0, 25]

; Monsoon Index from XAVIER et al. 2007
IF zone EQ 'XAV1' THEN box = [ 40, 100,  5, 35]
IF zone EQ 'XAV2' THEN box = [ 40, 100,-15,  5]
IF zone EQ 'XAV3' THEN box = [ 50,  95,  0, 15]

; OCI INDEX (WANG ET AL 2009)
IF zone EQ 'OCI' THEN box = [ 40, 80, 5, 15]

; AIMR = "ALL INDIAN SUMMER MONSOON RAINFALL"
IF zone EQ 'AIMR'  THEN box = [ 70,  92,  8, 40]
IF zone EQ 'AIMRR' THEN box = [ 64,  96,  5, 36]

; EIMR = "EXTENDED INDIAN SUMMER MONSOON RAINFALL" (Goswami et al. 1999)
IF zone EQ 'EIMR' THEN box = [ 70, 110, 10, 30]


; SWEN SMALL & BIG DOMAINS (SOUTH WEST PACIFIC)
IF zone EQ 'PAC'   THEN box = [130, 290,-45, 45]
IF zone EQ 'SWPS'  THEN box = [142.8336, 196.8047, -28.6197, -4.8259]
IF zone EQ 'SWPB'  THEN box = [90., 240., -40., 20.]

; other BASINS
IF zone EQ 'ALL'  THEN box = [ 20, 380,-46, 46]
IF zone EQ 'TROP' THEN box = [ 20, 380,-30, 30]
IF zone EQ 'EQUA' THEN box = [ 20, 380,-10, 10]

IF zone EQ 'ATL'  THEN box = [-70,  30,-30, 30]
IF zone EQ 'PAC'  THEN box = [130, 290,-45, 45]
IF zone EQ 'TAO'  THEN box = [130, 290,-10, 10]
IF zone EQ 'NH'   THEN box = [ 20, 380,  0, 45]
IF zone EQ 'ASIA' THEN box = [ 30, 140,  0, 40]
;IF zone EQ 'ASIA' THEN box = [ 40, 160,-20, 45]
IF zone EQ 'AFRASIA'  THEN box = [ 20, 140,-40, 40]
IF zone EQ 'ARAB'    THEN box = [ 38,  70, 10, 40] ;[ 30, 70, 15, 40]
IF zone EQ 'INDASIA' THEN box = [ 70, 110, 10, 40] ;[ 30, 70, 15, 40]


; TEST DOMAIN
IF zone EQ 'TMP' THEN box = [ 75, 90, 20, 35]
