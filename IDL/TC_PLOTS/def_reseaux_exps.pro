CASE tc_name OF
  'IVAN'     : date_list = [ '20080212H00', '20080212H06', '20080212H12', '20080212H18', '20080213H00', '20080213H06', '20080213H12', '20080213H18', '20080214H00' ]
  'GAEL'     : date_list = [ '20090203H00', '20090203H06', '20090203H12', '20090203H18', '20090204H00' ]
  'GELANE'   : date_list = [ '20100216H00', '20100216H12', '20100216H18', '20100217H00' ] ; '20100216H06' -> comblement -> viré
  'BINGIZA'  : date_list = [ '20110209H00', '20110209H06', '20110210H00' ]  ; ! NOT WORKING !
  'GIOVANNA' : date_list = [ '20120209H00', '20120209H06', '20120209H12', '20120209H18', '20120210H00' ]
  'FELLENG'  : date_list = [ '20130127H00', '20130127H06', '20130127H12', '20130127H18', '20130128H00', '20130128H06', '20130128H12' ]
ENDCASE
nb_date = n_elements(date_list) & help, nb_date
