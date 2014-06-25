
  computegrid,box[0], box[2], bin_size, bin_size, (box[1]-box[0]) / bin_size+1, (box[3]-box[2]) / bin_size+1
  plt, scg_density, 0, 1., /realcont, /nocont, lct=51, xtitle='', ytitle='', subtitle='CG/YEAR',title='IBTRACS CG DENSITY'
  STOP
  plt, scg_density / total(scg_density) * 100., 0, 20., /realcont, /nocont, lct=51, xtitle='', ytitle='', subtitle='',title='NORMALIZED CG DENSITY (%) '+exp_name
  STOP
  plt, stc_density, 0, 5., /realcont, /nocont, lct=51, xtitle='', ytitle='', subtitle='TC.DAY/YEAR',title='IBTRACS TC DENSITY'
  STOP
  plt, stc_density / total(stc_density) * 100., 0, 10., /realcont, /nocont, lct=51, xtitle='', ytitle='', subtitle='',title='NORMALIZED TC DENSITY (%) '+exp_name
  STOP

