PRO calcul_ohc_CPL
@all_cm


; parametres
exp_name = 'GIOV2km'
exp_path = '/homelocal-px/px-126/gsamson/WORK/AROME/TEST_CPL/EXPS_'+exp_name+'/'
exp_list = FILE_SEARCH( exp_path + 'EXP*CPL*/IVAN12_gridT_00H-96H_6h.nc' )
nb_exp   = n_elements(exp_list)
cp = 4.1855 ; water heat capacity (J.g-1.degC-1)


FOR m = 0, nb_exp-1 DO BEGIN

  ; init
  exp_file = exp_list[m] & help, exp_file
  initncdf, exp_file

  ; lecture
  t = REFORM( read_ncdf( 'votemper', /all, filename=exp_file, /nostruct)) & help, t
  s = REFORM( read_ncdf( 'vosaline', /all, filename=exp_file, /nostruct)) & help, s

  ; mask & density
  t[where(t LT 26.)] = !values.f_nan
  s[where(t LT 26.)] = !values.f_nan
  r = rhon(temporary(s),t) & help, r

  ; calcul
  ohc26 = fltarr(nxt, nyt, jpt) + !values.f_nan & help, ohc26
  dept26 = fltarr(nxt, nyt, jpt) + !values.f_nan & help, dept26
  FOR l = 0, jpt-1 DO BEGIN
    ohc26[*,*,l] = cp * moyenne( r[*,*,*,l] * (t[*,*,*,l]-26.), 'z', /integration, /nan) / 10000.
    FOR j = 0, nyt-1 DO BEGIN
      FOR i = 0, nxt-1 DO BEGIN
	indok = where(finite(t[i,j,*,l]) EQ 1)
	IF indok[0] NE -1 THEN dept26[i,j,l] = gdept[max(indok, /nan)]
      ENDFOR
    ENDFOR
  ENDFOR

  ; netcdf
  fid = NCDF_OPEN(exp_file, /WRITE)
  xid = NCDF_DIMID(fid,'x')
  yid = NCDF_DIMID(fid,'y')
  tid = NCDF_DIMID(fid,'time_counter')
  NCDF_CONTROL, fid, /REDEF  
  vid1  = NCDF_VARDEF(fid,  'ohc26', [xid, yid, tid], /FLOAT)
  vid2  = NCDF_VARDEF(fid, 'dept26', [xid, yid, tid], /FLOAT)
  NCDF_CONTROL, fid, /ENDEF
  NCDF_VARPUT, fid, vid1, ohc26
  NCDF_VARPUT, fid, vid2, dept26
  NCDF_CLOSE, fid
  print, 'ECRITURE NETCDF OK!' & print, ''

ENDFOR

END
