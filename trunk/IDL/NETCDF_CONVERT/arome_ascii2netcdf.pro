PRO arome_ascii2netcdf
COMPILE_OPT IDL2, STRICTARRSUBS


; LECTURE FICHIER ASCII-EDF
all = getfile('ASCII.2D.ICMSHFCST+0000')


; DECLARATIONS
idim = 625
jdim = 360
lon  = fltarr(idim,jdim) & help, lon
lat  = fltarr(idim,jdim) & help, lat
sft  = fltarr(idim,jdim) & help, sft
sfp  = fltarr(idim,jdim) & help, sfp
blu  = fltarr(idim,jdim) & help, blu
blv  = fltarr(idim,jdim) & help, blv
blt  = fltarr(idim,jdim) & help, blt
blr  = fltarr(idim,jdim) & help, blr
blq  = fltarr(idim,jdim) & help, blq


; REMPLISSAGE 1D->2D
ideb = 19 ; format fixe edf
ifin = n_elements(all)-1
FOR i = ideb, ifin DO BEGIN
  tmp1 = all[i]
  tmp2 = strsplit(tmp1, /extract)
  tmp3 = float(tmp2)
  lon[i-19] = tmp3[0] 
  lat[i-19] = tmp3[1]
  sft[i-19] = tmp3[2]
  sfp[i-19] = tmp3[3]
  blu[i-19] = tmp3[4]
  blv[i-19] = tmp3[5]
  blt[i-19] = tmp3[6]
  blr[i-19] = tmp3[7]
  blq[i-19] = tmp3[8]
ENDFOR


; ECRITURE NETCDF
fid = NCDF_CREATE('ICMSHFCST+0000_2D.nc', /CLOBBER)
xid = NCDF_DIMDEF(fid, 'lon', n_elements(lon[*,0]))
yid = NCDF_DIMDEF(fid, 'lat', n_elements(lat[0,*]))
vid1 = NCDF_VARDEF(fid, 'lon', [xid], /FLOAT)
vid2 = NCDF_VARDEF(fid, 'lat', [yid], /FLOAT)
vid3 = NCDF_VARDEF(fid, 'lon2D', [xid,yid], /FLOAT)
vid4 = NCDF_VARDEF(fid, 'lat2D', [xid,yid], /FLOAT)
vid5 = NCDF_VARDEF(fid, 'sft'  , [xid,yid], /FLOAT)
vid6 = NCDF_VARDEF(fid, 'sfp'  , [xid,yid], /FLOAT)
vid7 = NCDF_VARDEF(fid, 'blu'  , [xid,yid], /FLOAT)
vid8 = NCDF_VARDEF(fid, 'blv'  , [xid,yid], /FLOAT)
vid9 = NCDF_VARDEF(fid, 'blt'  , [xid,yid], /FLOAT)
vid10 = NCDF_VARDEF(fid, 'blr'  , [xid,yid], /FLOAT)
vid11 = NCDF_VARDEF(fid, 'blq'  , [xid,yid], /FLOAT)

NCDF_ATTPUT,  fid, vid1, 'units', 'degrees_east'
NCDF_ATTPUT,  fid, vid2, 'units', 'degrees_north'
NCDF_ATTPUT,  fid, vid3, 'units', 'degrees_east'
NCDF_ATTPUT,  fid, vid4, 'units', 'degrees_north'
NCDF_ATTPUT,  fid, vid5, 'units', 'K'
NCDF_ATTPUT,  fid, vid6, 'units', 'hPa'
NCDF_ATTPUT,  fid, vid7, 'units', 'm/s'
NCDF_ATTPUT,  fid, vid8, 'units', 'm/s'
NCDF_ATTPUT,  fid, vid9, 'units', 'K'
NCDF_ATTPUT,  fid, vid10, 'units', '%'
NCDF_ATTPUT,  fid, vid11, 'units', 'g/kg'
NCDF_CONTROL, fid, /ENDEF
NCDF_VARPUT,  fid, vid1, lon[*,0]
NCDF_VARPUT,  fid, vid2, reform(lat[0,*])
NCDF_VARPUT,  fid, vid3, lon
NCDF_VARPUT,  fid, vid4, lat
NCDF_VARPUT,  fid, vid5, sft
NCDF_VARPUT,  fid, vid6, sfp*100. ; Pa -> hPa
NCDF_VARPUT,  fid, vid7, blu
NCDF_VARPUT,  fid, vid8, blv
NCDF_VARPUT,  fid, vid9, blt
NCDF_VARPUT,  fid, vid10, blr
NCDF_VARPUT,  fid, vid11, blq
NCDF_CLOSE,   fid

STOP
END
