PRO aladin_netcdf2nemo
@all_cm

date     = '201301'
file     = '/homelocal-px/px-126/gsamson/WORK/ALADIN/OPER_ASSIM_ECH6/'+date+'/ICMSHALAD+0006_'+date+'_v2.nc'
output   = '/homelocal-px/px-126/gsamson/WORK/ALADIN/OPER_ASSIM_ECH6/'+date+'/ICMSHALAD+0006_'+date+'_NEMO_v2.nc'
dom_ivan = [45.5000,68.0000,-21.7110,-9.21008]

initncdf, file
;domdef, dom_ivan
juld = ncdf_gettime(file) & help, juld
print, 'PERIOD READ :', jul2date(juld[0]), jul2date(juld[jpt-1]), f='(A13, 2(1X,F11.2))'
print, 'PERIOD CHECK:', jul2date(juld[0]), jul2date(juld[0]+(jpt-1)*0.25d), f='(A13, 2(1X,F11.2))'


; MASK + BIG_MASK
print, '' & print, 'CUISINE MASQUES...'
MASK = read_ncdf('IND.TERREMER', /allrecord, filename=file, /nostruct)
indlake = where((gphit GE -17.) AND (gphit LE -9.) AND (glamt GE 34.) AND (glamt LE 38.))
MASK = MASK[*,*,0] & MASK[indlake] = 1.
MASK[where(MASK EQ 1.)] = !VALUES.F_NAN & MASK[where(MASK EQ 0.)] = 1.

bigmask = mask
FOR l = 1, n_elements(gphit[0,firstyt:lastyt])-2 DO BEGIN
  FOR k = 1, n_elements(glamt[firstxt:lastxt,0])-2 DO BEGIN
    IF finite(mask[k,l]) EQ 0. THEN BEGIN
      bigmask[k-1,l-1] = !VALUES.F_NAN
      bigmask[k-1,l  ] = !VALUES.F_NAN
      bigmask[k-1,l+1] = !VALUES.F_NAN
      bigmask[k,l-1]   = !VALUES.F_NAN
      bigmask[k,l+1]   = !VALUES.F_NAN
      bigmask[k+1,l-1] = !VALUES.F_NAN
      bigmask[k+1,l  ] = !VALUES.F_NAN
      bigmask[k+1,l+1] = !VALUES.F_NAN
    ENDIF
  ENDFOR
ENDFOR
tmpmask = bigmask
FOR l = 1, n_elements(gphit[0,firstyt:lastyt])-2 DO BEGIN
  FOR k = 1, n_elements(glamt[firstxt:lastxt,0])-2 DO BEGIN
    IF finite(bigmask[k,l]) EQ 0. THEN BEGIN
      tmpmask[k-1,l-1] = !VALUES.F_NAN
      tmpmask[k-1,l  ] = !VALUES.F_NAN
      tmpmask[k-1,l+1] = !VALUES.F_NAN
      tmpmask[k,l-1]   = !VALUES.F_NAN
      tmpmask[k,l+1]   = !VALUES.F_NAN
      tmpmask[k+1,l-1] = !VALUES.F_NAN
      tmpmask[k+1,l  ] = !VALUES.F_NAN
      tmpmask[k+1,l+1] = !VALUES.F_NAN
    ENDIF
  ENDFOR
ENDFOR
bigmask = temporary(tmpmask)
bigmask = replicate_array(bigmask, jpt) & help, bigmask
mask = replicate_array(mask, jpt) & help, mask
MASK[where(finite(MASK) EQ 0.)] = 0.
BIGMASK[where(finite(BIGMASK) EQ 0.)] = 0.
DIFFMASK = MASK[*,*,0]*0. & DIFFMASK[where((BIGMASK[*,*,0] EQ 0.) AND (MASK[*,*,0] EQ 1.))] = 1.
print, 'OK' & print, ''


; VARIABLES
print, '' & print, 'LECTURE VARIABLES...'
MSLP   = reform(read_ncdf('MSLP_SEA',filename=file, /nostruct, /allrecord));*BIGMASK	              ; Pa
T2M    = reform(read_ncdf('T2M',filename=file, /nostruct, /allrecord)-273.15);*BIGMASK		      ; degC
RH2M   = reform(read_ncdf('RH2M',filename=file, /nostruct, /allrecord));*BIGMASK		      ; %
SST    = reform(read_ncdf('SST',filename=file, /nostruct, /allrecord)-273.15);*BIGMASK		      ; degC
U10M   = reform(read_ncdf('ZON10M',filename=file, /nostruct, /allrecord));*BIGMASK		      ; m/s
V10M   = reform(read_ncdf('MER10M',filename=file, /nostruct, /allrecord));*BIGMASK		      ; m/s
TAUX   = reform(read_ncdf('TENS.TURB.ZO',filename=file, /nostruct, /allrecord)/(6*3600.));*BIGMASK     ; N/m^2
TAUY   = reform(read_ncdf('SURFTENS.TURB.ME',filename=file, /nostruct, /allrecord)/(6*3600.));*BIGMASK ; N/m^2
SWDOWN = reform(read_ncdf('SURFRAYT SOLA DE',filename=file, /nostruct, /allrecord)/(6*3600.));*BIGMASK ; W/m^2 >0
LWDOWN = reform(read_ncdf('SURFRAYT THER DE',filename=file, /nostruct, /allrecord)/(6*3600.));*BIGMASK ; W/m^2 >0
LWNET  = reform(read_ncdf('FLU.RAY.THER',filename=file, /nostruct, /allrecord)/(6*3600.));*BIGMASK ; W/m^2 >0
SWNET  = reform(read_ncdf('FLU.RAY.SOLA',filename=file, /nostruct, /allrecord)/(6*3600.));*BIGMASK ; W/m^2 >0
PRECIP = reform(read_ncdf('PREC.EAU.CON',filename=file, /nostruct, /allrecord) + $
                read_ncdf('PREC.EAU.GEC',filename=file, /nostruct, /allrecord))/(6*3600.);*BIGMASK ; mm/s <0
EVAP   = reform(read_ncdf('FLU.MEVAP.EA',filename=file, /nostruct, /allrecord)/(6*3600.))*(-1.);*BIGMASK ; mm/s >0
LHF    = reform(read_ncdf('FLU.LAT.MEVA',filename=file, /nostruct, /allrecord)/(6*3600.));*BIGMASK     ; W/m^2
SHF    = reform(read_ncdf('FLU.CHA.SENS',filename=file, /nostruct, /allrecord)/(6*3600.));*BIGMASK     ; W/m^2

print, 'CHECK PRECIP > 0 : ', min(precip*mask, /nan), max(precip*mask, /nan)
print, 'CHECK   EVAP < 0 : ', min(  evap*mask, /nan), max(  evap*mask, /nan)
STOP
EMP    = -1. * (EVAP + PRECIP)                                                                                 ; mm/s
print, 'CHECK EMP : ', min(emp*mask, /nan), max(emp*mask, /nan)
print, 'OK' & print, ''


; TESTS EXTRAPOLATION MSLP
;indok = where(BIGMASK EQ 1.)
;ijok  = array_indices([jpi,jpj], indok, /DIMENSIONS)
;iok   = reform(ijok[0,*])
;jok   = reform(ijok[1,*])
;tmp   = MSLP[*,*,0]
;MSLP_int = GRID_TPS(glamt[indok], gphit[indok], tmp[indok], NGRID=[jpi,jpj])
;MSLP_int = MIN_CURVE_SURF(tmp[indok], glamt[indok], gphit[indok], XPOUT=glamt, YPOUT=gphit, /SPHERE)
;MSLP_int = TRI_SURF( tmp[indok], glamt[indok], gphit[indok], NX=jpi, NY=jpj, MISSING=!VALUES.F_NAN)


; EXTRAPOLATION SUR LES CONTINENTS
;jpt=3
print, '' & print, 'EXTRAPOLATION SPATIALE...'
FOR l = 0, jpt-1 DO BEGIN
  print, l, ' /', jpt-1

  MSLP[*,*,l] = EXTRAPSMOOTH(MSLP[*,*,l], BIGMASK[*,*,0])
  MSLP[*,*,l] = SMOOTH(MSLP[*,*,l], 3, /NAN)

  tmp1 = T2M[*,*,l] & tmp2 = tmp1[where(diffmask EQ 1.)] & tmp1 = EXTRAPSMOOTH(T2M[*,*,l], BIGMASK[*,*,0]) & tmp1[where(diffmask EQ 1.)] = tmp2 & T2M[*,*,l] = tmp1
  tmp1 = RH2M[*,*,l] & tmp2 = tmp1[where(diffmask EQ 1.)] & tmp1 = EXTRAPSMOOTH(RH2M[*,*,l], BIGMASK[*,*,0]) & tmp1[where(diffmask EQ 1.)] = tmp2 & RH2M[*,*,l] = tmp1
  tmp1 = SST[*,*,l] & tmp2 = tmp1[where(diffmask EQ 1.)] & tmp1 = EXTRAPSMOOTH(SST[*,*,l], BIGMASK[*,*,0]) & tmp1[where(diffmask EQ 1.)] = tmp2 & SST[*,*,l] = tmp1
  tmp1 = U10M[*,*,l] & tmp2 = tmp1[where(diffmask EQ 1.)] & tmp1 = EXTRAPSMOOTH(U10M[*,*,l], BIGMASK[*,*,0]) & tmp1[where(diffmask EQ 1.)] = tmp2 & U10M[*,*,l] = tmp1
  tmp1 = V10M[*,*,l] & tmp2 = tmp1[where(diffmask EQ 1.)] & tmp1 = EXTRAPSMOOTH(V10M[*,*,l], BIGMASK[*,*,0]) & tmp1[where(diffmask EQ 1.)] = tmp2 & V10M[*,*,l] = tmp1
  tmp1 = TAUX[*,*,l] & tmp2 = tmp1[where(diffmask EQ 1.)] & tmp1 = EXTRAPSMOOTH(TAUX[*,*,l], BIGMASK[*,*,0]) & tmp1[where(diffmask EQ 1.)] = tmp2 & TAUX[*,*,l] = tmp1
  tmp1 = TAUY[*,*,l] & tmp2 = tmp1[where(diffmask EQ 1.)] & tmp1 = EXTRAPSMOOTH(TAUY[*,*,l], BIGMASK[*,*,0]) & tmp1[where(diffmask EQ 1.)] = tmp2 & TAUY[*,*,l] = tmp1
  tmp1 = SWDOWN[*,*,l] & tmp2 = tmp1[where(diffmask EQ 1.)] & tmp1 = EXTRAPSMOOTH(SWDOWN[*,*,l], BIGMASK[*,*,0]) & tmp1[where(diffmask EQ 1.)] = tmp2 & SWDOWN[*,*,l] = tmp1
  tmp1 = LWDOWN[*,*,l] & tmp2 = tmp1[where(diffmask EQ 1.)] & tmp1 = EXTRAPSMOOTH(LWDOWN[*,*,l], BIGMASK[*,*,0]) & tmp1[where(diffmask EQ 1.)] = tmp2 & LWDOWN[*,*,l] = tmp1
  tmp1 = LWNET[*,*,l] & tmp2 = tmp1[where(diffmask EQ 1.)] & tmp1 = EXTRAPSMOOTH(LWNET[*,*,l], BIGMASK[*,*,0]) & tmp1[where(diffmask EQ 1.)] = tmp2 & LWNET[*,*,l] = tmp1
  tmp1 = SWNET[*,*,l] & tmp2 = tmp1[where(diffmask EQ 1.)] & tmp1 = EXTRAPSMOOTH(SWNET[*,*,l], BIGMASK[*,*,0]) & tmp1[where(diffmask EQ 1.)] = tmp2 & SWNET[*,*,l] = tmp1
  tmp1 = EMP[*,*,l] & tmp2 = tmp1[where(diffmask EQ 1.)] & tmp1 = EXTRAPSMOOTH(EMP[*,*,l], BIGMASK[*,*,0]) & tmp1[where(diffmask EQ 1.)] = tmp2 & EMP[*,*,l] = tmp1
  tmp1 = LHF[*,*,l] & tmp2 = tmp1[where(diffmask EQ 1.)] & tmp1 = EXTRAPSMOOTH(LHF[*,*,l], BIGMASK[*,*,0]) & tmp1[where(diffmask EQ 1.)] = tmp2 & LHF[*,*,l] = tmp1
  tmp1 = SHF[*,*,l] & tmp2 = tmp1[where(diffmask EQ 1.)] & tmp1 = EXTRAPSMOOTH(SHF[*,*,l], BIGMASK[*,*,0]) & tmp1[where(diffmask EQ 1.)] = tmp2 & SHF[*,*,l] = tmp1
  tmp1 = PRECIP[*,*,l] & tmp2 = tmp1[where(diffmask EQ 1.)] & tmp1 = EXTRAPSMOOTH(PRECIP[*,*,l], BIGMASK[*,*,0]) & tmp1[where(diffmask EQ 1.)] = tmp2 & PRECIP[*,*,l] = tmp1
ENDFOR
print, 'OK' & print, ''


; EXTRACTION TEMPORELLE
MSLP = MSLP[*,*,0:jpt-2]
T2M = T2M[*,*,0:jpt-2]
RH2M = RH2M[*,*,0:jpt-2]
SST = SST[*,*,0:jpt-2]
U10M = U10M[*,*,0:jpt-2]
V10M = V10M[*,*,0:jpt-2]
; !!! FLUX CUMULES ENTRE T->T+1 STOCKES A T+1 = FLUX MOYEN A T+1/2  !!!
; !!! FLUX MOYEN A T+1/2 ASSOCIE AUX VARIABLES A T DANS NEMO
TAUX = TAUX[*,*,1:jpt-1]
TAUY = TAUY[*,*,1:jpt-1]
SWDOWN = SWDOWN[*,*,1:jpt-1]
LWDOWN = LWDOWN[*,*,1:jpt-1]
SWNET = SWNET[*,*,1:jpt-1]
LWNET = LWNET[*,*,1:jpt-1]
EMP = EMP[*,*,1:jpt-1]
LHF = LHF[*,*,1:jpt-1]
SHF = SHF[*,*,1:jpt-1]
PRECIP = PRECIP[*,*,1:jpt-1]


; DIAGS
UV10M  = SQRT(U10M^2+V10M^2)
TAUM   = SQRT(TAUX^2+TAUY^2)
QNS    = SHF + LHF + LWNET
QTOT   = QNS + SWNET


; ECRITURE NETCDF
print, '' & print, 'ECRITURE NETCDF...'
fid = NCDF_CREATE(output, /CLOBBER)
xid = NCDF_DIMDEF(fid, 'lon', jpi)
yid = NCDF_DIMDEF(fid, 'lat', jpj)
tid = NCDF_DIMDEF(fid, 'time', /UNLIMITED)
vid1 = NCDF_VARDEF(fid, 'lon', [xid], /FLOAT)
vid2 = NCDF_VARDEF(fid, 'lat', [yid], /FLOAT)
vid3 = NCDF_VARDEF(fid, 'nav_lon', [xid,yid], /FLOAT)
vid4 = NCDF_VARDEF(fid, 'nav_lat', [xid,yid], /FLOAT)
vid5 = NCDF_VARDEF(fid, 'somslpre', [xid,yid,tid], /FLOAT)
vid6 = NCDF_VARDEF(fid, 'sotemair', [xid,yid,tid], /FLOAT)
vid7 = NCDF_VARDEF(fid, 'sohumrel', [xid,yid,tid], /FLOAT)
vid8 = NCDF_VARDEF(fid, 'sst', [xid,yid,tid], /FLOAT)
vid9 = NCDF_VARDEF(fid, 'sowinu10', [xid,yid,tid], /FLOAT)
vid10 = NCDF_VARDEF(fid, 'sowinv10', [xid,yid,tid], /FLOAT)
vid11 = NCDF_VARDEF(fid, 'wind10m', [xid,yid,tid], /FLOAT)
vid12 = NCDF_VARDEF(fid, 'sozotaux', [xid,yid,tid], /FLOAT)
vid13 = NCDF_VARDEF(fid, 'sometauy', [xid,yid,tid], /FLOAT)
vid14 = NCDF_VARDEF(fid, 'taum', [xid,yid,tid], /FLOAT)
vid15 = NCDF_VARDEF(fid, 'sosudosw', [xid,yid,tid], /FLOAT)
vid16 = NCDF_VARDEF(fid, 'sosudolw', [xid,yid,tid], /FLOAT)
vid17 = NCDF_VARDEF(fid, 'emp', [xid,yid,tid], /FLOAT)
vid18 = NCDF_VARDEF(fid, 'lhf', [xid,yid,tid], /FLOAT)
vid19 = NCDF_VARDEF(fid, 'shf', [xid,yid,tid], /FLOAT)
vid20 = NCDF_VARDEF(fid, 'qns', [xid,yid,tid], /FLOAT)
vid21 = NCDF_VARDEF(fid, 'sowaprec', [xid,yid,tid], /FLOAT)
vid22 = NCDF_VARDEF(fid, 'qtot', [xid,yid,tid], /FLOAT)
vid23 = NCDF_VARDEF(fid, 'qsr', [xid,yid,tid], /FLOAT)


NCDF_ATTPUT,  fid, vid1, 'name', 'longitude 1D'
NCDF_ATTPUT,  fid, vid2, 'name', 'latitude 1D'
NCDF_ATTPUT,  fid, vid3, 'name', 'longitude 2D'
NCDF_ATTPUT,  fid, vid4, 'name', 'latitude 2D'
NCDF_ATTPUT,  fid, vid5, 'name', 'sea surface pressure'
NCDF_ATTPUT,  fid, vid6, 'name', '2m air temperature'
NCDF_ATTPUT,  fid, vid7, 'name', '2m air reltive humidity'
NCDF_ATTPUT,  fid, vid8, 'name', 'sea surface temperature'
NCDF_ATTPUT,  fid, vid9, 'name', '10m wind x-component'
NCDF_ATTPUT,  fid, vid10, 'name', '10m wind y-component'
NCDF_ATTPUT,  fid, vid11, 'name', '10m wind module'
NCDF_ATTPUT,  fid, vid12, 'name', 'surface stress x-component'
NCDF_ATTPUT,  fid, vid13, 'name', 'surface stress y-component'
NCDF_ATTPUT,  fid, vid14, 'name', 'surface stress module'
NCDF_ATTPUT,  fid, vid15, 'name', 'surface downward shortwave flux'
NCDF_ATTPUT,  fid, vid16, 'name', 'surface downward longwave flux'
NCDF_ATTPUT,  fid, vid17, 'name', 'net upward freshwater flux'
NCDF_ATTPUT,  fid, vid18, 'name', 'turbulent latent heat flux'
NCDF_ATTPUT,  fid, vid19, 'name', 'turbulent sensible heat flux'
NCDF_ATTPUT,  fid, vid20, 'name', 'net non-solar heat flux'
NCDF_ATTPUT,  fid, vid21, 'name', 'precipiation'
NCDF_ATTPUT,  fid, vid22, 'name', 'surface net heat flux '
NCDF_ATTPUT,  fid, vid23, 'name', 'net radiative flux '

NCDF_ATTPUT,  fid, vid1, 'units', 'degrees_east'
NCDF_ATTPUT,  fid, vid2, 'units', 'degrees_north'
NCDF_ATTPUT,  fid, vid3, 'units', 'degrees_east'
NCDF_ATTPUT,  fid, vid4, 'units', 'degrees_north'
NCDF_ATTPUT,  fid, vid5, 'units', 'Pa'
NCDF_ATTPUT,  fid, vid6, 'units', 'degC'
NCDF_ATTPUT,  fid, vid7, 'units', '%'
NCDF_ATTPUT,  fid, vid8, 'units', 'degC'
NCDF_ATTPUT,  fid, vid9, 'units', 'm.s-1'
NCDF_ATTPUT,  fid, vid10, 'units', 'm.s-1'
NCDF_ATTPUT,  fid, vid11, 'units', 'm.s-1'
NCDF_ATTPUT,  fid, vid12, 'units', 'N.m-2'
NCDF_ATTPUT,  fid, vid13, 'units', 'N.m-2'
NCDF_ATTPUT,  fid, vid14, 'units', 'N.m-2'
NCDF_ATTPUT,  fid, vid15, 'units', 'W.m-2'
NCDF_ATTPUT,  fid, vid16, 'units', 'W.m-2'
NCDF_ATTPUT,  fid, vid17, 'units', 'mm.s-1'
NCDF_ATTPUT,  fid, vid18, 'units', 'W.m-2'
NCDF_ATTPUT,  fid, vid19, 'units', 'W.m-2'
NCDF_ATTPUT,  fid, vid20, 'units', 'W.m-2'
NCDF_ATTPUT,  fid, vid21, 'units', 'mm.s-1'
NCDF_ATTPUT,  fid, vid22, 'units', 'W.m-2'
NCDF_ATTPUT,  fid, vid23, 'units', 'W.m-2'
NCDF_CONTROL, fid, /ENDEF

NCDF_VARPUT,  fid, vid1, glamt[*,0]
NCDF_VARPUT,  fid, vid2, reform(gphit[0,*])
NCDF_VARPUT,  fid, vid3, glamt
NCDF_VARPUT,  fid, vid4, gphit
NCDF_VARPUT,  fid, vid5, MSLP
NCDF_VARPUT,  fid, vid6, T2M
NCDF_VARPUT,  fid, vid7, RH2M
NCDF_VARPUT,  fid, vid8, SST
NCDF_VARPUT,  fid, vid9, U10M
NCDF_VARPUT,  fid, vid10, V10M
NCDF_VARPUT,  fid, vid11, UV10M
NCDF_VARPUT,  fid, vid12, TAUX
NCDF_VARPUT,  fid, vid13, TAUY
NCDF_VARPUT,  fid, vid14, TAUM
NCDF_VARPUT,  fid, vid15, SWDOWN
NCDF_VARPUT,  fid, vid16, LWDOWN
NCDF_VARPUT,  fid, vid17, EMP
NCDF_VARPUT,  fid, vid18, LHF
NCDF_VARPUT,  fid, vid19, SHF
NCDF_VARPUT,  fid, vid20, QNS
NCDF_VARPUT,  fid, vid21, PRECIP
NCDF_VARPUT,  fid, vid22, QTOT
NCDF_VARPUT,  fid, vid23, SWNET
NCDF_CLOSE,   fid
print, 'OK' & print, ''

STOP
RETURN
END
