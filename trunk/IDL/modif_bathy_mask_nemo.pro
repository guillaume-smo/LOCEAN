PRO modif_bathy_mask_nemo
@all_cm

path='/home/gsamson/WORK/NESTING_TOOLS/CONFIG_IVAN12/IVAN12/'
fileo='bathy_meter_IVAN12_PSY3V3R1.nc'
filei='bathy_meter_IVAN12_PSY3V3R1_interp_SAVE.nc'
nfile='bathy_meter_IVAN12_PSY3V3R1_interp.nc'
initncdf, path+fileo

bathyo = read_ncdf('Bathymetry', filename=path+fileo, /nostruct) & help, bathyo
bathyi = read_ncdf('Bathymetry', filename=path+filei, /nostruct) & help, bathyi

masko = bathyo*0. +1. & masko[where(finite(bathyo) EQ 0)] = 0. & help, masko
maski = bathyi*0. +1. & maski[where(finite(bathyi) EQ 0)] = 0. & help, maski
newbat = bathyi & newbat[where(finite(bathyi) EQ 0)] = 0. & newbat = newbat*masko & help, newbat

index_diffmask = where(masko EQ 1. AND maski EQ 0.) & help, index_diffmask


FOR i = 0, n_elements(index_diffmask)-1 DO BEGIN

  index = neighbor(glamt[index_diffmask[i]],gphit[index_diffmask[i]],glamt[where(maski EQ 1.)],gphit[where(maski EQ 1.)])
  val = bathyi[where(maski EQ 1.)] & lon = glamt[where(maski EQ 1.)] & lat = gphit[where(maski EQ 1.)]
  newbat[index_diffmask[i]] = val[index]

;  DBG:
;  print, glamt[index_diffmask[i]], gphit[index_diffmask[i]]
;  print, val[index], lon[index], lat[index]

ENDFOR


fid = NCDF_OPEN(path+nfile, /write)
NCDF_VARPUT, fid, 'Bathymetry', newbat
NCDF_CLOSE, fid


STOP
END
