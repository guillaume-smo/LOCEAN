; INIT ERAI MASK FILE
grid_file = 'mask_ERAI.nc'
initncdf, path+grid_file, glam=[20,380] & domdef, box, /MEMEINDICES


; SEAMASK + LANDMASK
seamask = read_ncdf('lsm', filename=path+grid_file, /ALLRECORDS, /NOSTRUCT)
IF exp_name EQ 'ERAI-NEMO' OR exp_name EQ 'ERAI-WRF' THEN seamask = REVERSE(read_ncdf('lsm', filename=path+grid_file, /ALLRECORDS, /NOSTRUCT),2)
landmask = seamask
landmask[where(FIX(seamask) EQ 1)] = !VALUES.F_NAN
landmask[where(FIX(seamask) EQ 0)] = 1.
seamask[ where(FIX(seamask) EQ 0)] = !VALUES.F_NAN


; HIGHMASK
highmask = read_ncdf('z', filename=path+'surfgeopot_ERAI.nc', /ALLRECORDS, /NOSTRUCT) / 9.81
orog = highmask
highmask[where(landmask EQ 1)] = 0.
highmask[where(highmask GE highmask_height)] = !VALUES.F_NAN
highmask[where(highmask LT highmask_height)] = 1.
help, landmask, seamask, highmask


; REINIT DATA FILE
initncdf, path+file, glam=[20,380] & domdef, box
gdept = REVERSE(gdept, /OVERWRITE)
e3t   = REVERSE(e3t,   /OVERWRITE)


IF debug THEN STOP
