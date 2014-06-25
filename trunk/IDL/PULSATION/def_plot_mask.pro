; DEFINE MASK TITLE

IF force_landmask THEN mask_title = 'OCEAN'
IF force_seamask  THEN mask_title = 'LAND'
IF force_seamask       AND force_highmask     THEN mask_title = 'LANDLOW'
IF force_landmask EQ 0 AND force_seamask EQ 0 THEN mask_title = 'OCEAN+LAND'
