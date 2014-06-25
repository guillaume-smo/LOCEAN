G  = 9.80665        ; m/s2 = gravitational acceleration
RD = 287.04         ; specific gas constant for dry air in J/(kg.K) 
GAMMA = 0.0065      ; K/m = temperature lapse rate (g/Cp for dry air)
c = 2.0/7.0


; value for water from WRF 3.3
emiss   = 0.980
stebolt = 5.6701510e-8
albedo  = 0.080

; value for water from NEMO
;emiss   = 1.
;stebolt = 5.67e-8
;albedo  = 0.066

; value for water from ERA-Interim
;albedo  = 0.07

; default flags value
flag_z    = 0
flag_nemo = 0

; default var value
model     = 'wrf'
grid      = 'trop075'
