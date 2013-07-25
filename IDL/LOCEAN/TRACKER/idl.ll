# @ environment = COPY_ALL
# @ wall_clock_limit = 05:00:00
# @ as_limit   = 4gb
# @ job_name   = IDL
# @ output     = $(job_name)_$(jobid).out
# @ error      =  $(job_name)_$(jobid).out
# @ notification = error
# @ queue

#!/bin/bash
set -x
module load idl

idl idl.pro # -args $prog
