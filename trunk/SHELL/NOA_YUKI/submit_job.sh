#!/bin/bash
set -x


# PROFILS YUKI:
#-----------------------------------------------------------
# - "ARO4km+NEMO OPT"
# - "ARO4km+NEMO DBG"
# - "ARO2km+NEMO OPT"
# - "ARO2km+NEMO DBG"
# - "ARO4km OPT"
# - "ARO2km OPT"
# - "ARO4km DBG"
# - "ARO4km DBG"
# - "NEMO"
PROFIL="ARO2km OPT"


# GLOBAL PARAMETERS:
#-----------------------------------------------------------
EXP_NAME=ECUME		#   nom experience
INI_DATE=2010-02-15     #  date START
INI_HOUR=18             # heure START (00 / 06 / 12 / 18)
RST_DATE=2010-02-15     #  date RESTART
FIN_DATE=2010-02-19     #  date END (00H)
RUN_TIME=24             # duree du run en heures (1h, 24h, 48h, ...)
RUN_MODE=AROME		# CPL / NEMO / AROME
CPL_FREQ=15             # frequence de couplage en minutes (mode CPL only)
RST_AUTO=1              # restart automatique ( rst_auto x run_time )
#-----------------------------------------------------------

# AROME PARAMETERS:
#-----------------------------------------------------------
CFG_AROM=GELA2km	# configuration AROME ( IVAN4km / IVAN2km / GIOV2km / FELL2km / BING2km )
NAM_AROM=namel_previ_dyn_off_${CFG_AROM}_${RUN_MODE}_PC
NAM_SURF=EXSEG1.nam_ECUME
BDY_FREQ=6              # frequence forcage lateral atmospherique en heures
TSTEP=60                # pas de temps en secondes (dtmax(4km)=90s, dtmax(2km)=60s)
NSTOP=h${RUN_TIME}      # h1 = t60
NCONF=001
VERSION=meteo
ADVEC=sli
MODEL=aladin
AROME_BIN=MASTER_DEBUG	# MASTER_DEBUG
#-----------------------------------------------------------

# NEMO PARAMETERS:
#-----------------------------------------------------------
CFG_NEMO=IVAN12         # configuration NEMO (IVAN12)
NAM_NEMO=namelist-${CFG_NEMO}_${RUN_MODE}
BDY_FORC=GLORYS2V3      # conditions initiales + obc : PSY3V3R1 ou GLORYS2V1
ATM_FORC=ALADIN         # forcage atmospherique (mode "NEMO") : ECMWF ou ALADIN
NEMO_TSTEP=450          # pas de temps en secondes (450s max)
# nombre de pas de temps du restart initial (after spinup+DFI)
INIKT=$(ncdump -v kt /cnrm/gc/mrgf/samson/INPUTS_NEMO/${CFG_NEMO}/${BDY_FORC}/RESTART/restart_$(echo $INI_DATE | sed -e s/-//g)H${INI_HOUR}.nc | grep 'kt =' | cut -d ' ' -f4)

#-----------------------------------------------------------

# OASIS PARAMETERS:
#-----------------------------------------------------------
CFG_OASIS=IVAN2km_IVAN12
#-----------------------------------------------------------


#PBS HEADER
sed -n '1,8p' pbs_list.txt > job.pbs
case $PROFIL in
  "ARO4km+NEMO OPT") sed -n '17,21p' pbs_list.txt >> job.pbs ;;
  "ARO4km+NEMO DBG") sed -n '27,31p' pbs_list.txt >> job.pbs ;; 
  "ARO2km+NEMO OPT") sed -n '37,41p' pbs_list.txt >> job.pbs ;;
  "ARO2km+NEMO DBG") sed -n '47,51p' pbs_list.txt >> job.pbs ;;
  "ARO4km OPT"     ) sed -n '57,61p' pbs_list.txt >> job.pbs ;;
  "ARO2km OPT"     ) sed -n '67,71p' pbs_list.txt >> job.pbs ;;
  "ARO4km DBG"     ) sed -n '77,81p' pbs_list.txt >> job.pbs ;;
  "ARO2km DBG"     ) sed -n '87,91p' pbs_list.txt >> job.pbs ;;
  "NEMO"           ) sed -n '67,71p' pbs_list.txt >> job.pbs ;;
esac
sed -n '17,62p' submit_job.sh >> job.pbs
cat job_noheader.pbs >> job.pbs
sed -i -e s"/INIKT=.*/INIKT=$INIKT/"g job.pbs


# SUBMIT PBS JOB
qsub job.pbs &> job.tmp


# SAVE PBS JOB FOR RESTART
FICPATH=/work/samson/TEST_CPL/FICDIR
JOBID=$( cat job.tmp | cut -d" " -f2 | cut -d"." -f1 )
TMPPATH=/work/samson/TEST_CPL/TMPDIR/JOB_$JOBID

mkdir -p $TMPPATH
cp -f ${FICPATH}/job.pbs ${TMPPATH}/job.pbs
rm -f ${FICPATH}/job.tmp

exit
