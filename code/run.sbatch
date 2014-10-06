#!/bin/bash
## run with `sbatch -N[#machines] -J[jobname] code/run.sbatch'
## `jobname' determines the result directory name, and will overwrite any existing
#SBATCH --time=36:0:0
#SBATCH --mem-per-cpu=2000
#SBATCH --output=%j.err
#SBATCH --constraint=ib
#SBATCH --exclusive

SCRIPT="cv" # this determines the *-fit and *-combine scripts used
PENWEIGHT=20 # this controls weight of penalty on variables of interest vs controls

module load parallel

OUT=results/$SLURM_JOB_NAME
LOG=$OUT/log/an_sbatch.log

mkdir -p $OUT
rm -rf $OUT
mkdir -p $OUT/data
mkdir -p $OUT/log

echo `date` > $LOG
echo "jobid $SLURM_JOBID running $SCRIPT" >> $LOG
echo "$SLURM_NNODES nodes" >> $LOG
echo "tau=$PENWEIGHT" >> $LOG

export para="parallel -j $SLURM_NNODES --joblog $OUT/log/para.log"
export srun="srun --exclusive -N1 -c16 --ntasks=1"  

R CMD BATCH --no-save code/build.R $OUT/log/build.Rout
cp code/$SCRIPT-*.R $OUT/log/.

$para $srun "Rscript code/$SCRIPT-fit.R {1} $PENWEIGHT > $OUT/log/r{1}.log" ::: {1..256}

Rscript code/$SCRIPT-combine.R > $OUT/log/combine.log

echo "done @" `date` >> $LOG


