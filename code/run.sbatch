#!/bin/bash
## run with `sbatch -N[#machines] -J[jobname] code/run.sbatch'
#SBATCH --time=36:0:0
#SBATCH --mem-per-cpu=2000
#SBATCH --output=%j.err
#SBATCH --constraint=ib
#SBATCH --exclusive

PENWEIGHT=10 # this controls weight of penalty on variables of interest vs controls

module load parallel

OUT=results/$SLURM_JOB_NAME/${SLURM_JOBID}
mkdir -p $OUT-data
mkdir -p $OUT-log

LOG=$OUT-log/an_sbatch.log
echo `date` > $LOG
echo "$SLURM_NNODES nodes" >> $LOG

export para="parallel -j $SLURM_NNODES --joblog $OUT-log/para.log"
export srun="srun --exclusive -N1 -c16 --ntasks=1"  

cp code/build.R $OUT-log/.
cp code/mn-*.R $OUT-log/.

$para $srun "Rscript code/mn-fit.R {1} $PENWEIGHT > $OUT-log/r{1}.log" ::: {1..128}

Rscript code/mn-combine.R > $OUT-log/combine.log

echo "done @" `date` >> $LOG


