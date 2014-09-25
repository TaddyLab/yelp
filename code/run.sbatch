#!/bin/bash
## run with `sbatch -N[#machines] -J[jobname] code/run.sbatch'
#SBATCH --time=36:0:0
#SBATCH --mem-per-cpu=2000
#SBATCH --output=%j.err
#SBATCH --constraint=ib
#SBATCH --exclusive

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
cp code/$SLURM_JOB_NAME-*.R $OUT-log/.

$para $srun "Rscript code/$SLURM_JOB_NAME-fit.R {1} > $OUT-log/r{1}.log" ::: {1..128}

Rscript code/$SLURM_JOB_NAME-combine.R > $OUT-log/combine.log

echo "done @" `date` >> $LOG


