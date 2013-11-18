#!/bin/bash
#SBATCH -N32
#SBATCH --job-name=ylpboot
#SBATCH --output=logs/%j.err
#SBATCH --mem-per-cpu=2000
#SBATCH --constraint="ib"
#SBATCH --exclusive

module load parallel

LD=logs/$SLURM_JOB_NAME-$SLURM_JOBID
mkdir -p $LD
LOG="$LD/sbatch.log"
echo `date` > $LOG
echo "$SLURM_NNODES nodes" >> $LOG

export para="parallel -j $SLURM_NNODES --joblog $LD/para.log"
export srun="srun --exclusive -N1 -c16 --ntasks=1"  

rm -rf data/snowboot
mkdir data/snowboot 

$para $srun "Rscript code/bootstrap.R {1} > $LD/r{1}.log" ::: {1..64}

echo "done IR @" `date` >> $LOG

$srun Rscript code/bootfwd.R > $LD/fwd.log

echo "done @" `date` >> $LOG



