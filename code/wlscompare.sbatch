#!/bin/bash
#SBATCH -N32
#SBATCH --job-name=wlscompare
#SBATCH --output=logs/%j.err
#SBATCH --constraint=ib
#SBATCH --exclusive

module load parallel

LD=logs/$SLURM_JOB_NAME-$SLURM_JOBID
mkdir -p $LD
LOG="$LD/sbatch.log"
echo `date` > $LOG
echo "$SLURM_NNODES nodes" >> $LOG

export para="parallel -j $SLURM_NNODES --joblog $LD/para.log"
export srun="srun --exclusive -N1 -c16 --ntasks=1"  

rm results/wlscompare.txt

$para $srun "Rscript code/wlscompare.R {1} > $LD/r{1}.log" ::: {1..64}

echo "done @" `date` >> $LOG
