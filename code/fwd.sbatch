#!/bin/bash
#SBATCH --constraint="ib"
#SBATCH --exclusive
#SBATCH --job-name=fwd
#SBATCH --output=logs/fwd.log
#SBATCH --mem-per-cpu=2000

LOG=logs/fwd.log
echo `date` >> $LOG
Rscript code/fwd.R >> $LOG
echo "done @" `date` >> $LOG
