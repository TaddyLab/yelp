#!/bin/bash
#SBATCH --partition=taddy
#SBATCH --job-name=rf
#SBATCH --output=logs/rf.log
#SBATCH --mem-per-cpu=8000
#SBATCH --exclusive

LOG=logs/rf.log
echo `date` >> $LOG
Rscript code/rf.R >> $LOG
echo "done @" `date` >> $LOG
