#!/bin/bash
#SBATCH --constraint="ib"
#SBATCH -c1
#SBATCH --job-name=net
#SBATCH --output=logs/net.log
#SBATCH --mem-per-cpu=12000

LOG=logs/net.log
echo `date` >> $LOG
Rscript code/net.R >> $LOG
echo "done @" `date` >> $LOG
