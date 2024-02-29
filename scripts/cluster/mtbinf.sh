#!/bin/bash
#SBATCH --job-name='mtbinf'
#SBATCH --output=./logs/%x_%A.log
#SBATCH --time=00:10:00 
#SBATCH --chdir='.'
#SBATCH --mem=500M 
#SBATCH --mail-type=ALL

eval "$(conda shell.bash hook)"
conda activate mtb-env

FILE=$(echo 'mtbburden_hpc.R')
Rscript $FILE $1 $2
