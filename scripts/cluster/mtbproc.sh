#!/bin/bash
#SBATCH --job-name='mtbproc'
#SBATCH --output=./logs_p/%x_%A.log
#SBATCH --time=02:00:00 
#SBATCH --chdir='.'
#SBATCH --mem=10G 
#SBATCH --mail-type=ALL

eval "$(conda shell.bash hook)"
conda activate mtb-env

Rscript mtbproc_hpc.R
