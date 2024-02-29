#!/bin/bash
#SBATCH --job-name=mtbinf
#SBATCH --output=./logs/%x_%A.log
#SBATCH --time=02:00:00
#SBATCH --account=ec232
#SBATCH --mem=1G
#SBATCH --chdir='.'

eval "$(conda shell.bash hook)"
conda activate mtb-env

# To create the iso_list.txt and rep_list.txt used in the loops below, uncomment the following line:
Rscript prep.R "ARI_rev_mix_pop.fst"

max_jobs=500

while read country
do
  while read repetition
  do

    while [ $CURRENT_JOB_N -eq $max_jobs ];
    do
      echo "Maximum number of queued jobs reached. Waiting..."
      sleep 10
      CURRENT_JOB_N=$(squeue -u $USER | wc -l)
      CURRENT_JOB_N=$((CURRENT_JOB_N - 1))
    done

    echo "Running script for country: $country and rep: $repetition"
    sbatch run_ind_script.sh $country $repetition

    CURRENT_JOB_N=$(squeue -u $USER | wc -l)
    CURRENT_JOB_N=$((CURRENT_JOB_N - 1))

  done < rep_list.txt
done < iso_list.txt
