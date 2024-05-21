#!/bin/bash

# Remove/create directories
rm -r logs logs_q logs_p mtb
mkdir logs logs_q logs_p mtb

sbatch mtbinf_queue.sh
