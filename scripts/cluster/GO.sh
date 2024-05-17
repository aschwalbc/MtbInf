#!/bin/bash

# Remove/create directories
rm -r logs logs_q mtb
mkdir logs logs_q mtb

sbatch mtbinf_queue.sh
