#!/bin/bash

# Remove/create directories
rm -r logs logs_q mtb ari
mkdir logs logs_q mtb ari

sbatch mtbinf_queue.sh
