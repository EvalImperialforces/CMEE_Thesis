#!/bin/bash
# Author: Eva Linehan el1718@ic.ac.uk
# Script: Run Simulation
# Desc: Script to run Simulations
# Date: Aug 2019

# Running
echo "Simulations running..."
counter=1
while [ $counter -le 10 ] 
do  
    echo "Simulation $counter in progress..."
    python3 run_sims.py $counter.csv
    ((counter++))
done
echo "Simulations finished."