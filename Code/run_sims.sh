#!/bin/bash

# Running
echo "Simulations running..."
counter=1
while [ $counter -le 10 ] 
do  
    echo "Simulation $counter in progress..."
    python3 run_sims.py $counter
    ((counter++))
done
echo "Simulations finished."
