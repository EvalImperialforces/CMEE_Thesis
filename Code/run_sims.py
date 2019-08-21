#!usr/bin/env python3
""" Run simulations """
__author__= 'Eva Linehan (eva.linehan18@imperial.ac.uk)'
__version__ = 0.01
__date__ = 'Aug 2019'


import single_sim
import pandas as pd
import itertools
import time
import sys, os

animals = ["reef", "nurse", "whale_shark", "ray", "manatee", "booby", "frigate", "tern"]
animal_path = ["straight", "stop25", "stop50", "stop75", "random60", "random120", "random180"]
camera = ['garmin', 'sony', 'nadir']
uav_path = ["lawnmower", "figure8"]
speed = [1, 2]
bias = ["yes", "no"]
#colnames = ['Flight', 'UAV_path', 'Camera', 'Speed', 'Animal', 'Animal_path', 'Bias', 'Total_hits', 'Actual_hits']

combo = [animals, animal_path, camera, uav_path, speed, bias] 
table_data = list(itertools.product(*combo)) # Every combination of paramater variables

idx = ['{}'.format(i) for i in range(1, len(table_data)+1)] # Index
colnames = ['Animal', 'Animal_path', 'Camera', 'UAV_path', 'Speed', 'Bias'] # Colnames
df = pd.DataFrame(table_data, index=idx, columns=colnames)
#sub_df = df[:5]

total_hits = []
actual_hits = []

#start_time = time.time()
for row in df.itertuples(index=False):
    flight = single_sim.simulation(row[0], row[1], row[2], row[3], row[4], row[5], timestep = 1000)
    total_hits.append(flight[0])
    actual_hits.append(flight[1])

#print("--- %s seconds ---" % (time.time() - start_time))


hit_df = pd.DataFrame({'Total_hits': total_hits, 'Actual_hits': actual_hits})

table = pd.concat([df.reset_index(drop=True), hit_df.reset_index(drop=True)], axis=1)

#envi_val = int(os.environ["PBS_ARRAY_INDEX"])

table.to_csv('../Data/Simulations/sim_test.csv',index=False)
