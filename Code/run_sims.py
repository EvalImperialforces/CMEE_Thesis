#!usr/bin/env python3
""" Run simulations """
__author__= 'Eva Linehan (eva.linehan18@imperial.ac.uk)'
__version__ = 0.01
__date__ = 'Aug 2019'

import single_sim
import pandas as pd
import itertools

animals = ["reef", "nurse", "whale_shark", "ray", "manatee", "booby", "frigate", "tern"]
animal_path = ["straight", "stop25", "stop50", "stop75", "random60", "random120", "random180"]
camera = ['garmin', 'sony', 'nadir']
uav_path = ["lawnmower", "figure8"]
speed = [1, 2]
bias = ["yes", "no"]
colnames = ['Flight', 'UAV_path', 'Camera', 'Speed', 'Animal', 'Animal_path', 'Bias', 'Total_hits', 'Actual_hits']

combo = [animals, animal_path, camera, uav_path, speed, bias] 
table_data = list(itertools.product(*combo)) # Every combination of paramater variables

idx = ['{}'.format(i) for i in range(1, len(table_data)+1)] # Index
colnames = ['Animal', 'Animal_path', 'Camera', 'UAV_path', 'Speed', 'Bias'] # Colnames
df = pd.DataFrame(table_data, index=idx, columns=colnames)

#test = map(single_sim.simulation(), df[1:5,])
test = df.apply(single_sim.simulation, axis = 0, args=[6])

#single_sim