#!usr/bin/env python3
""" Run Simulations on gREM UAV model """
__author__= 'Eva Linehan (eva.linehan18@imperial.ac.uk)'
__version__ = 0.01
__date__ = 'July 2019'

import random
import numpy as np
from scipy.stats import truncnorm
import math

# Simulation fixed conditions;

# Sampling time: 1600 timesteps to reflect 20 minute flight, T, in seconds
# Survey area: 7500m x 7500m, camera skirts perimeter to avoid going outside
# Sensor detection zone: Based on camera model (different for Maldives and Belize) 


# Random variables:

# Survey Effort

# UAV path (Lawn-mower/ Figure 8/ Random Walk)
# FOV (BIOT/ Maldives)
# UAV speed (lowest speed-average/ average-max speed)

# Animal availability

# Animal movement model (Straight Line/ Stop-Start/ Random Walk)
# Animal speed (1-average/ average to max?)
# Availability bias (None/Sharks/Rays/Birds)


# gREM model def

D = z / a

# D = Density
# z = number of capture instances/ encounters 
# (presence or absence, so 3 terns and 4 boobies in one photo will 7)
# A = Area to reflect 2vrt (2rv = area covered by sensor per unit time), while t = time



# Initialise random seed depending on the loop
for i in range(1,100+1):
    random.seed(i)
    print("Random seed is", i)



def hit_evaluation (UAV_coord, Animal_coord):
    pass



def detection_zone (Camera):
    # FOV calculated per camera model in Strip_Sampling_Density.

    camdict = {"garmin": [83,113], "sony": [113, 75]}
    try:
        return camdict[camera.lower()]
    except KeyError:
        raise KeyError ('Invalid camera model used, should be :'{}.format([x for x in camdict.keys()]))


def get_truncated_normal(mean=0, sd=1, low=0, upp=10):
    # More intuitive method to use truncnorm for speed_profile distribution

    return truncnorm(
        (low - mean) / sd, (upp - mean) / sd, loc=mean, scale=sd)



def speed_profile (level):
    # Speed profile for random sample distribution
    # Average speed: 18 m/s 
    # Level 1 = low, Level 2 = high
    
    speed_dist = get_truncated_normal(mean = 18, sd = 2, low = 4, upp = 33)
    value = speed_dist.rvs(1)
    if level == 1:
        while value > 18:
            value = speed_dist.rvs(1)
    else:
        while value < 18:
            value = speed_dist.rvs(1)
    return value



def start_uav (Camera):
    # Start coordinate will depend on FOV of camera
    if Camera == 'Garmin':
        FOV = [83, 113] # lamda functions
        #start_coord = 
    else:
        FOV = [113,75]
    return FOV


def find_sample_points(start_coords, end_coords, angle, speed_level):

    # Retrieves coordinates for x number of timesteps between 2 points

    horiz_dist = abs(end_coord[0] - start_coord[0]) # x coordinates
    vert_dist = abs(end_coord[1] - start_coord[1]) # y coordinates
    length = math.sqrt((horiz_dist**2) + (vert_dist**2)) # length in metres between both points
    angle_radians = math.radians(angle)

    #s = speed_profile(speed_level) # sum of speeds, using index and value to add cumulatively
    #for index, value in s:
    #    s.append(speed_profile(speed_level))
    # enumerate wraps and keeps a counter at same time - for loops through list gives value and index

    total_x = 0 # Sum of all x values (should be below x value of end_coord)
    total_y = 0 # Sum of all y values (should be below x value of end_coord)
    total_length_covered = 0
    x_list = [start_coords[0]] # x value at every step
    y_list = [start_coords[1]] # y value at every step
    iteration = 0 # used to cumulatively add the last x, y values to recent

    while True:
        s = speed_profile(speed_level) # get our speed for this step
        total_length_covered += s # cumulatively add to total length covered
        if total_length_covered > length:
            break

        delta_x = x_list[iteration] + (math.cos(angle_radians)* s) # From start x coordinate, add the length with direction calculated through cosine 
        delta_y = y_list[iteration] + (math.sin(angle_radians)* s) # From start y coordinate, add the length with direction calculated through sine
        total_x += delta_x # add the new x value cumulatively
        total_y += delta_y # add the new y value cumulatively
        x_list.append(x_list[iteration] + delta_x) # adding the new coordinates cumulatively???
        y_list.append(y_list[iteration] + delta_y)

        iteration += 1


    sample_coords = zip(x_list, y_list)     # zip up to create list of tuples, each being a set of coordinates where a sample has been taken

    print(total_x)
    print(total_y)
    print(total_length_covered)
    print(sample_coords)
    return True

find_sample_points([0,0], [100,0], 90, 1)


#def simulation_flight (UAV_path, UAV_speed_profile, Camera, loops):

    # Animal moves in straight line, average speed distribution, no availability bias
    # All function inputs are strings except for loops
    # Vectorize operations in which the drone coordinates are in numpy array which change with speed and flight path
    # Flight path will only change direction of coordinates until certain x and y coordinates are reached

   


    
    
    Animal_coord = np.empty((0,2), int)
    Animal_start = random.sample(range(7500), 2) # Random start position
    
    
    
    FOV = detection_zone (Camera) # Camera must be a string
    



#def simulation_biological (Animal_path, Animal_speed_profile, Availability, loops):
    # UAV moves in lawnmower transect, average speed distribution, Garmin

