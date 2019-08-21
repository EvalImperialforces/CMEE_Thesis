#!usr/bin/env python3
""" Single simuation run, all functions from jupyter notebook """
__author__= 'Eva Linehan (eva.linehan18@imperial.ac.uk)'
__version__ = 0.01
__date__ = 'Aug 2019'


import numpy as np
import pandas as pd
import random
from scipy.stats import truncnorm
import scipy.stats as stats
import statistics
import math


##### Speed profiles

def get_truncated_normal(mean=0, sd=1, low=0, upp=10):
    # More intuitive method to use truncnorm for speed_profile distribution

    return truncnorm(
        (low - mean) / sd, (upp - mean) / sd, loc=mean, scale=sd)




def speed_profile (level = 0, mean = 18, sd = 1, low = 4, upp = 33):
    # Speed profile for random sample distribution
    # Average speed: 18 m/s 
    # Level 1 = low, Level 2 = high
    
    speed_dist = get_truncated_normal(mean, sd, low, upp)
    value = speed_dist.rvs(size=1) # Select 1 random number from this distribution
    
    if level == 1:
        while value > mean:
            value = speed_dist.rvs(size=1)


    elif level == 2:
        while value < mean:
            value = speed_dist.rvs(size=1)

    else:
        value = ('You have not selected correct uav level, must be 1 or 2', 0)
    
    return value[0]
        
def animal_speed (animal):
    # Speed distribution is based on crude figures for each animal taxa
    
    speed_dict = {"reef": [0.35, 0.64, 1.23, 0.22], "nurse": [0.21, 0.37, 0.59, 0.1], "whale_shark" : [0.09, 0.6, 1.06, 0.24], "ray": [0.046, 1.42, 2.51, 0.5], 
                  "manatee": [0.06, 0.7, 1.14, 0.27], "booby": [2.7, 10.5, 22, 4.82], "frigate": [2.7, 4.5, 5.1, 0.6], "tern": [7, 8.5, 10, 0.75]}
    
    try:
        x =  speed_dict[animal.lower()]
        mean = x[1]
        upp = x[2]
        sd = x[3]
        low = x[0]
        speed_dist = get_truncated_normal(mean, sd, low, upp)
        value = speed_dist.rvs(size=1)
        return value[0]
    
    except KeyError:
        raise KeyError ('Invalid animal species, please try again')



#### Movement functions


def round_up(n, decimals=2):
    multiplier = 10 ** decimals
    return math.ceil(n * multiplier) / multiplier

def get_speed(individual, level = 0):
    
    # Speed retrieved depending on which object we are simulating
    # individual = string of uav or animal

    if individual == 'uav':
        s = speed_profile(level)
    else:
        s = animal_speed(individual)
        
    return s


def find_sample_points(start_coord, end_coord, angle, individual, speed_level = 0, probability = 0, timestep = 10000):

    # Retrieves coordinates for x number of timesteps between 2 points
    
    # start_coord = start point of animal or drone
    # end_coord = end point of animal or drone
    # angle = angle of travel in degrees
    # individual = animal or drone?
    # speed_level = low(1) or high(2)
    # probability = probability of animal remaining stationary in that timestep(default = 0)
    
    
    horiz_dist = abs(end_coord[0] - start_coord[0]) # x coordinates
    vert_dist = abs(end_coord[1] - start_coord[1]) # y coordinates
    length = math.sqrt((horiz_dist**2) + (vert_dist**2)) # length in metres between both points
    angle_radians = math.radians(angle)

    
    total_length_covered = 0
    x_list = [start_coord[0]] # list of x values at every step
    y_list = [start_coord[1]] # list of y values at every step
    iteration = 0 # used to cumulatively add the last x, y values to recent

    while True:
        s = get_speed(individual, speed_level) # get our speed for this step
        total_length_covered += s # cumulatively add to total length covered
        #print(iteration)
        if total_length_covered > length or iteration > timestep:
            break
        
        point_probability = np.random.choice(100,1) # assign probability to coordinate
        #print(point_probability)
        if point_probability > probability:
            delta_x = x_list[iteration] + (math.cos(angle_radians)* s) # From start x coordinate, add the length with direction calculated through cosine 
            delta_y = y_list[iteration] + (math.sin(angle_radians)* s) # From start y coordinate, add the length with direction calculated through sine
          
            x = round_up(delta_x)
            y = round_up(delta_y)
            
            x_list.append(x) 
            y_list.append(y)
        
        else:
            total_length_covered = total_length_covered - s # Take last distance as we have not travelled here
            x_list.append(x_list[iteration]) 
            y_list.append(y_list[iteration])

        iteration += 1


    sample_coords = list(zip(x_list, y_list))     # zip up to create list of tuples, each being a set of coordinates where a sample has been taken
    
    
    sample_coords = sample_coords[0:timestep]
    
    
    return sample_coords 



def detection_zone(camera):
    # FOV calculated per camera model in Strip_Sampling_Density.
    # [Height, width]
    
    camdict = {"garmin": [83,113], "sony": [113, 75], "nadir": [164, 109]}
    try:
        return camdict[camera.lower()]
    except KeyError:
        raise KeyError ('Invalid camera model used, should be :{}'.format([x for x in camdict.keys()]))



def boundary_coords_uav(camera, boundary_length = 5000):
    
    # Boundary_length (m)
    # x and y coordinates 
    # coord 1 = bottom righthand side (0,0)
    # coord 2 = bottom lefthand side (7500,0)
    # coord 3 = top lefthand side (7500, 7500)
    # coord 4 = top righthand side (0, 7500)
    
    area = detection_zone(camera)
    half_area = [x/2  for x in area]
    boundary = {'coord_1' : [(0 + half_area[1]), (0 + half_area[0])],
                'coord_2' : [((boundary_length) - half_area[1]), (0 + half_area[0])],
                'coord_3' : [((boundary_length) - half_area[1]), ((boundary_length) - half_area[0])],
                'coord_4' : [(0 + half_area[1]), ((boundary_length) - half_area[0])]}
    
    return boundary




#### UAV Paths



def figure_8 (camera, speed_level=1, timestep = 10000):
    
    # Figure 8 path simulation which will depend on boundary coords from camera model 
    # Returns vector of coords from simulated flight
    
    # camera is string variable - garmin or sony
    
    bc = boundary_coords_uav(camera)
    
    track = []
    angle = []
    
    while True :
        # Keep figure 8 going to survey completion
        leg_1 = find_sample_points(bc['coord_1'], bc['coord_2'], 360, 'uav', speed_level)
        leg_2 = find_sample_points(bc['coord_2'], bc['coord_4'], 135, 'uav', speed_level)
        leg_3 = find_sample_points(bc['coord_4'], bc['coord_3'], 360, 'uav', speed_level)
        leg_4 = find_sample_points(bc['coord_3'], bc['coord_1'], 225, 'uav', speed_level)

        track = track + leg_1 + leg_2 + leg_3 + leg_4 
        
        ang = [np.repeat(360, len(leg_1)),  np.repeat(135, len(leg_2)), np.repeat(360, len(leg_3)), np.repeat(225, len(leg_4))]
        angle.append(ang)
        
        if len(track) > timestep:
            break
    
    
    angle_list = []
    for sublist in angle:
        for item in sublist:
            for thing in item:
                angle_list.append(thing)
    
    track = track[0:timestep]
    angle_list = angle_list[0:timestep]
    
    return track, angle_list


def lawn_shift (last_coord, camera):
    
    # Lawnmower shifts up across the virtual environment
    
    detect_area = detection_zone(camera)
    shift = detect_area[0] # Width of camera detection zone
    new_coord = [last_coord[0], last_coord[1] + shift]
    
    return new_coord


def lawnmower (camera, speed_level = 1, timestep = 10000):
    
    # Figure 8 path simulation which will depend on boundary coords from camera model 
    # Returns vector of coords from simulated flight
    
    bc = boundary_coords_uav(camera)
    
    point1 = bc['coord_1'] # right hand side coord
    x1 = point1[0] # x value of coord that does not change throughout the changong lawn_shifts
    
    point2 = bc['coord_2'] # left hand side coord
    x2 = point2[0] # x value of coord that does not change throughout the changong lawn_shifts
    
    point3 = bc['coord_3']
    y_limit = point3[1]
    #print(y_limit)
    
    y_shifts = [0] # y value at each new shift
    right_list = []
    left_list = []
    
    while True:
        right_coord = [x1, y_shifts[-1]]
        left_coord = [x2, y_shifts[-1]]
        
        right_list.append(right_coord)
        left_list.append(left_coord)
        
        shift_coord = lawn_shift(right_coord, camera) 
        new_y = shift_coord[1]
        
        if new_y <= y_limit:
            y_shifts.append(new_y)
        else: break
    
    right_list.pop(0) # Remove first y value which is 0
    left_list.pop(0)
    
    points = [[(0,0)],[(x1,0)]] # rough starting point for vertical shift coords to be added, two added because of iteration
    shift_points = []

    counter = 0
    
    for i in range(0, len(right_list)):
        #print(i)
        if i % 2 == 1:
            #print('i is odd')
            degree = 180
            leg = find_sample_points(left_list[i], right_list[i], degree, 'uav', speed_level)
         

        elif i % 2 == 0:
            #print('i is even')
            degree = 360
            leg = find_sample_points(right_list[i], left_list[i], degree, 'uav', speed_level)
            
          
        points.append(leg)
        counter += len(leg)
        shift = find_sample_points(points[i+1][-1], leg[0], 90, 'uav', speed_level) 
        shift_points.append(shift)
            
        if counter >= timestep:
            break
        
    if(camera == 'nadir'):
        # nadir always goes comes to the end before 10k timesteps so add another leg
        # i.e bring it back to the start
        leg = find_sample_points(right_list[0], left_list[0], 360, 'uav', speed_level)
        #print(leg)
        points.append(leg)
        shift = find_sample_points([0,0], [110.5,154], 90, 'uav', speed_level)
        shift_points.append(shift)
        shift2 = find_sample_points([4889.5, 154], [4889.5, 308], 90, 'uav', speed_level)
        shift_points.append(shift2)
        #print(shift2)
        leg2 = find_sample_points(left_list[1], right_list[1], 180, 'uav', speed_level)
        #print(leg2)
        points.append(leg2)
        shift3 = find_sample_points([110.5, 308],[110.5, 462], 90, 'uav', speed_level)
        shift_points.append(shift3)
        leg3 = find_sample_points(right_list[2], left_list[2], 360, 'uav', speed_level)
        points.append(leg3)
        shift4 = find_sample_points([4889.5, 462],[4889.5, 616], 90, 'uav', speed_level)
        shift_points.append(shift4)
        leg4 = find_sample_points(left_list[3], right_list[3], 180, 'uav', speed_level)
        points.append(leg4)
        shift5 = find_sample_points([110.5, 616],[110.5, 770], 90, 'uav', speed_level)
        shift_points.append(shift5)
        leg5 = find_sample_points(right_list[4], left_list[4], 360, 'uav', speed_level)
        points.append(leg5)
        shift6 = find_sample_points([4889.5, 770],[4889.5, 924], 90, 'uav', speed_level)
        shift_points.append(shift6)
        leg6 = find_sample_points(left_list[5], right_list[5], 180, 'uav', speed_level)
        points.append(leg6)
        shift7 = find_sample_points([110.5, 924],[110.5, 1078], 90, 'uav', speed_level)
        shift_points.append(shift7)
        leg7 = find_sample_points(right_list[6], left_list[6], 360, 'uav', speed_level)
        points.append(leg7)
    else: pass
        
    points.pop(0) # Remove the dummy coords above
    points.pop(0)
    
    coords = []
    angle = []
    
    for i in range(0, len(points)):
        coords.append(shift_points[i])
        angle.append(np.repeat(90, len(shift_points[i])))
        coords.append(points[i])
        angle.append(np.repeat(360, len(points[i])))
        
    
    coords.pop(0) # Start from first boundary coordinate
    flat_list = []
    for sublist in coords:
        #Flatten list of lists while retaining nested lists, i.e coordinates
        for item in sublist:
            flat_list.append(item)

    flat_list = flat_list[0:timestep]
    
    angle.pop(0)
    flat2 = np.concatenate(angle)
    flat2 = flat2[0:timestep]
  
    
    return flat_list, flat2



#### Animal paths



def angle_dist(angle, size = 1):
    
    # Sampling from a uniform distribution between a chosen angle range
    
    low = -(angle)
    high = angle
    t = np.random.uniform(low = low, high = high, size = size)
    
    #plt.hist(t, bins=25, color='#75BEBB')
    #plt.xlabel('Angle (degrees)')
    #plt.ylabel('Frequency')
    #plt.xlim([low - 10, high +10])
    #plt.ylim([0, 75])
    #plt.show()
    
    return t


def random_walk (animal, start_coord, angle, timestep = 10000):
    
    # Correlated random walk for a specified animal with a start coordinate, speed profile to sample from and angle to bound uniform dist
    # x, y and then both x and y coordinates are inversed to ensure point remains within the boundary
    
    x_list = [start_coord[0]] # list of x values at every step
    y_list = [start_coord[1]] # list of y values at every step
    iteration = 0 # used to cumulatively add the last x, y values to recent
    
    
    for i in range(timestep):

            # Select angle and convert to radians
            angle_value = angle_dist(angle)
            angle_radians = math.radians(angle_value)
        
            # Select speed
            s = animal_speed(animal)
        
        
            delta_x = x_list[iteration] + (math.cos(angle_radians)* s) # From start x coordinate, add the length with direction calculated through cosine 
            delta_y = y_list[iteration] + (math.sin(angle_radians)* s) # From start y coordinate, add the length with direction calculated through sine
       
            coords = [delta_x, delta_y]
            
            test = all(i > 0 and i < 7500 for i in coords) # If out of bounds = False 
                
            if test == True: 
                #print('Correct first go:', coords)
                x = round_up(delta_x)
                y = round_up(delta_y)
            
                x_list.append(x) 
                y_list.append(y)
                
                iteration += 1
                
            else: 
                # If out of bounds reverse the x, then the y and then both x and y until you are back within the bounds
                #print('Coords before change:', coords)
                counter = 0
                while test == False:
                    counter = counter + 1
                    if counter == 1:
                        new_x = x_list[iteration] - (math.cos(angle_radians)* s)
                        coords = [new_x, delta_y]
                        #print('Coords first change:', coords)
                        test = all(i > 0 and i < 7500 for i in coords)
                    if counter == 2:
                        new_y = y_list[iteration] - (math.sin(angle_radians)* s)
                        coords = [delta_x, new_y]
                        #print('Coords second change:', coords)
                        test = all(i > 0 and i < 7500 for i in coords)
                    if counter == 3:
                        coords = [new_x, new_y]
                        #print('Coords third change:', coords)
                        test = all(i > 0 and i < 7500 for i in coords)
                        
                    
                    #print('coords passed and being appended:', coords)
                    x = round_up(coords[0])
                    y = round_up(coords[1])
            
                    x_list.append(x) 
                    y_list.append(y)
                
                    iteration += 1
                
                
     
    sample_coords = list(zip(x_list, y_list)) # zip up to create list of tuples, each being a set of coordinates where a sample has been taken
    sample_coords = sample_coords[0:timestep]
    return sample_coords 



#### Simulation runs

def perception_bias(animal):
    # Animal availability - proportion of time spent at surface
    # All obtained from http://www.penguiness.net/thebook (Laran,17)
    # Manatee: Hagihara,14
    
    biasdict = {"reef": 0.1, "nurse": 0.1, "whale_shark" : 0.6, "ray": 0.5, 
                  "manatee": 0.7, "booby": 1, "frigate": 1, "tern": 1}
    try:
        return biasdict[animal.lower()]
    except KeyError:
        raise KeyError ('Invalid animal used, should be :{}'.format([x for x in biasdict.keys()]))




def point_distance (pt1, pt2):
    # Trigonometric distance between 2 points
    
    horiz_dist = abs(pt2[0] - pt1[0]) # x coordinates
    vert_dist = abs(pt2[1] - pt1[1]) # y coordinates
    length = math.sqrt((horiz_dist**2) + (vert_dist**2)) # length in metres between both points
    
    return length

def new_dist(dcoord, acoord, angle):
    
    # Account for shifting detction zone with uav direction
    # Transform uav to the origin and with angle, calculate absolute distance
    
    origin = [(dcoord[0] - dcoord[0]), (dcoord[1] - dcoord[1])]
    animal_coord = [(acoord[0] - dcoord[0]), (acoord[1] - dcoord[1])]
    #print(animal_coord)
    
    rotate_x = (animal_coord[0]*math.cos(angle)) - (animal_coord[1]*math.sin(angle))
    rotate_y = (animal_coord[1])*math.cos(angle) + (animal_coord[0]*math.sin(angle))  
    
    new_animal_coord = (rotate_x, rotate_y)
    #print(new_animal_coord)
    
    distance = point_distance(origin, animal_coord)
                
    return(distance)

def capture(dist_vect, animal, camera, bias = 'no'):
    # Return a list of total hits and at what timestep (list of lists)
    # Capture occurs if point lies within boundary so dist between animal and uav must be < half height and half distance
    
    
    # Specify boundary
    detect_area = detection_zone(camera)
    detect_width = detect_area[0]
    #print(detect_width)
    

    point_inbounds = [(i) for (i,el) in enumerate(dist_vect) if el < detect_width]
    if bias == 'yes' and len(point_inbounds) > 0:
        # Remove bias if specified, if not [] or [0,0]
        no_remove = int(round(len(point_inbounds)*perception_bias(animal)))
        if no_remove > 0:
            point_inbounds = point_inbounds[:-(no_remove)]
        else: pass
    else : pass
    
    return point_inbounds


def random_animal(path):
    
    if path == 1:
        # Straight paths
        point_x = random.uniform(1,4900)
        point_y = 0
        point2_x = point_x
        
    if path == 2:
        point_x = random.choice([1,4900])
        point_y = random.choice([1,4900])
        
    coord = [point_x, point_y]
    
    return coord



def simulation_animal(animal, path, timestep = 1000):
    # Output vector from simulation = 
    # S : probability of stopping
    # A: angle
    
    # Straight
    
    
    if path == 'straight':
        coord = random_animal(1)
        vector = find_sample_points([coord[0],0], [coord[0],7500], 90, animal, 0, 0, timestep)
    
    if path == 'stop25':
        coord = random_animal(1)
        vector = find_sample_points([coord[0],0], [coord[0],7500], 90, animal, 0, 0.25, timestep)
    
    if path == 'stop50':
        coord = random_animal(1)
        vector = find_sample_points([coord[0],0], [coord[0],7500], 90, animal, 0, 0.50, timestep)
    
    if path == 'stop75':
        coord = random_animal(1)
        vector = find_sample_points([coord[0],0], [coord[0],7500], 90, animal, 0, 0.75, timestep)
    
    if path == 'random60':
         coord = random_animal(2)
         vector = random_walk(animal, [coord[0],coord[1]], 60, timestep)
            
    if path == 'random120':
        coord = random_animal(2)
        vector = random_walk(animal, [coord[0],coord[1]], 120, timestep)
        
    if path == 'random180':
        coord = random_animal(2)
        vector = random_walk(animal, [coord[0],coord[1]], 180, timestep)

      
    return vector


def simulation_uav(camera, path, speed = 1, timestep = 10000):
    # Output vector for simulation
    
    if path == 'lawnmower':
        vector, angle = lawnmower(camera, speed, timestep)
        
        
    if path == 'figure8':
        vector, angle = figure_8(camera, speed, timestep)
        
    return vector, angle
    


def hit_count(vect):
    
    if len(vect) > 0:
    # If not an empty hit list
        total = len(vect)
    
        hit1 = vect[:-1] # Remove last
        hit2 = vect[1:] # Remove first
    
        count = 1
    
        for i in range(0, len(hit2)):
            if(hit2[i] - hit1[i] == 1):
                count == 1 # Remains as 1 if timesteps are ensuing
            else: 
                count += 1 # Add to count score to signifiy new individual
    
        sim_score = [total, count]
        
    else:
        sim_score = [0,0]
    
    return sim_score



def simulation (animal, animal_path, camera, uav_path, speed=1, bias = 'no', timestep = 10000):
    # Running the simulation to produce the number of hits
    
    animal_vect = simulation_animal(animal, animal_path, timestep = timestep)
    uav_vect, uav_angle = simulation_uav(camera, uav_path, speed, timestep = timestep)

    
    coords_table = pd.DataFrame(list(zip(animal_vect, uav_vect, uav_angle)))
    
    dist_vect = coords_table.iloc[:,].apply(lambda x: new_dist(x[1], x[0], x[2]), axis=1)
    
    hits = capture(dist_vect, animal, camera, bias = 'no')
    score = hit_count(hits)
 
    return score
