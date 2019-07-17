#!usr/bin/env python3
""" Get GPS Coordinates from Images """
__author__= 'Eva Linehan (eva.linehan18@imperial.ac.uk)'
__version__ = 0.01
__date__ = 'May 2019'



from GPSPhoto import gpsphoto
import pandas as pd
import numpy as np
import csv
from os import listdir
from os.path import isfile, join
import re
from PIL import Image
import exifread

def getGPS(mypath,image):
    '''
    returns gps and altitude data from images
    '''
    image_location = mypath+image
    data = gpsphoto.getGPSData(image_location) # Lat, Long and Altitude data
    return data 


mypath = '../Data/Flight_data/GPS_Images/' # Path to folder containing images to be processed
files = [f for f in listdir(mypath) if isfile(join(mypath, f))] # List of image names

for image in files:
    print('Extracting information from image', image)
    data = getGPS(mypath,image) # Compile image details
    df = pd.DataFrame(data, index = [0]) # Generate dataframe
    df['Image'] = image # Add image ID
    df['Image'] = df['Image'].map(lambda x: x.rstrip('.JPG')) # Remove '.JPG', similar to metadata file
    with open("../Data/Flight_data/Missing_gps.csv", "a") as file:
        df.to_csv(file, mode='a', header=file.tell()==0) # Append to .csv file
  

# Match images between Missing_gps.csv and metadata file and append missing info
# Load Missing_gps data with index as image column
df = pd.read_csv("../Data/Flight_data/Missing_gps.csv", index_col=4)
df = df.dropna(0) # Drop Maldive images with no GPS data
# Remove altitude and index or 'Unnamed' column
df.drop('Altitude', axis=1, inplace = True)
df.drop('Unnamed: 0', axis=1, inplace = True)

# Load BIOT metadata
df2 = pd.read_csv("../Data/Metadata/BIOT_NOGROUND_2018.csv")

# Convert missing gps data to dictionary
df.to_dict(orient='records') 

# Match dict key with image in BIOT metadata and input dict values for lat and long
for key, value in df.iterrows():
        df2.loc[df2['Image'] == key, 'Lat'] = value[0]
        df2.loc[df2['Image'] == key, 'Long'] = value[1]

# Export
df2.to_csv("../Data/BIOT_NOGROUND_ADD7_8.csv")


# Extract correct timestamp from Maldives images

Maldive_path = '../Data/Flight_data/MALDIVES/'
folder = [f for f in listdir(Maldive_path) if isfile(join(Maldive_path, f))]

for picture in folder:
        print('Extracting timestamp from Maldive image', picture)
        path = Maldive_path + picture
        with open(path, 'rb') as f:
                # Extracts just timstamp info
                info = exifread.process_file(f, stop_tag="EXIF DateTimeOriginal")
                Tag = info["EXIF DateTimeOriginal"]
                Picture = picture.replace('.JPG', '') # Remove .JPG 
                mylist = [(Picture, Tag)] # Round brackets helps df array
                headers = ['Image', 'Time']
                df = pd.DataFrame(mylist, columns = headers)
                with open("../Data/BIOT_TLOGS/Maldive_Times.csv", "a") as file:
                        df.to_csv(file, mode='a', header=file.tell()==0) # Append to .csv file
                

       
# Match images between Maldive_Times.csv and metadata file and append missing info
# Load Maldive_Times data with index as image column
mal1 = pd.read_csv("../Data/BIOT_TLOGS/Maldive_Times.csv") # Index col is Image
mal1.drop('Unnamed: 0', axis=1, inplace = True)

# Load BIOT metadata
MALD = pd.read_csv("../Data/Metadata/BIOT_NOGROUND_2018.csv")
MALD = MALD[MALD.Country == 'Maldives']

# Convert missing gps data to dictionary
mal1.to_dict(orient='records') 

# Match dict key with image in Maldive metadata and input dict values for time
for key, value in mal1.iterrows():
        print(key)
        MALD.loc[MALD['Image'] == key, 'Timecheck'] = value[0]
        #df2.loc[df2['Image'] == key, 'Long'] = value[1]

# Check all images have been matched
MALD.count() # 372 image matches for Timecheck
mal1.Image.nunique() # 715 unique images but not all matching up 

# Export
MALD.to_csv("../Data/Metadata/Maldives.csv")



