#!/bin/bash
#Author:eva.EL1718@imperial.ac.uk
#Script: GPS_from_images.sh
#Desc: Get gps coordinates from images in various files
#Date: May 2019

echo 'Creating GPS_Images folder....' 
#mkdir '../Data/DATA/GPS_Images'

echo 'Moving images from flight 7....'
#cp ../Data/DATA/Flight7/Stills -name '*.JPG' -print0 | xargs -0 mv -t ../Data/DATA/GPS_Images

echo 'Moving images from flight 8....'
#cp ../Data/DATA/Flight7_8/Stills -name '*.JPG' -print0 | xargs -0 mv -t ../Data/DATA/GPS_Images

echo 'Moving Maldive images ...'
#find ../Data/DATA/MALDIVES/Mission_1_Photos -name '*.JPG' -print0 | xargs -0 mv -t ../Data/DATA/MALDIVES/Mission_1_Photos

#find ../Data/DATA/MALDIVES/Mission_2/Photos -name '*.JPG' -print0 | xargs -0 mv -t ../Data/DATA/GPS_Images
