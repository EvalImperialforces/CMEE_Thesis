#!/bin/bash
#Author:eva.EL1718@imperial.ac.uk
#Script: GPS_from_TLOGS.sh
#Desc: Get GPS coordinates from .tlog files using mavlogdump.py
#Date: May 2019

# mavlogdump.py documentation: http://www.ardusub.com/operators-manual/logging.html

filename=../Data/BIOT_TLOGS/$(basename "$1" .tlog).csv
echo "Extracting GPS coordinates from $1 to $filename..."
mavlogdump.py --types=GLOBAL_POSITION_INT --format=csv $1 > $filename
# Note type=GPS_RAW_INT is not always used but may show more information concerning GPS errors

mv ../Data/BIOT_TLOGS/*.csv ../Data/BIOT_TLOGS/GPS_csvs