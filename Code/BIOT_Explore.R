#!/usr/bin/env Rscript
# Author: Eva Linehan
# Date: April 2019
# Desc: BIOT Flight Analysis


#clear environments
rm(list=ls())

library(dplyr)
library(tidyr)
library(anytime)


biot <- read.csv("../Data/Metadata/BIOT_2018.csv", header = T)
# Grounded stills removed from original dataset
biot <- subset(biot, Flight.or.Grounded == 'Flight')

maldives <- subset(biot, Country == "Maldives")


################## Flight Alignement (.fit and metadata) #######################

# Cannot match times and so will match on number of images and GPS coordinates

# Flight 1
F1 <- read.csv('../Data/Flight_data/Flight1/Flight1.csv', header = T)
F3 <- read.csv('../Data/Flight_data/Flight3_Flight4/Flight3.csv', header = T)
F4 <- read.csv('../Data/Flight_data/Flight3_Flight4/Flight4.csv', header = T)
F5 <- read.csv('../Data/Flight_data/Flight5/Flight5.csv', header = T)
F6 <- read.csv('../Data/Flight_data/Flight6/Flight6.csv', header = T)
F9 <- read.csv('../Data/Flight_data/Flight9/Flight9.csv', header = T)
F10 <- read.csv('../Data/Flight_data/Flight10/Flight10.csv', header = T)


file_comparison <- function(csv, index){
  # Check if number of images are the same for each flight in the metadata file and .fit file
  metafile <- original_biot %>% group_by(Flight) %>% summarise(Number_of_images = length(Flight))
  check1 <- metafile$Number_of_images[index] # Index doesn't match flight number as flight 2 is missing
  print(check1)
  check2 <- length(csv$Index)
  print(check2)
  if (check1 == check2){
    return(print("True"))
  } else { print("False, flights are not of same length")}
}

#file_comparison(F1, 1) # Match
#file_comparison(F3, 2) # Match
file_comparison(F4, 3) # More .fit photos
file_comparison(F5, 4) # More .fit photos but just 1 off!!
file_comparison(F6, 5) # More .fit photos but just 1 off!!
file_comparison(F9, 8) # More .fit photos
file_comparison(F10, 9) # More metadata photos



first_image_match <- function(flight_no, .fit){
  # Check index of both files where first image may match
  .fit <- subset(.fit, select = c("Lat", "Long"))
  meta <- subset(biot, biot$Flight == flight_no)
  image <- c(meta$Lat[1], meta$Long[1])
  #print(image)
  counter = 0
  for(i in .fit$Long){
    counter = counter + 1
    #print(i)
    #print(image1[1])
    #print(image1[2])
    if(isTRUE(all.equal(i, image[2]))){
      #print("Matched Longitudes")
      #print(counter)
      #print(flight1csv$Lat[counter])
      if (isTRUE(all.equal(.fit$Lat[counter], image[1]))){
        print(paste0("Image matched at index ", counter))
      }
    }
  }
}

first_image_match(1, F1) # Match at index 649 with subsequent GPS coordinates matching upon visual inspection
first_image_match(3, F3) # Match at index 439 with subsequent GPS coordinates matching upon visual inspection
first_image_match(4, F4) # Match at index 1 with subsequent GPS coordinates matching upon visual inspection
first_image_match(5, F5) # Match at index 1 with subsequent GPS coordinates matching upon visual inspection
first_image_match(6, F6) # Match at index 650 but in comparison to metadata file, should take index 674 as beginning
first_image_match(9, F9) # Match at index 240 but in comparison to metadata file, should take index 441 as beginning (441 - 464?)
first_image_match(10, F10) # Match at index 901 but in comparison to metadata file, should take index 918 as beginning 



final_image_match <- function(flight_no, .fit){
  # Check coordinates of both files where the last image matches
  .fit <- subset(.fit, select = c("Lat", "Long"))
  meta <- subset(biot, biot$Flight == flight_no)
  Lat <- tail(meta$Lat, n=1)
  Long <- tail(meta$Long, n=1)
  image <- c(Lat, Long)
  print(image)
  counter = 0
  for(i in .fit$Long){
    counter = counter + 1
    #print(i)
    #print(image1[1])
    #print(image1[2])
    if(isTRUE(all.equal(i, image[2]))){
      #print("Matched Longitudes")
      #print(counter)
      #print(flight1csv$Lat[counter])
      if (isTRUE(all.equal(.fit$Lat[counter], image[1]))){
        print(paste0("Image matched at index ", counter))
      }
    }
  }
}


final_image_match(1, F1) # Match at 1077
final_image_match(3, F3) # Match at 1172 - 1177, according to metadata it should be 1173
final_image_match(4, F4) # Match at 1048
final_image_match(5, F5) # Match at 3386
final_image_match(6, F6) # Match at 1998
final_image_match(9, F9) # Index 464
final_image_match(10, F10) # Index 1232


# Subset .fit files to match photos

Flight1 <- F1[649:1077,]
Flight3 <- F3[439:1173,]
Flight4 <- F4[1:1048,]
Flight5 <- F5[1:3386,] # 1 more than total_count but checks out?
Flight6 <- F6[674:1998,] # 1 more than total_count but checks out?
Flight9 <- F9[441:464,]
Flight10 <- F10[918:1232,] # 1 more than total_count but checks out?


# Check against basemap
# So matching up the cropped .fits (according to the in-flight images) and comparing against cropped track on basemap,
# Flight 1, 4, 6 and 10 look strange.
# Comparing distance, we are not way off. I would be more inclined to rely on calculated estimates because of the decimal places in the metafile.
# Flight 3, 4 and 5 (by 0.6km) are close between calculation and .fit.


# Flight 1: map track looks incorrect, distance off by 900m
# Flight 3: map track good, distance correct
# Flight 4: map track ok, distance correct
# Flight 5: map track good, distance correct
# Flight 6: map track iffy overlap, distance doubled
# Flight 9: map track ok, distance ok (off 3m)
# Flight 10: map track looks incorrect, distance off by 40m


######### .TLOGS #########

T1 <- read.csv('../Data/BIOT_TLOGS/GPS_csvs/2018-06-06_07-20-31.csv', header = T)
T2 <- read.csv('../Data/BIOT_TLOGS/GPS_csvs/2018-06-06_23-21-32.csv', header = T)
T3 <- read.csv('../Data/BIOT_TLOGS/GPS_csvs/2018-06-07_01-09-51.csv', header = T)
T4 <- read.csv('../Data/BIOT_TLOGS/GPS_csvs/2018-06-07_03-00-55.csv', header = T)
T5 <- read.csv('../Data/BIOT_TLOGS/GPS_csvs/2018-06-07_03-09-47.csv', header = T)
T6 <- read.csv('../Data/BIOT_TLOGS/GPS_csvs/2018-06-07_04-21-11.csv', header = T)
T7 <- read.csv('../Data/BIOT_TLOGS/GPS_csvs/2018-06-07_14-35-39.csv', header = T)
T8 <- read.csv('../Data/BIOT_TLOGS/GPS_csvs/2018-06-09_12-48-53.csv', header = T)
T9 <- read.csv('../Data/BIOT_TLOGS/GPS_csvs/2018-06-10_04-13-36.csv', header = T)
T10 <- read.csv('../Data/BIOT_TLOGS/GPS_csvs/2018-06-11_00-36-00.csv', header = T) 

M1 <- read.csv('../Data/BIOT_TLOGS/Maldive_MissingLogs/2016-11-28_03-51-15.csv', header = T) 
M2 <- read.csv('../Data/BIOT_TLOGS/Maldive_MissingLogs/2016-11-29_02-26-20.csv', header = T) 

# Calulate average alttude

alt_hist <- function(TLOG){
  # Covert mm to m and then construct hist
  alts <- c(TLOG$GLOBAL_POSITION_INT.alt)
  alts_m <- sapply(alts, function(x) x/1000)
  alts_m <- alts_m[alts_m > 0 ]
  #hist(alts_m)
  #Average <- sum(alts_m)/length(alts_m)
  return(alts_m)
}

T1_alt <- alt_hist(T1) # U shape past 30
#alt_hist(T2) # Discount (all in negative)
T3_alt <- alt_hist(T3) # 20 up to 50 with bar out at 100
#T4_alt <- alt_hist(T4) # Bellcurve between 1 and 5 m
#alt_hist(T5) # Discount (most in negative)
T6_alt <- alt_hist(T6) # U from 0 to over 100
#alt_hist(T7) # Discount (most in negative)
T8_alt <- alt_hist(T8) # U from 10 to 16
T9_alt <- alt_hist(T9) # U from -20 to over 80
T10_alt <- alt_hist(T10) # Most under 20 but up to 100


Alt_df <- list(T1_alt, T3_alt, T4_alt, T6_alt, T8_alt, T9_alt, T10_alt)
Average_Altitude <- do.call(sum, Alt_df)/(5913 + 6097 + 1605 + 8948 + 3578 + 4294) # Integers refer to total number of entries
# Average 50m

M1_alt <- alt_hist(M1)
M2_alt <- alt_hist(M2)
m_alt <- list(M1_alt, M2_alt)
Maldive_Average_alt <- do.call(sum, m_alt)/(6770+7705)
# Average 77.5m

# Convert unix timestamp to date and time
# See can you match any of the logs by date to .fit files or metadata

T1$timestamp <- anytime(T1$timestamp) # 06-06-18 12:20 - 13:03
T2$timestamp <- anytime(T2$timestamp) # 07-06-18 04:21 - 05:52
T3$timestamp <- anytime(T3$timestamp) # 07-06-18 06:09 - 06:20
T4$timestamp <- anytime(T4$timestamp) # 07-06-18 08:00 - 08:09
T5$timestamp <- anytime(T5$timestamp) # 07-06-18 08:09 - 08:28
T6$timestamp <- anytime(T6$timestamp) # 07-06-18 09:21 - 10:21
T7$timestamp <- anytime(T7$timestamp) # 07-06-18 14:35 - 15:15
T8$timestamp <- anytime(T8$timestamp) # 09-06-18 12:48 - 13:19
T9$timestamp <- anytime(T9$timestamp) # 10-06-18 04:13 - 05:31
T10$timestamp <- anytime(T10$timestamp) # 11-06-18 05:36 - 06:13

M1$timestamp <- anytime(M1$timestamp)
M2$timestamp <- anytime(M2$timestamp)

# Convert UTC to UTC+5 for the Maldives

m1 <- as.POSIXct(M1$timestamp, "Indian/Maldives")
m2 <- as.POSIXct(M2$timestamp, "Indian/Maldives")

M1 <- cbind(M1, "UTC+5"=format(m1, tz="Indian/Maldives"))


# So UTC+5 appears to match timestamp according to that from Maldive photo proprties. 
# However, this timestamp differs from metadata timestamp and not enough photos to fix this (2262 metadata images vs. 716 actual images)

Distance_from_TLOG <- function(flight_no){
  df <- maldives %>% group_by(Flight == flight_no)
  start_time <- c(df$Time[1])
}

####################### Distance ############################

# Rough Distance Calculation for Maldives from TLOGS

# According to mission planner, M1 started 8:59:10 and finished 9:36:54
# Transect started 09:00 and finish 09:28ish - obvious gaps in between

# According to mission planner, M2 started 7:30:31 and finished 8:15:56
# Transect start 7:32 and finish 8:09ish


# From metadata file, duration should be as such;

Maldive_count <- maldives %>%
  group_by(Flight) %>% 
  tally(c(TotalElasmo, TotalBirds, FruitBat, Human)) 

Duration <- maldives %>% 
  group_by(Flight) %>%
  summarise(Duration = length(Flight))


Maldive_count <- cbind(Maldive_count, Duration$Duration)

Duration_minutes <- lapply(Duration, function(x) {x/60})
Maldive_count <- cbind(Maldive_count, Duration_minutes$Duration)

# Animal speeds

# Maximum based on blue shark: 24mph - 10.72m/s (http://www.elasmo-research.org/education/topics/r_haulin'_bass.htm)
# Average based on blue shark cruise speed: 17mph - 7.6m/s
# Lowest based on large shark cruise speed: 1.5 mph - 0.7m/s
# Maximum ray speed is 24km/h - 6.6m/s (https://en.wikipedia.org/wiki/Reef_manta_ray#cite_ref-16)
# Airspeed for 138 bird species 8 - 23m/s but gulls and terns in lower part of this range between 8 and 15? (Alerstam,07