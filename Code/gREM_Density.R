#!/usr/bin/env Rscript
# Author: Eva Linehan
# Date: July 2019
# Desc: Strip Sampling Density

#clear environments
rm(list=ls())

library(dplyr)
library(tidyr)
library(geosphere)
library(reshape2)
library(ggplot2)
library(ggthemes)


# Import cleaned datasets from the Data_Wrangling
source('Data_Wrangling.R')

# Import original files for duration 
original_biot <- read.csv("../Data/Metadata/BIOT/BIOT_2018.csv", header = T)
# Subset by particular flights
original_biot <- original_biot[original_biot$Flight == 1 | original_biot$Flight == 3 | original_biot$Flight == 7 | original_biot$Flight == 8 | original_biot$Flight == 10 | original_biot$Flight == 11 | original_biot$Flight == 12,  ]
original_bel1 <- read.csv("../Data/Metadata/Belize/21_02_19.csv", header = T)
original_bel1 <- cbind('Flight' = 1, original_bel1)

######################## Total count for each flight ###################################


biot_sum <- biot_lite %>%
  group_by(Flight) %>% 
  tally(c(Reefshark, Nurseshark, Whaleshark, Eagleray, Mantaray, Whitetern, 
          Sootytern, Tern_other, Redfootedboobie, Frigatebird, Brownnoddy_any, 
          Bird, FruitBat, Human))

belize_sum <- belize_lite %>%
  group_by(Flight) %>% 
  tally(c(Er1., Er2., manatee., turtle., ray., shark.))


total_count <- rbind(biot_sum, belize_sum)
# Rename Belize flights
total_count[8,1] <- 13

Species_count_biot <- biot_lite %>%
  group_by(Flight) %>% 
  summarise(Reef_shark = sum(Reefshark),
            Nurse_shark = sum(Nurseshark),
            Whale_shark = sum(Whaleshark),
            Eagleray = sum(Eagleray),
            Mantaray = sum(Mantaray),
            White_tern = sum(Whitetern),
            Sooty_tern = sum(Sootytern),
            Tern_other = sum(Tern_other),
            Redfooted_boobie = sum(Redfootedboobie),
            Frigate_bird = sum(Frigatebird),
            Brown_noddy_any = sum(Brownnoddy_any),
            Bird = sum(Bird),
            Fruit_Bat = sum(FruitBat), 
            Human = sum(Human))


######################################## Area #####################################################


################## Calculate Elapsed Time in Seconds #####################


# Original metadata file duration
# Can I use the full flight biot time on the biot_lite dataset? I would say yes because the overlap is only to fix encounter rate


Duration_per_flight <- function(df){
  time <- df %>% 
    group_by(Flight) %>%
    summarise(Duration = length(Flight))
  return(time[,2])
}


Images_biot <- Duration_per_flight(original_biot)
Images_belize <- Duration_per_flight(original_bel1)

Duration <- rbind(Images_biot, Images_belize)

#Duration <- Duration_per_flight(biot) # Full flight time
# Total_count <- cbind(Total_count, Duration)
#Duration <- Duration[-c(3,4,5,8),] # Remove unwanted flights


total_count <- cbind(total_count, Duration)


Duration_minutes <- lapply(Duration, function(x) {x/60})
#Total_count <- cbind(Total_count, Duration_minutes$Duration)
total_count <- cbind(total_count, Duration_minutes$Duration)





################## Calculate Distance in Metres #########################


# Geodesic distance between two points specified by radian latitude/longitude
# VincentyEllipsoid formula specified as most accurate
# Was stuck between calculating distance with full biot or biot_lite 
# Would use biot to account for full distance (specifically at end of flight) but as t is being factored into area later...



Calculate_dist_per_flight <- function(flight_no, d){
  # Distance calculated in metres
  df <- subset(d, d$Flight == flight_no, select = c(Lat, Long))
  pts <- df[c("Long", "Lat")] # Dataframes lagged by one point so as to calculate distance
  segDists <- distVincentyEllipsoid(p1 = pts[-nrow(df),], 
                                    p2 = pts[-1,])
  return(sum(segDists))
}


Calculate_distance <- function(df){
  Distance <- c()
  Flight_no <- c()
  Flights <- unique(df$Flight)
  for(i in Flights){
    Flight_no <- c(Flight_no, i)
    dist <- Calculate_dist_per_flight(i, df)
    Distance <- c(Distance, dist)
  }
  x <- cbind.data.frame(Flight_no, Distance)
  return(x)
}


# Add vector to df
Distance <- Calculate_distance(biot_lite)
#Distance <- Distance[-c(3,4,5,8),] # Remove unwanted flights

# Add supposed distances from Melissas report 

f7 <- 10.7*1000
f8 <- 13.1*1000
f10 <- 5.74*1000
f11 <- 33.5*1000
f12 <- 33.5*1000

Distance[3,2] <- f7
Distance[4,2] <- f8
Distance[5,2] <- f10
Distance[6,2] <- f11
Distance[7,2] <- f12


Total_count_lite <- cbind(Total_count_lite, Distance$Distance)
Total_count_lite <- cbind(Total_count_lite, Distance$Distance/1000)


names(Total_count_lite)[2] <- "Captures"
names(Total_count_lite)[3] <- "Images_used"
names(Total_count_lite)[4] <- "Duration (seconds)"
names(Total_count_lite)[5] <- "Duration (minutes)"
names(Total_count_lite)[6] <- "Distance (metres)"
names(Total_count_lite)[7] <- "Distance (km)"


Total_count_lite <- Total_count_lite %>% mutate(Speed <- (Total_count_lite[,6]/ Total_count_lite[,4]))
Total_count_lite <- Total_count_lite %>% mutate(Speed <- (Total_count_lite[,6]/ Total_count_lite[,4]) * 3.6)

names(Total_count_lite)[8] <- "Speed (m/s)"
names(Total_count_lite)[9] <- "Speed (km/h)"

Average_speed_overall <- sum(Total_count_lite[,8]/ nrow(Total_count_lite)) # 17.8 m/s or 64 km/h
#hist(Total_count_lite[,9])

#################################### FOV ###############################################


# Ferguson et al., 18 - Area in each image 

# The total area of each image was calculated as the product of the horizontal coverage
# (coverage.h, in metres) and vertical coverage (coverage.v, in metres), divided by 1 × 10^6 to produce a value in square kilometres. 

# Horizontal and vertical coverage were calculated as follows;

# coverage.h <- (sensor.h/f)*alt
# coverage.v <- (sensor.v/f)*alt
# where sensor.h is the horizontal dimension (mm) ofthe camera’s sensor, sensor.v is the vertical dimension (mm) ofthe camera’s sensor, 
# f is the focal length (mm) of the lens, and alt is the survey altitude (m).


# Sensor dimensions for 1/2.3" as specified in VIRB spec(wikipedia page: https://en.wikipedia.org/wiki/Image_sensor_format)
# Width = 6.17 mm
# Height = 4.55 mm
# Flight altidude calculated in BIOT_Explore according to 6 decent TLOGS : 49.84m = 50m

# Garmin
sensor.h <- 4.55
sensor.v <- 6.17
f <- 2.73
alt <- 50

# Sony DSLR ILCE-5100
sensor.h <- 23.5
sensor.v <- 15.6
f <- 16
alt <- 77.5

coverage.h <- (sensor.h/f)*alt
coverage.v <- (sensor.v/f)*alt


Area_covered_m2 <- coverage.h * coverage.v

#################################### Calculating Density ###########################################

# So our total distance for each flight was calculated using every flight image because of lat/long points

calcArea <- function(D){
  # Calculate area per survey in metres squared
  # Average covered by camera which is influenced by altitude and position (tilt, roll, pitch)
  # D : Distance calculated for each survey using VincentyEllipsoid function between lat/long points recorded each second
  
  return (A <- (Area_covered_m2 + D))
}


calcDensity <- function(z, A){
  # Calculate density using ideal gas model from capture rate and survey area covered
  # z : The number of encounters/captures.
  # A : Area covered by sensor per unit time.
  
    # Double check parameters
  if(z < 0 | !is.numeric(z)) stop("Number of individuals must be a positive number")
  if (A <= 0 | !is.numeric(A)) stop("Area, A, must be a positive number.")
  # Calculate density
  return(D <- z/A)
}


# Calculate Area (m2) and Density for each flight

Flight_Density <- function(df){
  # Calculate area and subsequent density for each flight
  Density_vec <- c()
  for(i in 1:nrow(df)){
    Area <- sapply(df[i,5], function(x) calcArea(x))
    #print(paste("Area is", Area, "m2"))
    Density <- sapply(df[i,2],  function(x) calcDensity(x[1], Area))
    #print(paste("Density is", Density, "m2"))
    Density_vec <- c(Density_vec, Density)
  }
  return(Density_vec)
}

final <- Flight_Density(Total_count_lite)
Total_count_lite <- cbind(Total_count_lite, final)
print(Total_count_lite)


Average_speed <- function(df){
  Average_speed <- c()
  for(i in 1:nrow(df)){
    speed <- (df[i,5])/(df[i,3])
    Average_speed <- c(Average_speed, speed)
  }
  return(Average_speed)
}

Speed_ms <- Average_speed(Total_count_lite)
Speed_kmh <- sapply(Speed_ms, function(x) x*3.6)

Total_count_lite <- cbind(Total_count_lite, Speed_ms)
Total_count_lite <- cbind(Total_count_lite, Speed_kmh)
