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



df <- read.csv("../Data/Metadata/BIOT_2018.csv", header = T)
# Grounded stills removed from original dataset
df <- subset(df, select = -c(X)) # Weird NA column
df <- subset(df, Flight.or.Grounded == 'Flight')
#biot <- na.omit(biot) # Remove flights with no Lat/Long = Maldives 
F1 <- read.csv("../Data/Metadata/Fixed_Flights/Flight1_revised.csv", header = T)
F1 <- F1[, 1:28]
names(F1)[6] <- 'Time'
F3 <- read.csv("../Data/Metadata/Fixed_Flights/Flight3_revised.csv", header = T)
F3 <- F3[, 1:28]
F7 <- subset(df, Flight == 7)
F7 <- F7[, 1:28]
names(F7)[2] <- 'Image.ID'
F8 <- subset(df, Flight == 8)
names(F8)[2] <- 'Image.ID'
F8 <- F8[, 1:28]
names(F8)[2] <- 'Image.ID'
F10 <- read.csv("../Data/Metadata/Fixed_Flights/Flight10_revised.csv", header = T)
F11 <- subset(df, Flight == 11)
F11 <- F11[, 1:28]
names(F11)[2] <- 'Image.ID'
F12 <- subset(df, Flight == 12)
F12 <- F12[, 1:28]
names(F12)[2] <- 'Image.ID'


biot <- rbind(F1, F3, F7, F8, F10, F11, F12)


################################## Strip Sampling ######################################

# Pseudoreplication as we must account for number of objects counted within each area

# Formula: D = n / a
# D is density
# n is total number of objects counted
# a is 2wl or rather sampled area

# So pseudorep will reduce estimates to remove repeated individuals (in runs)
# area counted by FOV * distance (length of flight)



################################### Pseudo-rep ##########################################


Image_Overlap <- function(flight_no, start_col, end_col){
  
  # Subset data where animals are captured
  # Count repeated occurences to calculate mean overlap per flight
  # Function only looks at runs and not total number of animals
  
  # flight_no = Flight Number
  # start_col = First column of animal capture recording
  # end_col = Last column of animal capture recording
  
  df <- subset(biot, biot$Flight == flight_no)
  #df <- df[,1:end_col] # Up to last capture
  df$Image.ID <- as.character(df$Image.ID)
  df <- df[,-grep("Total", colnames(df))] # Remove total columns
  index <- which(df > 0, arr.ind = T, useNames = T) # Array of dims for captures in subset
  # The column index is subject to your specific subset so do all together 
  index <- subset(index, index[,2] >= start_col)
  index <- subset(index, index[,2] <= end_col)
  #print(index)
  # Compare shifted datasets to calculate hits
  ind1 <- head(index, -1) # Remove last row
  ind2 <- index[-1,] # Remove first row
  counter <- 0
  hit_mat <- c()
  for(i in 1:length(ind1[,1])){
    if ((ind2[i,1] - ind1[i,1]) == 1 && ind1[i,2] == ind2[i,2]){
      counter <- counter 
      new_row <- c(counter, colnames(df)[(ind1[i,2])])
      hit_mat <- rbind(hit_mat, new_row)
    }
    else {
      counter <- counter + 1
      next}
  }
  hit_df <- as.data.frame(hit_mat)
  colnames(hit_df) <- c("Hits", "Animals")
  Actual_hits <- hit_df %>% group_by(Hits, Animals) %>% summarise(Actual_hits=n()+1)
  #print(Actual_hits)
  if(nrow(Actual_hits) == 1){
    # If there is one occurence of overlap, take that as average
    Average <- as.numeric(Actual_hits[1,3])
  }
  else{
    #print(sum(Actual_hits$Actual_hits))
    #print(nrow(Actual_hits))
    Average <- (sum(Actual_hits$Actual_hits))/(nrow(Actual_hits))}
  return(Average)
}



Image_Removal <- function(flight_no){
  # From the first image, jumps every x number of images and subsets accordingly
  Flight_df <- subset(biot, biot$Flight == flight_no)
  overlap_no <- floor(suppressWarnings(Image_Overlap(flight_no, 13, 28)))
  New_Flight <- c()
  for(i in 1:nrow(Flight_df)){
    if (i %% overlap_no == 1){
      New_Flight <- rbind(New_Flight, Flight_df[i,])
    }
  }
  flight_no <- as.data.frame(New_Flight)
  return(flight_no)
}



New_Flight1 <- Image_Removal(1)
New_Flight3 <- Image_Removal(3)
New_Flight7 <- Image_Removal(7)
New_Flight8 <- Image_Removal(8)
New_Flight10 <-Image_Removal(10)
New_Flight11 <- Image_Removal(11)
New_Flight12 <- Image_Removal(12)


biot_lite <- rbind(New_Flight1, New_Flight3, New_Flight7, New_Flight8, New_Flight10, New_Flight11, New_Flight12)


# From Mulero Palzmany et al., 14 could rely on percentage overlap to calculate area and then reduce the number of individuals depending on runs? 


#  O <-  ((k*h) - (S/P)) / (k*h)

#  O is overlapping (%), h is altitude AGL (m), S is speed of the plane (m/s), P is the number of pictures the camera takes per second.
#  k is a constant that depends on cameras vertical sensor dimension. The equation to calculate it is: k = df/f
#  dv is vertical dimension of the sensor (5.6 mm in our camera), f is local length (5.1 mm in our camera) k~1:09 for the camera we used.

# This would be the same for every flight?




######################## Total count for each flight ###################################


Total_count_lite <- biot_lite %>%
  group_by(Flight) %>% 
  tally(c(TotalElasmo, TotalBirds, FruitBat, Human)) 



Species_count <- biot_lite %>%
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


Images_used <- Duration_per_flight(biot_lite)
Duration <- Duration_per_flight(biot) # Full flight time
# Total_count <- cbind(Total_count, Duration)
#Duration <- Duration[-c(3,4,5,8),] # Remove unwanted flights
Total_count_lite <- cbind(Total_count_lite, Images_used)
Total_count_lite <- cbind(Total_count_lite, Duration)


Duration_minutes <- lapply(Duration, function(x) {x/60})
#Total_count <- cbind(Total_count, Duration_minutes$Duration)
Total_count_lite <- cbind(Total_count_lite, Duration_minutes$Duration)





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
