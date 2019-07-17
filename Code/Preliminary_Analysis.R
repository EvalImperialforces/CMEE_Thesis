#!/usr/bin/env Rscript
# Author: Eva Linehan
# Date: April 2019
# Desc: BIOT Analysis

#clear environments
rm(list=ls())

library(dplyr)
library(tidyr)
library(geosphere)
library(reshape2)
library(ggplot2)
library(ggthemes)



biot <- read.csv("../Data/Metadata/BIOT_2018.csv", header = T)
# Grounded stills removed from original dataset
biot <- subset(biot, select = -c(X)) # Weird NA column
biot <- subset(biot, Flight.or.Grounded == 'Flight')
#biot <- na.omit(biot) # Remove flights with no Lat/Long = Maldives 
#biot <- subset(biot, Flight != 7) # Remove 2/3 rows 


######################## Pseudo-rep ############################


Image_Overlap <- function(flight_no, start_col, end_col){
  
  # Subset data where animals are captured
  # Count repeated occurences to calculate mean overlap per flight
  # Function only looks at runs and not total number of animals
  
  # flight_no = Flight Number
  # start_col = First column of animal capture recording
  # end_col = Last column of animal capture recording
  
  df <- subset(biot, biot$Flight == flight_no)
  df <- df[,1:end_col] # Up to last capture
  df$ImageID <- as.character(df$ImageID)
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



Image_Overlap(1, 13, 28)
# Average 4.6

Image_Overlap(3, 13, 28) 
# Average 3

Image_Overlap(10, 13, 28) 
# Average 8.5


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

#################################### FOV ###############################################

# Must calculate radius (FOV) to generate A
# Photographs 4000 x 3000 pixels
# Focal Length (Depth of focus) = 50cm
# At 65m each sample is 140m x 105m of ground (FOV)

# Sensor dimensions for 1/2.3" as specified in VIRB spec(wikipedia page: https://en.wikipedia.org/wiki/Image_sensor_format)
# Width = 6.17 mm
# Height = 4.55 mm
# Crop length = 5.64


# Angle of view
AOV <- 2*atan(6.17/(2*50)) * 180/pi # Convert AOV to degrees

# Field of view
FOV <- 2*(tan(AOV/2) * 65)



# Mulero Palzmany et al., 14 has FOV - Area covered by the picture considering flight altitude, speed and horiontal camera sensor dimension

# A  <- (S*h*k)/10
# A is area (ha/hr), S is speed (km/hr), H is altitude (m), k is a constant that depends on cameras vertical sensor dimension.


# Ferguson et al., 18 - Area in each image 

# The total area of each image was calculated as the product of the horizontal coverage
# (coverage.h, in metres) and vertical coverage (coverage.v, in metres), divided by 1 × 106 to produce a value in square kilometres. 

#Horizontal and vertical coverage were calculated as follows;

# coverage.h <- (sensor.h/f)*alt
# coverage.v <- (sensor.v/f)*alt

# where sensor.h is the horizontal dimension (mm) ofthe camera’s sensor, sensor.v is the vertical dimension (mm) ofthe camera’s sensor, 
# f is the focal length (mm) ofthe lens, and alt is the survey altitude (m).



############################# Analyze Flight Data #####################################


Total_count_lite <- biot_lite %>%
  group_by(Flight) %>% 
  tally(c(TotalElasmo, TotalBirds, FruitBat, Human)) 


Total_count <- biot %>%
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


#######################################################################################################

######### Ecounter rate per flight #########


e.rate <- function(Flight, df, total_count){
  # Proportion of encounters per flight
  # Depends on the dataframe used (Pseudo/non-pseudo) and subsequent totl_count
  total <- nrow(df)
  capture <- total_count$n[total_count$Flight == Flight]
  print(total)
  print(capture)
  return(capture/total)
}

total <- nrow(biot_lite)
capture <- total_count$n[total_count$Flight == Flight]
print(total)
print(capture)

Encounter_per_flight <- function(dataf, total_count){
  Encounter_rate <- c() # Preallocated vector to add to df
  Flights <- unique(dataf$Flight)
  for(i in Flights){
    # Calculate encounter rate for each flight and append to vector
    df <- subset(dataf, dataf$Flight == i)
    rate <- e.rate(i, df, total_count)
    Encounter_rate <- c(Encounter_rate, rate)
  }
  # Add vector to df
  return(Encounter_rate)
}

#Encounter_vector <- Encounter_per_flight(biot)
Encounter_vector <- Encounter_per_flight(biot_lite, Total_count_lite)
#Total_count <- cbind(Total_count, Encounter_vector)
Total_count_lite <- cbind(Total_count_lite, Encounter_vector)


######################################## Distance #####################################################

# Calculate Elapsed Time in seconds
# Original metadata file duration
# Can I use the full flight time on the biot_lite dataset? I would say yes because the overlap is only to fix encounter rate

Duration_per_flight <- function(df){
  time <- df %>% 
    group_by(Flight) %>%
    summarise(Duration = length(Flight))
  return(time[,2])
}

Duration <- Duration_per_flight(biot)
#Total_count <- cbind(Total_count, Duration)
Duration <- Duration[-c(3,4,5,8),]
Total_count_lite <- cbind(Total_count_lite, Duration)

Duration_minutes <- lapply(Duration, function(x) {x/60})

#Total_count <- cbind(Total_count, Duration_minutes$Duration)
Total_count_lite <- cbind(Total_count_lite, Duration_minutes$Duration)


# Alternative using lubridate package/ tapply



# Calculate speed for each flight which should be approximately 60 - 65km/hr
# Geodesic distance between two points specified by radian latitude/longitude
# VincentyEllipsoid formula specified as most accurate

#Haversine <- function(lon1, lat1, lon2, lat2){
#  p = 0.017453292519943295     #Pi/180
#  a = 0.5 - cos((lat2 - lat1) * p)/2 + cos(lat1 * p) * cos(lat2 * p) * (1 - cos((lon2 - lon1) * p)) / 2
#  print ((12742 * asin(sqrt(a)))*1000)
#}

#Haversine2 <- function(p1, p2){
#  return(distHaversine(p1,p2))
#}

#Vincenty <- function(long1, lat1, long2, lat2) {
#  dist_m <- distVincentySphere(c(long1, lat1), c(long2, lat2), r = 6378137)
#  return(dist_m) 
#}

#Vincenty2 <- function(p1, p2){
#  return(distVincentyEllipsoid(p1,p2))
#}

#Ellipsoid <-function(p1,p2){
#  return(distGeo(p1,p2))
#}


#Haver <- Haversine(72.21150, -5.312633, 72.21150, -5.312650) # Haversine formula (metres)
#Haver2 <- Haversine2(c(72.21150, -5.312633), c(72.21150, -5.312650))
#Vinc <- Vincenty(72.21150, -5.312633, 72.21150, -5.312650) # Vincenty (metres)
#Vinc2 <- Vincenty2(c(72.21150, -5.312633), c(72.21150, -5.312650))
#Ellips <- Ellipsoid(c(72.21150, -5.312633), c(72.21150, -5.312650)) # Ellipsoid (metres)

#test <- distVincentyEllipsoid(c(-77.037852, 38.898556), c(-77.035934, 38.898551))
#test2 <- distVincentyEllipsoid(c(72.21150, -5.312633), c(72.21150, -5.312650))
# Tested against https://andrew.hedges.name/experiments/haversine/


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
  Flights <- unique(df$Flight)
  for(i in Flights){
    dist <- Calculate_dist_per_flight(i, df)
    Distance <- c(Distance, dist)
  }
  return(Distance)
}


# Add vector to df
Distance <- Calculate_distance(biot)
Total_count <- cbind(Total_count, Distance)
Total_count <- cbind(Total_count, Distance/1000)


#### Distances for 7,8,11 and 12 from Melissas report #####

f7 <- 10.7
f8 <- 13.1
f11 <- 33.5
f12 <- 33.5

print(Total_count$`Distance/1000`)


########################################### Speed #####################################################

# Calculate average speed 

Total_count <- Total_count %>% mutate(Speed <- (New_Distance/ Duration$Duration)*3.6)
#Total_encounter <- cbind(Total_encounter, Speed)


# Speed profile per flight

Speed_per_point <- function(flight_no){
  df <- subset(biot, biot$Flight == flight_no, select = c(Lat, Long))
  pts <- df[c("Long", "Lat")] # Dataframes lagged by one point so as to calculate distance
  segDists <- distVincentyEllipsoid(p1 = pts[-nrow(df),], 
                                    p2 = pts[-1,]) # Metres per second between 2 points
  return(speed_km <- segDists*3.6)
}

m <- list()
counter = 0
for(i in Flights){
  counter <- counter +1
  m[[counter]] <- Speed_per_point(i)
}


n <- sapply(m, length) # length of list vectors
seq.max <- seq_len(max(n)) # select longest sequence
speed_profile <- as.data.frame(sapply(m, "[", i = seq.max))
colnames(speed_profile) <- c(1, 3, 4, 5, 6, 9, 10)
speed_profile <- speed_profile %>% gather('Key', 'Value', '1', '3', '4', '5', '6', '9', '10')

sum(is.na(speed_profile$Value))

ggplot(data = speed_profile, aes(x = Value)) +
  geom_histogram(aes(weights = Value, fill = Value), binwidth = 5) +
  facet_wrap(~Key)

# Rename variables

names(Total_count)[2] <- "Captures"
names(Total_count)[4] <- "Duration (seconds)"
names(Total_count)[5] <- "Duration (minutes)"
names(Total_count)[6] <- "Distance (metres)"
names(Total_count)[7] <- "Distance (km)"
names(Total_count)[8] <- "Speed (km/hr)"



################ Model Dev ###################
# Animal signals detectable from any direction on a 2D plane
# Signal width changes with field of view but is 2 pi. 
# Simple gas model 


calcArea <- function(D){
  # Calculate area per survey in metres squared
  # FOV: Average field of view of camera which is influenced by altitude and position (tilt, roll, pitch)
  # FOV taken as 105 x 140m in this case
  # D : Distance calculated for each survey using VincentyEllipsoid function between lat/long points recorded each second
  
  return (A <- (140*105 + D))
}


calcDensity <- function(z, A){
  # Calculate density using ideal gas model from capture rate and survey area covered
  # z : The number of encounters/captures.
  # A : Area covered by sensor per unit time.
  
  
  # Double check parameters
  if(z < 0 | !is.numeric(z)) stop("Capture rate must be a positive number")
  if (A <= 0 | !is.numeric(A)) stop("Area, A, must be a positive number.")
  # Calculate density
  return(D <- z/A)
}


# Calculate Area and Density for each flight
# Assuming FOV = 105m * 140m

df <- na.omit(Total_count)

Area <- sapply(df[,"Distance"], function(x) calcArea(x))
df <- cbind(df, Area)

Density <- apply(df[, c("Encounter_rate", "Area")], 1,  function(x) calcDensity(x[1], x[2]))
df <- cbind(df, Density)


sum(Density)
# Very low density estimation when calculated per flight and summed up

# Calculating proportion of area covered for each MPA
# Chagos (640,000 km2) including 55 small islands 7 atolls

Location <- biot %>% group_by(Flight) %>%  distinct(Atoll, Island)

# According to Wiki:
# Saloman Atoll/ Islands : 