#!/usr/bin/env Rscript
# Author: Eva Linehan
# Date: July 2019
# Desc: gREM Density

#clear environments
rm(list=ls())

library(dplyr)
library(tidyr)
library(geosphere)
library(reshape2)
library(ggplot2)
library(ggthemes)



df <- read.csv("../Data/Metadata/BIOT_2018.csv", header = T)
df1 <- read.csv()
# Grounded stills removed from original dataset
df <- subset(df, select = -c(X)) # Weird NA column
df <- subset(df, Flight.or.Grounded == 'Flight')
#biot <- na.omit(biot) # Remove flights with no Lat/Long = Maldives 
F1 <-
F3 <-
F7 <- subset(df, Flight == 7) # Remove 2/3 rows 
F8 <- subset(df, Flight == 8)
F10 <- 

# Convert all captures to presence/ absent data as the number of individuals does not matter
biot_pa <- biot

columns_to_change <- biot[,13:28]
cols <- columns_to_change %>% mutate_if(is.numeric, ~1 * (. > 0))
biot_pa[,13:28] <- cols
biot_pa <- biot_pa[,-grep("Total", colnames(biot_pa))] # Remove total columns



############################### Total count for each flight #######################################


Total_count <- biot_pa %>%
  group_by(Flight) %>% 
  tally(c(Reefshark, Nurseshark, Whaleshark, Eagleray, Mantaray, Whitetern, Sootytern, 
          Tern_other, Redfootedboobie, Frigatebird, Brownnoddy_any, Bird, FruitBat, Human)) 


Total_count <- Total_count[-c(3,4,5,8),]

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


Duration <- Duration_per_flight(biot) # Full flight time
# Total_count <- cbind(Total_count, Duration)
Duration <- Duration[-c(3,4,5,8),] # Remove unwanted flights
Total_count <- cbind(Total_count, Duration)


Duration_minutes <- lapply(Duration, function(x) {x/60})
#Total_count <- cbind(Total_count, Duration_minutes$Duration)
Total_count<- cbind(Total_count, Duration_minutes$Duration)




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
Distance <- Calculate_distance(biot_pa)
Distance <- Distance[-c(3,4,5,8),] # Remove unwanted flights

# Add supposed distances from Melissas report 

f7 <- 10.7*1000
f8 <- 13.1*1000
f11 <- 33.5*1000
f12 <- 33.5*1000

Distance[3,2] <- f7
Distance[4,2] <- f8
Distance[6,2] <- f11
Distance[7,2] <- f12


Total_count <- cbind(Total_count, Distance$Distance)
Total_count <- cbind(Total_count, Distance$Distance/1000)


names(Total_count)[2] <- "Captures"
names(Total_count)[3] <- "Duration (seconds)"
names(Total_count)[4] <- "Duration (minutes)"
names(Total_count)[5] <- "Distance (metres)"
names(Total_count)[6] <- "Distance (km)"
#names(Total_count)[8] <- "Speed (km/hr)"


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


sensor.h <- 4.55
sensor.v <- 6.17
f <- 2.73
alt <- 50

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

final <- Flight_Density(Total_count)
Total_count <- cbind(Total_count, final)
print(Total_count)



