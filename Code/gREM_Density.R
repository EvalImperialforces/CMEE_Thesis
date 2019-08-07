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
original_biot <- subset(original_biot, original_biot$Flight.or.Grounded == 'Flight') # All images in flight
# Subset by particular flights
original_biot <- original_biot[original_biot$Flight == 1 | original_biot$Flight == 3 | original_biot$Flight == 7 | original_biot$Flight == 8 | original_biot$Flight == 10 | original_biot$Flight == 11 | original_biot$Flight == 12,  ]
original_bel1 <- read.csv("../Data/Metadata/Belize/21_02_19.csv", header = T)
#original_bel1 <- cbind('Flight' = 1, original_bel1)
original_bel1<- na.omit(original_bel1) 
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
# Can I use the full flight biot time on the biot_lite dataset? I would say yes because the overlap is only to fix encounter rate and calculate area
# Full flight duration used to calculate accurate speed


Duration_per_flight <- function(df){
  time <- df %>% 
    group_by(Flight) %>%
    summarise(Duration = length(Flight))
  return(time[,2])
}


Images_biot <- Duration_per_flight(original_biot)
# Belize duration from LOG_BELIZE_FLIGHTS.xlsx acording to observer (first 10 flights)
Duration_belize <- c(960, 1380, 1500, 1620, 300, 840, 960)

Duration <- rbind(Images_biot, Duration_belize[1])

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

#orig_10 <- Calculate_dist_per_flight(10,original_biot)
#lite_10 <- Calculate_dist_per_flight(10, biot_lite)
#orig_bel <- Calculate_dist_per_flight(1, original_bel1)
#lite_bel <- Calculate_dist_per_flight(1, belize_lite)


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
Distance_BIOT <- Calculate_distance(original_biot)
Distance_belize <- Calculate_distance(original_bel1)
Distance <- rbind(Distance_BIOT, Distance_belize)
#Distance <- Distance[-c(3,4,5,8),] # Remove unwanted flights

# Add supposed distances from Melissas report 

f7 <- 10.7*1000
f8 <- 13.1*1000
#f10 <- 5.74*1000
f11 <- 33.5*1000
f12 <- 33.5*1000

# Use recorded distances on log instead of calculated - slightly off but who knows.
#f13 <- 10.89*1000

Distance[3,2] <- f7
Distance[4,2] <- f8
#Distance[5,2] <- f10
Distance[6,2] <- f11
Distance[7,2] <- f12
#Distance[8,2] <- f13

total_count <- cbind(total_count, Distance$Distance)
total_count <- cbind(total_count, Distance$Distance/1000)


names(total_count)[2] <- "Captures"
names(total_count)[3] <- "Duration_(seconds)"
names(total_count)[4] <- "Duration_(minutes)"
names(total_count)[5] <- "Distance_(metres)"
names(total_count)[6] <- "Distance_(km)"


total_count <- total_count %>% mutate(Speed <- (total_count [,5]/ total_count [,3]))
total_count <- total_count %>% mutate(Speed <- (total_count [,5]/ total_count [,3]) * 3.6)

names(total_count)[7] <- "Speed_(m/s)"
names(total_count)[8] <- "Speed_(km/h)"

#Average_speed_overall <- sum(Total_count_lite[,8]/ nrow(Total_count_lite)) # 17.8 m/s or 64 km/h
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
g_sensor.h <- 4.55
g_sensor.v <- 6.17
g_f <- 2.73
g_alt <- 50

# Sony DSLR ILCE-5100
s_sensor.h <- 23.5
s_sensor.v <- 15.6
s_f <- 16
s_alt <- 77.5

g_coverage.h <- (g_sensor.h/g_f)*g_alt
g_coverage.v <- (g_sensor.v/g_f)*g_alt

s_coverage.h <- (s_sensor.h/s_f)*s_alt
s_coverage.v <- (s_sensor.v/s_f)*s_alt

garmin_m2 <- g_coverage.h * g_coverage.v
sony_m2 <- s_coverage.h * s_coverage.v



###################################### Calculate Area ############################################

# Two different methodologies here - practically analogous but not producing the same figures??
# gREM method changed to account for a moving sensor
# Strip sampling method which is more realisitc

# Duration of pseudo-flight, i.e number of images for calcArea
pseudo_BIOT <-  Duration_per_flight(biot_lite)
pseudo_belize <-  Duration_per_flight(belize_lite)
pseudo_duration <- rbind(pseudo_BIOT, pseudo_belize)

pseudo_garmin <- pseudo_duration[1:5,]
pseudo_sony <- pseudo_duration[6:8,]



calcArea <- function(FOV, no_images){
  #Calculate area per survey in metres squared
  #Average covered by camera which is influenced by altitude and position (tilt, roll, pitch)

  #FOV: calculated for each camera model (2rv)
  return (A <- (FOV * no_images))
}

Flight_Area_m2 <- function(no.images, fov){
  return(Area <- sapply(no.images, function(x) calcArea(fov, x)))
}


# Get total area m2
Area_BIOT <- Flight_Area_m2(pseudo_garmin, garmin_m2)
Area_mal_bel <- Flight_Area_m2(pseudo_sony, sony_m2)
gREM_Area <- c(Area_BIOT, Area_mal_bel)
gREM_km <- sapply(gREM_Area, function(x) x/1000)
total_count <- cbind(total_count, gREM_km)


# Strip sampling area
Straight_Area_m2 <- function(df, fov_width){
  Area_m2 <- sapply(df$`Distance_(metres)`, function (x) x * fov_width)
  return(Area_m2)
}

garmin_data <- total_count[1:5,]
sony_data <- total_count[6:8,]
strip_area_garmin <- Straight_Area_m2(garmin_data, g_coverage.v)
strip_area_sony <- Straight_Area_m2(sony_data, s_coverage.v)
strip_area <- c(strip_area_garmin, strip_area_sony)
Strip_km <- sapply(strip_area, function(x) x/1000)
total_count <- cbind(total_count, Strip_km)

# Get total area km2
Total_Area_km2 <- sapply(total_count$strip, function(x) x/1000)
total_count <- cbind(total_count, Total_Area_km2)



#################################### Calculating Density ###########################################

# So our total distance for each flight was calculated using every flight image because of lat/long  
# My biggest issue with the area calc is the 2rvt element of the equation - FOV*t but t?? It can't be the original time in seconds because
# that isn't taking into acount image overlap - our sensor is moving. So I wll have to take it 2rv*pseudo-t really (same thing)


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

Flight_Density <- function(total_df){
  # Calculate area and subsequent density for each flight
  Density_vec <- c()
  for(i in 1:nrow(total_df)){
    # Calculate densities for each area and capture rate
    Density <- calcDensity(total_df$Captures[i], Total_Area_km2[i])
    Density_vec <- c(Density_vec, Density)
  }
  return(Density_vec)
}



# Calculate densities for each dataset
Density_Estimate_km2 <- Flight_Density(total_count)

total_count <- cbind(total_count, Density_Estimate_km2)
#total_count <- within(total_count, rm(Density_Estimate_m2))







# Tom's figures

####### Density per flight #######
tc_no8 <- total_count[-4,]
tc_no8 <- cbind(a = c(1,2,3,4,5,6,7), tc_no8)

pdf(file = paste("../Results/Density_per_flight.pdf", sep = ""))
print(ggplot(data = tc_no8, aes(x = tc_no8$a, y = tc_no8$Density_Estimate_km2)) +
                      geom_point() +
                      xlab("Flight Number") +
                      ylab(expression(paste(
                        "Density Estimate (per",
                         km^2,
                        ")", sep=""))) +
                      scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7)) +
                      theme_bw())
dev.off()


#hist(tc_no8$Density_Estimate_km2)


###### Density per area #######


ggplot(data = tc_no8, aes(x = tc_no8$Total_Area_km2, y = tc_no8$Density_Estimate_km2)) +
  geom_point() +
  xlab(expression(paste(
    "Area( per",
    km^2,
    ")", sep=""))) +
  ylab(expression(paste(
    "Density Estimate ( per",
    km^2,
    ")", sep=""))) +
  theme_bw()
