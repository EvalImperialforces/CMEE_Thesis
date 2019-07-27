#!/usr/bin/env Rscript
# Author: Eva Linehan
# Date: July 2019
# Desc: Clean Data

#clear environments
rm(list=ls())

library(dplyr)
library(tidyr)
library(gdata)

F1 <- read.csv("../Data/Metadata/BIOT/Fixed_Flights/Flight1_revised.csv", header = T)
F3 <- read.csv("../Data/Metadata/BIOT/Fixed_Flights/Flight3_revised.csv", header = T)

# Flights 7 - 12 from original dataset
df <- read.csv("../Data/Metadata/BIOT/BIOT_2018.csv", header = T)
df <- subset(df, Flight.or.Grounded == 'Flight') # Grounded stills removed from original dataset
F7 <- subset(df, Flight == 7)
F8 <- subset(df, Flight == 8)
F11 <- subset(df, Flight == 11)
F12 <- subset(df, Flight == 12)

F10 <- read.csv("../Data/Metadata/BIOT/Fixed_Flights/Flight10_revised.csv", header = T)



clean_data <- function(df, flight_no, biot = 'yes'){
  # Have to make all BIOT flights below the same length with the same variable names
  # Prep for pseudo functioning 
  names(df)[2] <- 'Image.ID'
  if(biot == 'yes' && length(df > 28)){
    df <- df[,-grep("Total", colnames(df))]
    df <- df[, 1:26] # 26 because you got rid of the total columns earlier
    names(df)[6] <- 'Time'
  }
  else if (biot == 'no'){df <- cbind('Flight' = flight_no, df)}
  return(df)
}

F1 <- suppressWarnings(clean_data(F1)) # Operation on factors causes warnings
F3 <- suppressWarnings(clean_data(F3)) # Operation on factors causes warnings
F7 <- suppressWarnings(clean_data(F7)) # Operation on factors causes warnings
F8 <- suppressWarnings(clean_data(F8)) # Operation on factors causes warnings
F10 <- F10[,-grep("Total", colnames(df))]
F11 <- suppressWarnings(clean_data(F11)) # Operation on factors causes warnings
F12 <- suppressWarnings(clean_data(F12)) # Operation on factors causes warnings



## Belize 
b1 <- read.csv("../Data/Metadata/Belize/21_02_19.csv", header = T)
miss_b1 <- na.omit(b1) 
# nrow(b1) - nrow(miss_b) - 69 images missing which would make a difference 
miss_b1 <- clean_data(miss_b1, 1, biot = 'no')


# Append to larger data files

biot <- rbind(F1, F3, F7, F8, F10, F11, F12)
belize <- miss_b1


################################## Strip Sampling ######################################

# Pseudoreplication as we must account for number of objects counted within each area

# Formula: D = n / a
# D is density
# n is total number of objects counted
# a is 2wl or rather sampled area

# So pseudorep will reduce estimates to remove repeated individuals (in runs)
# area counted by FOV * distance (length of flight)



################################### Pseudo-rep ##########################################


Image_Overlap <- function(flight_no, dataset = biot, start_col = 13, end_col = 26){
  
  # Subset data where animals are captured
  # Count repeated occurences to calculate mean overlap per flight
  # Function only looks at runs and not total number of animals
  
  # flight_no = Flight Number
  # start_col = First column of animal capture recording
  # end_col = Last column of animal capture recording
  
  df <- subset(dataset, dataset$Flight == flight_no)
  #df <- df[,1:end_col] # Up to last capture
  df$Image.ID <- as.character(df$Image.ID)
  index <- which(df > 0, arr.ind = T, useNames = T) # Array of dims for captures in subset
  # The column index is subject to your specific subset so do all together 
  # Subsets index including only columns in between and including selected columns
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

#Image_Overlap(1, belize, 7, 12)
#Image_Overlap(3)

Image_Removal <- function(flight_no, dataset = biot, start = 13, end = 26){
  # From the first image, jumps every x number of images and subsets accordingly
  Flight_df <- subset(dataset, dataset$Flight == flight_no)
  overlap_no <- floor(suppressWarnings(Image_Overlap(flight_no, dataset, start, end))) # Warnings relating to non-integer variables
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


New_B1 <- Image_Removal(1, belize, 7, 12) # Cut from 3534 down to 74 images because the overlap was 48. 


biot_lite <- rbind(New_Flight1, New_Flight3, New_Flight7, New_Flight8, New_Flight10, New_Flight11, New_Flight12)
belize_lite <- New_B1


# Remove everything except what we need
rm(list=ls()[! ls() %in% c("biot_lite","belize_lite")])

