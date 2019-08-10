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
  #names(df)[2] <- 'Image.ID'
  if(biot == 'yes' && length(df > 28)){
    names(df)[2] <- 'Image.ID'
    df <- df[,-grep("Total", colnames(df))]
    df <- df[, 1:26] # 26 because you got rid of the total columns earlier
    names(df)[6] <- 'Time'
  }
  else if (biot == 'no'){
    df <- cbind('Flight' = flight_no, df)}
    #print(names(df)[2])}
    #names(df)[2] <- 'Image.ID'}
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
b1 <- read.csv("../Data/Metadata/Belize/01.03(1).csv", header = T)
b2 <- read.csv("../Data/Metadata/Belize/3.3.19(1).csv", header = T)
b3 <- read.csv("../Data/Metadata/Belize/3.3.19(2).csv", header = T)
b4 <- read.csv("../Data/Metadata/Belize/3.3.19(3).csv", header = T)
b5 <- read.csv("../Data/Metadata/Belize/8.3.19(2).csv", header = T)
b6 <- read.csv("../Data/Metadata/Belize/21_02_19.csv", header = T)


# Pesky NAs
ray_col <- function(df, n){
    df$Ray.[is.na(df$Ray.)] <- 0
    df <- na.omit(df)
    df <- clean_data(df, n, biot = 'no')
    colnames(df) <- c('Flight', 'Image.ID', 'Lat', 'Long', 'Altitude', 'eagle.ray', 'turtle', 'manatee', 'shark', 'ray')
    return(df)
  }

# Convert NAs to 0s for Ray column and remove coords with no GPS coords
b1 <- ray_col(b1, 1)
b2 <- ray_col(b2, 2)
b3 <- ray_col(b3, 3)
b4 <- ray_col(b4, 4)
b5 <- ray_col(b5, 5)
b6 <- ray_col(b6, 6)


# Append to larger data files

biot <- rbind(F1, F3, F7, F8, F10, F11, F12)
belize <- rbind(b1, b2, b3, b4, b5, b6)

rownames(biot) <- NULL
rownames(belize) <- 1:nrow(belize)


################################### Pseudo-rep ##########################################


Image_Overlap <- function(flight_no, dataset = biot, start_col = 13, end_col = 26){
  
  # Subset data where animals are captured
  # Count repeated occurences to calculate mean overlap per flight
  # Function only looks at runs and not total number of animals
  
  # flight_no = Flight Number
  # start_col = First column of animal capture recording
  # end_col = Last column of animal capture recording
  
  df <- subset(dataset, dataset$Flight == flight_no)
  df <- df[,1:end_col] # Up to last capture
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
  counter <- as.numeric(0)
  hit_df <- data.frame(A= numeric(0), A= numeric(0))
  for(i in 1:length(ind1[,1])){
    if ((ind2[i,1] - ind1[i,1]) == 1 && ind1[i,2] == ind2[i,2]){
      counter <- counter 
      new_row <- c(counter, colnames(df)[(ind1[i,2])])
      #print(new_row)
      hit_df[i,] <- new_row
      #print(hit_mat)
    }
    else {
      counter <- counter + 1
      next}
  }
  colnames(hit_df) <- c("Hits", "Animals")
  #print(hit_df)
  hit_df <- na.omit(hit_df) # NA's appear because rows are missed on the else loop
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

#Over_1 <- Image_Overlap(1)
#Over_3 <- Image_Overlap(3)
#Over_7 <- Image_Overlap(7)
#Over_8 <- Image_Overlap(8)
#Over_10 <- Image_Overlap(10)
#Over_11 <- Image_Overlap(11)
#Over_12 <- Image_Overlap(12)
#Over_bel <- Image_Overlap(1, belize, 7, 12)
#overlapb <- Image_Overlap(1, belize, 6, 10)



Image_Removal <- function(flight_no, dataset = biot, start = 13, end = 26){
  # From the first image, jumps every x number of images and subsets accordingly
  Flight_df <- subset(dataset, dataset$Flight == flight_no)
  overlap_no <- ceiling(suppressWarnings(Image_Overlap(flight_no, dataset, start, end))) # Warnings relating to non-integer variables
  New_Flight <- c()
  for(i in 1:nrow(Flight_df)){
    if (i %% overlap_no == 1){
      New_Flight <- rbind(New_Flight, Flight_df[i,])
    }
  }
  flight_no <- as.data.frame(New_Flight)
  return(flight_no)
}

overlap <- c(11, 29, 9, 38, 113, 12, 13, 22, 77, 20, 26, 31, 13) 
                  

Image_Removal2 <- function(flight_no, overlap, dataset = biot, start = 13, end = 26){
  # Removes according to figures calculated from average speed (jupyter notebook, speed calculated from original_biot)
  Flight_df <- subset(dataset, dataset$Flight == flight_no)
  overlap_no <- overlap
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

New_B1 <- Image_Removal(1, belize, 6, 10) 
New_B2 <- Image_Removal(2, belize, 6, 10) 
New_B3 <- Image_Removal(3, belize, 6, 10) 
New_B4 <- Image_Removal(4, belize, 6, 10) 
New_B5 <- Image_Removal(5, belize, 6, 10) 
New_B6 <- Image_Removal(6, belize, 6, 10) 

Diff_Flight1 <- Image_Removal2(1, overlap[1])
Diff_Flight3 <- Image_Removal2(3, overlap[2])
Diff_Flight7 <- Image_Removal2(7, overlap[3])
Diff_Flight8 <- Image_Removal2(8, overlap[4])
Diff_Flight10 <- Image_Removal2(10, overlap[5])
Diff_Flight11 <- Image_Removal2(11, overlap[6])
Diff_Flight12 <- Image_Removal2(12, overlap[7])

Diff_B1 <- Image_Removal2(1, overlap[8], belize, 6, 10) 
Diff_B2 <- Image_Removal2(2, overlap[9], belize, 6, 10) 
Diff_B3 <- Image_Removal2(3, overlap[10], belize, 6, 10) 
Diff_B4 <- Image_Removal2(4, overlap[11], belize, 6, 10) 
Diff_B5 <- Image_Removal2(5, overlap[12], belize, 6, 10) 
Diff_B6 <- Image_Removal2(6, overlap[13], belize, 6, 10) 

biot_lite <- rbind(New_Flight1, New_Flight3, New_Flight7, New_Flight8, New_Flight10, New_Flight11, New_Flight12)
belize_lite <- rbind(New_B1, New_B2, New_B3, New_B4, New_B5, New_B6)

biot_lite2 <- rbind(Diff_Flight1, Diff_Flight3, Diff_Flight7, Diff_Flight8, Diff_Flight10, Diff_Flight11, Diff_Flight12)
belize_lite2 <- rbind(Diff_B1, Diff_B2, Diff_B3, Diff_B4, Diff_B5, Diff_B6)

# Remove everything except what we need
rm(list=ls()[! ls() %in% c("biot_lite","biot_lite2", "belize_lite", "belize_lite2", "belize")])

