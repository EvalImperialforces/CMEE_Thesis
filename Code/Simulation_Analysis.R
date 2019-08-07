#!/usr/bin/env Rscript
# Author: Eva Linehan
# Date: July 2019
# Desc: Simulation Analysis

#clear environments
rm(list=ls())

library(dplyr)
library(tidyr)
library(geosphere)
library(reshape2)
library(ggplot2)
library(ggthemes)

# Run 1 simulations
sim1 <- read.csv("../Data/Simulations/Run_1/sim1.csv", header = T)
sim2 <- read.csv("../Data/Simulations/Run_1/sim2.csv", header = T)
sim3 <- read.csv("../Data/Simulations/Run_1/sim3.csv", header = T)
sim4 <- read.csv("../Data/Simulations/Run_1/sim4.csv", header = T)
sim5 <- read.csv("../Data/Simulations/Run_1/sim5.csv", header = T)
sim6 <- read.csv("../Data/Simulations/Run_1/sim6.csv", header = T)
sim7 <- read.csv("../Data/Simulations/Run_1/sim7.csv", header = T)
sim8 <- read.csv("../Data/Simulations/Run_1/sim8.csv", header = T)
sim9 <- read.csv("../Data/Simulations/Run_1/sim9.csv", header = T)
sim10 <- read.csv("../Data/Simulations/Run_1/sim10.csv", header = T)
sim11 <- read.csv("../Data/Simulations/Run_1/simsim1.csv", header = T)



# True Density Analysis on entire dataset
current_sims <- list(sim1, sim2, sim3, sim4, sim5, sim6, sim7, sim8, sim9, sim10, sim11)
all_sims <- rbind(sim1, sim2, sim3, sim4, sim5, sim6, sim7, sim8, sim9, sim10, sim11)

# Greater instances of smaller counts in actual_counts whereas total_counts is spread out
# Actual hits in the dataset

Actual_hit_plot <- ggplot(data = all_sims) + 
                      geom_histogram(aes(x=Actual_hits), fill = 'red',  binwidth = 0.5) 

Total_hit_plot <- ggplot(data = all_sims) +
                      geom_histogram(aes(x=Total_hits), fill = 'blue', position = "identity", binwidth = 0.5)


# Proportion of hits vs. non-hits
non_zero_sims <- subset(all_sims, all_sims$Actual_hits > 0)
Prop_hits <- nrow(non_zero_sims)/nrow(all_sims)


#################################### Density calculation ##########################################
# For sims 1, area = 5000m or 5km

calcDensity <- function(z, A){
  # Calculate density using ideal gas model from capture rate and survey area covered
  # z : The number of encounters/captures.
  # A : Area covered by sensor per unit time.
  
  # Double check parameters
  #if(z < 0 | !is.numeric(z)) stop("Number of individuals must be a positive number")
  if (A <= 0 | !is.numeric(A)) stop("Area, A, must be a positive number.")
  # Calculate density
  return(D <- z/A)
}

A = 5

# Calculate density per simulation
Density_km <- calcDensity(all_sims$Actual_hits, A)
False_Density_km <- calcDensity(all_sims$Total_hits, A)

all_sims <- cbind(all_sims, Density_km)
all_sims <- cbind(all_sims, False_Density_km)




# Proportion of correct densities 
#correct <- length(which(all_sims$Density_km == 1))


# Density histogram
true_sim_den <- mean(all_sims$Density_km)


all_sim_den <- ggplot(data = all_sims) + 
                geom_histogram(aes(x=Density_km), fill = 'skyblue1', binwidth = 0.1) +
                xlab(expression(paste("Density(per ", km^2,")", sep=" "))) +
                ylab('Frequency') +
                theme_bw()
all_sim_den + geom_vline(xintercept = true_sim_den, color = 'darkblue')


standard_deviation <- sd(Density_km)



# Get Density for each individual combination

per_sim_density <- function(df, A){
  
  # Calculate density for each simulation and add
  Density_km <- calcDensity(df$Actual_hits, A)
  False_Density_km <- calcDensity(df$Total_hits, A)
  
  df <- cbind(df, Density_km)
  df <- cbind(df, False_Density_km)
  
  return(df)
}

sim1 <- per_sim_density(sim1, A)
sim2 <- per_sim_density(sim2, A)
sim3 <- per_sim_density(sim3, A)
sim4 <- per_sim_density(sim4, A)
sim5 <- per_sim_density(sim5, A)
sim6 <- per_sim_density(sim6, A)
sim7 <- per_sim_density(sim7, A)
sim8 <- per_sim_density(sim8, A)
sim9 <- per_sim_density(sim9, A)
sim10 <- per_sim_density(sim10, A)
sim11 <- per_sim_density(sim11, A)

per_density_table <- function(df){
  ##### Calculate density for each individual combination ####
  den_vect <- subset(df, select = ('Density_km'))
  print(den_vect)
  return(den_vect)
}

den_table <- c(1:1344)
for(i in current_sims){
  t <- per_density_table(i)
  den_table <- cbind(den_table, t)
}

cols.dont.want <- c('den_table')
den_table <- den_table[, ! names(den_table) %in% cols.dont.want, drop = F]

# Calculate mean, sd and cv for all individual combos
combo_mean <- apply(den_table, 1, function(x) mean(x))
combo_sd <- apply(den_table, 1, function(x) sd(x))

combo_stats <- cbind(combo_mean, combo_sd)

combo_cv <- apply(combo_stats, 1, function(x) (x[2]/ x[1])*100)
combo_stats <- cbind(combo_stats, combo_cv)


cam_name <- c("garmin", "sony", "nadir")
cam_area <- c(9.296,8.475,17.876)

cam_table <- data.frame(cam_name, cam_area)

area_covered <- c()
for(i in all_sims$Camera){
  if(i == "garmin"){
    area_covered <- c(area_covered, cam_table[1,2])
  }
  if(i == "sony"){
    area_covered <- c(area_covered, cam_table[2,2])
  }
  if(i == "nadir"){
    area_covered <- c(area_covered, cam_table[3,2])
  }
}

all_sims <- cbind(all_sims, area_covered)
