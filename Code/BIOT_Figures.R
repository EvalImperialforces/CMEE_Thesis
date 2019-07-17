#!/usr/bin/env Rscript
# Author: Eva Linehan
# Date: June 2019
# Desc: BIOT Figures

#clear environments
rm(list=ls())

library(dplyr)
library(tidyr)
library(geosphere)
library(reshape2)
library(ggplot2)
library(ggthemes)


biot <- read.csv("../Data/BIOT_2018.csv", header = T)
# Grounded stills removed from original dataset

biot <- na.omit(biot) # Remove flights with no Lat/Long = Maldives


##################### Species Count Barplot ##########################

# Convert all captures to presence/ absent data as the number of individuals does not matter
biot_pa <- biot

columns_to_change <- biot[,13:28]
cols <- columns_to_change %>% mutate_if(is.numeric, ~1 * (. > 0))
biot_pa[,13:28] <- cols
biot_pa <- biot_pa[,-grep("Total", colnames(biot_pa))] # Remove total columns

Species_count2 <- biot_pa %>%
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


# Transpose dataframe for graphing
sp.bar <- data.frame(t(Species_count2[-1])) # Transpose
colnames(sp.bar) <- unlist(Species_count2[,1]) # Column names = flight.no
sp.bar$Total <- apply(sp.bar, 1, function(x) sum(x)) # Total 
sp.bar$Prop <- lapply(sp.bar$Total, function(x) x/sum(sp.bar$Total))



bar.plot <- ggplot(data = sp.bar, aes(x = rownames(sp.bar), y = Prop)) +
  geom_bar(stat = "identity") +
  xlab("Species") +
  ylab("Proportion") +
  scale_x_discrete(limits = rownames(sp.bar)) +
  theme_clean() +
  theme(axis.text.x = element_text(angle = 50), axis.ticks.length = unit(.5, "cm")) 




######################## Speed Profiles ##############################

# Calculate average speed 

Total_count <- Total_count %>% mutate(Speed <- (Distance/ Duration$Duration)*3.6)
#Total_encounter <- cbind(Total_encounter, Speed)

# Speed profile per flight


Speed_per_point <- function(flight_no){
  df <- subset(biot, biot$Flight == flight_no, select = c(Lat, Long))
  pts <- df[c("Long", "Lat")] # Dataframes lagged by one point so as to calculate distance
  segDists <- distVincentyEllipsoid(p1 = pts[-nrow(df),], 
                                    p2 = pts[-1,]) # Metres per second between 2 points
  return(speed_km <- segDists*3.6)
}

Flights <- c(1,3,10)
m <- list()
counter = 0
for(i in Flights){
  counter <- counter +1
  m[[counter]] <- Speed_per_point(i)
}


n <- sapply(m, length) # length of list vectors
seq.max <- seq_len(max(n)) # select longest sequence
speed_profile <- as.data.frame(sapply(m, "[", i = seq.max))
colnames(speed_profile) <- c(1, 3, 10)
speed_profile <- speed_profile %>% gather('Key', 'Value', '1', '3', '10')

sum(is.na(speed_profile$Value))

ggplot(data = speed_profile, aes(x = Value)) +
  geom_histogram(aes(weights = Value, fill = Value), binwidth = 5) +
  facet_wrap(~Key)

###################### Dist. of Density per transect ##############################



######################## FOV Camera Specs ##############################
# Specs for each table and xtable to LaTeX