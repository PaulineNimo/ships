# load libraries
library(geodist)
library(dplyr)
library(readr)

# load all data at once
ship_data <- read_csv("data/ships_04112020.zip")

# calculate distance in meters between observations
ship_dist <- ship_data %>%
    group_by(SHIP_ID) %>%
    arrange(DATETIME, .by_group = T) %>%
    mutate(obs_dist = geodist(cbind(LON,LAT), sequential = T, pad = T, measure = "geodesic"))

# write to csv
write.csv(ship_dist, "data/ship data distance.csv", row.names = F)
