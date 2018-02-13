## setwd 
setwd("H:/Dissertation/Dissertation Chapters/Data Chapters/United States Urban Bird Patterns/Data")

### Note that this file is run after the 'making_temp_spatial_data_eBird_checklists.R' file!
### This file is intended to calculate the nearest city for each point in the dataset
### as well as the geodesic distance to each city



## Necessary packages 
library(readr)
library(dplyr)
library(tidyr)
library(data.table)
library(geosphere)
library(RANN)


### read in sampling data
load("eBird data/eBird_sampling_spatial_data.RData")

### read in urban layer file
cities <- read_csv("Urban Layer Classification/urban_layer.txt")

### calculate the nearest cities for each of the localities
closest <- as.data.frame(nn2(cities[, 11:10], eBird_sampling_spatial_data[, 3:2], 1))

eBird_sampling_spatial_data <- cbind(eBird_sampling_spatial_data, closest)

city_id <- as.data.frame(cities$NAME10)
city_id$id <- 1:nrow(city_id)

locality_closest_city <- eBird_sampling_spatial_data %>%
  rename(id=nn.idx) %>%
  inner_join(., city_id, by="id") %>%
  rename(nearest_city = 'cities$NAME10') 


eBird_sampling_spatial_data <- cities %>%
  select(NAME10, Latitude, Longitude) %>%
  rename(nearest_city=NAME10,
         nearest_city_lat=Latitude,
         nearest_city_long=Longitude) %>%
  right_join(., locality_closest_city, by="nearest_city") %>%
  select(-one_of(c("id", "nn.dists")))

### now calculate the geodesic distance from each hotspot to the nearest city
setDT(eBird_sampling_spatial_data)[, distance.km := distGeo(matrix(c(nearest_city_long, nearest_city_lat), ncol=2),
                                                      matrix(c(LONGITUDE, LATITUDE), ncol=2))/1000]






