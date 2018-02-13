#### setwd
setwd("H:/Dissertation/Dissertation Chapters/Data Chapters/United States Urban Bird Patterns/Data/eBird data/eBird_BCR_split_data")

#### packages
library(readr)
library(dplyr)
library(ggplot2)


#### Read in landcover per city information
zonal_histogram <- read_csv("H:/Dissertation/Dissertation Chapters/Data Chapters/United States Urban Bird Patterns/Data/Pixels per urban city/zonal_histogram.csv")

#### Read in information on the urban layer (cities)
urban_area <- read_csv("H:/Dissertation/Dissertation Chapters/Data Chapters/United States Urban Bird Patterns/Data/Urban Layer Classification/urban_layer.txt")

#### create a landcover table that each dataframe will be joined with
LANDCOVER <- data.frame(NLCD_2011_value = as.integer(c(11, 21, 22, 23, 24, 31, 41, 42, 43, 52, 71, 81, 82, 90, 95)),
                        LC = c("Open Water", "Developed, Open Space", "Developed, Low Intensity", "Developed, Medium Intensity", 
                               "Developed, High Intensity", "Barren Land (Rock/Sand/Clay)", "Deciduous Forest", "Evergreen Forest", 
                               "Mixed Forest", "Shrub/Scrub", "Grassland/Herbaceous", "Pasture/Hay", "Cultivated Crops", 
                               "Woody Wetlands", "Emergent Herbaceous Wetlands"),
                        AGGREGATED_LANDCOVER = c("Green Area", "Open-urban", "Low-intensity Developed", 
                                                 "Medium/high-intensity Developed", "Medium/high-intensity Developed",
                                                 "Open-urban", "Green Area", "Green Area", 
                                                 "Green Area","Green Area", "Agriculture", "Agriculture",
                                                 "Agriculture", "Green Area", "Green Area"), stringsAsFactors = FALSE)



#### calculate the frequency for each landcover class used in analyses per city
frequency_landcover <- zonal_histogram %>%
  select(NAME10, Total_Green_Area, Total_Open_Urban, Total_Agriculture, 
         Total_Low_Intensity_Developed, 
         Total_Medium_High_Intensity_Developed, Total) %>%
  mutate(Green_Area = Total_Green_Area/Total,
         Open_Urban = Total_Open_Urban/Total,
         Agriculture = Total_Agriculture/Total,
         Low_Intensity_Developed = Total_Low_Intensity_Developed/Total,
         Medium_High_Intensity_Developed = Total_Medium_High_Intensity_Developed/Total)
  

#### collating frequency of landcover dataset with dataset of size etc. for each city
city_variables <- urban_area %>%
  mutate(NAME10 = gsub(",", "__", .$NAME10)) %>%
  mutate(NAME10 = gsub(" ", "_", .$NAME10)) %>%
  mutate(NAME10 = gsub("--", "__", .$NAME10)) %>%
  mutate(NAME10 = gsub("___", "__", .$NAME10)) %>%
  inner_join(., frequency_landcover, by="NAME10")

#### plot the relationship between area of a city and the frequency of green area
ggplot(city_variables, aes(x=log(Area), y=Green_Area))+
  geom_point()+
  geom_smooth()

summary(lm(city_variables$Area ~ city_variables$Green_Area))














