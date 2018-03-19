## setwd("H:/Dissertation/Dissertation Chapters/Data Chapters/United States Urban Bird Patterns/Git/United-States-Urban-Birds-Patterns")

## packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(vegan)

## load data in
city_metadata <- read_csv("Data/Zonal statistics for random polygons/Zonal_metadata.txt")

city_tabulate_area <- read_csv("Data/Zonal statistics for random polygons/Zonal_counts.txt")

## create a landcover table that each dataframe will be joined with
LANDCOVER <- data.frame(NLCD_2011_value = c("VALUE_11", "VALUE_21", "VALUE_22", "VALUE_23", "VALUE_24", "VALUE_31", 
                                                       "VALUE_41", "VALUE_42", "VALUE_43", "VALUE_52", "VALUE_71", "VALUE_81", 
                                                       "VALUE_82", "VALUE_90", "VALUE_95"),
                        LC = c("Open Water", "Developed, Open Space", "Developed, Low Intensity", "Developed, Medium Intensity", 
                               "Developed, High Intensity", "Barren Land (Rock/Sand/Clay)", "Deciduous Forest", "Evergreen Forest", 
                               "Mixed Forest", "Shrub/Scrub", "Grassland/Herbaceous", "Pasture/Hay", "Cultivated Crops", 
                               "Woody Wetlands", "Emergent Herbaceous Wetlands"),
                        AGGREGATED_LANDCOVER = c("Green Area", "Open-urban", "Low-intensity Developed", 
                                                 "Medium/high-intensity Developed", "Medium/high-intensity Developed",
                                                 "Open-urban", "Green Area", "Green Area", "Green Area", 
                                                 "Green Area", "Agriculture", "Agriculture",
                                                 "Agriculture", "Green Area", "Green Area"), stringsAsFactors = FALSE)

## create a clean dataframe
clean_df <- city_tabulate_area %>%
  filter(VALUE_0 == 0) %>%
  select(-VALUE_0) %>%
  filter(VALUE_12 == 0) %>%
  select(-VALUE_12) %>%
  mutate(OBJECTID = as.character(as.integer(OBJECTID))) %>%
  gather(., key="NLCD_2011_value", value="value", VALUE_11:VALUE_95) %>%
  inner_join(., LANDCOVER, by="NLCD_2011_value") %>%
  select(-OBJECTID_1)

## merge with the metadata
merged_df <- city_metadata %>%
  select(OBJECTID, BCR, BCRNAME, Shape_Length, 
         Shape_Area, Join_Count) %>%
  mutate(OBJECTID = as.character(as.numeric(OBJECTID))) %>%
  inner_join(., clean_df, by="OBJECTID")
  
## rename "Green Area" based on urban or nonurban delineation
merged_df <- within(merged_df, AGGREGATED_LANDCOVER[AGGREGATED_LANDCOVER == "Green Area" & Join_Count == 1] <- "Urban Green Area")
merged_df <- within(merged_df, AGGREGATED_LANDCOVER[AGGREGATED_LANDCOVER == "Green Area" & Join_Count == 0] <- "Natural Green Area")

  
## calculate shannon index of habitat pixel counts
diversity_values <- merged_df %>%
  group_by(OBJECTID) %>%
  summarise(div = diversity(value))

## merge the diversity values back with the sampling data
final_df <- merged_df %>%
  select(-value, -Join_Count, -Shape_Length, -Shape_Area, -NLCD_2011_value) %>%
  distinct(OBJECTID, .keep_all=TRUE) %>%
  inner_join(., diversity_values, by="OBJECTID")

## make plot
final_df %>%
  filter(AGGREGATED_LANDCOVER %in% c("Urban Green Area", "Natural Green Area")) %>%
  ggplot(., aes(x=AGGREGATED_LANDCOVER, y=div))+
  geom_violin(fill="azure3")+
  stat_summary(fun.y=mean, geom="point", size=2.5, color="red")+
  theme_classic()+
  ylab("Shannon diversity of habitat values")+
  xlab("Landcover")+
  coord_flip()



  
  
  
  
  
  


