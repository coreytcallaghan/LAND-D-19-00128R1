### R script for assessing eBird lists within
### random habitat buffers throughout the US
### This needs to be done on local machine as file is too 
### large for uploading to github

### setwd("H:/Dissertation/Dissertation Chapters/Data Chapters/United States Urban Bird Patterns")

### packages
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(vegan)

### read in habitat buffer assignment data
checklists_assignment <- read_csv("Data/Zonal statistics for random polygons/joined_eBird_lists_with_random_buffers.txt")

joined_checklists <- checklists_assignment %>%
  filter(JOIN_FID != -1) %>%
  rename(SAMPLING_EVENT_IDENTIFIER = SAMPLING_E) %>%
  dplyr::select(SAMPLING_EVENT_IDENTIFIER, TARGET_FID)


### read in zonal statistics and zonal metadata
## load data in
city_metadata <- read_csv("Data/Zonal statistics for random polygons/Zonal_metadata.txt")

city_tabulate_area <- read_csv("Data/Zonal statistics for random polygons/Zonal_statistics.txt")

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
  mutate(ORIG_FID = as.character(as.integer(ORIG_FID))) %>%
  gather(., key="NLCD_2011_value", value="value", VALUE_11:VALUE_95) %>%
  inner_join(., LANDCOVER, by="NLCD_2011_value") %>%
  select(-OBJECTID)

## merge with the metadata
merged_df <- city_metadata %>%
  select(ORIG_FID, BCR, BCRNAME, Join_Count) %>%
  mutate(ORIG_FID = as.character(as.numeric(ORIG_FID))) %>%
  inner_join(., clean_df, by="ORIG_FID")

## rename "Green Area" based on urban or nonurban delineation
merged_df <- within(merged_df, AGGREGATED_LANDCOVER[AGGREGATED_LANDCOVER == "Green Area" & Join_Count == 1] <- "Urban Green Area")
merged_df <- within(merged_df, AGGREGATED_LANDCOVER[AGGREGATED_LANDCOVER == "Green Area" & Join_Count == 0] <- "Natural Green Area")


## calculate shannon index of habitat pixel counts
diversity_values <- merged_df %>%
  group_by(ORIG_FID) %>%
  summarise(div = diversity(value))

## merge checklists with associated habitat buffers
checklists_and_buffers <- joined_checklists %>%
  mutate(ORIG_FID = as.character(as.integer(TARGET_FID))) %>%
  inner_join(., diversity_values, by="ORIG_FID")


###############################
#### Now need to associate all the checklists with a species richness and diversity value
### load in eBird analysis data
load("Data/Data for Analysis/ANALYSIS_1_DATA.RData")


lists_to_filter <- unique(checklists_and_buffers$SAMPLING_EVENT_IDENTIFIER)

species_richness_lists <- species_richness_analysis %>%
  filter(SAMPLING_EVENT_IDENTIFIER %in% lists_to_filter) %>%
  dplyr::select(SAMPLING_EVENT_IDENTIFIER, DURATION_SAMPLING, BCR_name, CLASSIFICATION, AGGREGATED_LANDCOVER, SR)

species_diversity_lists <- species_diversity_abundance_analysis %>%
  filter(SAMPLING_EVENT_IDENTIFIER %in% lists_to_filter) %>%
  dplyr::select(SAMPLING_EVENT_IDENTIFIER, EFFECTIVE_SD)

final_analysis_data <- checklists_and_buffers %>%
  inner_join(., species_richness_lists, by="SAMPLING_EVENT_IDENTIFIER") %>%
  inner_join(., species_diversity_lists, by="SAMPLING_EVENT_IDENTIFIER")
    

## clean workspace
rm(list=setdiff(ls(), "final_analysis_data"))

## calculate an average species richness for each random buffer
average_richness <- final_analysis_data %>%
  group_by(ORIG_FID, CLASSIFICATION, AGGREGATED_LANDCOVER) %>%
  summarise(Mean_Richness = mean(SR, na.rm=TRUE),
            Mean_Diversity = mean(EFFECTIVE_SD, na.rm=TRUE),
            habitat_div = mean(div, na.rm=TRUE)) %>%
  replace_na(list(CLASSIFICATION = "Missing")) %>%
  filter(CLASSIFICATION != "Missing") %>%
  filter(AGGREGATED_LANDCOVER %in% c("Natural Green Area", "Urban Green Area")) %>%
  ungroup() %>%
  mutate(CLASSIFICATION = gsub("E", "Exotic species", .$CLASSIFICATION)) %>%
  mutate(CLASSIFICATION = gsub("N", "Native species", .$CLASSIFICATION)) %>%
  gather(., key = "key", value="value", Mean_Diversity, Mean_Richness) %>%
  mutate(key = gsub("_", " ", .$key))

ggplot(average_richness, aes(x=value, y=habitat_div))+
  geom_smooth(method="lm")+
  facet_wrap(key~CLASSIFICATION, scales="free_x")+
  theme_classic()+
  xlab("Mean Respone Variable")+
  ylab("Shannon diversity of habitat pixels")

ggsave("H:/Dissertation/Dissertation Chapters/Data Chapters/United States Urban Bird Patterns/Appendices/Supplementary Figure 1/SuppFig1.png",
       width=100, height=130, units="mm", dpi=600)


## get r2 for each figure panel
exotic_div <- average_richness %>%
  filter(key == "Mean Diversity") %>%
  filter(CLASSIFICATION == "Exotic species")
  

cor.test(exotic_div$habitat_div, exotic_div$value)

exotic_div_r2 <- lm(habitat_div ~ value, data=exotic_div)
summary(exotic_div_r2)


