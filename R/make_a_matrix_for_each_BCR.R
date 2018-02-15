#### setwd
setwd("H:/Dissertation/Dissertation Chapters/Data Chapters/United States Urban Bird Patterns/Data/eBird data/eBird_BCR_split_data")

#### packages
library(readr)
library(dplyr)
library(tidyr)

#### read in species classification excel document
Species_classification <- read_csv("H:/Dissertation/Dissertation Chapters/Data Chapters/United States Urban Bird Patterns/Data/Bird Species Classifications/Species_classifications.csv")


#### create a landcover table that each dataframe will be joined with
LANDCOVER <- data.frame(NLCD_2011_value = as.integer(c(11, 21, 22, 23, 24, 31, 41, 42, 43, 52, 71, 81, 82, 90, 95)),
                        LC = c("Open Water", "Developed, Open Space", "Developed, Low Intensity", "Developed, Medium Intensity", 
                               "Developed, High Intensity", "Barren Land (Rock/Sand/Clay)", "Deciduous Forest", "Evergreen Forest", 
                               "Mixed Forest", "Shrub/Scrub", "Grassland/Herbaceous", "Pasture/Hay", "Cultivated Crops", 
                               "Woody Wetlands", "Emergent Herbaceous Wetlands"),
                        AGGREGATED_LANDCOVER = c("Green Area", "Open-urban", "Low-intensity Developed", 
                                                 "Medium/high-intensity Developed", "Medium/high-intensity Developed",
                                                 "Open-urban", "Green Area", "Green Area", "Green Area", 
                                                 "Green Area", "Agriculture", "Agriculture", 
                                                 "Agriculture", "Green Area", "Green Area"), stringsAsFactors = FALSE)
          
                                                 
                                                                                        
#### create a list of all filenames in the folder of data
fileNames <- Sys.glob("*.RData")

## create a matrix for each file
for (fileName in fileNames) {

load(fileName)

## merge dataframe with species classifications dataframe
## this removes any checklists which don't have full species on them
df <- df %>% inner_join(., Species_classification, by=c("COMMON_NAME", "SCIENTIFIC_NAME"))

## merge dataframe with the LANDCOVER dataframe created above
df <- df %>% inner_join(., LANDCOVER, by="NLCD_2011_value")

## rename "Green Area" based on urban or nonurban delineation
df <- within(df, AGGREGATED_LANDCOVER[AGGREGATED_LANDCOVER == "Green Area" & URBAN_NONURBAN == "URBAN"] <- "Urban Green Area")
df <- within(df, AGGREGATED_LANDCOVER[AGGREGATED_LANDCOVER == "Green Area" & URBAN_NONURBAN == "NONURBAN"] <- "Natural Green Area")

## Apparently a small number of checklists weren't assigned an urban or nonurban layer when
## doing the spatial joins. These remained as "Green Area" as opposed to Urban or Natural Green Area
## in this step, I filtered these out, before saving the data for analysis 1
df <- df %>% filter(AGGREGATED_LANDCOVER != "Green Area")

## remove any checklist that has a single X on it
### Counts how many 'x's per checklist
X_missing <- df %>%
  group_by(SAMPLING_EVENT_IDENTIFIER) %>%
  summarise(number_X = sum(OBSERVATION_COUNT=="X"))

### extract sampling info
sampling_event_info <- df %>%
  select(SAMPLING_EVENT_IDENTIFIER, STATE_PROVINCE, LOCALITY_ID, LOCALITY_TYPE, LATITUDE, LONGITUDE, 
         OBSERVATION_DATE, TIME_OBSERVATIONS_STARTED, OBSERVER_ID, PROTOCOL_TYPE, DURATION_MINUTES,
         EFFORT_DISTANCE_KM, EFFORT_AREA_HA, NUMBER_OBSERVERS, ALL_SPECIES_REPORTED, GROUP_IDENTIFIER,
         BCR_CODE, GEOID10, NAME10, UATYP10, NLCD_2011_value, URBAN_NONURBAN, BCR_name, LC, AGGREGATED_LANDCOVER) %>%
  distinct(SAMPLING_EVENT_IDENTIFIER, .keep_all=TRUE)


### accounts for the instance in which people submit 
### the same species both at the species and subspecies level
df_clean <- df %>%
  group_by(SAMPLING_EVENT_IDENTIFIER, COMMON_NAME) %>%
  summarise(COUNT_SPP = sum(as.numeric(as.character(OBSERVATION_COUNT)))) %>%
  rename(OBSERVATION_COUNT = COUNT_SPP) %>%
  inner_join(., sampling_event_info, by="SAMPLING_EVENT_IDENTIFIER") %>%
  inner_join(., X_missing, by="SAMPLING_EVENT_IDENTIFIER") %>%
  filter(number_X==0)


## matrix for relative proportion of a species on a checklist

### first, number of checklists per location
checklists_per_locality <- df_clean %>%
  group_by(LOCALITY_ID) %>%
  summarise(total_checklists=length(unique(SAMPLING_EVENT_IDENTIFIER)))

### second, select important information to include
locality_id_info <- df_clean %>%
  ungroup() %>%
  select(LOCALITY_ID, URBAN_NONURBAN, LC, AGGREGATED_LANDCOVER, NAME10) %>%
  distinct(LOCALITY_ID, .keep_all=TRUE)

### third, make the matrix
matrix <- df_clean %>%
  group_by(LOCALITY_ID, COMMON_NAME) %>%
  summarise(observation_count = length(COMMON_NAME)) %>%
  inner_join(., checklists_per_locality, by="LOCALITY_ID") %>%
  mutate(relative_proportion = observation_count/total_checklists) %>%
  inner_join(., locality_id_info, by="LOCALITY_ID") %>%
  select(-observation_count) %>%
  spread(COMMON_NAME, relative_proportion, fill=0)

fileName = gsub(".RData", "", fileName)


save(matrix, file=paste0("H:/Dissertation/Dissertation Chapters/Data Chapters/United States Urban Bird Patterns/Data/Matrix for each BCR/", fileName, "_matrix.RData"))

rm(df)
rm(df_clean)
rm(sampling_event_info)
rm(locality_id_info)
rm(X_missing)
rm(fileName)
rm(checklists_per_locality)
rm(matrix)

}

