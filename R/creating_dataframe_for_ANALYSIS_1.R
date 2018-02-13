#### setwd
setwd("H:/Dissertation/Dissertation Chapters/Data Chapters/United States Urban Bird Patterns/Data/eBird data/eBird_BCR_split_data")

#### packages
library(readr)
library(dplyr)
library(vegan)

#### read in species classification excel document
Species_classification <- read_csv("H:/Dissertation/Dissertation Chapters/Data Chapters/United States Urban Bird Patterns/Data/Bird Species Classifications/Species_classifications.csv")


#### create a list of all filenames in the folder of data
fileNames <- Sys.glob("*.RData")
 
#### create a landcover table that each dataframe will be joined with
LANDCOVER <- data.frame(NLCD_2011_value = as.integer(c(11, 21, 22, 23, 24, 31, 41, 42, 43, 52, 71, 81, 82, 90, 95)),
                        LC = c("Open Water", "Developed, Open Space", "Developed, Low Intensity", "Developed, Medium Intensity", 
                               "Developed, High Intensity", "Barren Land (Rock/Sand/Clay)", "Deciduous Forest", "Evergreen Forest", 
                               "Mixed Forest", "Shrub/Scrub", "Grassland/Herbaceous", "Pasture/Hay", "Cultivated Crops", 
                               "Woody Wetlands", "Emergent Herbaceous Wetlands"),
                        AGGREGATED_LANDCOVER = c("Green Area", "Open-urban", "Low-intensity Developed", 
                                                 "Medium/high-intensity Developed", "Medium/high-intensity Developed",
                                                 "Open-urban", "Green Area", "Green Area", "Green Area", 
                                                 "Green Area","Green Area", "Agriculture", "Agriculture",
                                                 "Agriculture", "Green Area", "Green Area"), stringsAsFactors = FALSE)


for (fileName in fileNames) {
  
  ## read in data
  load(fileName)
  
  ## merge dataframe with species classifications dataframe
  df <- df %>% inner_join(., Species_classification, by=c("COMMON_NAME", "SCIENTIFIC_NAME"))
  
  ## merge dataframe with the LANDCOVER dataframe created above
  df <- df %>% inner_join(., LANDCOVER, by="NLCD_2011_value")
  
  ## rename "Green Area" based on urban or nonurban delineation
  df <- within(df, AGGREGATED_LANDCOVER[AGGREGATED_LANDCOVER == "Green Area" & URBAN_NONURBAN == "URBAN"] <- "Urban Green Area")
  df <- within(df, AGGREGATED_LANDCOVER[AGGREGATED_LANDCOVER == "Green Area" & URBAN_NONURBAN == "NONURBAN"] <- "Natural Green Area")
  
  ## calculate richness by checklist
  SR <- df %>% group_by(CLASSIFICATION, SAMPLING_EVENT_IDENTIFIER, AGGREGATED_LANDCOVER, BCR_name, OBSERVATION_DATE) %>%
    summarise(SR = length(unique(COMMON_NAME)), DURATION_SAMPLING = mean(DURATION_MINUTES),
              LATITUDE = mean(LATITUDE), LONGITUDE = mean(LONGITUDE))
  
  ## calculate abundance and diversity by checklist
  ## this is a bit more difficult as I first remove any checklist that has a single X on it
  
  # Counts how many 'x's per checklist
  X_missing <- df %>%
    group_by(SAMPLING_EVENT_IDENTIFIER) %>%
    summarise(number_X = sum(OBSERVATION_COUNT=="X"))
  
  # extract sampling info
  sampling_event_info <- df %>%
    select(SAMPLING_EVENT_IDENTIFIER, STATE_PROVINCE, LOCALITY_ID, LOCALITY_TYPE, LATITUDE, LONGITUDE, 
           OBSERVATION_DATE, TIME_OBSERVATIONS_STARTED, OBSERVER_ID, PROTOCOL_TYPE, DURATION_MINUTES,
           EFFORT_DISTANCE_KM, EFFORT_AREA_HA, NUMBER_OBSERVERS, ALL_SPECIES_REPORTED, GROUP_IDENTIFIER,
           BCR_CODE, GEOID10, NAME10, UATYP10, NLCD_2011_value, URBAN_NONURBAN, BCR_name, LC, AGGREGATED_LANDCOVER) %>%
    distinct(SAMPLING_EVENT_IDENTIFIER, .keep_all=TRUE)
  
  # extract bird info
  bird_info <- df %>%
    select(COMMON_NAME, CATEGORY, order, family, CLASSIFICATION) %>%
    distinct()
  
  # accounts for the instance in which people submit 
  # the same species both at the species and subspecies level
  df_clean <- df %>%
    group_by(SAMPLING_EVENT_IDENTIFIER, COMMON_NAME) %>%
    summarise(COUNT_SPP = sum(as.numeric(as.character(OBSERVATION_COUNT)))) %>%
    rename(OBSERVATION_COUNT = COUNT_SPP) %>%
    inner_join(., sampling_event_info, by="SAMPLING_EVENT_IDENTIFIER") %>%
    inner_join(., X_missing, by="SAMPLING_EVENT_IDENTIFIER") %>%
    inner_join(., bird_info, by="COMMON_NAME") %>%
    filter(number_X==0)
  
  ## Now, actually calculate diversity and abundance for each checklist
  SD_A <- df_clean %>% group_by(CLASSIFICATION, SAMPLING_EVENT_IDENTIFIER, AGGREGATED_LANDCOVER, BCR_name, OBSERVATION_DATE) %>%
    summarise(EFFECTIVE_SD = exp(diversity(OBSERVATION_COUNT)), A=sum(OBSERVATION_COUNT),
              DURATION_SAMPLING = mean(DURATION_MINUTES), LATITUDE = mean(LATITUDE), LONGITUDE = mean(LONGITUDE))
  
  ## save Abundance column (A) as an integer to avoid parsing failures!!!
  SD_A$A <- as.integer(SD_A$A)
  
  write.csv(SR, paste0("H:/Dissertation/Dissertation Chapters/Data Chapters/United States Urban Bird Patterns/Data/eBird data/eBird_BCR_split_data/temp_data_", fileName, "_SR.csv"), row.names=FALSE)
  write.csv(SD_A, paste0("H:/Dissertation/Dissertation Chapters/Data Chapters/United States Urban Bird Patterns/Data/eBird data/eBird_BCR_split_data/temp_data_", fileName, "_SD_A.csv"), row.names=FALSE)
  
  rm(df)
  rm(df_clean)
  rm(sampling_event_info)
  rm(bird_info)
  rm(X_missing)
  rm(fileName)
  rm(SR)
  rm(SD_A)
}


## clear workspace
rm(list = ls())

## read in all species richness files
temp <- list.files(pattern="*SR.csv")
myfiles <- lapply(temp, read_csv)

species_richness_analysis <- bind_rows(myfiles)

rm(temp)
rm(myfiles)

## read in all diversity/abundance files
temp <- list.files(pattern="*SD_A.csv")
myfiles <- lapply(temp, read_csv)

species_diversity_abundance_analysis <- bind_rows(myfiles)

rm(temp)
rm(myfiles)

## Apparently a small number of checklists weren't assigned an urban or nonurban layer when
## doing the spatial joins. These remained as "Green Area" as opposed to Urban or Natural Green Area
## in this step, I filtered these out, before saving the data for analysis 1
species_richness_analysis <- species_richness_analysis %>% filter(AGGREGATED_LANDCOVER != "Green Area")
species_diversity_abundance_analysis <- species_diversity_abundance_analysis %>% filter(AGGREGATED_LANDCOVER != "Green Area")


## add season component to each dataframe for further analyses
getSeason <- function(DATES) {
  WS <- as.Date("2012-12-15", format = "%Y-%m-%d") # Winter Solstice
  SE <- as.Date("2012-3-15",  format = "%Y-%m-%d") # Spring Equinox
  SS <- as.Date("2012-6-15",  format = "%Y-%m-%d") # Summer Solstice
  FE <- as.Date("2012-9-15",  format = "%Y-%m-%d") # Fall Equinox
  
  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(DATES, format="2012-%m-%d"))
  
  ifelse (d >= WS | d < SE, "Winter",
          ifelse (d >= SE & d < SS, "Spring",
                  ifelse (d >= SS & d < FE, "Summer", "Fall")))
}

species_diversity_abundance_analysis$SEASON <- getSeason(species_diversity_abundance_analysis$OBSERVATION_DATE)
species_richness_analysis$SEASON <- getSeason(species_richness_analysis$OBSERVATION_DATE)

rm(getSeason)

## save the data frames as .RData file for analyses
save.image("H:/Dissertation/Dissertation Chapters/Data Chapters/United States Urban Bird Patterns/Data/Data for Analysis/ANALYSIS_1_DATA.RData")













