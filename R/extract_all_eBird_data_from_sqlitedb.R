#### setwd
setwd("H:/Dissertation/Dissertation Chapters/Data Chapters/United States Urban Bird Patterns/Data")

### write a function to select data for each BCR and 
### export the image to a folder named eBird_BCR_split_data
write_to_folder <- function(BCR) {
  
  ## require necessary packages
  library(readr)
  library(dplyr)
  library(RSQLite)
  
  ### connect to the sqlite db
  ### this is stored on my D drive because it is so large!
  eBird_all_db <- src_sqlite("D:/eBird_November2017/eBird_November2017.sqlite", create=FALSE)
  eBird_all <- tbl(eBird_all_db, "usa-2017")
  
  ### read in eBird spatial dataset to select the checklists necessary to extract from sqlite database 
  load("eBird data/eBird_sampling_spatial_data.RData")
  
  ### filter to the checklists for a given BCR name
  checklists <- eBird_sampling_spatial_data %>%
    filter(BCR_name == BCR) %>%
    .$SAMPLING_EVENT_IDENTIFIER
  
  ### retrieve the data for that given BCR from the sqlite db
  df <- eBird_all %>%
    select(SAMPLING_EVENT_IDENTIFIER, COMMON_NAME, SCIENTIFIC_NAME, OBSERVATION_COUNT, STATE_PROVINCE,
           LOCALITY_ID, LOCALITY_TYPE, LATITUDE, LONGITUDE, OBSERVATION_DATE,
           TIME_OBSERVATIONS_STARTED, OBSERVER_ID, PROTOCOL_TYPE, DURATION_MINUTES, 
           EFFORT_DISTANCE_KM, EFFORT_AREA_HA, NUMBER_OBSERVERS, ALL_SPECIES_REPORTED, GROUP_IDENTIFIER) %>%
    filter(SAMPLING_EVENT_IDENTIFIER %in% checklists) %>%
    collect(n=Inf)
  
  ### merge back together with the sampling data to keep some of the necessary sampling items
  df <- df %>% inner_join(., eBird_sampling_spatial_data, by="SAMPLING_EVENT_IDENTIFIER")
  
  ### remove everything besides dataframe and BCR
  rm(list=setdiff(ls(), c("df", "BCR")))
  
  ### save the image as .RData                        
  save(df, file=paste0("eBird data/eBird_BCR_split_data/", BCR, ".RData"))
}

### This is bad coding, and I should have used apply! But, I was lazy!
write_to_folder("BADLANDS_AND_PRAIRIES")
write_to_folder("SIERRA_MADRE_OCCIDENTAL")
write_to_folder("CENTRAL_MIXED_GRASS_PRAIRIE")
write_to_folder("PIEDMONT")
write_to_folder("PENINSULAR_FLORIDA")
write_to_folder("LOWER_GREAT_LAKES_ST_LAWRENCE_PLAIN")
write_to_folder("GREAT_BASIN")
write_to_folder("TAMAULIPAN_BRUSHLANDS")
write_to_folder("NORTHERN_ROCKIES")
write_to_folder("PRAIRIE_HARDWOOD_TRANSITION")
write_to_folder("SONORAN_AND_MOJAVE_DESERTS")
write_to_folder("GULF_COASTAL_PRAIRIE")
write_to_folder("CHIHUAHUAN_DESERT")
write_to_folder("EDWARDS_PLATEAU")
write_to_folder("OAKS_AND_PRAIRIES")
write_to_folder("SIERRA_NEVADA")
write_to_folder("APPALACHIAN_MOUNTAINS")
write_to_folder("BOREAL_HARDWOOD_TRANSITION")
write_to_folder("NEW_ENGLAND_MID_ATLANTIC_COAST")
write_to_folder("ATLANTIC_NORTHERN_FOREST")
write_to_folder("CENTRAL_HARDWOODS")
write_to_folder("COASTAL_CALIFORNIA")
write_to_folder("SOUTHERN_ROCKIES_COLORADO_PLATEAU")
write_to_folder("EASTERN_TALLGRASS_PRAIRIE")
write_to_folder("SOUTHEASTERN_COASTAL_PLAIN")
write_to_folder("MISSISSIPPI_ALLUVIAL_VALLEY")
write_to_folder("WEST_GULF_COASTAL_PLAIN_OUACHITAS")
write_to_folder("SHORTGRASS_PRAIRIE")
write_to_folder("PRAIRIE_POTHOLES")




