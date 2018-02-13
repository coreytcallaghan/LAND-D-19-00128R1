library(readr)
library(dplyr)

ebd_sampling_relAug_2017 <- read_delim("H:/Dissertation/Dissertation Chapters/Data Chapters/United States Urban Bird Patterns/Data/eBird data/USA_complete_checklists/ebd_sampling_relNov-2017/ebd_sampling_relNov-2017.txt/ebd_sampling_relNov-2017.txt", delim="\t", quote="", col_names=TRUE, trim_ws=TRUE)

USA <- ebd_sampling_relAug_2017 %>% filter(COUNTRY == "United States")
USA <- USA %>% filter(STATE != "Hawaii")
USA <- USA %>% filter(STATE != "Alaska")

colnames(USA) <- gsub(" ", "_", colnames(USA))

USA <- USA %>% filter(ALL_SPECIES_REPORTED == 1)

USA <- USA %>% select(SAMPLING_EVENT_IDENTIFIER, LATITUDE, LONGITUDE, COUNTRY, STATE, COUNTY, BCR_CODE, 
                      LOCALITY_ID, LOCALITY_TYPE, PROTOCOL_TYPE, OBSERVATION_DATE, TIME_OBSERVATIONS_STARTED, NUMBER_OBSERVERS,
                      GROUP_IDENTIFIER, DURATION_MINUTES, EFFORT_DISTANCE_KM, EFFORT_AREA_HA, OBSERVER_ID)

write.table(USA, "H:/Dissertation/Dissertation Chapters/Data Chapters/United States Urban Bird Patterns/Data/eBird data/USA_complete_checklists/USA_complete_checklists.txt", sep="\t", row.names=FALSE)
