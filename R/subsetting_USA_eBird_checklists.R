### set wd
setwd("H:/Dissertation/Dissertation Chapters/Data Chapters/United States Urban Bird Patterns/Data")

### packages
library(readr)
library(dplyr)
library(lubridate)

### read in eBird USA complete checklists
USA_complete_checklists <- read_delim("eBird data/USA_complete_checklists/USA_complete_checklists.txt", 
                                      "\t", escape_double = FALSE, trim_ws = TRUE)

### subset eBird USA complete checklists
Final_sampling_data <- USA_complete_checklists %>% 
  filter(DURATION_MINUTES > 5) %>%
  filter(DURATION_MINUTES < 240) %>%
  filter(EFFORT_DISTANCE_KM < 10) %>%
  mutate(YEAR=year(OBSERVATION_DATE)) %>%
  filter(YEAR %in% c(2010:2017)) %>%
  filter(PROTOCOL_TYPE %in% c("eBird - Traveling Count", "eBird - Stationary Count", "eBird - Exhaustive Area Count"))
  

### write table out
write.table(Final_sampling_data, "eBird data/Final_sampling_data/Final_sampling_data.txt", row.names=FALSE, sep="\t")
