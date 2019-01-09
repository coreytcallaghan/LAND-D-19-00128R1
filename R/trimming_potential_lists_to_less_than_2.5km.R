## AN R script to subset the sampling spatial data to
## just checklists which travel < 2.5 km
## This is an intermediate script
## using two already processed scripts and reading the data in

## packages
library(readr)
library(dplyr)

# load a data file with the sampling information for each checklists, spatially
load("H:/Dissertation/Dissertation Chapters/Data Chapters/United States Urban Bird Patterns/Data/eBird data/eBird_sampling_spatial_data.RData")

# load in the checklists which have distance less than 2.5 km
checklists_2.5 <- read_delim("H:/Dissertation/Dissertation Chapters/Data Chapters/United States Urban Bird Patterns/Data/eBird data/Final_sampling_data/Final_sampling_data_2.5_km.txt", 
                                         "\t", escape_double = FALSE, trim_ws = TRUE)

lists_for_re_analysis <- checklists_2.5$SAMPLING_EVENT_IDENTIFIER

# subset sampling spatial data file
eBird_sampling_spatial_data_trimmed <- eBird_sampling_spatial_data %>%
  dplyr::filter(SAMPLING_EVENT_IDENTIFIER %in% lists_for_re_analysis)

save.image("H:/Dissertation/Dissertation Chapters/Data Chapters/United States Urban Bird Patterns/Data/eBird data/eBird_sampling_spatial_data_trimmed.RData")
