#### setwd
setwd("H:/Dissertation/Dissertation Chapters/Data Chapters/United States Urban Bird Patterns/Data")

### packages
library(readr)
library(dplyr)
library(RSQLite)

### connect to the sqlite db
### this is stored on my D drive because it is so large!
eBird_all_db <- src_sqlite("D:/eBird_November2017/eBird_November2017.sqlite", create=FALSE)
eBird_all <- tbl(eBird_all_db, "usa-2017")

### extract all the unique species across all states but get rid of Alaska and Hawaii
all_unique_species <- eBird_all %>%
  select(COMMON_NAME, SCIENTIFIC_NAME, STATE_PROVINCE, CATEGORY) %>%
  filter(STATE_PROVINCE != "Alaska") %>%
  filter(STATE_PROVINCE != "Hawaii") %>%
  distinct(., .keep_all=TRUE) %>%
  collect(n=Inf)

### filter for only species/domestic/subspecies (which could show up as species in some cases)
### then filter for the distinct species common name
all_unique_species <- all_unique_species %>%
  select(COMMON_NAME, SCIENTIFIC_NAME, CATEGORY) %>%
  filter(CATEGORY %in% c("species", "issf", "domestic")) %>%
  distinct(COMMON_NAME, .keep_all=TRUE)

### read in clements checklist to assign order/family for each species
Clements_Checklist_v2017_August_2017_2 <- read_csv("H:/Dissertation/Dissertation Chapters/Data Chapters/United States Urban Bird Patterns/Data/Bird Species Classifications/Clements-Checklist-v2017-August-2017_2.csv")

### clean_up_clements
clements_clean <- Clements_Checklist_v2017_August_2017_2 %>%
  filter(category == 'species') %>%
  select(`English name`, order, family, `scientific name`) %>%
  rename(COMMON_NAME = `English name`)

### final join with the clements list after cleaning so that each species had order/family (besides the domestics)
unique_species_merged_with_clements <- all_unique_species %>%
  left_join(., clements_clean, by="COMMON_NAME")
  
  
### write out file as csv to process further manually in excel  
write.csv(unique_species_merged_with_clements, "H:/Dissertation/Dissertation Chapters/Data Chapters/United States Urban Bird Patterns/Data/Bird Species Classifications/unique_species_merged_with_clements.csv", row.names=FALSE)

  
  
  

