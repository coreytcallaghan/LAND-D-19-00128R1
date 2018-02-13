#### setwd
setwd("H:/Dissertation/Dissertation Chapters/Data Chapters/United States Urban Bird Patterns/Data/eBird data/eBird_BCR_split_data")

#### packages
library(readr)
library(dplyr)

#### read in species classification excel document
Species_classification <- read_csv("H:/Dissertation/Dissertation Chapters/Data Chapters/United States Urban Bird Patterns/Data/Bird Species Classifications/Species_classifications.csv")


#### create a list of all filenames in the folder of data
fileNames <- Sys.glob("*.RData")

for (fileName in fileNames) {
  
  ## read in data
  load(fileName)
  
  ## merge dataframe with species classifications dataframe
  df <- df %>% inner_join(., Species_classification, by=c("COMMON_NAME", "SCIENTIFIC_NAME"))
  
  assign(paste0("unique_", fileName), df %>% 
           select(COMMON_NAME, SCIENTIFIC_NAME, order, family, CLASSIFICATION) %>%
           distinct(COMMON_NAME, .keep_all=TRUE))
  rm(df)
}

rm(Species_classification)

dfs <- Filter(function(x) is(x, "data.frame"), mget(ls()))

species <- do.call(rbind, dfs)

species <- species %>% distinct(COMMON_NAME, .keep_all=TRUE)

write.csv(species, "H:/Dissertation/Dissertation Chapters/Data Chapters/United States Urban Bird Patterns/Appendices/Appendix 1/Appendix_1.csv", row.names=FALSE)












