## setwd 
setwd("H:/Dissertation/Dissertation Chapters/Data Chapters/United States Urban Bird Patterns/Data")

## load data
load("Data for Analysis/ANALYSIS_1_DATA.RData")

## total number of checklists used for species richness
length(unique(species_richness_analysis$SAMPLING_EVENT_IDENTIFIER))

## total number of checklists used for species diversity/abundance
length(unique(species_diversity_abundance_analysis$SAMPLING_EVENT_IDENTIFIER))

table(species_richness_analysis$AGGREGATED_LANDCOVER)
table(species_diversity_abundance_analysis$AGGREGATED_LANDCOVER)

BCR_table <- species_richness_analysis %>% 
  group_by(BCR_name) %>%
  summarise(count=n()) %>%
  arrange(desc(count))

write.csv(BCR_table, "H:/Dissertation/Dissertation Chapters/Data Chapters/United States Urban Bird Patterns/Appendices/Supplementary Table 1/STable1.csv", row.names=FALSE)

mean(BCR_table$count)
sd(BCR_table$count)
