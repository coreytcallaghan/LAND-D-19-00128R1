### An R Script to respond to reviewers for a couple of things

# packages
library(readr)
library(dplyr)
library(ggplot2)


# read in data
entropy_lc <- read_csv("H:/Dissertation/Dissertation Chapters/Data Chapters/United States Urban Bird Patterns/Data/habitat_entropy_at_each_checklist.csv")

# read in final sampling spatial data
Final_sampling_data_2_5_km <- read_delim("H:/Dissertation/Dissertation Chapters/Data Chapters/United States Urban Bird Patterns/Data/eBird data/Final_sampling_data/Final_sampling_data_2.5_km.txt", 
                                         "\t", escape_double = FALSE, trim_ws = TRUE)


# join the two for the final set of points included
final_points <- entropy_lc %>%
  rename(SAMPLING_EVENT_IDENTIFIER = SAMPLIN) %>%
  inner_join(., Final_sampling_data_2_5_km, by="SAMPLING_EVENT_IDENTIFIER")

# First a histogram showing the number of points and the percentage of pixels which are similarly classified
hist(final_points$lcagg_pc, breaks=50)

ggplot(final_points, aes(x=lcagg_pc))+
  geom_density()


# Make a histogram showing the length of checklists included
hist(final_points$EFFORT_DISTANCE_KM, breaks=100)


boxplot(final_points$lcagg_pc)

quantile(final_points$lcagg_pc, .8)
