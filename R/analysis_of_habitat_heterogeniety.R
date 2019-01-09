# An R scipt to look at the relationship between habitat heterogeniety
# and species diversity for urban green and natural green areas


# packages
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)


# read in data
entropy_lc <- read_csv("H:/Dissertation/Dissertation Chapters/Data Chapters/United States Urban Bird Patterns/Data/habitat_entropy_at_each_checklist.csv")

# read in final sampling spatial data
Final_sampling_data_2_5_km <- read_delim("H:/Dissertation/Dissertation Chapters/Data Chapters/United States Urban Bird Patterns/Data/eBird data/Final_sampling_data/Final_sampling_data_2.5_km.txt", 
                                         "\t", escape_double = FALSE, trim_ws = TRUE)


# join the two for the final set of points included
final_points <- entropy_lc %>%
  rename(SAMPLING_EVENT_IDENTIFIER = SAMPLIN) %>%
  inner_join(., Final_sampling_data_2_5_km, by="SAMPLING_EVENT_IDENTIFIER")

# load analysis data in
## load data
load("H:/Dissertation/Dissertation Chapters/Data Chapters/United States Urban Bird Patterns/Data/Data for Analysis/ANALYSIS_1_DATA.RData")

## create a 'green area' dataframe
green_area <- final_points %>%
  dplyr::select(SAMPLING_EVENT_IDENTIFIER, lc_ent) %>%
  inner_join(., species_richness_analysis, by="SAMPLING_EVENT_IDENTIFIER") %>%
  filter(AGGREGATED_LANDCOVER %in% c("Urban Green Area", "Natural Green Area"))

## add species diversity to the dataframe to accompany species richness
temp <- species_diversity_abundance_analysis %>%
  dplyr::select(SAMPLING_EVENT_IDENTIFIER, EFFECTIVE_SD, A)

green_area <- green_area %>%
  inner_join(., temp, by="SAMPLING_EVENT_IDENTIFIER")

## Make a plot of the distribution of habitat heterogeniety differences between natural
## and urban green areas
## will use this as an appendix figure
green_area %>%
  dplyr::select(AGGREGATED_LANDCOVER, SAMPLING_EVENT_IDENTIFIER, lc_ent) %>%
  distinct(.) %>%
  ggplot(., aes(y=lc_ent, x=AGGREGATED_LANDCOVER))+
  geom_violin()+
  stat_summary(fun.y=mean, geom="point", size=6, shape=15, alpha=0.7, color="red", aes(color=BCR))+
  theme_classic()+
  ylab("Habitat heterogeneity")+
  xlab("")+
  coord_flip()+
  theme(axis.text.x=element_text(size=16))+
  theme(axis.text.y=element_text(size=16))+
  theme(axis.title.x = element_text(size=20))+
  theme(axis.title.y=element_text(size=20))

ggsave(filename="H:/Dissertation/Dissertation Chapters/Data Chapters/United States Urban Bird Patterns/Submissions/Landscape and Urban Planning/Revision 1/Figures/habitat_heterogeneity.png",
       height=4, width=6, units="in")

# do a t-test to look for differences
urban_green <- green_area %>%
  filter(AGGREGATED_LANDCOVER == "Urban Green Area") %>%
  dplyr::select(AGGREGATED_LANDCOVER, SAMPLING_EVENT_IDENTIFIER, lc_ent) %>%
  distinct(.)
natural_green <- green_area %>%
  filter(AGGREGATED_LANDCOVER == "Natural Green Area") %>%
  dplyr::select(AGGREGATED_LANDCOVER, SAMPLING_EVENT_IDENTIFIER, lc_ent) %>%
  distinct(.)

t.test(urban_green$lc_ent, natural_green$lc_ent)

mean(urban_green$lc_ent)
sd(urban_green$lc_ent)
mean(natural_green$lc_ent)
sd(natural_green$lc_ent)

# highly significant difference between the two


## make plots for supplementary information
# species richness
green_area %>%
  dplyr::select(SR, lc_ent, AGGREGATED_LANDCOVER, CLASSIFICATION) %>%
  mutate(CLASSIFICATION=gsub("E", "Exotic", .$CLASSIFICATION)) %>%
  mutate(CLASSIFICATION=gsub("N", "Native", .$CLASSIFICATION)) %>%
  ggplot(., aes(x=lc_ent, y=SR, color=CLASSIFICATION, linetype=AGGREGATED_LANDCOVER))+
  geom_smooth(method="lm")+
  theme_bw()+
  ylab("Species richness")+
  xlab("Habitat heterogeneity")+
  theme(axis.text.x=element_text(size=16))+
  theme(axis.text.y=element_text(size=16))+
  theme(axis.title.x = element_text(size=20))+
  theme(axis.title.y=element_text(size=20))+
  facet_wrap(~CLASSIFICATION, scales="free_x")+
  guides(color=FALSE)+
  guides(linetype=FALSE)

ggsave(filename="H:/Dissertation/Dissertation Chapters/Data Chapters/United States Urban Bird Patterns/Submissions/Landscape and Urban Planning/Revision 1/Appendix S4/species_richness.png")

green_area %>%
  dplyr::select(EFFECTIVE_SD, lc_ent, AGGREGATED_LANDCOVER, CLASSIFICATION) %>%
  mutate(CLASSIFICATION=gsub("E", "Exotic", .$CLASSIFICATION)) %>%
  mutate(CLASSIFICATION=gsub("N", "Native", .$CLASSIFICATION)) %>%
  ggplot(., aes(x=lc_ent, y=EFFECTIVE_SD, color=CLASSIFICATION, linetype=AGGREGATED_LANDCOVER))+
  geom_smooth(method="lm")+
  theme_bw()+
  ylab("Effective diversity")+
  xlab("Habitat heterogeneity")+
  theme(axis.text.x=element_text(size=16))+
  theme(axis.text.y=element_text(size=16))+
  theme(axis.title.x = element_text(size=20))+
  theme(axis.title.y=element_text(size=20))+
  facet_wrap(~CLASSIFICATION, scales="free_x")+
  guides(color=FALSE)+
  guides(linetype=FALSE)

ggsave(filename="H:/Dissertation/Dissertation Chapters/Data Chapters/United States Urban Bird Patterns/Submissions/Landscape and Urban Planning/Revision 1/Appendix S4/species_diversity.png")















### OTHER STUFF NOT INCLUDED!!!! ####

## Now plot the relationship between habitat heterogeneity surrounding a checklist
## and the species richness and species diversity recorded on that checklist
green_area %>%
  dplyr::select(SR, lc_ent, EFFECTIVE_SD, AGGREGATED_LANDCOVER, CLASSIFICATION) %>%
  gather(., key="Diversity_metric", value="value", SR, EFFECTIVE_SD) %>%
  ggplot(., aes(x=lc_ent, y=value, color=CLASSIFICATION))+
  geom_smooth(method="lm")+
  facet_wrap(Diversity_metric~CLASSIFICATION, scales="free_x")








ggplot(green_area, aes(x=lc_ent, y=SR))+
  geom_smooth(method="lm")+
  facet_wrap(AGGREGATED_LANDCOVER~CLASSIFICATION, scales="free")





## make a few prelim plots, showing relationship between entropy values and species richness

hist(green_area$lc_ent)



ggplot(green_area, aes(y=SR, x=AGGREGATED_LANDCOVER))+
  geom_violin()+
  coord_flip()


ggplot(green_area, aes(x=lc_ent, y=SR))+
  geom_smooth(method="lm")+
  facet_wrap(~CLASSIFICATION, scales="free")

  
ggplot(green_area, aes(x=lc_ent, y=EFFECTIVE_SD))+
  geom_smooth(method="lm")+
  facet_wrap(AGGREGATED_LANDCOVER~CLASSIFICATION, scales="free")

  
  
  
  