## setwd("H:/Dissertation/Dissertation Chapters/Data Chapters/United States Urban Bird Patterns/Git/United-States-Urban-Birds-Patterns")

## packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(vegan)

## load data in
city_metadata <- read_csv("Data/Zonal statistics for random polygons/Zonal_metadata.txt")

city_tabulate_area <- read_csv("Data/Zonal statistics for random polygons/Zonal_statistics.txt")

## create a landcover table that each dataframe will be joined with
LANDCOVER <- data.frame(NLCD_2011_value = c("VALUE_11", "VALUE_21", "VALUE_22", "VALUE_23", "VALUE_24", "VALUE_31", 
                                                       "VALUE_41", "VALUE_42", "VALUE_43", "VALUE_52", "VALUE_71", "VALUE_81", 
                                                       "VALUE_82", "VALUE_90", "VALUE_95"),
                        LC = c("Open Water", "Developed, Open Space", "Developed, Low Intensity", "Developed, Medium Intensity", 
                               "Developed, High Intensity", "Barren Land (Rock/Sand/Clay)", "Deciduous Forest", "Evergreen Forest", 
                               "Mixed Forest", "Shrub/Scrub", "Grassland/Herbaceous", "Pasture/Hay", "Cultivated Crops", 
                               "Woody Wetlands", "Emergent Herbaceous Wetlands"),
                        AGGREGATED_LANDCOVER = c("Green Area", "Open-urban", "Low-intensity Developed", 
                                                 "Medium/high-intensity Developed", "Medium/high-intensity Developed",
                                                 "Open-urban", "Green Area", "Green Area", "Green Area", 
                                                 "Green Area", "Agriculture", "Agriculture",
                                                 "Agriculture", "Green Area", "Green Area"), stringsAsFactors = FALSE)

## create a clean dataframe
clean_df <- city_tabulate_area %>%
  filter(VALUE_0 == 0) %>%
  select(-VALUE_0) %>%
  filter(VALUE_12 == 0) %>%
  select(-VALUE_12) %>%
  mutate(ORIG_FID = as.character(as.integer(ORIG_FID))) %>%
  gather(., key="NLCD_2011_value", value="value", VALUE_11:VALUE_95) %>%
  inner_join(., LANDCOVER, by="NLCD_2011_value") %>%
  select(-OBJECTID)

## merge with the metadata
merged_df <- city_metadata %>%
  select(ORIG_FID, BCR, BCRNAME, Join_Count) %>%
  mutate(ORIG_FID = as.character(as.numeric(ORIG_FID))) %>%
  inner_join(., clean_df, by="ORIG_FID")
  
## rename "Green Area" based on urban or nonurban delineation
merged_df <- within(merged_df, AGGREGATED_LANDCOVER[AGGREGATED_LANDCOVER == "Green Area" & Join_Count == 1] <- "Urban Green Area")
merged_df <- within(merged_df, AGGREGATED_LANDCOVER[AGGREGATED_LANDCOVER == "Green Area" & Join_Count == 0] <- "Natural Green Area")

  
## calculate shannon index of habitat pixel counts
diversity_values <- merged_df %>%
  group_by(ORIG_FID) %>%
  summarise(div = diversity(value))

## merge the diversity values back with the sampling data
final_df <- merged_df %>%
  select(-value, -Join_Count, -NLCD_2011_value) %>%
  distinct(ORIG_FID, .keep_all=TRUE) %>%
  inner_join(., diversity_values, by="ORIG_FID")

## make plot
mean_BCR <- final_df %>%
  filter(AGGREGATED_LANDCOVER %in% c("Urban Green Area", "Natural Green Area")) %>%
  group_by(BCRNAME, AGGREGATED_LANDCOVER) %>%
  summarise(mean_div = mean(div))

all_data <- final_df %>%
  filter(AGGREGATED_LANDCOVER %in% c("Urban Green Area", "Natural Green Area")) 


ggplot(all_data, aes(x=AGGREGATED_LANDCOVER, y=div))+
  geom_violin(fill="azure3")+
  geom_point(data=mean_BCR, aes(x=AGGREGATED_LANDCOVER, y=mean_div, color=BCRNAME))+
  geom_line(data=mean_BCR, aes(x=AGGREGATED_LANDCOVER, y=mean_div, group=BCRNAME, color=BCRNAME))+
  stat_summary(fun.y=mean, geom="point", size=10, shape=15, alpha=0.7, color="red", aes(color=BCRNAME))+
  theme_classic()+
  ylab("Shannon diversity of habitat values")+
  xlab("Landcover")+
  coord_flip()+
  theme(axis.text.x=element_text(size=16))+
  theme(axis.text.y=element_text(size=16))+
  theme(axis.title.x = element_text(size=20))+
  theme(axis.title.y=element_text(size=20))

ggsave(file="H:/Dissertation/Dissertation Chapters/Data Chapters/United States Urban Bird Patterns/Figures/Figure 5/Habitat_diversity.png",
       width=480, height=300, units="mm", dpi=600)


## stats to report for paper
all_data %>%
  group_by(AGGREGATED_LANDCOVER) %>%
  summarise(mean=mean(div),
            sd=sd(div))



urban <- all_data %>%
  filter(AGGREGATED_LANDCOVER == "Urban Green Area") %>%
  .$div

natural <- all_data %>%
  filter(AGGREGATED_LANDCOVER == "Natural Green Area") %>%
  .$div


var.test(urban, natural) 

t.test(urban, natural, var.equal=FALSE, paired=FALSE)
  
  
  
  


