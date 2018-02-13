library(dplyr)
library(readr)

eBird_sampling_spatial_data  <- read_csv("H:/Dissertation/Dissertation Chapters/Data Chapters/United States Urban Bird Patterns/Data/eBird data/eBird_spatial_joins.txt")


eBird_sampling_spatial_data$NLCD_2011_value <- eBird_sampling_spatial_data$RASTERVALU


eBird_sampling_spatial_data$URBAN_NONURBAN <- eBird_sampling_spatial_data$Join_Count
eBird_sampling_spatial_data$URBAN_NONURBAN <- gsub("0", "NONURBAN", eBird_sampling_spatial_data$URBAN_NONURBAN)
eBird_sampling_spatial_data$URBAN_NONURBAN <- gsub("1", "URBAN", eBird_sampling_spatial_data$URBAN_NONURBAN)


eBird_sampling_spatial_data$TARGET_FID <- NULL
eBird_sampling_spatial_data$OBJECTID <- NULL
eBird_sampling_spatial_data$AFFGEOID10 <- NULL
eBird_sampling_spatial_data$UACE10 <- NULL
eBird_sampling_spatial_data$RASTERVALU <- NULL
eBird_sampling_spatial_data$Join_Count <- NULL
eBird_sampling_spatial_data$LSAD10 <- NULL

eBird_sampling_spatial_data$BCR_name <- eBird_sampling_spatial_data$BCR_CODE


eBird_sampling_spatial_data$BCR_name <- gsub("10", "NORTHERN_ROCKIES", eBird_sampling_spatial_data$BCR_name)
eBird_sampling_spatial_data$BCR_name <- gsub("11", "PRAIRIE_POTHOLES", eBird_sampling_spatial_data$BCR_name)
eBird_sampling_spatial_data$BCR_name <- gsub("12", "BOREAL_HARDWOOD_TRANSITION", eBird_sampling_spatial_data$BCR_name)
eBird_sampling_spatial_data$BCR_name <- gsub("13", "LOWER_GREAT_LAKES_ST_LAWRENCE_PLAIN", eBird_sampling_spatial_data$BCR_name)
eBird_sampling_spatial_data$BCR_name <- gsub("14", "ATLANTIC_NORTHERN_FOREST", eBird_sampling_spatial_data$BCR_name)
eBird_sampling_spatial_data$BCR_name <- gsub("15", "SIERRA_NEVADA", eBird_sampling_spatial_data$BCR_name)
eBird_sampling_spatial_data$BCR_name <- gsub("16", "SOUTHERN_ROCKIES_COLORADO_PLATEAU", eBird_sampling_spatial_data$BCR_name)
eBird_sampling_spatial_data$BCR_name <- gsub("17", "BADLANDS_AND_PRAIRIES", eBird_sampling_spatial_data$BCR_name)
eBird_sampling_spatial_data$BCR_name <- gsub("18", "SHORTGRASS_PRAIRIE", eBird_sampling_spatial_data$BCR_name)
eBird_sampling_spatial_data$BCR_name <- gsub("19", "CENTRAL_MIXED_GRASS_PRAIRIE", eBird_sampling_spatial_data$BCR_name)
eBird_sampling_spatial_data$BCR_name <- gsub("20", "EDWARDS_PLATEAU", eBird_sampling_spatial_data$BCR_name)
eBird_sampling_spatial_data$BCR_name <- gsub("21", "OAKS_AND_PRAIRIES", eBird_sampling_spatial_data$BCR_name)
eBird_sampling_spatial_data$BCR_name <- gsub("22", "EASTERN_TALLGRASS_PRAIRIE", eBird_sampling_spatial_data$BCR_name)
eBird_sampling_spatial_data$BCR_name <- gsub("23", "PRAIRIE_HARDWOOD_TRANSITION", eBird_sampling_spatial_data$BCR_name)
eBird_sampling_spatial_data$BCR_name <- gsub("24", "CENTRAL_HARDWOODS", eBird_sampling_spatial_data$BCR_name)
eBird_sampling_spatial_data$BCR_name <- gsub("25", "WEST_GULF_COASTAL_PLAIN_OUACHITAS", eBird_sampling_spatial_data$BCR_name)
eBird_sampling_spatial_data$BCR_name <- gsub("26", "MISSISSIPPI_ALLUVIAL_VALLEY", eBird_sampling_spatial_data$BCR_name)
eBird_sampling_spatial_data$BCR_name <- gsub("27", "SOUTHEASTERN_COASTAL_PLAIN", eBird_sampling_spatial_data$BCR_name)
eBird_sampling_spatial_data$BCR_name <- gsub("28", "APPALACHIAN_MOUNTAINS", eBird_sampling_spatial_data$BCR_name)
eBird_sampling_spatial_data$BCR_name <- gsub("29", "PIEDMONT", eBird_sampling_spatial_data$BCR_name)
eBird_sampling_spatial_data$BCR_name <- gsub("30", "NEW_ENGLAND_MID_ATLANTIC_COAST", eBird_sampling_spatial_data$BCR_name)
eBird_sampling_spatial_data$BCR_name <- gsub("31", "PENINSULAR_FLORIDA", eBird_sampling_spatial_data$BCR_name)
eBird_sampling_spatial_data$BCR_name <- gsub("32", "COASTAL_CALIFORNIA", eBird_sampling_spatial_data$BCR_name)
eBird_sampling_spatial_data$BCR_name <- gsub("33", "SONORAN_AND_MOJAVE_DESERTS", eBird_sampling_spatial_data$BCR_name)
eBird_sampling_spatial_data$BCR_name <- gsub("34", "SIERRA_MADRE_OCCIDENTAL", eBird_sampling_spatial_data$BCR_name)
eBird_sampling_spatial_data$BCR_name <- gsub("35", "CHIHUAHUAN_DESERT", eBird_sampling_spatial_data$BCR_name)
eBird_sampling_spatial_data$BCR_name <- gsub("36", "TAMAULIPAN_BRUSHLANDS", eBird_sampling_spatial_data$BCR_name)
eBird_sampling_spatial_data$BCR_name <- gsub("37", "GULF_COASTAL_PRAIRIE", eBird_sampling_spatial_data$BCR_name)
eBird_sampling_spatial_data$BCR_name <- gsub("5", "NORTHERN_PACIFIC_RAINFOREST", eBird_sampling_spatial_data$BCR_name)
eBird_sampling_spatial_data$BCR_name <- gsub("9", "GREAT_BASIN", eBird_sampling_spatial_data$BCR_name)


save.image("H:/Dissertation/Dissertation Chapters/Data Chapters/United States Urban Bird Patterns/Data/eBird data/eBird_sampling_spatial_data.RData")


