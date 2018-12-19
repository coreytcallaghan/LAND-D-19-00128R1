library(rgdal)
library(raster)

load("eBird_sampling_spatial_data.RData")
point <- as.data.frame(eBird_sampling_spatial_data[,1:3])
sp::coordinates(point) = ~LONGITUDE+LATITUDE
sp::proj4string(point) <- CRS("+proj=longlat +ellps=WGS84")

raster::shapefile(point, "checklist_points.shp")
