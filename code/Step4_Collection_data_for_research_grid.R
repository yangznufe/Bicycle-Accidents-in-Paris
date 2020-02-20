###### LIBRARYS ##########
library(dplyr)
library(tidyr)
library(sp)
library(raster)
library(rgeos)
library(rgbif)
library(viridis)
library(gridExtra)
library(rasterVis)
library(rgdal) 
library(spatstat)
library(maptools)
library(GISTools)
library(tmap)
library(sf)
library(geojson)
library(geojsonio)
library(tmaptools)
library(geosphere)
library(sp)

#######  LOAD RESEARCH GRID ###########################
# qtm(point_density,fill = 'n')
# load grid
hex_grid <- readOGR('N:/GIS Bicycle Project/research grid/research_grid.shp')


#use over to get the data of each grid

#1. cycleable road

#load data
cycleable_route <- readOGR('N:\\GIS Bicycle Project\\road\\reseau-cyclable.geojson')
cycleable_route_prj <- spTransform(cycleable_route, crs(hex_grid))
plot(cycleable_route_prj)
summary(cycleable_route_prj)

#count the length of each grid
cycleable_length <- over(hex_grid, cycleable_route_prj, returnList = TRUE)
View(cycleable_length)

#cycleable_length
x <- tibble(id = -1,cycleable_lens = -1)
x <- x[-c(1),]
for (i in 1:length(cycleable_length)){
  lens = sum(cycleable_length[[i]]$length)
  x <- add_row(x,id = i, cycleable_lens = lens)
}
# Try to coerce to SpatialPolygonsDataFrame with data (will throw error)
research_grid <- SpatialPolygonsDataFrame(hex_grid, x) 
# Plot and save
qtm(research_grid,fill = 'cycleable_lens')
shapefile(research_grid, filename='N:/GIS Bicycle Project/research grid/research_grid.shp')


############################################################################################
######################## Other Data to Grid ################################################
# 
bike_accident <- read.csv('.\\data\\bicycle_accidents\\2016_17_18_BicycleAccidents.csv')
coordinates(bike_accident) <- ~ long + lat
crs(bike_accident) <- crs(p_map)
bike_accdt <- spTransform(bike_accident, crs(hex_grid))

#count the number of each grid
bike_accdt_grid <- over(research_grid, bike_accdt, returnList = TRUE)
View(bike_accdt_grid)


#bike_accdt_grid
x <- tibble(id = -1,bike_accident = -1, bike_accdt_night= -1)
x <- x[-c(1),]
for (i in 1:length(bike_accdt_grid)){
  id = research_grid$id[i]
  num = length(bike_accdt_grid[[i]]$ID)
  num_night = sum(bike_accdt_grid[[i]]$Time>1800)+ sum(bike_accdt_grid[[i]]$Time<600)
  x <- add_row(x,id = id, bike_accident = num, bike_accdt_night = num_night)
}
View(x)
write.csv(x,file = '.\\data\\bicycle_accidents\\bicycle_accident_with_gridnet.csv')
###########################################################################
###########################################################################

methods(class = "sf")

r_grid <- readOGR('./data/research grid/research_grid.shp')
plot(r_grid)

car_park <- readOGR('.\\data\\For model two\\car_park.geojson')


