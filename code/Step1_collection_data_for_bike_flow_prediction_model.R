####### libraries  ##############
library(geosphere)
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
library(lubridate)
library(spatstat)
library(maptools)
library(GISTools)
library(tmap)
library(sf)
library(geojson)
library(geojsonio)
library(tmaptools)
library(RANN)
####################################################################################################
############# load bicycle flow data of 61 couter ################################################
bick_counter_site <- readOGR('.\\data\\bicycle count\\comptage-velo-sites-de-comptage.geojson')

p_map <- readOGR(".\\data\\paris\\paris_merge.shp")
paris_map <- readOGR(".\\data\\paris\\paris_merge_projection2.shp")
# plot(paris_map)

# projection 
bcs <- spTransform(bick_counter_site, crs(paris_map))
# plot(bcs)
# summary(bcs)

####################################################################################################
########### creae a buffer polygon of each point ###################################################
# bcs100m <- gBuffer( bcs, width=100, byid=TRUE )
# # Add data, and write to shapefile
# bcs100m <- SpatialPolygonsDataFrame(bcs100m, data= bcs100m@data)
# #writeOGR( pc100km, "pc100km", "pc100km", driver="ESRI Shapefile" ) 
# plot(bcs100m)
# plot(paris_map,add = T)

####################################################################################################
####### load 2019 Annual average daily traffic (AADT) data of bicycle  #############################

bike_count_hourly <- read.csv('.\\data\\bicycle count\\comptage-velo-donnees-compteurs.csv',sep = ';')
bike_count_hourly$year <- year(bike_count_hourly$Date.et.heure.de.comptage)

# Filter data by year == 2019
bike_count_hourly_2019 <- bike_count_hourly[bike_count_hourly$year == 2019,]

#group by site to get hourly bicycle count
bike_counter_aadt <- bike_count_hourly_2019 %>%
  group_by(Identifiant.du.point.de.comptage) %>%
  summarise_at(vars(Comptage.horaire), funs(mean(., na.rm=TRUE)))

bike_counter_aadt$id <- factor(bike_counter_aadt$Identifiant.du.point.de.comptage)

#join the data to the @data slot in the SP data frame
bcs@data <- data.frame(bcs@data %>% left_join(bike_counter_aadt, by = c("id" = "id")))

#check it's joined. and plot
head(bcs@data)
tmap_mode("view")
data(bcs)
tm_shape(bcs) + tm_bubbles(size = "Comptage.horaire",col = 'blue')

###################################################################################
###### Nearest Neighbour Search for Spatial Points ################################
###### from bicycle counter site to thier street   ################################

#load grid to get crs
hex_grid <- readOGR('./data/research grid/research_grid.shp')

#load cycleable route
cycleable_route <- readOGR('.\\data\\road\\reseau-cyclable.geojson')
cycleable_route_prj <- spTransform(cycleable_route, crs(hex_grid))

# snap bicycle counter to nearest bicycle road
# snapPointsToLines() a function of maptool
bick_counter_street = snapPointsToLines(bcs, cycleable_route_prj)

# test_bcs <- bcs[bcs$id == '100003098',]
# plot(test_bcs)

# creae a 0.5m buffer of each snap point
cb <- gBuffer( bick_counter_street, width=0.5, byid=TRUE )

# to sf , request by 'over' function
cb <- SpatialPolygonsDataFrame(cb, data= cb@data)
# writeOGR( cb, driver="ESRI Shapefile" ) 
# plot(cb,col = 'red')
# plot(cycleable_route_prj,add = T)

# create FID for cycleable_route
cycleable_route_prj$FID<- 1:length(cycleable_route_prj)

# over to get the road infomation
cb_info2 <-over(cb,cycleable_route_prj)

#select street by FID
lst = cb_info2$FID
street_counter <- cycleable_route_prj[cycleable_route_prj$FID %in% lst,]

####################################################################################################
# Dissolve bicycle route by road name "voie"
bike_road_agged <- aggregate(cycleable_route_prj, by= "voie",fun = sums)
lst2 = street_counter$voie
street <- bike_road_agged[as.character(bike_road_agged$voie) %in% lst2,]

#over to get the counted road 
counted_road_site_data <-over(cb,street,returnList = TRUE, id= 'id') %>%
  plyr::ldply(.fun = function(x) x, .id = "id") %>%
  mutate(id = cb$id) %>%
  mutate(count = as.double(cb$Comptage.horaire))
View(counted_road_site_data)

#some road have two counter 
#so sum them together, as road's bike flow ()

counted_road_data <- counted_road_site_data %>% 
  group_by(counted_road_site_data$voie) %>% 
  summarise(count = sum(count))

#left this table to spatial lines
unique_street <- street[!duplicated(street$voie),]
df <- data.frame(counted_road_data)
names(df)[names(df) == "counted_road_site_data.voie"] <- "voie"
# df$voie <- df$counted_road_site_data.voie
unique_street@data <- unique_street@data %>% left_join(df, 
                                              by = 'voie')


tmap_mode("view")
# tm_shape(unique_street)+tm_lines(title.col)+tm_layout(panel.labels = "street bicycle flow")

tm_shape(unique_street) +
  tm_lines(col="lightgreen", lwd="count", scale=8) 
##########################################################################
###### create buffer(50m) for those street ###############################

counter_street_buffer <-  gBuffer( unique_street, width=50, byid=TRUE )
qtm(counter_street_buffer)

street_buffer <- gBuffer(bike_road_agged,width = 50,byid = TRUE)
qtm(street_buffer)


############################################################################
###### count data for those street buffer ##################################

###### 1. count data for counter street buffer
###### 1.1 basic attribute info(form road map)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#drop na of cycleable route
cycle_route_prj_dropna <- cycleable_route_prj@data[complete.cases(cycleable_route_prj@data$voie),]

#1.1 get basic info of road
df <-  cycle_route_prj_dropna %>%
  group_by(voie) %>%
  summarise(
    arrdt1 = getmode(arrdt),
    statut1 = getmode(statut),
    two_way_driving = getmode(bidirectionnel),
    length1 = sum(length),
    road_type = getmode(typologie_simple)
  )
#1.2 length of bandes cycle road
df2 <- cycle_route_prj_dropna[cycle_route_prj_dropna$typologie_simple == 'Bandes cyclables',]%>%
  group_by(voie) %>%
  summarise(
    road_type_bandes = sum(length)
  )
#1.3 length of pistes cycle road
df3 <- cycle_route_prj_dropna[cycle_route_prj_dropna$typologie_simple == 'Pistes cyclables',]%>%
  group_by(voie) %>%
  summarise(
    road_type_pistes = sum(length))
#1.4 length of bus mixed cycle road
df4 <- cycle_route_prj_dropna[cycle_route_prj_dropna$typologie_simple == 'Couloirs de bus ouverts aux vélos',]%>%
  group_by(voie) %>%
  summarise(
    road_type_bus = sum(length))
#1.5 length of other cycle road
df5 <- cycle_route_prj_dropna[cycle_route_prj_dropna$typologie_simple == 'Autres itinéraires cyclables (ex : Aires piétonnes - Contre-sens cyclables)',]%>%
  group_by(voie) %>%
  summarise(
    road_type_other = sum(length))

# combine those data
library(plyr)
df_merge <- join_all(list(df,df2,df3,df4,df5), by='voie', type='left')

# df_m <- df_merge[,-8]  
bike_road_agged@data <- bike_road_agged@data %>% left_join(df_merge, by = 'voie')
# save as a shapefile and csv
shapefile(street_buffer,filename = './data/road/street_buffer.shp')
shapefile(bike_road_agged, filename='./prediction_bike_flow/bike_road_agged.shp')
write.csv(bike_road_agged@data,"./prediction_bike_flow/bike_road_agged.csv", row.names = FALSE)

#################################################################################################
# 2. count data for street buffer
# 2.1 load data of building and poi
traffic_counter <- readOGR(".\\data\\traffic_flow\\referentiel-comptages-routiers\\referentiel-comptages-routiers.shp")
str(traffic_counter@data)
qtm(traffic_counter)

#traffic flow N:\GIS Bicycle Project\traffic_flow
traffic_flow <- read.csv('.\\data\\traffic_flow\\traffic_flow_by_road.csv')
View(traffic_flow)
# crs(traffic_counter_buffer) <- crs(hex_grid)
traffic_counter_prj <- spTransform(traffic_counter, crs(hex_grid))

#combined traffic_counter and traffic flow
traffic_counter_prj@data <- traffic_counter_prj@data %>% left_join(
  traffic_flow,by = 'iu_ac'
)

#plot
traffic_counter_buffer <-  gBuffer( traffic_counter_prj, width=20, byid=TRUE )
qtm(traffic_counter_buffer,fill = 'q',fill.n = 12)

###########################################################################################################
# traffic flow to bicycle counter street, using over
# counter street buffer
# traffic_counter

over_counters <- over(counter_street_buffer,traffic_counter_prj, returnList = TRUE)

over_counters_data <- tibble(voie = -1,tf_q = -1,tf_k=-1)
over_counters_data <- over_counters_data[-c(1),]
for (i in 1:length(counter_street_buffer)){
   voie_id = counter_street_buffer$voie[i]
   q = mean(over_counters[[i]]$q ,na.rm = T)
   k = mean(over_counters[[i]]$k ,na.rm = T)
   over_counters_data <- add_row(over_counters_data,voie = voie_id, tf_q = q,tf_k = k)
 }
View(over_counters_data)

counter_street_buffer@data$voie <- as.character(counter_street_buffer@data$voie)

counter_street_buffer@data <- counter_street_buffer@data %>% left_join(over_counters_data, by = c('voie' = 'voie'))

shapefile(counter_street_buffer, filename='./prediction_bike_flow/counter_street_buffer1.shp')

###########################################################################################################
###########################################################################################################
# population density

#load population density
pop_density <- readOGR('.\\data\\population_density_200m\\paris_pop_density_200m.shp')
qtm(pop_density,fill = 'Ind')
pop_ind <- spTransform(pop_density, crs(counter_street_buffer))

pop_ind@data <- pop_ind@data[,c("IdINSPIRE",'Ind')]

##################### COUNTER ######################################################################
over_pop_counter <- over(counter_street_buffer,pop_ind, returnList = TRUE)

over_pop_counter_data <- tibble(voie = -1,pop_ind_2010 = -1)
over_pop_counter_data <- over_pop_counter_data[-c(1),]
for (i in 1:length(counter_street_buffer)){
  voie_id = counter_street_buffer$voie[i]
  pop_i = mean(over_pop_counter[[i]]$Ind ,na.rm = T)
  over_pop_counter_data <- add_row(over_pop_counter_data,voie = voie_id, pop_ind_2010 = pop_i)
}
View(over_pop_counter_data)

counter_street_buffer@data$voie <- as.character(counter_street_buffer@data$voie)

counter_street_buffer@data <- counter_street_buffer@data %>% left_join(over_pop_counter_data, by = c('voie' = 'voie'))

shapefile(counter_street_buffer, filename='./prediction_bike_flow/counter_street_buffer.shp')
write.csv(counter_street_buffer@data,"./prediction_bike_flow/counter_street_buffer1.csv", row.names = FALSE)
################### ALL CYCLEABLE ROUTE ###########################################################

over_pop_street <- over(street_buffer,pop_ind, returnList = TRUE)
over_pop_street_data <- tibble(voie = -1,pop_ind_2010 = -1)
over_pop_street_data <- over_pop_street_data[-c(1),]
for (i in 1:length(street_buffer)){
  voie_id = street_buffer$voie[i]
  pop_i = mean(over_pop_street[[i]]$Ind ,na.rm = T)
  over_pop_street_data <- add_row(over_pop_street_data,voie = voie_id, pop_ind_2010 = pop_i)
}
View(over_pop_street_data)

bike_road_agged@data$voie <- as.character(bike_road_agged@data$voie)

bike_road_agged@data <- bike_road_agged@data %>% left_join(over_pop_street_data, by = c('voie' = 'voie'))

shapefile(bike_road_agged, filename='./prediction_bike_flow/bike_road_agged2.shp')
write.csv(bike_road_agged@data,"./prediction_bike_flow/bike_road_agged.csv", row.names = FALSE)


###########################################################################################################
###########################################################################################################
# poi
# download from osm
# load data
poi_paris <- readOGR('.\\data\\poi\\poi_paris.shp')
poi_paris_prj <- spTransform(poi_paris, crs(cycleable_route_prj))

street_buffer <- readOGR('./data/road/street_buffer.shp')
qtm(street_buffer)

counter_street_buffer<- readOGR('./prediction_bike_flow/counter_street_buffer.shp')
qtm(counter_street_buffer)

################## COUNTER ####################counter_street_buffer
over_poi_counter <- over(counter_street_buffer,poi_paris_prj, returnList = TRUE)
over_poi_counter_data <- tibble(voie = -1,poi_num = -1,
                               parking_num = -1, bus_stop_num=-1,
                               bike_eqp_num =-1, edu_num =-1, restaurant_num =-1)

for (i in 1:46){
  
  voie_id = counter_street_buffer$voie[i]
  
  poi_num = length(over_poi_counter[[i]]$full_id)
  
  parking_num = length(over_poi_counter[[i]][over_poi_counter[[i]]$amenity == 'parking',]$amenity)
  
  bus_stop_num = length(over_poi_counter[[i]][over_poi_counter[[i]]$amenity == 'bus_station',]$amenity)
  
  bike_eqp_num = length(over_poi_counter[[i]][over_poi_counter[[i]]$amenity == 'bicycle_rental' |
                                                over_poi_counter[[i]]$amenity == 'bicycle_parking'|
                                                over_poi_counter[[i]]$amenity ==  'bicycle_repair_station',]$amenity)
  
  edu_num = length(over_poi_counter[[i]][over_poi_counter[[i]]$amenity == 'college'|
                                           over_poi_counter[[i]]$amenity == 'school'|
                                           over_poi_counter[[i]]$amenity == 'language_school'|
                                           over_poi_counter[[i]]$amenity ==  'driving_school'|
                                           over_poi_counter[[i]]$amenity == 'music_school' |
                                           over_poi_counter[[i]]$amenity == 'library' |
                                           over_poi_counter[[i]]$amenity == 'toy_library' |
                                           over_poi_counter[[i]]$amenity == 'university'  |
                                           over_poi_counter[[i]]$amenity == 'kindergarten',]$amenity)
  
  restaurant_num = length(over_poi_counter[[i]][over_poi_counter[[i]]$amenity == 'bar'|
                                                  over_poi_counter[[i]]$amenity == 'bbq'|
                                                  over_poi_counter[[i]]$amenity == 'cafe'|
                                                  over_poi_counter[[i]]$amenity ==  'drinking_water'|
                                                  over_poi_counter[[i]]$amenity == 'fast_food' |
                                                  over_poi_counter[[i]]$amenity == 'ice_cream' |
                                                  over_poi_counter[[i]]$amenity == 'pub' |
                                                  over_poi_counter[[i]]$amenity == 'restaurant'  ,]$amenity)
  
  over_poi_counter_data <- add_row(over_poi_counter_data,voie = voie_id, poi_num = poi_num ,parking_num = parking_num, bus_stop_num=bus_stop_num,
                                   bike_eqp_num =bike_eqp_num, edu_num =edu_num, restaurant_num =restaurant_num )
}
over_poi_counter_data <- over_poi_counter_data[-c(1),]
str(over_poi_counter_data)

counter_street_buffer <- readOGR('./prediction_bike_flow/counter_street_buffer.shp')

counter_street_buffer@data$voie <- as.character(counter_street_buffer@data$voie)
counter_street_buffer@data <- counter_street_buffer@data %>% left_join(over_poi_counter_data, by = c('voie' = 'voie'))
shapefile(counter_street_buffer, filename='./prediction_bike_flow/counter_street_buffer3.shp')
write.csv(counter_street_buffer@data,"./prediction_bike_flow/counter_street_buffer3.csv", row.names = FALSE)

################## ALL ROUTE ##################
over_poi_street <- over(street_buffer,poi_paris_prj, returnList = TRUE)
over_poi_street_data <- tibble(voie = -1,poi_num = -1,
                               parking_num = -1, bus_stop_num=-1,
                               bike_eqp_num =-1, edu_num =-1, restaurant_num =-1)


for (i in 1:2888){
  
  voie_id = street_buffer$voie[i]
  
  poi_num = length(over_poi_street[[i]]$full_id)
  
  parking_num = length(over_poi_street[[i]][over_poi_street[[i]]$amenity == 'parking',]$amenity)
  
  bus_stop_num = length(over_poi_street[[i]][over_poi_street[[i]]$amenity == 'bus_station',]$amenity)
  
  bike_eqp_num = length(over_poi_street[[i]][over_poi_street[[i]]$amenity == 'bicycle_rental' |
                                               over_poi_street[[i]]$amenity == 'bicycle_parking'|
                                               over_poi_street[[i]]$amenity ==  'bicycle_repair_station',]$amenity)
  
  edu_num = length(over_poi_street[[i]][over_poi_street[[i]]$amenity == 'college'|
                                          over_poi_street[[i]]$amenity == 'school'|
                                          over_poi_street[[i]]$amenity == 'language_school'|
                                          over_poi_street[[i]]$amenity ==  'driving_school'|
                                          over_poi_street[[i]]$amenity == 'music_school' |
                                          over_poi_street[[i]]$amenity == 'library' |
                                          over_poi_street[[i]]$amenity == 'toy_library' |
                                          over_poi_street[[i]]$amenity == 'university'  |
                                          over_poi_street[[i]]$amenity == 'kindergarten',]$amenity)
  
  restaurant_num = length(over_poi_street[[i]][over_poi_street[[i]]$amenity == 'bar'|
                                                 over_poi_street[[i]]$amenity == 'bbq'|
                                                 over_poi_street[[i]]$amenity == 'cafe'|
                                                 over_poi_street[[i]]$amenity ==  'drinking_water'|
                                                 over_poi_street[[i]]$amenity == 'fast_food' |
                                                 over_poi_street[[i]]$amenity == 'ice_cream' |
                                                 over_poi_street[[i]]$amenity == 'pub' |
                                                 over_poi_street[[i]]$amenity == 'restaurant'  ,]$amenity)
  
  over_poi_street_data <- add_row(over_poi_street_data,voie = voie_id, poi_num = poi_num ,parking_num = parking_num, bus_stop_num=bus_stop_num,
                                  bike_eqp_num =bike_eqp_num, edu_num =edu_num, restaurant_num =restaurant_num )
}
over_poi_street_data <- over_poi_street_data[-c(1),]
str(over_poi_street_data)


bike_road_agged<- readOGR('./prediction_bike_flow/bike_road_agged2.shp')
bike_road_agged@data$voie <- as.character(bike_road_agged@data$voie)

bike_road_agged@data <- bike_road_agged@data %>% left_join(over_poi_street_data, by = c('voie' = 'voie'))

shapefile(bike_road_agged, filename='./prediction_bike_flow/bike_road_agged3.shp')
write.csv(bike_road_agged@data, './prediction_bike_flow/bike_road_agged3.CSV')

#building
# building <- readOGR('.\\data\\environment\\building.geojson')
# View(building@data)





