library(tidyverse)
library(tmap)
library(geojsonio)
library(plotly)
library(rgdal)
library(broom)
library(mapview)
library(crosstalk)
library(sf)
library(sp)
library(spdep)
library(car)
#  using this one https://www.rayshader.com/

bike_count_hourly <- read.csv('.\\data\\bicycle count\\comptage-velo-donnees-compteurs.csv',sep = ';')
bike_count_hourly$year <- year(bike_count_hourly$Date.et.heure.de.comptage)
bike_count_hourly$time <- hours(bike_count_hourly$Date.et.heure.de.comptage)
# Filter data by year == 2019
bike_count_hourly_2019 <- bike_count_hourly[bike_count_hourly$year == 2019,]

bike_count_hourly$hours <- substr(bike_count_hourly$time,1,4)
bike_count_hourly$hour <- strptime(bike_count_hourly$hours,'%H%M')

bike_count_hourly$month <- month(bike_count_hourly$Date.et.heure.de.comptage)

#group by site to get hourly bicycle count
bike_volume <- bike_count_hourly %>%
  group_by(month) %>%
  summarise_at(vars(Comptage.horaire), funs(mean(., na.rm=TRUE)))

hist(bike_volume)

bike_counter_aadt$id <- as.character(bike_counter_aadt$Identifiant.du.point.de.comptage)

#join the data to the @data slot in the SP data frame
bcs@data <- data.frame(bcs@data %>% left_join(bike_counter_aadt, by = c("id" = "id")))

shapefile(bcs,'N:\\GIS Bicycle Project\\data\\bicycle count\\counter_data.shp')


plot(bcs@data$timezone)
#check it's joined. and plot
head(bcs@data)
tmap_mode("view")
data(bcs)
tm_basemap()+
  tm_shape(bcs) + tm_bubbles(size = "Comptage.horaire",col = 'blue')




# bike_volume
# bike_accidents
bike_accident_time <- bike_accident@data %>%
  group_by(bike_accident@data$Time)%>%
  summarise_at(vars(ID), funs(length(.)))

bike_accident_month$month <- as.integer(bike_accident_month$`bike_accident@data$Month`)

plot(bike_accident_month$month,bike_accident_month$ID,ylim= c(0,450),type = 'o',col = 'blue')
lines(bike_volume$month,bike_volume$Comptage.horaire,ylim = c(0,80),type = 'o',col = 'red')






###########################################################
## study area










