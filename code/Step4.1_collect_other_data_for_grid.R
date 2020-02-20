

job_desity <- readOGR('.\\data\\job_density\\paris_job_density1.shp')

counted_road <- readOGR('./prediction_bike_flow/counter_street_buffer3.shp')

bike_road_agged3 <- readOGR('.\\prediction_bike_flow\\bike_road_agged3.shp')
qtm(job_desity,fill = 'pt_avail_r')

job_desity_prj <- spTransform(job_desity, crs(hex_grid))

##################### ALL BICYCLEABLE ROAD ###############################################
over_job_all_road <- over(bike_road_agged3,job_desity_prj, returnList = TRUE)

over_job_all_road_data <- tibble(voie = -1,job_density = -1,pt_density = -1, pt_avail_r = -1)
over_job_all_road_data <- over_job_all_road_data[-c(1),]
for (i in 1:length(bike_road_agged3)){
  voie_id = bike_road_agged3$voie[i]
  job_density = mean(over_job_all_road[[i]]$job_densit ,na.rm = T)
  pt_density = mean(over_job_all_road[[i]]$pt_density ,na.rm = T)
  pt_avail_r = mean(over_job_all_road[[i]]$pt_avail_r ,na.rm = T)
  over_job_all_road_data <- add_row(over_job_all_road_data,voie = voie_id, job_density = job_density,pt_density = pt_density, pt_avail_r = pt_avail_r)
}
View(over_job_all_road_data)

bike_road_agged3@data$voie <- as.character(bike_road_agged3@data$voie)
bike_road_agged3@data <- bike_road_agged3@data %>% left_join(over_job_all_road_data, by = c('voie' = 'voie'))

shapefile(bike_road_agged3, filename='./prediction_bike_flow/bike_road_agged4.shp')
write.csv(bike_road_agged3@data,"./prediction_bike_flow/bike_road_agged4.csv", row.names = FALSE)

#####################  COUNTED ROAD  ###############################################
###
over_job_counter <- over(counted_road,job_desity_prj, returnList = TRUE)

over_job_counter_data <- tibble(voie = -1,job_density = -1,pt_density = -1, pt_avail_r = -1)
over_job_counter_data <- over_job_counter_data[-c(1),]
for (i in 1:length(counted_road)){
  voie_id = counted_road$voie[i]
  job_density = mean(over_job_counter[[i]]$job_densit ,na.rm = T)
  pt_density = mean(over_job_counter[[i]]$pt_density ,na.rm = T)
  pt_avail_r = mean(over_job_counter[[i]]$pt_avail_r ,na.rm = T)
  over_job_counter_data <- add_row(over_job_counter_data,voie = voie_id, job_density = job_density,pt_density = pt_density, pt_avail_r = pt_avail_r)
}
View(over_job_counter_data)

counted_road@data$voie <- as.character(counted_road@data$voie)
counted_road@data <- counted_road@data %>% left_join(over_job_counter_data, by = c('voie' = 'voie'))

shapefile(counted_road, filename='./prediction_bike_flow/counted_road4.shp')
write.csv(counted_road@data,"./prediction_bike_flow/counted_road4.csv", row.names = FALSE)
