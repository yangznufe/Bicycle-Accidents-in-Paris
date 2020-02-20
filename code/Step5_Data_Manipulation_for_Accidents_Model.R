
#######################################################################################
#load grid to get the FID of each grid
library(rgdal)
research_grid <- readOGR('.\\data\\research grid\\research_grid.shp')

#load new data
data_all_1 <- read.csv('./data/For model two/all_street_id_stastics.txt')
data_all_2 <- read.csv('./data/For model two/all_street_id_statics2.txt')
data_all_3 <- read.csv('./data/For model two/all_street_id_stastics3.txt')
data_all_4 <- read.csv('./data/For model two/all_street_identity_statics4.txt')

data_all_2_bandes <- data_all_2[data_all_2$rd_typ_x == 'Bandes cyclables',c("FID_research_grid","SUM_lngth1_x")]
data_all_2_pista <- data_all_2[data_all_2$rd_typ_x == 'Pistes cyclables',c("FID_research_grid","SUM_lngth1_x")]
data_all_2_bus <- data_all_2[data_all_2$rd_typ_x == 'Couloirs de bus ouverts aux vÃ©los',c("FID_research_grid","SUM_lngth1_x")]
names(data_all_2_bandes)[2]<-"sum_len_road_bandes"
names(data_all_2_pista)[2]<-"sum_len_road_pista"
names(data_all_2_bus)[2]<-"sum_len_road_bus"

data_all_3_limit_30 <- data_all_3[data_all_3$sttt1_x == 'Voie 30',c("FID_research_grid","SUM_lngth1_x")]
data_all_3_limit_50 <- data_all_3[data_all_3$sttt1_x == 'Voie 50',c("FID_research_grid","SUM_lngth1_x")]
data_all_3_limit_walk_way <- data_all_3[data_all_3$sttt1_x == 'Aire piÃ©tonne',c("FID_research_grid","SUM_lngth1_x")]
data_all_3_limit_20 <- data_all_3[data_all_3$sttt1_x == 'Zone de rencontre',c("FID_research_grid","SUM_lngth1_x")] #walker first
names(data_all_3_limit_30)[2]<-"sum_len_limit30"
names(data_all_3_limit_50)[2]<-"sum_len_limit_50"
names(data_all_3_limit_20)[2]<-"sum_len_limit_20"
names(data_all_3_limit_walk_way)[2]<-"sum_len_walk_way"

data_altitude <- read.csv('./data/For model two/altitude_id_stastics.txt')
data_altitude <- data_altitude[,c("FID_research_grid","STD_altitude")]

data_carpark <- read.csv('./data/For model two/car_park_identity_sttistics.txt')
data_carpark <- data_carpark[,c("FID_research_grid", "FREQUENCY")]
names(data_carpark)[2]<- 'carpark_count'

data_cross_road <- read.csv('./data/For model two/cross_road_id_statics.txt')
data_cross_road <- data_cross_road[,c("FID_research_grid","COUNT_FID_CrossRoad_Project")]

data_light <- read.csv('./data/For model two/light_id_stastics.txt')
data_light <- data_light[,c("FID_research_grid" ,"COUNT_OBJECTID" ,"SUM_Flux_de_la_lampe__en_Lumen_")]
names(data_light)[2]<- 'light_count'
names(data_light)[3]<- 'sum_light_flux'

data_traffic_light <- read.csv('./data/For model two/traffic_light_id_statics.txt')
data_traffic_light <- data_traffic_light[,c('FID_research_grid', 'FREQUENCY')]
names(data_traffic_light)[2]<- 'tfc_light_count'

######################################################################################
df <- research_grid@data
df1 <- df %>% left_join( data_all_1, by = c('id' = 'FID_research_grid'))
df2 <- df1 %>% left_join(data_all_2_bandes, by = c('id' = 'FID_research_grid'))
df3 <- df2 %>% left_join(data_all_2_pista, by = c('id' = 'FID_research_grid'))
df4 <- df3 %>% left_join(data_all_2_bus, by = c('id' = 'FID_research_grid'))
df5 <- df4 %>% left_join(data_all_3_limit_20, by = c('id' = 'FID_research_grid'))
df6 <- df5 %>% left_join(data_all_3_limit_30, by = c('id' = 'FID_research_grid'))
df7 <- df6 %>% left_join(data_all_3_limit_50 , by = c('id' = 'FID_research_grid'))
df8 <- df7 %>% left_join(data_all_3_limit_walk_way, by = c('id' = 'FID_research_grid'))
df9 <- df8 %>% left_join(data_altitude, by = c('id' = 'FID_research_grid'))
df10 <- df9 %>% left_join(data_carpark, by = c('id' = 'FID_research_grid'))
df11 <- df10 %>% left_join(data_cross_road, by = c('id' = 'FID_research_grid'))
df12 <- df11 %>% left_join(data_light, by = c('id' = 'FID_research_grid'))
df13 <- df12 %>% left_join(data_traffic_light, by = c('id' = 'FID_research_grid'))

df14 <- df13[,-c(6:9)]
summary(df14)
#       id           cyclbl_         OBJECTID     FREQUENCY      SUM_lngth1_x    
# Min.   :  1.0   Min.   :    0   Min.   :  2   Min.   : 1.00   Min.   :  150.3  
# 1st Qu.:103.2   1st Qu.: 1604   1st Qu.: 93   1st Qu.: 4.00   1st Qu.: 4296.6  
# Median :205.5   Median : 3511   Median :184   Median :11.00   Median : 7552.1  
# Mean   :205.5   Mean   : 4096   Mean   :184   Mean   :12.81   Mean   : 8136.0  
# 3rd Qu.:307.8   3rd Qu.: 5860   3rd Qu.:275   3rd Qu.:19.00   3rd Qu.:10550.0  
# Max.   :410.0   Max.   :18668   Max.   :366   Max.   :51.00   Max.   :32804.5  
#                                 NA's   :45    NA's   :45      NA's   :45    

#   MEAN_tf_q_x      MEAN_tf_k_x     SUM_bs_stp__x      MEAN_pred     sum_len_road_bandes
#  Min.   :   0.0   Min.   : 0.000   Min.   :  0.00   Min.   : 0.00   Min.   :  10.18    
#  1st Qu.: 201.5   1st Qu.: 2.902   1st Qu.:  4.00   1st Qu.:17.26   1st Qu.: 424.67    
#  Median : 362.1   Median : 4.646   Median : 24.00   Median :27.90   Median :1004.96    
#  Mean   : 609.4   Mean   : 5.157   Mean   : 40.35   Mean   :26.23   Mean   :1358.97    
#  3rd Qu.: 645.4   3rd Qu.: 6.939   3rd Qu.: 58.00   3rd Qu.:34.96   3rd Qu.:1868.46    
#  Max.   :5023.8   Max.   :17.912   Max.   :243.00   Max.   :69.67   Max.   :9129.72    
#  NA's   :45       NA's   :45       NA's   :45       NA's   :45      NA's   :191      

# sum_len_road_pista sum_len_road_bus   sum_len_limit_50.x sum_len_limit30   
# Min.   :   15.04   Min.   :   16.71   Min.   :  45.85    Min.   :   9.739  
# 1st Qu.: 2207.66   1st Qu.:  672.45   1st Qu.: 157.46    1st Qu.: 375.278  
# Median : 4267.49   Median : 1810.45   Median : 276.05    Median : 738.494  
# Mean   : 4964.94   Mean   : 2179.26   Mean   : 544.17    Mean   :1594.541  
# 3rd Qu.: 6802.98   3rd Qu.: 3018.44   3rd Qu.: 706.39    3rd Qu.:1850.500  
# Max.   :23955.31   Max.   :10004.84   Max.   :2867.76    Max.   :7961.993  
# NA's   :126        NA's   :195        NA's   :323        NA's   :274       

# sum_len_limit_50.y sum_len_walk_way    STD_altitude    carpark_count  
# Min.   :   48.79   Min.   :   65.05   Min.   : 0.000   Min.   :  1.0  
# 1st Qu.: 2325.92   1st Qu.:  342.65   1st Qu.: 1.109   1st Qu.: 33.0  
# Median : 4772.16   Median :  661.30   Median : 1.989   Median :176.0  
# Mean   : 5426.93   Mean   : 1208.97   Mean   : 2.834   Mean   :158.5  
# 3rd Qu.: 7954.33   3rd Qu.: 1321.14   3rd Qu.: 3.578   3rd Qu.:245.0  
# Max.   :26542.26   Max.   :11638.42   Max.   :17.281   Max.   :437.0  
# NA's   :60         NA's   :273        NA's   :31       NA's   :32     

# COUNT_FID_CrossRoad_Project  light_count     sum_light_flux     tfc_light_count 
# Min.   :  2.0               Min.   :   1.0   Min.   :   15380   Min.   :   2.0  
# 1st Qu.: 64.0               1st Qu.: 314.8   1st Qu.: 3054784   1st Qu.: 169.0  
# Median :138.0               Median : 524.0   Median : 4694180   Median : 304.5  
# Mean   :132.9               Mean   : 578.3   Mean   : 5059366   Mean   : 327.9  
# 3rd Qu.:186.5               3rd Qu.: 696.2   3rd Qu.: 6240556   3rd Qu.: 462.5  
# Max.   :670.0               Max.   :2990.0   Max.   :33965675   Max.   :1057.0  
# NA's   :11                  NA's   :14       NA's   :14         NA's   :52     

############################################################################
# var to Y and X
df_y <- read.csv('.\\data\\bicycle_accidents\\bicycle_accident_with_gridnet.csv')
df15 <- df14 %>% left_join(df_y[,c('id','bike_accident','bike_accdt_night')],by = 'id')

# drop null rows
dataset <- df15[,-c(2:4)]
library(mice)
dataset <- dataset[complete.cases(dataset$SUM_lngth1_x),]


summary(dataset)

dataset$y <- (dataset$bike_accident / 
               (dataset$SUM_lngth1_x / 1000)) /
                 (dataset$MEAN_pred+10)*(365*24/10000)
summary(dataset$y)


dataset$X1 <- dataset$MEAN_tf_q_x  #tf_q
dataset$x2 <- dataset$MEAN_tf_k_x  #tf_k
dataset$x3 <- dataset$SUM_bs_stp__x*1000  / dataset$SUM_lngth1_x #number of bus stop per km, you chong fu zhi
dataset$x4 <- dataset$sum_len_road_bandes / dataset$SUM_lngth1_x # % of bandes 
dataset$x5 <- dataset$sum_len_road_pista  / dataset$SUM_lngth1_x # % of pista
dataset$x6 <- dataset$sum_len_road_bus    / dataset$SUM_lngth1_x # % of bus line
dataset$x7 <- dataset$sum_len_limit_20    / dataset$SUM_lngth1_x # % of limit 20km/h
dataset$x8 <- dataset$sum_len_limit30     / dataset$SUM_lngth1_x # % of limit 30km/h
dataset$x9 <- dataset$sum_len_limit_50    / dataset$SUM_lngth1_x # % of limit 50km/h
dataset$x10 <- dataset$sum_len_walk_way   / dataset$SUM_lngth1_x # % of walk way
dataset$x11 <- dataset$STD_altitude  # altitude to calculate the slope of road
dataset$x12 <- (dataset$carpark_count/1000)      / dataset$SUM_lngth1_x # num of car parking per km
dataset$x13 <- (dataset$COUNT_FID_CrossRoad_Project/1000) / dataset$SUM_lngth1_x # density of cross road
dataset$x14 <- (dataset$tfc_light_count/1000)    / dataset$SUM_lngth1_x # number of traffic signal light per km
dataset$x15 <- (dataset$light_count/1000)        / dataset$SUM_lngth1_x # num of public street light per km
dataset$x16 <- (dataset$sum_light_flux/1000) / dataset$SUM_lngth1_x # public street light flux per km

dataset_combined <- dataset[,c('id',"y","X1", "x2",
                                "x3","x4","x5",
                                "x6","x7","x8",
                                "x9","x10","x11",
                                "x12","x13","x14",
                                "x15","x16" )]

write.csv(dataset_combined,'./data/For model two/dataset_combined.csv',row.names = F)
write.csv(dataset,'./data/For model two/dataset_0109.csv',row.names = F)

########################################################################################



############################################################################################################

# library(jtools)
# 
# # plot regression coefficients for poisson.model2
# plot_summs(m3,m4, scale = TRUE, exp = TRUE)
# # m4 better
# 
# # cha kan jiaohu
# library(interactions)
# interact_plot(m4)
















