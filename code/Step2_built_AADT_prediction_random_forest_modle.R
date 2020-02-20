#### libraries  #############################
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

#########################
#load data ######################
dataset_all_street1 <- read.csv('.\\prediction_bike_flow\\bike_road_agged.csv')
dataset_all_street3 <- read.csv('.\\prediction_bike_flow\\bike_road_agged3.csv')
dataset_all_street4 <- read.csv('.\\prediction_bike_flow\\bike_road_agged4.csv')

counted_road_data <- read.csv("./prediction_bike_flow/counted_road4.csv")
# counted_road$voie <- counted_road_data$voie
# counted_road_data <- counted_road_data[,c("voie","count")]

# 3 na value , so no work
dataset_counter <- counted_road_data %>%
  left_join(dataset_all_street4[,c("voie",'statut1','tw_wy_d',
                                   'length1','rod_typ',"rd_typ_p")],
            by = c('voie'='voie'))

summary(dataset_counter)
densityplot(dataset_counter$count)
# voie               count              tf_q             tf_k            p__20       
# Length:46          Min.   :  5.857   Min.   : 211.9   Min.   : 1.334   Min.   :  27.0  
# Class :character   1st Qu.: 33.072   1st Qu.: 454.2   1st Qu.: 5.165   1st Qu.: 592.0  
# Mode  :character   Median : 55.375   Median : 509.1   Median : 6.667   Median : 875.7  
#                    Mean   : 68.643   Mean   : 675.0   Mean   : 7.227   Mean   : 893.5  
#                    3rd Qu.: 77.556   3rd Qu.: 697.0   3rd Qu.: 8.284   3rd Qu.:1222.3  
#                    Max.   :217.918   Max.   :3775.5   Max.   :16.413   Max.   :2110.1  
#                                      NA's   :2        NA's   :2        NA's   :2     

#      p_nm_x         prkng_nm_x       bs_stp_nm_       bk_qp_nm_x       ed_nm_x      
#  Min.   :  1.00   Min.   : 0.000   Min.   : 0.000   Min.   : 0.00   Min.   : 0.000  
#  1st Qu.: 14.25   1st Qu.: 1.000   1st Qu.: 0.000   1st Qu.: 5.00   1st Qu.: 0.250  
#  Median : 57.50   Median : 2.000   Median : 2.000   Median :10.50   Median : 4.000  
#  Mean   : 95.13   Mean   : 5.652   Mean   : 5.065   Mean   :20.09   Mean   : 6.283  
#  3rd Qu.:138.25   3rd Qu.: 7.750   3rd Qu.: 8.500   3rd Qu.:28.00   3rd Qu.: 9.500  
#  Max.   :335.00   Max.   :30.000   Max.   :27.000   Max.   :94.00   Max.   :35.000  
#                                                                                     
#    rstrnt_nm_         p_nm_y         prkng_nm_y       bs_stp_n_1       bk_qp_nm_y   
#  Min.   :  0.00   Min.   :  1.00   Min.   : 0.000   Min.   : 0.000   Min.   : 0.00  
#  1st Qu.:  3.25   1st Qu.: 14.25   1st Qu.: 1.000   1st Qu.: 0.000   1st Qu.: 5.00  
#  Median : 14.50   Median : 57.50   Median : 2.000   Median : 2.000   Median :10.50  
#  Mean   : 36.11   Mean   : 95.13   Mean   : 5.652   Mean   : 5.065   Mean   :20.09  
#  3rd Qu.: 49.00   3rd Qu.:138.25   3rd Qu.: 7.750   3rd Qu.: 8.500   3rd Qu.:28.00  
#  Max.   :137.00   Max.   :335.00   Max.   :30.000   Max.   :27.000   Max.   :94.00  
#                                                                                     
#     ed_nm_y         rstrnt_n_1      job_density      pt_density       pt_avail_r    
#  Min.   : 0.000   Min.   :  0.00   Min.   : 70.0   Min.   : 22.00   Min.   : 65.20  
#  1st Qu.: 0.250   1st Qu.:  3.25   1st Qu.:132.0   1st Qu.: 34.67   1st Qu.: 86.20  
#  Median : 4.000   Median : 14.50   Median :176.2   Median : 79.50   Median : 94.40  
#  Mean   : 6.283   Mean   : 36.11   Mean   :186.5   Mean   : 70.38   Mean   : 90.61  
#  3rd Qu.: 9.500   3rd Qu.: 49.00   3rd Qu.:229.5   3rd Qu.: 89.75   3rd Qu.: 97.76  
#  Max.   :35.000   Max.   :137.00   Max.   :440.0   Max.   :131.00   Max.   :100.00  
#                                                                                     
#               statut1   tw_wy_d     length1       
#  Aire piÃ©tonne   : 1   Non:28   Min.   :  125.6  
#  Voie 30          : 3   Oui:18   1st Qu.:  618.0  
#  Voie 50          :39            Median : 1839.0  
#  Zone 30          : 2            Mean   : 2170.1  
#  Zone de rencontre: 1            3rd Qu.: 2710.7  
#                                  Max.   :10190.1  
#                                                   
#                                                                           rod_typ  
#  Autres itinÃ©raires cyclables (ex : Aires piÃ©tonnes - Contre-sens cyclables): 3  
#  Bandes cyclables                                                             : 2  
#  Couloirs de bus ouverts aux vÃ©los                                           : 7  
#  Pistes cyclables                                                             :34  
#                                                                                    
#                                                                                    
#                                                                                    
#     rd_typ_p      
#  Min.   :  31.24  
#  1st Qu.: 576.32  
#  Median :1378.28  
#  Mean   :1946.14  
#  3rd Qu.:2524.48  
#  Max.   :8250.58  
#  NA's   :9   

##################################################################################
########## identity y is poisson  or  Negative Binomial  #########################
######  http://www.di.fc.ul.pt/~jpn/r/distributions/fitting.html  ################
x = as.integer(dataset_counter$count)
library(MASS)
library(broom)
d <- rpois(50,200)
broom::glance(fitdistr(x,"poisson"))
# lambda  
# 68.195652 
# ( 1.217585)
#         n  logLik AIC   BIC
#     <int>  <dbl> <dbl> <dbl>
#   1    46  -903. 1808. 1810.
broom::glance(fitdistr(x, "Negative Binomial"))
# size          mu    
# 2.0259348   68.1956522 
# ( 0.4093554) ( 7.1683943)
#         n logLik   AIC   BIC
#     <int>  <dbl> <dbl> <dbl>
#   1    46  -235.  475.  478.
#### Result shows that we should use Negative Binomial model #####################
##################################################################################

#################################################################################
###########  NA VALUE  ##########################################################
###### In Future, USE K-MEAN TO INSERT VALUE ####################################
# drop na should be improved,using other function
dataset_counter$rd_typ_p[is.na(dataset_counter$rd_typ_p)] <-0

# using mean
dataset_counter$tf_q[is.na(dataset_counter$tf_q)] <- mean(dataset_counter$tf_q)
dataset_counter$tf_q[is.na(dataset_counter$tf_k)] <- mean(dataset_counter$tf_k)
dataset_counter$tf_q[is.na(dataset_counter$p__20)] <- mean(dataset_counter$p__20)
#################################################################################

################################################################################
########### Change Data to x ##################################################
#change data type
dataset_counter$arrdt1 <- as.character(dataset_counter$arrdt1)
dataset_counter$count <- as.integer(dataset_counter$count)

#
dataset_counter$y <- dataset_counter$count
# dataset_counter$x_arrdt <- dataset_counter$arrdt1
dataset_counter$x1_statut <- dataset_counter$statut1
dataset_counter$x2_two_way <- dataset_counter$tw_wy_d
dataset_counter$x3_type <- dataset_counter$rod_typ
# dataset_counter$x4_type_b <- dataset_counter$rd_typ_bn / dataset_counter$length1
# dataset_counter$x5_type_bus <- dataset_counter$rd_typ_bs / dataset_counter$length1
dataset_counter$x6_type_pistes <- dataset_counter$rd_typ_p / dataset_counter$length1
# dataset_counter$x7_type_other <- dataset_counter$rd_typ_t / dataset_counter$length1
dataset_counter$x8_pop_ind <- dataset_counter$p__20
dataset_counter$x9_tf_q <- dataset_counter$tf_q 
dataset_counter$x10_tf_k <- dataset_counter$tf_k
dataset_counter$x11_poi <- dataset_counter$p_nm_x / dataset_counter$length1
dataset_counter$x12_park <- dataset_counter$prkng_nm_x / dataset_counter$length1
dataset_counter$x13_bus <- dataset_counter$bs_stp_nm_ / dataset_counter$length1
dataset_counter$x14_bike_eqp <- dataset_counter$bk_qp_nm_x / dataset_counter$length1
dataset_counter$x15_edu <- dataset_counter$ed_nm_x / dataset_counter$length1
dataset_counter$x16_food <- dataset_counter$rstrnt_nm_ / dataset_counter$length1
dataset_counter$x17_job_d <- dataset_counter$job_density
dataset_counter$x18_pt_density <- dataset_counter$pt_density
dataset_counter$x19_pt_avail_r <- dataset_counter$pt_avail_r

#################################################################################
dataset <- dataset_all_street4
dataset$rd_typ_p[is.na(dataset$rd_typ_p)] <-0

#change data type
dataset$arrdt1 <- as.character(dataset$arrdt1)
dataset$count <- as.integer(dataset$count)

#
dataset$x1_statut <- dataset$statut1
dataset$x2_two_way <- dataset$tw_wy_d
dataset$x3_type <- dataset$rod_typ
# dataset$x4_type_b <- dataset$rd_typ_bn / dataset$length1
# dataset$x5_type_bus <- dataset$rd_typ_bs / dataset$length1
dataset$x6_type_pistes <- dataset$rd_typ_p / dataset$length1
# dataset$x7_type_other <- dataset$rd_typ_t / dataset$length1
dataset$x8_pop_ind <- dataset$p__2010
dataset$x9_tf_q <- dataset$tf_q 
dataset$x10_tf_k <- dataset$tf_k
dataset$x11_poi <- dataset$poi_num / dataset$length1
dataset$x12_park <- dataset$prkng_n / dataset$length1
dataset$x13_bus <- dataset$bs_stp_ / dataset$length1
dataset$x14_bike_eqp <- dataset$bk_qp_n / dataset$length1
dataset$x15_edu <- dataset$edu_num / dataset$length1
dataset$x16_food <- dataset$rstrnt_ / dataset$length1
dataset$x17_job_d <- dataset$job_density
dataset$x18_pt_density <- dataset$pt_density
dataset$x19_pt_avail_r <- dataset$pt_avail_r
#######################################################################################
########## Drop na of rows(na value more than 20%) ####################################
library("DMwR")
dataset1 <- dataset[-manyNAs(dataset,0.2),]


#######################################################################################
############## Build Regression Model #############################################################
# 1. possion regression
m <- glm(y ~ x1_statut + x2_two_way + x3_type +x6_type_pistes +
            x8_pop_ind + x9_tf_q + x10_tf_k +
            x11_poi + x12_park + x13_bus + 
           x14_bike_eqp + x15_edu + x16_food + 
           x17_job_d + x18_pt_density + x19_pt_avail_r, 
         family="poisson", data=dataset_counter)
summary(m)

# 2. negative binomial regression
m2 <- glm(y ~ x1_statut + x2_two_way + x3_type +x6_type_pistes +
            x8_pop_ind + x9_tf_q + x10_tf_k +
            x11_poi + x12_park + x13_bus + 
            x14_bike_eqp + x15_edu + x16_food + 
            x17_job_d + x18_pt_density + x19_pt_avail_r, 
          family=negative.binomial(theta=1), data=dataset_counter)
summary(m2)

# 3. step-wise of negative binomial regression,It is greater!!!!!!!!!!
m3 <- step(m2)
summary(m3)


# 4. model test
#     Overdispersion value = 0.2996134,less than 1
plot(m3)

deviance(m3)/df.residual(m3)

confint(m3)

library(car)
car::vif(m3)
# 4. model of log(y)
m4 <- glm(log(y) ~ x1_statut + x3_type + x6_type_pistes + x8_pop_ind + 
            x9_tf_q + x10_tf_k + x11_poi + x15_edu  + x18_pt_density, 
          family = negative.binomial(theta = 1), 
          data = dataset_counter)
summary(m4)
plot(m4)

##############################################################################################
# compare model use anova  https://www.dataquest.io/blog/tutorial-poisson-regression-in-r/
print("R square:")
print(1-(mmmm2$deviance/mmmm2$null.deviance))
print(1-(m3$deviance/m3$null.deviance))
AIC(m4, m3)

# load library arm that contains the function se.coef()
library(arm)

# extract coefficients from first model using 'coef()'
coef1 = coef(m3)
coef2 = coef(m4)

# extract standard errors from first model using 'se.coef()'
se.coef1 = se.coef(m3)
se.coef2 = se.coef(m4)

# use 'cbind()' to combine values into one dataframe
cbind(coef1, se.coef1, coef2, se.coef2, exponent = exp(coef1))



################################################################################################
##############  BUILD MODEL USING library(MASS) ,why different result?  ########################
require(foreign)
require(ggplot2)
require(MASS)

mmm1 <- glm.nb(y ~ x1_statut + x2_two_way + x3_type +x6_type_pistes +
                       x8_pop_ind +  x10_tf_k +
                       x11_poi + x12_park + x13_bus + 
                       x14_bike_eqp + x15_edu  + 
                       x17_job_d + x18_pt_density + x19_pt_avail_r, 
                     data = dataset_counter)

mmm2 <- glm.nb(y ~ x1_statut + x3_type + x6_type_pistes + x8_pop_ind + 
  x9_tf_q + x10_tf_k + x11_poi + x15_edu  + x18_pt_density, 
  data = dataset_counter)
summary(mmm2)
print("R square:")
print(1-(mmm2$deviance/mmm2$null.deviance))
plot(mmm2)

df <- dataset_counter[,c('y' ,  'x6_type_pistes' ,'x8_pop_ind' ,  'x10_tf_k' ,
                           'x11_poi' , 'x12_park' , 'x13_bus' , 
                           'x14_bike_eqp' , 'x15_edu'  , 'x9_tf_q',
                           'x17_job_d' , 'x18_pt_density' , 'x19_pt_avail_r')]
cor(df)
#################################################################################################
############# prediction bicycle flow ##########################################################
# make a dataframe with new data
#
#0, 2, 9  to 3,3,10
# dataset$x_arrdt[dataset$x_arrdt == '0'] <- '3'
# dataset$x_arrdt[dataset$x_arrdt == '2'] <- '3'
# dataset$x_arrdt[dataset$x_arrdt == '9'] <- '10'

# use 'predict()' to run model on new data
p <- predict(mmm2, dataset1, type = "response")
summary(p)
# df$depth[df$depth<10] <- 0
hist(p,breaks = 150,xlim = c(0,300))

##############################################################################################
############# plot in map ###################################################################

dataset$p <- p
#left join to street buffer
street_buffer@data <- street_buffer@data %>% left_join(
  dataset, by= 'voie'
)
# street_buffer@data$p.y
# tmap_style('bw')
tmap_tip()
tmap_mode('view')
data(street_buffer)
tm_shape(street_buffer) +
  tm_polygons(col = "p.y.y", palette = 'BuGn',title = "Bicycle Flow")+
  tm_tiles("Bicycle Flow of Paris 2019")+tm_borders(lty = .2)

street_buffer$p <- as.integer(street_buffer$p)
street_buffer$voie <- as.character(street_buffer$voie)
street_buffer$x1_arrdt <- as.character(street_buffer$arrdt1_x)
street_buffer@data <- street_buffer@data[,c('voie',"p","x1_arrdt.y", "x2_two_way.y"      
                                            ,"x3_type.y"      ,  "x4_type_b.y"   ,   "x5_type_bus.y"   ,   "x6_type_pistes.y"  
                                            ,"x7_type_other.y"  ,  "x8_pop_ind.y"   ,    "x9_tf_q.y"      ,    "x10_tf_k.y"        
                                            ,"x11_poi.y"      ,    "x12_park.y"     ,    "x13_bus.y"      ,    "x14_bike_eqp.y"    
                                            ,"x15_edu.y"      ,    "x16_food.y"     ,      "x1_statut")]
# shapefile(street_buffer,'./data/prediction_bike_flow/prediction/street_buffer_p.shp')

####################################################################################

####################################################################################
#####################################  m2  #########################################
# Call:
#   glm(formula = y ~ x1_statut + x2_two_way + x3_type + x4_type_b + 
#         x5_type_bus + x6_type_pistes + x7_type_other + x8_pop_ind + 
#         x9_tf_q + x10_tf_k + x11_poi + x12_park + x13_bus + x14_bike_eqp + 
#         x15_edu + x16_food, family = negative.binomial(theta = 1), 
#       data = dataset_counter)
# 
# Deviance Residuals: 
#   Min        1Q    Median        3Q       Max  
# -1.42069  -0.22405  -0.00077   0.14403   0.73576  
# 
# Coefficients: (2 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                3.3943834  0.5868593   5.784 4.31e-06 ***
#   x1_statutVoie 30                          -0.6990904  1.0339356  -0.676  0.50492    
# x1_statutVoie 50                          -1.2288667  1.0838154  -1.134  0.26721    
# x1_statutZone 30                          -1.6065551  1.0430342  -1.540  0.13558    
# x1_statutZone de rencontre                 0.9699795  0.9661824   1.004  0.32466    
# x2_two_wayOui                             -0.3013088  0.2653464  -1.136  0.26651    
# x3_typeBandes cyclables                    3.4521054  1.1543144   2.991  0.00602 ** 
#   x3_typeCouloirs de bus ouverts aux vÃ©los  1.6640098  0.8698076   1.913  0.06681 .  
# x3_typePistes cyclables                    0.9223590  0.9008134   1.024  0.31531    
# x4_type_b                                 -1.0458457  0.9522097  -1.098  0.28213    
# x5_type_bus                                       NA         NA      NA       NA    
# x6_type_pistes                             1.4583681  0.4899881   2.976  0.00623 ** 
#   x7_type_other                                     NA         NA      NA       NA    
# x8_pop_ind                                -0.0002919  0.0002538  -1.150  0.26052    
# x9_tf_q                                   -0.0007727  0.0002166  -3.567  0.00143 ** 
#   x10_tf_k                                   0.0877914  0.0545534   1.609  0.11963    
# x11_poi                                   -0.0035358  0.0061249  -0.577  0.56872    
# x12_park                                  -0.0498096  0.1204034  -0.414  0.68249    
# x13_bus                                    0.1090920  0.1286350   0.848  0.40414    
# x14_bike_eqp                               0.0092850  0.0130674   0.711  0.48369    
# x15_edu                                   -0.0805754  0.0595312  -1.353  0.18755    
# x16_food                                   0.0147542  0.0129459   1.140  0.26481    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for Negative Binomial(1) family taken to be 0.242426)
# 
# Null deviance: 24.9998  on 45  degrees of freedom
# Residual deviance:  8.7778  on 26  degrees of freedom
# AIC: 504.91
# 
# Number of Fisher Scoring iterations: 11
#############################################################################################

############################################################################################
###########################  m3  ###########################################################
# Call:
#   glm(formula = y ~ x3_type + x6_type_pistes + x9_tf_q + x12_park + 
#         x15_edu + x16_food, family = negative.binomial(theta = 1), 
#       data = dataset_counter)
# 
# Deviance Residuals: 
#   Min        1Q    Median        3Q       Max  
# -1.57071  -0.30910  -0.01188   0.16481   0.96275  
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                3.319e+00  2.960e-01  11.212 1.86e-13 ***
#   x3_typeBandes cyclables                    1.478e+00  4.570e-01   3.234 0.002572 ** 
#   x3_typeCouloirs de bus ouverts aux vÃ©los  3.232e-01  3.532e-01   0.915 0.365979    
# x3_typePistes cyclables                   -6.139e-05  4.475e-01   0.000 0.999891    
# x6_type_pistes                             1.142e+00  3.872e-01   2.949 0.005493 ** 
#   x9_tf_q                                   -5.035e-04  1.307e-04  -3.851 0.000451 ***
#   x12_park                                   9.738e-02  4.687e-02   2.078 0.044707 *  
#   x15_edu                                   -1.358e-01  4.519e-02  -3.005 0.004743 ** 
#   x16_food                                   1.552e-02  3.509e-03   4.423 8.24e-05 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for Negative Binomial(1) family taken to be 0.2230234)
# 
# Null deviance: 25.000  on 45  degrees of freedom
# Residual deviance: 11.086  on 37  degrees of freedom
# AIC: 485.22
# 
# Number of Fisher Scoring iterations: 8
############################################################################################


##########################################################################################
#### FOR Rubust prediction, Using Randem Forest !!!!!!!!!!!####################################
library(xgboost)
library(readr)
library(stringr)
library(caret)
library(car)
library(randomForest)

rf <-  randomForest(log(y) ~ x1_statut + x2_two_way + x3_type +
                            x6_type_pistes + x8_pop_ind + x9_tf_q + 
                            x10_tf_k + x11_poi + x12_park + x13_bus + 
                            x14_bike_eqp + x15_edu + x16_food + 
                            x17_job_d + x18_pt_density + x19_pt_avail_r,
                    data = dataset_counter,
                    na.action = na.roughfix, 
                    importance = T,
                    sampsize = 46,
                    maxnodes = 4)

rf
# Call:
#   randomForest(formula = log(y) ~ x1_statut + x2_two_way + x3_type +      x6_type_pistes + x8_pop_ind + x9_tf_q + x10_tf_k + x11_poi +      x12_park + x13_bus + x14_bike_eqp + x15_edu + x16_food +      x17_job_d + x18_pt_density + x19_pt_avail_r, data = dataset_counter,      importance = T, sampsize = 46, maxnodes = 4, na.action = na.roughfix) 
# Type of random forest: regression
# Number of trees: 500
# No. of variables tried at each split: 5
# 
# Mean of squared residuals: 0.6418704
# % Var explained: -1.44
importance(rf,type = 2)
#                IncNodePurity
# x1_statut         0.19764413
# x2_two_way        0.03609127
# x3_type           0.28880397
# x6_type_pistes    1.35346176**
# x8_pop_ind        0.51184101.
# x9_tf_q           0.61838180.
# x10_tf_k          2.56797819***
# x11_poi           1.09613812**
# x12_park          1.50204656**
# x13_bus           0.88037912*
# x14_bike_eqp      0.70105668*
# x15_edu           0.95146972*
# x16_food          0.79480522*
# x17_job_d         1.35519141**
# x18_pt_density    1.13179698**
# x19_pt_avail_r    1.16983832**

p <- predict(rf, dataset1, type = "response")
# transfor p to traffic flow value
pred <- exp(p)
hist(pred)

# add prediction value to dataset
dataset1$pred <- pred

#left join to cycleable street 
all_street4 <- readOGR('.\\prediction_bike_flow\\bike_road_agged4.shp')

all_street4@data <- all_street4@data %>% left_join(
  dataset1, by= 'voie'
)
#plot it in map, to test the prediction value
tmap_mode('plot')
data(all_street4)
tm_shape(all_street4) +
  tm_lines(lwd = 'pred',n = 12,col = 'pred',group = 'rod_typ',scale = 4,colorNA = 'Grey',showNA = T,palette = 'YlOrBr',legend.hist = T)

# change data type
all_street4$pred <- as.integer(all_street4$pred)
all_street4$voie <- as.character(all_street4$voie)
all_street4$arrdt1 <- as.character(all_street4$arrdt1)

# save
shapefile(all_street4,'./prediction_bike_flow/prediction/all_street4.shp',overwrite=TRUE)
write.csv(all_street4@data, './prediction_bike_flow/prediction/all_street4.csv')
