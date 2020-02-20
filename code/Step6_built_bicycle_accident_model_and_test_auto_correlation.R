# libraries
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

df <- read.csv('.\\data\\For model two\\dataset_combined.csv')
summary(df)
# id              y                 X1               x2               x3         
# Min.   :  3.0   Min.   :0.00000   Min.   :   0.0   Min.   : 0.000   Min.   : 0.0000  
# 1st Qu.:113.0   1st Qu.:0.01151   1st Qu.: 201.5   1st Qu.: 2.902   1st Qu.: 0.8402  
# Median :216.0   Median :0.02502   Median : 362.1   Median : 4.646   Median : 3.1294  
# Mean   :212.4   Mean   :0.05655   Mean   : 609.4   Mean   : 5.157   Mean   : 5.0020  
# 3rd Qu.:314.0   3rd Qu.:0.05432   3rd Qu.: 645.4   3rd Qu.: 6.939   3rd Qu.: 6.8895  
# Max.   :409.0   Max.   :3.88535   Max.   :5023.8   Max.   :17.912   Max.   :59.1377  
# 
# x4                x5                x6                x7                x8         
# Min.   :0.00438   Min.   :0.00159   Min.   :0.00085   Min.   :0.00434   Min.   :0.00124  
# 1st Qu.:0.05100   1st Qu.:0.36206   1st Qu.:0.11659   1st Qu.:0.01551   1st Qu.:0.04182  
# Median :0.11639   Median :0.61443   Median :0.19786   Median :0.03187   Median :0.12747  
# Mean   :0.18860   Mean   :0.59312   Mean   :0.28285   Mean   :0.06308   Mean   :0.20034  
# 3rd Qu.:0.22433   3rd Qu.:0.81386   3rd Qu.:0.36624   3rd Qu.:0.07432   3rd Qu.:0.31572  
# Max.   :1.00000   Max.   :1.00000   Max.   :1.00000   Max.   :0.70084   Max.   :1.00000  
# NA's   :146       NA's   :81        NA's   :150       NA's   :278       NA's   :229      
#        x9               x10               x11              x12                x13           
#  Min.   :0.02308   Min.   :0.00611   Min.   : 0.000   Min.   :0.000000   Min.   :0.0000003  
#  1st Qu.:0.46546   1st Qu.:0.03642   1st Qu.: 1.206   1st Qu.:0.000009   1st Qu.:0.0000118  
#  Median :0.68364   Median :0.07537   Median : 2.039   Median :0.000021   Median :0.0000180  
#  Mean   :0.67483   Mean   :0.12852   Mean   : 2.964   Mean   :0.000033   Mean   :0.0000305  
#  3rd Qu.:0.95646   3rd Qu.:0.17076   3rd Qu.: 3.680   3rd Qu.:0.000034   3rd Qu.:0.0000299  
#  Max.   :1.00000   Max.   :0.63074   Max.   :17.281   Max.   :0.001424   Max.   :0.0008033  
#  NA's   :15        NA's   :228       NA's   :11       NA's   :11         NA's   :1          
# x14                x15                 x16          
# Min.   :0.000000   Min.   :0.0000002   Min.   : 0.00164  
# 1st Qu.:0.000025   1st Qu.:0.0000459   1st Qu.: 0.43570  
# Median :0.000042   Median :0.0000722   Median : 0.62230  
# Mean   :0.000064   Mean   :0.0001306   Mean   : 1.25218  
# 3rd Qu.:0.000067   3rd Qu.:0.0001107   3rd Qu.: 0.97135  
# Max.   :0.001799   Max.   :0.0023218   Max.   :34.50120  
# NA's   :25         NA's   :1           NA's   :1       

#####  OUTLIER               #############################################################
outlier_id = 1
df = df[as.numeric(df$y) < 2,]
densityplot(log(df$y))



##### Explore Missing Value  ##############################################################

library('VIM')
VIM::aggr(df,prop= F, numbers = T)

VIM::matrixplot(df,prop= F, numbers = T)

df$y_log = log(df$y)
df$x4[is.na(df$x4)] <- 0
df$x5[is.na(df$x5)] <- 0
df$x6[is.na(df$x6)] <- 0
df$x7[is.na(df$x7)] <- 0
df$x8[is.na(df$x8)] <- 0
df$x9[is.na(df$x9)] <- 0
df$x10[is.na(df$x10)] <- 0
df$x11[is.na(df$x11)] <- median(df$x11,na.rm = T)
df$x12[is.na(df$x12)] <- 0
df$x13[is.na(df$x13)] <- median(df$x13,na.rm = T)
df$x14[is.na(df$x14)] <- median(df$x14,na.rm = T)
df$x15[is.na(df$x15)] <- median(df$x15,na.rm = T)
df$x16[is.na(df$x16)] <- median(df$x16,na.rm = T)
summary(df)

#########################################################################################

fit1 <- lm(y ~ X1 + x2 + x3 + x4 + x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15+x16,data = df)
summary(fit1)
fit2 <- step(fit1)
summary(fit2)
# Call:
#   lm(formula = y ~ x2 + x5 + x8 + x9 + x12 + x13 + x14 + x15, data = df)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.12474 -0.02629 -0.00923  0.01206  0.56071 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  5.552e-02  1.026e-02   5.413 1.14e-07 ***
#   x2          -4.359e-03  9.758e-04  -4.467 1.07e-05 ***
#   x5           1.788e-02  1.102e-02   1.622 0.105736    
# x8           4.665e-02  2.275e-02   2.050 0.041057 *  
#   x9          -2.408e-02  1.464e-02  -1.644 0.100981    
# x12          4.032e+02  8.869e+01   4.546 7.51e-06 ***
#   x13          9.925e+02  1.741e+02   5.702 2.49e-08 ***
#   x14         -1.850e+02  9.165e+01  -2.019 0.044274 *  
#   x15         -9.574e+01  2.569e+01  -3.726 0.000226 ***
#   ---
#   Signif. codes:  
#   0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.06093 on 355 degrees of freedom
# Multiple R-squared:  0.3253,	Adjusted R-squared:  0.3101 
# F-statistic:  21.4 on 8 and 355 DF,  p-value: < 2.2e-16
hist(fit2$residuals)

fit_x8 <- lm(y ~ X1 ,data = df)
summary(fit_x8)
##########################################################################################
###########    GWR   ####################################################################

#library a bunch of packages we may (or may not) use - install them first if not installed already. 
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



selected_grid <- readOGR('./data/research grid/Export_Output.shp')
Grid_SF　<- st_as_sf(selected_grid)
qtm(Grid_SF)

# Grid_Profiles <- left_join(Grid_SF,df,by = c('id'='id') )
Grid_Profiles <- Grid_SF
qtm(Grid_Profiles,fill = 'res',borders = NULL)


df_fit1 <- df
df_fit1$fit2_resdiual <- residuals(fit2)

write.csv(df_fit1,'dataset_with_residuals.csv')

Grid_with_residual <- left_join(Grid_SF,df_fit1,by = 'id')
vif(fit2)


# uing durbin to check auto-correlation
durbinWatsonTest(fit2)
# lag Autocorrelation D-W Statistic p-value
# 1      -0.0488182      2.092952   0.362
# Alternative hypothesis: rho != 0

# check spatial auto-correlation
tmap_mode("view")
#qtm(LonWardProfiles, fill = "model1_resids")

tm_shape(Grid_with_residual) +
  tm_polygons("fit2_resdiual",
              palette = "RdYlBu") 

################################################################################################
##### Moran'sI  ###############################################################################


#Firstly convert our SF object into an SP object:
Grid_with_residual_SP <- as(Grid_Profiles,"Spatial")
names(Grid_with_residual_SP)

#and calculate the centroids of all Wards in London
Grid_sW <- coordinates(Grid_with_residual_SP)
plot(Grid_sW)

#Now we need to generate a spatial weights matrix (remember from the lecture a couple of weeks ago). We'll start with a simple binary matrix of queen's case neighbours
library(spdep)
#create a neighbours list of queens contiguity
Grid_nb <- poly2nb(Grid_with_residual_SP,queen = T)

#or nearest neighbours
knn_grids <- knearneigh(Grid_sW, k=6)

grid_knn <- knn2nb(knn_grids)

#plot them
plot(Grid_nb, coordinates(Grid_sW), col="red")
plot(grid_knn, coordinates(Grid_sW), col="blue")

#create a spatial weights matrix object from these weights
Grid.nb_weight <- nb2listw(Grid_nb, style="C")
Grid.knn_4_weight <- nb2listw(grid_knn, style="C")

#now run a moran's I test on the residuals
#first using queens neighbours
moran.test(Grid_with_residual_SP@data$res, Grid.knn_4_weight)

#######  Moran I test under randomisation  #######
# 
# data:  Grid_with_residual_SP@data$res  
# weights: Grid.knn_4_weight    
# 
# Moran I statistic standard deviate = 0.10863, p-value =  0.4567
#   
# alternative hypothesis: greater
# sample estimates:
#   Moran I statistic       Expectation          Variance 
#        0.0002726676     -0.0027548209      0.0007767893 
