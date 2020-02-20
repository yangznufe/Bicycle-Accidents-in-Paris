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
#load paris map
p_map <- readOGR(".\\data\\paris\\paris_merge.shp")
paris_map <- readOGR(".\\data\\paris\\paris_merge_projection2.shp")
plot(paris_map)

#funcion of make grid
make_grid <- function(x, type, cell_width, cell_area, clip = FALSE) {
        if (!type %in% c("square", "hexagonal")) {
                stop("Type must be either 'square' or 'hexagonal'")
        }
        
        if (missing(cell_width)) {
                if (missing(cell_area)) {
                        stop("Must provide cell_width or cell_area")
                } else {
                        if (type == "square") {
                                cell_width <- sqrt(cell_area)
                        } else if (type == "hexagonal") {
                                cell_width <- sqrt(2 * cell_area / sqrt(3))
                        }
                }
        }
        # buffered extent of study area to define cells over
        ext <- as(extent(x) + cell_width, "SpatialPolygons")
        projection(ext) <- projection(x)
        # generate grid
        if (type == "square") {
                g <- raster(ext, resolution = cell_width)
                g <- as(g, "SpatialPolygons")
        } else if (type == "hexagonal") {
                # generate array of hexagon centers
                g <- spsample(ext, type = "hexagonal", cellsize = cell_width, offset = c(0, 0))
                # convert center points to hexagons
                g <- HexPoints2SpatialPolygons(g, dx = cell_width)
        }
        
        # clip to boundary of study area
        if (clip) {
                g <- gIntersection(g, x, byid = TRUE)
        } else {
                g <- g[x, ]
        }
        # clean up feature IDs
        row.names(g) <- as.character(1:length(g))
        return(g)
}

hex_grid <- make_grid(paris_map, type = "hexagonal", cell_area = 600*500, clip = FALSE)
plot(paris_map, col = "grey50", bg = "light blue", axes = FALSE)
plot(hex_grid, border = "orange", add = TRUE)
box()

#size <- 1000
#hex_points <- spsample(paris_map, type = "hexagonal", cellsize = size)
#hex_grid <- HexPoints2SpatialPolygons(hex_points, dx = size)
#plot(paris_map, col = "grey50", bg = "light blue", axes = TRUE)
#plot(hex_points, col = "black", pch = 20, cex = 0.5, add = T)
#plot(hex_grid, border = "orange", add = T)
#plot(bike_accdt, add = T)

#########################################################################
#load bicycle accidets data
bike_accident <- read.csv('.\\data\\bicycle_accidents\\2016_17_18_BicycleAccidents.csv')
coordinates(bike_accident) <- ~ long + lat
crs(bike_accident) <- crs(p_map)
#crs(hex_grid) <- crs(paris_map)
bike_accdt <- spTransform(bike_accident, crs(hex_grid))
summary(hex_grid)
summary(bike_accdt)


#count the number of each grid
#res <- over(hex_grid, bike_accdt)
#colSums(gContains(hex_grid, bike_accdt, byid = TRUE)) -> res
fill_missing <- expand.grid(id = row.names(hex_grid), stringsAsFactors = FALSE)
point_density <- over(hex_grid, bike_accdt, returnList = TRUE) %>% 
        plyr::ldply(.fun = function(x) x, .id = "id") %>%
        mutate(id = as.character(id)) %>% 
        count(id) %>% 
        left_join(fill_missing, ., by = c("id")) %>%
        # log transform
        mutate(n = ifelse(is.na(n), 0, n)) %>% 
        #spread(family, n, fill = -1) %>% 
        SpatialPolygonsDataFrame(hex_grid, .)

spplot(point_density,'n',
       main = "Paris Bicycle Accidents 2016-2018",
       col.regions = c("grey20", viridis(255)),
       xlim = bbexpand(bbox(point_density)[1, ], 0.04), 
       ylim = bbexpand(bbox(point_density)[2, ], 0.04),
       par.strip.text = list(col = "white"),
       par.settings = list(
               strip.background = list(col = "grey40"))
       )


