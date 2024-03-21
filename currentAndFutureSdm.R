# scripts for generating current and future Species Distribution Models

#### Start Current SDM ######
# 0. Load packages

# 1. Get occurrence Data 

# start with our data

occurrenceCoords<-read_csv("data/cleanedData.csv") %>%
  dplyr::select( decimalLongitude, decimalLatitude)

occurrenceSpatialPts <- SpatialPoints(occurrenceCoords, 
                                      proj4string = CRS("+proj=longlat"))

# now get the climate data
# make sure RAM is bumped up

worldclim_global(var="bio", res=2.5, path="data/", version="2.1") 

# update .gitignore to prevent huge files getting pushed to github

#Here are the meanings of the bioclimatic variables (bio1 to bio19) provided by WorldClim:
#bio1: Mean annual temperature
#bio2: Mean diurnal range (mean of monthly (max temp - min temp))
#bio3: Isothermality (bio2/bio7) (* 100)
#bio4: Temperature seasonality (standard deviation *100)
#bio5: Max temperature of warmest month
#bio6: Min temperature of coldest month
#bio7: Temperature annual range (bio5-bio6)
#bio8: Mean temperature of wettest quarter
#bio9: Mean temperature of driest quarter
#bio10: Mean temperature of warmest quarter
#bio11: Mean temperature of coldest quarter
#bio12: Annual precipitation
#bio13: Precipitation of wettest month
#bio14: Precipitation of driest month
#bio15: Precipitation seasonality (coefficient of variation)
#bio16: Precipitation of wettest quarter
#bio17: Precipitation of driest quarter
#bio18: Precipitation of warmest quarter
#bio19: Precipitation of coldest quarter

climList <- list.files(path = "data/wc2.1_2.5m/", 
                       pattern = ".tif$", 
                       full.names = T)
#Knowing the files needed to make a raster stack^
#A raster allows for the color coding to occur on top of the map
currentClimRasterStack <- raster::stack(climList)

plot(currentClimRasterStack[[1]]) 

plot(occurrenceSpatialPts, add = TRUE) 

#2. Create pseudo-absence points
#^Using where the data is as a model to vaguely guess where the species isn't ... which is also needed
mask <- raster(climList[[1]]) #Just picking 1 at random ... could be climList[[1--18]]
#Mask is information about the particular raster
geographicExtent <- extent(x = occurrenceSpatialPts)
#geographicExtent gets more specific in the raster and makes a bounding box -- highest lattitude and lowest lattitude and furthest longitude points
set.seed(45) 
#^Says to create random points in the data ... if you want to recreate the same random data use the particular num 45
backgroundPoints <- randomPoints(mask = mask, 
                                 n = nrow(occurrenceCoords), #same n, keeps what we know and what we don't equal
                                 ext = geographicExtent, 
                                 extf = 1.25, # draw a slightly larger area 
                                 warn = 0) 
#These are the random guesses of where the species ISN'T

colnames(backgroundPoints) <- c("longitude", "latitude")

# 3. Convert occurrence and environmental data into format for model

# Data for observation sites (presence and background), with climate data

occEnv <- na.omit(raster::extract(x = currentClimRasterStack, y = occurrenceCoords))

absenceEnv<- na.omit(raster::extract(x = currentClimRasterStack, y = backgroundPoints))

presenceAbsenceV <- c(rep(1, nrow(occEnv)), rep(0, nrow(absenceEnv))) 

presenceAbsenceEnvDf <- as.data.frame(rbind(occEnv, absenceEnv))

# 4. Create Current SDM with maxent

# If you get a Java error, restart R, and reload the packages
habronattusCurrentSDM <- dismo::maxent(x = presenceAbsenceEnvDf, ## env conditions
                                       p = presenceAbsenceV,   ## 1:presence or 0:absence
                                       path=paste("maxent_outputs"), #maxent output dir 
)                              


# 5. Plot the current SDM with ggplot

predictExtent <- 1.25 * geographicExtent 

geographicArea <- crop(currentClimRasterStack, predictExtent, snap = "in")
#^setting up the area of where we want to do our map 
habronattusPredictPlot <- raster::predict(habronattusCurrentSDM, geographicArea) 

raster.spdf <- as(habronattusPredictPlot, "SpatialPixelsDataFrame")

habronattusPredictDf <- as.data.frame(raster.spdf)

wrld <- ggplot2::map_data("world")

xmax <- max(habronattusPredictDf$x)
xmin <- min(habronattusPredictDf$x)
ymax <- max(habronattusPredictDf$y)
ymin <- min(habronattusPredictDf$y)

ggplot() +
  geom_polygon(data = wrld, mapping = aes(x = long, y = lat, group = group),
               fill = "grey75") +
  geom_raster(data = habronattusPredictDf, aes(x = x, y = y, fill = layer)) + 
  scale_fill_gradientn(colors = terrain.colors(10, rev = T)) +
  coord_fixed(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = F) +#expand=F fixes margin
  scale_size_area() +
  borders("state") +
  borders("world", colour = "black", fill = NA) + 
  labs(title = "SDM of Habronattus americanus Under Current Climate Conditions",
       x = "longitude",
       y = "latitude",
       fill = "Environmental Suitability")+ 
  theme(legend.box.background=element_rect(),legend.box.margin=margin(5,5,5,5)) 

ggsave("output/habronattusCurrentSdm.jpg",  width = 8, height = 6)

#### End Current SDM #########

#### Start Future SDM ########

# 6. Get Future Climate Projections

# CMIP6 is the most current and accurate modeling data
# More info: https://pcmdi.llnl.gov/CMIP6/

futureClimateRaster <- cmip6_world("CNRM-CM6-1", "585", "2061-2080", var = "bioc", res=2.5, path="data/cmip6")

# 7. Prep for the model
#feed the current and future weather data -- make sure columns for future and current match
names(futureClimateRaster)=names(currentClimRasterStack)

geographicAreaFutureC6 <- crop(futureClimateRaster, predictExtent)
#^Keeps it within our bounding box

# 8. Run the future SDM

habronattusFutureSDM <- raster::predict(habronattusCurrentSDM, geographicAreaFutureC6)
#^Combines current and future to do the modelling for the future SDM
# 9. Plot the future SDM

habronattusFutureSDMDf <- as.data.frame(habronattusFutureSDM, xy=TRUE)


xmax <- max(habronattusFutureSDMDf$x)
xmin <- min(habronattusFutureSDMDf$x)
ymax <- max(habronattusFutureSDMDf$y)
ymin <- min(habronattusFutureSDMDf$y)

ggplot() +
  geom_polygon(data = wrld, mapping = aes(x = long, y = lat, group = group),
               fill = "grey75") +
  geom_raster(data = habronattusFutureSDMDf, aes(x = x, y = y, fill = maxent)) + 
  scale_fill_gradientn(colors = terrain.colors(10, rev = T)) +
  coord_fixed(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = F) +
  scale_size_area() +
  borders("state") +
  borders("world", colour = "black", fill = NA) + 
  labs(title = "Future SDM of Habronattus americanus Under CMIP6 Climate Conditions",
       x = "longitude",
       y = "latitude",
       fill = "Env Suitability") +
  theme(legend.box.background=element_rect(),legend.box.margin=margin(5,5,5,5)) 
#The scale should be different here -- it should be from 0~0.16
ggsave("output/habronattusFutureSdm.jpg",  width = 8, height = 6)

##### END FUTURE SDM ######