library(sf)
library(sp)
library(rgeos)
library(raster)
library(tidyverse)
library(dplyr)
library(caret)
library(rsample)
library(scales)

########################################################
# Open FIRMS file
########################################################

# Set CRS
wgs48 <- crs("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
# equalarea <- crs("+proj=eqdc +lat_0=0 +lon_0=0 +lat_1=-5 +lat_2=-42 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs") 

# Load South America Shapefile
sa_shp <- st_read("./Data/South America Shp/SouthAmerica.shp") 

# Load FIRMS Data
firms_path <-"./Data/FIRMS/2.22 FIRMS" %>%
  list.files(full.names = TRUE) %>% 
  map(~list.files(., pattern = ".shp$", full.names = TRUE)) %>% 
  unlist() 

vars <- c("LATITUDE", "LONGITUDE", "ACQ_DATE")
MODIS <- firms_path[grepl("MODIS", firms_path)] %>% st_read(stringsAsFactors = FALSE) %>% 
  st_transform(wgs48) %>% select(vars)
VNP <- firms_path[grepl("VNP", firms_path)] %>% st_read(stringsAsFactors = FALSE) %>% 
  st_transform(wgs48) %>% select(vars)
VIIRS <- firms_path[grepl("VIIRS", firms_path)] %>% st_read(stringsAsFactors = FALSE) %>% 
  st_transform(wgs48) %>% select(vars)


occurrence <- MODIS %>% rbind(VNP) %>% rbind(VIIRS)%>%
  mutate(dummy = 1) # if binary output

# Create Base Unit
base.unit <- 0.1 # unit: degree # the greater the base unit, the larger the disparencies of areas across gridcells
raster.base <- raster(ncol = 360/base.unit, 
                      nrow= 180/base.unit) %>% 
  crop(extent(st_transform(sa_shp, crs(raster()))))


wildfire <- data.frame(matrix(nrow = length(raster.base), ncol = 0))

dates <- c(occurrence$ACQ_DATE) %>% unique(); print(dates)

for (i in seq_along(dates)){
  date <- dates[i]
  
  byday <- occurrence %>% filter(ACQ_DATE == date) %>%
    {rasterize(., raster.base, .$dummy, fun = mean)}%>% 
    raster::getValues() %>% {.>0} %>% as.data.frame() %>% 
    `colnames<-`(date); wildfire <- bind_cols(wildfire, byday)
}

wildfire <- wildfire %>% mutate_all(~ replace_na(.,0)) 

locs <- raster.base %>% 
  rasterToPoints() %>% 
  as.data.frame() %>% 
  `colnames<-`(c("LONGITUDE", "LATITUDE")) %>% 
  mutate_all(~round(.,2))

wildfire <- bind_cols(wildfire, locs)
# wildfire$area <- area(raster.base) %>% raster::getValues()

########################################################
# Open World Climate files
########################################################


# World Climate, note: format is RasterStack
wc <- raster::getData('worldclim', res=10, var='bio')
wc.extract <- raster::extract(wc, wildfire[,9:10])
wildfire <- wildfire %>% cbind(wc.extract) %>% drop_na()


########################################################
# Fit a Model
########################################################

wildfire <- wildfire %>% dplyr::select(-c(as.character(max(dates)))) # remove the most recent date because it is incomplete

# Split the data into 2 dataframe with 6 days 
wildfire.1_6 <- wildfire[,-7] ; colnames(wildfire.1_6)[1:6] <- c(paste0("day", 1:6))
wildfire.2_7 <- wildfire[,-1] ; colnames(wildfire.2_7)[1:6] <- c(paste0("day", 1:6))

# Create Background Data
set.seed(233)
presence <- which(wildfire.1_6$day6 == 1)
presence <- sample(presence, length(presence)/2)
absence <- which(wildfire.1_6$day6 == 0) %>% sample(length(presence)*0.7)
train <- wildfire.1_6[c(presence, absence),]

## lda
set.seed(233)
ctrl <- trainControl(method = "cv", number = 10)
lda.Fit <- train(as.factor(day6)~.,
                 data = train,
                 method = "lda",
                 trControl = ctrl)

# train MSE
lda.Fit$results$Accuracy

# train Confusion Matrix 
confusionMatrix(predict(lda.Fit, train[, -grep("day6", colnames(train))]), as.factor(train$day6))

# Confusion Matrix 1-6
confusionMatrix(predict(lda.Fit, wildfire.1_6[, -grep("day6", colnames(wildfire.1_6))]), as.factor(wildfire.1_6$day6))

# Confusion Matrix 2-7
confusionMatrix(predict(lda.Fit, wildfire.2_7[, -grep("day6", colnames(wildfire.2_7))]), as.factor(wildfire.2_7$day6))

# Plot 
predictions <- predict(lda.Fit, wildfire.2_7[, -grep("day6", colnames(wildfire.2_7))]) %>% 
  as.data.frame() %>% `colnames<-`("predictions") %>%
  bind_cols(wildfire[,grep("^LON.*|^LAT.*", colnames(wildfire))])

#reference <- wildfire.2_7 %>% dplyr::select(LATITUDE, LONGITUDE, day6) %>% filter(day6 == 1)
reference <- occurrence %>% filter(ACQ_DATE == dates[7])

# world <- map_data("world")

ggplot() +
  geom_tile(data = predictions, aes(x = LONGITUDE, y = LATITUDE, fill = predictions)) +
  geom_sf(data= st_transform(sa_shp, wgs48), fill = NA) + 
  scale_fill_brewer(palette = "Pastel2", direction = 1) +
 # geom_point(data = reference, aes(x = LONGITUDE, y = LATITUDE), color = "red", alpha = 0.5, size = 0.3) + 
  geom_sf(data = reference, size = 0.3, color = alpha("red", 0.5)) +
  

  theme_bw()

  
