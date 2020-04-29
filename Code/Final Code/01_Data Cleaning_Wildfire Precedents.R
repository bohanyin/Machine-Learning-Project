########################################################################

# Data Cleaning 1.1: Wildfire Precedents
# Data Source: NASA's Fire Information for Resource Management System
# link: https://firms.modaps.eosdis.nasa.gov/download/ (FIRMS)

# Additional Data: SA shapefile
# https://www.arcgis.com/home/item.html?id=d3d2bae5413845b193d038e4912d3da9 

# This script creates a 0.1 x 0.1 degree raster layer over the SA
# and calculate a wildfire occurence dummy for each grid cell.
# The time range covers the entire year of 2019 and Jan-March in 2020.

# Step 1: Load NASA FIRMS data and SA shapefiles
# Step 2: Create a 0.1 x 0.1 degree raster layer 
# Step 3: Map NASA FRIMS data into the raster layer
# Step 4: Output in .csv 

########################################################################


library(sf)
library(sp)
library(rgeos)
library(raster)
library(tidyverse)
library(dplyr)
library(caret)
library(rsample)
library(scales)
library(glmnet)

########################################################
# Open FIRMS 2019-2010 Wildfire Data 
########################################################

# Set CRS
wgs48 <- crs("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
# equalarea <- crs("+proj=eqdc +lat_0=0 +lon_0=0 +lat_1=-5 +lat_2=-42 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs") 

# Load South America Shapefile
sa_shp <- st_read("./Data/South America Shp/SouthAmerica.shp")

# Load FIRMS Data
firms_path <-"./Data/FIRMS/2019-2020 FIRMS" %>%
  list.files(full.names = TRUE) %>% 
  map(~list.files(., pattern = ".shp$", full.names = TRUE)) %>% 
  unlist()

vars <- c("LATITUDE", "LONGITUDE", "ACQ_DATE")
nrt_M6 <- firms_path[grepl("nrt_M6", firms_path)] %>% st_read(stringsAsFactors = FALSE) %>% 
  filter(CONFIDENCE > 40) %>% st_transform(wgs48) %>% dplyr::select(vars)
archive_M6 <- firms_path[grepl("archive_M6", firms_path)] %>% st_read(stringsAsFactors = FALSE) %>%
  st_transform(wgs48) %>% dplyr::select(vars)
nrt_V1 <- firms_path[grepl("nrt_V1", firms_path)] %>% st_read(stringsAsFactors = FALSE) %>%
  st_transform(wgs48) %>% dplyr::select(vars)
archive_V1 <- firms_path[grepl("archive_V1", firms_path)] %>% st_read(stringsAsFactors = FALSE) %>%
  st_transform(wgs48) %>% dplyr::select(vars)

occurrence <- nrt_M6 %>% rbind(archive_M6) %>% rbind(nrt_V1) %>% rbind(archive_V1) %>%
  mutate(dummy = 1) # if binary output

# Create SA raster layer and map wildfire data into it
base.unit <- 0.1 # unit: degree # the greater the base unit, the larger the disparencies of areas across gridcells
raster.base <- raster(ncol = 360/base.unit, 
                      nrow= 180/base.unit) %>% 
  crop(extent(st_transform(sa_shp, crs(raster()))))


wildfire <- data.frame(matrix(nrow = length(raster.base), ncol = 0))

dates <- c(occurrence$ACQ_DATE) %>% unique(); print(dates)
dates <- dates[order(dates, decreasing = TRUE)]

for (i in seq_along(dates)){
  date <- dates[i]
  
  byday <- occurrence %>% filter(ACQ_DATE == date) %>%
    {rasterize(., raster.base, .$dummy, fun = mean)}%>% 
    raster::getValues() %>% {.>0} %>% as.data.frame() %>% 
    `colnames<-`(date); wildfire <- bind_cols(wildfire, byday)
  print(date)
}


wildfire1 <- wildfire %>% mutate_all(~ replace_na(.,0))

locs <- raster.base %>% 
  rasterToPoints() %>% 
  as.data.frame() %>% 
  `colnames<-`(c("LONGITUDE", "LATITUDE")) %>% 
  mutate_all(~round(.,2))

wildfire1 <- bind_cols(wildfire1, locs)
colnames(wildfire1) <- paste("fire", colnames(wildfire1), sep = "")

# Remove oceans
# Note: we use the worldclimate data (which only covers lands) to remove grid cells in the sea
# there definitely is a smarter way to do this, but we accidently run into this method and gonna keep it
wc <- raster::getData('worldclim', res=10, var='bio')
wc.extract <- raster::extract(wc, wildfire1[,427:428]) ## lon lat col
wildfire2 <- wildfire1 %>% cbind(wc.extract) %>% drop_na()
wildfire2 <- wildfire2 %>% select(-starts_with("bio"))

# Save output as csv 
# Note: because the data is too large, it is splited into 12 separate files by month
for (i in c(1:12)) {
  if (i <10){i<-paste0(0,i)}
  pattern <- paste0(".*-", i,"-.*")
  x <- wildfire2 %>% colnames() %>% {grep(pattern, .)}
  month <- wildfire2 %>% select(x)
  write_csv(month, paste0("./Data/Wildfire Precedents/", i, "month.csv"))
}

# reunite the saved file to check 
# reunite <- map(list.files("./Data/Wildfire Precedents", pattern = "month.csv$", full.names = TRUE), read.csv) 
#           %>% bind_cols()


########################################################
# Unsupervised Clustering
########################################################

cluster <- wildfire2 %>% mutate(id = 1:367) %>% column_to_rownames(var = "id")

wildfire3 <- wildfire2[366:367] 
for (i in 2:8){
  cluster.kmeans <- kmeans(wildfire2, center = i) %>% {.$cluster} %>% as.data.frame() %>% `colnames<-`(i) #cluster 2,3 
  # wildfire2$Cluster <-NULL
  wildfire3 <- wildfire2 %>% bind_cols(cluster.kmeans)
}
cluster_result <- wildfire3 %>%
  rename(cluster2 = `2`,
         `cluster3` = `3`,
         `cluster4` = `4`,
         `cluster5` = `5`,
         `cluster6` = `6`,
         `cluster7` = `7`,
         `cluster8` = `8`,)

########################################################
# Wind data combination
########################################################

uwind <- read_csv("uwind.csv")
wind_base <- uwind[, c(366:367)] %>% SpatialPoints() %>% extent() %>%
  raster(nrows = 25, ncols = 25) 

startdate <- as.Date("2019-01-01") 
a <- uwind %>% 
  select(-c("lon", "lat")) 
colnames(a) <- seq(startdate, by="1 day", length.out=365)
colnames(a) <- paste("wind", colnames(a), sep = "")
uwind <- cbind(a, uwind[c("lon", "lat")])


cord <- uwind[c("lon", "lat")]
# b <- rasterize(cord, r, uwind$`wind2019-12-31`, fun = mean)
# b.extract <- raster::extract(b, wildfire1[,c("LONGITUDE", "LATITUDE")])


dates_wind <- colnames(a) %>% unique(); print(dates_wind)
dates_wind <- dates_wind[order(dates_wind, decreasing = TRUE)]
wind <- data.frame(matrix(nrow = length(raster.base), ncol = 0))


for (i in seq_along(dates_wind)){
  date <- dates_wind[i]
  b <- rasterize(cord, wind_base, uwind[,i], fun = mean)
  b.extract <- raster::extract(b, wildfire1[,c("fireLONGITUDE", "fireLATITUDE")])%>% as.data.frame() %>% 
    `colnames<-`(date); wind <- bind_cols(wind, b.extract)
  print(date)
}

wind <- bind_cols(wind, locs) %>%
  mutate_all(~ replace_na(.,0))


fire_wind <- cbind(wildfire1[-428:-427], wind1)


########################################################
# Climate Data
########################################################

## Precipitation
uprep <- read_csv("./Climate/precip.csv")

prep_base <- uprep[c("lon", "lat")] %>% SpatialPoints() %>% extent() %>%
  raster(nrows = 25, ncols = 25) 

startdate <- as.Date("2019-01-01") 
a <- uprep %>% 
  select(-c("lon", "lat")) 
colnames(a) <- seq(startdate, by="1 day", length.out=365)
colnames(a) <- paste("prep", colnames(a), sep = "")
uprep <- cbind(a, uprep[c("lon", "lat")])


cord <- uprep[c("lon", "lat")]
# b <- rasterize(cord, r, uwind$`wind2019-12-31`, fun = mean)
# b.extract <- raster::extract(b, wildfire1[,c("LONGITUDE", "LATITUDE")])


dates_prep <- colnames(a) %>% unique()
dates_prep <- dates_prep[order(dates_prep, decreasing = TRUE)]
prep <- data.frame(matrix(nrow = length(raster.base), ncol = 0))


for (i in seq_along(dates_prep)){
  date <- dates_prep[i]
  b <- rasterize(cord, prep_base, uprep[,i], fun = mean)
  b.extract <- raster::extract(b, locs[,c("LONGITUDE", "LATITUDE")])%>% as.data.frame() %>% 
    `colnames<-`(date); prep <- bind_cols(prep, b.extract)
  print(date)
}

prep <- bind_cols(prep, locs) %>%
  mutate_all(~ replace_na(.,0))

# prep %>% ggplot(aes(x= LONGITUDE, y=LATITUDE, fill = `prep2019-12-29`) ) + geom_tile()



## Temperature

utemp <- read_csv("temp.csv")
temp_base <- utemp[c("lon", "lat")] %>% SpatialPoints() %>% extent() %>%
  raster(nrows = 25, ncols = 25) 

startdate <- as.Date("2019-01-01") 
a <- utemp %>% 
  select(-c("lon", "lat")) 
colnames(a) <- seq(startdate, by="1 day", length.out=365)
colnames(a) <- paste("temp", colnames(a), sep = "")
utemp <- cbind(a, utemp[c("lon", "lat")])


cord <- utemp[c("lon", "lat")]
# b <- rasterize(cord, r, uwind$`wind2019-12-31`, fun = mean)
# b.extract <- raster::extract(b, wildfire1[,c("LONGITUDE", "LATITUDE")])


dates_temp <- colnames(a) %>% unique()
dates_temp <- dates_temp[order(dates_temp, decreasing = TRUE)]
temp <- data.frame(matrix(nrow = length(raster.base), ncol = 0))


for (i in seq_along(dates_temp)){
  date <- dates_temp[i]
  b <- rasterize(cord, temp_base, utemp[,i], fun = mean)
  b.extract <- raster::extract(b, locs[,c("LONGITUDE", "LATITUDE")])%>% as.data.frame() %>% 
    `colnames<-`(date); temp <- bind_cols(temp, b.extract)
  print(date)
}

temp <- bind_cols(temp, locs) %>%
  mutate_all(~ replace_na(.,0))
# temp %>% ggplot(aes(x= LONGITUDE, y=LATITUDE, fill = `temp2019-12-29`) ) + geom_tile()


## Combine with fire data
fire_climate <- cbind(wildfire1[-366:-367], prep[-366:-367]) %>% cbind(temp[-366:-367] )%>% cbind(wind[-1:-2])


# Remove oceans
# Note: we use the worldclimate data (which only covers lands) to remove grid cells in the sea
# there definitely is a smarter way to do this, but we accidently run into this method and gonna keep it
# World Climate, note: format is RasterStack
wc <- raster::getData('worldclim', res=10, var='bio')
# wc.extract <- raster::extract(wc, wildfire1[,427:428]) ## lon lat col
wc.extract <- raster::extract(wc, fire_climate[,1461:1462])
fire_climate1 <- fire_climate %>% cbind(wc.extract) %>% drop_na()
# fire_climate1 <- fire_climate1 %>% select(-starts_with("bio"))

fire_worldClimate <- cbind(wildfire2[1:22], cluster_result)

wildfire2$x <- wildfire2 %>% raster.base() %>% area()


########################################################
# LASSO feature selection 
########################################################

timecols <- grep("^time", colnames(fire_wind2))
# colnames(fire_wind2)<- c(colnames(fire_wind2)[timecols], paste0("wind", colnames(fire_wind2)[-timecols]))

fw <- wildfire2[-366:-367] ## remove longitude and latitude


colnames(fw) <- gsub("\\-", "", colnames(fw)) 


set.seed(123)

X <- sparse.model.matrix(`fire20191231` ~ ., data= fw)[,-1] # create sparse matrix
y <- fw$`fire20191231`
n <- length(y)

cv_output <- cv.glmnet(X, y, 
                       alpha = 1, nfolds = 10)

# identifying best lamda
best_lam <- cv_output$lambda.1se

lasso_best <- glmnet(X, y, alpha = 1, lambda = best_lam)
glmnet(X, y, alpha = 1, lambda = best_lam)
plot(cv_output)
plot(cv_output$glmnet.fit, xvar = "lambda")


# store the selected coefficients into var
var <- coef(lasso_best)
var1 <- data.frame(summary(var))
var1 <- var1 %>% select(-j) %>%
  rename(`Days_before_20191231` = i,
         `Coef` = x) 
write.csv(var1, "lasso_firedate.csv")

