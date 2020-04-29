########################################################################

# Data Cleaning 1.3: Temp and Water and Wind Data
# Data Source: The NOAA Earth System Research Laboratory (ESRL)
# link: https://www.esrl.noaa.gov/psd/cgi-bin/db_search/DBSearch.pl?Dataset=NCEP+Reanalysis+Daily+Averages&Variable=U-wind&group=0&submit=Search

# This script 
# Convert temp and water (uwind typically) from netcdf form to csv file 


# Step 1: Load climate data (netcdf format) and county shapefiles
# Step 2: Extract climate data and geomotry data from netcdf format
# Step 3: Add Date information
# Step 4: Output in .csv 

########################################################################

library(tidyverse)
library(readr)
library(broom)
library(modelr)
library(patchwork)
library(readxl)
library(margins)
library(knitr)
library(RColorBrewer)
library(lubridate)
library(chron)
library(ncdf4)
library(lattice)


########################################################
## Data Cleaning for Wind Data 
########################################################
nc <- nc_open("./Data/wind/uwnd.10m.gauss.2019.nc")
wind <- ncvar_get(nc, "uwnd")

length(na.omit(as.vector(wind[,,1])))
dim(wind[,,3])  ## 192 rows, 94 col      
lon <- ncvar_get(nc, "lon")
nlon <- dim(lon)
lat <- ncvar_get(nc, "lat")
nlat <- dim(lat)

time <- ncvar_get(nc, "time")
time
tunits <- ncatt_get(nc,"time","units")
nt <- dim(time)
nt


# create dataframe -- reshape data
# matrix (nlon*nlat rows by 2 cols) of lons and lats
lonlat <- as.matrix(expand.grid(lon,lat))
dim(lonlat)
# reshape the array into vector
wind_vec <- as.vector(wind)
length(wind_vec)
# reshape the vector into a matrix
wind_mat <- matrix(wind_vec, nrow=nlon*nlat, ncol=nt)
dim(wind_mat)
head(na.omit(wind_mat))
# create a dataframe
lonlat <- as.matrix(expand.grid(lon,lat))
wind_df02 <- data.frame(cbind(lonlat,wind_mat))
startdate <- as.Date("2019-01-01") 
seq(startdate, by="1 day", length.out=365)
names(wind_df02) <- c("lon","lat", seq(startdate, by="1 day", length.out=365))
# names(wind_df02) <- c("lon","lat", 1:365)

# options(width=96)
head(na.omit(wind_df02, 20))
dim(na.omit(wind_df02))
# write.table(na.omit(wind_df02),"vwind.csv", row.names=FALSE, sep=",")


a <- wind_df02 %>%
  mutate(lon = ifelse(lon >180, lon-360, lon))%>%
  filter(lon > -81 & lon < -34 & lat > -54 & lat < 13) 
write.table(na.omit(a),"uwind.csv", row.names=FALSE, sep=",")


########################################################
## Data Cleaning for Temperature Data 
########################################################

nc <- nc_open("air.mon.mean.nc")
nc
temp <- ncvar_get(nc, "air")
temp

length(na.omit(as.vector(temp[,,1])))
dim(temp[,,3])  ## 720 rows, 360 col      
lon <- ncvar_get(nc, "lon")
nlon <- dim(lon)
lat <- ncvar_get(nc, "lat")
nlat <- dim(lat)

time <- ncvar_get(nc, "time")
time
tunits <- ncatt_get(nc,"time","units")
nt <- dim(time)
nt

# convert time -- split the time units string into fields
tustr <- strsplit(tunits$value, " ")
tdstr <- strsplit(unlist(tustr)[3], "-")
tmonth <- as.integer(unlist(tdstr)[2])
tday <- as.integer(unlist(tdstr)[3])
tyear <- as.integer(unlist(tdstr)[1])
chron(time,origin=c(tmonth, tday, tyear))

temp_slice <- temp[,,1]
library(RColorBrewer)


# create dataframe -- reshape data
# matrix (nlon*nlat rows by 2 cols) of lons and lats
lonlat <- as.matrix(expand.grid(lon,lat))
dim(lonlat)
# reshape the array into vector
temp_vec <- as.vector(temp)
length(temp_vec)
# reshape the vector into a matrix
temp_mat <- matrix(temp_vec, nrow=nlon*nlat, ncol=nt)

head(na.omit(temp_mat))
# create a dataframe
lonlat <- as.matrix(expand.grid(lon,lat))
temp_df02 <- data.frame(cbind(lonlat,temp_mat))
startdate <- as.Date("2019-01-01") 
seq(startdate, by="1 day", length.out=365)
names(temp_df02) <- c("lon","lat", seq(startdate, by="1 day", length.out=365))
# names(wind_df02) <- c("lon","lat", 1:365)

a <- temp_df02[1:367]

a<- a %>%
  mutate(lon = ifelse(lon >180, lon-360, lon))%>%
  filter(lon > -81 & lon < -34 & lat > -54 & lat < 13) 
write.table(na.omit(a),"temp.csv", row.names=FALSE, sep=",")


########################################################
## Data Cleaning for the precipitation data
########################################################

nc_1 <- nc_open("precip.mon.mean.nc")
nc_1
prep <- ncvar_get(nc_1, "precip")


length(na.omit(as.vector(prep[,,1])))
dim(temp[,,3])  ## 720 rows, 360 col      
lon <- ncvar_get(nc_1, "lon")
nlon <- dim(lon)
lat <- ncvar_get(nc_1, "lat")
nlat <- dim(lat)

time <- ncvar_get(nc_1, "time")
time
tunits <- ncatt_get(nc_1,"time","units")
nt <- dim(time)
nt

# convert time -- split the time units string into fields
tustr <- strsplit(tunits$value, " ")
tdstr <- strsplit(unlist(tustr)[3], "-")
tmonth <- as.integer(unlist(tdstr)[2])
tday <- as.integer(unlist(tdstr)[3])
tyear <- as.integer(unlist(tdstr)[1])
chron(time,origin=c(tmonth, tday, tyear))

prep_slice <- prep[,,1]
library(RColorBrewer)


# create dataframe -- reshape data
# matrix (nlon*nlat rows by 2 cols) of lons and lats
lonlat <- as.matrix(expand.grid(lon,lat))
dim(lonlat)
# reshape the array into vector
prep_vec <- as.vector(prep)
length(prep_vec)
# reshape the vector into a matrix
prep_mat <- matrix(prep_vec, nrow=nlon*nlat, ncol=nt)

head(na.omit(prep_mat))
# create a dataframe
lonlat <- as.matrix(expand.grid(lon,lat))
prep_df02 <- data.frame(cbind(lonlat,prep_mat))
startdate <- as.Date("2019-01-01") 
seq(startdate, by="1 day", length.out=365)
names(prep_df02) <- c("lon","lat", seq(startdate, by="1 day", length.out=365))


a <- prep_df02[1:367]

a<- a%>%
  mutate(lon = ifelse(lon >180, lon-360, lon))%>%
  filter(lon > -81 & lon < -34 & lat > -54 & lat < 13) 
write.table(na.omit(a),"precipitation.csv", row.names=FALSE, sep=",")