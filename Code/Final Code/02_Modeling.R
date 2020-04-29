########################################################################

# Modeling 2.0
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


theme_set(
  theme_bw()+
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.subtitle = element_text(hjust = 0.5)) +
    theme(plot.tag.position = c(0.8, 0)) +
    theme(plot.tag = element_text(size=8)) +
    theme(strip.text.y = element_blank())
)

########################################################
# Open Cleaned Precedents Data and Cluster Groups
########################################################

precedents <- map(list.files("./data/Wildfire Precedents", 
                             pattern = "month.csv$", full.names = TRUE), read.csv) %>% bind_cols() 
clusterset <- read.csv("./Output/clustering.csv") %>% select(-X)

date.range <- colnames(precedents) %>% {gsub("^fire", "", .)} %>% as.Date(format = "%Y.%m.%d")
precedents <- precedents[, order(date.range)] # order cols by date

########################################################
# Stratified Sampling and Subset Training Data
########################################################

date.range %>% {gsub("-..$", "", .)} %>% duplicated() %>% {grep("FALSE", .)} %>% print()

set.seed(5904)

spring <- c(244:335) %>% sample(10) # randomly sample 1 day per season
summer <- c(335:365, 22:32) %>% sample(10) # the first 22 days do not have precedents so are removed
autumn <- c(60:152) %>% sample(10)
winter <- c(152:244) %>% sample(10)

date.sample <- c(spring, summer, autumn, winter); print(date.sample)

wildfire.full <- data.frame(matrix(nrow = 0, ncol = 0))

n <- 250
absence.rate <- 1 # What percentage of non-occurence (as a percentage of occurence)
# do you want to include in the training data?


for (i in date.sample){
  
  cols.range <- c((i-2):(i-21), i) 
  single <- precedents[, cols.range] 
  print(colnames(single))
  colnames(single) <- (c(paste0(1:20, "pre"), "target"))
  
  single <- bind_cols(single, clusterset) %>% mutate(date = i)
  
  set.seed(5904)
  presence <- which(single$target == 1)
  presence <- sample(presence, n) # you can also change this to a specific number
  absence <- which(single$target == 0) %>% sample(n*absence.rate)
  
  single <- single[c(presence, absence),]
  
  wildfire.full <- bind_rows(wildfire.full, single)
  
  cat("-------- Done with sample ", i, Sys.time(), "-------- ","\n")
  
}

colnames(wildfire.full) <- colnames(wildfire.full) %>% {gsub("^fire", "", .)}

# Choose a test day: Which day do you want to predict?
set.seed(678)
test.day <- sample(22:365, 1); print(test.day)

cols.range <- c((test.day-2):(test.day-21), test.day)
test <- precedents[, cols.range] %>% `colnames<-`(c(paste0(1:20, "pre"), "target")) %>%
  bind_cols(clusterset) 
colnames(test) <- colnames(test) %>% {gsub("^fire", "", .)}; colnames(test)

train <- wildfire.full
train$date <- NULL

########################################################
# Fit a Model 
########################################################


clustercols <- paste0("cluster", 2:8)
precols <- paste0(1:20, "pre")
set.seed(5904)
ctrl <- trainControl(method = "cv", number = 10)

# LDA 

lda.results <- data.frame(matrix(nrow = 0, ncol = 0))

for (i in seq_along(clustercols)){
  clusters <- clustercols[i]
  train.cluster <- train %>% select(-starts_with("cluster"))
  train.cluster$cluster <- train[, clusters]
  
  test.cv <- test %>% select(-starts_with("cluster"))
  test.cv$cluster <- test[, clusters]
  
  for (k in 7:20){
    train.cluster <- train.cluster %>% select(-ends_with("pre"))
    pres.train <- train %>% select(precols[-c(k:20)])
    train.cluster <- bind_cols(train.cluster, pres.train)

    test.cv <- test.cv %>% select(-ends_with("pre"))
    pres.test <- test %>% select(precols[-c(k:20)])
    test.cv <- bind_cols(test.cv, pres.test)
  
  Fit <- train(as.factor(target)~as.factor(cluster)+.,
                   data = train.cluster,
                   method = "lda",
                   trControl = ctrl)
  
  result <- tibble(cluster = clusters, nfeatures = k,
                   train_mse = mean(predict(Fit, train.cluster) != train.cluster$target),
                   test_mse = mean(predict(Fit, test.cv) != test.cv$target))
  
  lda.results <- lda.results %>% bind_rows(result)
  print(paste(result$train_mse, result$test_mse))
  
  cat("-------- Done with cluster ", i ,k,Sys.time(), "-------- ","\n")
  
  }
};  write_csv(lda.results, paste0("./Output/Regression Results/lda.csv"))

# lda without cluster

lda.results <- data.frame(matrix(nrow = 0, ncol = 0))

for (k in 7:20){
  train.cluster <- train %>% select(-starts_with("cluster"))
  train.cluster <- train.cluster %>% select(-ends_with("pre"))
  pres.train <- train %>% select(precols[-c(k:20)])
  train.cluster <- bind_cols(train.cluster, pres.train)
  
  test.cv <- test %>% select(-starts_with("cluster"))
  test.cv <- test.cv %>% select(-ends_with("pre"))
  pres.test <- test %>% select(precols[-c(k:20)])
  test.cv <- bind_cols(test.cv, pres.test)
  
  Fit <- train(as.factor(target)~.,
               data = train.cluster,
               method = "lda",
               trControl = ctrl)
  result <- tibble(cluster = clusters, nfeatures = k,
                   train_mse = mean(predict(Fit, train.cluster) != train.cluster$target),
                   test_mse = mean(predict(Fit, test.cv) != test.cv$target))
  
  lda.results <- lda.results %>% bind_rows(result)
  print(paste(result$train_mse, result$test_mse))
  cat("-------- Done with nfreatures ",k,Sys.time(), "-------- ","\n")
  
};  write_csv(lda.results, paste0("./Output/Regression Results/ldawithout.csv"))
# Logistic 

logistic.results <- data.frame(matrix(nrow = 0, ncol = 0))

for (i in seq_along(clustercols)){
  clusters <- clustercols[i]
  train.cluster <- train %>% select(-starts_with("cluster"))
  train.cluster$cluster <- train[, clusters]
  
  test.cv <- test %>% select(-starts_with("cluster"))
  test.cv$cluster <- test[, clusters]
  
  for (k in 7:20){
    train.cluster <- train.cluster %>% select(-ends_with("pre"))
    pres.train <- train %>% select(precols[-c(k:20)])
    train.cluster <- bind_cols(train.cluster, pres.train)
    
    test.cv <- test.cv %>% select(-ends_with("pre"))
    pres.test <- test %>% select(precols[-c(k:20)])
    test.cv <- bind_cols(test.cv, pres.test)
    
    Fit <- train(as.factor(target)~as.factor(cluster)+.,
                     data = train.cluster,
                     method = "glm",
                     trControl = ctrl)
    result <- tibble(cluster = clusters, nfeatures = k,
                     train_mse = mean(predict(Fit, train.cluster) != train.cluster$target),
                     test_mse = mean(predict(Fit, test.cv) != test.cv$target))
    
    logistic.results <- logistic.results %>% bind_rows(result)
    print(paste(result$train_mse, result$test_mse))
    cat("-------- Done with cluster ", i ,k,Sys.time(), "-------- ","\n")
    
  }
};  write_csv(logistic.results, paste0("./Output/Regression Results/logistic.csv"))

# Without cluster

logistic.results <- data.frame(matrix(nrow = 0, ncol = 0))

for (k in 7:20){
    train.cluster <- train %>% select(-starts_with("cluster"))
    train.cluster <- train.cluster %>% select(-ends_with("pre"))
    pres.train <- train %>% select(precols[-c(k:20)])
    train.cluster <- bind_cols(train.cluster, pres.train)
    
    test.cv <- test %>% select(-starts_with("cluster"))
    test.cv <- test.cv %>% select(-ends_with("pre"))
    pres.test <- test %>% select(precols[-c(k:20)])
    test.cv <- bind_cols(test.cv, pres.test)
    
    Fit <- train(as.factor(target)~.,
                 data = train.cluster,
                 method = "glm",
                 trControl = ctrl)
    result <- tibble(cluster = clusters, nfeatures = k,
                     train_mse = mean(predict(Fit, train.cluster) != train.cluster$target),
                     test_mse = mean(predict(Fit, test.cv) != test.cv$target))
    
    logistic.results <- logistic.results %>% bind_rows(result)
    print(paste(result$train_mse, result$test_mse))
    cat("-------- Done with nfreatures ",k,Sys.time(), "-------- ","\n")
    
};  write_csv(logistic.results, paste0("./Output/Regression Results/logisticwithout.csv"))

# NBayes 

nb.results <- data.frame(matrix(nrow = 0, ncol = 0))

for (i in seq_along(clustercols)){
  clusters <- clustercols[i]
  train.cluster <- train %>% select(-starts_with("Cluster"))
  train.cluster$cluster <- train[, clusters]
  
  test.cv <- test %>% select(-starts_with("Cluster"))
  test.cv$cluster <- test[, clusters]
  
  for (k in 7:20){
    train.cluster <- train.cluster %>% select(-ends_with("pre"))
    pres.train <- train %>% select(precols[-c(k:20)])
    train.cluster <- bind_cols(train.cluster, pres.train)
    
    test.cv <- test.cv %>% select(-ends_with("pre"))
    pres.test <- test %>% select(precols[-c(k:20)])
    test.cv <- bind_cols(test.cv, pres.test)
    
    Fit <- train(as.factor(target)~as.factor(cluster)+.,
                 data = train.cluster,
                 method = "nb",
                 trControl = ctrl)
    result <- tibble(cluster = clusters, nfeatures = k,
                     train_mse = mean(predict(Fit, train.cluster) != train.cluster$target),
                     test_mse = mean(predict(Fit, test.cv) != test.cv$target))
    
    nb.results <- nb.results %>% bind_rows(result)
    print(paste(result$train_mse, result$test_mse))
    cat("-------- Done with cluster ", i ,k,Sys.time(), "-------- ","\n")
    
  }
};  write_csv(nb.results, paste0("./Output/Regression Results/nb.csv"))


# without
nb.results <- data.frame(matrix(nrow = 0, ncol = 0))

for (k in 7:20){
  train.cluster <- train %>% select(-starts_with("cluster"))
  train.cluster <- train.cluster %>% select(-ends_with("pre"))
  pres.train <- train %>% select(precols[-c(k:20)])
  train.cluster <- bind_cols(train.cluster, pres.train)
  
  test.cv <- test %>% select(-starts_with("cluster"))
  test.cv <- test.cv %>% select(-ends_with("pre"))
  pres.test <- test %>% select(precols[-c(k:20)])
  test.cv <- bind_cols(test.cv, pres.test)
  
  Fit <- train(as.factor(target)~.,
               data = train.cluster,
               method = "nb",
               trControl = ctrl)
  result <- tibble(cluster = clusters, nfeatures = k,
                   train_mse = mean(predict(Fit, train.cluster) != train.cluster$target),
                   test_mse = mean(predict(Fit, test.cv) != test.cv$target))
  
  nb.results <- nb.results %>% bind_rows(result)
  print(paste(result$train_mse, result$test_mse))
  cat("-------- Done with nfreatures ",k,Sys.time(), "-------- ","\n")
  
};  write_csv(nb.results, paste0("./Output/Regression Results/nbwithout.csv"))


# visualize the results
# best model
Fit <- train(as.factor(target)~.,
             data = train[, c(1:7, 21:23)],
             method = "nb",
             trControl = ctrl)
# data prep 
predictions <- predict(Fit, test[, c(1:7, 21:23)]) %>% 
  as.data.frame() %>% `colnames<-`("predictions") %>%
  bind_cols(test[,grep("^LON.*|^LAT.*", colnames(test))])

reference <- test %>% select(target,LONGITUDE, LATITUDE) %>% filter(target == 1)

# world <- map_data("world")

ggplot() +
  geom_tile(data = predictions, aes(x = LONGITUDE, y = LATITUDE, fill = predictions)) +
  geom_sf(data= st_transform(sa_shp, wgs48), fill = NA) + 
  scale_fill_brewer(palette = "Set3", direction = 1) +
  geom_point(data = reference, aes(x = LONGITUDE, y = LATITUDE), color = "orangered1", alpha = 0.5, size = 0.3) 

