rm(list = ls())
library(dplyr)
library(stringr)

#clusters using kmeans
get.clusters <- function(x){
  
  set.seed(7)
  # initialize results vector
  if(is.null( nrow(x))){
    final.result <- rep(0, length(x))
  }else{
    final.result <- rep(0, nrow(x))
  }
  # perform kmeans clustering
  result <- kmeans(x, center = 2, nstart = 50)$cluster
  
  result.a <- sum(result == 1)
  result.b <- sum(result == 2)
  # assign 1 to larger group
  if(result.a > result.b){
    final.result[result == 1] <- 1   
    return(final.result)
  }else{
    final.result[result == 2] <- 1
    return(final.result)
  }
}

# cluster using hierarchal cluster
get.hier.clusters <- function(x){
  
  set.seed(7)
  # initialize results vector
  if(is.null( nrow(x))){
    final.result <- rep(0, length(x))
  }else{
    final.result <- rep(0, nrow(x))
  }
  
  hc.complete <- hclust(dist(x), method = "complete")
  result <- cutree(hc.complete, 2)
  result.a <- sum(result == 1)
  result.b <- sum(result == 2)
  # assign 1 to larger group
  if(result.a > result.b){
    final.result[result == 1] <- 1   
    return(final.result)
  }else{
    final.result[result == 2] <- 1
    return(final.result)
  }
  
}

# read in processed data for driver trips and stops
test.data<- read.csv("driverHistogramDistance20150219.csv")[-1]


#modify filename to show just the number of the file
file.names.sub.output <- str_extract(test.data$file.name, "/[0-9]{1,3}\\.")
file.names.sub.output <- sub("/", "", file.names.sub.output)
file.names.sub.output <- sub("\\.", "", file.names.sub.output)

# reorder array by filename
test.data$fout <- as.numeric(file.names.sub.output)
test.data <- test.data[order(test.data$id, test.data$fout),]
names(test.data)[2:13] <- c("hist1",
                            "hist2","hist3",
                            "hist4","hist5",
                            "hist6","hist7",
                            "hist8","hist9",
                            "hist10","hist11",
                            "hist12")

# apply Hierarchical clustering to: 
# stops, trip length, mean distance, median distance, 
# standard devation of distance, cumulative distance,
# and total distance
final.result <- test.data %>% group_by(id) %>%
  mutate(
    prediction = get.hier.clusters(cbind(hist1,
                                   hist2,hist3,
                                   hist4,hist5,
                                   hist6,hist7,
                                   hist8,hist9,
                                   hist10,hist11,
                                   hist12))
  )

summary(factor(final.result$prediction))


#write results to file 
driver_trip <- paste0(final.result$id, "_", final.result$fout)
write.csv(data.frame(driver_trip, prob = final.result$prediction),
          file = "submission_20150219_3_histogram_hier.csv",
          row.names = F)
