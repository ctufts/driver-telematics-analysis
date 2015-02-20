rm(list = ls())
library(dplyr)
library(stringr)

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
test.data.stops <- read.csv("driverStopStats_20150219.csv")
test.data.distance  <- read.csv("driverTripStats20150219.csv")
test.data <- merge(test.data.stops[,-1 ], test.data.distance[,-1], by = c("id", "file.name"))
rm(test.data.stops)
rm(test.data.distance)

#modify filename to show just the number of the file
file.names.sub.output <- str_extract(test.data$file.name, "/[0-9]{1,3}\\.")
file.names.sub.output <- sub("/", "", file.names.sub.output)
file.names.sub.output <- sub("\\.", "", file.names.sub.output)

# reorder array by filename
test.data$fout <- as.numeric(file.names.sub.output)
test.data <- test.data[order(test.data$id, test.data$fout),]


# apply Hierarchical clustering to: 
# stops, trip length, mean distance, median distance, 
# standard devation of distance, cumulative distance,
# and total distance
final.result <- test.data %>% group_by(id) %>%
  mutate(
    prediction = get.hier.clusters(scale
                              (cbind(stops,trip.length,
                                     mean.d, median.d, sd.d,
                                     start.end.d, total.d)))
    )

summary(factor(final.result$prediction))


#write results to file 
driver_trip <- paste0(final.result$id, "_", final.result$fout)
write.csv(data.frame(driver_trip, prob = final.result$prediction),
          file = "submission_20150219_2_hierCluster.csv",
          row.names = F)
