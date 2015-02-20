rm(list = ls())
library(dplyr)
library(stringr)

# function clusters using kmeans
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

sample.sub <- read.csv("sampleSubmission.csv")
test.data.stops <- read.csv("driverStopStats_copy_20150219.csv")
test.data.distance  <- read.csv("driverTripStats20150219.csv")

test.data <- merge(test.data.stops[,-1 ], test.data.distance[,-1], by = c("id", "file.name"))
rm(test.data.stops)
rm(test.data.distance)

file.names.sub.output <- str_extract(test.data$file.name, "/[0-9]{1,3}\\.")
file.names.sub.output <- sub("/", "", file.names.sub.output)
file.names.sub.output <- sub("\\.", "", file.names.sub.output)

test.data$fout <- as.numeric(file.names.sub.output)
test.data <- test.data[order(test.data$id, test.data$fout),]



# determine principle components
pr.out <- prcomp(test.data[, 3:12 ], scale = TRUE)
biplot(pr.out, scale = 0, xlabs = rep("", nrow(test.data)))
pr.var <- pr.out$sdev^2
pve <- pr.var/sum(pr.var)

plot(pve, xlab = "PC", ylab = "Proportion of Variance Explained",
     ylim = c(0,1), type = 'b')
plot(cumsum (pve ), xlab=" Principal Component ", 
     ylab ="Cumulative Proportion of Variance Explained ", ylim=c(0,1) ,
     type ="b")

# use kmeans cluster to determine class
# attributes used are: unique stops, start.end.d (total distance), and trip length
final.result <- test.data %>% group_by(id) %>%
  mutate(
    prediction = get.clusters(scale
                              (cbind(unique.stops,start.end.d, trip.length)))
    )

# veiw results
summary(factor(final.result$prediction))

# append driver/file name as per Kaggle rules
driver_trip <- paste0(final.result$id, "_", final.result$fout)
#write results to file
write.csv(data.frame(driver_trip, prob = final.result$prediction),
          file = "DriverTelemetrics/submission_20150219_1.csv",
          row.names = F)
