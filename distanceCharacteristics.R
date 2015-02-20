rm(list = ls())
library(doMC)
library(foreach)


# function for euclidean distance between two points
euc.dist <- function(x1, x2){ 
  sqrt(sum((x1 - x2) ^ 2)) 
}

# create fingerprint based on 
# the distance between each points
get.finger.print <- function(fin, id){
  # initialize dataframe to length(fin)
  n <- length(fin)
  d.stats.all <- data.frame(
    file.name     = character(n),
    mean.d        = numeric(n),
    median.d      = numeric(n),
    sd.d          = numeric(n),
    max.d         = numeric(n),
    min.d         = numeric(n),
    start.end.d   = numeric(n),
    total.d       = numeric(n),
    id            = character(n),
    stringsAsFactors = F
  )
  
  
  for(i in 1:n){
    
    temp <- read.csv(fin[i]) 
    # calculate the mean distance between samples
    d <- foreach(j = 2:(nrow(temp)), .combine = c ) %do% {euc.dist(temp[(j-1),],temp[j,])}
    
    d.stats.all$file.name[i]  <- fin[i]
    d.stats.all$mean.d[i]     <- mean(d)
    d.stats.all$median.d[i]   <- median(d)
    d.stats.all$sd.d[i]       <- sd(d)
    d.stats.all$max.d[i]      <- max(d)
    d.stats.all$min.d[i]      <- min(d)
    d.stats.all$total.d[i]    <- sum(d)
    d.stats.all$start.end.d[i]<- euc.dist(temp[1,], temp[nrow(temp),])
    d.stats.all$id            <- id
    
  }
  
  return(d.stats.all)
}


# initialize cores

registerDoMC(32)

dirs <- list.dirs("drivers/", full.names = F)[-1]

# get fingerprint for each file/driver
system.time(
  driver.trip.stats <- foreach(i=1:length(dirs), .combine = rbind) %dopar% {
    get.finger.print(list.files(paste0("drivers/",dirs[i]), full.names = T), id = dirs[i])
  }
)

write.csv(driver.trip.stats, "driverTripStats20150219.csv")

