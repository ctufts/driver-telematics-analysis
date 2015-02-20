rm(list = ls())
library(doMC)
library(foreach

# function determines if a stop occurs
get.stops <- function(x1, x2){ 
    return(x1 == x2)
}

# function determines unique stops (non-consecutive)
get.unique.stops <- function(stops){ 
  if(any(stops)){
    return(sum(abs(diff(which(stops)))  > 1 ))
  }else{
    return(0)
  }
}

# create fingerprint based on 
# stopping data and trip length
get.finger.print <- function(fin, id){
  # initialize dataframe to length(fin)
  n <- length(fin)
  d.stats.all <- data.frame(
    file.name     = character(n),
    stops         = numeric(n),
    unique.stops  = numeric(n),
    trip.length   = numeric(n), 
    id            = character(n),
    stringsAsFactors = F
  )
  
  
  for(i in 1:n){
    
    temp <- read.csv(fin[i]) 
    n.rows <- nrow(temp)
    # calculate the mean distance between samples
    stops <- foreach(j = 2:n.rows, .combine = c ) %do% {get.stops(temp[(j-1),],temp[j,])}
    
    d.stats.all$file.name[i]      <- fin[i]
    d.stats.all$stops[i]          <- sum(stops)
    d.stats.all$unique.stops[i]   <- get.unique.stops(stops)
    d.stats.all$trip.length[i]    <- n.rows
    d.stats.all$id[i]             <- id
    
  }
  
  return(d.stats.all)
}


# initialize cores

registerDoMC(32)

dirs <- list.dirs("drivers/", full.names = F)[-1]

# get fingerprint for each driver/file 
system.time(
  driver.trip.stats <- foreach(i=length(dirs), .combine = rbind) %dopar% {
    get.finger.print(list.files(paste0("drivers/",dirs[i]), full.names = T), id = dirs[i])
  }
)

write.csv(driver.trip.stats, "driverStopStats.csv")




