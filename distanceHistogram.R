rm(list = ls())
library(doMC)
library(foreach)


# function for euclidean distance
euc.dist <- function(x1, x2){ 
  sqrt(sum((x1 - x2) ^ 2)) 
}

# get the finger print 
# defined using histograms of
# the distance between each sample
get.finger.print <- function(fin, id){
  
  
  # initialize dataframe to length(fin)
  n <- length(fin)
  d.stats.all <- data.frame(
    file.name     = character(n),
    hist1     <- numeric(n),
    hist2     <- numeric(n),
    hist3     <- numeric(n),
    hist4     <- numeric(n),
    hist5     <- numeric(n),
    hist6     <- numeric(n),
    hist7     <- numeric(n),
    hist8     <- numeric(n),
    hist9     <- numeric(n),
    hist10    <- numeric(n),
    hist11    <- numeric(n),
    hist12    <- numeric(n),
    id            = character(n),
    stringsAsFactors = F
  )
  
  # 12 bins range from zero to 48
  min.d <- 0
  max.d <- 48
  for(i in 1:n){
    
    temp <- read.csv(fin[i]) 
    # calculate the distance between samples
    d <- foreach(j = 2:(nrow(temp)), .combine = c ) %do% {euc.dist(temp[(j-1),],temp[j,])}
    
    d.stats.all$file.name[i]  <- fin[i]
    d.stats.all[i, 2:13]         <- hist(d[d <= max.d & d >= min.d], 
                                      plot = F, breaks= seq(min.d, max.d, 4) )$count
    d.stats.all$id            <- id
    
  }
  
  return(d.stats.all)
}



# initialize cores

registerDoMC(32)

dirs <- list.dirs("drivers/", full.names = F)[-1]

# get fingerprint for each file/driver combo
system.time(
  driver.trip.stats <- foreach(i=1:length(dirs), .combine = rbind) %dopar% {
    get.finger.print(list.files(paste0("drivers/",dirs[i]), full.names = T), id = dirs[i])
  }
)

# write results to file
write.csv(driver.trip.stats, "driverHistogramDistance20150219.csv")

