


resource.pool <- 1000000
gain.rate <- 1.057

resource.fun <- function(pool.resource) {
  resources.needed <- rnorm(1, (pool.resource/250), (pool.resource/2400)/50)
  return(resources.needed)
  }

job.resource.frame <- data.frame(matrix(data=0,nrow=2600, ncol=3))
colnames(job.resource.frame) <- c("state.index", "resources.allocated", "latency")

jobs.online <- 0
jobs.resources <- 0
jobs.skipped <- 0
jobs.total <- 0
jobs.current <- 0
resources.used <- 0
resources.unused <- vector()

for (i in 1:365) {
  resources.gained <- 0
  jobs.new <- rpois(1, (2400/365))
  jobs.total <- jobs.total + jobs.new
  new.indices <- (1:jobs.new) + jobs.current

  
  for (j in new.indices[1]:(length(new.indices)+new.indices[1])) {
    job.resource.frame$state.index[j] <- i
    if(i==1) {
      job.resource.frame$resources.allocated[j] <- resource.fun(resource.pool)
      latency.rand <- runif(1)
      job.resource.frame$latency[j] <- latency.dist[which(abs(latency.dist$prob - latency.rand) == min(abs(latency.dist$prob - latency.rand))),1]
    }
    else if((i>1) & (resources.unused[i-1] > 0)){
      job.resource.frame$resources.allocated[j] <- resource.fun(resource.pool)
      latency.rand <- runif(1)
      job.resource.frame$latency[j] <- latency.dist[which(abs(latency.dist$prob - latency.rand) == min(abs(latency.dist$prob - latency.rand))),1]
    }
    else {
      job.resource.frame$resources.allocated[j] <- 0
      job.resource.frame$latency[j] <- 0
      }
  }
  
  
  for (k in 1:jobs.total) {
    if((job.resource.frame$resources.allocated[k] > 0) & ((job.resource.frame$latency[k] + job.resource.frame$state.index[k]) == i)) {
      resources.gained <- (job.resource.frame$resources.allocated[k] * gain.rate)
      job.resource.frame$resources.allocated[k] <- 0
    }
  }
  
  resources.gained <- resources.gained - sum(job.resource.frame$resources.allocated[new.indices])
  resources.used <- sum(job.resource.frame$resources.allocated)
  resource.pool <- resource.pool + resources.gained
  resources.unused[i] <- resource.pool
  if(resource.pool <= 0) {jobs.skipped <- jobs.skipped + jobs.new}
  jobs.current <- length(new.indices) + jobs.current
}

ALPHA.error <- jobs.skipped
BETA.error <- sum(resources.unused)/365
UNIFORMITY <- sd(job.resource.frame$resources.allocated[which(job.resource.frame$resources.allocated > 0)])

print(matrix(c("ALPHA.error", "BETA.error", "UNIFORMITY",ALPHA.error, BETA.error, UNIFORMITY),nrow=3))