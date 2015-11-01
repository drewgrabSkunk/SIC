yahoo.data.pull.lag <- function(ticker.vector, adxlag) {
  for (i in 1:length(ticker.vector)) {
    
    URL <- yahoo.url.builder(ticker.vector[i])
    
    try(tech.transform.lag.fun(URL, ticker.vector[i], adxlag))
    
    print(c(i, length(ticker.vector), ticker.vector[i]))
    
  }
}