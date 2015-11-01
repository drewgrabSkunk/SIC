yahoo.data.pull <- function(ticker.vector) {
  for (i in 1:length(ticker.vector)) {
    URL <- yahoo.url.builder(ticker.vector[i])
    
    try(tech.transform.fun(URL, ticker.vector[i]))
    
    print(c(i, length(ticker.vector),ticker.vector[i]))
  }
}
 