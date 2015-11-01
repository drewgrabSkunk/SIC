yahoo.url.builder <- function(ticker) {
  URL.start <- "http://ichart.yahoo.com/table.csv?s="
  
  date.in.start <- "1950-01-01"
  date.split.start <- unlist(strsplit(date.in.start, "-"))
  month.in.start <- as.numeric(date.split.start[2]) - 1
  a <- paste("&a=", month.in.start, sep = "")
  b <- paste("&b=", as.numeric(date.split.start[3]), sep = "")
  c <- paste("&c=", date.split.start[1], sep = "")
  
  date.in.end <- as.character(Sys.Date())
  date.split.end <- unlist(strsplit(date.in.end, "-"))
  month.in.end <- as.numeric(date.split.end[2]) - 1
  d <- paste("&d=", month.in.end, sep = "")
  e <- paste("&e=", as.numeric(date.split.end[3]), sep = "")
  f <- paste("&f=", date.split.end[1], sep = "")
  
  #interval <- readline("What is the time interval: d, w, m ?     ")
  #g <- paste("&g=", interval, "&ignore=.csv", sep = "")
  g = "d"
  
  URL <- paste(URL.start, ticker, a, b, c, d, e, f, g, sep = "")
  
  return(URL)
  
}