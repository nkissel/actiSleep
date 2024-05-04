to_dec <- function(x, add = T) {
  w <- which(!is.na(x))
  h <- m <- rep(-1, length(x))
  h[w] <- substr(x[w], 12, 13)
  m[w] <- substr(x[w], 15, 16)
  h <- as.numeric(h); m <- as.numeric(m)
  if(add) {
    h[w] <- ifelse(h[w] < 12, h[w] + 24, h[w])
  }
  rt <- h + m/60
  ifelse(nchar(as.character(x)) == 10, 24, rt)

}


to_time <- function(x) {
  # x = c(22.52, 11.11)

  h <- floor(x)
  minsec <- (x - h)*60
  min <- round(minsec)
  h <- ifelse(h == 24, 0, h)
  as.POSIXct(paste0(h, ':', min, ':00'), format = '%H:%M:%S', tz='UTC')

}
