# Daily Sampling from Time series @ a random time

getDailySample <- function(Prices) {
  res <- list()
  indx <- index(Prices)
  dt <- unique(as.Date(indx))
  t <- unique(format(indx, format= "%H:%M:%S"))
  rnd.sample <- sample(1:length(t), 1)
  indx2 <- paste(dt, t[rnd.sample], "EDT")
  res$time <- t[rnd.sample]
  res$prices <- Prices[indx2]
  return(res)
}

