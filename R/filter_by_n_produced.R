filter_by_n_produced <- function(d, says_code, less_than, more_than, show_hist = FALSE){
  x <- rowSums(d[,4:ncol(d)] == says_code, na.rm = TRUE)
  d$nProduced <- x
  if (show_hist) hist(x)
  z <- (x > more_than) & (x < less_than)
  return(z)
}

