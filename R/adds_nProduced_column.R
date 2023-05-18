adds_nProduced_column <- function(d, says_code) {
  # First three columns are assumed to be "subjectkey", "interview_age", and
  # "gender", and all others correspond to words
  x <- rowSums(d[,4:ncol(d)] == says_code, na.rm = TRUE)
  d$nProduced <- x
  return(d)
}
