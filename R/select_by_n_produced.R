select_by_n_produced <- function(d, says_code, less_than, more_than, show_hist = FALSE) {
  # First three columns are assumed to be "subjectkey", "interview_age", and
  # "gender", and all others correspond to words
  z <- filter_by_n_produced(d,says_code, less_than, more_than, show_hist = FALSE)
  return(d[z,])
}


