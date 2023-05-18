keep_best_assessments <- function(d) {
  # Identify and retain the best assessment for each subjectkey in the database.
  # Other assessments are dropped.
  # The function assumes that there is a variable: nProduced in the dataframe.
  subjects <- unique(d$subjectkey)
  for (i in 1:length(subjects)) {
    s <- subjects[i]
    z <- d$subjectkey == s
    row_ind <- which(z)
    if (sum(z) > 1) {
      ix <- which.max(d$nProduced[z])
      d <- d[-row_ind[-ix], ]
    }
  }
  return(d)
}
