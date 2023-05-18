merge_split_records <- function(d, subject, age, verbose = FALSE) {
  # Some records are split across two rows in the data frame. This will merge the two by:
  # 1. Identifying the missing data in the first row.
  # 2. Filling it in with corresponding values from the second row.
  # 3. Dropping the second row.
  z <- (as.character(d$subjectkey) == subject) & (d$interview_age == age)
  row_inds <- which(z)
  D <- d[z,]
  if (nrow(D) > 2) {
    # This may be because data was entered multiple times (duplicate entries).  
    z <- duplicated(D)
    D <- D[!z, ]
    if (nrow(D) > 2) stop("More than two rows match selection criteria.")
  }
  
  if (nrow(D) == 1) {
    warning("Only one record matches selection criteria.")
    return(d)
  }
  if (verbose) print(D[,1:10])
  z <- as.vector(is.na(D[1,]))
  D[1,z] <- D[2,z]
  d[row_inds[1], ] <- D[1,]
  d <- d[-row_inds[length(row_inds)], ]
  return(d)
}
