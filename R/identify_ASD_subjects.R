identify_ASD_subjects <- function(d){
  z <- d$phenotype[, "ASD"]
  return(d[z])
}