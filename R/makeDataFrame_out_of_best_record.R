identifyBestRecord <- function(d){
  x <- ave(d$Produced, d$form, d$rowID, FUN = sum)
  return(x == max(x))
}
selectBestRecord <- function(d){
  return(d[identifyBestRecord(d),])
}

makeDataFrame_out_of_best_record <- function(CDI_combine, CDI_combine_subjectkey){
  x <- lapply(split(CDI_combine, CDI_combine$subjectkey), selectBestRecord)
  return(do.call(rbind, x))
}