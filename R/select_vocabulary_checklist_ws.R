select_vocabulary_checklist_ws <- function(d){
  x <- names(d)
  z <- grepl("^mcs_vc[0-9]", x)
  return(d[,z])
}