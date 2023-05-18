select_vocabulary_checklist_wg <-function(d){
  x <- names(d)
  z <- grepl("^mcg_vc[0-9]", x)
  return(d[,z])
}