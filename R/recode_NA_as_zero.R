recode_NA_as_zero <- function(vocab_checklist){
  z <- (rowSums(vocab_checklist > 0 , na.rm = TRUE) > 0) & (rowSums(is.na(vocab_checklist), na.rm = TRUE) > 1) 
  x <- vocab_checklist[z, ]
  x[is.na(x)] <- 0
  vocab_checklist[z, ] <- x
  return(vocab_checklist)
}