Identify_empty_checklist <-  function(vocab_checklist, p_na){
  return(rowMeans(is.na(vocab_checklist)) >= p_na)
}