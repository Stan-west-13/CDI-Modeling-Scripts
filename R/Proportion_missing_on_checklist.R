Proportion_missing_on_checklist <-  function(vocab_checklist){
  return(rowMeans(is.na(vocab_checklist)))
}