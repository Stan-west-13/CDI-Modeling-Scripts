filter_missing_data_and_words_produced <- function(vocab_checklist, says_code, less_than, more_than, p_na){
  filter_A <- Proportion_missing_on_checklist(vocab_checklist) < p_na
  filter_B <- filter_by_n_produced(vocab_checklist, says_code,less_than, more_than)
  return(filter_A & filter_B)
}
