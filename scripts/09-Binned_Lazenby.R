load("data/significant_binBinom_words.Rdata")
load("data/logit_diff_all.Rdata")
load("data/cdi-metadata.Rdata")
View(q)
library(dplyr)

logit_greaterOne <- q %>%
    filter(abs(diff_logit) > 1 &
    abs(non_asd_native) >= 20 &
    abs(non_asd_native) <= 600 &
    abs(asd_native) >= 20 &
    abs(asd_native) <= 600) 


sig_binBinom_words_log <- sig_binBinom_words %>%
    left_join(., logit_greaterOne) %>%
    arrange(desc(abs(diff_logit))) %>%
    left_join(.,select(cdi_metadata, num_item_id, word))

sig_binBinom_words_ASD_more <- sig_binBinom_words_log %>%
    filter(diff_logit > 0)

sig_binBinom_words_ASD_less <- sig_binBinom_words_log %>%
    filter(diff_logit < 0)
