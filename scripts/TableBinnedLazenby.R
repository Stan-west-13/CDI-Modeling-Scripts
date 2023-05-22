library(tidyverse)


load("data/vocab_bin_significant_differences.Rdata")
load("data/logit_diff_all.Rdata")
load("data/cdi-metadata.Rdata")

df_binnedStats_LogitStats <- x %>%
    left_join(., q) %>%
    left_join(., select(cdi_metadata, num_item_id, word)) %>%
    arrange(desc(abs(diff_logit)))


write.csv(df_binnedStats_LogitStats %>% select(-binom_test), "data/BinnedLazenbytbl.csv")
save(df_binnedStats_LogitStats, file = "data/BinnedLazenbytbl.Rdata")
