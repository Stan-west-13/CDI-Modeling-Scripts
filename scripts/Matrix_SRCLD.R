load("data/itemID_by_vocabBin.Rdata")
library(dplyr)
library(ggplot2)
library(tidyr)



sum_bins_mat_ASD_more <- x_breakdown %>%
    select(vocab_bin, num_item_id,ASD_more) %>%
    filter(ASD_more == TRUE) %>%
    select(vocab_bin, num_item_id) %>%
    group_by(num_item_id) %>% 
    mutate(num_bins = length(num_item_id)) %>%
    ungroup() %>%
    group_by(vocab_bin, num_bins) %>%
    mutate(sum_bins = length(num_item_id)) %>%
    select(-num_item_id) %>%
    unique() %>%
    arrange(num_bins, vocab_bin) %>%
    mutate(num_bins = as.factor(num_bins)) %>%
        pivot_wider(.,
                names_from = "num_bins",
                values_from = "sum_bins",
                values_fill = 0) %>%
        pivot_longer(.,
                    cols = 2:ncol(.),
                    names_to = "num_bins",
                    values_to = "sum_bins") %>%
        mutate(vocab_bin = factor(vocab_bin)) %>%
    ggplot(.,aes(vocab_bin, num_bins))+
        geom_tile(aes(fill = sum_bins))+
        geom_text(aes(label = sum_bins))+
        scale_fill_gradient(low = "#FDD023", high = '#461D7C' )+
        scale_x_discrete(labels = seq(50,600,50))+
        labs(x = "Vocabulary bin (upper limit)", y = "Number of significant bins")+
        labs(fill = "Sum of words/bin")+
        ggtitle("Number of Significant Bins per Bin: ASD More Likely")+
        theme(text=element_text(size = 24))



sum_bins_mat_ASD_less <- x_breakdown %>%
    select(vocab_bin, num_item_id,ASD_more) %>%
    filter(ASD_more == FALSE) %>%
    select(vocab_bin, num_item_id) %>%
    group_by(num_item_id) %>% 
    mutate(num_bins = length(num_item_id)) %>%
    ungroup() %>%
    group_by(vocab_bin, num_bins) %>%
    mutate(sum_bins = length(num_item_id)) %>%
    select(-num_item_id) %>%
    unique() %>%
    arrange(num_bins, vocab_bin) %>%
    mutate(num_bins = as.factor(num_bins)) %>%
        pivot_wider(.,
                names_from = "num_bins",
                values_from = "sum_bins",
                values_fill = 0) %>%
        pivot_longer(.,
                    cols = 2:ncol(.),
                    names_to = "num_bins",
                    values_to = "sum_bins") %>%
        mutate(vocab_bin = factor(vocab_bin)) %>%
    ggplot(.,aes(vocab_bin, num_bins))+
        geom_tile(aes(fill = sum_bins))+
        geom_text(aes(label = sum_bins))+
        scale_fill_gradient(low = "#FDD023", high = '#461D7C' )+
        scale_x_discrete(labels = seq(50,600,50))+
        labs(x = "Vocabulary bin (upper limit)", y = "Number of significant bins")+
        labs(fill = "Sum of words/bin")+
        ggtitle("Number of Significant Bins per Bin: ASD Less Likely ")+
        theme(text=element_text(size = 24))
