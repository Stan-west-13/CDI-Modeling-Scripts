load("data/itemID_by_vocabBin.Rdata")
library(dplyr)
library(ggplot2)
library(tidyr)



x_breakdown %>%
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
        scale_x_discrete(labels = seq(50,600,50), expand = c(0,0))+
        scale_y_discrete(breaks = seq(1,9,1), expand = c(0,0))+
        labs(x = "vocabulary bin (upper limit)", y = "number of significant bins")+
        labs(fill = "words/bin")+
        theme(text=element_text(size = 24), legend.position = "none")
ggsave("Figures/sig_words_mat_ASD_more.pdf", width =24,height = 12.5, units = "cm" )

x_add <- data.frame(vocab_bin = unique(x_breakdown$vocab_bin), 
    num_bins = rep(10,12), 
    sum_bins = rep(0,12))
 
 x_breakdown %>%
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
    arrange(vocab_bin) %>%
    arrange(num_bins) %>%
        pivot_wider(.,
                names_from = "num_bins",
                values_from = "sum_bins",
                values_fill = 0) %>%
        pivot_longer(.,
                    cols = 2:ncol(.),
                    names_to = "num_bins",
                    values_to = "sum_bins") %>%
        mutate(vocab_bin = factor(vocab_bin)) %>%
        mutate(num_bins = as.numeric(num_bins)) %>%
        rbind(.,x_add) %>%        
    ggplot(.,aes(vocab_bin, num_bins))+
        geom_tile(aes(fill = sum_bins))+
        geom_text(aes(label = sum_bins))+
        scale_fill_gradient(low = "#FDD023", high = '#461D7C' )+
        scale_x_discrete(labels = seq(50,600,50), expand = c(0,0))+
        scale_y_continuous(breaks = seq(1,12,1), expand = c(0,0))+
        labs(x = "vocabulary bin (upper limit)", y = "number of significant bins")+
        labs(fill = "words/bin")+
        theme(text=element_text(size = 24))
ggsave("Figures/sig_words_mat_ASD_less.pdf", width =24,height = 12.5, units = "cm" )
