library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(readr)
load("data/cdi-metadata.Rdata")
load("data/ASD_NA_metadataNetstats_2023_RAN.Rdata")
participants <- ASD_NA_metadataNetstats_2023_RAN %>%
    select(group, subjectkey, vocab_size = nProduced, interview_age, sex) %>%
    unique() %>%
    mutate(vocab_bin = cut(vocab_size, c(20, seq(50, 600, by = 50)), include.lowest = TRUE))

# Count participants and confirm that all subjectkeys are unique in this sample
participants %>%
    count(group)

participants %>%
    group_by(group) %>%
    summarize(n_distinct(subjectkey))

items <- cdi_metadata %>%
    select(num_item_id, category, lexical_class, word) %>%
    arrange(num_item_id) %>%
    distinct()

d <- ASD_NA_metadataNetstats_2023_RAN %>%
    filter(Produces) %>%
    drop_na() %>%
    select(group, subjectkey, interview_age, vocab_size = nProduced, num_item_id) %>%
    distinct() %>%
    mutate(vocab_bin = cut(vocab_size, c(20, seq(50, 600, by = 50)), include.lowest = TRUE))


bin_size <- participants %>%
    group_by(vocab_bin, group) %>%
    tally( name = "bin_size") %>%
    pivot_wider(
        id_cols = "vocab_bin",
        names_from = "group",
        names_prefix = "bin_size_",
        values_from = "bin_size"
    )



x <- d %>%
    group_by(vocab_bin, group, num_item_id) %>%
    tally() %>%
    pivot_wider(
        id_cols = c(vocab_bin, num_item_id),
        names_from = group,
        values_from = n,
        names_prefix = "n_",
        values_fill = 0
    ) %>%
    left_join(bin_size) %>%
    mutate(
        p_NA = n_NA / bin_size_NA,
        p_NA_adj = (n_NA+1) / (bin_size_NA +1),
        p_ASD = n_ASD / bin_size_ASD
    ) %>%
    ungroup()
summary(x)


x$binom_test <- pmap(x %>% select(x = n_ASD, n = bin_size_ASD, p = p_NA_adj), binom.test)
x$pval <- map_dbl(x$binom_test, `[[`, "p.value")
x$pval_fdr <- p.adjust(x$pval, method = "BH")

tmp <- x %>%
    filter(pval_fdr < .05) %>%
    mutate(ASD_more = p_ASD > p_NA_adj) %>%
    count(vocab_bin, ASD_more)

x %>%
    filter(pval_fdr < .05) %>%
    summarize(n_distinct(num_item_id))

x %>%
    filter(pval_fdr < .05) %>%
    write_csv(file = "data/vocab_bin_significant_differences.csv") 
    save(x, file = "data/vocab_bin_significant_differences.Rdata")



ggplot(tmp, aes(x = vocab_bin, y = n, fill = ASD_more)) +
    geom_bar(stat = "identity", position = position_stack()) +
    scale_x_discrete(labels = seq(50,600,by = 50))+
    theme_classic(base_size = 24)+
    labs(y = "number of words with significant difference",
         x = "vocabulary bin size (upper limit)")+
    scale_fill_grey(breaks = c("TRUE", "FALSE"),
                         labels = c("more likely w/ autism", "less likely w/ autism"))+
    theme(legend.title = element_blank(),
          legend.position = c(0.1,0.9),
          legend.justification = c(0.1,0.9),
          )+
    annotate("label", 
        x=10.5, 
        y = 150, 
        label = "Total number of \nsignificant words = 389",
        size = 7)
ggsave("Figures/word_knowledge_bar.pdf", height = 24, width = 30, unit = "cm")  


q <- x %>% filter(vocab_bin == "(300,350]")
ggplot(q, aes(x = n_NA, y = p_NA)) +
    geom_line(aes(y = p_NA), color = "red") +
    geom_line(aes(y = p_ASD), color = "blue")


x_breakdown <- x %>%
    filter(pval_fdr < 0.05) %>%
    mutate(ASD_more = p_ASD > p_NA) 



rep_x <- x_breakdown %>%
    group_by(num_item_id) %>%
    mutate(reversed = ifelse(n_distinct(ASD_more) > 1, TRUE, FALSE)) %>%
    filter(reversed == TRUE) %>%
    select(num_item_id) %>%
    unique()

x_breakdown <- x_breakdown %>%
    filter(!num_item_id %in% rep_x$num_item_id)

x_unique <- x_breakdown %>%
    select(num_item_id, ASD_more) %>%
    filter(!num_item_id %in% rep_x$num_item_id) %>%
    unique() 
sig_binBinom_words <- x_unique
save(sig_binBinom_words, file = 'data/significant_binBinom_words.Rdata')


x_tbl2 <- x_breakdown %>%
    filter(!num_item_id %in% rep_x$num_item_id) %>%
    group_by(num_item_id) %>%
    mutate(num_bins = length(vocab_bin)) %>%
    select(ASD_more, num_bins, num_item_id)
table(x_tbl2$num_bins, x_tbl2$ASD_more)



library(reshape2)
x_heat_ASD_more<- x_breakdown %>%
    select(vocab_bin, num_item_id,ASD_more) %>%
    filter(ASD_more == TRUE) %>%
    select(vocab_bin, num_item_id) %>%
    table() %>%
    t() %>%
    cor() %>%
    melt()

x_heat_ASD_less<- x_breakdown %>%
    select(vocab_bin, num_item_id,ASD_more) %>%
    filter(ASD_more == FALSE) %>%
    select(vocab_bin, num_item_id) %>%
    table() %>%
    t() %>%
    cor() %>%
    melt()

View(x_heat_ASD_less)
heatmap(x_heat_ASD_more, Rowv = NA, Colv = NA)
heatmap(x_heat_ASD_less)




ggplot(x_heat_ASD_less, aes(Var1,Var2, fill = value))+
    geom_tile()+
    ggtitle("Less likely in autism")

ggplot(x_heat_ASD_more, aes(Var1,Var2, fill = value))+
    geom_tile(color = "white")+
    ggtitle("More likely in autism")



############ SRCLD bins by number of significant bins

x_ASD_more<- x_breakdown %>%
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
                as.matrix()


x_ASD_less<- x_breakdown %>%
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
                as.matrix()
#######################################