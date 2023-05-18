library(dplyr)
library(purrr)
library(furrr)
library(progressr)
library(readr)
library(tidyr)
library(ggplot2)
library(effects)
library(gridExtra)

source("R/decision_boundary.R")

load("data/cdi-metadata.Rdata")
load("data/ASD_NA_metadataNetstats_2023_RAN.Rdata")
participants <- ASD_NA_metadataNetstats_2023_RAN %>%
    select(group, subjectkey, vocab_size = nProduced, interview_age, sex) %>%
    distinct()

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

d <- ASD_NA_metadataNetstats_2023_RAN  %>%
    mutate(nproduced = nProduced) %>%
    ungroup()

g <- d %>%
    group_by(num_item_id) %>%
    group_keys()

future::plan("multisession")
with_progress({
    p <- progressor(steps = n_distinct(d$num_item_id))
    g$m <- d %>%
        mutate(group = factor(group, levels = c("NA", "ASD"))) %>%
        group_by(num_item_id) %>%
        mutate(
            nproducedc = nproduced - mean(nproduced)
        ) %>%
        group_split() %>%
        future_map(~{
            m <- glm(Produces ~ nproduced * group, data = ., family = "binomial")
            p()
            return(m)
        })
    p <- progressor(steps = n_distinct(d$num_item_id))
})

future::plan("multisession")
with_progress({
    p <- progressor(steps = nrow(g))
    g$m_logit <- future_map(g$m, function(m) {
            .data <- model.frame(m)
            .data$nproduced <- .data$nproduced * coef(m)["nproduced"]
            m <- glm(Produces ~ nproduced * group, data = .data, family = "binomial")
            p()
            return(m)
        })
})
g <- left_join(g, select(cdi_metadata, num_item_id, word))

save(g, file = "data/model_output_logit.Rdata")

.plots <- g %>%
    select(m = m_logit, word) %>%
    pmap(function(m, word) {
        p <- plot(Effect(c("nproduced", "group"), m), grid = FALSE, multiline = TRUE, ci.style = "band", main = word)
        return(p)
    })
ml <- marrangeGrob(.plots, nrow = 3, ncol = 2)
ggsave("all_plots_logit.pdf", ml, width = 8.5, height = 11, units = "in")


tmp <- d %>%
    filter(Produces) %>%
    group_by(num_item_id, group) %>%
    summarize(
        vocab_size_mean = mean(nproduced),
        vocab_size_sd = sd(nproduced),
        n_kids = n()
    ) %>%
    pivot_wider(
        id_cols = "num_item_id",
        names_from = "group",
        values_from = c(starts_with("vocab_"), "n_kids")
    )

q <- g %>%
    select(m, m_logit) %>%
    pmap_dfr(function(m, m_logit) {
    x <- decision_boundary(m)
    x_logit <- decision_boundary(m_logit)
    tbl <- tibble(
        scale = factor(1:2, levels = 1:2, labels = c("native", "logit")),
        non_asd = c(x["NA"], x_logit["NA"]),
        asd = c(x["ASD"], x_logit["ASD"])
    )
    tbl$diff <- tbl$asd - tbl$non_asd
    return(tbl)
}, .id = 'num_item_id') %>%
    mutate(num_item_id = as.numeric(num_item_id)) %>%
    pivot_wider(
        id_cols = "num_item_id",
        names_from = "scale",
        values_from = c("non_asd", "asd", "diff")
    )

save(q, file = "data/logit_diff_all.Rdata")


words_with_logit_diff <- q %>%
    left_join(select(cdi_metadata, num_item_id, word)) %>%
    filter(abs(diff_logit) > 1)

write_csv(words_with_logit_diff, file = "words_with_logit_diff.csv")


x <- expand_grid(group = factor(c("ASD", "NA")), nproduced = 20:600)

link <- map_dfr(g$m_logit, function(m, df) {
  df$p_logit <- predict(m, df, type = "link")
  df$p_probability <- predict(m, df, type = "response")

  avg_produced <- m %>%
    model.frame %>%
    pull(nproduced) %>%
    mean

  df <- df %>%
    mutate(
      nproduced_c = nproduced - avg_produced,
      nprod_logit = nproduced * coef(m)["nproduced"],
      nprod_logit_c = nproduced_c * coef(m)["nproduced"]
    )
  return(df)
}, df = select(x, c("group", "nproduced")), .id = "num_item_id") %>%
  mutate(
    num_item_id = as.numeric(num_item_id)
  ) %>%
  left_join(select(cdi_metadata, num_item_id, word)) %>%
  as_tibble()



ggplot(link[link$word == "boat",], aes(x = nprod_logit_c, y = p_probability, color = group))+
  geom_line(size = 2)+
  scale_color_manual(labels = c("Autistic", "Non-Autisitc"), values = c("black", "grey"))+
  geom_point(aes(x = 1.75, y = 0.5), shape = 17, color = "black", size = 5)+
  geom_point(aes(x = 3.5, y = 0.5), shape = 17, color = "black", size = 5)+
  theme_bw()+
  labs(x = "Logit Ability", y = "Probability of Production")+
  annotate("text",x = -1.5 , y = 0.95 , label = "Logit difference = 1.75", size = 5)+
  annotate("text",x = -0.75 , y = 0.85 , label = "The autistic group learned 'hold' later.", size = 5)
ggsave("Figures/LCC_hold_text.png")








