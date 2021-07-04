library(publicaccountstm)
library(magrittr)

data("pac_equip_oral")

transcript <- purrr::map_df(pac_equip_oral, function(x){
  publication_reference <- x$meta$publication_reference
  dplyr::tibble(publication_reference, x$transcript)
})

df <- transcript %>% 
  dplyr::mutate(text = stringr::str_remove(dialogue, paste0(person, ":"))) %>% 
  dplyr::mutate(text = stringr::str_remove(text, paste0("^", question)))


dtm <- textmineR::CreateDtm(
  doc_vec = df$text,
  doc_names = paste(df$publication_reference, df$paragraph_id, sep = "_"),
  ngram_window = c(1,2),
  stopword_vec = stopwords::stopwords("en"),
  verbose = FALSE, remove_punctuation = TRUE, remove_numbers = TRUE
)

model <- textmineR::FitLdaModel(
  dtm = dtm,
  k = 10, # number of topic
  iterations = 500,
  burnin = 180,
  alpha = 0.1, beta = 0.05,
  optimize_alpha = TRUE,
  calc_likelihood = TRUE,
  calc_coherence = TRUE,
  calc_r2 = FALSE
)

model$top_terms <- textmineR::GetTopTerms(phi = model$phi, M = 5)
model$prevalence <- colSums(model$theta)/sum(model$theta)*100

summary <- data.frame(
  topic = rownames(model$phi),
  coherence = round(model$coherence,3),
  prevalence = round(model$prevalence,3),
  top_terms = apply(model$top_terms,2,function(x){paste(x,collapse = ", ")})
)

row.names(summary) <- 1:nrow(summary)
summary

summary %>% 
  tidyr::pivot_longer(cols = c(coherence,prevalence)) %>%
  ggplot2::ggplot(ggplot2::aes(x = factor(topic,levels = unique(topic)), y = value, group = 1)) +
  ggplot2::geom_point() + 
  ggplot2::geom_line() +
  ggplot2::facet_wrap(~name,scales = "free_y",nrow = 2) +
  ggplot2::theme_minimal() +
  ggplot2::labs(title = "Best topics by coherence and prevalence score",
                subtitle = "",
                x = "Topics", y = "Value")


tuned_mdl <- textmineR::FitLdaModel(
  dtm = dtm,
  k = 7, # number of topic
  iterations = 500,
  burnin = 180,
  alpha = 0.1, beta = 0.05,
  optimize_alpha = TRUE,
  calc_likelihood = TRUE,
  calc_coherence = TRUE,
  calc_r2 = FALSE
)

tuned_mdl$top_terms <- textmineR::GetTopTerms(phi = tuned_mdl$phi, M = 15)

topic <- tuned_mdl$theta %>%
  apply(1, function(x) {topic <- which(x==max(x)); 
  ifelse(length(topic)>1, NA, topic)})

dplyr::tibble(document = names(topic),
              topic = as.numeric(topic)) %>%
  tidyr::separate(document, c("publication_reference", "paragraph_id"), sep = "_") %>%
  dplyr::mutate(paragraph_id = as.numeric(paragraph_id)) %>%
  dplyr::left_join(df)  %>%
  dplyr::select(-text) %>%
  dplyr::filter(!is.na(person)) 
