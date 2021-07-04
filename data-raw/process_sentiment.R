library(magrittr)
load("data/pac_defence_equip.rda")
meta <- pac_defence_equip$meta
text <- pac_defence_equip$transcript

sentiment <- text %>%
  dplyr::mutate(id = match(publication_reference, meta$publication_reference)) %>%
  dplyr::group_split(id) %>%
  purrr::map(function(x) {
    sent <- x %>% 
      sentimentr::get_sentences() %>%
      sentimentr::sentiment_by(
        ., 
        by = c("publication_reference","paragraph_id", 
               "person")) 
    attr(sent, "averaging.function") <- sentimentr::average_downweighted_zero
    sent
  })
 

save(object = sentiment, file = "inst/pac_sentiment_defence_equip/pac_defence_equip_sentiment.rda")
save(object = meta, file = "inst/pac_sentiment_defence_equip/pac_defence_equip_meta.rda")

