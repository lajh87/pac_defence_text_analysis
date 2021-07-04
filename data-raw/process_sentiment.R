library(magrittr)

load("data/pac_equip_oral.rda")

pac_equip_sentiment <-purrr::map(pac_equip_oral, function(x){
  sent <- dplyr::tibble(publication_reference = x$meta$publication_reference,
                        x$transcript) %>%
    sentimentr::get_sentences() %>%
    sentimentr::sentiment_by(c("publication_reference","paragraph_id", 
                               "person"))
  attr(sent, "averaging.function") <- sentimentr::average_downweighted_zero
  sent
})

usethis::use_data(pac_equip_sentiment, overwrite = TRUE)
