inquiry_link <- "https://old.parliament.uk/business/committees/committees-a-z/commons-select/public-accounts-committee/inquiries/parliament-2010/mod-equip-plan-2013-23-major-proj-rep-2013/"
publication_link <- "http://data.parliament.uk/writtenevidence/committeeevidence.svc/evidencedocument/public-accounts-committee/ministry-of-defence-equipment-plan-and-major-projects-report-2013/oral/6661.html"

library(magrittr)

committee <- "Public Accounts Committee"
session <- "2013-14"
report_title <- "Fifty-Seventh Report"
inquiry <- "The Ministry of Defence Equipment Plan 2013-23 and Major Projects Report 2013"
publication_reference <- "HC 1060"

# Extract HTML ----
root <- publication_link %>%
  xml2::read_html()

nodes <- root %>%
  xml2::xml_contents() %>%
  magrittr::extract(2) %>% # body
  xml2::xml_children() %>%
  xml2::xml_children()

tbl <- nodes %>%
  purrr::map_df(function(x){
    dialogue <- xml2::xml_text(x)
    
    bold <- xml2::xml_children(x) %>%
      xml2::xml_attr("style") %>%
      stringr::str_detect("font-weight:bold")
    
    bold_text <- (xml2::xml_children(x) %>%
                    xml2::xml_text())[bold] %>%
      paste(collapse = " ") # to catch two or more span tags in person field
    
    bold_text <- ifelse(is.null(bold_text), NA, bold_text)
    dialogue <- ifelse(is.null(dialogue), NA, dialogue)
    
    dplyr::tibble(bold_text, dialogue)
    
  })

# Extract Meta ----
publication_title <- "Oral evidence: Ministry of Defence Equipment Plan and Major Projects Report 2013, HC 1060"
publication_date <- "Wednesday 12 February 2014"
members_present <- paste(tbl$dialogue[6], tbl$dialogue[7])
witnesses_present <-  tbl$dialogue[9]

meta <- dplyr::tibble(
  committee, inquiry, report_title, session, inquiry_link,
  publication_title, publication_date, 
  members_present, witnesses_present, publication_link,
  publication_reference
)

# Extract Text ----
first_row <- 13
last_row <- 698

text <- tbl %>%
  dplyr::slice(first_row:last_row) %>%
  dplyr::mutate(question = stringr::str_extract(bold_text, "Q[0-9]{3}|Q[0-9]{2}|Q[0-9]{1}")) %>%
  dplyr::mutate(dialogue = stringr::str_trim(dialogue)) %>%
  dplyr::rename(person = bold_text) %>%
  dplyr::mutate(person = stringr::str_remove(person, "Q[0-9]{3}|Q[0-9]{2}|Q[0-9]{1}")) %>%
  dplyr::mutate(person = stringr::str_remove(person, "\\:")) %>%
  dplyr::mutate(person = stringr::str_trim(person)) %>%
  dplyr::mutate(person = ifelse(person == "", NA, person)) %>%
  tidyr::fill(question, person, .direction = "down") %>%
  dplyr::filter(dialogue != "") %>%
  dplyr::mutate(paragraph_id = cumsum(paste(question, person) != paste(dplyr::lag(question), dplyr::lag(person)))) %>%
  dplyr::group_by(paragraph_id, question, person) %>%
  dplyr::summarise(dialogue = paste(dialogue, collapse = " "), .groups = "drop")

# Save ----
pac_201314_hc1060 <- list(meta = meta, transcript = text)
save(object = pac_201314_hc1060, file = "data/pac_201314_hc1060.rda")
