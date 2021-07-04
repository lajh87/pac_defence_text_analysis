library(magrittr)

committee <- "Public Accounts Committee"
session <- "2014-15"
report_title <- "Forty-Seventh Report"
inquiry <- "Equipment Plan and Major Projects Report 2014, reforming defence acquisition"
publication_reference <- "HC 1045"


publication_link <- "http://data.parliament.uk/writtenevidence/committeeevidence.svc/evidencedocument/public-accounts-committee/the-equipment-plan-and-major-projects-report-2014-and-reforming-defence-acquisition/oral/18418.html"
inquiry_link <- "https://old.parliament.uk/business/committees/committees-a-z/commons-select/public-accounts-committee/inquiries/parliament-2010/equipment-plan-and-major-projects-report-2014-and-reforming-defence-acquisition/"

# Extract HTML Text ----
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
publication_title <- tbl$dialogue[3]
publication_date <- tbl$dialogue[4]
members_present <- paste(tbl$dialogue[7], tbl$dialogue[9])
witnesses_present <-  tbl$dialogue[11]

meta <- dplyr::tibble(
  committee, inquiry, report_title, session, inquiry_link,
  publication_title, publication_date, 
  members_present, witnesses_present, publication_link,
  publication_reference
)

# Extract Text ----

first_row <- 14
last_row <- 626

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

pac_201415_hc1045 <- list(meta = meta, transcript = text)
