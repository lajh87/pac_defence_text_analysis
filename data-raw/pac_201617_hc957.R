committee <- "Public Accounts Committee"
session <- "2016-17"
report_title <- "Fifty-Sixth Report"
inquiry <- "The Defence Equipment Plan"

inquiry_link <- "https://old.parliament.uk/business/committees/committees-a-z/commons-select/public-accounts-committee/inquiries/parliament-2015/defence-equipment-plan-16-17/"
publication_link <- "http://data.parliament.uk/writtenevidence/committeeevidence.svc/evidencedocument/public-accounts-committee/defence-equipment-plan/oral/48389.html"
publication_reference <- "HC 957"


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
      paste(collapse = "") # to catch two or more span tags in person field
    
    bold_text_with_colon <- bold_text %>%
      stringr::str_detect("\\:")
    
    person <- bold_text[bold_text_with_colon]
    
    text_indent <- xml2::xml_attr(x, "style") %>%
      stringr::str_detect("text-indent:-39.7pt")
    
    question <- xml2::xml_text(x)[text_indent] %>%
      stringr::str_extract("Q[0-9]{3}|Q[0-9]{2}|Q[0-9]{1}")
    
    question <- ifelse(is.null(question), NA, question)
    person <- ifelse(is.null(person), NA, person)
    dialogue <- ifelse(is.null(dialogue), NA, dialogue)
    
    dplyr::tibble(question, person, dialogue)
    
    })

# Extract Meta
publication_title <- tbl$dialogue[3]
publication_date <- tbl$dialogue[4]
members_present <- tbl$dialogue[7]
witnesses_present <- tbl$dialogue[9]

meta <- dplyr::tibble(
     committee, inquiry, report_title, session, inquiry_link,
     publication_title, publication_date, 
     members_present, witnesses_present, publication_link,
     publication_reference
   )

first_row <- 13
last_row <- 439

text <- tbl %>%
  dplyr::slice(first_row:last_row) %>%
  tidyr::fill(c(question, person), .direction = "down") %>%
  dplyr::mutate(person = stringr::str_remove(person, "\\:")) %>%
  dplyr::mutate(paragraph_id = cumsum(paste(question, person) != paste(dplyr::lag(question), dplyr::lag(person)))) %>%
  dplyr::group_by(paragraph_id, question, person) %>%
  dplyr::summarise(dialogue = paste(dialogue, collapse = " "), .groups = "drop")

pac_201617_hc957 <- list(meta = meta, transcript = text)

