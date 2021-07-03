committee <- "Public Accounts Committee"
session <- "2019-21"
report_title <- "Fiftieth Report"
inquiry <- "Defence Equipment Plan 2020-2030"

inquiry_link <-  "https://committees.parliament.uk/work/920/defence-equipment-plan-20202030/"
publication_link <- "https://committees.parliament.uk/oralevidence/1642/default/"
publication_reference <- "HC 693"

# Extract Text from PDF ----
pdf_text <- publication_link %>%
  tabulizer::extract_text() %>%
  stringr::str_split("\r\n") %>%
  unlist()

# Extract Meta
publication_title <- paste(pdf_text[4:6], collapse = "")
publication_date <- pdf_text[6]
members_present <- paste(pdf_text[9:13], collapse = " ")
witnesses_present <- paste(pdf_text[16:20], collapse = "")

meta <- dplyr::tibble(
  committee, inquiry, report_title, session, inquiry_link,
  publication_title, publication_date, 
  members_present, witnesses_present, publication_link,
  publication_reference
)

# Extract Transcript
first_row <- 28

text <- pdf_text  %>%
  dplyr::tibble() %>%
  dplyr::rename(text = .data$`.`) %>%
  dplyr::slice(first_row:length(pdf_text)) %>%
  dplyr::mutate(question = stringr::str_extract(text, "^Q[0-9]{3}|^Q[0-9]{2}|^Q[0-9]{1}")) %>%
  tidyr::fill(question, .direction = "down") %>%
  dplyr::mutate(loc = stringr::str_locate(text, ":")[,"start"]) %>%
  dplyr::mutate(colon = stringr::str_detect(text, "[A-Z][a-z]+:")) %>%
  dplyr::mutate(person = ifelse(colon, stringr::str_sub(text, 1, loc-1), NA)) %>%
  dplyr::mutate(person = ifelse(!is.na(question), stringr::str_remove(person, question), person) %>%
                  stringr::str_trim()) %>%
  tidyr::fill(person, .direction = "down") %>%
  dplyr::mutate(seq = dplyr::lag(paste(question, person)) != paste(question, person)) %>%
  tidyr::replace_na(list(seq = TRUE)) %>%
  dplyr::mutate(paragraph_id = cumsum(seq)) %>%
  dplyr::group_by(paragraph_id, question, person) %>%
  dplyr::summarise(dialogue = paste(text, collapse = " "), .groups = "drop") 


pac_201921_hc693 <- list(meta = meta, transcript = text)
save(object = pac_201921_hc693, file = "data/pac_201921_hc693.rda")
