committee <- "Public Accounts Committee"
session <- "2017-19"
report_title <- "Thirty-Ninth Report"
inquiry <- "Defence Equipment Plan 2018–27"

inquiry_link <- "https://old.parliament.uk/business/committees/committees-a-z/commons-select/public-accounts-committee/inquiries/parliament-2017/defence-equipment-plan-2018-27-17-19/"
publication_link <- "http://data.parliament.uk/writtenevidence/committeeevidence.svc/evidencedocument/public-accounts-committee/defence-equipment-plan-201727/oral/80513.pdf"
publication_reference <- "HC 880"


# Extract PDF Text ----
pdf_text <- publication_link %>%
  tabulizer::extract_text() %>%
  stringr::str_split("\r\n") %>%
  unlist()

# Extract Meta
publication_title <- paste(pdf_text[2:3], collapse = "")
publication_date <- pdf_text[4]
members_present <- paste(pdf_text[7:11], collapse = " ")
witnesses_present <- paste(pdf_text[14:17], collapse = "")

meta <- dplyr::tibble(
  committee, inquiry, report_title, session, inquiry_link,
  publication_title, publication_date, 
  members_present, witnesses_present, publication_link,
  publication_reference
)

# Extract Transcript
first_row <- 26

text <- pdf_text  %>%
  dplyr::tibble() %>%
  dplyr::rename(text = .data$`.`) %>%
  dplyr::slice(first_row:length(pdf_text)) %>%
  dplyr::mutate(question = stringr::str_extract(text, "^Q[0-9]{3}|^Q[0-9]{2}|^Q[0-9]{1}")) %>%
  tidyr::fill(question, .direction = "down") %>%
  dplyr::mutate(loc = stringr::str_locate(text, ":")[,"start"]) %>%
  dplyr::mutate(valid = stringr::str_detect(text, "[A-Z][a-z]+:") & loc<45) %>%
  dplyr::mutate(person = ifelse(valid, stringr::str_sub(text, 1, loc-1), NA)) %>%
  dplyr::mutate(person = ifelse(!is.na(question), stringr::str_remove(person, question), person) %>%
                  stringr::str_trim()) %>%
  tidyr::fill(person, .direction = "down") %>%
  dplyr::mutate(seq = dplyr::lag(paste(question, person)) != paste(question, person)) %>%
  tidyr::replace_na(list(seq = TRUE)) %>%
  dplyr::mutate(paragraph_id = cumsum(seq)) %>%
  dplyr::group_by(paragraph_id, question, person) %>%
  dplyr::summarise(dialogue = paste(text, collapse = " "), .groups = "drop") 

# Save ---- 
pac_201719_hc880 <- list(meta = meta, transcript = text)
save(object = pac_201719_hc880, file = "data/pac_201719_hc880.rda")
