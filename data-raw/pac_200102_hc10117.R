library(magrittr)

# Meta ----
committee <- "Public Accounts Committee"
session <- "2001-02"
report_title <- "Fifth Report"
inquiry <- "MINISTRY OF DEFENCE: MAJOR PROJECTS REPORT 2000"
publication_reference <- "HC 10117"

inquiry_link <- "https://publications.parliament.uk/pa/cm200102/cmselect/cmpubacc/368/36802.htm"
publication_link <- "https://publications.parliament.uk/pa/cm200001/cmselect/cmpubacc/136/1011701.htm"

members_present <- 
  paste("Mr David Davis, in the Chair",
        "Mr Simon Burns",	"Mr Gerry Steinberg",
        "Mr Alan Campbell",	"Mr Alan Williams",
        "Mr Ian Davidson", sep = "; ")	

witnesses_present <- 
  paste("SIR ROBERT WALMSLEY, KCB, FREng, Chief of Defence Procurement, Defence Procurement Agency",
        "VICE ADMIRAL SIR JEREMY BLACKHAM, KCB, BA, Deputy Chief of Defence Staff (Equipment Capability), Ministry of Defence (MOD)", 
        sep = "; ")

publication_title <- "MINUTES OF EVIDENCE TAKEN BEFORE THE SELECT COMMITTEE OF PUBLIC ACCOUNTS"
publication_date <- "WEDNESDAY 17 JANUARY 2001"

meta <- dplyr::tibble(
  committee, inquiry, report_title, session, inquiry_link,
  publication_title, publication_date, 
  members_present, witnesses_present, publication_link,
  publication_reference
)


# Text ----
pages <- 2:8

src <- purrr::map(pages, function(p){
  p <- ifelse(nchar(p)==1, paste0(0,p), p)
  path <- "https://publications.parliament.uk/pa/cm200001/cmselect/cmpubacc/136/10117{p}.htm"
  url <- stringr::str_glue(path)  
  root <- xml2::read_html(url)
})

text <- src %>%
  purrr::map_df(function(x){
    nodes <- x %>%
      xml2::xml_find_all("//table") %>%
      magrittr::extract(3) %>%
      xml2::xml_children() %>%
      xml2::xml_children() %>%
      xml2::xml_children()
    
    tbl <- nodes %>%
      purrr::map(function(x){
        member <- xml2::xml_text(x)[(xml2::xml_name(x)=="center")]
        member <- ifelse(is.null(member), NA, member)
        content <- xml2::xml_contents(x)[xml2::xml_name(x)=="p"] 
        
        text <- content %>% xml2::xml_text() %>% paste(collapse = "") %>%
          stringr::str_replace_all("\r\n", " ") %>%
          stringr::str_trim()
        
        witness <- xml2::xml_text(content)[xml2::xml_name(content)=="i"]
        witness <- ifelse(is.null(witness), NA, witness)
        witness <- ifelse(stringr::str_detect(witness, "^[A-Z][a-z]+"), witness, NA)
        
        if(!is.na(witness))
          loc <- stringr::str_locate(text, witness)
        text <- ifelse(is.null(text), NA, text)
        
        if(!is.na(text) && !is.na(witness)){
          text <- c(stringr::str_sub(text, 1, loc[,"start"]-2),
                    stringr::str_sub(text, loc[,"start"]-1, nchar(text)))
        }
        
        dplyr::tibble(member, text, witness)
        
      }) %>%
      purrr::imap(~dplyr::tibble(.x, node_id = .y)) %>%
      purrr::map_df(function(x) x) %>%
      dplyr::mutate(question = stringr::str_extract(text, "^[0-9]{3}|^[0-9]{2}|^[0-9]{1}"))
    
    first_row <- c(which(!is.na(tbl$member)), which(!is.na(tbl$question))) %>% min()
    tbl %>%
      dplyr::slice(first_row:nrow(tbl))
    
  }) %>%
  tidyr::fill(c(member, question), .direction = "down") %>%
  dplyr::filter(text != "") %>%
  dplyr::mutate(witness_speak = stringr::str_detect(text, witness)) %>%
  tidyr::replace_na(list(witness_speak = FALSE)) %>%
  dplyr::mutate(person = ifelse(witness_speak, witness, member)) %>%
  dplyr::select(question, person, dialogue = text)  %>% 
  dplyr::mutate(paragraph_id = cumsum(paste(question, person) != paste(dplyr::lag(question), dplyr::lag(person)))) %>%
  dplyr::group_by(paragraph_id, question, person) %>%
  dplyr::summarise(dialogue = paste(dialogue, collapse = " "), .groups = "drop")

# Save ----
pac_200102_hc10117 <- list(meta = meta, transcript = text)
save(object = pac_200102_hc10117 , file = "data/pac_200102_hc10117.rda")
