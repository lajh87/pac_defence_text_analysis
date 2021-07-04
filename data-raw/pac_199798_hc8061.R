library(magrittr)

# Meta ----
committee <- "Public Accounts Committee"
session <- "1997-98"
report_title <- "First Report"
inquiry <- "MINISTRY OF DEFENCE: MAJOR PROJECTS REPORT 1997"
publication_reference <- "HC 8061"

inquiry_link <- "https://publications.parliament.uk/pa/cm199899/cmselect/cmpubacc/101/10102.htm"
publication_link <- "https://publications.parliament.uk/pa/cm199798/cmselect/cmpubacc/795/8061001.htm"


members_present <- 
  paste("Mr David Davis, in the Chair",
  "Mr Alan Campbell",	"Mr Christopher Leslie",
  "Mr Geoffrey Clifton-Brown",	"Mr Andrew Love",
  "Mr Geraint Davies",	"Mr Alan Williams",
  "Ms Maria Eagle", sep = "; ")	

witnesses_present <- 
  paste("SIR ROBERT WALMSLEY, KCB, MA, FIEE, Chief of Defence Procurement, Ministry of Defence",
  "MR FRANK MARTIN, Second Treasury Officer of Accounts", sep = "; ")

publication_title <- "MINUTES OF EVIDENCE TAKEN BEFORE THE SELECT COMMITTEE OF PUBLIC ACCOUNTS"
publication_date <- "WEDNESDAY 10 JUNE 1998"

meta <- dplyr::tibble(
  committee, inquiry, report_title, session, inquiry_link,
  publication_title, publication_date, 
  members_present, witnesses_present, publication_link,
  publication_reference
)

# Text ----
pages <- 3:7

src <- purrr::map(pages, function(p){
  p <- ifelse(nchar(p)==1, paste0(0,p), p)
  path <- stringr::str_sub(publication_link, 1, nchar(publication_link)-6) %>%
    paste0("{p}.htm")
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
    
    tbl <- purrr::map_df(nodes, function(x){
      member <- xml2::xml_text(x)[(xml2::xml_name(x) == "center")]
      
      text <- xml2::xml_text(x)
      contents <- xml2::xml_contents(x)
      text_i <- xml2::xml_text(contents)[xml2::xml_name(contents)=="i"]
      
      witness <- text_i[stringr::str_detect(text_i, "\\)")]
      witness <- stringr::str_remove(witness, "\\)") %>% stringr::str_trim()

      member <- ifelse(is.null(member), NA, member)
      text <- ifelse(is.null(text), NA, text)
      witness <- ifelse(is.null(witness), NA, witness)
      
      dplyr::tibble(text = text, member = member, witness)  %>% 
        dplyr::mutate(text= stringr::str_replace_all(text, "\r\n", " ")) %>%
        dplyr::mutate(text = stringr::str_trim(text)) %>%
        dplyr::mutate(question = stringr::str_extract(text, "^[0-9]{3}\\.|^[0-9]{2}\\.|^[0-9]{1}\\.")) %>%
        dplyr::mutate(question = stringr::str_sub(question, 1, nchar(question)-1)) 
      
    }) %>%
     tidyr::fill(member, .direction = "down") %>%
      dplyr::filter(text != "")
    
    first_row <- (which(!is.na(tbl$question)) %>% min())
    
    tbl %>%
      dplyr::slice(first_row:nrow(tbl)) %>%
      dplyr::mutate(text = stringr::str_split(text, paste0("\\(",witness,"\\)"))) %>%
      tidyr::unnest(text) %>%
      dplyr::mutate(text = stringr::str_trim(text)) %>%
      dplyr::mutate(q_lgl = stringr::str_detect(text, paste0("^",question))) %>%
      dplyr::mutate(person = ifelse(q_lgl, member, witness)) %>%
      dplyr::rename(dialogue = text) %>% 
      dplyr::mutate(paragraph_id = cumsum(paste(question, person) != paste(dplyr::lag(question), dplyr::lag(person)))) %>%
      dplyr::group_by(paragraph_id, question, person) %>%
      dplyr::summarise(dialogue = paste(dialogue, collapse = " "), .groups = "drop")
    
  }) 

# Save ----
pac_199798_hc8061 <- list(meta = meta, transcript = text)



