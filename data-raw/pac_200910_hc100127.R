library(magrittr)

committee <- "Public Accounts Committee"
session <- "2009-10"
report_title <- "Twenty-third Report"
inquiry <- "Ministry of Defence: Major Projects report 2009"
publication_reference <- "HC 338"

inquiry_link <- "https://publications.parliament.uk/pa/cm200910/cmselect/cmpubacc/338/33802.htm"
publication_link <- "https://publications.parliament.uk/pa/cm200910/cmselect/cmpubacc/338/10012701.htm"

# Extract Meta ---- 
members_present <- 
  paste("Mr Edward Leigh, in the Chair",
        "Mr Richard Bacon", "Nigel Griffiths",
        "Mr Douglas Carswell",	"Mr Austin Mitchell",
        "Mr David Curry",	"Dr John Pugh",
        "Mr Ian Davidson", sep = ";")

publication_title <- "Ministry of Defence: Major Projects Report 2009"
publication_date <- "Wednesday 27 January 2010"

witnesses_present <-  
  paste("Sir Bill Jeffrey KCB, Permanent Under Secretary of State for Defence", 
  "General Sir Kevin O'Donoghue KCB CBE, Chief of Defence Materiel",
  "Vice Admiral Paul Lambert CB, Deputy Chief of Defence Staff (Capability)",
  sep = "; ")
  
meta <- dplyr::tibble(
  committee, inquiry, report_title, session, inquiry_link,
  publication_title, publication_date, 
  members_present, witnesses_present, publication_link,
  publication_reference
)

# Extract Text ----
url <- "https://publications.parliament.uk/pa/cm200910/cmselect/cmpubacc/338/100127{p}.htm"
pages <- 2:8

src <- pages %>%
  purrr::map(function(p){
    p <- ifelse(nchar(p)==1, paste0(0,p), p)
    stringr::str_glue(url) %>%
      xml2::read_html()
  })

text <- src %>%
  purrr::map_df(function(x){
    nodes <- xml2::xml_find_all(x, "//div[@id='maincontent']
                       //table") %>%
      magrittr::extract(3) %>%
      xml2::xml_children() %>%
      xml2::xml_children() %>%
      xml2::xml_children()
    
    tbls <- nodes %>%
      purrr::map_df(function(x){
        contents <- xml2::xml_contents(x)
        bold <- contents[(xml2::xml_name(contents)=="b")] %>%
          xml2::xml_text()
        
        italics <- contents[(xml2::xml_name(contents)=="i")]
        bold_italics <- italics[(xml2::xml_name(xml2::xml_contents(italics)) == "b")] %>%
          xml2::xml_text()
        text <- xml2::xml_text(xml2::xml_contents(x)) %>% paste(collapse ="")
        
        bold <- ifelse(is.null(bold), NA, bold)
        bold_italics <- ifelse(is.null(bold_italics), NA, bold_italics)
        
        dplyr::tibble(bold, bold_italics, text)
        
      }) %>%
      dplyr::mutate(text = stringr::str_replace_all(text, "\r\n", " ")) %>%
      dplyr::mutate(text = stringr::str_trim(text)) %>%
      dplyr::mutate(question = stringr::str_extract(text, "^Q[0-9]{3}|^Q[0-9]{2}|^Q[0-9]{1}"))
    
    first_row <- (!is.na(tbls$question)) %>% which() %>% min()
    
    tbls %>%
      dplyr::slice(first_row:nrow(tbls)) %>%
      dplyr::mutate(person = ifelse(is.na(bold), bold_italics, bold)) %>%
      tidyr::fill(question, .direction = "down") %>%
      dplyr::mutate(person = stringr::str_remove_all(person, "^Q[0-9]{3}|^Q[0-9]{2}|^Q[0-9]{1}")) %>%
      dplyr::mutate(person = stringr::str_trim(person)) %>%
      dplyr::mutate(person = stringr::str_remove(person, ":")) %>%
      dplyr::select(question, person, dialogue = text)
    
  }) %>%
  dplyr::filter(dialogue != "")  %>%
  dplyr::mutate(paragraph_id = cumsum(paste(question, person) != paste(dplyr::lag(question), dplyr::lag(person)))) %>%
  dplyr::group_by(paragraph_id, question, person) %>%
  dplyr::summarise(dialogue = paste(dialogue, collapse = " "), .groups = "drop")

# Save ----
pac_200910_hc100127 <- list(meta = meta, transcript = text)
save(object = pac_200910_hc100127, file = "data/pac_200910_hc100127.rda")
