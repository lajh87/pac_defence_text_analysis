library(magrittr)

# Meta ----
committee <- "Public Accounts Committee"
session <- "2003-04"
report_title <- "Forty-Third Report"
inquiry <- "Ministry of Defence: Major Projects Report 2003"
publication_reference <- "HC 40225"

inquiry_link <- "https://publications.parliament.uk/pa/cm200304/cmselect/cmpubacc/383/38302.htm"
publication_link <- "https://publications.parliament.uk/pa/cm200304/cmselect/cmpubacc/383/4022301.htm"

members_present <- 
  paste("Mr Edward Leigh, in the Chair",
        "Mr Richard Bacon",	"Mr Brian Jenkins",
        "Jon Cruddas",	"Mr Gerry Steinberg",
        "Mr Ian Davidson",	"Jon Trickett",
        "Mr Frank Field",	"Mr Alan Williams", 
        sep = "; ")	

witnesses_present <- 
  paste("Sir Peter Spencer KCB, Chief of Defence Procurement",
        "Lieutenant General Rob Fulton, Deputy Chief of the Defence Staff for Equipment Capability, Ministry of Defence",
        sep = "; ")

publication_title <- "Oral evidence Taken before the Committee of Public Accounts"
publication_date <- "Wednesday 25 February 2004"

meta <- dplyr::tibble(
  committee, inquiry, report_title, session, inquiry_link,
  publication_title, publication_date, 
  members_present, witnesses_present, publication_link,
  publication_reference
)

# Text ---- 

url <- "https://publications.parliament.uk/pa/cm200304/cmselect/cmpubacc/383/40225{p}.htm"
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
  dplyr::mutate(paragraph_id = cumsum(paste(question, person) != paste(dplyr::lag(question), dplyr::lag(person)))) %>%
  dplyr::group_by(paragraph_id, question, person) %>%
  dplyr::summarise(dialogue = paste(dialogue, collapse = " "), .groups = "drop")

# Save ----
pac_200304_hc40225 <- list(meta = meta, transcript = text)
