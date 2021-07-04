library(magrittr)

# Meta ----
committee <- "Public Accounts Committee"
session <- "2007-08"
report_title <- "Thirty-Third Report"
inquiry <- "The Major Projects Report 2007"
publication_reference <- "HC 80317"

inquiry_link <- "https://publications.parliament.uk/pa/cm200708/cmselect/cmpubacc/433/43302.htm"
publication_link <- "https://publications.parliament.uk/pa/cm200708/cmselect/cmpubacc/433/8031701.htm"

members_present <- 
  paste("Mr Edward Leigh, in the Chair",
        "Mr Richard Bacon",	"Mr Austin Mitchell",
        "Nigel Griffiths",	"Mr Don Touhig",
        "Keith Hill",	
        sep = "; ")	

witnesses_present <- 
  paste("Sir Bill Jeffrey, KCB, Permanent Under Secretary",
        "General Sir Kevin O'Donoghue, KCB, CBE, Chief of Defence Materiel",
        "Lieutenant General Andrew Figgures, CBE, Deputy Chief of the Defence Staff (Equipment Capability)",
        sep = "; ")

publication_title <- "Oral evidence Taken before the Committee of Public Accounts"
publication_date <- "Monday 17 March 2008"

meta <- dplyr::tibble(
  committee, inquiry, report_title, session, inquiry_link,
  publication_title, publication_date, 
  members_present, witnesses_present, publication_link,
  publication_reference
)

# Text ---- 
url <- "https://publications.parliament.uk/pa/cm200708/cmselect/cmpubacc/433/80317{p}.htm"
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
pac_200708_hc80317 <- list(meta = meta, transcript = text)
