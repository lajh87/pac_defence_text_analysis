library(shiny)
library(magrittr)

load("pac_defence_equip_sentiment.rda")
load("pac_defence_equip_meta.rda")

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
                  #transcript {
                    height:400px;
                    overflow-y:scroll
                  }
                  ")),
    tags$style(HTML(
      "mark.pos {background-color: lightgreen; 
                 color: black;}
       mark.neg {background-color: pink; 
                 color: black;}
       h1 { \n    display: block;\n    font-size: 1.2em;\n    margin-top: 0.0em;\n    margin-bottom: 0.0em;\n    margin-left: 0;\n    margin-right: 0;\n    font-weight: bold;\n}\n.indented {\n    margin-left: 5%%;\n    margin-right: 5%%;\n}
       "
    ))
  ),
  tabsetPanel(id = "tabs",
              tabPanel("Summary Data", 
                       DT::dataTableOutput("meta"), 
                       uiOutput("transcript"))
  )
)

server <- function(input, output, session) {
  
  values <- reactiveValues(transcript_select = 1)
  
  output$meta <- DT::renderDataTable({
    
    createSpark <- function(x){
      if(length(x)<100){
        x <- stats::approx(x = seq_along(x), y = x, n = 100)[['y']]
      }
      sparkline::sparkline(syuzhet::get_dct_transform(x)) %>%
        htmltools::as.tags() %>%
        as.character()
    }
    
    createSparkBox <- function(x){
      sparkline::sparkline(x, type = "box") %>%
        htmltools::as.tags() %>%
        as.character()
    }
    
    createSparkBar <- function(x){
      sparkline::sparkline(x, type = "bar") %>%
        htmltools::as.tags() %>%
        as.character()
    }
    
      purrr::map_df(sentiment, sentimentr::uncombine) %>%
        dplyr::left_join(meta) %>%
        dplyr::mutate(id = match(publication_reference, meta$publication_reference)) %>%
      dplyr::group_by(id, session, report_title, inquiry,
                      publication_date, publication_reference) %>%
      dplyr::summarise(sentiment_time = createSpark(sentiment),
                       sentiment_spread = createSparkBox(sentiment),
                       .groups = "drop") %>% 
      DT::datatable(escape = F,
                    rownames = F,
                    selection = 'single',
                    extensions = 'Scroller',
                    options = list(
                      dom = "t",
                      scrollY = 200,
                      scroller = TRUE,
                      deferRender = TRUE,
                      fnDrawCallback = htmlwidgets::JS(
                        'function(){ HTMLWidgets.staticRender(); }'
                      )
                    )
      ) %>% 
      sparkline::spk_add_deps()
  })
  
  observeEvent(input$meta_row_last_clicked, {
    values$transcript_select <- input$meta_row_last_clicked
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  output$transcript <- renderUI({
    x <- sentiment[[values$transcript_select]]
    digits <- 3
    polarity <- grouping.var <- NULL
    if (!inherits(x, "sentiment_by")) 
      stop("Must be a `sentiment_by` object")
    y <- sentimentr::uncombine(x)
    grps <- attributes(x)[["groups"]]
    data.table::setDT(y)

    y[, `:=`(polarity, ifelse(sentiment > 0, "pos", 
                              ifelse(sentiment < 0, "neg", "")))][, `:=`(polarity, 
                                                                         ifelse(is.na(polarity), "", polarity))]
    txt <- sentimentr::get_sentences(x)
    y[["txt"]] <- unlist(txt)
    y[, `:=`(txt, ifelse(polarity == "", txt, sprintf("<mark class = \"%s\">%s</mark>", 
                                                      polarity, txt)))]
    mygrps_1 <- paste(sprintf("%s=%s[1L]", grps, grps), 
                      collapse = ", ")
    mygrps_2 <- parse(text = sprintf("paste(%s, sep=\", \")", 
                                     paste(grps, collapse = ", ")))
    suppressWarnings(y[, `:=`(gr, {
      gr = eval(mygrps_2)
      cumsum(c(TRUE, gr[-1] != gr[-.N]))
    })])
    y <- y[, list(sentiment = attributes(x)[["averaging.function"]](sentiment), 
                  txt = paste(txt, collapse = " ")), by = c(grps, 
                                                            "gr")]
    mygrps <- parse(text = sprintf("paste(%s, sep=\"_\")", 
                                   paste(grps, collapse = ", ")))
    y[, `:=`(grouping.var, eval(mygrps))]
    y[, `:=`(txt, sprintf("<h1>%s: <em><span style=\"color: %s\">%s</span></em></h1><p class=\"indented\">%s</p>", 
                          grouping.var, ifelse(sentiment < 0, "red", ifelse(sentiment > 
                                                                              0, "green", "#D0D0D0")), sentimentr:::formdig(sentiment, 
                                                                                                                            digits), txt))]
    body <- gsub(" rsreplacers", "", paste(y[["txt"]], 
                                           collapse = "\n"))
    
    HTML(body)
  })
  
}

shinyApp(ui, server)