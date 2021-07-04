library(shiny)
library(magrittr)
library(publicaccountstm)

data("pac_equip_oral")
data("pac_equip_sentiment")

meta <- purrr::map_df(pac_equip_oral, "meta")

ui <- fluidPage(
  highlight_sentimentUI(),
  tags$head(
    tags$style(HTML("#transcript { height:400px; overflow-y:scroll}"))
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
    
    purrr::map_df(pac_equip_sentiment, sentimentr::uncombine) %>%
      dplyr::left_join(meta, by = "publication_reference") %>%
      dplyr::mutate(id = match(publication_reference, meta$publication_reference)) %>%
      dplyr::group_by(id, session, report_title, inquiry,
                      publication_date, publication_reference) %>%
      dplyr::summarise(sentiment_time = createSpark(sentiment),
                       sentiment_spread = createSparkBox(sentiment),
                       .groups = "drop") %>% 
      DT::datatable(
        escape = F, rownames = F,
        selection = list(target = "row", selected = 1, mode = "single"),
        extensions = 'Scroller',
        options = list(
          dom = "t",
          scrollY = 200,
          scroller = TRUE,
          deferRender = TRUE,
          fnDrawCallback = htmlwidgets::JS('function(){ HTMLWidgets.staticRender(); }')
        )
      ) %>% 
      sparkline::spk_add_deps()
  })
  
  observeEvent(input$meta_row_last_clicked, {
    values$transcript_select <- input$meta_row_last_clicked
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  output$transcript <- renderUI({
    highlight_sentiment(pac_equip_sentiment[[values$transcript_select]])
  })
  
}

shinyApp(ui, server)