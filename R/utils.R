insertHead <- function(...) {
  if (is.null(.globals$astext) || .globals$astext) {
    shiny::tagList(...)
  } else {
    shiny::tags$head(...)
  }
}