#' Highlight Sentiment UI
#'
#' @return Style Tags
#' @export
#'
#' @examples \dontrun{ highlight_sentimentUI()}
highlight_sentimentUI <- function(rmd = FALSE){
  .globals$astext <- rmd
  insertHead(
    shiny::tags$style(shiny::HTML(
      "mark.pos {background-color: lightgreen; color: black;}
     mark.neg {background-color: pink; color: black;}
     .indented {\n    margin-left: 5%%;\n    margin-right: 5%%;\n}
       "
    ))
  )
}


#' Highlight Sentiment
#'
#' @param x A \code{sentimentr::sentiment_by} object
#'
#' @return HTML for use in shiny app
#' @export
#'
#' @examples \dontrun{ highlight_sentiment(x)}
#' @import data.table
highlight_sentiment <- function(x){
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
  y[, `:=`(txt, sprintf("<h4>%s: <em><span style=\"color: %s\">%s</span></em></h4><p class=\"indented\">%s</p>", 
                        grouping.var, ifelse(sentiment < 0, "red", ifelse(sentiment > 
                                                                            0, "green", "#D0D0D0")), sentimentr:::formdig(sentiment, 
                                                                                                                          digits), txt))]
  body <- gsub(" rsreplacers", "", paste(y[["txt"]], 
                                         collapse = "\n"))
  
  HTML(body)
}