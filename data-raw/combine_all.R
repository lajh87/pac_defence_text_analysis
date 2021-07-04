rm(list=ls())
d <- "data"
files <- list.files(d)

for(file in files){
  load(file.path(d, file))
}

rm(d, files, file)  

objects <- ls()

meta <- purrr::map_df(objects, function(x){
  get(x)$meta
}) 

meta$publication_date[15] <- "Wednesday 1 March 2017"
meta$publication_date <- as.Date(meta$publication_date, format = "%A %d %B %Y")


text <- purrr::map_df(objects, function(x){
  publication_reference <- get(x)$meta$publication_reference
  dplyr::tibble(publication_reference, get(x)$transcript)
})

pac_defence_equip <- list(meta = meta, transcript = text)
save(object = pac_defence_equip, file =  "data/pac_defence_equip.rda")
