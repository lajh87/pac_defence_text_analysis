d <- "data-raw"
files <- list.files(d, pattern = "pac_")

pac <- purrr::map(file.path(d, files), function(f){
  source(f)
})


pac[[15]]$value$meta$publication_date <- "Wednesday 1 March 2017"

pac_equip_oral <- purrr::map(pac, function(x) x$value)

usethis::use_data(pac_equip_oral, overwrite = TRUE)


