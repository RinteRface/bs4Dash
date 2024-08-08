pak::pak("parmsam/r-shinylive@feat/encode-decode-url")

create_shinylive_links <- function(path) {

  dirs <- list.dirs(path)[-1]

  vapply(
    list.dirs(path)[-1],
    shinylive:::url_encode_dir,
    FUN.VALUE = character(1)
  )
}

create_vignettes_links <- function() {
  create_shinylive_links("inst/examples/vignettes-demos")
}

shinylive_links <- create_vignettes_links()
usethis::use_data(shinylive_links, internal = TRUE, overwrite = TRUE)