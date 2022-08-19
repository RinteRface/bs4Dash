#' @title Launch the bs4Dash Gallery
#'
#' @description A gallery of all components available in bs4Dash.
#' 
#' @export
#'
#' @examples
#'
#' if (interactive()) {
#'
#'  bs4DashGallery()
#'
#' }
bs4DashGallery <- function() { # nocov start
  if (!requireNamespace(package = "thematic"))
    message("Package 'thematic' is required to run this function")
  if (!requireNamespace(package = "waiter"))
    message("Package 'waiter' is required to run this function")
  
  shiny::shinyAppDir(
    system.file(
      "examples/showcase", 
      package = 'bs4Dash', 
      mustWork = TRUE
    )
  )
}
