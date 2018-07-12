#' @title Launch the bs4Dash Gallery
#'
#' @description A gallery of all components available in bs4Dash.
#' 
#' @param theme bs4Dash theme. Either "classic" or "old_school". "classic" by default.
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
bs4DashGallery <- function(theme = "classic") { # nocov start
  if (!requireNamespace(package = "shinyWidgets"))
    message("Package 'shinyWidgets' is required to run this function")
  if (!requireNamespace(package = "plotly"))
    message("Package 'plotly' is required to run this function")
  
  shiny::shinyAppFile(
    system.file(
      paste0("examples/showcase/", theme, "/app.R"), 
      package = 'bs4Dash', 
      mustWork = TRUE
    )
  )
}