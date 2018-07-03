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
  if (!requireNamespace(package = "shinyWidgets"))
    message("Package 'shinyWidgets' is required to run this function")
  if (!requireNamespace(package = "plotly"))
    message("Package 'plotly' is required to run this function")
  
  shiny::shinyAppFile(system.file('examples/showcase/app.R', package = 'bs4Dash', mustWork = TRUE))
}