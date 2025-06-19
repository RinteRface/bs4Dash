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
bs4DashGallery <- function() {
  # nocov start
  rlang::check_installed(c("thematic", "waiterr"), "to run `bs4DashGallery()`.")

  shiny::shinyAppDir(
    system.file(
      "examples/showcase", 
      package = 'bs4Dash', 
      mustWork = TRUE
    )
  )
  # nocov end
}
