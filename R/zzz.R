.onLoad <- function(...) {
  shiny::addResourcePath(
    "placeholders",
    system.file("placeholders", package = "bs4Dash")
  )
}
