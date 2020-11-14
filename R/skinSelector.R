#' AdminLTE3 skin selector
#' 
#' This creates a skin selector element.
#'
#' @export
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(bs4Dash)
#'  
#'  shinyApp(
#'    ui = dashboardPage(
#'      header = dashboardHeader(),
#'      sidebar = dashboardSidebar(),
#'      body = dashboardBody(),
#'      controlbar = dashboardControlbar(skinSelector(), pinned = TRUE),
#'      title = "Skin Selector"
#'    ),
#'    server = function(input, output) { }
#'  )
#' }
skinSelector <- function() {
  navbar_themer <- tagList(
    h6("Navbar themer"),
    div(
      class = "d-flex",
      div(
        class = "d-flex flex-wrap mb-3",
        lapply(validStatusesPlus, function(theme) {
          div(
            class = sprintf("bg-%s elevation-2", theme),
            style = "width: 40px; height: 20px; border-radius: 25px; margin-right: 10px; margin-bottom: 10px; opacity: 0.8; cursor: pointer;",
            onclick = sprintf("updateNavbarTheme('navbar-%s');", theme)
          )
        })
      )
    )
  )
  navbar_themer
}