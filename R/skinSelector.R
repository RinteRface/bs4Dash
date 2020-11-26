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
#'      sidebar = dashboardSidebar(
#'       sidebarMenu(
#'        menuItem(
#'         text = "Item 1"
#'        ),
#'        menuItem(
#'         text = "Item 2"
#'        )
#'       )
#'      ),
#'      body = dashboardBody(),
#'      controlbar = dashboardControlbar(skinSelector(), pinned = TRUE),
#'      title = "Skin Selector"
#'    ),
#'    server = function(input, output) { }
#'  )
#' }
skinSelector <- function() {
  lapply(themer_list, createThemer, skins = validStatusesPlus)
}

themer_list <- c("Navbar", "Sidebar", "Accents", "Controlbar")


createThemer <- function(name, skins) {
  
  titleTag <- shiny::h6(sprintf("%s themer", name))
  
  shiny::tagList(
    if (name %in% c("Sidebar", "Controlbar")) {
      shiny::fluidRow(
        titleTag,
        shiny::div(
          class = "mx-2 custom-control custom-switch", 
          shiny::tags$input(
            id = sprintf("%s-skin", tolower(name)),
            type = "checkbox", 
            class = "custom-control-input"
          ),
          shiny::tags$label(
            shiny::icon("sun", class = sprintf("%s-themer-icon", tolower(name))), 
            `for` = sprintf("%s-skin", tolower(name)), 
            class = "custom-control-label"
          )
        )
      )
    } else {
      titleTag
    },
    if (name != "Controlbar") {
      shiny::div(
        class = "d-flex",
        shiny::div(
          class = "d-flex flex-wrap mb-3",
          lapply(skins, function(theme) {
            shiny::div(
              class = sprintf("bg-%s elevation-2 %s-themer-chip", theme, tolower(name)),
              style = "width: 40px; height: 20px; border-radius: 25px; margin-right: 10px; margin-bottom: 10px; opacity: 0.8; cursor: pointer;",
              onclick = if (name == "Navbar") {
                sprintf("update%sTheme('navbar-%s');", name, theme)
              } else if (name == "Sidebar") {
                sprintf("update%sTheme('%s');", name, theme)
              } else if (name == "Accents") {
                sprintf("update%sTheme('accent-%s');", name, theme)
              }
            )
          })
        )
      )
    }
  )
}