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

themer_list <- c("Navbar", "Sidebar")


createThemer <- function(name, skins) {
  
  titleTag <- h6(sprintf("%s themer", name))
  
  tagList(
    if (name == "Sidebar") {
      fluidRow(
        titleTag,
        div(
          class = "mx-2 custom-control custom-switch", 
          tags$input(
            id = "sidebar_skin",
            type = "checkbox", 
            class = "custom-control-input"
          ),
          tags$label(
            shiny::icon("sun", class = "sidebar-themer-icon"), 
            `for` = "sidebar_skin", 
            class = "custom-control-label"
          )
        )
      )
    } else {
      titleTag
    },
    div(
      class = "d-flex",
      div(
        class = "d-flex flex-wrap mb-3",
        lapply(skins, function(theme) {
          div(
            class = sprintf("bg-%s elevation-2", theme),
            style = "width: 40px; height: 20px; border-radius: 25px; margin-right: 10px; margin-bottom: 10px; opacity: 0.8; cursor: pointer;",
            onclick = if (name == "Navbar") {
              sprintf("update%sTheme('navbar-%s');", name, theme)
            } else {
              sprintf("update%sTheme('%s');", name, theme)
            }
          )
        })
      )
    )
  )
}