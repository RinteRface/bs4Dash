#' Create a Boostrap 4 dashboard right sidebar
#'
#' Build an adminLTE3 dashboard right sidebar
#'
#' @param ... Any UI element.
#' @param id To access the current state of the controlbar. Open is TRUE, closed
#' is FALSE. NULL by default.
#' @param disable If \code{TRUE}, the sidebar will be disabled.
#' @param width Controlbar width. 250 px by default.
#' @param collapsed Whether the control bar on the right side is collapsed or not at start. TRUE by default.
#' @param overlay Whether the sidebar covers the content when expanded. Default to TRUE.
#' @param skin Controlbar skin. "dark" or "light".
#' @param pinned Whether to block the controlbar state (TRUE or FALSE). Default to NULL.
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
bs4DashControlbar <- function(..., id = NULL, disable = FALSE, width = 250,
                              collapsed = TRUE, overlay = TRUE, skin = "dark", 
                              pinned = NULL) {
  
  if (is.null(id)) inputId <- "controlbarId"

  controlbarTag <- shiny::tags$aside(
    class = paste0("control-sidebar control-sidebar-", skin),
    id = id,
    `data-collapsed` = if (collapsed) "true" else "false",
    `data-overlay` = if (overlay) "true" else "false",
    `data-show` = if (disable) "false" else "true",
    `data-pin` = if (!is.null(pinned)) tolower(pinned),
    `data-slide` = "true",
    if (!is.null(pinned)) {
      shiny::tags$button(
        id = "controlbarPin",
        class = "m-2 p-1 btn btn-xs btn-outline-secondary",
        type = "button", 
        shiny::icon("thumbtack")
      )
    },
    shiny::tags$div(
      class = "p-3",
      id = "controlbarTitle",
      ...
    )
  )
  
  # change controlbar width
  shiny::tagList(
    shiny::singleton(
      shiny::tags$head(
        shiny::tags$style(
          shiny::HTML(
            paste0(
              ".control-sidebar, .control-sidebar:before {
                width: ", width, "px;
                right: ", -width, "px;
                bottom: 0;
                transition: right 0.3s ease-in-out;
              }
              "
            )
          )
        )
      )
    ),
    controlbarTag
  )
  
}




#' @inheritParams tabsetPanel
#' @export
controlbarMenu <- tabsetPanel




#' @inheritParams shiny::tabPanel
#' @export
controlbarItem <- shiny::tabPanel




#' @inheritParams shiny::updateTabsetPanel
#' @export
updateControlbarMenu <- shiny::updateTabsetPanel




#' Function to programmatically toggle the state of the controlbar
#'
#' @param id Controlbar id.
#' @param session Shiny session object.
#' @export
#'
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(bs4Dash)
#'  
#'  shinyApp(
#'    ui = dashboardPage(
#'      header = dashboardHeader(),
#'      sidebar = dashboardSidebar(),
#'      body = dashboardBody(
#'        actionButton(inputId = "controlbarToggle", label = "Toggle Controlbar")
#'      ),
#'      controlbar = dashboardControlbar(
#'        id = "controlbar",
#'        collapsed = FALSE,
#'        overlay = TRUE
#'      ),
#'      title = "updateControlbar"
#'    ),
#'    server = function(input, output, session) {
#'      
#'      observeEvent(input$controlbar, {
#'        if (input$controlbar) {
#'          showModal(modalDialog(
#'            title = "Alert",
#'            "The controlbar is opened.",
#'            easyClose = TRUE,
#'            footer = NULL
#'          ))
#'        }
#'      })
#'      
#'      observeEvent(input$controlbarToggle, {
#'        updateControlbar(id = "controlbar", session = session)
#'      })
#'      
#'      observe({
#'        print(input$controlbar)
#'      })
#'    }
#'  )
#' }
updateControlbar <- function(id, session = shiny::getDefaultReactiveDomain()) {
  session$sendInputMessage(id, NULL)
}