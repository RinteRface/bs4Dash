#' Create a Boostrap 4 dashboard right sidebar
#'
#' Build an adminLTE3 dashboard right sidebar
#'
#' @param ... Any UI element.
#' @param id To access the current state of the controlbar. Open is TRUE, closed
#' is FALSE. NULL by default.
#' @param disable If \code{TRUE}, the sidebar will be disabled.
#' @param width Controlbar width. This must either be a number which specifies the width 
#' in pixels, or a string that specifies the width in CSS units. 250 px by default.
#' @param collapsed Whether the control bar on the right side is collapsed or not at start. TRUE by default.
#' @param overlay Whether the sidebar covers the content when expanded. Default to TRUE.
#' @param skin Controlbar skin. "dark" or "light".
#' @param pinned Whether to block the controlbar state (TRUE or FALSE). Default to NULL.
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @rdname dashboardControlbar
#'
#' @export
bs4DashControlbar <- function(..., id = NULL, disable = FALSE, width = 250,
                              collapsed = TRUE, overlay = TRUE, skin = "dark",
                              pinned = NULL) {
  if (is.null(id)) id <- "controlbarId"

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
        shiny::icon("thumbtack", class = if (pinned) "fa-lg")
      )
    },
    shiny::tags$div(
      class = "control-sidebar-content",
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
                width: ", shiny::validateCssUnit(width), ";
                right: -", shiny::validateCssUnit(width), ";
                bottom: 0;
                transition: right 0.3s ease-in-out;
              }
              .control-sidebar-slide-open.control-sidebar-push .content-wrapper, 
              .control-sidebar-slide-open.control-sidebar-push .main-footer, 
              .control-sidebar-slide-open.control-sidebar-push-slide .content-wrapper, 
              .control-sidebar-slide-open.control-sidebar-push-slide .main-footer {
                margin-right: ", shiny::validateCssUnit(width), ";
              }

              @media (min-width: 768px)
                body:not(.sidebar-mini-md) .content-wrapper, 
                body:not(.sidebar-mini-md) .main-footer, 
                body:not(.sidebar-mini-md) .main-header {
                  transition: margin-left .3s ease-in-out;
                  margin-left: ", shiny::validateCssUnit(width), ";
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
#' @rdname dashboardControlbar
#' @export
controlbarMenu <- tabsetPanel




#' @inheritParams shiny::tabPanel
#' @rdname dashboardControlbar
#' @export
controlbarItem <- function(title, ..., value = title, icon = NULL) {
  shiny::tabPanel(title, ..., value = value, icon = icon)
}




#' @inheritParams shiny::updateTabsetPanel
#' @rdname dashboardControlbar
#' @export
updateControlbarMenu <- function (session = getDefaultReactiveDomain(), inputId, selected = NULL) {
  shiny::updateTabsetPanel(session, inputId, selected)
}
  




#' Function to programmatically toggle the state of the controlbar
#'
#' @param id Controlbar id.
#' @param session Shiny session object.
#' @export
#'
#' @rdname dashboardControlbar
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   library(bs4Dash)
#'
#'   shinyApp(
#'     ui = dashboardPage(
#'       header = dashboardHeader(),
#'       sidebar = dashboardSidebar(),
#'       body = dashboardBody(
#'         actionButton(inputId = "controlbarToggle", label = "Toggle Controlbar")
#'       ),
#'       controlbar = dashboardControlbar(
#'         id = "controlbar",
#'         collapsed = FALSE,
#'         overlay = TRUE
#'       ),
#'       title = "updateControlbar"
#'     ),
#'     server = function(input, output, session) {
#'       observeEvent(input$controlbar, {
#'         if (input$controlbar) {
#'           showModal(modalDialog(
#'             title = "Alert",
#'             "The controlbar is opened.",
#'             easyClose = TRUE,
#'             footer = NULL
#'           ))
#'         }
#'       })
#'
#'       observeEvent(input$controlbarToggle, {
#'         updateControlbar(id = "controlbar", session = session)
#'       })
#'
#'       observe({
#'         print(input$controlbar)
#'       })
#'     }
#'   )
#' }
updateControlbar <- function(id, session = shiny::getDefaultReactiveDomain()) {
  session$sendInputMessage(id, NULL)
}