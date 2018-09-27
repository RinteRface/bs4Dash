#' Create a Boostrap 4 dashboard right sidebar
#'
#' Build an adminLTE3 dashboard right sidebar
#'
#' @param ... Any UI element.
#' @param skin Controlbar skin. "dark" or "light".
#' @param title Controlbar title.
#' @param width COntrolbar width. 250 px by default.
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
bs4DashControlbar <- function(..., skin = "dark", title = NULL, width = 250) {

  controlbarTag <- shiny::tags$aside(
    class = paste0("control-sidebar control-sidebar-", skin),
    shiny::tags$div(
      class = "p-3",
      shiny::tags$h5(title),
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
