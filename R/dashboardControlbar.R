#' Create a Boostrap 4 dashboard right sidebar
#'
#' Build an adminLTE3 dashboard right sidebar
#'
#' @param ... Any UI element.
#' @param skin Controlbar skin. "dark" or "light".
#' @param title Controlbar title.
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
bs4DashControlbar <- function(..., skin = "dark", title = NULL) {

  shiny::tags$aside(
    class = paste0("control-sidebar control-sidebar-", skin),
    shiny::tags$div(
      class = "p-3",
      shiny::tags$h5(title),
      ...
    )
  )
}
