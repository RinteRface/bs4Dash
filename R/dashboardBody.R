#' Create a Boostrap 4 dashboard body
#'
#' Build an adminLTE3 dashboard body
#'
#' @param ... Body content.
#' 
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
bs4DashBody <- function(...) {
  shiny::tags$div(
    class = "content-wrapper",
    ...
  )
}
