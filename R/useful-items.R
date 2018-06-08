#' Create a Bootstrap 4 dashboard badge item
#' 
#' Beautiful badge from AdminLTE3 
#'
#' @param ... BDage content.
#' @param position Badge position: "left" or "right".
#' @param status Bdage color. "primary", "danger", "info", "success" or "warning"
#' 
#' @examples 
#' if(interactive()){
#'  library(shiny)
#'  
#'  shiny::shinyApp(
#'    ui = bs4DashPage(
#'      navbar = bs4DashNavbar(),
#'      sidebar = bs4DashSidebar(),
#'      controlbar = bs4DashControlbar(),
#'      footer = bs4DashFooter(),
#'      title = "test",
#'      body = bs4DashBody(
#'       bs4DashBadge(
#'        position = "right",
#'        status = "warning",
#'        "Warning"
#'       )
#'      )
#'    ),
#'    server = function(input, output) {}
#'  )
#' }
#' 
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
bs4DashBadge <- function(..., position = c("left", "right"), status) {
  
  position <- match.arg(position)
  
  shiny::tags$span(
    class = paste0(position, " badge", " badge-", status),
    ...
  )
}