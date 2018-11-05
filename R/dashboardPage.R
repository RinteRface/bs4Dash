#' Create a Boostrap 4 dashboard page
#'
#' Build an adminLTE3 dashboard page
#'
#' @param navbar Slot for \link{bs4DashNavbar}.
#' @param sidebar Slot for \link{bs4DashSidebar}.
#' @param body Slot for \link{bs4DashBody}.
#' @param controlbar Slot for \link{bs4DashControlbar} (right side).
#' @param footer Slot for \link{bs4DashFooter}.
#' @param title App title.
#' @param old_school Whether to use the wonderful sketchy design for Bootstrap 4. FALSE
#' by default.
#' @param sidebar_collapsed Whether the sidebar is collapsed of not at start. FALSE by default.
#'
#' @examples
#' if(interactive()){
#'  library(shiny)
#'  library(bs4Dash)
#'
#'  shiny::shinyApp(
#'    ui = bs4DashPage(
#'     navbar = bs4DashNavbar(),
#'     sidebar = bs4DashSidebar(),
#'     controlbar = bs4DashControlbar(),
#'     footer = bs4DashFooter(),
#'     title = "test",
#'     body = bs4DashBody()
#'    ),
#'    server = function(input, output) {}
#'  )
#' }
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
bs4DashPage <- function(navbar = NULL, sidebar = NULL, body = NULL, 
                        controlbar = NULL, footer = NULL, title = NULL,
                        old_school = FALSE, sidebar_collapsed = FALSE){
  
  if (!is.null(sidebar)) {
    sidebar_cl <- if (sidebar_collapsed) "sidebar-collapse" else "sidebar-open"
  }

  shiny::tags$html(
    # Head
    shiny::tags$head(
      shiny::tags$meta(charset = "utf-8"),
      shiny::tags$meta(
        name = "viewport",
        content = "width=device-width, initial-scale=1"
      ),
      shiny::tags$meta(`http-equiv` = "x-ua-compatible", content = "ie=edge"),

      shiny::tags$title(title)
    ),
    # Body
    addDeps(
      theme = old_school,
      shiny::tags$body(
        class = paste0("hold-transition sidebar-mini ", sidebar_cl),
        shiny::tags$div(
          class = "wrapper",
          navbar,
          sidebar,
          # page content
          body,
          controlbar,
          footer
        )
      )
    )
  )
}
