#' Create a Boostrap 4 dashboard page
#'
#' Build an adminLTE3 dashboard page
#'
#' @param ... Body content.
#' @param navbar Bootstrap 4 dashboard navbar.
#' @param sidebar Bootstrap 4 dashboard main sidebar.
#' @param controlbar Bootstrap 4 dashboard control sidebar (right side).
#' @param footer Bootstrap 4 dashboard footer.
#' @param title App title.
#'
#' @examples
#' if(interactive()){
#'  library(shiny)
#'
#'  shiny::shinyApp(
#'    ui = bs4DashboardPage(
#'     navbar = bs4DashboardNavbar(),
#'     sidebar = bs4DashboardSidebar(),
#'     controlbar = bs4DashboardControlbar(),
#'     footer = bs4DashboardFooter(),
#'     title = "test"
#'    ),
#'    server = function(input, output) {}
#'  )
#' }
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
bs4DashboardPage <- function(..., navbar = NULL, sidebar = NULL, controlbar = NULL, footer = NULL, title = NULL){

  shiny::tags$html(
    # Head
    shiny::tags$head(
      shiny::tags$meta(charset = "utf-8"),
      shiny::tags$meta(
        name = "viewport",
        content = "width=device-width, initial-scale=1"
      ),
      shiny::tags$meta(`http-equiv` = "x-ua-compatible", content = "ie=edge"),

      shiny::tags$title(title),

      shiny::includeCSS(system.file("css/font-awesome.min.css", package = "shinydashboard2")),
      shiny::includeCSS(system.file("css/adminlte.min.css", package = "shinydashboard2")),
      shiny::tags$link(href = "https://fonts.googleapis.com/css?family=Source+Sans+Pro:300,400,400i,700", rel = "stylesheet"),
      shiny::includeScript(system.file("js/bootstrap.bundle.min.js", package = "shinydashboard2")),
      shiny::includeScript(system.file("js/adminlte.min.js", package = "shinydashboard2"))

    ),
    # Body
    shiny::tags$body(
      class = "hold-transition sidebar-mini",
      shiny::tags$div(
        class = "wrapper",
        navbar,
        sidebar,
        # page content
        shiny::tags$div(
          class = "content-wrapper",
          ...
        ),
        controlbar,
        footer
      )
    )
  )
}
