#' Create a Boostrap 4 dashboard page
#'
#' Build an adminLTE3 dashboard page
#'
#' @param navbar Bootstrap 4 dashboard navbar.
#' @param sidebar Bootstrap 4 dashboard main sidebar.
#' @param body Bootstrap 4 dashboard body wrapper.
#' @param controlbar Bootstrap 4 dashboard control sidebar (right side).
#' @param footer Bootstrap 4 dashboard footer.
#' @param title App title.
#'
#' @examples
#' if(interactive()){
#'  library(shiny)
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
bs4DashPage <- function(navbar = NULL, sidebar = NULL, body = NULL, controlbar = NULL, footer = NULL, title = NULL){

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

      # icons
      shiny::includeCSS(system.file("css/font-awesome.min.css", package = "bs4Dash")),
      shiny::tags$link(href = "https://use.fontawesome.com/releases/v5.0.4/css/all.css", rel = "stylesheet"),
      shiny::tags$link(href = "http://code.ionicframework.com/ionicons/2.0.1/css/ionicons.min.css", rel = "stylesheet"),
      # AdminLTE3
      shiny::includeCSS(system.file("css/adminlte.min.css", package = "bs4Dash")),
      
      shiny::tags$link(href = "https://fonts.googleapis.com/css?family=Source+Sans+Pro:300,400,400i,700", rel = "stylesheet"),
      # javascript
      shiny::includeScript(system.file("js/bootstrap.bundle.min.js", package = "bs4Dash")),
      shiny::includeScript(system.file("js/adminlte.min.js", package = "bs4Dash"))

    ),
    # Body
    shiny::tags$body(
      class = "hold-transition sidebar-mini",
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
}
