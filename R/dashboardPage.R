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
#' @param old_school Whether to use the wonderful sketchy design for BS4. FALSE
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

      shiny::tags$title(title),

      # icons
      shiny::includeCSS(system.file("css/font-awesome.min.css", package = "bs4Dash")),
      shiny::tags$link(href = "https://use.fontawesome.com/releases/v5.0.13/css/all.css", rel = "stylesheet"),
      shiny::tags$link(href = "https://unpkg.com/ionicons@4.4.2/dist/css/ionicons.min.css", rel = "stylesheet"),
      # AdminLTE3
      shiny::includeCSS(system.file("css/adminlte.min.css", package = "bs4Dash")),
      
      # Old school design (bootswatch sketchy design)
      if (old_school) {
        shiny::tags$link(href = "https://bootswatch.com/4/sketchy/bootstrap.min.css", rel = "stylesheet")
      },
      shiny::tags$link(href = "https://fonts.googleapis.com/css?family=Source+Sans+Pro:300,400,400i,700", rel = "stylesheet")
    ),
    # Body
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
      ),
      # javascript
      shiny::includeScript(system.file("js/bootstrap.bundle.min.js", package = "bs4Dash")),
      shiny::includeScript(system.file("js/adminlte.min.js", package = "bs4Dash")),
      shiny::includeScript(system.file("js/bs4Dash.js", package = "bs4Dash"))
    )
  )
}
