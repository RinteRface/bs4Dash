#' Plot auto-color module
#'
#' This piece of code is necessary so that plots get the
#' good background color, automatically. It requires the use
#' of the thematic package and shiny dev.
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#'
#' @return An observer telling Shiny to update the current theme. It has to be inserted
#' at the top of the main server function.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   library(bs4Dash)
#'   library(thematic)
#'
#'   thematic_shiny()
#'   shinyApp(
#'     ui = dashboardPage(
#'       header = dashboardHeader(
#'         title = bs4DashBrand(
#'           title = "My dashboard",
#'           color = "primary",
#'           href = "https://adminlte.io/themes/v3",
#'           image = "https://adminlte.io/themes/v3/dist/img/AdminLTELogo.png"
#'         )
#'       ),
#'       sidebar = dashboardSidebar(),
#'       body = dashboardBody(
#'         sliderInput("obs", "Number of observations:",
#'           min = 0, max = 1000, value = 500
#'         ),
#'         plotOutput("distPlot")
#'       ),
#'       controlbar = dashboardControlbar(),
#'       title = "DashboardPage"
#'     ),
#'     server = function(input, output, session) {
#'       useAutoColor()
#'       output$distPlot <- renderPlot({
#'         hist(rnorm(input$obs))
#'       })
#'     }
#'   )
#' }
useAutoColor <- function(input, output, session) {
  input <- get("input", envir = parent.frame())
  session <- shiny::getDefaultReactiveDomain()
  # input$dark_mode is created on the client
  shiny::observeEvent(input$dark_mode, {
    session$setCurrentTheme(NULL)
  })
}