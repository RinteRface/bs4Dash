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
useAutoColor <- function(input, output, session = shiny::getDefaultReactiveDomain()) {
  input <- get("input", envir = parent.frame())
  # Now we need to set up a initial theme so that
  # session$setCurrentTheme does not complain about
  # changing the bootstrap version
  theme <- bslib::bs_theme(version = 4)
  shiny::shinyOptions(bootstrapTheme = theme)
  # input$dark_mode is created on the client
  shiny::observeEvent(input$dark_mode, {
    # We actually don't do anything fancy. We send the same theme as
    # the initial one
    session$setCurrentTheme(theme)
  })
}
