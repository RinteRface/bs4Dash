#' @title AdminLTE2 special large button
#'
#' @description Create a large button ideal for web applications but identical
#' to the classic Shiny action button.
#'
#' @inheritParams shiny::actionButton
#' @param color Button backgroun color. Valid statuses are defined as follows:
#' \itemize{
#'   \item \code{primary}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#007bff")}.
#'   \item \code{secondary}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#6c757d")}.
#'   \item \code{info}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#17a2b8")}.
#'   \item \code{success}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#28a745")}.
#'   \item \code{warning}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#ffc107")}.
#'   \item \code{danger}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#dc3545")}.
#'   \item \code{gray-dark}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#343a40")}.
#'   \item \code{gray}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#adb5bd")}.
#'   \item \code{light}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#1f2d3d")}.
#'   \item \code{indigo}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#6610f2")}.
#'   \item \code{lightblue}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#3c8dbc")}.
#'   \item \code{navy}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#001f3f")}.
#'   \item \code{purple}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#605ca8")}.
#'   \item \code{fuchsia}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#f012be")}.
#'   \item \code{pink}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#e83e8c")}.
#'   \item \code{maroon}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#d81b60")}.
#'   \item \code{orange}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#ff851b")}.
#'   \item \code{lime}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#01ff70")}.
#'   \item \code{teal}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#39cccc")}.
#'   \item \code{olive}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#3d9970")}.
#' }
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(bs4Dash)
#'  
#'  shinyApp(
#'   ui = dashboardPage(
#'     dashboardHeader(),
#'     dashboardSidebar(),
#'     dashboardBody(
#'      box(
#'       title = "App Buttons",
#'       status = NULL,
#'       appButton(
#'         inputId = "myAppButton",
#'         label = "Users", 
#'         icon = icon("users"), 
#'         dashboardBadge(textOutput("btnVal"), color = "blue")
#'       )
#'      )
#'     ),
#'     title = "App buttons"
#'   ),
#'   server = function(input, output) {
#'    output$btnVal <- renderText(input$myAppButton)
#'   }
#'  )
#' }
#'
#' @export
appButton <- function(..., inputId, label, icon = NULL, width = NULL, color = NULL) {
  
  if (!is.null(icon)) tagAssert(icon, type = "i")
  if (!is.null(color)) validateStatusPlus(color)
  
  shiny::tagAppendAttributes(
    shiny::actionButton(inputId, label, icon, width, ...),
    class = "btn-app",
    class = if (!is.null(color)) paste0("bg-", color)
  )
}