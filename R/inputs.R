#' Bootstrap 4 Action button/link
#'
#' Creates an action button or link whose value is initially zero, and increments by one
#' each time it is pressed.
#'
#' @param inputId The `input` slot that will be used to access the value.
#' @param label The contents of the button or link--usually a text label, but
#'   you could also use any other HTML, like an image.
#' @param icon An optional [icon()] to appear on the button.
#' @param width The width of the input, e.g. `'400px'`, or `'100%'`;
#'   see [validateCssUnit()].
#' @param ... Named attributes to be applied to the button or link.
#' @param status Button status color. Valid statuses are defined as follows:
#' \itemize{
#'   \item \code{primary}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#007bff")}.
#'   \item \code{secondary}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#6c757d")}.
#'   \item \code{info}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#17a2b8")}.
#'   \item \code{success}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#28a745")}.
#'   \item \code{warning}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#ffc107")}.
#'   \item \code{danger}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#dc3545")}.
#' }
#' @param gradient Whether to apply gradient to color. Default to FALSE.
#' @param outline Whether to display an outline style. Status must not be NULL if TRUE. Default to
#' FALSE.
#' @param size Button size. Default to NULL. Possible choices: \code{c("lg", "sm", "xs")}.
#' @param flat Whether to apply a flat style. Default to FALSE.
#' 
#' @note One may also pass the status directly via the ... parameter using \code{class = "btn-primary"},
#' for the primary status for instance. Same thing for other styles like the size.
#'
#' @family input elements
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'  library(shiny)
#'  library(bs4Dash)
#'
#'  shinyApp(
#'   ui = dashboardPage(
#'     header = dashboardHeader(
#'       title = bs4DashBrand(
#'         title = "My dashboard",
#'         color = "primary",
#'         src = "https://adminlte.io/themes/v3",
#'         image = "https://adminlte.io/themes/v3/dist/img/AdminLTELogo.png"
#'       )
#'     ),
#'     sidebar = dashboardSidebar(),
#'     body = dashboardBody(
#'       sliderInput("obs", "Number of observations", 0, 1000, 500),
#'       actionButton(
#'        "goButton", "Go!", 
#'        status = "danger", 
#'        outline = TRUE, 
#'        flat = TRUE, 
#'        size = "lg"
#'       ),
#'      plotOutput("distPlot")
#'     ),
#'     controlbar = dashboardControlbar(),
#'     title = "DashboardPage"
#'   ),
#'   server = function(input, output) {
#'    output$distPlot <- renderPlot({
#'     # Take a dependency on input$goButton. This will run once initially,
#'     # because the value changes from NULL to 0.
#'     input$goButton
#'
#'     # Use isolate() to avoid dependency on input$obs
#'     dist <- isolate(rnorm(input$obs))
#'     hist(dist)
#'    })
#'   }
#'  )
#'
#' }
#'
#' ## Example of adding extra class values
#' actionButton("largeButton", "Large Primary Button", class = "btn-primary btn-lg")
#'
#' @seealso [observeEvent()] and [eventReactive()]
#'
#' @section Server value:
#' An integer of class `"shinyActionButtonValue"`. This class differs from
#' ordinary integers in that a value of 0 is considered "falsy".
#' This implies two things:
#'   * Event handlers (e.g., [observeEvent()], [eventReactive()]) won't execute on initial load.
#'   * Input validation (e.g., [req()], [need()]) will fail on initial load.
#' @export
actionButton <- function(inputId, label, icon = NULL, width = NULL, ...,
                         status = NULL, gradient = FALSE, outline = FALSE, size = NULL,
                         flat = FALSE) {
  if (is.null(status) & outline) stop("outline cannot be used when color is NULL.")
  if (gradient && outline) stop("outline is not compatible with gradient.")
  
  if (!is.null(status)) validateStatus(status)
  value <- shiny::restoreInput(id = inputId, default = NULL)
  
  btnCl <- if (is.null(status)) {
    "btn btn-default action-button"
  } else {
    "btn action-button"
  }
  if (!is.null(status)) {
    if (outline) {
      btnCl <- paste0(btnCl, " btn-outline-", status)
    } else {
      if (gradient) {
        btnCl <- paste0(btnCl, " btn-gradient-", status)
      } else {
        btnCl <- paste0(btnCl, " btn-", status)
      }
    }
  }
  
  if (flat) btnCl <- paste0(btnCl, " btn-flat")
  if (!is.null(size)) btnCl <- paste0(btnCl, " btn-", size)
  
  shiny::tags$button(
    id = inputId,
    style = if (!is.null(width)) paste0("width: ", shiny::validateCssUnit(width), ";"),
    type = "button",
    class = btnCl,
    `data-val` = value,
    list(validateIcon(icon), label),
    ...
  )
}





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
#'   \item \code{white}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#fff")}.
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
#'         color = "orange",
#'         dashboardBadge(textOutput("btnVal"), color = "primary")
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
    shiny::actionButton(inputId, label, icon = icon, width = width, ...),
    class = "btn-app",
    class = if (!is.null(color)) paste0("bg-", color)
  )
}
