#' Create a Boostrap 4 dashboard page
#'
#' Build an adminLTE3 dashboard page
#'
#' @param header Slot for \link{bs4DashNavbar}.
#' @param sidebar Slot for \link{bs4DashSidebar}.
#' @param body Slot for \link{bs4DashBody}.
#' @param controlbar Slot for \link{bs4DashControlbar} (right side).
#' @param footer Slot for \link{bs4DashFooter}.
#' @param title App title.
#' @param freshTheme A skin powered by the fresh package. Not compatible with skin.
#' See \url{https://dreamrs.github.io/fresh/articles/vars-shinydashboard.html}.
#' @param preloader bs4Dash uses waiter (see \url{https://waiter.john-coene.com/#/}).
#' Pass a nested list with 2 elements like \code{list(waiter = list(html = spin_1(), color = "#333e48"), duration = 5)}.
#' \code{waiter} expects to provide a sub-list to configure \link[waiter]{waiter_show_on_load} (refer to
#' the package help for all styles). \code{duration} defines the loader timeout.
#' @param options Extra option to overwrite the vanilla AdminLTE configuration. See 
#' \url{https://adminlte.io/themes/AdminLTE/documentation/index.html#adminlte-options}.
#' Expect a list.
#' \url{https://www.w3schools.com/cssref/css_colors.asp}.
#'
#' @examples
#' if(interactive()){
#'  library(shiny)
#'  library(bs4Dash)
#'  library(fresh)
#'
#'  shinyApp(
#'    ui = dashboardPage(
#'      freshTheme = create_theme(
#'       bs4dash_vars(
#'         navbar_light_color = "#bec5cb",
#'         navbar_light_active_color = "#FFF",
#'         navbar_light_hover_color = "#FFF"
#'       ),
#'       bs4dash_yiq(
#'         contrasted_threshold = 10,
#'         text_dark = "#FFF", 
#'         text_light = "#272c30"
#'       ),
#'       bs4dash_layout(
#'         main_bg = "#353c42"
#'       ),
#'       bs4dash_sidebar_light(
#'         bg = "#272c30", 
#'         color = "#bec5cb",
#'         hover_color = "#FFF",
#'         submenu_bg = "#272c30", 
#'         submenu_color = "#FFF", 
#'         submenu_hover_color = "#FFF"
#'       ),
#'       bs4dash_status(
#'         primary = "#5E81AC", danger = "#BF616A", light = "#272c30"
#'       ),
#'       bs4dash_color(
#'         gray_900 = "#FFF", white = "#272c30"
#'       )
#'      ),
#'      options = NULL,
#'      header = dashboardHeader(),
#'      sidebar = dashboardSidebar(),
#'      body = dashboardBody(
#'       box(status = "danger"),
#'       box(status = "primary"),
#'       box(status = "orange")
#'      ),
#'      controlbar = dashboardControlbar(),
#'      title = "DashboardPage"
#'    ),
#'    server = function(input, output) { }
#'  )
#' }
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
bs4DashPage <- function(header, sidebar, body, controlbar = NULL, footer = NULL, title = NULL,
                        freshTheme = NULL, preloader = NULL, options = NULL){
  
  
  # some checks
  tagAssert(header, type = "nav", class = "main-header")
  tagAssert(sidebar, type = "aside", class = "main-sidebar")
  tagAssert(body, type = "div", class = "content-wrapper")
  if (!is.null(controlbar)) {
    tagAssert(controlbar[[2]], type = "aside", class = "control-sidebar")
  }
  if (!is.null(footer)) {
    tagAssert(footer, type = "footer", class = "main-footer")
  } 
  
  # create the body content
  bodyContent <- shiny::tags$div(
    class = "wrapper",
    header,
    sidebar,
    # page content
    body,
    controlbar,
    if (!is.null(footer)) footer,
    if (!is.null(freshTheme)) {
      fresh::use_theme(freshTheme)
    }
  )
  
  # page wrapper
  shiny::tagList(
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
    add_bs4Dash_deps(
      shiny::tags$body(
        if (!is.null(preloader)) {
          shiny::tagList(
            waiter::use_waiter(), # dependencies
            do.call(waiter::waiter_show_on_load, preloader$waiter)
          )
        },
        onload = if (!is.null(preloader)) {
          sprintf(
            "setTimeout(function(){
            window.loading_screen.finish();
          }, %s);", preloader$duration * 1000
          )
        },
        bodyContent
      ), 
      options = options
    )
  )
}
