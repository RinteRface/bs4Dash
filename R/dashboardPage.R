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
#' @param skin Deprecated skin parameters. See \link{skinSelector} for live theming.
#' @param freshTheme A skin powered by the fresh package. Not compatible with skin.
#' See \url{https://dreamrs.github.io/fresh/articles/vars-shinydashboard.html}.
#' @param preloader bs4Dash uses waiter (see \url{https://waiter.john-coene.com/#/}).
#' Pass a nested list with 2 elements like \code{list(waiter = list(html = spin_1(), color = "#333e48"), duration = 5)}.
#' \code{waiter} expects to provide a sub-list to configure \link[waiter]{waiter_show_on_load} (refer to
#' the package help for all styles). \code{duration} defines the loader timeout.
#' @param options Extra option to overwrite the vanilla AdminLTE configuration. See
#' \url{https://adminlte.io/themes/AdminLTE/documentation/index.html#adminlte-options}.
#' Expect a list.
#' @param fullscreen Whether to allow fullscreen feature in the navbar. Default to FALSE.
#' @param help Whether to enable/disable popovers and tooltips. This allows to seamlessly use
#' \link{tooltip} and \link{popover} without having to individually toggle them. Default to FALSE.
#' if TRUE, a help icon is display in the navigation bar.
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   library(bs4Dash)
#'   library(fresh)
#'
#'   shinyApp(
#'     ui = dashboardPage(
#'       freshTheme = create_theme(
#'         bs4dash_vars(
#'           navbar_light_color = "#bec5cb",
#'           navbar_light_active_color = "#FFF",
#'           navbar_light_hover_color = "#FFF"
#'         ),
#'         bs4dash_yiq(
#'           contrasted_threshold = 10,
#'           text_dark = "#FFF",
#'           text_light = "#272c30"
#'         ),
#'         bs4dash_layout(
#'           main_bg = "#353c42"
#'         ),
#'         bs4dash_sidebar_light(
#'           bg = "#272c30",
#'           color = "#bec5cb",
#'           hover_color = "#FFF",
#'           submenu_bg = "#272c30",
#'           submenu_color = "#FFF",
#'           submenu_hover_color = "#FFF"
#'         ),
#'         bs4dash_status(
#'           primary = "#5E81AC", danger = "#BF616A", light = "#272c30"
#'         ),
#'         bs4dash_color(
#'           gray_900 = "#FFF", white = "#272c30"
#'         )
#'       ),
#'       options = NULL,
#'       header = dashboardHeader(
#'         title = dashboardBrand(
#'           title = "My dashboard",
#'           color = "primary",
#'           href = "https://adminlte.io/themes/v3",
#'           image = "https://adminlte.io/themes/v3/dist/img/AdminLTELogo.png"
#'         )
#'       ),
#'       sidebar = dashboardSidebar(),
#'       body = dashboardBody(
#'         box(status = "danger"),
#'         box(status = "primary"),
#'         box(status = "orange")
#'       ),
#'       controlbar = dashboardControlbar(),
#'       title = "DashboardPage"
#'     ),
#'     server = function(input, output) { }
#'   )
#' }
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @rdname dashboardPage
#'
#' @export
bs4DashPage <- function(header, sidebar, body, controlbar = NULL, footer = NULL, title = NULL,
                        skin = NULL, freshTheme = NULL, preloader = NULL, options = NULL,
                        fullscreen = FALSE, help = FALSE) {
  titleTag <- header[[2]]

  sidebarDisabled <- sidebar$attribs$`data-disable`

  # layout changes if main sidebar is disabled
  if (!sidebarDisabled) {
    tagAssert(sidebar, type = "aside", class = "main-sidebar")
    # look for custom area and move it to third slot
    if (length(sidebar$children) > 1) {
      sidebar$children[[3]] <- sidebar$children[[2]]
    }
    # sidebar stuff are moved to second child
    sidebar$children[[2]] <- sidebar$children[[1]]

    # header content (brand logo) is moved to sidebar first child
    sidebar$children[[1]] <- if (!is.null(titleTag)) {
      if (inherits(titleTag, "shiny.tag")) {
        titleTag
      } else {
        shiny::div(class = "brand-link", titleTag)
      }
    }
  } else {
    header[[1]] <- tagInsertChild(header[[1]], header[[2]], 1)
    # remove navbar padding to better fit the brand logo
    header[[1]]$attribs$style <- "padding: 0rem 0rem;"
  }


  # some checks
  tagAssert(header[[1]], type = "nav", class = "main-header")
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
    header[[1]],
    if (!sidebarDisabled) sidebar,
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
        `data-help` = if (help) 1 else 0,
        `data-fullscreen` = if (fullscreen) 1 else 0,
        class = if (sidebarDisabled) "layout-top-nav",
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