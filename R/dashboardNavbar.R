#' Create a Boostrap 4 dashboard navbar
#'
#' Build an adminLTE3 dashboard navbar
#'
#' @param ... Any UI element between left and right Ui.
#' @param skin Navbar skin. "dark" or "light".
#' @param status Navbar status. "white" by default: "primary", "danger", "warning",
#' "success", "info", "white" or "gray-light".
#' @param border Whether to separate the navbar and body by a border. TRUE by default.
#' @param sidebarIcon Icon of the main sidebar toggle.
#' @param controlbarIcon Icon to toggle the controlbar (left).
#' @param leftUi Custom left Ui content. Any Ui element.
#' @param rightUi Custom right Ui content. Any Ui element.
#'
#' @examples
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
bs4DashboardNavbar <- function(..., skin = "dark", status = "white", border = TRUE,
                               sidebarIcon = "bars", controlbarIcon = "th-large",
                               leftUi = NULL, rightUi = NULL) {

  shiny::tags$nav(
    class = paste0("main-header navbar navbar-expand bg-", status,
                   " navbar-", skin, if (isTRUE(border)) " border-bottom" else NULL),

    # left sidebar elements
    shiny::tags$ul(
      class = "navbar-nav",

      # sidebar toggle (left)
      shiny::tags$li(
        class = "nav-item",
        shiny::tags$a(
          class = "nav-link",
          `data-widget` = "pushmenu",
          href = "#",
          shiny::tags$i(class = paste0("fa fa-", sidebarIcon))
        )
      ),
      leftUi
    ),

    # in between content
    ...,

    # right sidebar elements
    shiny::tags$ul(
      class = "navbar-nav ml-auto",
      rightUi,

      # controlbar toggle
      shiny::tags$li(
        class = "nav-item",
        shiny::tags$a(
          class = "nav-link",
          `data-widget` = "control-sidebar",
          `data-slide` = "true",
          href = "#",
          shiny::tags$i(class = paste0("fa fa-", controlbarIcon))
        )
      )
    )

  )
}
