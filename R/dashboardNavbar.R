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
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
bs4DashNavbar <- function(..., skin = "light", status = "white", border = TRUE,
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
          shiny::tags$i(class = paste0("fas fa-", sidebarIcon))
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
          shiny::tags$i(class = paste0("fas fa-", controlbarIcon))
        )
      )
    )

  )
}




#' Create a Boostrap 4 dashboard dropdown menu
#'
#' Build an adminLTE3 dashboard dropdown menu
#'
#' @param ... Slot for bs4Notification or bs4Message.
#' @param show Whether to start with the dropdown open. FALSE by default.
#' @param status Dropdown menu status. "primary", "success", "warning", "danger" or "info".
#' @param labelText Dropdown label text.,
#' @param src Dropdown link to an external ressource.
#' 
#' @examples
#' if(interactive()){
#'  library(shiny)
#'  
#'  shiny::shinyApp(
#'    ui = bs4DashPage(
#'      navbar = bs4DashNavbar(
#'       rightUi = bs4DropdownMenu(
#'        show = TRUE,
#'        labelText = "!",
#'        status = "danger",
#'        src = "http://www.google.fr",
#'        bs4DropdownMenuItem(
#'         text = "message 1",
#'         date = "today"
#'        ),
#'        bs4DropdownMenuItem(
#'         text = "message 2",
#'         date = "yesterday"
#'        )
#'       )
#'      ),
#'      sidebar = bs4DashSidebar(),
#'      controlbar = bs4DashControlbar(),
#'      footer = bs4DashFooter(),
#'      title = "test",
#'      body = bs4DashBody()
#'    ),
#'    server = function(input, output) {}
#'  )
#' }
#' 
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
bs4DropdownMenu <- function(..., show = FALSE, labelText = NULL, src = NULL,
                            status = c("primary", "warning", "danger", "info", "success")) {
  
  status <- match.arg(status)
  items <- list(...)
  n_items <- length(items)
  # remove the divider from the last item
  #items[[n_items]][[2]] <- NULL
  
  shiny::tags$li(
    class = if (isTRUE(show)) "nav-item dropdown show" else "nav-item dropdown",
    shiny::tags$a(
      class = "nav-link",
      `data-toggle` = "dropdown",
      href = "#",
      shiny::tags$i(class = "fa fa-bell"),
      shiny::tags$span(
        class = paste0("badge badge-", status, " navbar-badge"), 
        labelText
      )
    ),
    shiny::tags$div(
      class = if (isTRUE(show)){
        "dropdown-menu dropdown-menu-lg dropdown-menu-right show"
      } else {
        "dropdown-menu dropdown-menu-lg dropdown-menu-right"
      },
      shiny::tags$span(
        class = "dropdown-item dropdown-header", 
        paste0(n_items, " Notifications")
      ),
      shiny::tags$div(class = "dropdown-divider"),
      ...,
      shiny::tags$a(
        class = "dropdown-item dropdown-footer",
        href = src,
        target = "_blank",
        "See more"
      )
    )
  )
}




#' Create a Boostrap 4 dashboard dropdown menu item
#'
#' Build an adminLTE3 dashboard dropdown menu item
#'
#' @param text Item content.
#' @param date Item date.
#' @param icon Item icon.
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
bs4DropdownMenuItem <- function(text, date = NULL, icon = "info-circle") {
  
  shiny::tagList(
    shiny::tags$a(
      class = "dropdown-item",
      href = "#",
      shiny::tags$i(
        class = paste0("fas fa-", icon, " mr-2")
      ),
      text,
      shiny::tags$span(
        class = "float-right text-muted text-sm",
        date,
        shiny::tags$span(
          class = "time",
          shiny::icon("clock-o")
        )
      )
    ),
    shiny::tags$div(class = "dropdown-divider")
  )
  
  # shiny::tags$a(
  #   class = "dropdown-item",
  #   href = src, 
  #   shiny::tags$div(
  #     class  = "media",
  #     shiny::tags$div(
  #       class = "media-body",
  #       shiny::tags$h3(
  #         class = "dropdown-item-title",
  #         shiny::tags$i(
  #           class = paste0("fa fa-", icon, " mr-2")
  #         ),
  #         shiny::tags$span(
  #           class = "float-right text-sm text-danger",
  #           bs4Stars(grade = 1, color = "warning")
  #         )
  #       ),
  #       shiny::tags$p(class = "text-sm", text),
  #       shiny::tags$p(
  #         class = "text-sm text-muted",
  #         shiny::tags$i(class = "fa fa-clock-o mr-1"),
  #         date
  #       )
  #     )
  #   )
  # )
  
}