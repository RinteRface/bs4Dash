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
#' @param fixed Whether the navbar is fixed to the top. FALSE by default
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
bs4DashNavbar <- function(..., skin = "light", status = "white", border = TRUE,
                               sidebarIcon = "bars", controlbarIcon = "th",
                               leftUi = NULL, rightUi = NULL, fixed = FALSE) {

  navbarTag <- shiny::tags$nav(
    class = paste0(
      "main-header navbar navbar-expand bg-", status,
      " navbar-", skin, if (isTRUE(border)) " border-bottom" else NULL,
      if (fixed) " fixed-top" else NULL
    ),

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
          shiny::icon(sidebarIcon)
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
          id = "controlbar-toggle",
          class = "nav-link",
          `data-widget` = "control-sidebar",
          href = "#",
          shiny::icon(controlbarIcon)
        )
      ) 
    )
  )
  
  shiny::tagList(
    shiny::singleton(
      shiny::tags$head(
        shiny::tags$style(
          shiny::HTML(
            paste0(
              ".fa-", sidebarIcon, "{
                 color: #000;
              }
               .fa-", controlbarIcon, "{
                 color: #000;
               }
              "
            )
          )
        )
      )
    ),
    navbarTag
  )
  
}




#' Create a Boostrap 4 dashboard dropdown menu
#'
#' Build an adminLTE3 dashboard dropdown menu
#'
#' @param ... Slot for \link{bs4DropdownMenuItem}.
#' @param show Whether to start with the dropdown open. FALSE by default.
#' @param status Dropdown menu status. "primary", "success", "warning", "danger" or "info".
#' @param labelText Dropdown label text.
#' @param src Dropdown link to an external ressource.
#' @param menuIcon Fontawesome icon (default = "bell")
#' @param align Menu alignment (default = "right")
#' 
#' @examples
#' if(interactive()){
#'  library(shiny)
#'  library(bs4Dash)
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
                            status = c("primary", "warning", "danger", "info", "success"), 
                            menuIcon = "bell", align = "right") {
  
  status <- match.arg(status)
  items <- list(...)
  n_items <- length(items)
  # remove the divider from the last item
  #items[[n_items]][[2]] <- NULL
  
  labelText <- n_items
  
  dropdownMenuTag <- shiny::tags$li(
    class = if (isTRUE(show)) "nav-item dropdown show" else "nav-item dropdown",
    shiny::tags$a(
      class = "nav-link",
      `data-toggle` = "dropdown",
      href = "#",
      shiny::icon(menuIcon),
      shiny::tags$span(
        class = paste0("badge badge-", status, " navbar-badge"), 
        labelText
      )
    ),
    shiny::tags$div(
      class = if (isTRUE(show)) {
        sprintf("dropdown-menu dropdown-menu-lg dropdown-menu-%s show", align)
      } else {
        sprintf("dropdown-menu dropdown-menu-lg dropdown-menu-%s", align)
      },
      shiny::tags$span(
        class = "dropdown-item dropdown-header", 
        paste0(n_items, " Items")
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
  
  shiny::tagList(
    shiny::singleton(
      shiny::tags$head(
        shiny::tags$style(
          shiny::HTML(
            sprintf(
              ".fa-%s {
                color: #000;
               }
              "
            , menuIcon)
          )
        )
      )
    ),
    dropdownMenuTag
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
  
  dropdownItemTag <- shiny::tagList(
    shiny::tags$a(
      class = "dropdown-item",
      href = "#",
      shiny::icon(icon),
      text,
      shiny::tags$span(
        class = "float-right text-muted text-sm",
        date,
        shiny::tags$span(
          class = "time",
          shiny::icon(icon)
        )
      )
    ),
    shiny::tags$div(class = "dropdown-divider")
  )
  
  shiny::tagList(
    shiny::singleton(
      shiny::tags$head(
        shiny::tags$style(
          shiny::HTML(
            paste0(
              ".fa-", icon, "{
                color: #000;
              }
              "
            )
          )
        )
      )
    ),
    dropdownItemTag
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