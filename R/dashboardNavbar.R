#' Create a Boostrap 4 dashboard navbar
#'
#' Build an adminLTE3 dashboard navbar
#'
#' @param ... Any UI element between left and right Ui.
#' @param skin Navbar skin. "dark" or "light".
#' @param status Navbar status. "primary", "danger", "warning",
#' "success", "info", "white", "gray-light" and all other available colors. See
#' \link{getAdminLTEColors}.
#' @param border Whether to separate the navbar and body by a border. TRUE by default.
#' @param compact Whether items should be compacted. FALSE by default.
#' @param sidebarIcon Icon of the main sidebar toggle.
#' @param controlbarIcon Icon to toggle the controlbar (left).
#' @param leftUi Custom left Ui content. Any Ui element.
#' @param rightUi Custom right Ui content. Any Ui element.
#' @param fixed Whether to fix the navbar to the top. FALSE by default
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
bs4DashNavbar <- function(..., skin = "light", status = NULL, border = TRUE,
                          sidebarIcon = "bars", compact = FALSE, controlbarIcon = "th",
                          leftUi = NULL, rightUi = NULL, fixed = FALSE) {
  
  shiny::tags$nav(
    `data-fixed` = tolower(fixed),
    class = paste0(
      "main-header navbar navbar-expand navbar-", status,
      " navbar-", skin, if (isTRUE(border)) " border-bottom-0" else NULL,
      if (compact) " text-sm" else NULL
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
#'        show = FALSE,
#'        status = "danger",
#'        src = "https://www.google.fr",
#'        bs4DropdownMenuItem(
#'          inputId = "triggerAction",
#'          message = "message 1",
#'          from = "Divad Nojnarg",
#'          src = "https://adminlte.io/themes/v3/dist/img/user3-128x128.jpg",
#'          time = "today",
#'          status = "danger",
#'          type = "message"
#'        ),
#'        bs4DropdownMenuItem(
#'          message = "message 2",
#'          from = "Nono Gueye",
#'          src = "https://adminlte.io/themes/v3/dist/img/user3-128x128.jpg",
#'          time = "yesterday",
#'          status = "success",
#'          type = "message"
#'        )
#'       )
#'      ),
#'      sidebar = bs4DashSidebar(),
#'      controlbar = bs4DashControlbar(),
#'      footer = bs4DashFooter(),
#'      title = "test",
#'      body = bs4DashBody()
#'    ),
#'    server = function(input, output) {
#'     observeEvent(input$triggerAction, {
#'      showModal(modalDialog(
#'       title = "Important message",
#'       "This is an important message!"
#'      ))
#'     })
#'    }
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
  
  shiny::tags$li(
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
}




#' Create a Boostrap 4 dashboard dropdown menu item
#'
#' Build an adminLTE3 dashboard dropdown menu item
#'
#' @param inputId Whether to allow the item to act as a \link[shiny]{actionButton}.
#' @param message Item content.
#' @param from Item sender. Only if type is message.
#' @param time Item date.
#' @param icon Item icon.
#' @param src Item image. Only if type is message.
#' @param status Item status. Leave NULL if type is notification. See
#' \link{getAdminLTEColors} for valid statuses.
#' If not NULL, Indicate the message priority.
#' @param type Item type: notification or message.
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
bs4DropdownMenuItem <- function(inputId = NULL, message, from = NULL, time = NULL, icon = "info-circle", src = NULL,
                                status = NULL, type = c("notification", "message")) {
  
  type <- match.arg(type)
  
  itemCl <- if (is.null(inputId)) {
    "dropdown-item"
  } else {
    "dropdown-item action-button"
  }
  
  dropdownItemTag <- if (type == "notification") {
    shiny::tagList(
      shiny::tags$button(
        type = "button",
        class = itemCl,
        `disabled` = if (is.null(inputId)) NA else NULL,
        id = inputId,
        shiny::tagAppendAttributes(shiny::icon(icon), class = "mr-2"),
        message,
        shiny::tags$span(
          class = "float-right text-muted text-sm",
          time,
          shiny::tags$span(
            class = "time",
            shiny::icon(icon)
          )
        )
      ),
      shiny::tags$div(class = "dropdown-divider")
    )
  } else if (type == "message") {
    shiny::tags$button(
      type = "button",
      class = itemCl,
      `disabled` = if (is.null(inputId)) NA else NULL,
      id = inputId,
      shiny::tags$div(
        class = "media",
        shiny::tags$img(
          src = src,
          class = "img-size-50 mr-3 img-circle"
        ),
        shiny::tags$div(
          class = "media-body",
          shiny::tags$h3(
            class = "dropdown-item-title",
            from,
            shiny::tags$span(
              class = paste0("float-right text-sm text-", status),
              shiny::tags$i(class = "fas fa-star")
            )
          ),
          shiny::tags$p(class = "text-sm", message),
          shiny::tags$p(
            class = "text-sm text-muted",
            shiny::tags$i(class = "far fa-clock mr-1"),
            time
          )
        )
      )
    ) 
  }
  
  dropdownItemTag
  
}





#' Create a Bootstrap 4 user profile.
#'
#' @param ... Body content.
#' @param name User name.
#' @param src User profile picture.
#' @param title A title.
#' @param subtitle A subtitle.
#' @param footer Footer is any.
#' @param status Ribbon status: "primary", "danger", "success", "warning", "info" and 
#' "secondary".
#' 
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(bs4Dash)
#'  
#'  shinyApp(
#'   ui = dashboardPage(
#'     navbar = dashboardHeader(
#'       rightUi = bs4UserMenu(
#'        name = "Divad Nojnarg", 
#'        status = "primary",
#'        src = "https://adminlte.io/themes/AdminLTE/dist/img/user2-160x160.jpg", 
#'        title = "bs4Dash",
#'        subtitle = "Author", 
#'        footer = p("The footer", class = "text-center"),
#'        "This is the menu content."
#'       )
#'     ),
#'     sidebar = dashboardSidebar(),
#'     body = dashboardBody(),
#'     title = "bs4UserMenu"
#'   ),
#'   server = function(input, output) {}
#'  )
#' }
#' 
#' @export
bs4UserMenu <- function(..., name = NULL, src = NULL, title = NULL,
                        subtitle = NULL, footer = NULL,
                        status = c("primary", "danger", "success", "warning", "info", "secondary")) {
  
  status <- match.arg(status)
  
  shiny::tags$li(
    class = "nav-item dropdown user-menu",
    shiny::tags$a(
      href = "#",
      class = "nav-link dropdown-toggle",
      `data-toggle` = "dropdown",
      `aria-expanded` = "false",
      shiny::tags$img(
        src = src,
        class = "user-image img-circle elevation-2",
        alt = "User Image"),
      shiny::tags$span(class = "d-none d-md-inline", name)
    ),
    shiny::tags$ul(
      class = "dropdown-menu dropdown-menu-lg dropdown-menu-right",
      shiny::tags$li(
        class = paste0("user-header bg-", status),
        shiny::tags$img(
          src = src,
          class = "img-circle elevation-2",
          alt = "User Image"),
        shiny::tags$p(title, shiny::tags$small(subtitle))),
      shiny::tags$li(class = "user-body", ...),
      if(!is.null(footer)) shiny::tags$li(class = "user-footer", footer)
    )
  )
}