#' Create a Boostrap 4 dashboard navbar
#'
#' Build an adminLTE3 dashboard navbar
#'
#' @param ... Any UI element between left and right Ui.
#' @param title Dashboard title (displayed top-left side). Alternatively, use \link{bs4DashBrand}
#' for more evolved title.
#' @param titleWidth The width of the title area. This must either be a number
#'   which specifies the width in pixels, or a string that specifies the width
#'   in CSS units.
#' @param disable If \code{TRUE}, don't display the header bar.
#' @param leftUi Custom left Ui content. Any Ui element.
#' @param rightUi Custom right Ui content. Any Ui element.
#' @param skin Navbar skin. "dark" or "light".
#' @param status Navbar status. "primary", "danger", "warning",
#' "success", "info", "white", "gray-light" and all other available colors. See
#' \link{getAdminLTEColors}.
#' @param border Whether to separate the navbar and body by a border. TRUE by default.
#' @param compact Whether items should be compacted. FALSE by default.
#' @param sidebarIcon Icon of the main sidebar toggle.
#' @param controlbarIcon Icon to toggle the controlbar (left).
#' @param fixed Whether to fix the navbar to the top. FALSE by default
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
bs4DashNavbar <- function(..., title = NULL, titleWidth = NULL, disable = FALSE, 
                          leftUi = NULL, rightUi = NULL, skin = "light", status = NULL, 
                          border = TRUE, compact = FALSE, sidebarIcon = shiny::icon("bars"),
                          controlbarIcon = shiny::icon("th"), fixed = FALSE) {
  
  titleWidth <- shiny::validateCssUnit(titleWidth)
  
  # Set up custom CSS for custom width.
  custom_css <- NULL
  if (!is.null(titleWidth)) {
    # This CSS is derived from the header-related instances of '230px' (the
    # default sidebar width) from inst/AdminLTE/AdminLTE.css. One change is that
    # instead making changes to the global settings, we've put them in a media
    # query (min-width: 768px), so that it won't override other media queries
    # (like max-width: 767px) that work for narrower screens.
    custom_css <- shiny::tags$head(
      shiny::tags$style(
        shiny::HTML(
          gsub(
            "_WIDTH_", 
            titleWidth, 
            fixed = TRUE, 
            '@media (min-width: 768px) {
              .main-header.navbar {
                margin-left: _WIDTH_;
              }
              .main-header .brand-link {
                width: _WIDTH_;
              }
            }
           '
          )
        )
      )
    )
  }
  
  headerTag <- shiny::tags$nav(
    custom_css, 
    style = if (disable) "display: none;",
    `data-fixed` = tolower(fixed),
    class = paste0(
      "main-header navbar navbar-expand", if (!is.null(status)) paste0(" navbar-", status),
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
          sidebarIcon
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
          controlbarIcon
        )
      ) 
    )
  )
  
  list(headerTag, title)
}





#' Alternative to simple text title 
#'
#' @param title Brand title. 
#' @param color Brand color.
#' @param src External link to point to.
#' @param image External image location.
#' @param opacity Brand opacity: value between 0 and 1.
#'
#' @return A title tag to be inserted in the title slot of \link{bs4DashNavbar}.
#' @export
bs4DashBrand <- function(title, color = NULL, src = NULL, image = NULL, opacity = .8) {
  shiny::tags$a(
    class = if (!is.null(color)) paste0("brand-link bg-", color) else "brand-link",
    href = if (!is.null(src)) src else "#",
    if (!is.null(image)) {
      shiny::tags$img(
        src = image,
        class = "brand-image img-circle elevation-3",
        style = paste0("opacity: ", opacity)
      )
    },
    shiny::tags$span(class = "brand-text font-weight-light", title)
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
#' Insert in the rightUi or leftUi slot of \link{dashboardHeader}.
#'
#' @param ... Body content. Slot for \link{dashboardUserItem}.
#' @param name User name.
#' @param image User profile picture.
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
#'     header = dashboardHeader(rightUi = userOutput("user")),
#'     sidebar = dashboardSidebar(),
#'     body = dashboardBody(),
#'     title = "DashboardPage"
#'   ),
#'   server = function(input, output) {
#'    output$user <- renderUser({
#'     dashboardUser(
#'        name = "Divad Nojnarg", 
#'        image = "https://adminlte.io/themes/AdminLTE/dist/img/user2-160x160.jpg", 
#'        title = "shinydashboardPlus",
#'        subtitle = "Author", 
#'        footer = p("The footer", class = "text-center"),
#'        fluidRow(
#'         dashboardUserItem(
#'          width = 6,
#'          "Item 1"
#'         ),
#'         dashboardUserItem(
#'          width = 6,
#'          "Item 2"
#'         )
#'        )
#'       )
#'    })
#'   }
#'  )
#' }
#' 
#' @export
bs4UserMenu <- function(..., name = NULL, image = NULL, title = NULL,
                        subtitle = NULL, footer = NULL, status = NULL) {
  
  # Create line 1 for menu
  if (!is.null(title)) {
    line_1 <- paste0(name, " - ", title)
  } else {
    line_1 <- name
  }
  
  # Create user_text based on if subtitle exists
  # If subtitle doesn't exist, make the menu height smaller
  if (!is.null(subtitle)) {
    user_text <- shiny::tags$p(line_1, shiny::tags$small(subtitle))
    user_header_height <- NULL
  } else {
    user_text <- shiny::tags$p(line_1)
    user_header_height <- shiny::tags$script(shiny::HTML('$(".user-header").css("height", "145px")'));
  }
  
  shiny::tagList(
    shiny::tags$head(
      shiny::tags$script(
        "$(function() {
          $('.dashboard-user').on('click', function(e){
            e.stopPropagation();
          });
        });
        "
      )
    ), 
    shiny::tags$a(
      href = "#",
      class = "nav-link dropdown-toggle",
      `data-toggle` = "dropdown",
      `aria-expanded` = "false",
      shiny::tags$img(
        src = image,
        class = "user-image img-circle elevation-2",
        alt = "User Image"),
      shiny::tags$span(class = "d-none d-md-inline", name)
    ),
    shiny::tags$ul(
      class = "dropdown-menu dropdown-menu-lg dropdown-menu-right dashboard-user",
      shiny::tags$li(
        class = paste0("user-header bg-", status),
        shiny::tags$img(
          src = image,
          class = "img-circle elevation-2",
          alt = "User Image"
        ),
        shiny::tags$p(title, shiny::tags$small(subtitle))),
      if(length(list(...)) > 0) shiny::tags$li(class = "user-body", shiny::fluidRow(...)),
      if(!is.null(footer)) shiny::tags$li(class = "user-footer", footer)
    )
  )
}




#' Create a dashboard user profile item 
#'
#' This can be inserted in a \code{\link{dashboardUser}}.
#'
#' @param item HTML Tag.
#' @param width Item width between 1 and 12.
#'
#' @export
dashboardUserItem <- function(item, width) {
  item <- shiny::div(
    class = paste0("col-", width, " text-center"),
    item
  ) 
}




#' Create a dynamic user output (client side)
#'
#' This can be used as a placeholder for dynamically-generated \code{\link{dashboardUser}}.
#'
#' @param id Output variable name.
#' @param tag A tag function, like \code{tags$li} or \code{tags$ul}.
#'
#' @seealso \code{\link{renderUser}} for the corresponding server side function
#'   and examples.
#' @family user outputs
#' @export
userOutput <- function(id, tag = shiny::tags$li) {
  shiny::uiOutput(outputId = id, container = tag, class = "nav-item dropdown user-menu")
}

#' Create dynamic user output (server side)
#'
#' @inheritParams shiny::renderUI
#'
#' @seealso \code{\link{userOutput}} for the corresponding client side function
#'   and examples.
#' @family user outputs
#' @export
renderUser <- shiny::renderUI

globalVariables("func")