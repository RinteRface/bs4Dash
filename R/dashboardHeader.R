#' Boostrap 4 dashboard navbar
#'
#' \link{dashboardHeader} creates an adminLTE3 dashboard navbar to be included in
#' \link{dashboardPage}.
#'
#' @param ... Any UI element between left and right Ui.
#' @param title Dashboard title (displayed top-left side). Alternatively, use \link{bs4DashBrand}
#' for more evolved title.
#' @param titleWidth The width of the title area. This must either be a number
#'   which specifies the width in pixels, or a string that specifies the width
#'   in CSS units.
#' @param disable If \code{TRUE}, don't display the header bar.
#' @param .list An optional list containing items to put in the header. Same as
#'   the \code{...} arguments, but in list format. This can be useful when
#'   working with programmatically generated items.
#' @param leftUi Custom left Ui content. Any element like \link{dropdownMenu}.
#' @param rightUi Custom right Ui content. Any element like \link{dropdownMenu}.
#' @param skin Navbar skin. "dark" or "light".
#' @param status Navbar status. "primary", "danger", "warning",
#' "success", "info", "white", "gray-light" and all other available colors. See
#' \link{getAdminLTEColors}.
#' @param border Whether to separate the navbar and body by a border. TRUE by default.
#' @param compact Whether items should be compacted. FALSE by default.
#' @param sidebarIcon Icon of the main sidebar toggle.
#' @param controlbarIcon Icon to toggle the controlbar (left).
#' @param fixed Whether to fix the navbar to the top. FALSE by default.
#' @param fullscreen Whether to allow fullscreen feature in the navbar. Default to FALSE.
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
bs4DashNavbar <- function(..., title = NULL, titleWidth = NULL, disable = FALSE, 
                          .list = NULL, leftUi = NULL, rightUi = NULL, skin = "light", status = NULL, 
                          border = TRUE, compact = FALSE, sidebarIcon = shiny::icon("bars"),
                          controlbarIcon = shiny::icon("th"), fixed = FALSE, fullscreen = FALSE) {
  
  items <- c(list(...), .list)
  
  if (!is.null(leftUi)) {
    if (inherits(leftUi, "shiny.tag.list")) {
      lapply(leftUi, function(item) {
        tagAssert(item, type = "li", class = "dropdown")
      })
    } else {
      tagAssert(leftUi, type = "li", class = "dropdown")
    }
  }
  
  if (!is.null(rightUi)) {
    if (inherits(rightUi, "shiny.tag.list")) {
      lapply(rightUi, function(item) {
        tagAssert(item, type = "li", class = "dropdown")
        # add dropdown-menu-right class to correctly open the dropdown
        item$children[[2]]$attribs$class <- paste0(
          item$children[[2]]$attribs$class,
          " dropdown-menu-right"
        )
      })
    } else {
      tagAssert(rightUi, type = "li", class = "dropdown")
      # add dropdown-menu-right class to correctly open the dropdown
      rightUi$children[[2]]$attribs$class <- paste0(
        rightUi$children[[2]]$attribs$class,
        " dropdown-menu-right"
      )
    } 
  }
  
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
    items,
    
    # right sidebar elements
    shiny::tags$ul(
      class = "navbar-nav ml-auto",
      rightUi,
      # fullscreen widget
      if (fullscreen) {
        shiny::tags$li(
          class = "nav-item",
          shiny::tags$a(
            class = "nav-link",
            `data-widget` = "fullscreen",
            href = "#",
            role = "button",
            shiny::tags$i(class = "fas fa-expand-arrows-alt")
          )
        )
      },
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




#' Boostrap 4 dashboard dropdown menu
#'
#' \link{dropdownMenu} creates an adminLTE3 dashboard dropdown menu, to be inserted in
#' a \link{dashboardHeader}.
#'
#' @param ... Items to put in the menu. Typically, message menus should contain
#'   \code{\link{messageItem}}s, notification menus should contain
#'   \code{\link{notificationItem}}s, and task menus should contain
#'   \code{\link{taskItem}}s.
#' @param type The type of menu. Should be one of "messages", "notifications",
#'   "tasks".
#' @param badgeStatus The status of the badge which displays the number of items
#'   in the menu. This determines the badge's color. Valid statuses are defined as follows:
#' \itemize{
#'   \item \code{primary}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#007bff")}.
#'   \item \code{secondary}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#6c757d")}.
#'   \item \code{info}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#17a2b8")}.
#'   \item \code{success}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#28a745")}.
#'   \item \code{warning}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#ffc107")}.
#'   \item \code{danger}: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#dc3545")}.
#' }. 
#' A value of \code{NULL} means to not display a badge.
#' @param icon An icon to display in the header. By default, the icon is
#'   automatically selected depending on \code{type}, but it can be overriden
#'   with this argument.
#' @param headerText An optional text argument used for the header of the
#'   dropdown menu (this is only visible when the menu is expanded). If none is
#'   provided by the user, the default is "You have \code{x} messages," where
#'   \code{x} is the number of items in the menu (if the \code{type} is
#'   specified to be "notifications" or "tasks," the default text shows "You
#'   have \code{x} notifications" or  "You have \code{x} tasks," respectively).
#' @param .list An optional list containing items to put in the menu Same as the
#'   \code{...} arguments, but in list format. This can be useful when working
#'   with programmatically generated items.
#' @param href External link.
#' 
#' @examples
#' if(interactive()){
#'  library(shiny)
#'  library(bs4Dash)
#'  
#'  shinyApp(
#'    ui = dashboardPage(
#'      header = dashboardHeader(
#'       rightUi = dropdownMenu(
#'        badgeStatus = "danger",
#'        type = "messages",
#'        messageItem(
#'          inputId = "triggerAction1",
#'          message = "message 1",
#'          from = "Divad Nojnarg",
#'          image = "https://adminlte.io/themes/v3/dist/img/user3-128x128.jpg",
#'          time = "today",
#'          color = "lime"
#'        )
#'       ),
#'       leftUi = tagList(
#'        dropdownMenu(
#'         badgeStatus = "info",
#'         type = "notifications",
#'         notificationItem(
#'           inputId = "triggerAction2",
#'           text = "Error!",
#'           status = "danger"
#'         )
#'        ),
#'        dropdownMenu(
#'         badgeStatus = "info",
#'         type = "tasks",
#'         taskItem(
#'           inputId = "triggerAction3",
#'           text = "My progress",
#'           color = "orange",
#'           value = 10
#'         )
#'        )
#'       )
#'      ),
#'      sidebar = dashboardSidebar(),
#'      controlbar = dashboardControlbar(),
#'      footer = dashboardFooter(),
#'      title = "dropdownMenu",
#'      body = dashboardBody()
#'    ),
#'    server = function(input, output) {
#'     observeEvent(input$triggerAction1, {
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
bs4DropdownMenu <- function(..., type = c("messages", "notifications", "tasks"),
                            badgeStatus = "primary", icon = NULL, headerText = NULL, 
                            .list = NULL, href = NULL) {
  
  type <- match.arg(type)
  if (!is.null(badgeStatus)) validateStatus(badgeStatus)
  items <- c(list(...), .list)
  
  # Make sure the items are a tags
  #lapply(items, tagAssert, type = "a", class = "dropdown-item")
  
  if (is.null(icon)) {
    icon <- switch(
      type,
      messages = shiny::icon("comments"),
      notifications = shiny::icon("bell"),
      tasks = shiny::icon("tasks")
    )
  }
  
  numItems <- length(items)
  
  if (is.null(badgeStatus)) {
    badge <- NULL
  } else {
    badge <- shiny::span(class = paste0("badge badge-", badgeStatus, " navbar-badge"), numItems)
  }
  
  if (is.null(headerText)) {
    headerText <- paste("You have", numItems, type)
  }
  
  shiny::tags$li(
    class = "nav-item dropdown",
    shiny::tags$a(
      class = "nav-link",
      `data-toggle` = "dropdown",
      href = "#",
      `aria-expanded` = "false",
      icon,
      badge
    ),
    shiny::tags$div(
      class = sprintf("dropdown-menu dropdown-menu-lg"),
      shiny::tags$span(
        class = "dropdown-item dropdown-header", 
        headerText
      ),
      shiny::tags$div(class = "dropdown-divider"),
      items,
      if (!is.null(href)) {
        shiny::tags$a(
          class = "dropdown-item dropdown-footer",
          href = href,
          target = "_blank",
          "More"
        ) 
      }
    )
  )
}




#' Bootstrap 4 message item
#'
#' \link{messageItem} creates a message item to place in a \link{dropdownMenu}.
#'
#' @param from Who the message is from.
#' @param message Text of the message.
#' @param icon An icon tag, created by \code{\link[shiny]{icon}}.
#' @param time String representing the time the message was sent. Any string may
#'   be used. For example, it could be a relative date/time like "5 minutes",
#'   "today", or "12:30pm yesterday", or an absolute time, like "2014-12-01 13:45".
#'   If NULL, no time will be displayed.
#' @param href An optional URL to link to.
#' @param image User image. 
#' @param color Item color. Valid colors are defined as follows:
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
#' If not NULL, Indicate the message priority.
#' @param inputId Whether to allow the item to act as a \link[shiny]{actionButton}.
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
messageItem <- function(from, message, icon = shiny::icon("user"), time = NULL, 
                        href = NULL, image = NULL, color = "secondary", inputId = NULL) {
  
  tagAssert(icon, type = "i")
  if (is.null(href)) href <- "#"
  if (!is.null(color)) validateStatusPlus(color)
  
  itemCl <- "dropdown-item"
  if (!is.null(inputId)) itemCl <- paste0(itemCl, " action-button")
  
  shiny::tagList(
    shiny::a(
      class = itemCl,
      id = inputId,
      href = if (is.null(inputId)) {
        "#"
      } else {
        href
      },
      target = if (is.null(inputId)) {
        if (!is.null(href)) "_blank"
      },
      shiny::div(
        class = "media",
        if (!is.null(image)) {
          shiny::img(
            src = image, 
            alt = "User Avatar", 
            class = "img-size-50 mr-3 img-circle"
          )
        },
        shiny::tags$div(
          class = "media-body",
          shiny::tags$h3(
            class = "dropdown-item-title",
            from,
            if (!is.null(icon)) {
              shiny::tags$span(
                class = paste0("float-right text-sm text-", color),
                icon
              )
            }
          ),
          shiny::tags$p(class = "text-sm", message),
          if (!is.null(time)) {
            shiny::tags$p(
              class = "text-sm text-muted",
              shiny::tags$i(class = "far fa-clock mr-1"),
              time
            )
          }
        )
      )
    ),
    shiny::tags$div(class = "dropdown-divider")
  )
}

#' Bootstrap 4 notification item
#'
#' \link{messageItem} creates a message item to place in a \link{dropdownMenu}.
#'
#' @param text The notification text.
#' @param icon An icon tag, created by \code{\link[shiny]{icon}}.
#' @param status The status of the item. This determines the item's background
#'   color. Valid statuses are defined as follows:
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
#' @param href An optional URL to link to.
#' @param inputId Whether to allow the item to act as a \link[shiny]{actionButton}.
#'
#' @export
notificationItem <- function(text, icon = shiny::icon("warning"), 
                             status = "success", href = NULL, inputId = NULL) {
  
  tagAssert(icon, type = "i")
  if (is.null(href)) href <- "#"
  if (!is.null(status)) validateStatusPlus(status)
  
  itemCl <- "dropdown-item"
  if (!is.null(inputId)) itemCl <- paste0(itemCl, " action-button")
  
  if (!is.null(status)) {
    icon <- shiny::tagAppendAttributes(icon, class = paste0("text-", status)) 
  }
  
  shiny::tagList(
    shiny::tags$a(
      class = itemCl,
      `disabled` = if (is.null(inputId)) NA else NULL,
      href = if (is.null(inputId)) {
        "#"
      } else {
        href
      },
      target = if (is.null(inputId)) {
        if (!is.null(href)) "_blank"
      },
      id = inputId,
      shiny::tagAppendAttributes(icon, class = "mr-2"),
      text
    ),
    shiny::tags$div(class = "dropdown-divider")
  )
}



#' Bootstrap 4 task item
#'
#' \link{taskItem} creates a task item to place in a \link{dropdownMenu}.
#'
#' @param text The task text.
#' @param value A percent value to use for the bar.
#' @param color A color for the bar. Valid colors are defined as follows:
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
#' @param href An optional URL to link to.
#' @param inputId Whether to allow the item to act as a \link[shiny]{actionButton}.
#'
#' @family menu items
#' @seealso \code{\link{dashboardHeader}} for example usage.
#' @export
taskItem <- function(text, value = 0, color = "info", href = NULL, inputId = NULL) {
  validateStatusPlus(color)
  if (is.null(href)) href <- "#"
  
  itemCl <- "dropdown-item"
  if (!is.null(inputId)) itemCl <- paste0(itemCl, " action-button")
  
  shiny::tagList(
    shiny::tags$a(
      class = itemCl,
      href = if (is.null(inputId)) {
        "#"
      } else {
        href
      },
      target = if (is.null(inputId)) {
        if (!is.null(href)) "_blank"
      },
      id = inputId,
      shiny::h5(
        shiny::tags$small(text),
        shiny::tags$small(class = "float-right", paste0(value, "%"))
      ),
      progressBar(
        value = value,
        animated = TRUE,
        striped = TRUE,
        size = "xs",
        status = color
      )
    ),
    shiny::tags$div(class = "dropdown-divider")
  )
}




#' Bootstrap 4 user profile.
#' 
#' \link{dashboardUser} to insert in the rightUi or leftUi slot of 
#' \link{dashboardHeader}.
#'
#' @param ... Body content. Slot for \link{dashboardUserItem}.
#' @param name User name.
#' @param image User profile picture.
#' @param title A title.
#' @param subtitle A subtitle.
#' @param footer Footer is any.
#' @param status Ribbon status. Valid colors are defined as follows:
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
  
  if (!is.null(status)) validateStatusPlus(status)
  
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
        class = paste0("user-header", if (!is.null(status)) paste0(" bg-", status)),
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