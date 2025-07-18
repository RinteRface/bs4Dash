#' Boostrap 4 dashboard navbar
#'
#' \link{dashboardHeader} creates an adminLTE3 dashboard navbar to be included in
#' \link{dashboardPage}.
#'
#' @rdname dashboardHeader
#'
#' @param ... Any UI element between left and right Ui. Can include \link{navbarMenu} to host
#' the navigation in the navbar.
#' @param title Dashboard title (displayed top-left side). Alternatively, use \link{dashboardBrand}
#' for more evolved title.
#' @param titleWidth This argument is deprecated; bs4Dash (AdminLTE3) title width
#' is tightly related to the sidebar width, contrary to shinydashboard (AdminLTE2).
#' @param disable If `TRUE`, don't display the header bar.
#' @param .list An optional list containing items to put in the header. Same as
#'   the `...` arguments, but in list format. This can be useful when
#'   working with programmatically generated items.
#' @param leftUi Custom left Ui content. Any element like \link{dropdownMenu}.
#' @param rightUi Custom right Ui content. Any element like \link{dropdownMenu}.
#' @param skin Navbar skin. "dark" or "light".
#' @param status Navbar status. Valid statuses are defined as follows:
#' \itemize{
#'   \item `primary`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#007bff")}.
#'   \item `secondary`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#6c757d")}.
#'   \item `info`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#17a2b8")}.
#'   \item `success`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#28a745")}.
#'   \item `warning`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#ffc107")}.
#'   \item `danger`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#dc3545")}.
#'   \item `gray-dark`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#343a40")}.
#'   \item `gray`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#adb5bd")}.
#'   \item `white`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#fff")}.
#'   \item `indigo`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#6610f2")}.
#'   \item `lightblue`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#3c8dbc")}.
#'   \item `navy`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#001f3f")}.
#'   \item `purple`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#605ca8")}.
#'   \item `fuchsia`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#f012be")}.
#'   \item `pink`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#e83e8c")}.
#'   \item `maroon`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#d81b60")}.
#'   \item `orange`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#ff851b")}.
#'   \item `lime`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#01ff70")}.
#'   \item `teal`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#39cccc")}.
#'   \item `olive`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#3d9970")}.
#' }
#' @param border Whether to separate the navbar and body by a border. TRUE by default.
#' @param compact Whether items should be compacted. FALSE by default.
#' @param sidebarIcon Icon of the main sidebar toggle.
#' @param controlbarIcon Icon to toggle the controlbar (left).
#' @param fixed Whether to fix the navbar to the top. FALSE by default.
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
bs4DashNavbar <- function(
  ...,
  title = NULL,
  titleWidth = NULL,
  disable = FALSE,
  .list = NULL,
  leftUi = NULL,
  rightUi = NULL,
  skin = "light",
  status = "white",
  border = TRUE,
  compact = FALSE,
  sidebarIcon = shiny::icon("bars"),
  controlbarIcon = shiny::icon("table-cells"),
  fixed = FALSE
) {
  items <- c(list(...), .list)

  # make sure default status for dark skin is gray-dark if status is not specified
  # by the end user.
  if (skin == "dark" && is.null(status)) {
    status <- "gray-dark"
  }

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
      })
    } else {
      tagAssert(rightUi, type = "li", class = "dropdown")
    }
  }

  headerTag <- shiny::tags$nav(
    style = if (disable) "display: none;",
    `data-fixed` = tolower(fixed),
    class = paste0(
      "main-header navbar navbar-expand",
      if (!is.null(status)) paste0(" navbar-", status),
      " navbar-",
      skin,
      if (!border) " border-bottom-0" else NULL,
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
      class = "navbar-nav ml-auto navbar-right",
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

#' Navbar tab item
#'
#' Similar to \link{menuItem} but for the
#' \link{dashboardHeader}.
#'
#' @param text Tab text.
#' @param ... Slot for nested \link{navbarTab}. You can nest as many elements
#' as you want.
#' @param tabName Should correspond exactly to the tabName given in \link{tabItem}.
#' @param icon An icon tag, created by shiny::icon. If NULL, don't display an icon.
#' @param .list Use this slot if you had to programmatically pass \link{navbarTab}
#' like with \link{lapply}.
#'
#' @note You can nest \link{navbarTab} so it does like
#' \link{menuSubItem}. This is to avoid to create too many functions.
#' @export
#' @rdname navbar-menu
navbarTab <- function(text, ..., tabName = NULL, icon = NULL, .list = NULL) {
  items <- c(list(...), .list)
  if (length(items) > 0) {
    do.call(navbarDropdown, list(text, items))
  } else {
    shiny::tags$li(
      class = "nav-item",
      shiny::tags$a(
        class = "nav-link",
        id = paste0("tab-", tabName),
        href = paste0("#shiny-tab-", tabName),
        `data-toggle` = "tab",
        `data-value` = tabName,
        icon,
        shiny::tags$p(text)
      )
    )
  }
}

#' Build navbar dropdown for navigation
#'
#' This is different from \link{dropdownMenu}.
#'
#' @param text Dropdown menu title.
#' @param ... Slot for nested items such as \link{navbarTab}.
#'
#' @keywords internal
navbarDropdown <- function(text, ...) {
  shiny::tags$li(
    class = "nav-item dropdown",
    shiny::tags$a(
      href = "#",
      `data-toggle` = "dropdown",
      `aria-haspopup` = "true",
      `aria-expanded` = "false",
      class = "nav-link dropdown-toggle",
      text
    ),
    shiny::tags$ul(
      class = "dropdown-menu border-0 shadow",
      style = "left: 0px; right: inherit;",
      ...
    )
  )
}

#' Dropdown header helper
#'
#' Display header text within dropdown menu
#'
#' @param text Text to display.
#'
#' @return A shiny tag.
#' @export
dropdownHeader <- function(text) {
  shiny::tags$h6(class = "dropdown-header", text)
}

#' Navbar menu
#'
#' Like \link{sidebarMenu} but inside \link{dashboardHeader}.
#'
#' @param ... Slot for \link{navbarTab}.
#' @param id Menu id. Useful to leverage \link{updateNavbarTabs} on the
#' server.
#' @rdname navbar-menu
#' @export
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(bs4Dash)
#'
#'  tabs <- tabItems(.list = lapply(1:7, function(i) {
#'   tabItem(tabName = sprintf("Tab%s", i), sprintf("Tab %s", i))
#'  }))
#'
#'  shinyApp(
#'    ui = dashboardPage(
#'      header = dashboardHeader(
#'        navbarMenu(
#'          id = "navmenu",
#'          navbarTab(tabName = "Tab1", text = "Tab 1"),
#'          navbarTab(tabName = "Tab2", text = "Tab 2"),
#'          navbarTab(
#'            text = "Menu",
#'            dropdownHeader("Dropdown header"),
#'            navbarTab(tabName = "Tab3", text = "Tab 3"),
#'            dropdownDivider(),
#'            navbarTab(
#'              text = "Sub menu",
#'              dropdownHeader("Another header"),
#'              navbarTab(tabName = "Tab4", text = "Tab 4"),
#'              dropdownHeader("Yet another header"),
#'              navbarTab(tabName = "Tab5", text = "Tab 5"),
#'              navbarTab(
#'                text = "Sub sub menu",
#'                navbarTab(tabName = "Tab6", text = "Tab 6"),
#'                navbarTab(tabName = "Tab7", text = "Tab 7")
#'              )
#'            )
#'          )
#'        )
#'      ),
#'      body = dashboardBody(tabs),
#'      controlbar = dashboardControlbar(
#'        sliderInput(
#'          inputId = "controller",
#'          label = "Update the first tabset",
#'          min = 1,
#'          max = 4,
#'          value = 1
#'        )
#'      ),
#'      sidebar = dashboardSidebar(disable = TRUE)
#'    ),
#'    server = function(input, output, session) {
#'      observeEvent(input$controller, {
#'        updateNavbarTabs(
#'          session,
#'          inputId = "navmenu",
#'          selected = paste0("Tab", input$controller)
#'        )
#'      },
#'      ignoreInit = TRUE
#'      )
#'    }
#'  )
#' }
navbarMenu <- function(..., id = NULL) {
  if (is.null(id)) {
    id <- paste0("tabs_", round(stats::runif(1, min = 0, max = 1e9)))
  }

  items <- list(...)
  items <- htmltools::tagQuery(items)$find(".nav-item.dropdown")$removeClass(
    "nav-item dropdown"
  )$addClass("dropdown-submenu dropdown-hover")$find("ul")$removeAttrs(
    "style"
  )$reset()$selectedTags()

  shiny::tags$ul(
    class = "navbar-nav sidebar-menu",
    role = "menu",
    items,
    shiny::div(
      id = id,
      class = "sidebarMenuSelectedTabItem",
      `data-value` = "null",
    )
  )
}

#' Update navbar menu from the server.
#'
#' @inheritParams updatebs4TabItems
#' @rdname navbar-menu
#' @export
updateNavbarTabs <- updatebs4TabItems

#' Alternative to simple text title
#'
#' @param title Brand title.
#' @param color Brand color. Valid colors are defined as follows:
#' \itemize{
#'   \item `primary`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#007bff")}.
#'   \item `secondary`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#6c757d")}.
#'   \item `info`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#17a2b8")}.
#'   \item `success`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#28a745")}.
#'   \item `warning`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#ffc107")}.
#'   \item `danger`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#dc3545")}.
#'   \item `gray-dark`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#343a40")}.
#'   \item `gray`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#adb5bd")}.
#'   \item `white`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#fff")}.
#'   \item `indigo`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#6610f2")}.
#'   \item `lightblue`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#3c8dbc")}.
#'   \item `navy`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#001f3f")}.
#'   \item `purple`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#605ca8")}.
#'   \item `fuchsia`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#f012be")}.
#'   \item `pink`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#e83e8c")}.
#'   \item `maroon`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#d81b60")}.
#'   \item `orange`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#ff851b")}.
#'   \item `lime`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#01ff70")}.
#'   \item `teal`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#39cccc")}.
#'   \item `olive`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#3d9970")}.
#' }
#' @param href External link to point to.
#' @param image External image location.
#' @param opacity Brand opacity: value between 0 and 1.
#'
#' @rdname dashboardBrand
#'
#' @return A title tag to be inserted in the title slot of \link{bs4DashNavbar}.
#' @export
bs4DashBrand <- function(
  title,
  color = NULL,
  href = NULL,
  image = NULL,
  opacity = .8
) {
  if (!is.null(color)) {
    validateStatusPlus(color)
  }

  shiny::tags$a(
    class = if (!is.null(color)) {
      paste0("brand-link bg-", color)
    } else {
      "brand-link"
    },
    href = if (!is.null(href)) href else "#",
    target = if (!is.null(href)) "_blank",
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
#'   \item `primary`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#007bff")}.
#'   \item `secondary`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#6c757d")}.
#'   \item `info`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#17a2b8")}.
#'   \item `success`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#28a745")}.
#'   \item `warning`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#ffc107")}.
#'   \item `danger`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#dc3545")}.
#' }.
#' A value of `NULL` means to not display a badge.
#' @param icon An icon to display in the header. By default, the icon is
#'   automatically selected depending on `type`, but it can be overriden
#'   with this argument.
#' @param headerText An optional text argument used for the header of the
#'   dropdown menu (this is only visible when the menu is expanded). If none is
#'   provided by the user, the default is "You have `x` messages," where
#'   `x` is the number of items in the menu (if the `type` is
#'   specified to be "notifications" or "tasks," the default text shows "You
#'   have `x` notifications" or  "You have `x` tasks," respectively).
#' @param .list An optional list containing items to put in the menu Same as the
#'   `...` arguments, but in list format. This can be useful when working
#'   with programmatically generated items.
#' @param href External link.
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   library(bs4Dash)
#'
#'   shinyApp(
#'     ui = dashboardPage(
#'       header = dashboardHeader(
#'         rightUi = dropdownMenu(
#'           badgeStatus = "danger",
#'           type = "messages",
#'           messageItem(
#'             inputId = "triggerAction1",
#'             message = "message 1",
#'             from = "Divad Nojnarg",
#'             image = "https://adminlte.io/themes/v3/dist/img/user3-128x128.jpg",
#'             time = "today",
#'             color = "lime"
#'           )
#'         ),
#'         leftUi = tagList(
#'           dropdownMenu(
#'             badgeStatus = "info",
#'             type = "notifications",
#'             notificationItem(
#'               inputId = "triggerAction2",
#'               text = "Error!",
#'               status = "danger"
#'             )
#'           ),
#'           dropdownMenu(
#'             badgeStatus = "info",
#'             type = "tasks",
#'             taskItem(
#'               inputId = "triggerAction3",
#'               text = "My progress",
#'               color = "orange",
#'               value = 10
#'             )
#'           )
#'         )
#'       ),
#'       sidebar = dashboardSidebar(),
#'       controlbar = dashboardControlbar(),
#'       footer = dashboardFooter(),
#'       title = "dropdownMenu",
#'       body = dashboardBody()
#'     ),
#'     server = function(input, output) {
#'       observeEvent(input$triggerAction1, {
#'         showModal(modalDialog(
#'           title = "Important message",
#'           "This is an important message!"
#'         ))
#'       })
#'     }
#'   )
#' }
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @rdname dropdownMenu
#'
#' @export
bs4DropdownMenu <- function(
  ...,
  type = c("messages", "notifications", "tasks"),
  badgeStatus = "primary",
  icon = NULL,
  headerText = NULL,
  .list = NULL,
  href = NULL
) {
  type <- match.arg(type)
  if (!is.null(badgeStatus)) {
    validateStatus(badgeStatus)
  }
  items <- c(list(...), .list)

  # Make sure the items are a tags
  # lapply(items, tagAssert, type = "a", class = "dropdown-item")

  if (is.null(icon)) {
    icon <- switch(
      type,
      messages = shiny::icon("comments"),
      notifications = shiny::icon("bell"),
      tasks = shiny::icon("list-check")
    )
  }

  numItems <- length(items)

  if (is.null(badgeStatus)) {
    badge <- NULL
  } else {
    badge <- shiny::span(
      class = paste0("badge badge-", badgeStatus, " navbar-badge"),
      numItems
    )
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
#'   \item `primary`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#007bff")}.
#'   \item `secondary`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#6c757d")}.
#'   \item `info`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#17a2b8")}.
#'   \item `success`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#28a745")}.
#'   \item `warning`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#ffc107")}.
#'   \item `danger`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#dc3545")}.
#'   \item `gray-dark`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#343a40")}.
#'   \item `gray`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#adb5bd")}.
#'   \item `white`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#fff")}.
#'   \item `indigo`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#6610f2")}.
#'   \item `lightblue`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#3c8dbc")}.
#'   \item `navy`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#001f3f")}.
#'   \item `purple`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#605ca8")}.
#'   \item `fuchsia`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#f012be")}.
#'   \item `pink`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#e83e8c")}.
#'   \item `maroon`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#d81b60")}.
#'   \item `orange`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#ff851b")}.
#'   \item `lime`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#01ff70")}.
#'   \item `teal`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#39cccc")}.
#'   \item `olive`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#3d9970")}.
#' }
#' If not NULL, Indicate the message priority.
#' @param inputId Whether to allow the item to act as a \link[shiny]{actionButton}.
#'
#' @rdname dropdownMenu
#'
#' @export
messageItem <- function(
  from,
  message,
  icon = shiny::icon("user"),
  time = NULL,
  href = NULL,
  image = NULL,
  color = "secondary",
  inputId = NULL
) {
  tagAssert(icon, type = "i")
  if (is.null(href)) {
    href <- "#"
  }
  if (!is.null(color)) {
    validateStatusPlus(color)
  }

  itemCl <- "dropdown-item"
  if (!is.null(inputId)) {
    itemCl <- paste0(itemCl, " action-button")
  }

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
#'   \item `primary`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#007bff")}.
#'   \item `secondary`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#6c757d")}.
#'   \item `info`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#17a2b8")}.
#'   \item `success`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#28a745")}.
#'   \item `warning`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#ffc107")}.
#'   \item `danger`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#dc3545")}.
#'   \item `gray-dark`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#343a40")}.
#'   \item `gray`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#adb5bd")}.
#'   \item `white`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#fff")}.
#'   \item `indigo`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#6610f2")}.
#'   \item `lightblue`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#3c8dbc")}.
#'   \item `navy`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#001f3f")}.
#'   \item `purple`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#605ca8")}.
#'   \item `fuchsia`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#f012be")}.
#'   \item `pink`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#e83e8c")}.
#'   \item `maroon`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#d81b60")}.
#'   \item `orange`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#ff851b")}.
#'   \item `lime`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#01ff70")}.
#'   \item `teal`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#39cccc")}.
#'   \item `olive`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#3d9970")}.
#' }
#' @param href An optional URL to link to. When inputId is set, href will be ignored.
#' @param inputId Whether to allow the item to act as a \link[shiny]{actionButton}.
#'
#' @rdname dropdownMenu
#'
#' @export
notificationItem <- function(
  text,
  icon = shiny::icon("triangle-exclamation"),
  status = "success",
  href = NULL,
  inputId = NULL
) {
  tagAssert(icon, type = "i")
  if (is.null(href)) {
    href <- "#"
  }
  if (!is.null(status)) {
    validateStatusPlus(status)
  }

  itemCl <- "dropdown-item"
  if (!is.null(inputId)) {
    itemCl <- paste0(itemCl, " action-button")
  }

  if (!is.null(status)) {
    icon <- shiny::tagAppendAttributes(icon, class = paste0("text-", status))
  }

  shiny::tagList(
    shiny::tags$a(
      class = itemCl,
      `disabled` = if (is.null(inputId)) NA else NULL,
      href = if (is.null(inputId)) {
        if (!is.null(href)) href else "#"
      } else {
        "#"
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
#'   \item `primary`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#007bff")}.
#'   \item `secondary`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#6c757d")}.
#'   \item `info`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#17a2b8")}.
#'   \item `success`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#28a745")}.
#'   \item `warning`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#ffc107")}.
#'   \item `danger`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#dc3545")}.
#'   \item `gray-dark`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#343a40")}.
#'   \item `gray`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#adb5bd")}.
#'   \item `white`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#fff")}.
#'   \item `indigo`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#6610f2")}.
#'   \item `lightblue`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#3c8dbc")}.
#'   \item `navy`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#001f3f")}.
#'   \item `purple`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#605ca8")}.
#'   \item `fuchsia`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#f012be")}.
#'   \item `pink`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#e83e8c")}.
#'   \item `maroon`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#d81b60")}.
#'   \item `orange`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#ff851b")}.
#'   \item `lime`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#01ff70")}.
#'   \item `teal`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#39cccc")}.
#'   \item `olive`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#3d9970")}.
#' }
#' @param href An optional URL to link to.
#' @param inputId Whether to allow the item to act as a \link[shiny]{actionButton}.
#'
#' @family menu items
#' @seealso \code{\link{dashboardHeader}} for example usage.
#'
#' @rdname dropdownMenu
#'
#' @export
taskItem <- function(
  text,
  value = 0,
  color = "info",
  href = NULL,
  inputId = NULL
) {
  validateStatusPlus(color)
  if (is.null(href)) {
    href <- "#"
  }

  itemCl <- "dropdown-item"
  if (!is.null(inputId)) {
    itemCl <- paste0(itemCl, " action-button")
  }

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
#' @rdname dashboardUser
#'
#' @param ... Body content. Slot for \link{dashboardUserItem}.
#' @param name User name.
#' @param image User profile picture.
#' @param title A title.
#' @param subtitle A subtitle.
#' @param footer Footer is any.
#' @param status Ribbon status. Valid colors are defined as follows:
#' \itemize{
#'   \item `primary`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#007bff")}.
#'   \item `secondary`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#6c757d")}.
#'   \item `info`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#17a2b8")}.
#'   \item `success`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#28a745")}.
#'   \item `warning`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#ffc107")}.
#'   \item `danger`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#dc3545")}.
#'   \item `gray-dark`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#343a40")}.
#'   \item `gray`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#adb5bd")}.
#'   \item `white`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#fff")}.
#'   \item `indigo`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#6610f2")}.
#'   \item `lightblue`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#3c8dbc")}.
#'   \item `navy`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#001f3f")}.
#'   \item `purple`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#605ca8")}.
#'   \item `fuchsia`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#f012be")}.
#'   \item `pink`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#e83e8c")}.
#'   \item `maroon`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#d81b60")}.
#'   \item `orange`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#ff851b")}.
#'   \item `lime`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#01ff70")}.
#'   \item `teal`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#39cccc")}.
#'   \item `olive`: \Sexpr[results=rd, stage=render]{bs4Dash:::rd_color_tag("#3d9970")}.
#' }
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   library(bs4Dash)
#'
#'   shinyApp(
#'     ui = dashboardPage(
#'       header = dashboardHeader(rightUi = userOutput("user")),
#'       sidebar = dashboardSidebar(),
#'       body = dashboardBody(),
#'       title = "DashboardPage"
#'     ),
#'     server = function(input, output) {
#'       output$user <- renderUser({
#'         dashboardUser(
#'           name = "Divad Nojnarg",
#'           image = "https://adminlte.io/themes/AdminLTE/dist/img/user2-160x160.jpg",
#'           title = "shinydashboardPlus",
#'           subtitle = "Author",
#'           footer = p("The footer", class = "text-center"),
#'           fluidRow(
#'             dashboardUserItem(
#'               width = 6,
#'               "Item 1"
#'             ),
#'             dashboardUserItem(
#'               width = 6,
#'               "Item 2"
#'             )
#'           )
#'         )
#'       })
#'     }
#'   )
#' }
#' @export
bs4UserMenu <- function(
  ...,
  name = NULL,
  image = NULL,
  title = NULL,
  subtitle = NULL,
  footer = NULL,
  status = NULL
) {
  if (!is.null(status)) {
    validateStatusPlus(status)
  }

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
    user_header_height <- shiny::tags$script(shiny::HTML(
      '$(".user-header").css("height", "145px")'
    ))
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
        alt = "User Image"
      ),
      shiny::tags$span(class = "d-none d-md-inline", name)
    ),
    shiny::tags$ul(
      class = "dropdown-menu dropdown-menu-lg dropdown-menu-right dashboard-user",
      shiny::tags$li(
        class = paste0(
          "user-header",
          if (!is.null(status)) paste0(" bg-", status)
        ),
        shiny::tags$img(
          src = image,
          class = "img-circle elevation-2",
          alt = "User Image"
        ),
        shiny::tags$p(title, shiny::tags$small(subtitle))
      ),
      if (length(list(...)) > 0) {
        shiny::tags$li(class = "user-body", shiny::fluidRow(...))
      },
      if (!is.null(footer)) shiny::tags$li(class = "user-footer", footer)
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
#' @rdname dashboardUser
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
#' @param tag A tag function, like `tags$li` or `tags$ul`.
#'
#' @seealso \code{\link{renderUser}} for the corresponding server side function
#'   and examples.
#' @family user outputs
#' @rdname dashboardUser
#' @export
userOutput <- function(id, tag = shiny::tags$li) {
  shiny::uiOutput(
    outputId = id,
    container = tag,
    class = "nav-item dropdown user-menu"
  )
}

#' Create dynamic user output (server side)
#'
#' @inheritParams shiny::renderUI
#'
#' @seealso \code{\link{userOutput}} for the corresponding client side function
#'   and examples.
#' @family user outputs
#' @rdname dashboardUser
#' @export
renderUser <- function(
  expr,
  env = parent.frame(),
  quoted = FALSE,
  outputArgs = list()
) {
  if (!quoted) {
    expr <- substitute(expr)
    quoted <- TRUE
  }
  shiny::renderUI(expr, env = env, quoted = quoted, outputArgs = outputArgs)
}

globalVariables("func")
