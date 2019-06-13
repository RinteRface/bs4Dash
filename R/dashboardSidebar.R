#' Create a Boostrap 4 dashboard main sidebar
#'
#' Build an adminLTE3 dashboard main sidebar
#'
#' @param ... Slot for \link{bs4SidebarMenu}.
#' @param title Sidebar title.
#' @param skin Sidebar skin. "dark" or "light"
#' @param status Sidebar status. "primary", "danger", "warning", "success", "info".
#' @param brandColor Brand color. NULL by default: "primary", "danger", "warning",
#' "success", "info", "white" or "gray-light".
#' @param url Sidebar brand link.
#' @param src Sidebar brand image.
#' @param elevation Sidebar elevation. 4 by default (until 5).
#' @param opacity Sidebar opacity. From 0 to 1. 0.8 by default.
# #' @param width Sidebar width. 250 px by default.
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
bs4DashSidebar <- function(..., title = NULL, skin = "dark", status = "primary",
                                brandColor = NULL, url = NULL, src = NULL,
                                elevation = 4, opacity = .8) {

  # brand logo
  brandTag <- if (!is.null(title)) {
    shiny::tags$a(
      class = if (!is.null(brandColor)) paste0("brand-link bg-", brandColor) else "brand-link",
      href = url,
      shiny::tags$img(
        src = src,
        class = "brand-image img-circle elevation-3",
        style = paste0("opacity: ", opacity)
      ),
      shiny::tags$span(class = "brand-text font-weight-light", title)
    )
  }

  # sidebar content
  contentTag <- shiny::tags$div(
    class = "sidebar",
    shiny::tags$nav(
      class = "mt-2",
      ...
    )
  )

  sidebarTag <- shiny::tags$aside(
    class = paste0(
      "main-sidebar sidebar-", skin, "-", 
      status, " elevation-", elevation
    )
   )

  sidebarTag <- shiny::tagAppendChildren(sidebarTag, brandTag, contentTag)
  sidebarTag
  
  
  ## change sidebar width
  #shiny::tagList(
  #  shiny::singleton(
  #    shiny::tags$head(
  #      shiny::tags$style(
  #        shiny::HTML(
  #          paste0(
  #            ".main-sidebar, .main-sidebar:before {
  #              transition: margin-left 0.3s ease-in-out, width 0.3s ease-in-out;
  #              width: ", width, "px;
  #            }
  #            .sidebar-mini.sidebar-collapse .main-sidebar:hover {
  #                width: ", width, "px;
  #            }
  #            @media (min-width: 768px) {
  #              .content-wrapper,
  #              .main-footer,
  #              .main-header {
  #                transition: margin-left 0.3s ease-in-out;
  #                margin-left: ", width, "px;
  #                z-index: 3000;
  #              }
  #            }
  #            .nav-sidebar:hover {
  #              overflow: hidden;
  #            }
  #            "
  #          )
  #        )
  #      )
  #    )
  #  ),
  #  sidebarTag
  #)
  
}



#' Create a Boostrap 4 dashboard main sidebar menu
#'
#' Build an adminLTE3 dashboard main sidebar menu
#'
#' @param ... Slot for \link{bs4SidebarMenuItem} or \link{bs4SidebarHeader}.
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
bs4SidebarMenu <- function(...) {
  
  # menu Tag
  shiny::tags$ul(
    class = "nav nav-pills nav-sidebar flex-column",
    `data-widget` = "treeview",
    id = "mymenu",
    role = "menu",
    `data-accordion` = "false",
    ...
  )
  
}






#' Create a Boostrap 4 dashboard main sidebar menu item
#'
#' Build an adminLTE3 dashboard main sidebar menu item
#'
#' @param text Item name.
#' @param ... \link{bs4SidebarMenuSubItem}.
#' @param tabName Should correspond exactly to the tabName given in \code{\link{bs4TabItem}}.
#' @param icon Item icon.
#' @param startExpanded Whether to expand the \link{bs4SidebarMenuItem} at start.
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
bs4SidebarMenuItem <- function(text, ..., tabName = NULL, icon = NULL, startExpanded = FALSE) {
  
  subitems <- list(...)
  
  # classic menuItem with 1 element
  if (length(subitems) == 0) {
    return(
      shiny::tags$li(
        class = "nav-item",
        shiny::tags$a(
          class = "nav-link",
          id = paste0("tab-", tabName),
          href = paste0("#shiny-tab-", tabName),
          `data-toggle` = "tab",
          `data-value` = tabName,
          shiny::tags$i(class = paste0("nav-icon fa fa-", icon)),
          shiny::tags$p(text)
        )
      )
    )
    # in case we have multiple subitems
  } else {
    menuItemCl <- "nav-item has-treeview"
    if (startExpanded) menuItemCl <- paste0(menuItemCl, " menu-open")
    shiny::tags$li(
      class = menuItemCl,
      shiny::tags$a(
        href = "#",
        class = "nav-link",
        shiny::tags$i(class = paste0("nav-icon fas fa-", icon)),
        shiny::tags$p(
          text,
          shiny::tags$i(class = "right fas fa-angle-left")
        )
      ),
      shiny::tags$ul(
        class = "nav nav-treeview",
        ...
      )
    )
  }
  
}





#' Create a Boostrap 4 dashboard main sidebar menu sub-item
#'
#' Build an adminLTE3 dashboard main sidebar menu sub-item
#'
#' @param text Item name.
#' @param tabName Should correspond exactly to the tabName given in \code{\link{bs4TabItem}}.
#' @param icon Item icon.
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
#' @examples 
#' if (interactive()) {
#'  library(shiny)
#'  library(bs4Dash)
#'  
#'  shiny::shinyApp(
#'    ui = bs4DashPage(
#'      navbar = bs4DashNavbar(),
#'      sidebar = bs4DashSidebar(
#'        bs4SidebarMenu(
#'          bs4SidebarHeader("List of items 1"),
#'          bs4SidebarMenuItem(
#'            text = "Item List",
#'            icon = "bars",
#'            startExpanded = TRUE,
#'            bs4SidebarMenuSubItem(
#'              text = "Item 1",
#'              tabName = "item1",
#'              icon = "circle-thin"
#'            ),
#'            bs4SidebarMenuSubItem(
#'              text = "Item 2",
#'              tabName = "item2",
#'              icon = "circle-thin"
#'            )
#'          ),
#'          bs4SidebarHeader("Classic Items"),
#'          bs4SidebarMenuItem(
#'            text = "Item 3",
#'            tabName = "item3"
#'          ),
#'          bs4SidebarHeader("List of items 2"),
#'          bs4SidebarMenuItem(
#'            text = "Item List 2",
#'            icon = "bars",
#'            startExpanded = FALSE,
#'            #active = FALSE,
#'            bs4SidebarMenuSubItem(
#'              text = "Item 4",
#'              tabName = "item4",
#'              icon = "circle-thin"
#'            ),
#'            bs4SidebarMenuSubItem(
#'              text = "Item 5",
#'              tabName = "item5",
#'              icon = "circle-thin"
#'            )
#'          )
#'        )
#'      ),
#'      controlbar = bs4DashControlbar(),
#'      footer = bs4DashFooter(),
#'      title = "test",
#'      body = bs4DashBody(
#'        bs4TabItems(
#'          bs4TabItem(
#'            tabName = "item1",
#'            bs4Card(
#'              title = "Card 1", 
#'              closable = TRUE, 
#'              width = 6,
#'              solidHeader = TRUE, 
#'              status = "primary",
#'              collapsible = TRUE,
#'              p("Box Content")
#'            )
#'          ),
#'          bs4TabItem(
#'            tabName = "item2",
#'            bs4Card(
#'              title = "Card 2", 
#'              closable = TRUE, 
#'              width = 6,
#'              solidHeader = TRUE, 
#'              status = "warning",
#'              collapsible = TRUE,
#'              p("Box Content")
#'            )
#'          ),
#'          bs4TabItem(
#'            tabName = "item3",
#'            bs4Card(
#'              title = "Card 3", 
#'              closable = TRUE, 
#'              width = 6,
#'              solidHeader = TRUE, 
#'              status = "danger",
#'              collapsible = TRUE,
#'              p("Box Content")
#'            )
#'          ),
#'          bs4TabItem(
#'            tabName = "item4",
#'            bs4Card(
#'              title = "Card 4", 
#'              closable = TRUE, 
#'              width = 6,
#'              solidHeader = TRUE, 
#'              status = "info",
#'              collapsible = TRUE,
#'              p("Box Content")
#'            )
#'          ),
#'          bs4TabItem(
#'            tabName = "item5",
#'            bs4Card(
#'              title = "Card 5", 
#'              closable = TRUE, 
#'              width = 6,
#'              solidHeader = TRUE, 
#'              status = "success",
#'              collapsible = TRUE,
#'              p("Box Content")
#'            )
#'          )
#'        )
#'      )
#'    ),
#'    server = function(input, output) {}
#'  )
#' }
bs4SidebarMenuSubItem <- function(text, tabName = NULL, icon = NULL) {
  shiny::tags$li(
    class = "nav-item",
    shiny::tags$a(
      class = "nav-link",
      id = paste0("tab-", tabName),
      href = paste0("#shiny-tab-", tabName),
      `data-toggle` = "tab",
      `data-value` = tabName,
      shiny::tags$i(class = paste0("nav-icon fa fa-", icon)),
      shiny::tags$p(text)
    )
  )
}


#' Create a Boostrap 4 dashboard main sidebar header
#'
#' Build an adminLTE3 dashboard main sidebar header
#'
#' @param title SidebarHeader title.
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
bs4SidebarHeader <- function(title) {
  shiny::tags$li(
    class = "nav-header",
    title
  )
}



#' Create a Boostrap 4 dashboard main sidebar user panel
#'
#' Build an adminLTE3 dashboard main sidebar user panel
#'
#' @param img User panel image path or url.
#' @param text User panel text.
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
bs4SidebarUserPanel <- function(img = NULL, text = NULL) {
  shiny::tags$div(
    class = "user-panel mt-3 pb-3 mb-3 d-flex",
    shiny::tags$div(
      class = "image",
      shiny::img(src = img, class = "img-circle elevation-2")
    ),
    shiny::tags$div(
      class = "info",
      shiny::a(class = "d-block", href = "#", text)
    )
  )
}
