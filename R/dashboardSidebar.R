#' Create a Boostrap 4 dashboard main sidebar
#'
#' Build an adminLTE3 dashboard main sidebar
#'
#' @param ... Slot for bs4SidebarMenu.
#' @param title Sidebar title.
#' @param skin Sidebar skin. "dark" or "light"
#' @param status Sidebar status. "primary", "danger", "warning", "success", "info".
#' @param brandColor Brand color. NULL by default: "primary", "danger", "warning",
#' "success", "info", "white" or "gray-light".
#' @param url Sidebar brand link.
#' @param src Sidebar brand image.
#' @param elevation Sidebar elevation. 4 by default.
#' @param opacity Sidebar opacity. From 0 to 1. 0.8 by default.
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
    class = paste0("main-sidebar sidebar-", skin,
                   "-", status, " elevation-", elevation))

  sidebarTag <- shiny::tagAppendChildren(sidebarTag, brandTag, contentTag)
  sidebarTag

}



#' Create a Boostrap 4 dashboard main sidebar menu
#'
#' Build an adminLTE3 dashboard main sidebar menu
#'
#' @param ... Slot for bs4SidebarMenuItemList or bs4SidebarMenuItem or bs4SidebarHeader
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
bs4SidebarMenu <- function(...) {
 shiny::tags$ul(
   class = "nav nav-pills nav-sidebar flex-column",
   `data-widget` = "treeview",
   role = "menu",
   `data-accordion` = "false",
   ...
 )
}




#' Create a Boostrap 4 dashboard main sidebar menu item list
#'
#' Build an adminLTE3 dashboard main sidebar menu item list
#'
#' @param ... Slot for bs4SidebarMenuItem.
#' @param name Item list name.
#' @param icon Item list icon.
#' @param open Whether to display the item list in an open state. FALSE by default.
#' @param active Whether the section is active. FALSE by default.
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
bs4SidebarMenuItemList <- function(..., name = NULL, icon = NULL,
                                   open = FALSE, active = FALSE) {

  menuItemCl <- "nav-item has-treeview"
  if (isTRUE(open)) menuItemCl <- paste0(menuItemCl, " menu-open")

  shiny::tags$li(
    class = menuItemCl,
    shiny::tags$a(
      href = "#",
      `data-toggle` = "tab",
      class = if (isTRUE(active)) "nav-link active" else "nav-link",
      shiny::tags$i(class = paste0("nav-icon fas fa-", icon)),
      shiny::tags$p(
        name,
        shiny::tags$i(class = "right fas fa-angle-left")
      )
    ),
    shiny::tags$ul(
      class = "nav nav-treeview",
      ...
    )
  )
}




#' Create a Boostrap 4 dashboard main sidebar menu item
#'
#' Build an adminLTE3 dashboard main sidebar menu item
#'
#' @param ... Item name.
#' @param tabName Should correspond exactly to the tabName given in \code{\link{bs4TabItem}}.
#' @param icon Item icon.
#' @param active Whether the item is active. FALSE by default.
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
bs4SidebarMenuItem <- function(..., tabName = NULL, icon = NULL, active = FALSE) {

  shiny::tags$li(
    class = "nav-item",
    shiny::tags$a(
      href = paste0("#shiny-tab-", tabName),
      `data-toggle` = "tab",
      `data-value` = tabName,
      class = if (isTRUE(active)) "nav-link active" else "nav-link",
      shiny::tags$i(class = paste0("nav-icon fas fa-", icon)),
      shiny::tags$p(
        ...
      )
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
