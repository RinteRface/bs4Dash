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
#' @examples
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
bs4DashboardSidebar <- function(..., title, skin = "dark", status = "primary",
                                brandColor = NULL, url = NULL, src = NULL,
                                elevation = 4, opacity = .8) {

  stopifnot(!is.null(title))

  # brand logo
  brandTag <- shiny::tags$a(
    class = if (!is.null(brandColor)) paste0("brand-link bg-", brandColor) else "brand-link",
    href = url,
    shiny::tags$img(
      src = src,
      class = "brand-image img-circle elevation-3",
      style = paste0("opacity: ", opacity)
    ),
    shiny::tags$span(class = "brand-text font-weight-light", title)
  )

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
#' @param ... Slot for bs4SidebarMenuItemList or bs4SidebarMenuItem.
#'
#' @examples
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
#' @examples
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
      class = if (isTRUE(active)) "nav-link active" else "nav-link",
      shiny::tags$i(class = paste0("nav-icon fa fa-", icon)),
      shiny::tags$p(
        name,
        shiny::tags$i(class = "right fa fa-angle-left")
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
#' @param ... Any element such as badge.
#' @param name Item list name.
#' @param icon Item icon.
#' @param active Whether the item is active. FALSE by default.
#'
#' @examples
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
bs4SidebarMenuItem <- function(..., name = NULL, icon = NULL, active = FALSE) {

  shiny::tags$li(
    class = "nav-item",
    shiny::tags$a(
      href = "#",
      class = if (isTRUE(active)) "nav-link active" else "nav-link",
      shiny::tags$i(class = paste0("nav-icon fa fa-", icon)),
      shiny::tags$p(
        name,
        ...
      )
    )
  )
}
