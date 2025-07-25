#' Boostrap 4 dashboard body
#'
#' \link{dashboardBody} creates the main body container for a \link{dashboardPage}.
#'
#' @param ... Body content, slot for \link{tabItems}.
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @rdname dashboardBody
#'
#' @seealso \link{dashboardSidebar}
#'
#' @export
bs4DashBody <- function(...) {
  shiny::tags$div(
    class = "content-wrapper",
    shiny::tags$section(
      class = "content",
      ...
    )
  )
}


#' Boostrap 4 body items
#'
#' \link{tabItems} creates a wrapper for multiple \link{tabItem}.
#'
#' @param ... Items to put in the container. Each item should be a
#'   \code{\link{tabItem}}.
#' @param .list Pass items as list with \link{lapply} family functions.
#'
#' @rdname dashboardBody
#'
#' @export
bs4TabItems <- function(..., .list = NULL) {
  items <- c(list(...), .list)
  lapply(items, tagAssert, class = "tab-pane")

  shiny::tags$div(class = "tab-content", items)
}

#' Boostrap 4 body item
#'
#' \link{tabItem} creates a body tab content.
#'
#' @param tabName The name of a tab. This must correspond to the `tabName`
#'   of a sidebar \code{\link{menuItem}}.
#' @param ... Contents of the tab.
#'
#' @rdname dashboardBody
#' @export
bs4TabItem <- function(tabName = NULL, ...) {
  if (is.null(tabName)) {
    cli::cli_abort("Need tabName")
  }

  validateTabName(tabName)

  shiny::tags$div(
    role = "tabpanel",
    class = "tab-pane container-fluid",
    id = paste0("shiny-tab-", tabName),
    ...
  )
}
