#' Create a Boostrap 4 dashboard body
#'
#' Build an adminLTE3 dashboard body
#'
#' @param ... Body content, slot for \link{bs4TabItems}.
#' 
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
bs4DashBody <- function(...) {
  shiny::tags$div(
    class = "content-wrapper",
    ...
  )
}




#' A container for tab items
#'
#' @param ... Items to put in the container. Each item should be a
#'   \code{\link{bs4TabItem}}.
#'   
#' @export
bs4TabItems <- function(...) {
  lapply(list(...), tagAssert, class = "tab-pane")
  
  shiny::tags$div(class = "tab-content", ...)
}

#' One tab to put inside a tab items container
#'
#' @param tabName The name of a tab. This must correspond to the \code{tabName}
#'   of a \code{\link{bs4SidebarMenuItem}}.
#' @param ... Contents of the tab.
#' 
#' @export
bs4TabItem <- function(tabName = NULL, ...) {
  if (is.null(tabName))
    stop("Need tabName")
  
  validateTabName(tabName)
  
  shiny::tags$div(
    role = "tabpanel",
    class = "tab-pane container-fluid",
    id = paste0("shiny-tab-", tabName),
    shiny::br(),
    ...
  )
}
