#' Dashboard Footer
#'
#' This creates a dashboard footer to insert in \link{dashboardPage}.
#'
#' @param left Left text.
#' @param right Right text.
#' @param fixed Whether to fix the footer. Default to FALSE.
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#' @rdname dashboardFooter
#'
#' @export
bs4DashFooter <- function(left = NULL, right = NULL, fixed = FALSE) {

  shiny::tags$footer(
    class = "main-footer",
    `data-fixed` = tolower(fixed),
    # right element
    shiny::tags$div(
      class = "float-right d-none d-sm-inline",
      right
    ),
    left
  )
}
