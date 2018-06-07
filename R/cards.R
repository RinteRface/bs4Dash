#' Create a Boostrap 4 card
#'
#' Build an adminLTE3 card
#'
#' @param ... Contents of the box.
#' @param title Optional title.
#' @param footer Optional footer text.
#' @param status The status of the item This determines the item's background
#'   color. "primary", "success", "warning", "danger". NULL by default.
#' @param solidHeader Should the header be shown with a solid color background?
#' @param gradientColor If NULL (the default), the background of the box will be
#'   white. Otherwise, a color string. "primary", "success", "warning" or "danger".
#' @param width The width of the box, using the Bootstrap grid system. This is
#'   used for row-based layouts. The overall width of a region is 12, so the
#'   default valueBox width of 4 occupies 1/3 of that width. For column-based
#'   layouts, use \code{NULL} for the width; the width is set by the column that
#'   contains the box.
#' @param height The height of a box, in pixels or other CSS unit. By default
#'   the height scales automatically with the content.
#' @param collapsible If TRUE, display a button in the upper right that allows
#'   the user to collapse the box.
#' @param collapsed If TRUE, start collapsed. This must be used with
#'   \code{collapsible=TRUE}.
#' @param closable If TRUE, display a button in the upper right that allows the user to close the box.
#' @param labelStatus status of the box label: "danger", "success", "primary", "warning".
#' @param labelText Label text.
#' @param labelTooltip Label tooltip displayed on hover.
#' @param dropdownMenu List of items in the the boxtool dropdown menu. Use dropdownItemList().
#' @param dropdownIcon Dropdown icon. "wrench" by default.
#'
#' @examples
#' if(interactive()){
#'  library(shiny)
#'
#'  shiny::shinyApp(
#'    ui = bs4DashPage(
#'     navbar = bs4DashNavbar(),
#'     sidebar = bs4DashSidebar(),
#'     controlbar = bs4DashControlbar(),
#'     footer = bs4DashFooter(),
#'     title = "test",
#'     body = bs4DashBody(
#'     fluidRow(
#'      column(
#'       width = 6,
#'       bs4Card(
#'        title = "Closable Box with dropdown", 
#'        closable = TRUE, 
#'        width = 12,
#'        status = "warning", 
#'        solidHeader = FALSE, 
#'        collapsible = TRUE,
#'        labelText = 1,
#'        labelStatus = "danger",
#'        labelTooltip = "Hi Bro!",
#'        dropdownIcon = "wrench",
#'        dropdownMenu = dropdownItemList(
#'          dropdownItem(url = "http://www.google.com", name = "Link to google"),
#'          dropdownItem(url = "#", name = "item 2"),
#'          dropdownDivider(),
#'          dropdownItem(url = "#", name = "item 3")
#'        ),
#'        p("Box Content")
#'       )
#'      ),
#'      column(
#'       width = 6, 
#'       bs4Card(
#'        title = "Closable Box with gradient", 
#'        closable = TRUE, 
#'        width = 12,
#'        status = "warning", 
#'        solidHeader = FALSE, 
#'        gradientColor = "success",
#'        collapsible = TRUE,
#'        p("Box Content")
#'       )
#'      )
#'      ),
#'      bs4Card(
#'        title = "Closable Box with solidHeader", 
#'        closable = TRUE, 
#'        width = 6,
#'        solidHeader = TRUE, 
#'        status = "primary",
#'        collapsible = TRUE,
#'        p("Box Content")
#'       )
#'     )
#'    ),
#'    server = function(input, output) {}
#'  )
#' }
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
bs4Card <- function(..., title = NULL, footer = NULL, status = NULL,
                    solidHeader = FALSE, gradientColor = NULL, width = 6, 
                    height = NULL, collapsible = TRUE, collapsed = FALSE, 
                    closable = TRUE, labelStatus = NULL, labelText = NULL, 
                    labelTooltip = NULL, dropdownMenu = NULL, dropdownIcon = "wrench") {
  
  cardCl <- if (!is.null(gradientColor)) {
    paste0("card bg-", gradientColor, "-gradient")
  } else {
    if (is.null(status)) {
      "card card-default"
    } else {
      if (isTRUE(solidHeader)) {
        paste0("card card-outline card-", status)
      } else {
        paste0("card card-", status)
      }
    }
  }
    
  if (isTRUE(collapsed)) cardCl <- paste0(cardCl, " collapsed-card")
  
  cardToolTag <- shiny::tags$div(
    class = "card-tools",
    
    # labels
    if (!is.null(labelText) || !is.null(labelStatus) || !is.null(labelTooltip)) {
      shiny::tags$span(
        class = paste0("badge bg-", labelStatus),
        title = if (!is.null(labelTooltip)) labelTooltip,
        `data-toggle` = "tooltip",
        labelText
      )
    },
    
    if (!is.null(dropdownMenu)) {
      shiny::tags$div(
        class = "btn-group",
        shiny::tags$button(
          type = "button",
          class = "btn btn-tool dropdown-toggle",
          `data-toggle` = "dropdown",
          shiny::icon(dropdownIcon)
        ),
        dropdownMenu
      )
    },
    
    # collapse
    if (isTRUE(collapsible)) {
      collapseIcon <- if (collapsed) 
        "plus"
      else "minus"
      shiny::tags$button(
        type = "button",
        class = "btn btn-tool",
        `data-widget` = "collapse",
        shiny::icon(collapseIcon)
      )
    },
    
    # close
    if (isTRUE(closable)) {
      shiny::tags$button(
        type = "button",
        class = "btn btn-tool",
        `data-widget` = "remove",
        shiny::tags$i(class = "fa fa-times")
      )
    }
  )
  
  # header
  headerTag <- shiny::tags$div(
    class = "card-header",
    shiny::tags$h3(class = "card-title", title)
  )
  headerTag <- shiny::tagAppendChild(headerTag, cardToolTag)
  
  # body
  bodyTag <- shiny::tags$div(
    class = "card-body",
    ...
  )
  
  footerTag <- shiny::tags$div(
    class = "card-footer",
    footer
  )
  
  style <- NULL
  if (!is.null(height)) {
    style <- paste0("height: ", shiny::validateCssUnit(height))
  }
  
  cardTag <- shiny::tags$div(class = cardCl)
  cardTag <- shiny::tagAppendChildren(cardTag, headerTag, bodyTag, footerTag)
  
  shiny::tags$div(
    class = if (!is.null(width)) paste0("col-sm-", width),
   cardTag
  )
    
}




#' Create a box dropdown item list
#'
#' Can be used to add dropdown items to a cardtool.
#'
#' @param ... Slot for dropdownItem.
#'
#' @export
dropdownItemList <- function(...) {
  shiny::tags$div(
    class = "dropdown-menu dropdown-menu-right",
    role = "menu",
    ...
  )
}



#' Create a box dropdown item 
#'
#' @param url Target url or page.
#' @param name Item name.
#'
#' @export
dropdownItem <- function(url = NULL, name = NULL) {
  shiny::tags$a(
    class = "dropdown-item",
    href = url,
    target = "_blank",
    name 
  )
}



#' Create a box dropdown divider 
#'
#' @note Useful to separate 2 sections of dropdown items.
#'
#' @export
dropdownDivider <- function() {
  shiny::tags$a(class = "divider")
}