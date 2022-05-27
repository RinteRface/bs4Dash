#' Boostrap 4 column system
#' 
#' This function overwrites that of Shiny since there
#' are differences between the Bootstrap 3 and Bootstrap 4 grid systems
#'
#' @param width The grid width of the column (must be between 1 and 12.
#' @param ... Elements to include within the column.
#' @param offset The number of columns to offset this column from the end of the previous column.
#' 
#' @export
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(bs4Dash)
#'  
#'  ui <- bs4DashPage(
#'    navbar = bs4DashNavbar(),
#'    sidebar = dashboardSidebar(
#'      bs4SidebarMenu(
#'        bs4SidebarMenuItem(
#'          "Welcome!",
#'          tabName = "tab_welcome",
#'          icon = "home"
#'        )
#'      )
#'    ),
#'    
#'    body = bs4DashBody(
#'      bs4TabItems(
#'        bs4TabItem(
#'          tabName = "tab_welcome",
#'          fluidRow(
#'            column(
#'              width = 1,
#'              offset = 11,
#'              actionButton(
#'                "mybutton",label = "",icon = icon("question-circle")
#'              )
#'            )
#'            
#'          ),
#'          fluidRow(
#'            h2("Placeholder")
#'          )
#'        )
#'      )
#'    )
#'  )
#'  
#'  server <- function(input, output, session) {}
#'  shinyApp(ui = ui, server = server)
#' }
column <- function(width, ..., offset = 0) {
  if (!is.numeric(width) || (width < 1) || (width > 12)) 
    stop("column width must be between 1 and 12")
  colClass <- paste0("col-sm-", width)
  if (offset > 0) 
    colClass <- paste0(colClass, " offset-sm-", offset)
  shiny::div(class = colClass, ...)
}


#' TBD
bs4col <- function
(
    ...,
    width = 0,
    width.xs = NULL, width.md = NULL, width.lg = NULL, width.xl = NULL,
    offset = NULL,
    offset.xs = NULL, offset.md = NULL, offset.lg = NULL, offset.xl = NULL,
    align = NULL, class = c()
) {
    colClass = class
    breakpoints <- c(".xs", "-sm"="", "-md"=".md", "-lg"=".lg", "-xl"=".xl")
    for (i in seq_along(breakpoints)) {
        col = paste0("col", names(breakpoints[i]))
        w = base::get(paste0("width", breakpoints[i]))
        if (is.null(w)) {
            if (breakpoints[i] == "") {
                stop("column width must be 0-12 or 'auto'")
            }
        } else if  (w == "auto" || (is.numeric(w) && w >= 1 && w <= 12)) {
            colClass <- c(colClass, paste0(col, "-", w))
        } else if (width == 0) {
            colClass <- c(colClass, col)
        } else {
            stop("column width must be 0-12 or 'auto'")
        }
        col = paste0("offset", names(breakpoints[i]))
        w = base::get(paste0("offset", breakpoints[i]))
        if (is.null(w)) {
        } else if (is.numeric(w) && w >= 0 && w <= 12) {
            colClass <- c(colClass, paste0(col, "-", w))
        } else if (width == 0) {
            colClass <- c(colClass, col)
        } else {
            stop("offset must be 0-12")
        }
    }
    if (!is.null(align)) {
        if (!align %in% c("start", "center", "end")) {
            stop("vertical alignment must be start, center or end")
        }
        colClass <- c(colClass, paste0("align-self-", align))
    }
    shiny::div(class = colClass, ...)
}

#' TBD
bs4row <- function(..., align = NULL, justify = NULL, class = c()) {
    colClass <- c(class, "row")
    if (!is.null(align)) {
        if (!align %in% c("start", "center", "end")) {
            stop("vertical alignment must be start, center or end")
        }
        colClass <- c(colClass, paste0("align-self-", align))
    }
    if (!is.null(justify)) {
        if (!align %in% c("start", "center", "end", "around", "between")) {
            stop("vertical alignment must be start, center, end, around or between")
        }
        colClass <- c(colClass, paste0("justify-content-", align))
    }
    shiny::div(class = class, ...)
}
