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