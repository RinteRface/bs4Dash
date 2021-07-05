#' Create a value box (server side)
#'
#' This is the server-side function for creating a dynamic
#' \code{\link{bs4ValueBox}}.
#'
#' @inheritParams shiny::renderUI
#' @seealso \code{\link{bs4ValueBoxOutput}} for the corresponding UI-side function.
#'
#' @rdname valueBox
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   library(bs4Dash)
#'
#'   shiny::shinyApp(
#'     ui = bs4DashPage(
#'       navbar = bs4DashNavbar(),
#'       sidebar = bs4DashSidebar(),
#'       controlbar = bs4DashControlbar(),
#'       footer = bs4DashFooter(),
#'       title = "test",
#'       body = bs4DashBody(
#'         fluidRow(
#'           bs4ValueBoxOutput("vbox"),
#'           bs4InfoBoxOutput("ibox")
#'         )
#'       )
#'     ),
#'     server = function(input, output) {
#'       output$vbox <- renderbs4ValueBox({
#'         bs4ValueBox(
#'           value = 150,
#'           subtitle = "New orders",
#'           status = "primary",
#'           icon = "shopping-cart",
#'           href = "#"
#'         )
#'       })
#'
#'       output$ibox <- renderbs4InfoBox({
#'         bs4InfoBox(
#'           title = "Comments",
#'           gradientColor = "success",
#'           value = 41410,
#'           icon = "comments"
#'         )
#'       })
#'     }
#'   )
#' }
#' @export
renderbs4ValueBox <- function(expr, env = parent.frame(), quoted = FALSE) {
  # Convert the expression to a function
  vbox_fun <- shiny::exprToFunction(expr, env, quoted)

  # Wrap that function in another function which strips off the outer div and
  # send it to renderUI.
  shiny::renderUI({
    vbox <- vbox_fun()
    tagAssert(vbox, type = "div")

    # Strip off outer div, since it's already present in output
    vbox$children[[1]]
  })
}



#' @rdname infoBox
#' @inheritParams renderbs4ValueBox
#' @export
renderbs4InfoBox <- renderbs4ValueBox



#' Create a value box output (client side)
#'
#' This is the UI-side function for creating a dynamic \code{\link{bs4ValueBox}}.
#'
#' @inheritParams bs4ValueBox
#' @param outputId Output variable name.
#' @seealso \code{\link{renderbs4ValueBox}} for the corresponding server-side
#'   function and examples.
#' @export
#' @rdname valueBox
bs4ValueBoxOutput <- function(outputId, width = 4) {
  shiny::uiOutput(outputId, class = paste0("col-sm-", width))
}



#' @rdname infoBox
#' @inheritParams bs4ValueBoxOutput
#' @export
bs4InfoBoxOutput <- bs4ValueBoxOutput





#' Create a dynamic menu output for bs4Dash (client side)
#'
#' This can be used as a placeholder for dynamically-generated
#' \code{\link{dropdownMenu}}, \code{\link{notificationItem}},
#' \code{\link{messageItem}}, \code{\link{taskItem}} \code{\link{sidebarMenu}},
#' or \code{\link{menuItem}}. If called directly, you must make sure to supply
#' the correct type of tag. It is simpler to use the wrapper functions if
#' present; for example, \code{\link{dropdownMenuOutput}} and
#' \code{\link{sidebarMenuOutput}}.
#'
#' @param outputId Output variable name.
#' @param tag A tag function, like \code{tags$li} or \code{tags$ul}.
#'
#' @family menu outputs
#' @seealso \code{\link{renderMenu}} for the corresponding server side function
#'   and examples.
menuOutput <- function(outputId, tag = shiny::tags$li) {
  tag(id = outputId, class = "bs4Dash-menu-output dropdown")
}




#' Create a dropdown menu output (client side)
#'
#' This is the UI-side function for creating a dynamic dropdown menu.
#'
#' @inheritParams menuOutput
#' @family menu outputs
#' @seealso \code{\link{renderMenu}} for the corresponding server-side function
#'   and examples, and \code{\link{dropdownMenu}} for the corresponding function
#'   for generating static menus.
#' @export
dropdownMenuOutput <- function(outputId) {
  menuOutput(outputId = outputId, tag = shiny::tags$li)
}




#' Create a sidebar menu output (client side)
#'
#' This is the UI-side function for creating a dynamic sidebar menu.
#'
#' @inheritParams menuOutput
#' @family menu outputs
#' @seealso \code{\link{renderMenu}} for the corresponding server-side function
#'   and examples, and \code{\link{sidebarMenu}} for the corresponding function
#'   for generating static sidebar menus.
#' @export
sidebarMenuOutput <- function(outputId) {
  menuOutput(outputId = outputId, tag = shiny::tags$ul)
}





#' Create a sidebar menu item output (client side)
#'
#' This is the UI-side function for creating a dynamic sidebar menu item.
#'
#' @inheritParams menuOutput
#' @family menu outputs
#' @seealso \code{\link{renderMenu}} for the corresponding server-side function
#'   and examples, and \code{\link{menuItem}} for the corresponding function
#'   for generating static sidebar menus.
#' @export
menuItemOutput <- function(outputId) {
  menuOutput(outputId = outputId, tag = shiny::tags$li)
}




#' Create dynamic menu output (server side)
#'
#' @inheritParams shiny::renderUI
#'
#' @seealso \code{\link{menuOutput}} for the corresponding client side function
#'   and examples.
#' @family menu outputs
#' @export
#' @examples
#' ## Only run these examples in interactive R sessions
#'
#' if (interactive()) {
#'   library(shiny)
#'   library(bs4Dash)
#'   messageData <- data.frame(
#'     from = c("Admininstrator", "New User", "Support"),
#'     message = c(
#'       "Sales are steady this month.",
#'       "How do I register?",
#'       "The new server is ready."
#'     ),
#'     stringsAsFactors = FALSE
#'   )
#'
#'   # ========== Dynamic dropdownMenu ==========
#'   ui <- dashboardPage(
#'     dashboardHeader(
#'       title = "Dynamic menus",
#'       dropdownMenuOutput("messageMenu")
#'     ),
#'     dashboardSidebar(),
#'     dashboardBody(
#'       fluidRow(
#'         box(
#'           title = "Controls",
#'           sliderInput("slider", "Number of observations:", 1, 100, 50)
#'         )
#'       )
#'     )
#'   )
#'
#'   server <- function(input, output) {
#'     output$messageMenu <- renderMenu({
#'       # Code to generate each of the messageItems here, in a list. messageData
#'       # is a data frame with two columns, 'from' and 'message'.
#'       # Also add on slider value to the message content, so that messages update.
#'       msgs <- apply(messageData, 1, function(row) {
#'         messageItem(
#'           from = row[["from"]],
#'           message = paste(row[["message"]], input$slider)
#'         )
#'       })
#'
#'       dropdownMenu(type = "messages", .list = msgs)
#'     })
#'   }
#'
#'   shinyApp(ui, server)
#'
#'
#'   # ========== Dynamic sidebarMenu ==========
#'   ui <- dashboardPage(
#'     dashboardHeader(title = "Dynamic sidebar"),
#'     dashboardSidebar(
#'       sidebarMenuOutput("menu")
#'     ),
#'     dashboardBody()
#'   )
#'
#'   server <- function(input, output) {
#'     output$menu <- renderMenu({
#'       sidebarMenu(
#'         menuItem("Menu item", icon = icon("calendar"))
#'       )
#'     })
#'   }
#'
#'   shinyApp(ui, server)
#' }
renderMenu <- shiny::renderUI