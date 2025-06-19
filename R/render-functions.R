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
#'     ui = dashboardPage(
#'       header = dashboardHeader(),
#'       sidebar = dashboardSidebar(),
#'       controlbar = dashboardControlbar(),
#'       footer = dashboardFooter(),
#'       title = "test",
#'       body = dashboardBody(
#'         fluidRow(
#'           valueBoxOutput("vbox"),
#'           infoBoxOutput("ibox")
#'         )
#'       )
#'     ),
#'     server = function(input, output) {
#'       output$vbox <- renderValueBox({
#'         valueBox(
#'           value = 150,
#'           subtitle = "New orders",
#'           color = "primary",
#'           icon = icon("shopping-cart"),
#'           href = "#"
#'         )
#'       })
#'
#'       output$ibox <- renderInfoBox({
#'         infoBox(
#'           title = "Comments",
#'           fill = TRUE,
#'           gradient = TRUE,
#'           color = "success",
#'           value = 41410,
#'           icon = icon("comments")
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
#' @param tag A tag function, like `tags$li` or `tags$ul`.
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
#'         menuItem("Menu item", icon = icon("calendar-days"))
#'       )
#'     })
#'   }
#'
#'   shinyApp(ui, server)
#' }
renderMenu <- function(expr, env = parent.frame(), quoted = FALSE, outputArgs = list()) {
  if (!quoted) {
    expr <- substitute(expr)
    quoted <- TRUE
  }
  shiny::renderUI(expr, env = env, quoted = quoted, outputArgs = outputArgs)
}





