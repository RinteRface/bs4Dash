#' Create a value box (server side)
#'
#' This is the server-side function for creating a dynamic
#' \code{\link{bs4ValueBox}}.
#'
#' @inheritParams shiny::renderUI
#' @seealso \code{\link{bs4ValueBoxOutput}} for the corresponding UI-side function.
#' 
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @examples
#' if(interactive()){
#'  library(shiny)
#'  library(bs4Dash)
#'  
#'  shiny::shinyApp(
#'    ui = bs4DashPage(
#'      navbar = bs4DashNavbar(),
#'      sidebar = bs4DashSidebar(),
#'      controlbar = bs4DashControlbar(),
#'      footer = bs4DashFooter(),
#'      title = "test",
#'      body = bs4DashBody(
#'       fluidRow(
#'        bs4ValueBoxOutput("vbox"),
#'        bs4InfoBoxOutput("ibox")
#'       )
#'      )
#'    ),
#'    server = function(input, output) {
#'    
#'      output$vbox <- renderbs4ValueBox({
#'       bs4ValueBox(
#'         value = 150,
#'         subtitle = "New orders",
#'         status = "primary",
#'         icon = "shopping-cart",
#'         href = "#"
#'        )
#'     })
#'     
#'     output$ibox <- renderbs4InfoBox({
#'       bs4InfoBox(
#'        title = "Comments",
#'        gradientColor = "success",
#'        value = 41410,
#'        icon = "comments"
#'        )
#'     })
#'     
#'    }
#'  )
#' }
#' 
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



#' @rdname renderbs4ValueBox
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
bs4ValueBoxOutput <- function(outputId, width = 4) {
  shiny::uiOutput(outputId, class = paste0("col-sm-", width))
}



#' @rdname bs4ValueBoxOutput
#' @export
bs4InfoBoxOutput <- bs4ValueBoxOutput